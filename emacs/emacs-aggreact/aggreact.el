;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record all keys for each command.

(setq aggreact--current-single-command-key-groups nil)
(setq aggreact--current-single-command-raw-key-groups nil)

(defun aggreact--read-key-sequence-advice (&rest args)
  ;; TODO - should this use this-single-command-keys, this-single-command-raw-keys, this-command-keys, or what?
  (setq aggreact--current-single-command-key-groups
        (cons (this-single-command-keys)
              aggreact--current-single-command-key-groups))
  (setq aggreact--current-single-command-raw-key-groups
        (cons (this-single-command-raw-keys)
              aggreact--current-single-command-raw-key-groups)))

(defun aggreact-this-command-all-keys (&optional raw as-groups)
  "Return a vector of keys used for this command, including any uses of `read-key-sequence', at least up to the point where this function is used.
Returns a vector, or if AS-GROUPS is 't', return a list of vectors where each vector represents a different call to `read-key-sequence' or a command loop read.
If RAW, return keys as in `this-single-command-raw-keys' instead of `this-single-command-keys'.

Requires aggreact-mode to be enabled, since it depends on `read-key-sequence' advice to not lose key sequences.
"
  (when (not aggreact-mode)
    (error "aggreact-this-command-all-keys requires aggreact-mode be enabled"))
  ;; TODO - error when aggreact mode not enabled
  (let ((data (reverse
               (cons (if raw (this-single-command-raw-keys)
                       (this-single-command-keys))
                     (if raw
                         aggreact--current-single-command-raw-key-groups
                       aggreact--current-single-command-key-groups)))))
    (if as-groups
        data
      (apply 'seq-concatenate 'vector data))))


;; (defun aggreact--post-command--single-command-record ()
;;   (let ((data (reverse
;;                (cons (this-single-command-keys)
;;                      aggreact--current-single-command-key-groups))))
;;     (setq aggreact--current-single-command-key-groups nil)
;;     (ring-insert aggreact--ring
;;                  (reverse data))))
;; ;; TODO - allow depth configuration.  The post-command hook clears the current value such that aggreact-this-command-all-keys no longer works, and the value goes into the history ring.
;; (add-hook 'post-command-hook 'aggreact--post-command--single-command-record 99 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record metadata about each command, and group commands.

(setq aggreact--current-groups nil)
(setq aggreact--pre-command-prefix-arg-state nil)

(defun aggreact--pre-command ()
  (setq aggreact--pre-command-prefix-arg-state prefix-arg))

(defvar aggreact-command-group-split-predicate nil
  "Predicate for when to split a command group.
Splits when the predicate returns true.
If the predicate itself is nil instead of a function, always split.
Predicate takes a single argument, which is a (reversed) list of commands so far, as alists of details about the commands.
")

(defvar aggreact-command-history-enrichment-functions nil
  "List of functions for enriching command history.
Each function takes a single argument, the command data alist so far, and returns either nil to make no change, or an alist to extend the history with.
IE if you want to add one field, you can return an alist with the one field, which will be appended to the other alist in the enrichment loop.
")

(defvar aggreact-command-group-split-functions nil
  "List of functions to run once a command group is split (according to 'aggreact-command-group-split-predicate').
Each function receives a single argument: the new command group.
The command group is a list of alists, where each alist contains details of a command in the group.
This is useful to eg. keep histories of commands with interesting properties.
")

(defun aggreact--post-command ()
  (let* ((new-command-details
          `((command . ,this-command)
            ;; TODO - command arguments
            (keys-vectors . ,(aggreact-this-command-all-keys nil t))
            (raw-keys-vectors . ,(aggreact-this-command-all-keys t t))
            (modified-prefix-arg-p . ,(equal aggreact--pre-command-prefix-arg-state
                                             prefix-arg)))))
    (setq new-command-details
          (seq-reduce (lambda (accum enrich-func)
                        (let ((enrich-result
                               (with-demoted-errors
                                   "error during command history enrichment: %s"
                                 (funcall enrich-func accum))))
                          (if enrich-result
                              (append enrich-result accum)
                            accum)))
                      aggreact-command-history-enrichment-functions
                      new-command-details))
    (setq aggreact--current-groups (cons new-command-details aggreact--current-groups))
    (when (if aggreact-command-group-split-predicate
              (funcall aggreact-command-group-split-predicate aggreact--current-groups)
            t)
      (let ((finalized-group (reverse aggreact--current-groups)))
        (setq aggreact--current-groups nil)
        (with-demoted-errors
            "error during aggreact-command-group-split-functions: %s"
          (mapcar (lambda (func) (funcall func finalized-group))
                  aggreact-command-group-split-functions))))))

(defun aggreact--get-keys-for-command-group (command-group &optional keys-type)
  "Return the keys used for the entire COMMAND-GROUP as a flat vector.
KEYS-TYPE can be 'raw, 'single, or nil.
TODO - explanation of difference...
"
  ;; TODO - use keys-type
  (apply 'seq-concatenate 'vector
         (mapcar (lambda (x) (apply 'seq-concatenate 'vector
                                    (cdr (assq 'raw-keys-vectors x))))
                 command-group)))

(defun aggreact-execute-command-group-as-keyboard-macro (command-group)
  "Use the recorded keys of COMMAND-GROUP to execute as keyboard macro.
This may cause issues if not run in a state where the keys will do the same thing...
"
  (execute-kbd-macro (aggreact--get-keys-for-command-group command-group)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor-mode that adds advice and hooks

(define-minor-mode aggreact-mode
  "A minor-mode for recording commands.
It adds advice to `read-key-sequence' so that all keys for a given command can be recorded.
For each command, it records an alist that includes the the keys:
* command - the command executed
* keys-vectors - list of vectors of keys, as from `this-single-command-keys'
* raw-keys-vectors - list of vectors of keys, as from `this-single-command-raw-keys'
* modified-prefix-arg-p - non-nil if the command modified the prefix argument.

Additionally, it uses 'aggreact-command-history-enrichment-functions' to add more keys to the alist.

Commands are also grouped.
The variable 'aggreact-command-group-split-predicate' determines when command groups are split.
When a group is split, the 'aggreact-command-gorup-split-functions' list is run.
You can add a function to the list to eg. keep a list of interesting command groups.

The original motivation behind this mode was to provide command recording and replay that can work for my other packages, estate-mode and composiphrase.
That requires replaying groups of commands, and filtering to decide which groups are interesting for replaying.
See the composiphrase demo config at TODO to see an example setup using aggreact-mode.
"
  :global t
  (if aggreact-mode
      (progn
        (advice-add 'read-key-sequence
                    :before
                    'aggreact--read-key-sequence-advice)
        (advice-add 'read-key-sequence-vector
                    :before
                    'aggreact--read-key-sequence-advice)
        (add-hook 'pre-command-hook 'aggreact--pre-command)
        (add-hook 'post-command-hook 'aggreact--post-command)
        (add-hook 'after-change-functions 'aggreact--after-change)
        )
    (progn
      (advice-remove 'read-key-sequence
                     'aggreact--read-key-sequence-advice)
      (advice-remove 'read-key-sequence-vector
                     'aggreact--read-key-sequence-advice)
      (remove-hook 'pre-command-hook 'aggreact--pre-command)
      (remove-hook 'post-command-hook 'aggreact--post-command)
      (remove-hook 'after-change-functions 'aggreact--after-change)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO - move out -- the below is config for composiphrase + estate demo to have a good repeat key
(setq aggreact--editing-groups-ring (make-ring 100))

(setq aggreact--current-command-modified-buffers nil)
(defun aggreact--after-change (&rest args)
  (when (not (member (current-buffer) aggreact--current-command-modified-buffers))
    (setq aggreact--current-command-modified-buffers
          (cons (current-buffer) aggreact--current-command-modified-buffers))))

(setq aggreact-command-group-split-predicate
      (lambda (reversed-command-group)
        (and (equal estate-state 'normal)
             (equal composiphrase-current-sentence nil)
             (equal aggreact--pre-command-prefix-arg-state prefix-arg)
             )))

(setq aggreact-command-history-enrichment-functions
      (list
       (lambda (x)
         `(
           ;;(estate-state-after-command . ,estate-state)
           ;;(composiphrase-current-sentence-after-command . ,composiphrase-current-sentence)
           (edited-buffer-that-command-ended-in
            .
            ,(let ((result (member (current-buffer)
                                   aggreact--current-command-modified-buffers)))
               (setq aggreact--current-command-modified-buffers nil)
               result))))))
(setq aggreact-command-group-split-functions
      (list
       (lambda (group)
         (let* ((edited-p (seq-find (lambda (cmd) (cdr (assq 'edited-buffer-that-command-ended-in cmd))) group))
                (length-1-command (and (equal (length group) 1)
                                       (cdr (assq 'command (car group)))))
                (single-undo-p (and length-1-command
                                    (member length-1-command
                                            '(undo undo-tree-undo undo-tree-redo)))))
           (when (and edited-p
                      (not single-undo-p))
             (ring-insert aggreact--editing-groups-ring group))))))

(defun aggreact-repeat-latest-editing (&optional count)
  (interactive "p")
  (let ((last-command (ring-ref aggreact--editing-groups-ring 0)))
    (dotimes (i (or count 1))
      (aggreact-execute-command-group-as-keyboard-macro last-command))))



(provide 'aggreact)
