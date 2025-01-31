;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record all keys for each command.

(setq aggreact--current-command-key-groups nil)
(setq aggreact--current-single-command-key-groups nil)
(setq aggreact--current-single-command-raw-key-groups nil)

(defun aggreact--read-key-sequence-advice (&rest args)
  (setq aggreact--current-single-command-raw-key-groups
        (cons (this-single-command-raw-keys)
              aggreact--current-single-command-raw-key-groups))
  (setq aggreact--current-single-command-key-groups
        (cons (this-single-command-keys)
              aggreact--current-single-command-key-groups))
  (setq aggreact--current-command-key-groups
        (cons (this-command-keys)
              aggreact--current-command-key-groups)))

(defun aggreact-this-command-all-keys (cook-style as-groups)
  "Return a vector of keys used for this command, including any uses of `read-key-sequence', at least up to the point where this function is used.
Returns a vector, or if AS-GROUPS is non-nil, return a list of vectors where each vector represents a different call to `read-key-sequence' or a command loop read.
COOK-STYLE can be 'raw, 'single, or 'tck to receive keys as in `this-single-command-raw-keys', `this-single-command-keys', or `this-command-keys', respectively.

Requires aggreact-mode to be enabled, since it depends on `read-key-sequence' advice to not lose key sequences.
"
  (when (not aggreact-mode)
    (error "aggreact-this-command-all-keys requires aggreact-mode be enabled"))
  (let ((data (reverse
               (cons (cond ((eq cook-style 'raw) (this-single-command-raw-keys))
                           ((eq cook-style 'single) (this-single-command-keys))
                           ((eq cook-style 'tck) (this-command-keys))
                           (t (error "bad value for cook-style: %s" cook-style)))
                     (cond ((eq cook-style 'raw) aggreact--current-single-command-raw-key-groups)
                           ((eq cook-style 'single) aggreact--current-single-command-key-groups)
                           ((eq cook-style 'tck) aggreact--current-command-key-groups)
                           (t (error "bad value for cook-style: %s" cook-style)))))))
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
            (keys-vectors . ,(aggreact-this-command-all-keys 'tck t))
            (single-keys-vectors . ,(aggreact-this-command-all-keys 'single t))
            (raw-keys-vectors . ,(aggreact-this-command-all-keys 'raw t)))))
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

(defun aggreact--get-keys-for-command-group (command-group)
  "Return the raw keys used for the entire COMMAND-GROUP as a flat vector."
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
        (add-hook 'post-command-hook 'aggreact--post-command)
        )
    (progn
      (advice-remove 'read-key-sequence
                     'aggreact--read-key-sequence-advice)
      (advice-remove 'read-key-sequence-vector
                     'aggreact--read-key-sequence-advice)
      (remove-hook 'post-command-hook 'aggreact--post-command)
      )))


(provide 'aggreact)
