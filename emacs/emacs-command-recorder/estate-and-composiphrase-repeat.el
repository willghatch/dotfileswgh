;; -*- lexical-binding: t; -*-

(require 'this-command-all-keys)

(setq eacsr--current-groups nil)
(setq eacsr--pre-command-prefix-arg-state nil)

(defun eacsr--pre-command ()
  (setq eacsr--pre-command-prefix-arg-state prefix-arg))

(defvar eacsr-group-split-predicate nil
  "Predicate for when to split a command group.
Splits when the predicate returns true.
If the predicate itself is nil instead of a function, always split.
Predicate takes a single argument, which is a (reversed) list of commands so far, as alists of details about the commands.
")

(defvar eacsr-command-history-enrichment-functions nil
  "List of functions for enriching command history.
Each function takes a single argument, the command data alist so far, and returns either nil to make no change, or an alist to extend the history with.
IE if you want to add one field, you can return an alist with the one field, which will be appended to the other alist in the enrichment loop.
")

(defvar eacsr-command-group-split-functions nil
  "List of functions to run once a command group is split (according to 'eacsr-group-split-predicate').
Each function receives a single argument: the new command group.
The command group is a list of alists, where each alist contains details of a command in the group.
This is useful to eg. keep histories of commands with interesting properties.
")

(defun eacsr--post-command ()
  (let* ((new-command-details
          `((command . ,this-command)
            ;; TODO - command arguments
            (keys-vectors . ,(this-command-all-keys nil t))
            (raw-keys-vectors . ,(this-command-all-keys t t))
            (modified-prefix-arg-p . ,(equal eacsr--pre-command-prefix-arg-state
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
                      eacsr-command-history-enrichment-functions
                      new-command-details))
    (setq eacsr--current-groups (cons new-command-details eacsr--current-groups))
    (when (if eacsr-group-split-predicate
              (funcall eacsr-group-split-predicate eacsr--current-groups)
            t)
      (let ((finalized-group (reverse eacsr--current-groups)))
        (setq eacsr--current-groups nil)
        (with-demoted-errors
            "error during eacsr-command-group-split-functions: %s"
          (mapcar (lambda (func) (funcall func finalized-group))
                  eacsr-command-group-split-functions))))))

(defun eacsr--get-keys-for-command-group (command-group &optional keys-type)
  "Return the keys used for the entire COMMAND-GROUP as a flat vector.
KEYS-TYPE can be 'raw, 'single, or nil.
TODO - explanation of difference...
"
  ;; TODO - use keys-type
  (apply 'seq-concatenate 'vector
         (mapcar (lambda (x) (apply 'seq-concatenate 'vector
                                    (cdr (assq 'raw-keys-vectors x))))
                 command-group)))

(defun eacsr-execute-command-group-as-keyboard-macro (command-group)
  "Use the recorded keys of COMMAND-GROUP to execute as keyboard macro.
This may cause issues if not run in a state where the keys will do the same thing...
"
  (execute-kbd-macro (eacsr--get-keys-for-command-group command-group)))



;; TODO - add minor mode that enables/disables hooks
(add-hook 'pre-command-hook 'eacsr--pre-command)
(add-hook 'post-command-hook 'eacsr--post-command)
(add-hook 'after-change-functions 'eacsr--after-change)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO - move out -- the below is config for composiphrase + estate demo to have a good repeat key
(setq eacsr--editing-groups-ring (make-ring 100))

(setq eacsr--current-command-modified-buffers nil)
(defun eacsr--after-change (&rest args)
  (when (not (member (current-buffer) eacsr--current-command-modified-buffers))
    (setq eacsr--current-command-modified-buffers
          (cons (current-buffer) eacsr--current-command-modified-buffers))))

(setq eacsr-group-split-predicate
      (lambda (reversed-command-group)
        (and (equal estate-state 'normal)
             (equal composiphrase-current-sentence nil)
             (equal eacsr--pre-command-prefix-arg-state prefix-arg)
             )))

(setq eacsr-command-history-enrichment-functions
      (list
       (lambda (x)
         `(
           ;;(estate-state-after-command . ,estate-state)
           ;;(composiphrase-current-sentence-after-command . ,composiphrase-current-sentence)
           (edited-buffer-that-command-ended-in
            .
            ,(let ((result (member (current-buffer)
                                   eacsr--current-command-modified-buffers)))
               (and result (message "edited buffers: %s" eacsr--current-command-modified-buffers))
               (setq eacsr--current-command-modified-buffers nil)
               result))))))
(setq eacsr-command-group-split-functions
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
             (ring-insert eacsr--editing-groups-ring group))))))

(defun eacsr-repeat-latest-editing (&optional count)
  (interactive "p")
  (eacsr-execute-command-group-as-keyboard-macro (ring-ref eacsr--editing-groups-ring 0)))

(provide 'estate-and-composiphrase-repeat)
