;; -*- lexical-binding: t; -*-

(require 'this-command-all-keys)

;; Record each command in post-command as an alist, including keys (as a list of vectors), raw-keys (as a list of vectors), edited-p (bool), command (the command executed), and any extra based on user lists

(setq eacsr--current-groups nil)
(setq eacsr--current-groups-modified-buffer-p nil)
(setq eacsr--pre-command-prefix-arg-state nil)

;; TODO ring size config
(setq eacsr--all-groups-ring (make-ring 100))
(setq eacsr--editing-groups-ring (make-ring 100))

(defun eacsr--pre-command ()
  (setq eacsr--pre-command-prefix-arg-state prefix-arg))

(defvar eacsr-group-split-predicate nil
  "Predicate for when to split a command group.
Splits when the predicate returns true.
If the predicate itself is nil instead of a function, always split.
Predicate takes a single argument, which is a (reversed) list of commands so far, as alists of details about the commands.")

(setq eacsr-group-split-predicate
      (lambda (reversed-command-group)
        (and (equal estate-state 'normal)
             (equal composiphrase-current-sentence nil)
             (equal eacsr--pre-command-prefix-arg-state prefix-arg)
             )))

;; TODO - maybe have a predicate for splitting command groups, then this could be something that is part of estate-mode without composiphrase or composiphrase without estate mode or together or whatever.  Also I want to be able to change it dynamically.  Eg. I want a function that sets up recording until some condition is met, then return to the previous/default predicate.  Eg. have a key that means “record all commands until back in normal state with an empty command sentence after an edit has been made”, which would allow chaining some prefix movements to be part of the recording.  This would be slightly less setup than an anonymous keyboard macro (one less key since you don't have to manually stop the recording) and maybe easier to nest with keyboard macros, since there is one level less that you need to keep track of register names for.
;; TODO - keep extra data about the command group, including user-supplied data from hooks.  But specifically include each command, whether each command made edits, key groups (instead of a flat vector, have the list of vectors, and maybe do both raw keys and cooked keys), plus have a list of user functions that each get the command group (with enriched data so far) and return nil or some extra metadata to append.
;; TODO - keep history of command in user-specified rings -- have alists from ring name to predicate on group.
;; TODO - when repeating, it duplicates the command in the ring.  Probably I should filter it.
;; TODO - also I want to be able to filter to reject certain commands.  Eg. I want to filter out undo.
(defun eacsr--post-command ()
  (let* ((new-command-details
          `((command . ,this-command)
            ;; TODO - command arguments
            ;; TODO - things that I want to track that I should do as “user” additions -- estate state, composiphrase-current-sentence state, whether a command was an undo, ...
            (keys-vectors . ,(this-command-all-keys nil t))
            (raw-keys-vectors . ,(this-command-all-keys t t))
            (did-edit-p . ,eacsr--current-groups-modified-buffer-p)
            (modified-prefix-arg-p . ,(equal eacsr--pre-command-prefix-arg-state
                                             prefix-arg)))))
    (setq eacsr--current-groups-modified-buffer-p nil)
    ;; TODO - user enrichment of command
    (setq eacsr--current-groups (cons new-command-details eacsr--current-groups))
    (when (if eacsr-group-split-predicate
              (funcall eacsr-group-split-predicate eacsr--current-groups)
            t)
      (let ((finalized-group (reverse eacsr--current-groups)))
        (ring-insert eacsr--all-groups-ring finalized-group)
        (when (seq-find (lambda (x) (cdr (assq 'did-edit-p x)))
                        eacsr--current-groups)
          (ring-insert eacsr--editing-groups-ring finalized-group)))
      (setq eacsr--current-groups nil))))

(defun eacsr--after-change (&rest args)
  (setq eacsr--current-groups-modified-buffer-p t))

(defun eacsr-get-keys-for-previous-nth-command-group-for-key (group-key n)
  ;; TODO - handle group key
  (apply 'seq-concatenate 'vector
         (mapcar (lambda (x) (apply 'seq-concatenate 'vector
                                    (cdr (assq 'raw-keys-vectors x))))
                 (ring-ref eacsr--editing-groups-ring n))))

(defun eacsr-get-keys-for-previous-nth-command-group (n)
  (eacsr-get-keys-for-previous-nth-command-group-for-key 'all n))

(defun eacsr-get-keys-for-previous-nth-editing-command-group (n)
  (eacsr-get-keys-for-previous-nth-command-group-for-key 'editing n))

(defun eacsr-repeat-latest (&optional count)
  (interactive "p")
  (execute-kbd-macro (eacsr-get-keys-for-previous-nth-command-group 0) count))
(defun eacsr-repeat-latest-editing (&optional count)
  (interactive "p")
  (execute-kbd-macro (eacsr-get-keys-for-previous-nth-editing-command-group 0) count))

;; TODO - add minor mode that enables/disables hooks
(add-hook 'pre-command-hook 'eacsr--pre-command)
(add-hook 'post-command-hook 'eacsr--post-command)
(add-hook 'after-change-functions 'eacsr--after-change)
(provide 'estate-and-composiphrase-repeat)
