;; -*- lexical-binding: t; -*-

(require 'this-command-all-keys)

(setq eacsr--current-groups nil)
(setq eacsr--current-groups-modified-buffer-p nil)
(setq eacsr--pre-command-prefix-arg-state nil)

;; TODO ring size config
(setq eacsr--all-groups-ring (make-ring 100))
(setq eacsr--editing-groups-ring (make-ring 100))

(defun eacsr--pre-command ()
  (setq eacsr--pre-command-prefix-arg-state prefix-arg))

;; TODO - maybe have a predicate for splitting command groups, then this could be something that is part of estate-mode without composiphrase or composiphrase without estate mode or together or whatever.  Also I want to be able to change it dynamically.  Eg. I want a function that sets up recording until some condition is met, then return to the previous/default predicate.  Eg. have a key that means “record all commands until back in normal state with an empty command sentence after an edit has been made”, which would allow chaining some prefix movements to be part of the recording.  This would be slightly less setup than an anonymous keyboard macro (one less key since you don't have to manually stop the recording) and maybe easier to nest with keyboard macros, since there is one level less that you need to keep track of register names for.
;; TODO - when repeating, it duplicates the command in the ring.  Probably I should filter it.
;; TODO - also I want to be able to filter to reject certain commands.  Eg. I want to filter out undo.
(defun eacsr--post-command ()
  (setq eacsr--current-groups (cons (this-command-all-keys) eacsr--current-groups))
  (when (and (equal estate-state 'normal)
             (equal composiphrase-current-sentence nil)
             (equal eacsr--pre-command-prefix-arg-state prefix-arg)
             )
    (let ((finalized-group (reverse eacsr--current-groups)))
      (ring-insert eacsr--all-groups-ring finalized-group)
      (when eacsr--current-groups-modified-buffer-p
        (ring-insert eacsr--editing-groups-ring finalized-group)))
    (setq eacsr--current-groups nil)
    (setq eacsr--current-groups-modified-buffer-p nil)))

(defun eacsr--after-change (&rest args)
  (setq eacsr--current-groups-modified-buffer-p t))

(defun eacsr-get-keys-for-previous-nth-command-group (n)
  (apply 'seq-concatenate 'vector (ring-ref eacsr--all-groups-ring n)))
(defun eacsr-get-keys-for-previous-nth-editing-command-group (n)
  (apply 'seq-concatenate 'vector (ring-ref eacsr--editing-groups-ring n)))

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
