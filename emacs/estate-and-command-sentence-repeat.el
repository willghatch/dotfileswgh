;; -*- lexical-binding: t; -*-

(require 'this-command-all-keys)

(setq eacsr--current-groups nil)
(setq eacsr--current-groups-modified-buffer-p nil)
(setq eacsr--pre-command-prefix-arg-state nil)

;; TODO ring size config
(setq eacsr--all-groups-ring (make-ring 100))
(setq eacsr--editing-groups-ring (make-ring 100))

(defun eacsr--pre-command ()
  (setq eacsr--pre-command-prefix-arg-state current-prefix-arg))

;; TODO - maybe have a predicate for splitting command groups, then this could be something that is part of estate-mode without command sentence or command-sentence without estate mode or together or whatever.
;; TODO - when repeating, it duplicates the command in the ring.  Probably I should filter it.
;; TODO - also I want to be able to filter to reject certain commands.  Eg. I want to filter out undo.
(defun eacsr--post-command ()
  (setq eacsr--current-groups (cons (this-command-all-keys) eacsr--current-groups))
  (when (and (equal estate-state 'normal)
             (equal command-sentence-current-sentence nil)
             ;; TODO - I want to include commands that modify the prefix argument with the overall command, but I'm having trouble identifying which commands do that...
             ;;(equal eacsr--pre-command-prefix-arg-state current-prefix-arg)
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
(provide 'estate-and-command-sentence-repeat)
