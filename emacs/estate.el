;; TODO - make state definition macro, clean up to be “publishable”, add current state variable and indicator, how to deal with repeating commands (should it be integrated or separate), ...

(setq -estate-original-global-map (current-global-map))
(setq -estate-mode-current-keymap (make-sparse-keymap))

(define-minor-mode estate-local-mode
  "TODO docstring here..."
  :lighter " estate "
  :keymap -estate-mode-current-keymap
  (estate-command-state)
  )
(defun -estate-mode-initialize ()
  (unless (minibufferp)
    ;; TODO - maybe I should make a minibuffer-specific map instead?
    (estate-local-mode 1)))
(define-globalized-minor-mode estate-mode
  estate-local-mode -estate-mode-initialize)

(setq estate-motion-keymap (make-sparse-keymap))
(suppress-keymap estate-motion-keymap)
(setq estate-command-keymap (make-sparse-keymap))
(set-keymap-parent estate-command-keymap estate-motion-keymap)
(setq estate-visual-keymap (make-sparse-keymap))
(set-keymap-parent estate-visual-keymap estate-motion-keymap)
(setq estate-pager-keymap (make-sparse-keymap))
(set-keymap-parent estate-pager-keymap estate-motion-keymap)
(setq estate-insert-keymap (make-sparse-keymap))
(set-keymap-parent estate-insert-keymap -estate-original-global-map)

;; Let's set one key here so the estate keymap isn't a death trap if not configured further.
(define-key estate-command-keymap "i" 'estate-insert-state)

(setq estate-state nil)

(defun estate-insert-state ()
  (interactive)
  (setq estate-state 'insert)
  (set-keymap-parent -estate-mode-current-keymap estate-insert-keymap)
  )
(defun estate-command-state ()
  (interactive)
  (setq estate-state 'command)
  (set-keymap-parent -estate-mode-current-keymap estate-command-keymap)
  )
(defun estate-visual-state ()
  (interactive)
  (setq estate-state 'visual)
  (set-keymap-parent -estate-mode-current-keymap estate-visual-keymap)
  )


;(set-keymap-parent -estate-mode-current-keymap estate-command-keymap)

(provide 'estate)
