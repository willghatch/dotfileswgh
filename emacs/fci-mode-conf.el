(require 'fill-column-indicator)
(setq-default fci-rule-color "#153040")
(fci-mode 1) ; this only activates it in *scratch*, my mode hooks add it to others

(setq-default fci-rule-column 80)

(setq fci-handle-truncate-lines nil)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fci-mode-use-p t)
(defun auto-fci-mode (&optional unused)
  (if (and fci-mode-use-p (> (window-width) fci-rule-column))
      (fci-mode 1)
    (fci-mode 0))
  )
(defun fci-mode-toggle ()
  (interactive)
  (setq fci-mode-use-p (not fci-mode-use-p))
  (auto-fci-mode))

(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
(add-hook 'window-configuration-change-hook 'auto-fci-mode)
(add-hook 'window-setup-hook 'auto-fci-mode)

; advise fci-put-overlays-region with fci-delete-overlays-region for line
(defadvice fci-put-overlays-region (after fci-not-current-line activate)
  "Don't put column rule on current line."
  (fci-delete-overlays-region (line-beginning-position 0) (line-beginning-position 1)))


