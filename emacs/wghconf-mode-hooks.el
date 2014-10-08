
(add-hook 'Buffer-menu-mode-hook
          (lambda ()
            (define-prefix-command 'my-buffer-menu-mode-map)
            (define-key evil-motion-state-local-map "m" 'my-buffer-menu-mode-map)
            (set-keymap-parent 'my-buffer-menu-mode-map Buffer-menu-mode-map)))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; These two highlighters kill the formatting of
            ;; list-faces-display... and I don't know why
            (hc-toggle-highlight-tabs)
            (hc-toggle-highlight-trailing-whitespace)
            (rainbow-delimiters-mode-enable)
            ))
(add-hook 'python-mode-hook
          (lambda ()
            (electric-indent-mode -1)
            ))
(add-hook 'js-mode-hook
          (lambda ()
            (load-library "wghconf-js")
            ))

(defun add-to-hooks (fun hooklist)
  (mapcar (lambda (hook) (add-hook hook fun)) hooklist))


;; TODO -- these are default values that I should deal with now
;evil-overriding-maps
;Value: ((Buffer-menu-mode-map)
 ;(color-theme-mode-map)
 ;(comint-mode-map)
 ;(compilation-mode-map)
 ;(grep-mode-map)
 ;(dictionary-mode-map)
 ;(ert-results-mode-map . motion)
 ;(Info-mode-map . motion)
 ;(speedbar-key-map)
 ;(speedbar-file-key-map)
 ;(speedbar-buffers-key-map))
;
;evil-intercept-maps
;edebug-mode-map


