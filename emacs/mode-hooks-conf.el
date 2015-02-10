(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'Buffer-menu-mode-hook
          (lambda ()
            (define-prefix-command 'my-buffer-menu-mode-map)
            (define-key evil-motion-state-local-map "m" 'my-buffer-menu-mode-map)
            (set-keymap-parent 'my-buffer-menu-mode-map Buffer-menu-mode-map)))

;(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (whitespace)
            (rainbow-delimiters-mode-enable)
            (rainbow-identifiers-mode 1)
            (company-mode 1)
            (projectile-mode 1)
            ))
(add-hook 'python-mode-hook
          (lambda ()
            (electric-indent-mode -1)
            ))
(add-hook 'js-mode-hook
          (lambda ()
            (load-library "js-conf")
            ))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            ))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key evil-motion-state-local-map "gdd" 'elisp-slime-nav-find-elisp-thing-at-point)
            (define-key evil-motion-state-local-map "gdp" 'pop-tag-mark)
            (define-key evil-motion-state-local-map "gD" 'elisp-slime-nav-describe-elisp-thing-at-point)
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


