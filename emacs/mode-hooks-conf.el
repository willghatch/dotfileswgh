(defun add-to-hooks (fun hooklist)
  (mapcar (lambda (hook) (add-hook hook fun)) hooklist))

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.install" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.service" . systemd-mode))
(add-to-list 'auto-mode-alist '("\\.socket" . systemd-mode))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defun sensitive-if-prifs ()
  (let ((real-name (and (buffer-file-name)
                        (car (process-lines "readlink" "-f" (buffer-file-name))))))
    (cond
     ((not real-name) nil)
     ((string-match "/prifs/" real-name) (sensitive-mode 1))
     ((string-match "/prifsb/" real-name) (sensitive-mode 1))
     (t nil))))

(add-hook 'find-file-hook 'sensitive-if-prifs)

(add-hook 'Buffer-menu-mode-hook
          (lambda ()
            (define-prefix-command 'my-buffer-menu-mode-map)
            (define-key evil-motion-state-local-map "m" 'my-buffer-menu-mode-map)
            (set-keymap-parent 'my-buffer-menu-mode-map Buffer-menu-mode-map)))

;(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (smartparens-mode 1)
            (rainbow-delimiters-mode-enable)
            (rainbow-identifiers-mode 1)
            (whitespace-mode 1)
            ;(company-mode 1)
            ;(projectile-mode 1)
            ))
(defun racket-close-paren-hack (&optional arg)
  (interactive "p")
  (if (on-parens-on-close?)
      (self-insert-command arg)
    (racket-insert-closing)))
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map "[" nil)
            (define-key racket-mode-map ")" 'racket-close-paren-hack)
            (define-key racket-mode-map "]" nil)
            (define-key racket-mode-map "}" nil)
            ))
(add-hook 'python-mode-hook
          (lambda ()
            (electric-indent-mode -1)
            ))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (require 'sgml-mode)
            ))

(add-to-hooks
 (lambda ()
   (define-key evil-motion-state-local-map "gdd" 'elisp-slime-nav-find-elisp-thing-at-point)
   (define-key evil-motion-state-local-map "gdp" 'pop-tag-mark)
   (define-key evil-motion-state-local-map "gD" 'elisp-slime-nav-describe-elisp-thing-at-point)
   (eldoc-mode 1)
   )
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Don't let dired override evil.  This is the easiest way to do that,
            ;; since I don't actually use dired for anything.
            (setcdr dired-mode-map nil)
            ))

;; so it doesn't barf when it's not set!
(setq start-on-pager-state nil)
(add-hook 'server-switch-hook
          (lambda ()
            (when start-on-pager-state
                (progn
                  (evil-pager-state)
                  (pkmap "q" 'kill-buffer-or-quit-emacs-ignore-dirty)
                  (pkmap "tic" 'kill-buffer-or-quit-emacs-ignore-dirty)
                  (ansi-color-buffer)
                  ))))

 (load-library "js-conf")

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


