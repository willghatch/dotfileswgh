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

(add-hook 'hexl-mode-hook
          (lambda ()
            ;; These first two are in the default hook, but add-hook seems to be
            ;; replacing rather than adding...
            (hexl-follow-line)
            (hexl-activate-ruler)
            (display-line-numbers-mode 0)))

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
            (company-mode 1)
            ;(projectile-mode 1)
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

(add-hook 'java-mode-hook
          (lambda ()
            (setq lsp-headerline-breadcrumb-enable nil)
            (require 'lsp-mode)
            (require 'lsp-java)
            ;; This lsp download url should work with openjdk11.
            (setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
            (lsp)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit

(defun wgh/magit-keys-setup ()
  ;; set up some keymaps that don't cleanly belong to a particular mode
  ;; TODO - do some checks to only run this once, some time after magit loads.
  ;; TODO - I should figure out what interesting stuff the default map holds,
  ;; but bind it to keys I'll use in my keymap that don't override with my motion
  ;; keys, etc.
  ;; Don't override my evil-mode keymap with your default bindings.
  (setcdr magit-hunk-section-map nil)
  (setcdr magit-file-section-map nil)
  )

(add-hook
 'magit-revision-mode-hook
 (lambda ()
   ;; Don't override my evil-mode keymap with your default bindings.
   (setcdr magit-revision-mode-map nil)
   (wgh/magit-keys-setup)
  ))
(add-hook
 'magit-diff-mode-hook
 (lambda ()
   ;; Don't override my evil-mode keymap with your default bindings.
   (setcdr magit-diff-mode-map nil)
   (wgh/magit-keys-setup)
  ))

(add-hook
 'magit-log-mode-hook
 (lambda ()
   ;; Don't override my evil-mode keymap with your default bindings.
   (load-library "magit-conf")
   (setcdr magit-log-mode-map nil)
   (setq magit-log-margin
         (list
          ;; show margin initially
          t
          ;; time as a string for format-time-string, or 'age or 'age-abbreviated
          ;;"%Y-%m-%d@%H:%M %z"
          ;; Less precise, but shorter
          "%Y-%m-%d@%H:%M"
          ;; “don't change”
          'magit-log-margin-width
          ;; show author
          t
          ;; author width, default 18
          12
          ))
   ;; TODO - what operations do I want handy in log view?
   ;; • delete remote branch (with confirmation)
   ;; • delete remote tag (with confirmation)
   ;; • push branch/tag to remote
   ;; • cherry pick commit onto current branch
   ;; • diff selected commit vs current branch (or HEAD)
   ;; • view commit details
   ;; • merge selected commit/branch into current branch
   ;; • copy commit hash
   ;; • mark commit to use as “current” commit for other operations, like diff with commit at point.  I can do this by checking out a detatched head at point and then doing the diff, but a mark without checking out would be nice.
   ;; • stash operations
   (setq wgh/magit-log-m-keymap (make-sparse-keymap))
   (define-key magit-log-mode-map "m" wgh/magit-log-m-keymap)
   (define-key magit-log-mode-map "C-l" 'magit-refresh)
   (define-key magit-log-mode-map (kbd "RET") 'magit-show-commit)
   (define-key wgh/magit-log-m-keymap "v" 'magit-show-commit)
   (define-key wgh/magit-log-m-keymap "co" 'wgh/magit-checkout-at-point)
   (define-key wgh/magit-log-m-keymap "bo" 'wgh/magit-branch-checkout-at-point)
   (define-key wgh/magit-log-m-keymap "bc" 'wgh/magit-branch-create-at-point)
   (define-key wgh/magit-log-m-keymap "bd" 'wgh/magit-branch-delete-at-point)
   (define-key wgh/magit-log-m-keymap "bn" 'wgh/magit-branch-rename-at-point)
   (define-key wgh/magit-log-m-keymap "tc" 'wgh/magit-tag-create-at-point)
   ;;(define-key wgh/magit-log-m-keymap "td" 'wgh/magit-tag-delete-at-point)
   (define-key wgh/magit-log-m-keymap "brs" 'wgh/magit-current-branch-reset-soft-at-point)
   (define-key wgh/magit-log-m-keymap "brh" 'wgh/magit-current-branch-reset-hard-at-point)
   (define-key wgh/magit-log-m-keymap "brm" 'wgh/magit-current-branch-reset-mixed-at-point)
   ))
;
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


(define-derived-mode wgh-checklist-mode text-mode "wgh-checklist-mode"
  (nkmap "m" 'wgh/increment-number-at-end-of-line))
(add-to-list 'auto-mode-alist '("\\.checklist" . wgh-checklist-mode))

