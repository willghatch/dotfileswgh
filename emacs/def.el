(setq term-file-aliases
      '(("xterm" . "xterm-256color")
        ("screen" . "xterm-256color")
        ("rackterm" . "xterm-256color")
        ("screen-256color" . "xterm-256color")))

;; macro for more robust starting-up
(defmacro nobreak (&rest args)
  `(progn
     ,@(mapcar (lambda (form)
                 (list 'with-demoted-errors "Error %S" form))
               args)))

(setq dotfileswgh (getenv "DOTFILESWGH"))

(setq package-user-dir (concat dotfileswgh "/dotlocal/emacs.d/elpa"))
;; Set up load path for requires
(let ((default-directory (concat dotfileswgh "/dotlocal/emacs.d")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat (getenv "HOME") "/.emacs.d")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat dotfileswgh "/emacs")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat dotfileswgh "/external/emacs")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat dotfileswgh "/pri/emacs")))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons (concat dotfileswgh "/emacs") load-path))
(setq load-path (cons (concat dotfileswgh "/pri/emacs") load-path))

(defun load-library--around (orig-fun &rest args)
  (let ((curtime (current-time))
        (mylib (car args)))
    (apply orig-fun args)
    (let ((diff (time-to-seconds (time-subtract (current-time) curtime))))
      (when (>= diff 0.005)
        (message (format "load time for %s: %f" mylib diff))))))
(advice-add 'load-library :around #'load-library--around)
(advice-add 'require :around #'load-library--around)

(nobreak (require 'wgh-theme))
(load-theme 'wgh)
(setq custom-file (concat dotfileswgh "/dotlocal/emacs.d/custom-file.el"))
(nobreak (load custom-file))

;; backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.cache/bakmacs"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 8
   kept-old-versions 4
   version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms '((".*" "~/.cache/automacs/\\1" t))) ; auto-saves (## files) here
(make-directory "~/.cache/bakmacs/" t)
(make-directory "~/.cache/automacs/" t)

;; mouse settings
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode) nil) ; shift-click for normal xterm mouse behavior
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode) nil)

;; settings that need to be loaded before their packages
(setq
 evil-intercept-maps nil
 evil-overriding-maps nil
 evil-emacs-state-modes nil
 rainbow-identifiers-faces-to-override '(font-lock-variable-name-face)
 sp-highlight-pair-overlay nil
 sp-show-pair-from-inside t
 bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"
 )

(nobreak
 ;; compile settings
 (setq load-prefer-newer t)
 ;(require 'auto-compile)
 ;(auto-compile-on-load-mode 1)
 ;(auto-compile-on-save-mode 1)

 (setq vc-follow-symlinks t) ; Don't prompt to follow symlinks of version-controlled files
 (setq-default tab-width 4)
 (setq-default indent-tabs-mode nil) ; use spaces, not tabs
 (setq tab-stop-list (number-sequence 4 120 4)) ; setting tab expansion by setting stops
 (defun set-indent-auto (indent-p)
   "Set whether to indent when you hit enter"
   (interactive)
   (if indent-p
       (global-set-key (kbd "RET") 'newline-and-indent)
     (global-set-key (kbd "RET") 'newline)))
 (set-indent-auto t)

 (setq echo-keystrokes 0.01) ; echo keystrokes faster than default 1s

 (setq c-default-style "k&r"
       c-basic-offset 4)
 (setq org-startup-folded nil)
 (global-auto-revert-mode t) ; auto-reload files when they change on disk

 (menu-bar-mode -1) ; no menu-bar
 (tool-bar-mode -1)
 (setq inhibit-splash-screen t)
 (setq inhibit-startup-message t)
 )

;; keys, keys, keys!!!
(nobreak
 ;;; these are the most critical loads
 (require 'evil)
 (evil-mode 1)
 (require 'key-chord)
 (key-chord-mode 1)
 (require 'hydra)
 (setq hydra-lv nil)
 (load-library "keys")
 (load-library "vfuncs")
 (setq repeatable-motion-count-needed-prefix "rmo-c/")
 (setq repeatable-motion-definition-prefix "rmo/")
 (require 'repeatable-motion)
 )

(nobreak
 ;;; secondarily important
 (load-library "xclip-conf")
 (unless (display-graphic-p)
   (require 'evil-terminal-cursor-changer))

 ;; elscreen must start before other mode-line stuff, or it wouldn't be this high...
 (require 'elscreen)
 (setq elscreen-display-tab nil)
 (elscreen-start)
 (load-library "modeline-conf")
 ;; reset the header line in initial buffer, which gets messed up by elscreen
 (setq header-line-format (default-value 'header-line-format))

 )


(nobreak
 (load-library "ace-jump-mode-conf")
 (require 'evil-little-word)
 ;(require 'evil-args) ; autoloaded
 (require 'evil-surround)
 (global-evil-surround-mode 1)
 (require 'evil-textobj-between)
 (require 'evil-search-highlight-persist)
 (global-evil-search-highlight-persist 1)
 (require 'on-parens)

 (load-library "ido-conf")
 (ido-mode 1)
 (setq ido-enable-flex-matching t
       ido-everywhere t)
 (require 'flx-ido)
 (flx-ido-mode 1)

 ;(require 'linum)
 (global-linum-mode 1) ; add line numbers
 (require 'hlinum)
 (hlinum-activate)
 ;(require 'linum-relative) ;; I don't ever use this...
 ;(linum-relative-toggle) ; turn it off as the default

 ;(require 'smex) ; autoloaded
 ;(require 'rainbow-delimiters) ; autoloaded
 (load-library "package-conf")
 (load-library "yasnippet-conf")
 (load-library "org-mode-conf")
 (setq fill-column 80)
 (load-library "fci-mode-conf")
 (load-library "mode-hooks-conf")
 (load-library "company-conf")
 (load-library "auto-complete-conf")
 (load-library "hippie-expand-conf")
 (load-library "popwin-conf")
 ;(load-library "projectile-conf")
 (load-library "scratch-message")
 ;(winner-mode 1)
 (load-library "smartparens-conf")
 (show-smartparens-global-mode 1)
 ;(require 'yafolding)
 (yafolding-mode 1)
 (load-library "borrowed")
 (load-library "tty-format")
 (setq scroll-margin 5)
 ;(setq smooth-scroll-margin 5)
 ;(require 'smooth-scrolling)
 ;(require 'indent-guide)
 (setq indent-guide-recursive t)
 (setq indent-guide-delay 0.2)
 (load-library "windows")
 (setq hl-todo-activate-in-modes '(prog-mode))
 ;(require 'hl-todo)
 (global-hl-todo-mode 1)
 ;(require 'helm)
 (setq helm-swoop-pre-input-function (lambda () "")) ; disable symbol-at-point nonsense
 (global-anzu-mode 1)
 (setq guide-key/guide-key-sequence '("SPC"))
 (setq guide-key/recursive-key-sequence-flag t)
 (guide-key-mode 1)
)

(let ((file (concat dotfileswgh "/dotlocal/emacs")))
  (if (file-exists-p file) (load-file file) nil))

(message "-")
(message "--")
(message "---")
(message (format "start time: %f" (time-to-seconds (time-subtract (current-time) before-init-time))))


