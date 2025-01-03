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

(let ((conf-dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "./env-conf.el" conf-dir)))

(nobreak
 (setq package-user-dir (concat local-emacs.d-path "elpa/"))
 ;; Set up load path for requires
 ;;(let ((default-directory local-emacs.d-path))
 ;;  (normal-top-level-add-subdirs-to-load-path))
 ;;(let ((default-directory "/usr/share/emacs/site-lisp/"))
 ;;  (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh "emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh "external/emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh-pri "emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh-dotlocal "emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (setq load-path (cons (concat local-emacs.d-path "single-files/") load-path))
 (setq load-path (cons (concat dotfileswgh "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-pri "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-dotlocal "emacs/") load-path))
 ;; Add straight build dirs, so that emacs can search in them, rather than using
 ;; straight on every emacs load.  It won't process autoloads, but I can wrap
 ;; key bindings with requires or otherwise force autoload evaluation for things
 ;; that I care about.
 (let ((default-directory (concat straight-base-dir "straight/build")))
   (normal-top-level-add-subdirs-to-load-path))
 )

(defun load-library--around (orig-fun &rest args)
  (let ((curtime (current-time))
        (mylib (car args)))
    (apply orig-fun args)
    (let ((diff (time-to-seconds (time-subtract (current-time) curtime))))
      (when (>= diff 0.005)
        (message (format "load time for %s: %f" mylib diff))))))

(nobreak
 (advice-add 'load-library :around #'load-library--around)
 (advice-add 'require :around #'load-library--around))

(nobreak (require 'wgh-theme))
(setq custom-file (concat local-emacs.d-path "custom-file.el"))
(nobreak (load custom-file))


;; backup settings
(nobreak (load-library "sensitive-mode"))
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.cache/emacs/bak"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 8
   kept-old-versions 4
   version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto/\\1" t))) ; auto-saves (## files) here
(make-directory "~/.cache/emacs/bak/" t)
(make-directory "~/.cache/emacs/auto/" t)
;; For now, just don't make backups -- I haven't needed them since I started using git everywhere.
(setq make-backup-files nil)

;; mouse settings
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode) nil) ; shift-click for normal xterm mouse behavior
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode) nil)

;; settings that need to be loaded before their packages
(setq
 evil-intercept-maps nil
 evil-overriding-maps nil
 evil-emacs-state-modes nil
 evil-undo-system 'undo-tree
 rainbow-identifiers-faces-to-override '(font-lock-variable-name-face
                                         font-lock-builtin-face
                                         font-lock-keyword-face
                                         font-lock-function-name-face)
 sp-highlight-pair-overlay nil
 sp-show-pair-from-inside t
 bmkp-last-as-first-bookmark-file (concat local-emacs.d-path "bookmarks")
 )

(nobreak
 ;; compile settings
 (setq load-prefer-newer t)
 ;(require 'auto-compile)
 ;(auto-compile-on-load-mode 1)
 ;(auto-compile-on-save-mode 1)

 (setq vc-follow-symlinks t) ; Don't prompt to follow symlinks of version-controlled files
 (setq-default tab-width 2)
 (setq-default indent-tabs-mode nil) ; use spaces, not tabs
 (setq tab-stop-list (number-sequence 2 120 2)) ; setting tab expansion by setting stops
 (defun set-indent-auto (indent-p)
   "Set whether to indent when you hit enter"
   (interactive)
   (if indent-p
       (global-set-key (kbd "RET") 'newline-and-indent)
     (global-set-key (kbd "RET") 'newline)))
 (set-indent-auto t)

 (setq echo-keystrokes 0.01) ; echo keystrokes faster than default 1s

 (setq c-default-style "k&r"
       c-basic-offset 2)
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
 (load-library "init-helpers")

 ;(require 'evil)
 ;(evil-mode 1)

 ;; Don't litter with undo-tree history files everywhere.
 ;; But maybe I should also look for how to put them in my emacs cache dir?
 ;; Persistent undo would be useful, but also annoying for when I really want to just get a file to its original state when I opened it.
 (setq undo-tree-auto-save-history nil)
 (require 'undo-tree)
 (global-undo-tree-mode 1)
 (require 'hydra)
 (require 'mouse)
 (require 'mwheel)
 (setq hydra-lv nil)
 (load-library "scribble-funcs")
 (setq repeatable-motion-count-needed-prefix "rmo-c/")
 (setq repeatable-motion-definition-prefix "rmo/")
 (setq repeatable-motion-training-wheels-p nil)
 (require 'repeatable-motion)
 (load-library "vfuncs")
 (load-library "keys")
 (setq evil-motion-state-local-map (make-sparse-keymap)) ;; To prevent breakage where this is still used.  TODO - delete this.

 ;; I wanted to try one more time to see if maybe I should just use evil-mode with some configuration changes.  But it's too set in its on-character addressing ways.  The evil-move-cursor-back setting only affects what happens when exiting insert mode.  It doesn't change the fact that evil-forward-word-end puts the cursor ON the last character (IE before it) instead of after the last character, and that it has a bunch of logic for adjusting the ranges used for operations (eg. delete forward word end deletes one character farther than the movement goes).  And I think working my full plans in with evil-mode will be difficult, at best.  And the main parts of evil-mode that I really care about that don't conflict with how evil-mode is set up have been relatively easy to re-implement in a much smaller way.  The remaining parts that I care about I can mostly just keep using from evil-mode as a lazy require.
 ;(setq evil-move-cursor-back nil)
 ;(setq evil-move-beyond-eol t)
 ;(load-library "keys-old-evil-mode")
 )

(nobreak
 ;;; secondarily important
 (load-library "xclip-conf")
 (unless (display-graphic-p)
   (setq evil-normal-state-cursor 'box); █
   (setq evil-visual-state-cursor 'box); █
   (setq evil-insert-state-cursor 'bar); ⎸
   (setq evil-emacs-state-cursor 'hbar); _
   ;; evil-terminal-cursor-changer makes the font size reset (at least in Konsole)...
   ;; it's clearly a bug, and has annoyed me for a long time.  Let's not use it until it is fixed.
   ;(require 'evil-terminal-cursor-changer)
   )

 ;; elscreen must start before other mode-line stuff, or it wouldn't be this high...
 ;; Elscreen is for making tabs tike I used to use in Vim... but I haven't used it for a long time, so let's disable it.
 ;(require 'elscreen)
 ;(setq elscreen-display-tab nil)
 ;(elscreen-start)
 (load-library "modeline-conf")
 ;; reset the header line in initial buffer, which gets messed up by elscreen
 (setq header-line-format (default-value 'header-line-format))

 )


(nobreak
 ;;(load-library "ace-jump-mode-conf")
 ;(require 'evil-little-word)
 ;;(require 'evil-args) ; autoloaded
 ;(require 'evil-surround)
 ;(global-evil-surround-mode 1)
 ;;(require 'evil-cleverparens-text-objects)
 ;(require 'evil-textobj-between)
 ;(require 'evil-textobj-anyblock)
 ;(require 'on-parens)

 (load-library "ido-conf")
 (ido-mode 1)
 (setq ido-enable-flex-matching t
       ido-everywhere t)
 (require 'flx-ido)
 (flx-ido-mode 1)

 ;; don't shrink the line number width
 (setq display-line-numbers-grow-only t)
 (global-display-line-numbers-mode)

 ;; guide on disabling/enabling lsp-mode features: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
 (setq lsp-headerline-breadcrumb-enable nil)

 ;(require 'smex) ; autoloaded
 ;(require 'rainbow-delimiters) ; autoloaded

 ;;(load-library "package-conf")
 ;;(load-library "straight-install")

 ;(load-library "yasnippet-conf")
 (require 'indent-tree)
 (setq wgh/lisp-outline-regexp
       (rx (or
            ;; Allow headings to be after comment characters, or anywhere in a blank indented line.
            (seq (* blank) (* ";") (* blank) (+ "*"))
            )))
 (setq wgh/c-outline-regexp
       (rx (or
            ;; Allow headings to be after comment characters, or anywhere in a blank indented line.
            (seq (* blank) (? (seq "//" (* "/"))) (* blank) (+ "*"))
            )))
 (setq wgh/bash-outline-regexp
       (rx (or
            ;; Allow headings to be after comment characters, or anywhere in a blank indented line.
            (seq (* blank) (* "#") (* blank) (+ "*"))
            )))
 (setq wgh/lua-outline-regexp
       (rx (or
            ;; Allow headings to be after comment characters, or anywhere in a blank indented line.
            (seq (* blank) (? (seq "--" (* "-"))) (* blank) (+ "*"))
            )))
 (load-library "org-mode-conf")
 (setq-default fill-column 80)
 (global-display-fill-column-indicator-mode 1)
 (load-library "lsp-conf")
 (load-library "mode-hooks-conf")
 (load-library "racket-mode-conf")
 (load-library "haskell-mode-conf")
 (load-library "company-conf")
 ;(load-library "auto-complete-conf")
 (load-library "hippie-expand-conf")
 ;(load-library "popwin-conf")
 ;(load-library "projectile-conf")
 (load-library "scratch-message")
 ;(winner-mode 1)
 (load-library "delimiters-conf")
 (load-library "sexprw-conf")
 (show-smartparens-global-mode 1)
 ;(require 'yafolding)
 ;(yafolding-mode 1)
 (load-library "borrowed")
 ;; tty-format provides for coloring based on terminal escape codes.  I should set it to autoload at some point, but for now let's disable it.
 ;(load-library "tty-format")
 ;(require 'markchars)
 ;(markchars-global-mode t)
 ;; highlight literal tab characters and trailing whitespace
 (setq whitespace-style '(face tabs trailing))

 ;;; MAKE SCROLLING BE SANE, PLEASE¡
 (setq scroll-step 1)
 (setq scroll-conservatively 10000)
 (setq scroll-margin 5)
 (setq smooth-scroll-margin 5)
 (setq scroll-up-aggressively 0.0)
 (setq-default scroll-up-aggressively 0.0)
 (setq scroll-down-aggressively 0.0)
 (setq-default scroll-down-aggressively 0.0)
 (require 'smooth-scrolling)

 (setq highlight-indent-guides-auto-enabled nil)
 (load-library "windows")
 (setq hl-todo-activate-in-modes '(prog-mode))
 ;(require 'hl-todo)
 ;(global-hl-todo-mode 1)
 ;(require 'helm)
 (load-library "helm-autoloads")
 (setq helm-swoop-pre-input-function (lambda () "")) ; disable symbol-at-point nonsense
 ;(global-anzu-mode 1)
 (setq guide-key/guide-key-sequence '("SPC"))
 (setq guide-key/recursive-key-sequence-flag t)
 (require 'guide-key)
 (guide-key-mode 1)
 ;(global-git-commit-mode 1)
 (setq whitespace-final-newline-message "\n<-- No final newline")
 (require 'whitespace-final-newline)
 (global-whitespace-final-newline-mode 1)

 ;(load-library "keyfreq-conf")
)

(let ((file (concat dotfileswgh-pri "emacs/def.el")))
  (if (file-exists-p file) (load-file file) nil))
(let ((file (concat dotfileswgh-dotlocal "emacs/def.el")))
  (if (file-exists-p file) (load-file file) nil))


(nobreak
 (if (string-match "light"
                   (shell-command-to-string "lightdark-status"))
     (light-theme)
   (dark-theme)))
(nobreak (lightdark-update-theme-watch))

(nobreak
 ;; I recently started using Wezterm which supports the csi-u (also known as modify-other-keys) terminal protocol, which encodes keys to disambiguate things.  But it screws up handling of my Hatchak keyboard, so I get garbage when I type eg. shift+space (which I want to just send space) or any keys that take shift plus iso_level_3_shift (IE shift level 4).  So I want to simply disable it.
 (defun disable-csi-u-key-encoding ()
   (let* ((term-params (terminal-parameter nil 'tty-mode-set-strings))
          (csi-u-place (member "\e[>4;1m" term-params)))
     (when csi-u-place
       ;; First, disable future enabling of the csi-u protocol by removing the string from the set-strings.
       (setcar csi-u-place "")
       ;; Then send the terminal code to disable it immediately.
       (send-string-to-terminal "\e[>4;0m")
       )))
 (disable-csi-u-key-encoding))

(message "-")
(message "--")
(message "---")
(message (format "start time: %f" (time-to-seconds (time-subtract (current-time) before-init-time))))


