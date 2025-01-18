;; Setting a higher gc threshold makes startup way faster
(setq wgh/orig-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)

(setq term-file-aliases
      '(("xterm" . "xterm-256color")
        ("screen" . "xterm-256color")
        ("rackterm" . "xterm-256color")
        ("screen-256color" . "xterm-256color")))

;; Silence warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)


;; macro for more robust starting-up
(defmacro nobreak (&rest args)
  `(progn
     ,@(mapcar (lambda (form)
                 (list 'with-demoted-errors "Error %S" form))
               args)))

(let ((conf-dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "./dotfileswgh-env-conf.el" conf-dir)))

(nobreak
 (setq package-user-dir (concat local-emacs.d-path "elpa/"))
 ;; Set up load path for requires
 (let ((default-directory (concat dotfileswgh "emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh "external/emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh-pri "emacs/")))
   (normal-top-level-add-subdirs-to-load-path))
 (let ((default-directory (concat dotfileswgh-dotlocal "emacs/")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
 (setq load-path (cons (concat local-emacs.d-path "single-files/") load-path))
 (setq load-path (cons (concat dotfileswgh "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-pri "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-dotlocal "emacs/") load-path))
 ;; Add straight build dirs, so that emacs can search in them, rather than using
 ;; straight on every emacs load.  It won't process autoloads, but I can wrap
 ;; key bindings with requires or otherwise force autoload evaluation for things
 ;; that I care about.
 (let ((default-directory (concat straight-base-dir "straight/build")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
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
(nobreak (when (file-exists-p custom-file)
           (load custom-file)))


;; backup settings
(nobreak (require 'sensitive-mode))
(setq
   backup-by-copying t      ;; don't clobber symlinks
   backup-directory-alist '(("." . "~/.cache/emacs/bak"))    ;; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 8
   kept-old-versions 4
   version-control t)       ;; use versioned backups
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto/\\1" t))) ;; auto-saves (## files) here
(make-directory "~/.cache/emacs/bak/" t)
(make-directory "~/.cache/emacs/auto/" t)
;; For now, just don't make backups -- I haven't needed them since I started using git everywhere.
(setq make-backup-files nil)

;; mouse settings
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode) nil) ;; shift-click for normal xterm mouse behavior
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
 ;;(require 'auto-compile)
 ;;(auto-compile-on-load-mode 1)
 ;;(auto-compile-on-save-mode 1)

 (setq vc-follow-symlinks t) ;; Don't prompt to follow symlinks of version-controlled files
 (setq-default tab-width 2)
 (setq-default indent-tabs-mode nil) ;; use spaces, not tabs
 (setq tab-stop-list (number-sequence 2 120 2)) ;; setting tab expansion by setting stops
 (defun set-indent-auto (indent-p)
   "Set whether to indent when you hit enter"
   (interactive)
   (if indent-p
       (global-set-key (kbd "RET") 'newline-and-indent)
     (global-set-key (kbd "RET") 'newline)))
 (set-indent-auto t)

 (setq echo-keystrokes 0.01) ;; echo keystrokes faster than default 1s

 (setq enable-recursive-minibuffers t)

 (setq c-default-style "k&r"
       c-basic-offset 2)
 (setq org-startup-folded nil)

 (setq auto-revert-avoid-polling t) ;; avoid polling every X seconds to automatically refresh buffer.  TODO - maybe I want polling?
 (setq auto-revert-check-vc-info t)
 (global-auto-revert-mode t) ;; auto-reload files when they change on disk

 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (setq inhibit-splash-screen t)
 (setq inhibit-startup-echo-area-message (user-login-name))
 (setq inhibit-startup-message t)
 (setq display-time-default-load-average nil)

 )

;; keys, keys, keys!!!
(nobreak

 (setq repeatable-motion-count-needed-prefix "rmo-c/")
 (setq repeatable-motion-definition-prefix "rmo/")
 (setq repeatable-motion-training-wheels-p nil)
 (require 'repeatable-motion)

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
 (require 'vfuncs)
 (load-library "keys")

 ;; I wanted to try one more time to see if maybe I should just use evil-mode with some configuration changes.  But it's too set in its on-character addressing ways.  The evil-move-cursor-back setting only affects what happens when exiting insert mode.  It doesn't change the fact that evil-forward-word-end puts the cursor ON the last character (IE before it) instead of after the last character, and that it has a bunch of logic for adjusting the ranges used for operations (eg. delete forward word end deletes one character farther than the movement goes).  And I think working my full plans in with evil-mode will be difficult, at best.  And the main parts of evil-mode that I really care about that don't conflict with how evil-mode is set up have been relatively easy to re-implement in a much smaller way.  The remaining parts that I care about I can mostly just keep using from evil-mode as a lazy require.
 ;;(setq evil-move-cursor-back nil)
 ;;(setq evil-move-beyond-eol t)
 ;;(load-library "keys-old-evil-mode")
 )

(nobreak
 ;;; secondarily important
 (require 'xclip-conf)
 ;; (unless (display-graphic-p)
 ;;   (setq evil-normal-state-cursor 'box); █
 ;;   (setq evil-visual-state-cursor 'box); █
 ;;   (setq evil-insert-state-cursor 'bar); ⎸
 ;;   (setq evil-emacs-state-cursor 'hbar); _
 ;;   ;; evil-terminal-cursor-changer makes the font size reset (at least in Konsole)...
 ;;   ;; it's clearly a bug, and has annoyed me for a long time.  Let's not use it until it is fixed.
 ;;   ;;(require 'evil-terminal-cursor-changer)
 ;;   )

 ;; elscreen must start before other mode-line stuff, or it wouldn't be this high...
 ;; Elscreen is for making tabs tike I used to use in Vim... but I haven't used it for a long time, so let's disable it.
 ;;(require 'elscreen)
 ;;(setq elscreen-display-tab nil)
 ;;(elscreen-start)
 (require 'wgh-modeline)
 ;; reset the header line in initial buffer, which gets messed up by elscreen
 (setq header-line-format (default-value 'header-line-format))

 )


(nobreak
 ;; ido-completion-map inherits from ido-buffer-completion-map or ido-common-completion-map
 (add-hook 'ido-setup-hook
           (lambda ()
             (define-key ido-completion-map (kbd "C-l") 'ignore)
             (define-key ido-completion-map (kbd "C-f") 'ido-next-match)
             (define-key ido-completion-map (kbd "C-b") 'ido-prev-match)
             (define-key ido-completion-map (kbd "M-r") 'evil-paste-from-register)
             ))

 ;;(ido-mode 1)
 ;; (setq ido-enable-flex-matching t
 ;;       ido-everywhere t)
 ;;(require 'flx-ido)
 ;;(flx-ido-mode 1)

 ;; don't shrink the line number width
 (setq display-line-numbers-grow-only t)
 (global-display-line-numbers-mode)

 ;; guide on disabling/enabling lsp-mode features: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
 (setq lsp-headerline-breadcrumb-enable nil)


 ;;(load-library "yasnippet-conf")
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
 (require 'org-mode-conf)

 (setq-default fill-column 80)
 (global-display-fill-column-indicator-mode 1)

 (defun lsp-common-setup ()
   (require 'lsp)
   (setq lsp-headerline-breadcrumb-enable nil)
   (require 'lsp-ui)
   (require 'lsp-lens)
   (require 'lsp-modeline)

   ;; Unbind <mouse-movement> from showing documentation pop-ups.
   ;; It's not that it's a bad idea, but that it catches “movements” spuriously, and makes it so I constantly have to move the mouse cursor to stop getting pop-ups when I'm not using the mouse.
   (setcdr lsp-ui-mode-map nil)
   ;; The above isn't working, so let's...
   (defun lsp-ui-doc--handle-mouse-movement (event)
     (interactive "e")
     nil)

   ;; I could enable it here, but let's wait until after requiring mode-specific things.
   ;;(lsp)
   )
 (require 'mode-hooks-conf)
 (require 'company-conf)
 ;;(load-library "popwin-conf")
 ;;(load-library "projectile-conf")
 (load-library "scratch-message")
 (require 'delimiters-conf)
 ;;(load-library "sexprw-conf")
 (load-library "borrowed")
 ;; tty-format provides for coloring based on terminal escape codes.  I should set it to autoload at some point, but for now let's disable it.
 ;;(load-library "tty-format")
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
 ;;(require 'smooth-scrolling)

 (setq frame-resize-pixelwise t)

 (setq highlight-indent-guides-auto-enabled nil)
 (setq hl-todo-activate-in-modes '(prog-mode))
 (setq helm-swoop-pre-input-function (lambda () "")) ;; disable symbol-at-point nonsense
 (setq guide-key/guide-key-sequence '("SPC"))
 (setq guide-key/recursive-key-sequence-flag t)
 ;; I like the idea of guide-key, but in practice it doesn't show up in most places where I want it, so I rarely get any benefit.  I think I need to set up my keymaps in a different way for it to work well.
 ;;(require 'guide-key)
 ;;(guide-key-mode 1)
 (setq whitespace-final-newline-message "\n<-- No final newline")
 (require 'whitespace-final-newline)
 (global-whitespace-final-newline-mode 1)

 (savehist-mode 1) ;; Save minibuffer history.

 ;; TODO - completion config -- there is a lot of completion config that I've never really used that I should consider.
 ;;(setq completion-styles '(basic initials substring)) ;; default before editing was (basic partial-completion emacs22)
 ;; completion-cycle-threshold
 ;; completions-detailed
 ;; completion-auto-help
 ;; completions-max-height
 ;; completions-format
 ;; completions-group
 ;; completion-auto-select

 ;;(load-library "keyfreq-conf")
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

(setq gc-cons-threshold (or wgh/orig-gc-threshold 800000))

