;;; -*- lexical-binding: t; -*-
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
 (let ((default-directory (concat dotfileswgh-ghp "emacs/")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
 (let ((default-directory (concat dotfileswgh-pri "emacs/")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
 (let ((default-directory (concat dotfileswgh-dotlocal "emacs/")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
 (let ((default-directory (concat dotfileswgh-pri-dotlocal "emacs/")))
   (when (file-exists-p default-directory)
     (normal-top-level-add-subdirs-to-load-path)))
 (setq load-path (cons (concat local-emacs.d-path "single-files/") load-path))
 (setq load-path (cons (concat dotfileswgh "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-ghp "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-pri "emacs/") load-path))
 (setq load-path (cons (concat dotfileswgh-pri-dotlocal "emacs/") load-path))
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
(nobreak
 (make-directory "~/.cache/emacs/bak/" t)
 (make-directory "~/.cache/emacs/auto/" t))
;; For now, just don't make backups -- I haven't needed them since I started using git everywhere.
(setq make-backup-files nil)

;; mouse settings
(nobreak
 (if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode) nil) ;; shift-click for normal xterm mouse behavior
 (if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode) nil))

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
 ;(load-library "demo-keys")

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
 (define-global-minor-mode blacklisted-global-whitespace-final-newline-mode
   whitespace-final-newline-mode
   (lambda ()
     (when (or (buffer-file-name)
               (equal (buffer-name) "*scratch*"))
       (whitespace-final-newline-mode 1))))
 (blacklisted-global-whitespace-final-newline-mode 1)

 (setq browse-url-browser-function 'browse-url-firefox)
 (setq browse-url-firefox-program "ffxd")

 ;;(setq eldoc-echo-area-prefer-doc-buffer t) ;; only use echo area if doc buffer is not visible
 (setq eldoc-documentation-strategy 'eldoc-documentation-compose)
 (setq lsp-eldoc-render-all t)
 ;;(setq eldoc-echo-area-use-multiline-p nil) ;; TODO - the default is 'truncate-sym-name-if-fit
 ;; TODO - eldoc - try using display-buffer-alist to have dedicated space for eldoc-doc-buffers, as suggested in https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

 (savehist-mode 1) ;; Save minibuffer history.

 (setq copilot-idle-delay nil) ;; Don't auto-suggest, just complete when requested.


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

(nobreak
 (let ((file (concat dotfileswgh-ghp "emacs/def.el")))
   (if (file-exists-p file) (load-file file) nil))
 (let ((file (concat dotfileswgh-pri "emacs/def.el")))
   (if (file-exists-p file) (load-file file) nil))
 (let ((file (concat dotfileswgh-pri-dotlocal "emacs/def.el")))
   (if (file-exists-p file) (load-file file) nil))
 (let ((file (concat dotfileswgh-dotlocal "emacs/def.el")))
   (if (file-exists-p file) (load-file file) nil)))


(nobreak (lightdark-update-theme))
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
 (disable-csi-u-key-encoding)
 )

;; Some experiments with kkp package, the one thing I wanted was to distinguish C-i vs TAB and C-m vs RET.  I think it's possible if I fork the package and intercept the key as a string -- eg. it generates the string "C-i" or "C-m", but then sends those to the kbd function, which translates them to TAB and RET.
(nobreak
 ;; kitty keyboard protocol
 ;; I'm not sure how important this is.  It lets me get s-<key> bindings, but wezterm is not sending (or understanding, probably) hyper for H-<key> bindings.  But I typically use super and hyper for window management or global keys, and I don't think I'm going to even use them in emacs.
 ;; But it also lets me use C-. and C-' and such without special encoding.
 ;; However, it does not allow me do disambiguate C-i vs TAB, or C-m vs RET, or even C-R vs C-r.  So I'm still using some custom encodings in Wezterm anyway.
 ;; But I'm not sure how much I want to rely on key bindings that will only work within Wezterm, anyway, so I'm not sure why I'm even bothering with this.
 ;;(require 'kkp)
 ;;(global-kkp-mode 1)


 ;; kkp-alternatives-map is used as parent for local-function-key-map, whose parent is function-key-map
 ;;(setq function-key-map (delq '(kp-tab . [9]) function-key-map))
 ;;(setq function-key-map (delq '(tab . [9]) function-key-map))
 ;;(setq local-function-key-map (delq '(tab . [9]) local-function-key-map))
 ;;(define-key kkp-alternatives-map (kbd "TAB") (kbd "<tab>"))
 ;; (define-key local-function-key-map [tab] nil)
 ;; (define-key function-key-map [tab] nil)
 ;; (define-key kkp-alternatives-map [tab] nil)
 ;; (define-key local-function-key-map [kp-tab] nil)
 ;; (define-key function-key-map [kp-tab] nil)
 ;; (define-key kkp-alternatives-map [kp-tab] nil)
 ;;(define-key input-decode-map [?\C-i] [C-i])
 )

(message "-")
(message "--")
(message "---")
(message (format "start time: %f" (time-to-seconds (time-subtract (current-time) before-init-time))))

(setq gc-cons-threshold (or wgh/orig-gc-threshold 800000))

