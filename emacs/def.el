
(setq custom-file (concat (getenv "DOTFILESDIR") "/emacs/custom-file.el"))
(load custom-file)

(setq package-user-dir (concat (getenv "DOTFILESLOCALDIR") "/emacs.d/elpa"))
;; Set up load path for requires
(let ((default-directory (concat (getenv "DOTFILESLOCALDIR") "/emacs.d")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat (getenv "HOME") "/.emacs.d")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat (getenv "DOTFILESDIR") "/emacs")))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat (getenv "DOTFILESDIR") "/external/emacs")))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons (concat (getenv "DOTFILESDIR") "/emacs") load-path))

;; compile settings
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

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

(require 'evil)
(evil-mode 1)
(require 'key-chord)
(key-chord-mode 1)
(load-library "ace-jump-mode-conf")
(load-library "xclip-conf")
(require 'evil-args)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-little-word)
(require 'evil-textobj-between)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist 1)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

(load-library "vfuncs")
;; Load borrowed funcion file
(load-library "borrowed")

;; General options
(require 'linum)
(global-linum-mode 1) ; add line numbers
(require 'hlinum)
(hlinum-activate)
(require 'linum-relative)
(linum-relative-toggle) ; turn it off as the default

(add-hook 'linum-before-numbering-hook
          (lambda ()
            (setq-local linum-format-fmt
                        (let ((w (max 2 (length (number-to-string
                                          (count-lines (point-min) (point-max)))))))
                          (concat "%" (number-to-string w) "d")))))
(defun linum-format-func (line)
   (propertize (format linum-format-fmt line) 'face 'linum))
(setq linum-format 'linum-format-func)

;(add-hook 'find-file-hook (lambda () (linum-mode 1))) ; USE LINE NUMBERS ON EVERYTHING GOSH DARN IT!
;(column-number-mode) ;turn on column numbers in mode line
(setq vc-follow-symlinks t) ; Don't prompt to follow symlinks of version-controlled files
(setq tab-width 4) ; how many spaces for tab DISPLAY
(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(setq tab-stop-list (number-sequence 4 120 4)) ; setting tab expansion by setting stops
;(set-tab-stop-width 4) ; borrowed func to change tab stop list based on width
;(setq-default indent-line-function 'tab-to-tab-stop) ; just go to the tab stops that are 4 spaces apart...
(defun set-indent-auto (indent-p)
  "Set whether to indent when you hit enter"
  (interactive)
  (if indent-p
      (global-set-key (kbd "RET") 'newline-and-indent)
    (global-set-key (kbd "RET") 'newline)))
(set-indent-auto t)



;;;;;;;;;;;;;;;;; External Package Load

;; elpa packages loaded automatically:
;; evil
;; key-chord
;; ace-jump
;; undo-tree (auto installed with evil through package.el)

;; Emacs VI Layer - avail from ELPA as evil
(load-library "keys")
;(require 'undo-tree) ; ELPA package used by evil

(require 'ido) ; comes standard with emacs
(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(with-demoted-errors
  (require 'flx-ido)
  (flx-ido-mode 1))



(menu-bar-mode -1) ; no menu-bar
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


(require 'smex)
(require 'yafolding)
(require 'rainbow-delimiters)
(load-library "package-conf") ; load packaging config
(load-library "modeline-conf") ; load mode line config
(load-library "yasnippet-conf")
(load-library "org-mode-conf")
(load-library "fci-mode-conf")
(load-library "mode-hooks-conf")
(load-library "company-conf")
(load-library "hippie-expand-conf")
(load-library "popwin-conf")
(load-library "projectile-conf")
(load-library "scratch-message")
(winner-mode 1)
(show-smartparens-global-mode 1)
(yafolding-mode 1)

(setq echo-keystrokes 0.01) ; echo keystrokes faster than default 1s

(setq c-default-style "k&r"
      c-basic-offset 4)
(setq org-startup-folded nil)

(if (fboundp 'smex)
    (global-set-key (kbd "M-x") 'smex))

(global-auto-revert-mode t) ; auto-reload files when they change on disk

(let ((file (concat (getenv "DOTFILESLOCALDIR") "/emacs")))
  (if (file-exists-p file) (load-file file) nil))


(print (format "start time: %f" (time-to-seconds (time-subtract (current-time) before-init-time))))


