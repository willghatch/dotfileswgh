
;; Set up load path for requires
(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/dotfileswgh/emacs"))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/dotfileswgh/emacs" load-path))

;; backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist '(("." . "~/.dotlocal/bakmacs"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 8
   kept-old-versions 4
   version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms '((".*" "~/.dotlocal/automacs/\\1" t))) ; auto-saves (## files) here
(make-directory "~/.dotlocal/bakmacs/" t)
(make-directory "~/.dotlocal/automacs/" t)

;; mouse settings
(if (fboundp 'xterm-mouse-mode) (xterm-mouse-mode) nil) ; shift-click for normal xterm mouse behavior
(if (fboundp 'mouse-wheel-mode) (mouse-wheel-mode) nil)

;; Load borrowed funcion file
(load-library "borrowed")

;; General options
(require 'linum)
(setq linum-format "%4d") ; four digit number
(linum-mode 1)
(global-linum-mode 1) ; add line numbers
(add-hook 'find-file-hook (lambda () (linum-mode 1))) ; USE LINE NUMBERS ON EVERYTHING GOSH DARN IT!
(column-number-mode) ;turn on column numbers in mode line
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
(load-library "wghconf-evil")
;(require 'undo-tree) ; ELPA package used by evil

(require 'ido) ; comes standard with emacs
(ido-mode 1)

(defun myrd () (interactive)
  "pull in rainbow-delimiters (from elpa) with my config"
  (load-library "wghconf-rainbow-delimiters"))


(defun myslime () (interactive)
  "pulls in slime (in elpa) and my config"
  (load-library "wghconf-slime"))

(defun myac () (interactive)
  "pulls in auto-complete package (in elpa) with my config"
  (load-library "wghconf-auto-complete"))

(defun mycomp () (interactive)
  "pulls in company package (in elpa) with my config"
  (load-library "wghconf-company"))

(defun nop () (interactive)
  (+ 0 0))

(menu-bar-mode -1) ; no menu-bar
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(defun a () (interactive)
  "A function to set up things I want in every buffer, in case for some reason they don't work
      LIKE LINUM MODE!!!"
  (linum-mode 1)
  )

(load-library "wghconf-package") ; load packaging config
(load-library "wghconf-modeline") ; load mode line config
(require 'smex)
(setq custom-file "~/dotfileswgh/emacs/custom-file.el")
(load custom-file)

(setq echo-keystrokes 0.01) ; echo keystrokes faster than default 1s

(setq c-default-style "k&r"
      c-basic-offset 4)

(global-set-key (kbd "M-h") 'help-command)
(if (fboundp 'smex)
    (global-set-key (kbd "M-x") 'smex))

(if (file-exists-p "~/.dotlocal/emacs") (load-file "~/.dotlocal/emacs") nil)


