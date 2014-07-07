
;; Set up load path for requires
(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/dotfileswgh/emacs"))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/dotfileswgh/emacs" load-path))

;; Load borrowed funcion file
(load-file "~/dotfileswgh/emacs/borrowed.el")

;; General options
(global-linum-mode) ; add line numbers
(setq vc-follow-symlinks t) ; Don't prompt to follow symlinks of version-controlled files
(setq tab-width 4) ; how many spaces for tab DISPLAY
(setq-default indent-tabs-mode nil) ; use spaces, not tabs
(setq tab-stop-list (number-sequence 4 120 4)) ; setting tab expansion by setting stops
;(set-tab-stop-width 4) ; borrowed func to change tab stop list based on width
(defun set-indent-auto (indent-p)
  "Set whether to indent when you hit enter"
  (interactive)
  (if indent-p
      (global-set-key (kbd "RET") 'newline-and-indent)
    (global-set-key (kbd "RET") 'newline)))
(set-indent-auto t)
(defun setwrap () (interactive) (toggle-truncate-lines)) ; easier to type for a common function



;;;;;;;;;;;;;;;;; External Package Load

;; elpa packages loaded automatically:
;; evil
;; key-chord
;; ace-jump
;; undo-tree (auto installed with evil through package.el)

;; Emacs VI Layer - avail from ELPA as evil
(load-library "wghconf-evil")


;(require 'undo-tree) ; ELPA package used by evil

;; This command pulls in rainbow-delimiters (in elpa)
(defun royalrainbow () (interactive)
  (load-library "wghconf-rainbow-delimiters"))

;; This command pulls in package.el... the packaging system that comes defauld in emacs 24.
(defun packaging-load () (interactive)
  (load-library "wghconf-package"))

;; backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacsbak"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; mouse settings
(xterm-mouse-mode) ; shift-click for normal xterm mouse behavior

