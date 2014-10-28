
;; Set up load path for requires
(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/dotfileswgh/emacs"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/dotfileswgh/external/emacs"))
      (normal-top-level-add-subdirs-to-load-path))
(setq load-path (cons "~/dotfileswgh/emacs" load-path))

;; compile settings
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

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
(global-linum-mode 1) ; add line numbers
(require 'hlinum)
(hlinum-activate)
(setq linum-format "%4d") ; four digit number
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
(load-library "wghconf-evil")
;(require 'undo-tree) ; ELPA package used by evil

(require 'ido) ; comes standard with emacs
(ido-mode 1)
(flx-ido-mode 1)

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

(defun whitespace ()
  (interactive)
  ;; These two highlighters kill the formatting of
  ;; list-faces-display... and I don't know why
  (hc-toggle-highlight-tabs)
  (hc-toggle-highlight-trailing-whitespace))

(defun np-buffer-no-star (next-buffer-func)
  "Cycle buffers ignoring ** buffers.  If it circles back to the first buffer
it calls the next function one more time."
  (let ((cbuf (buffer-name)))
    (funcall next-buffer-func)
    (while (and (string= "*" (substring (buffer-name) 0 1))
                (not (string= cbuf (buffer-name))))
      (funcall next-buffer-func))
    (when (string= cbuf (buffer-name))
      (funcall next-buffer-func))))
(defun next-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'next-buffer))
(defun prev-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'previous-buffer))

(load-library "wghconf-package") ; load packaging config
(load-library "wghconf-modeline") ; load mode line config
(load-library "wghconf-yasnippet")
(load-library "wghconf-org-mode")
(load-library "wghconf-fci-mode")
(load-library "wghconf-mode-hooks")
(load-library "wghconf-hippie-expand")
(winner-mode 1)
(require 'smex)
(require 'yafolding)
(require 'rainbow-delimiters)
(show-smartparens-global-mode 1)
(yafolding-mode 1)
(setq custom-file "~/dotfileswgh/emacs/custom-file.el")
(load custom-file)

(setq echo-keystrokes 0.01) ; echo keystrokes faster than default 1s

(setq c-default-style "k&r"
      c-basic-offset 4)
(setq org-startup-folded nil)

(if (fboundp 'smex)
    (global-set-key (kbd "M-x") 'smex))

(global-auto-revert-mode t) ; auto-reload files when they change on disk

(if (file-exists-p "~/.dotlocal/emacs") (load-file "~/.dotlocal/emacs") nil)


(print (format "start time: %f" (time-to-seconds (time-subtract (current-time) before-init-time))))


