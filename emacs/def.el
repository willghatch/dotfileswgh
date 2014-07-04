
;; Set up load path for requires
(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/dotfileswgh/emacs"))
      (normal-top-level-add-subdirs-to-load-path))


;; General options
(global-linum-mode) ; add line numbers
(setq vc-follow-symlinks t) ; Don't prompt to follow symlinks of version-controlled files
;(load-file "~/.emacs.d/packaging.el") ; Load repository list -- I don't think I want normal sessions to do that

;;;;;;;;;;;;;;;;; External Package Load

;; Emacs VI Layer - avail from ELPA
(load-file "~/dotfileswgh/emacs/wghconf-evil.el")

;(require 'undo-tree) ; ELPA package

(defun royalrainbow () (interactive)
  (load-file "~/dotfileswgh/emacs/wghconf-rainbow-delimiters.el"))


;; backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacsbak"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

