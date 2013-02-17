
;; Set up load path for requires
(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "/usr/share/emacs/site-lisp"))
      (normal-top-level-add-subdirs-to-load-path))



;; Load repository list
(load-file "~/.emacs.d/packaging.el")


;;;;;;;;;;;;;;;; Color!!!
;(require 'color-theme) ; to load color themeses - requires color theme package from ELPA
;(color-theme-initialize)

;; Don't prompt to follow symlinks of version-controlled files
(setq vc-follow-symlinks t)


;; for undo-tree (req of EVIL), from ELPA
(require 'undo-tree)



;;;;;;;;;;;;;;;;;;;;;;     Key Map Section

;; Emacs VI Layer - avail from ELPA
(require 'evil)
(evil-mode 1)
(require 'key-chord)
(key-chord-mode 1)



; Add auto-completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/usr/share/emacs/site-lisp/auto-complete/ac-dict")
(ac-config-default)




;;;;;;;;;;;;;;;;;;;;;;;;;;;; Java editing stuff

;; This .emacs file illustrates the minimal setup
;; required to run the JDE.

;; Update the Emacs load-path to include the path to
;; the JDE and its require packages. This code assumes
;; that you have installed the packages in the emacs/site
;; subdirectory of your home directory.
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/jde/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/cedet/common"))
;;(add-to-list 'load-path (expand-file-name "~/emacs/site/elib"))

;; Initialize CEDET.
;;(load-file (expand-file-name "/usr/share/emacs/site-lisp/cedet/common/cedet.el"))
;;(load-file (expand-file-name "~/emacs/site/cedet/common/cedet.el"))
;(require 'cedet)

;;; If you want Emacs to defer loading the JDE until you open a
;;; Java file, edit the following line
;;(setq defer-loading-jde nil)
;;; to read:
;;;
;  (setq defer-loading-jde t)
;;;
;
;(if defer-loading-jde
;    (progn
;      (autoload 'jde-mode "jde" "JDE mode." t)
;      (setq auto-mode-alist
;          (append
;           '(("\\.java\\'" . jde-mode))
;           auto-mode-alist)))
;  (require 'jde))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end Java stuff



;; Add line numbers
(require 'linum)
(global-linum-mode)


;; Load custom keyset
(load-file "~/.emacs.d/keys.el")

(load-theme 'tsdh-dark)

