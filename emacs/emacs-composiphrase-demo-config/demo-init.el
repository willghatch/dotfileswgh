;;; -*- lexical-binding: t; -*-
;; Setting a higher gc threshold makes startup way faster
(setq gc-cons-threshold 10000000)

;; Use the demo repo dir as the user directory.
(setq emacs-user-directory (file-name-directory load-file-name))

;; Set up load path to include the demo files and the installed packages
(let ((demo-dir (file-name-directory load-file-name)))
  (setq load-path (cons demo-dir load-path))
  (let ((default-directory (concat demo-dir "straight/build")))
   (normal-top-level-add-subdirs-to-load-path))
  (setq custom-theme-directory demo-dir))


;; The key bindings!
(load-library "demo-keys")


;; I don't want to bother figuring out how to display things on the default
;; modeline right now.  So now the demo will include my mode line config.  Bask
;; in its beauty.  Or call it ugly.  Whatever, I don't care.  It shows the
;; estate-state and current sentence state.
(load-library "demo-modeline")
;; And the default emacs theme works poorly with my mode line, so now the demo
;; also gets a copy of my custom ad-hoc angry fruit salad theme of beauty.
(load-theme 'composiphrase-demo t)

;; In my personal config the mode line, which is almost exactly the same as this
;; one, is always up to date.  But in this one it was lagging...  Well, let's
;; brute force fix it.
(add-hook 'post-command-hook 'force-mode-line-update)



;; Provide undo/redo... the default Emacs undo system is weird.
(require 'undo-tree)
(global-undo-tree-mode 1)



(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message
      "
;;; COMPOSIPHRASE DEMO
;;; This scratch buffer is not saved, by the way.
;;; You are running the Composiphrase Demo configuration of Emacs!
;;; Remember, you can quit with :q or by pressing C-x C-c (Control+x then Control+c).

")



