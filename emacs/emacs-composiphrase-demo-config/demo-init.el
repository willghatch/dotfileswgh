;;; -*- lexical-binding: t; -*-
;; Setting a higher gc threshold makes startup way faster
(setq gc-cons-threshold 10000000)

;; Use the demo repo dir as the user directory.
(setq emacs-user-directory (file-name-directory load-file-name))

;; Set up load path to include the demo files and the installed packages
(let ((demo-dir (file-name-directory load-file-name)))
  (setq load-path (cons demo-dir load-path))
  (let ((default-directory (concat demo-dir "straight/build")))
   (normal-top-level-add-subdirs-to-load-path)))

;; I don't want to bother figuring out how to display things on the default
;; modeline right now.  So now the demo will include my mode line config.  Bask
;; in its beauty.  Or call it ugly.  Whatever, I don't care.  It shows the
;; estate-state and current sentence state.  Actually, now that I look at it, I
;; would also have to add some of my theme to get it to work right.  Too much
;; work.  So now the demo has an extra ugly version of my mode line.
(load-library "demo-modeline")
(load-library "demo-keys")
