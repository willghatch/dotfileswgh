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

(load-library "demo-keys")
