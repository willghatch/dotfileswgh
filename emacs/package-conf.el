
; load local settings first, in case of proxy or something
(with-demoted-errors "Error: %S" (load-file "~//dotfileswgh/dotlocal/emacs"))

(let ((local-e-d (concat (getenv "DOTFILESLOCALDIR") "/emacs.d")))
  (make-directory local-e-d t)
  (setq package-user-dir (concat local-e-d "/elpa"))

  (let ((default-directory local-e-d))
    (normal-top-level-add-subdirs-to-load-path))
  (let ((default-directory "~/.emacs.d"))
    (normal-top-level-add-subdirs-to-load-path))
  )

(require 'package)
(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)

