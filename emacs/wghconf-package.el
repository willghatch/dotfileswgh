
; load local settings first, in case of proxy or something
(ignore-errors (load-file "~//dotfileswgh/dotlocal/emacs"))

(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))


(require 'package)
(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)

