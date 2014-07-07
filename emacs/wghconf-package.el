
; load local settings first, in case of proxy or something
(load-file "~/.emacs.local")

(let ((default-directory "~/.emacs.d"))
      (normal-top-level-add-subdirs-to-load-path))


(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

