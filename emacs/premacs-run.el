;;; -*- lexical-binding: t; -*-
;; load a bunch of stuff so that whatever I actually load with a premacs daemon
;; loads FAST, which is the whole point!

(switch-to-buffer "*premacs-setup*")
(nobreak
 (org-mode)
 ;;(racket-mode)
 ;;(js-mode)
 ;;(python-mode)
 ;;(c-mode)
 ;;(emacs-lisp-mode)
 (shell-script-mode)
 ;;(php-mode)
 (helm-mode)
 ;;(projectile-mode)
 ;;(diff-mode)
 ;;(require 'company-eclim)
 ;;(require 'xref)
 ;;(require 'etags)
 (require 'magit)
 (message "----------- premacs load done -----------")
 (kill-buffer "*premacs-setup*")
 )
