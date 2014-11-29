;; load a bunch of stuff so that whatever I actually load with a premacs daemon
;; loads FAST, which is the whole point!

(switch-to-buffer "*premacs-setup*")
(nobreak
 (org-mode 1)
 (js-mode 1)
 (python-mode 1)
 (c-mode 1)
 (emacs-lisp-mode 1)
 (shell-script-mode 1)
 (php-mode 1)
 (helm-mode 1)
 (projectile-mode 1)
 (kill-buffer "*premacs-setup*")
 )
