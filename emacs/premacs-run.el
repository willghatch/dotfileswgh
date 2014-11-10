;; load a bunch of stuff so that whatever I actually load with a premacs daemon
;; loads FAST, which is the whole point!

(switch-to-buffer "*premacs-setup*")
(ignore-errors (org-mode 1))
(ignore-errors (js-mode 1))
(ignore-errors (python-mode 1))
(ignore-errors (c-mode 1))
(ignore-errors (emacs-lisp-mode 1))
(ignore-errors (shell-script-mode 1))
(ignore-errors (php-mode 1))
(ignore-errors (helm-mode 1))
(ignore-errors (projectile-mode 1))
(ignore-errors (kill-buffer "*premacs-setup*"))
