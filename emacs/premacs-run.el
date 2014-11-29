;; load a bunch of stuff so that whatever I actually load with a premacs daemon
;; loads FAST, which is the whole point!

(switch-to-buffer "*premacs-setup*")
(with-demoted-errors (org-mode 1))
(with-demoted-errors (js-mode 1))
(with-demoted-errors (python-mode 1))
(with-demoted-errors (c-mode 1))
(with-demoted-errors (emacs-lisp-mode 1))
(with-demoted-errors (shell-script-mode 1))
(with-demoted-errors (php-mode 1))
(with-demoted-errors (helm-mode 1))
(with-demoted-errors (projectile-mode 1))
(with-demoted-errors (kill-buffer "*premacs-setup*"))
