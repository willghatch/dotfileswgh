
(load-file "~/dotfileswgh/emacs/wghconf-package.el")

(package-refresh-contents)

(ignore-errors (package-install 'evil))
(ignore-errors (package-install 'key-chord))
(ignore-errors (package-install 'auto-compile))
(ignore-errors (package-install 'evil-surround))
(ignore-errors (package-install 'yafolding))
(ignore-errors (package-install 'company))

(ignore-errors (package-install 'evil-args))
(ignore-errors (package-install 'rainbow-delimiters))
(ignore-errors (package-install 'ace-jump-mode))

(ignore-errors (package-install 'xclip))
(ignore-errors (package-install 'ag))
(ignore-errors (package-install 'smex))
(ignore-errors (package-install 'flycheck))

