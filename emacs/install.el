
(load-file "~/dotfileswgh/emacs/package-conf.el")

(package-refresh-contents)

(with-demoted-errors (package-install 'evil))
(with-demoted-errors (package-install 'key-chord))
(with-demoted-errors (package-install 'auto-compile))
(with-demoted-errors (package-install 'evil-surround))
(with-demoted-errors (package-install 'evil-matchit))
(with-demoted-errors (package-install 'evil-search-highlight-persist))
(with-demoted-errors (package-install 'yafolding))
(with-demoted-errors (package-install 'company))

(with-demoted-errors (package-install 'evil-args))
(with-demoted-errors (package-install 'rainbow-delimiters))
(with-demoted-errors (package-install 'rainbow-identifiers))
(with-demoted-errors (package-install 'rainbow-mode)) ; for #123456 colors
(with-demoted-errors (package-install 'smartparens))
(with-demoted-errors (package-install 'ace-jump-mode))

(with-demoted-errors (package-install 'xclip))
(with-demoted-errors (package-install 'ag))
(with-demoted-errors (package-install 'smex))
(with-demoted-errors (package-install 'flycheck))
(with-demoted-errors (package-install 'fill-column-indicator))
(with-demoted-errors (package-install 'highlight-chars))
(with-demoted-errors (package-install 'hlinum))
(with-demoted-errors (package-install 'elisp-slime-nav))
(with-demoted-errors (package-install 'projectile))
(with-demoted-errors (package-install 'flx-ido))
(with-demoted-errors (package-install 'expand-region))
(with-demoted-errors (package-install 'popwin))
(with-demoted-errors (package-install 'linum-relative))

(with-demoted-errors (package-install 'helm))
(with-demoted-errors (package-install 'helm-swoop))
(with-demoted-errors (package-install 'helm-ag))
(with-demoted-errors (package-install 'helm-ag-r))
(with-demoted-errors (package-install 'helm-projectile))
(with-demoted-errors (package-install 'helm-package))
(with-demoted-errors (package-install 'helm-mode-manager))
(with-demoted-errors (package-install 'helm-helm-commands))
(with-demoted-errors (package-install 'helm-c-yasnippet))
(with-demoted-errors (package-install 'helm-company))
(with-demoted-errors (package-install 'helm-descbinds))




(with-demoted-errors (package-install 'haskell-mode))
(with-demoted-errors (package-install 'markdown-mode))
(with-demoted-errors (package-install 'slime))
(with-demoted-errors (package-install 'lua-mode))
(with-demoted-errors (package-install 'vimrc-mode))
