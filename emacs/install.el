
(load-file "~/dotfileswgh/emacs/package-conf.el")

(package-refresh-contents)

(defun in (package)
  (with-demoted-errors "Error installing: %S" (package-install package)))

(in 'evil)
(in 'key-chord)
(in 'auto-compile)
(in 'evil-surround)
(in 'evil-matchit)
(in 'evil-search-highlight-persist)
(in 'yafolding)
(in 'company)

(in 'evil-args)
(in 'rainbow-delimiters)
(in 'rainbow-identifiers)
(in 'rainbow-mode) ; for #123456 colors
(in 'smartparens)
(in 'ace-jump-mode)

(in 'xclip)
(in 'ag)
(in 'smex)
(in 'flycheck)
(in 'fill-column-indicator)
(in 'highlight-chars)
(in 'hlinum)
(in 'elisp-slime-nav)
(in 'projectile)
(in 'flx-ido)
(in 'expand-region)
(in 'popwin)
(in 'linum-relative)
(in 'smooth-scrolling)

(in 'helm)
(in 'helm-swoop)
(in 'helm-ag)
(in 'helm-ag-r)
(in 'helm-projectile)
(in 'helm-package)
(in 'helm-mode-manager)
(in 'helm-helm-commands)
(in 'helm-c-yasnippet)
(in 'helm-company)
(in 'helm-descbinds)




(in 'haskell-mode)
(in 'racket-mode)
(in 'markdown-mode)
(in 'slime)
(in 'lua-mode)
(in 'vimrc-mode)
(in 'anaconda-mode)
(in 'company-anaconda)
(in 'web-mode)
(in 'indent-guide)

