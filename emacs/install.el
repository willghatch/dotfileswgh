
(load-file "~/dotfileswgh/emacs/package-conf.el")

(message "Updating package lists...")
(package-refresh-contents)

(defun in (package)
  (with-demoted-errors "Error installing: %S" (package-install package)))

(message "Installing packages...")

(in 'evil)
(in 'repeatable-motion)
(in 'on-parens)
(in 'key-chord)
(in 'hydra)
(in 'auto-compile)
(in 'evil-surround)
(in 'evil-search-highlight-persist)
;; TODO - eventually targets.el (https://github.com/noctuid/targets.el) should
;; replace evil-textobj-anyblock.  But it's not ready yet, apparently.
(in 'evil-textobj-anyblock)
(in 'evil-cleverparens)
(in 'evil-terminal-cursor-changer)
(in 'evil-anzu)
(in 'yafolding)
(in 'company)

(in 'evil-args)
(in 'rainbow-delimiters)
(in 'rainbow-identifiers)
(in 'rainbow-mode) ; for #123456 colors
(in 'smartparens)
(in 'ace-jump-mode)
(in 'markchars)

(in 'elscreen)
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
(in 'guide-key)
(in 'magit)

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


(in 'js2-mode)
(in 'js2-refactor)
(in 'haskell-mode)
(in 'racket-mode)
(in 'scribble-mode)
(in 'markdown-mode)
(in 'slime)
(in 'lua-mode)
(in 'vimrc-mode)
(in 'anaconda-mode)
(in 'company-anaconda)
(in 'web-mode)
(in 'indent-guide)
(in 'hl-todo)
(in 'git-commit)
(in 'systemd)

(in 'keyfreq)

;; some other packages I don't currently use, but might want to later:

;; figlet -- make ascii banners

;; Link programs -- to give pentadactyl/vimium style hints to links on page
;; The most important thing for links is that I can define new ones and new ways of handling them.
;; Getting to the links I can use any movement command in emacs, so moving to the link is less of
;; an issue than recognizing and handling the link.
;;
;; Ace link doesn't support many modes (eg. it doesn't work in fundamental or markdown mode)
;; (in ace-link) ; -- ace jump to navigation links in info, help, eww, or compilation mode
;; link-hint -- I'm not sure there's a good way of defining new link types (eg. define a regex and a handler function).
;;              Also, it auto-follows a link if there is only one, which I don't like (may be configurable).
;; (in 'link-hint)

;; 

(message "Done installing packages!")

