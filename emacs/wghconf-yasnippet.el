
(require 'yasnippet)

(setq yas-snippet-dirs
      (cons "~/dotfileswgh/emacs/snippets" yas-snippet-dirs))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-s") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-m") 'yas-next-field-or-maybe-expand)

(yas-global-mode 1)

