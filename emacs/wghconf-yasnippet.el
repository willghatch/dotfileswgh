
(require 'yasnippet)

(setq yas-snippet-dirs
      (cons "~/dotfileswgh/emacs/snippets" yas-snippet-dirs))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-s") 'yas-expand)

(setcdr yas-keymap nil) ; keymap when expansion is active
(define-key yas-keymap (kbd "C-d") 'yas-skip-and-clear-or-delete-char)
(define-key yas-keymap (kbd "C-g") 'yas-abort-snippet)
(define-key yas-keymap (kbd "C-b") 'yas-prev-field)
(define-key yas-keymap (kbd "C-f") 'yas-next-field-or-maybe-expand)

; TODO - look into hippie expand, minibuffer prompting, dropdown-completion, and how to best play nice with other completion stuff

(yas-global-mode 1)

