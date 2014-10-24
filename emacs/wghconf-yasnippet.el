
(require 'yasnippet)

(setq yas-snippet-dirs '("~/dotfileswgh/emacs/yasnippets" "~/.dotlocal/yasnippets"))

(defun yas-insert-with-region ()
  (interactive )
  (evil-insert-state 1)
  (yas-insert-snippet))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(setcdr yas-keymap nil) ; keymap when expansion is active
(define-key yas-keymap (kbd "C-d") 'yas-skip-and-clear-or-delete-char)
(define-key yas-keymap (kbd "C-g") 'yas-abort-snippet)
(define-key yas-keymap (kbd "C-b") 'yas-prev-field)
(define-key yas-keymap (kbd "C-f") 'yas-next-field-or-maybe-expand)

; TODO - look into hippie expand, minibuffer prompting, dropdown-completion, and how to best play nice with other completion stuff

(yas-global-mode 1)

