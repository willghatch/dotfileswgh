(require 'smartparens)

;; remove the pairs that won't be good for lisp
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;; add some pairs that seem to be lacking
(sp-local-pair 'racket-mode "#|" "|#")
(setq sp-sexp-prefix '((racket-mode regexp "#?[`',]@?")))
