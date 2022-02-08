
(add-hook 'haskell-mode-hook #'lsp)

(add-hook 'haskell-mode-hook
          (lambda ()
            ;; Flymake mode is giving me tons of annoying warnings and errors.  Let's turn it off for now.
            (flymake-mode 0)
            ;; lsp-lens is the thing that shows eg. imports to the right of import statements, etc
            (lsp-lens-mode 0)
            ))
