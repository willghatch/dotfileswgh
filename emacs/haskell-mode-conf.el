
(load-library "haskell-mode-autoloads")

(add-hook 'haskell-mode-hook
          (lambda ()
            ;; This could probably be useful, this activates flymake, which gives a bunch of lame Haskell warnings.  These warnings are suspect for pushing point-free style (which can apparently have some efficiency gains in Haskell, but which is poor for reading code).  And I get these warnings for other people's code, which I don't want.  So... let's just not have this at all right now.
            (setq lsp-diagnostics-provider nil)

            ;; The default language server path should be generic, but I find it brittle.  So... Maybe I'll just update this each time I need to use a different GHC version...
            ;(setq lsp-haskell-server-path "haskell-language-server-wrapper")
            (setq lsp-haskell-server-path "haskell-language-server-9.0.2")

            (require 'lsp)
            (require 'lsp-ui)
            (require 'lsp-haskell)

            ;; TODO - if I haven't yet imported the project, this will not ask which project to load and it will basically fail.  I'm not sure the best way around that.
            (lsp)

            ;; Flymake mode is giving me tons of annoying warnings and errors.  Let's turn it off for now.
            ;; This isn't working, though.  Something is turning it on AFTER this hook is run...
            ;; I'm leaving this here for future reference.  Disabling flymake-mode here did nothing because it was enabled later (probably `lsp` activation does something in the background and activates it later).  I found where it was enabled with `debug-on-entry`.
            ;; Also, the best way to detect if a mode is on or not is by checking it's variable value -- eg. (symbol-value 'flymake-mode) ;; returns t or nil
            ;(flymake-mode -1)
            ;(debug-on-entry #'flymake-mode)

            ;; lsp-lens is the thing that shows eg. imports to the right of import statements, etc
            ;(lsp-lens-mode -1)
            ))
