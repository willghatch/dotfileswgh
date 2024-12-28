
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.rsh\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . (lambda () (require 'typescript-mode) (typescript-mode))))

(defun js-conf-setup ()
  (setq-local outline-regexp wgh/c-outline-regexp)
  ;; Define whether I want to end everything with semicolons,
  ;; for my snippets to know whether to add them
  (defvar-local js-want-semicolon nil)
  (defun js-maybe-semicolon ()
    (if js-want-semicolon ";" ""))
  (message "at start of js-conf-setup")

  ;; This really ought to be set per-file or per-project
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)

  (message "about to require lsp")
  (lsp-common-setup)
  (lsp)

  (require 'sgml-mode)

  (when nil
    (require 'js2-mode)
    (js2-minor-mode 1)
    (setq js2-strict-missing-semi-warning nil)

    (require 'js2-refactor)
    (define-key evil-normal-state-local-map "es" 'js2r-forward-slurp)
    (define-key evil-normal-state-local-map "eb" 'js2r-forward-barf)
    (define-prefix-command 'js2r-prefix-command)
    (define-key js2r-prefix-command "eo" 'js2r-expand-object)
    (define-key js2r-prefix-command "co" 'js2r-contract-object)
    (define-key js2r-prefix-command "eu" 'js2r-expand-function)
    (define-key js2r-prefix-command "cu" 'js2r-contract-function)
    (define-key js2r-prefix-command "ea" 'js2r-expand-array)
    (define-key js2r-prefix-command "ca" 'js2r-contract-array)
    (define-key js2r-prefix-command "wi" 'js2r-wrap-buffer-in-iife)
    (define-key js2r-prefix-command "ig" 'js2r-inject-global-in-iife)
    (define-key js2r-prefix-command "ev" 'js2r-extract-var)
    (define-key js2r-prefix-command "iv" 'js2r-inline-var)
    (define-key js2r-prefix-command "rv" 'js2r-rename-var)
    (define-key js2r-prefix-command "vt" 'js2r-var-to-this)
    (define-key js2r-prefix-command "ag" 'js2r-add-to-globals-annotation)
    (define-key js2r-prefix-command "sv" 'js2r-split-var-declaration)
    (define-key js2r-prefix-command "ss" 'js2r-split-string)
    (define-key js2r-prefix-command "ef" 'js2r-extract-function)
    (define-key js2r-prefix-command "em" 'js2r-extract-method)
    (define-key js2r-prefix-command "ip" 'js2r-introduce-parameter)
    (define-key js2r-prefix-command "lp" 'js2r-localize-parameter)
    (define-key js2r-prefix-command "tf" 'js2r-toggle-function-expression-and-declaration)
    (define-key js2r-prefix-command "ao" 'js2r-arguments-to-object)
    (define-key js2r-prefix-command "uw" 'js2r-unwrap)
    (define-key js2r-prefix-command "wl" 'js2r-wrap-in-for-loop)
    (define-key js2r-prefix-command "3i" 'js2r-ternary-to-if)
    (define-key js2r-prefix-command "lt" 'js2r-log-this)
    (define-key js2r-prefix-command "sl" 'js2r-forward-slurp)
    (define-key js2r-prefix-command "ba" 'js2r-forward-barf)
    (define-key js2r-prefix-command "k" 'js2r-kill)
    (define-key evil-normal-state-local-map "tm" 'js2r-prefix-command)
    )
  )

(add-hook 'js-mode-hook 'js-conf-setup)
(add-hook 'typescript-mode-hook 'js-conf-setup)


