;;; -*- lexical-binding: t; -*-
;; I'm going to give the Minad stack a try.  I haven't yet read through the docs on all of these, but I'll see how they go together for a bit.

(setq wgh/init-minad-done nil)
(defun wgh/init-minad ()
  (when (not wgh/init-minad-done)
    (setq wgh/init-minad-done t)

    (require 'vertico)
    (require 'marginalia)
    (require 'consult)
    (require 'embark)
    (require 'orderless)
    (require 'corfu)
    (require 'cape)

    (setq completion-styles '(orderless basic))

    (vertico-mode)
    (marginalia-mode)

    ;; Hide commands in M-x which do not work in the current mode.  Vertico
    ;; commands are hidden in normal buffers. This setting is useful beyond
    ;; Vertico.
    ;;(read-extended-command-predicate #'command-completion-default-include-p)

    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; TODO - consult provides commands, the most interesting one so far is probably consult-ripgrep

    ;; TODO - embark seems really powerful, but something I need to take time to learn, not just drop in and be using within 30 minutes.
    ;; (global-set-key (kbd "\C-o") nil)
    ;; (global-set-key (kbd "\C-o a") 'embark-act)
    ;; (global-set-key (kbd "\C-o d") 'embark-dwim)
    ;; (global-set-key (kbd "\C-h B") 'embark-bindings)

    ))


(provide 'minad-stack-conf)
