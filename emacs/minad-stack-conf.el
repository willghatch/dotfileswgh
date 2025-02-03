;;; -*- lexical-binding: t; -*-
;; I'm going to give the Minad stack a try.  I haven't yet read through the docs on all of these, but I'll see how they go together for a bit.

(setq wgh/init-minad-done nil)
(defun wgh/init-minad ()
  (when (not wgh/init-minad-done)
    (require 'vertico) ;; vertico provides a completion UI in a window at the bottom, like helm, good for M-x and big lists
    (require 'marginalia) ;; provides extra documentation to completion display
    (require 'consult)
    (require 'embark) ;; provides more ways to act on completion candidates
    (require 'orderless) ;; a completion filtering system, IE a function for how what you type filters and sorts the completion candidates

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

    (setq wgh/init-minad-done t)

    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Completion at point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Completion at point uses completion-at-point-functions, a variable with a list of completion functions, to get completion candidates.
;; The CAPF variable is always set, allowing for automatic background completion.
;; But I want to have different keys to start different kinds of completion.
;; It turns out that you can temporarily call specific completers using `cape-interactive'.
;; So there is no need for manually managing CAPF list at all times...


(setq-default completion-at-point-functions '(cape-dabbrev))

(setq wgh/init-corfu-done nil)
(defun wgh/init-corfu ()
  (when (not wgh/init-corfu-done)
    (setq wgh/init-corfu-done t)

    (wgh/init-minad)

    (setq corfu-cycle t)
    ;;(setq corfu-quit-at-boundary 'separator)

    (require 'corfu) ;; Corfu provides drop downs of completion candidates where you are typing over the buffer, similar to company mode.
    (global-corfu-mode)
    (require 'cape) ;; extra completion-at-point-functions, IE more ways to get completion candidates in different situations, and ways to compose them

    (require 'corfu-info) ;; for corfu-info-documentation, M-h in completion list
    (unless (display-graphic-p)
      ;; TODO - in emacs 31 supposedly this isn't necessary.  But as far as I can tell, the latest released version is 29... so that is probably a ways out.
      (require 'corfu-terminal)
      (corfu-terminal-mode +1))
    ))

;; TODO - I would like to be able to start completion without actually completing anything.  I can use undo if I don't like the completion given if there is a single completion, but it would be nice to just show available completions...
(defun wgh/completion-at-point-start ()
  (interactive)
  (wgh/init-corfu)
  (call-interactively 'completion-at-point))

(provide 'minad-stack-conf)
