;;; -*- lexical-binding: t; -*-
;; Configuration for minibuffer and completion-at-point interfaces.

(setq wgh/init-minibuffer-completion-done nil)
(setq wgh/minibuffer-completion-backend nil)
(setq wgh/minibuffer-completion-error nil)

(defun wgh/init-minibuffer-completion-fallback (error-data)
  "Enable built-in visible completion after ERROR-DATA prevented Vertico setup."
  ;; Vertico initialization should be atomic, but turn it off explicitly in
  ;; case the failure happened while enabling the mode.
  (when (bound-and-true-p vertico-mode)
    (vertico-mode -1))

  (require 'icomplete)
  (setq completion-styles '(substring partial-completion basic)
        completion-auto-help 'always
        icomplete-show-matches-on-no-input t
        icomplete-prospects-height 10)
  (if (fboundp 'icomplete-vertical-mode)
      (icomplete-vertical-mode 1)
    (icomplete-mode 1))

  (setq wgh/minibuffer-completion-backend 'icomplete
        wgh/minibuffer-completion-error error-data)
  (display-warning
   'wgh/completion
   (format "Vertico/Orderless initialization failed; using Icomplete: %S"
           error-data)))

(defun wgh/init-minibuffer-completion ()
  "Enable Vertico with Orderless, or a visible built-in fallback."
  (unless wgh/init-minibuffer-completion-done
    (setq wgh/minibuffer-completion-error nil)
    (condition-case err
        (progn
          ;; Load the complete preferred pair before changing global state.
          (require 'vertico)
          (require 'orderless)

          (when (bound-and-true-p fido-vertical-mode)
            (fido-vertical-mode -1))
          (when (bound-and-true-p fido-mode)
            (fido-mode -1))
          (when (bound-and-true-p icomplete-vertical-mode)
            (icomplete-vertical-mode -1))
          (when (bound-and-true-p icomplete-mode)
            (icomplete-mode -1))

          (setq completion-styles '(orderless basic))
          (vertico-mode 1)
          (setq wgh/minibuffer-completion-backend 'vertico))
      (error
       (wgh/init-minibuffer-completion-fallback err)))

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
    (unless (advice-member-p #'crm-indicator #'completing-read-multiple)
      (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

    (setq wgh/init-minibuffer-completion-done t)))

;; Marginalia is useful with either minibuffer backend, but it is not required
;; for completion itself and must not prevent the fallback from working.
(with-demoted-errors "Error initializing Marginalia: %S"
  (require 'marginalia)
  (marginalia-mode 1))



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
    (wgh/init-minibuffer-completion)

    ;; Load everything before enabling modes, so a missing component does not
    ;; leave Corfu partially initialized.
    (require 'corfu) ;; Corfu provides drop downs of completion candidates where you are typing over the buffer, similar to company mode.
    (require 'cape) ;; extra completion-at-point-functions, IE more ways to get completion candidates in different situations, and ways to compose them
    (require 'corfu-info) ;; for corfu-info-documentation, M-h in completion list
    (unless (display-graphic-p)
      ;; TODO - in emacs 31 supposedly this isn't necessary.  But as far as I can tell, the latest released version is 29... so that is probably a ways out.
      (require 'corfu-terminal))

    (setq corfu-cycle t)
    ;;(setq corfu-quit-at-boundary 'separator)

    (global-corfu-mode)
    (unless (display-graphic-p)
      (corfu-terminal-mode +1))

    (setq wgh/init-corfu-done t)
    ))

;; TODO - I would like to be able to start completion without actually completing anything.  I can use undo if I don't like the completion given if there is a single completion, but it would be nice to just show available completions...
(defun wgh/completion-at-point-start ()
  (interactive)
  (wgh/init-corfu)
  (call-interactively 'completion-at-point))

(defun wgh/cape-file (&optional interactive)
  "Complete file name at point, including without a leading path prefix.
Unlike `cape-file', this works even when the file name has no leading
`/', `./', or `../' prefix, as long as there is no whitespace in the name.
If INTERACTIVE is nil the function acts like a Capf."
  (interactive (list t))
  (if interactive
      (cape-interactive '(cape-file-directory-must-exist) #'wgh/cape-file)
    (let ((cape-file-directory-must-exist nil))
      (cape-file nil))))

(provide 'minad-stack-conf)
