

(define-prefix-command 'org-headline-map)
(define-key org-headline-map "l" 'org-metaright) ;indent heading
(define-key org-headline-map "h" 'org-metaleft) ;unindent heading
(define-key org-headline-map "i" 'org-insert-heading)
(define-key org-headline-map "f" 'outline-forward-same-level)
(define-key org-headline-map "b" 'outline-backward-same-level)

(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "me" 'org-mark-element)
            (define-key evil-normal-state-local-map " f" 'org-cycle)
            (define-key evil-normal-state-local-map "  f" 'org-shifttab) ;global cycle fold
            (define-key evil-normal-state-local-map "mh" 'org-headline-map)
            (key-chord-define evil-insert-state-local-map (kbd "wm") 'org-headline-map)
            (define-key org-mode-map (kbd "TAB") nil)
            (define-key org-mode-map (kbd "M-h") nil)
            (setq org-hide-leading-stars t)
            ;; Reload theme so that the stupid hidden asterisks are slightly visible
            (run-with-timer 1 nil (lambda ()
                                    ;; It only seems to work when called interactively...
                                    (call-interactively 'current-theme-reapply)))
            ))



