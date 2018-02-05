


(add-hook 'org-mode-hook
          (lambda ()
            (message "doing org-mode-hook")
            (lnkmap "eh" 'org-forward-heading-same-level)
            (lnkmap "oh" 'org-backward-heading-same-level)
            (lnkmap "og" 'outline-up-heading)
            (lnkmap "eg" 'outline-up-heading)

            (lnkmap "ml" 'org-metaright)
            (lnkmap "mh" 'org-metaleft)
            (lnkmap "mw" 'org-demote-subtree)
            (lnkmap "mb" 'org-promote-subtree)

            (lnkmap "mk" 'org-move-subtree-up)
            (lnkmap "mj" 'org-move-subtree-down)


            ;; org-mark-element highlights the current-line and its subtree
                                        ;(lnkmap "mh" 'org-mark-element)

            (define-key org-mode-map (kbd "TAB") nil)
            (define-key org-mode-map (kbd "M-h") nil)

            (setq fold-toggle-wgh-fold-func 'org-cycle)
            (setq fold-toggle-wgh-fold-all-func 'org-shifttab)
            (setq org-cycle-emulate-tab nil)


            (setq org-hide-leading-stars t)
            (setq indent-line-function 'indent-relative)
            ;; Reload theme so that the stupid hidden asterisks are slightly visible
            (run-with-timer 1 nil (lambda ()
                                    ;; It only seems to work when called interactively...
                                    (call-interactively 'current-theme-reapply)))
            ))



