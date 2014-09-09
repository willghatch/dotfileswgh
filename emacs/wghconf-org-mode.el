
(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "mf" 'org-metaright) ;indent heading
            (define-key evil-normal-state-local-map "mb" 'org-metaleft) ;unindent heading
            (define-key evil-normal-state-local-map "ms" 'org-cycle) ;cycle fold
            (define-key evil-normal-state-local-map "mt" 'org-shifttab) ;global cycle fold
            (define-key evil-normal-state-local-map "mi" 'org-insert-heading)
            (define-key org-mode-map (kbd "TAB") nil)
            ))



