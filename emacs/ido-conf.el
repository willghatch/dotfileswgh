
; ido-completion-map inherits from ido-buffer-completion-map or ido-common-completion-map
(require 'ido)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-l") 'ignore)
            (define-key ido-completion-map (kbd "C-f") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-b") 'ido-prev-match)
            (define-key ido-completion-map (kbd "M-r") 'evil-paste-from-register)
            ))


