
; ido-completion-map inherits from ido-buffer-completion-map or ido-common-completion-map

(defun ido-my-keys ()
  (define-key ido-completion-map (kbd "C-l") 'ignore) ; so... I thought I wanted to edit the map, but now I'm not sure... I'll leave this in case I want to later...
  )

(add-hook 'ido-setup-hook 'ido-my-keys)

