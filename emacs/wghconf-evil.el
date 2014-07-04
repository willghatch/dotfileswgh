;;; Evil package configuration

(require 'evil)
(evil-mode 1)


(define-key evil-insert-state-map "jj" 'evil-normal-state)
(define-key evil-normal-state-map "gt" 'next-buffer)
(define-key evil-normal-state-map "gT" 'previous-buffer)

