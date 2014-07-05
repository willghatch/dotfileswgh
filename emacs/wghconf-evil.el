;;; Evil package configuration

(require 'evil)
(evil-mode 1)


(define-key evil-insert-state-map "\C-h" 'evil-normal-state)
(define-key evil-normal-state-map "gt" 'next-buffer)
(define-key evil-normal-state-map "gT" 'previous-buffer)
(define-key evil-motion-state-map "gt" 'next-buffer)
(define-key evil-motion-state-map "gT" 'previous-buffer)
(setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
   (setq evil-emacs-state-modes nil)
(setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
   (setq evil-motion-state-modes nil)

