
(defun racket-close-paren-hack (&optional arg)
  (interactive "p")
  (if (on-parens-on-close?)
      (self-insert-command arg)
    (racket-insert-closing)))
(add-hook 'racket-mode-hook
          (lambda ()
            (setq racket-show-functions '(racket-show-echo-area))
            (define-key racket-mode-map "[" nil)
            (define-key racket-mode-map ")" 'racket-close-paren-hack)
            (define-key racket-mode-map "]" nil)
            (define-key racket-mode-map "}" nil)
            (racket-xp-mode 1)
            ))

;; Remove usage highlighting in racket-xp-mode by default.  It starts enabled.
;; It's pretty cool, but for normal programming the constant flashing of
;; definition/use highlighting is distracting and annoying.
;; I should also figure out how to stop just the highlighting -- the
;; automatic showing of eg. where a binding is from is awesome and not at
;; all distracting.
(setq wgh/racket-xp-pre-redisplay-active t)
(advice-add 'racket--add-overlay :around (lambda (f &rest args)
                                           (when wgh/racket-xp-pre-redisplay-active
                                             (apply f args))))
(defun wgh/racket-xp-pre-redisplay-toggle ()
  (interactive)
  (setq wgh/racket-xp-pre-redisplay-active (not wgh/racket-xp-pre-redisplay-active)))
