
;; load slime (available in melpa)
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/clisp")


;; key mapping
(defun slime-set-mykeys ()
  (define-key evil-normal-state-map "mh" 'slime-documentation-lookup)
  (define-key evil-insert-state-map "\t" 'slime-complete-symbol)
  )
  

(slime)

(slime-set-mykeys)

