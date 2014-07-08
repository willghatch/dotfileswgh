
;; load slime (available in melpa)
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/bin/clisp")


;; key mapping
(defun slime-set-mykeys ()
  (define-key evil-normal-state-map "mh" 'slime-documentation-lookup)
  (local-set-key "\t" 'slime-complete-symbol)
  )
(add-hook 'slime-mode-hook 'slime-set-mykeys t) ; locally add my keys
  

(slime)

(slime-set-mykeys)

