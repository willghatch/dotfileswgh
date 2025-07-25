;;; -*- lexical-binding: t; -*-
(require 'vfuncs)

(let* ((banner-dir (concat (getenv "DOTFILESWGH") "/emacs/banners/"))
       (banner-files (directory-files banner-dir t "^[^\.]"))
       (banner-file (nth (random (length banner-files)) banner-files)))
  (setq scratch-ascii-banner
        (apply #'concat (mapcar (lambda (x) (concat ";; " x "\n"))
                                (file->lines banner-file)))))

;; If I don't set this, straight single quotes are turned into curved quotes.
(setq text-quoting-style 'straight)

(setq scratch-useful-message
      "

;;;; Useful notes for run-time definitions:

;; s-expression rewriting: sexprw
;; Full docs at: $DOTFILESWGH/external/emacs/sexp-rewrite/REFERENCE.md
;;; Define a tactic and function sexprw-tactic/NAME
;;(define-sexprw-tactic/func NAME
;;  PATTERN
;;  RESULT)

;;(define-key evil-normal-state-map \"a\" 'evil-append)
;;(defalias 'eb 'eval-buffer)
;; e[m,n,i,I,v,w,t]map
;;(etmap \"p\" 'previous-something)
;;;; For terminal paste when newline-and-indent breaks things...
;;(eimap (kbd \"RET\") 'newline)
;;(eimap (kbd \"RET\") 'newline-and-indent)
;;
;;(defun foo (n)
;;  (interactive)
;;  (foo)) ; so I remember defun format
;;
;;(mapcar (lambda (b)
;;        (with-current-buffer b
;;          (reformat-file)))
;;      (file-visiting-buffer-list))

;; eval this line to run the hooks necessary for actual elisp editing
(lisp-interaction-mode)

")


(setq initial-scratch-message (concat scratch-ascii-banner scratch-useful-message))
