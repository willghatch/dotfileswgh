
;; default hippie expand list
;; (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)


(defun he-expand-file-name (prefix)
  (interactive "P")
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (hippie-expand prefix)))

(defun he-expand-lisp-symbol (prefix)
  (interactive "P")
  (let ((hippie-expand-try-functions-list '(try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
    (hippie-expand prefix)))

