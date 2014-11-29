
;; Define whether I want to end everything with semicolons,
;; for my snippets to know whether to add them
(defvar-local js-want-semicolon nil)
(defun js-maybe-semicolon ()
  (if js-want-semicolon ";" ""))

