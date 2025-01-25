;;; -*- lexical-binding: t; -*-
(require 'sexp-rewrite)

(defmacro define-sexprw-tactic/func (name &rest parts)
  (unless (and name (symbolp name)) (error "d-s-t/f expected symbol, got: %S" name))
  (let ((funcname (intern (concat "sexprw-tactic/" (symbol-name name)))) )
    `(progn (define-sexprw-tactic ,name ,@parts)
            (defun ,funcname ()
              (interactive)
              (sexprw-execute-tactic ',name 1)))))


;;; These are not tactics I'll re-use, they are just tactics I used while testing the package.

(define-sexprw-tactic/func parse-result-test-upgrade
                           (->results (parse* (open-input-string $str)
                                              $parser))
                           (p*/r $str !SL $parser))

(define-sexprw-tactic/func whole-parse-result-test-upgrade
                           (->results (whole-parse* (open-input-string $str)
                                                    $parser))
                           (wp*/r $str !SL $parser))


(require 'racket-rewrites)
