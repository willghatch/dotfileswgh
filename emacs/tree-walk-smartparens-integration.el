;; -*- lexical-binding: t; -*-

;; Smartparens is awesome!
;; But its motions typically don't strictly respect tree boundaries.
;; Eg. it doesn't have a motion to go specifically to a sibling, it will just walk out of the current node in a tree using sp-forward-sexp.
;; So this file defines some helpers for using smartparens in ways that respect tree structure more strictly.
;; Also, define movements that specifically go to the beginning or ending of sexps, rather than the emacs way of going to the end when forward, and the beginning when backward.
;;
;; This is based on my old on-parens library, which existed to do those same things, but also deal with the evil-mode/vim/vi quirk of the cursor being “on” characters in non-insert modes.
;; The library was a quick hack then, and is now, too.
;; It would be more efficient to use smartparens internals, rather than doing motions and looking at the results.
;; I should maybe see if smartparens has a stable public API for programming against without using the motions.
;; But that would take more time than just slightly editing the thing I already wrote over a decade ago.

(require 'dash)
(require 'smartparens)
(require 'tree-walk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Section for getting needed info out of smartparens settings

(defun sptw--get-specs (sp-def)
  ;; return a list of smartparen specs that are global or for an active mode
  (let ((mode (car sp-def)))
    (if (or
         (equal t mode)
         (equal mode major-mode)
         (-contains? minor-mode-list mode))
        (cdr sp-def)
      nil)))
(defun sptw--get-delim-from-spec (sp-spec open?)
  ;; get the delimiter from the spec
  (let ((kw (if open? :open :close)))
    (cond ((not sp-spec) nil)
          ((equal (car sp-spec) kw) (cadr sp-spec))
          (t (sptw--get-delim-from-spec (cdr sp-spec) open?)))))

(defun sptw--delim-list (open?)
  (let ((get-delim (lambda (x) (sptw--get-delim-from-spec x open?))))
    (-flatten (-map (lambda (def)
                      (-map get-delim
                            (sptw--get-specs def)))
                    sp-pairs))))

(defun sptw--at-delim-p (open? at-end?)
  "If at smartparens delimiter, return the delimiter string.
If OPEN?, use the opening delimiter, else the closing delimiter.
If AT-END?, check if point is immediately after the last character of the delimiter, else check if point is immediately before the first character of the delimiter.
Return nil if not at any current smartparens delimiter.
"
  (-reduce-from (lambda (prev cur)
                  (if prev prev
                    (and (save-mark-and-excursion
                           (when at-end? (backward-char (length cur)))
                           (looking-at-p (regexp-quote cur)))
                         cur)))
                nil
                (sptw--delim-list open?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates!

(defun sptw-at-open-beginning? ()
  "Is point at an opening smartparens delimiter?"
  (sptw--at-delim-p t nil))
(defun sptw-at-close-end? ()
  "Is point at the end of a closing smartparens delimiter? IE immediately after the last character."
  (sptw--at-delim-p nil t))
(defun sptw-at-delimiter? ()
  "Is point at the beginning of a smartparens open delimiter or the end of a smartparens close delimiter?"
  (or (sptw-at-open-beginning?) (sptw-at-close-end?)))


(defun sptw--advances? (move fwd?)
  ;; does this movement function move the direction we expect?
  (save-excursion
    (let ((cur-point (point))
          (end-point (progn
                       (funcall move)
                       (point))))
      (if fwd?
          (and (> end-point cur-point) end-point)
        (and (< end-point cur-point) end-point)))))

(defun sptw--movements-equal? (a b)
  (let ((am (save-excursion (funcall a) (point)))
        (bm (save-excursion (funcall b) (point))))
    (and (eql am bm) am)))

(defun sptw--from-close-on-last-sexp? ()
  (sptw--movements-equal?
   (lambda ()
     (sp-forward-sexp))
   (lambda ()
     (sp-up-sexp))))

(defun sptw--from-open-on-last-sexp? ()
  (sptw--advances? 'sp-next-sexp nil))
(defun sptw--on-last-sexp? ()
  (cond ((sptw-at-open-beginning?) (sptw--from-open-on-last-sexp?))
        ((sptw-at-close-end?) (sptw--from-close-on-last-sexp?))
        (t (sptw--on-last-symbol-sexp?))))

(defun sptw--from-open-on-first-sexp? ()
  (sptw--advances? 'sp-previous-sexp t))
(defun sptw--from-close-on-first-sexp? ()
  (sptw--advances? (lambda ()
                          (sp-previous-sexp)
                          (sp-previous-sexp))
                        t))
(defun sptw--on-first-sexp? ()
  (cond ((sptw-at-open-beginning?) (sptw--from-open-on-first-sexp?))
        ((sptw-at-close-end?) (sptw--from-close-on-first-sexp?))
        ;; they can't all be the same because sp functions act differently in
        ;; the middle of symbols
        (t (sptw--at-end-of-last-symbol-sexp?))))

(defun sptw--on-start-of-symbol-sexp? ()
  ;; If backward-sexp then forward-sexp nets a backward movement, it means
  ;; we were at the start of the symbol.
  (sptw--advances? (lambda ()
                          (sp-backward-sexp)
                          (sp-forward-sexp))
                        nil))
(defun sptw--at-end-of-symbol-sexp? ()
  (sptw--movements-equal?
   'ignore (lambda () (sp-backward-sexp) (sp-forward-sexp))))
(defun sptw--on-start-of-first-symbol-sexp? ()
  (sptw--movements-equal?
   'sp-backward-sexp 'sp-backward-up-sexp))
(defun sptw--at-end-of-last-symbol-sexp? ()
  (sptw--movements-equal?
   'sp-up-sexp (lambda () (sp-forward-sexp))))
(defun sptw--on-first-symbol-sexp? ()
  (sptw--movements-equal?
   'sp-forward-sexp (lambda () (sp-beginning-of-sexp) (sp-forward-sexp))))
(defun sptw--on-last-symbol-sexp? ()
  (or (sptw--movements-equal?
       'sp-backward-sexp (lambda () (sp-end-of-sexp) (sp-backward-sexp)))
      ;; IE on the first char of the last symbol-sexp
      (sptw--movements-equal?
       'ignore (lambda () (sp-end-of-sexp) (sp-backward-sexp)))))

(defun sptw-at-sexp-beginning? ()
  "Return true if point is at the beginning of a smartparens sexp.  (IE delimited list or symbol-like object.)"
  (or (sptw-at-open-beginning?)
      (sptw--on-start-of-symbol-sexp?)))

(defun sptw-at-sexp-end? ()
  "Return true if point is at the end of a smartparens sexp.  (IE delimited list or symbol-like object.)
IE point is immediately after the end.
Note that point can be both at the end and start of two different sexps, and commands prefer using the sexp that point is at the start of."
  (or (sptw-at-close-end?)
      (sptw--at-end-of-symbol-sexp?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motion commands

(defmacro sptw--command-wrap (name command opposite docs)
  ;; Wrapper code to make a command that takes an optionally negative
  ;; prefix argument, because they're all the same
  `(defun ,name (&optional arg)
     ,docs
     (interactive "p")
     (if (< arg 0)
         (,opposite (abs arg))
       (dotimes (_i arg)
         (,command)))))

(defun sptw--sptw-use-move-if-advances (move fwd)
  (let ((new-point (sptw--advances? move fwd)))
    (when new-point
      (goto-char new-point))))

(defun sptw--forward-sexp-from-on-open ()
  (sptw--sptw-use-move-if-advances 'sp-next-sexp t))
(defun sptw--forward-sexp-from-at-close ()
  (let ((move (lambda () (sp-next-sexp))))
    (sptw--sptw-use-move-if-advances move t)))
;; if not on delimiters, use normal sp-next-sexp
(defun sptw--forward-sexp ()
  (cond ((sptw-at-open-beginning?) (sptw--forward-sexp-from-on-open))
        ((sptw-at-close-end?) (sptw--forward-sexp-from-at-close))
        (t (unless (sptw--on-last-symbol-sexp?)
             (sp-forward-sexp)
             (sp-next-sexp)))))
;;;###autoload (autoload 'sptw-forward-sexp "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sexp-sibling-beginning
                    sptw--forward-sexp
                    sptw-backward-sexp-sibling-beginning
                    "Move forward to the start of the next smartparens sexp sibling.")

(defun sptw--backward-sexp-from-on-open ()
  (unless (sptw--on-first-sexp?)
    (sp-backward-sexp)))
(defun sptw--backward-sexp-from-at-close ()
  (sp-backward-sexp))
(defun sptw--backward-sexp ()
  (cond ((sptw-at-open-beginning?) (sptw--backward-sexp-from-on-open))
        ((sptw-at-close-end?) (sptw--backward-sexp-from-at-close))
        (t (unless (sptw--movements-equal? 'sp-backward-sexp
                                           'sp-backward-up-sexp)
             (sp-backward-sexp)))))
;;;###autoload (autoload 'sptw-backward-sexp "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sexp-sibling-beginning
                    sptw--backward-sexp
                    sptw-forward-sexp-sibling-beginning
                    "Move backward to the start of the next smartparens sexp sibling.")

(defun sptw--forward-sexp-end-from-on-open ()
  (sp-forward-sexp))
(defun sptw--forward-sexp-end-from-at-close ()
  (unless (sptw--on-last-sexp?)
    (sp-forward-sexp)))
(defun sptw--forward-sexp-end-else ()
  (unless (sptw--at-end-of-last-symbol-sexp?)
    (sp-forward-sexp)))
(defun sptw--forward-sexp-end ()
  (cond ((sptw-at-open-beginning?) (sptw--forward-sexp-end-from-on-open))
        ((sptw-at-close-end?) (sptw--forward-sexp-end-from-at-close))
        (t (sptw--forward-sexp-end-else))))
;;;###autoload (autoload 'sptw-forward-sexp-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sexp-sibling-end
                    sptw--forward-sexp-end
                    sptw-backward-sexp-sibling-end
                    "Move forward to the next end of a smartparens sexp sibling.")

(defun sptw--backward-sexp-end-from-on-open ()
  (unless (sptw--on-first-sexp?)
    (sptw--backward-sexp-from-on-open)
    (sptw--forward-sexp-end)))
(defun sptw--backward-sexp-end-from-at-close ()
  (unless (sptw--on-first-sexp?)
    (sp-backward-sexp)
    (sptw--backward-sexp-end-from-on-open)))
(defun sptw--backward-sexp-end-else ()
  (unless (sptw--on-first-symbol-sexp?)
    (unless (sptw--on-start-of-symbol-sexp?)
      (sptw--backward-sexp))
    (sptw--backward-sexp)
    (sptw--forward-sexp-end)))
(defun sptw--backward-sexp-end ()
  (cond ((sptw-at-open-beginning?) (sptw--backward-sexp-end-from-on-open))
        ((sptw-at-close-end?) (sptw--backward-sexp-end-from-at-close))
        (t (sptw--backward-sexp-end-else))))
;;;###autoload (autoload 'sptw-backward-sexp-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sexp-sibling-end
                    sptw--backward-sexp-end
                    sptw-forward-sexp-sibling-end
                    "Move backward to the next end of a smartparens sexp sibling.")

(defun sptw--up-sexp ()
  (sp-backward-up-sexp))
;;;###autoload (autoload 'sptw-up-sexp "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-up-sexp-beginning
                    ;; TODO - rename to up-sexp-beginning
                    sptw--up-sexp
                    sptw-down-sexp-beginning
                    "Move up to the start of the containing smartparens sexp.")
(defun sptw--up-sexp-end ()
  (let ((start-point (point)))
    (sp-up-sexp)
    (if (> start-point (point))
        ;; If we went backward, then we didn't actually go up to a closer, so go back to start.
        (goto-char start-point))))
;;;###autoload (autoload 'sptw-up-sexp-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-up-sexp-end
                    sptw--up-sexp-end
                    sptw-down-sexp-beginning
                    "Move up to the end of the containing smartparens sexp.")

(defun sptw--down-sexp ()
  ;; Surprise!!!
  (sp-down-sexp))
;;;###autoload (autoload 'sptw-down-sexp "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-down-sexp-beginning
                    sptw--down-sexp
                    sptw-up-sexp-beginning
                    "Move down to the beginning of the contained smartparens sexp.")
(defun sptw--down-sexp-end ()
  (sp-down-sexp)
  (sp-end-of-sexp)
  (backward-char))
;;;###autoload (autoload 'sptw-down-sexp-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-down-sexp-end
                    sptw--down-sexp-end
                    sptw-up-sexp-beginning
                    "Move down to the end of the contained smartparens sexp.")

;(defun sptw--forward-sexp-in-supersexp ()
;  (sptw--up-sexp)
;  (sptw--forward-sexp)
;  (sptw--down-sexp))
;(defun sptw--backward-sexp-in-supersexp ()
;  (sptw--up-sexp)
;  (sptw--backward-sexp-end)
;  (sptw--down-sexp))
;;;;###autoload (autoload 'sptw-forward-sexp-in-supersexp "tree-walk-smartparens-integration.el" "" t)
;(sptw--command-wrap sptw-forward-sexp-in-supersexp
;                    sptw--forward-sexp-in-supersexp
;                    sptw-backward-sexp-in-supersexp
;                    "up, forward, down")
;;;;###autoload (autoload 'sptw-backward-sexp-in-supersexp "tree-walk-smartparens-integration.el" "" t)
;(sptw--command-wrap sptw-backward-sexp-in-supersexp
;                    sptw--backward-sexp-in-supersexp
;                    sptw-forward-sexp-in-supersexp
;                    "up, backward, down")

(defun sptw-move-to-other-end-of-sexp ()
  "If at the beginning of a smartparens sexp, move to the end of the same sexp, and vice versa."
  (interactive)
  (cond ((sptw-at-open-beginning?) (sp-forward-sexp))
        ((sptw-at-close-end?) (sp-backward-sexp))
        ;; TODO - swap on symbols and stuff, too.
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing commands!

(defmacro sptw--maybe-forwarded (name forward-p action)
  `(defun ,name (&optional arg)
     (interactive "p")
     (let ((fwd (,forward-p)))
       (when fwd
         (forward-char))
       (,action arg)
       (when fwd
         (backward-char)))))

;; TODO - consider placement required for these ops, and where point lands at end.

;;;###autoload (autoload 'sptw-forward-slurp "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-forward-slurp
                       sptw-at-open-beginning?
                       sp-forward-slurp-sexp)
;;;###autoload (autoload 'sptw-backward-slurp "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-backward-slurp
                       sptw-at-open-beginning?
                       sp-backward-slurp-sexp)
;;;###autoload (autoload 'sptw-forward-barf "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-forward-barf
                       sptw-at-open-beginning?
                       sp-forward-barf-sexp)
;;;###autoload (autoload 'sptw-backward-barf "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-backward-barf
                       sptw-at-open-beginning?
                       sp-backward-barf-sexp)
;;;###autoload (autoload 'sptw-splice "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-splice
                       sptw-at-open-beginning?
                       sp-splice-sexp)
;;;###autoload (autoload 'sptw-split-supersexp "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-split-supersexp
                       sptw-at-close-end?
                       sp-split-sexp)
;;;###autoload (autoload 'sptw-join-neighbor-sexp "tree-walk-smartparens-integration.el" "" t)
(sptw--maybe-forwarded sptw-join-neighbor-sexp
                       sptw-at-close-end?
                       sp-join-sexp)
;;;###autoload
(defun sptw-kill-sexp (&optional arg)
  "Like sp-kill-sexp, except if at the end of a smartparens sexp, kill the sexp that you're at the end of rather than its parent."
  (interactive "p")
  (if (and (sptw-at-sexp-end?)
           (not (sptw-at-sexp-beginning?)))
      (progn
        (backward-char)
        (sp-kill-sexp arg))
    (sp-kill-sexp arg)))





(provide 'tree-walk-smartparens-integration)





;; use prefix sptw for now


;(put 'smartparens-sexp 'forward-op 'sp-forward-sexp)


;(tree-walk-define-operations
; :inorder-forward TODO
; :inorder-backward TODO
; :down-to-last-descendant TODO
; :no-end-inner-object TODO
; :no-end-outer-object TODO
;
; :def-expand-region TODO
; :def-expand-region-idempotent TODO
; :def-select-children-once TODO
; :def-expand-region-to-children/ancestor-generation TODO
;
; ;; TODO - I probably need to wrap these to constrain their behavior
; :up-to-parent sp-up-sexp
; :down-to-first-child sp-down-sexp
; :down-to-last-child TODO
; :next-sibling sp-forward-sexp
; :previous-sibling sp-backward-sexp
; :bounds-func-use (lambda () (bounds-of-thing-at-point 'smartparens-sexp))
; :children-bounds-func-use (lambda ()
;                             ;; TODO - this is wrong because I need to detect errors and force failure, but it should look something like this.
;                             (save-mark-and-excursion
;                               (sp-down-sexp)
;                               (cons (save-mark-and-excursion (sp-beginning-of-sexp) (point))
;                                     (save-mark-and-excursion (sp-end-of-sexp) (point)))))
; )
