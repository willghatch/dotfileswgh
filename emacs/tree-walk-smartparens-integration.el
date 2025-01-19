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
;;
;; TODO - improve handling of prefix quotes, unquotes, etc.

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
         (memq mode minor-mode-list))
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
    (apply #'append
           ;; The below returns a list of lists of specs.
           (mapcar (lambda (def)
                     (mapcar get-delim
                             (sptw--get-specs def)))
                   sp-pairs))))

(defun sptw--at-delim-p (open? at-end?)
  "If at smartparens delimiter, return the delimiter string.
If OPEN?, use the opening delimiter, else the closing delimiter.
If AT-END?, check if point is immediately after the last character of the delimiter, else check if point is immediately before the first character of the delimiter.
Return nil if not at any current smartparens delimiter.
"
  (seq-reduce (lambda (prev cur)
                (if prev prev
                  (and (save-mark-and-excursion
                         (when at-end? (backward-char (length cur)))
                         (looking-at-p (regexp-quote cur)))
                       cur)))
              (sptw--delim-list open?)
              nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates!

(defun sptw-at-open-delimiter-p ()
  "Is point at an opening smartparens delimiter?"
  (sptw--at-delim-p t nil))
(defun sptw-at-close-delimiter-p ()
  "Is point at the end of a closing smartparens delimiter? IE immediately after the last character."
  (sptw--at-delim-p nil t))
(defun sptw-at-delimiter-p ()
  "Is point at the beginning of a smartparens open delimiter or the end of a smartparens close delimiter?"
  (or (sptw-at-open-delimiter-p) (sptw-at-close-delimiter-p)))


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
  (cond ((sptw-at-open-delimiter-p) (sptw--from-open-on-last-sexp?))
        ((sptw-at-close-delimiter-p) (sptw--from-close-on-last-sexp?))
        (t (sptw--on-last-symbol-sexp?))))

(defun sptw--from-open-on-first-sexp? ()
  (sptw--advances? 'sp-previous-sexp t))
(defun sptw--from-close-on-first-sexp? ()
  (sptw--advances? (lambda ()
                     (sp-previous-sexp)
                     (sp-previous-sexp))
                   t))
(defun sptw--on-first-sexp? ()
  (cond ((sptw-at-open-delimiter-p) (sptw--from-open-on-first-sexp?))
        ((sptw-at-close-delimiter-p) (sptw--from-close-on-first-sexp?))
        ;; they can't all be the same because sp functions act differently in
        ;; the middle of symbols
        (t (sptw--at-end-of-last-symbol-sexp?))))

(defun sptw--at-start-of-symbol-sexp-if-not-at-other-start? ()
  "Predicate for whether point is at the start of a symbol sexp, but it is hacky and was only ever meant as a fallback for testing after having tested for whether point is at the start of a delimited sexp."
  (and (not (looking-at (rx space)))
       ;; If backward-sexp then forward-sexp nets a backward movement, it means
       ;; we were at the start of the symbol.
       (sptw--advances? (lambda ()
                          (sp-backward-sexp)
                          (sp-forward-sexp))
                        nil)))
(defun sptw--at-end-of-symbol-sexp-if-not-at-other-end? ()
  "Similar to `sptw--at-start-of-symbol-sexp-if-not-at-other-start?', hack."
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

(defun sptw-at-sexp-beginning-p ()
  "Return true if point is at the beginning of a smartparens sexp.  (IE delimited list or symbol-like object.)"
  (or (sptw-at-open-delimiter-p)
      (sptw--at-start-of-symbol-sexp-if-not-at-other-start?)))

(defun sptw-at-sexp-end-p ()
  "Return true if point is at the end of a smartparens sexp.  (IE delimited list or symbol-like object.)
IE point is immediately after the end.
Note that point can be both at the end and start of two different sexps, and commands prefer using the sexp that point is at the start of."
  (or (sptw-at-close-delimiter-p)
      (sptw--at-end-of-symbol-sexp-if-not-at-other-end?)))

(defun sptw-bounds-of-sexp-at-point (&optional point)
  "Return the bounds of the smartparens sexp at point as (beg . end), or nil if there is no sexp at point.
Note that if point is just before an opening delimiter or just after a closing delimiter, the sexp will be the one for those delimiters.
But if point is both after an end delimiter and before an open delimiter, it will prefer the open delimiter."
  (let ((point (or point (point))))
    (save-mark-and-excursion
      (goto-char point)
      (cond ((sptw-at-sexp-beginning-p)
             (cons (point) (save-mark-and-excursion (sp-forward-sexp) (point))))
            ;; TODO - sptw-at-sexp-end-p is causing a problem here for cases where the end delimiter matches the opening delimiter.
            ((sptw-at-sexp-end-p)
             (cons (save-mark-and-excursion (sp-backward-sexp) (point)) (point)))
            (t
             (let ((orig-point (point)))
               (save-mark-and-excursion
                 (sp-forward-sexp)
                 (let ((end-point (point)))
                   (sp-backward-sexp)
                   (if (<= (point) orig-point end-point)
                       (cons (point) end-point)
                     nil)))))))))

(defun sptw-bounds-of-delimited-sexp-at-point (&optional point)
  "Like `sptw-bounds-of-sexp-at-point' except if the sexp-at-point is a symbol sexp, then use the parent instead."
  (let ((bounds (sptw-bounds-of-sexp-at-point point)))
    (and bounds
         (if (save-mark-and-excursion (goto-char (car bounds))
                                      (sptw-at-open-delimiter-p))
             bounds
           (save-mark-and-excursion (goto-char (car bounds))
                                    (and (tree-walk--motion-moved 'sptw-up-parent-beginning)
                                         (sptw-bounds-of-sexp-at-point)))))))

(defun sptw-move-to-current-sexp-beginning ()
  "Move to the beginning of the current sexp at point."
  (interactive)
  (let ((bounds (sptw-bounds-of-sexp-at-point)))
    (when bounds
      (goto-char (car bounds)))))

(defun sptw-bounds-of-sexp-children-at-point ()
  "Like `sptw-bounds-of-sexp-at-point' but trimmed to only the area inside the delimiters."
  (save-mark-and-excursion
    (sptw-move-to-current-sexp-beginning)
    (if (sptw-at-open-delimiter-p)
        (progn
          (sptw-down-first-child-beginning 1)
          (cons (progn (sp-beginning-of-sexp) (point))
                (progn (sp-end-of-sexp) (point))))
      nil)))

(defun sptw-bounds-of-sexp-children (point)
  (save-mark-and-excursion
    (goto-char point)
    (sptw-bounds-of-sexp-children-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motion commands

(defmacro sptw--command-wrap (name command opposite docs)
  ;; Wrapper code to make a command that takes an optionally negative
  ;; prefix argument, because they're all the same
  `(defun ,name (&optional arg)
     ,docs
     (interactive "p")
     (cond ((not (integerp arg)) (,command))
           ((< arg 0) (,opposite (abs arg)))
           (t (dotimes (_i arg)
                (,command))))))

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
  (cond ((sptw-at-open-delimiter-p) (sptw--forward-sexp-from-on-open))
        ((sptw-at-close-delimiter-p) (sptw--forward-sexp-from-at-close))
        (t (unless (sptw--on-last-symbol-sexp?)
             (sp-forward-sexp)
             (sp-next-sexp)))))
;;;###autoload (autoload 'sptw-forward-sibling-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sibling-beginning
                    sptw--forward-sexp
                    sptw-backward-sibling-beginning
                    "Move forward to the start of the next smartparens sexp sibling.")

(defun sptw--backward-sexp-from-on-open ()
  (unless (sptw--on-first-sexp?)
    (sp-backward-sexp)))
(defun sptw--backward-sexp-from-at-close ()
  (sp-backward-sexp))
(defun sptw--backward-sexp ()
  (cond ((sptw-at-open-delimiter-p) (sptw--backward-sexp-from-on-open))
        ((sptw-at-close-delimiter-p) (sptw--backward-sexp-from-at-close))
        (t (unless (sptw--movements-equal? 'sp-backward-sexp
                                           'sp-backward-up-sexp)
             (sp-backward-sexp)))))
;;;###autoload (autoload 'sptw-backward-sibling-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sibling-beginning
                    sptw--backward-sexp
                    sptw-forward-sibling-beginning
                    "Move backward to the start of the next smartparens sexp sibling.  Note that if not at the beginning of the current sexp, it will move to the beginning of the current, not actually to the next sibling.")

(defun sptw--forward-sexp-end-from-on-open ()
  (sp-forward-sexp))
(defun sptw--forward-sexp-end-from-at-close ()
  (unless (sptw--on-last-sexp?)
    (sp-forward-sexp)))
(defun sptw--forward-sexp-end-else ()
  (unless (sptw--at-end-of-last-symbol-sexp?)
    (sp-forward-sexp)))
(defun sptw--forward-sexp-end ()
  (cond ((sptw-at-open-delimiter-p) (sptw--forward-sexp-end-from-on-open))
        ((sptw-at-close-delimiter-p) (sptw--forward-sexp-end-from-at-close))
        (t (sptw--forward-sexp-end-else))))
;;;###autoload (autoload 'sptw-forward-sibling-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sibling-end
                    sptw--forward-sexp-end
                    sptw-backward-sibling-end
                    "Move forward to the next end of a smartparens sexp sibling.  Note that if not at the end of the current sexp, it will move to the end of the current sexp, not actually to the next sibling.")

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
    (unless (sptw--at-start-of-symbol-sexp-if-not-at-other-start?)
      (sptw--backward-sexp))
    (sptw--backward-sexp)
    (sptw--forward-sexp-end)))
(defun sptw--backward-sexp-end ()
  (cond ((sptw-at-open-delimiter-p) (sptw--backward-sexp-end-from-on-open))
        ((sptw-at-close-delimiter-p) (sptw--backward-sexp-end-from-at-close))
        (t (sptw--backward-sexp-end-else))))
;;;###autoload (autoload 'sptw-backward-sibling-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sibling-end
                    sptw--backward-sexp-end
                    sptw-forward-sibling-end
                    "Move backward to the next end of a smartparens sexp sibling.")

(defun sptw--up-sexp ()
  (sp-backward-up-sexp))
;;;###autoload (autoload 'sptw-up-parent-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-up-parent-beginning
                    sptw--up-sexp
                    sptw-down-first-child-beginning
                    "Move up to the start of the containing smartparens sexp.")
(defun sptw--up-sexp-end ()
  (let ((start-point (point)))
    (sp-up-sexp)
    (if (> start-point (point))
        ;; If we went backward, then we didn't actually go up to a closer, so go back to start.
        (goto-char start-point))))
;;;###autoload (autoload 'sptw-up-parent-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-up-parent-end
                    sptw--up-sexp-end
                    sptw-down-first-child-beginning
                    "Move up to the end of the containing smartparens sexp.")

(defun sptw--move-if-within-bounds-of-current-delimited-sexp (movement)
  (let ((start-point (point))
        (bounds (sptw-bounds-of-sexp-at-point)))
    (when (not (and bounds
                    (and (save-mark-and-excursion (goto-char (car bounds))
                                                  (sptw-at-open-delimiter-p)))
                    (funcall movement)
                    (<= (car bounds) (point) (cdr bounds))))
      (goto-char start-point))))

(defun sptw--down-sexp ()
  (sptw--move-if-within-bounds-of-current-delimited-sexp
   (lambda ()
     (if (and (sptw-at-sexp-end-p)
              (not (sptw-at-sexp-beginning-p)))
         (progn
           (backward-char)
           (sp-down-sexp))
       (sp-down-sexp)))))

;;;###autoload (autoload 'sptw-down-first-child-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-down-first-child-beginning
                    sptw--down-sexp
                    sptw-up-parent-beginning
                    "Move down to the beginning of the contained smartparens sexp.")
(defun sptw--down-last-sexp-end ()
  (and (tree-walk--motion-moved 'sptw--down-sexp)
       (sp-end-of-sexp)))
(defun sptw--down-last-sexp-beginning ()
  (and (tree-walk--motion-moved 'sptw--down-sexp)
       (sp-end-of-sexp)
       (sptw-backward-sibling-beginning)))
;;;###autoload (autoload 'sptw-down-last-child-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-down-last-child-end
                    sptw--down-last-sexp-end
                    sptw-up-parent-beginning
                    "Move down to the end of the contained smartparens sexp.")
;;;###autoload (autoload 'sptw-down-last-child-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-down-last-child-beginning
                    sptw--down-last-sexp-beginning
                    sptw-up-parent-beginning
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
  (cond ((sptw-at-open-delimiter-p) (sp-forward-sexp))
        ((sptw-at-close-delimiter-p) (sp-backward-sexp))
        ;; TODO - swap on symbols and stuff, too.
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing commands!

(defun sptw--action-move-wrap (action mod-list action-arg)
  "mod-list is a list of lists like (predicate pre-move post-move).
If no predicate matches, just do the action.
If a predicate matches, the first match is used, and pre-move is done before action and post-move is done after it.
"
  (let ((matched (cl-find-if (lambda (x) (funcall (car x))) mod-list)))
    (if matched
        (progn
          (funcall (cadr matched))
          (funcall action action-arg)
          (funcall (caddr matched)))
      (funcall action action-arg))))

(defmacro sptw--def-action-with-adjusted-bounds (name action)
  "Define NAME that wraps ACTION, but it handles being at the begin/end of a thing differently.
Specifically it moves inside the parens."
  `(defun ,name (&optional interactive-arg)
     (interactive "p")
     (sptw--action-move-wrap
      ,action
      (list (list 'sptw-at-open-delimiter-p
                  (lambda () (sptw-down-first-child-beginning 1))
                  (lambda () (sptw-up-parent-beginning 1)))
            (list 'sptw-at-close-delimiter-p
                  (lambda () (sptw-down-first-child-beginning 1))
                  (lambda () (sptw-up-parent-end 1))))
      interactive-arg)))


;;;###autoload (autoload 'sptw-forward-slurp "tree-walk-smartparens-integration.el" "" t)
(sptw--def-action-with-adjusted-bounds
 sptw-forward-slurp
 'sp-forward-slurp-sexp)
;;;###autoload (autoload 'sptw-backward-slurp "tree-walk-smartparens-integration.el" "" t)
(sptw--def-action-with-adjusted-bounds
 sptw-backward-slurp
 'sp-backward-slurp-sexp)
;;;###autoload (autoload 'sptw-forward-barf "tree-walk-smartparens-integration.el" "" t)
(sptw--def-action-with-adjusted-bounds
 sptw-forward-barf
 'sp-forward-barf-sexp)
;;;###autoload (autoload 'sptw-backward-barf "tree-walk-smartparens-integration.el" "" t)
(sptw--def-action-with-adjusted-bounds
 sptw-backward-barf
 'sp-backward-barf-sexp)
;;;###autoload (autoload 'sptw-splice "tree-walk-smartparens-integration.el" "" t)
(sptw--def-action-with-adjusted-bounds
 sptw-splice
 'sp-splice-sexp)

;;;###autoload (autoload 'sptw-kill-sexp "tree-walk-smartparens-integration.el" "" t)
(defun sptw-kill-sexp (&optional arg)
  "Like sp-kill-sexp, except if at the end of a smartparens sexp, kill the sexp that you're at the end of rather than its parent."
  (interactive "p")
  (if (and (sptw-at-sexp-end-p)
           (not (sptw-at-sexp-beginning-p)))
      (progn
        (backward-char)
        (sp-kill-sexp arg))
    (sp-kill-sexp arg)))


;; sp-split-sexp is defined in a useful way that works for the model that I want, it doesn't need a wrapper.

;;;###autoload (autoload 'sptw-join-sexp-forward "tree-walk-smartparens-integration.el" "" t)
(defun sptw-join-sexp-forward (&optional count)
  "Join the delimited sexp at point with the one before it, if they have the same delimiter type."
  (interactive "p")
  (let ((count (or count 1)))
    (let ((bounds (sptw-bounds-of-delimited-sexp-at-point)))
      (when bounds
        (goto-char (if (<= 0 count) (cdr bounds) (car bounds)))
        (sp-join-sexp count)))))
;;;###autoload (autoload 'sptw-join-sexp-backward "tree-walk-smartparens-integration.el" "" t)
(defun sptw-join-sexp-backward (&optional count)
  "Join the delimited sexp at point with the one after it, if they have the same delimiter type."
  (interactive "p")
  (sptw-join-sexp-forward (- (or count 1))))


(tree-walk-define-operations
 ;; For all of these tree-walk operations, we move to the beginning of the tree node.
 ;; TODO - the traversal is inconsistent in handling string contents for forward vs backward.
 :def-inorder-forward sptw-forward-inorder-traversal
 :def-inorder-backward sptw-backward-inorder-traversal
 :def-down-to-last-descendant sptw-down-to-last-descendant

 :def-expand-region sptw-expand-region
 :def-expand-region-idempotent sptw-expand-region-idempotent
 :def-select-children-once sptw-select-children-region-idempotent
 :def-expand-region-to-children/ancestor-generation sptw-expand-region/children-region
 ;;:def-down-to-last-child sptw-down-to-last-child-beginning
 :def-transpose-sibling-forward sptw-transpose-sibling-forward
 :def-transpose-sibling-backward sptw-transpose-sibling-backward

 :use-down-to-last-child (lambda () (and (tree-walk--motion-moved 'sptw-down-last-child-end)
                                         (sptw-move-to-current-sexp-beginning)))

 :use-up-to-parent 'sptw-up-parent-beginning
 :use-down-to-first-child 'sptw-down-first-child-beginning
 :use-next-sibling 'sptw-forward-sibling-beginning
 :use-previous-sibling (lambda () (and (tree-walk--motion-moved 'sptw-backward-sibling-end)
                                       (sptw-backward-sibling-beginning)))
 :use-bounds 'sptw-bounds-of-sexp-at-point
 :use-children-bounds 'sptw-bounds-of-sexp-children
 )


(defun sptw--expand-region-until-predicate (expansion-func predicate)
  "Call EXPANSION-FUNC until PREDICATE is true.
If the expansion func stops expanding the region before the predicate is ever true, return to the original region and return nil.
If the predicate succeeds, leave the expanded region and return the new bounds."
  (let ((orig-bounds (if (region-active-p) (cons (region-beginning) (region-end)) (cons (point) (point))))
        result)
    (save-mark-and-excursion
      (while (and (not result)
                  (tree-walk--motion-moved-region expansion-func))
        (if (funcall predicate)
            (setq result (cons (point) (mark)))
          ;; Keep expanding until predicate is true or expansion stops
          nil)))
    (if result
        (progn
          ;; Go to the new point/mark, but return the region canonicalized to (beg . end).
          (goto-char (car result))
          (set-mark (cdr result))
          (if (<= (car result) (cdr result))
              result
            (cons (cdr result) (car result))))
      nil)))

(defun sptw-expand-region-to-any-delimiter ()
  "Like `sptw-expand-region' but specifically expanding to a delimited region, not just something like a symbol."
  (interactive)
  (sptw--expand-region-until-predicate
   'sptw-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (if (region-active-p) (region-beginning) (point)))
                     (sptw-at-open-delimiter-p))))))

(defun sptw-expand-region-to-delimiter (delimiter)
  "DELIMITER must be an opening delimiter used by smartparens.
Expand region until hitting that specific delimiter."
  (sptw--expand-region-until-predicate
   'sptw-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (region-beginning))
                     (looking-at (regexp-quote delimiter)))))))

(defun sptw-expand-region-to-delimiter/children-region (delimiter)
  "DELIMITER must be an opening delimiter used by smartparens.
Expand region until hitting that specific delimiter.
Except only expand to the inner area inside the parens."
  (sptw--expand-region-until-predicate
   'sptw-expand-region/children-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (region-beginning))
                     (< (length delimiter) (point))
                     (progn (backward-char (length delimiter))
                            (looking-at (regexp-quote delimiter))))))))

(defun sptw--open-sibling-extra-lines (bounds)
  "Get the number of extra lines to add.
This is already assuming that the sibling will have newlines added.
BOUNDS is the bounds of the current sexp."
  (or (let ((prev-sib-bounds (save-mark-and-excursion
                               (goto-char (car bounds))
                               (and (tree-walk--motion-moved 'sptw-backward-sibling-beginning)
                                    (sptw-bounds-of-sexp-at-point)))))
        (if prev-sib-bounds
            (let* ((sib-end-line (line-number-at-pos (cdr prev-sib-bounds)))
                   (this-start-line (line-number-at-pos (car bounds)))
                   (line-diff (- this-start-line sib-end-line)))
              (when (<= 2 line-diff)
                (- line-diff 1)))
          (let* ((next-sib-bounds (save-mark-and-excursion
                                    (goto-char (car bounds))
                                    (and (tree-walk--motion-moved 'sptw-forward-sibling-beginning)
                                         (sptw-bounds-of-sexp-at-point))))
                 (sib-start-line (and next-sib-bounds
                                      (line-number-at-pos (car next-sib-bounds))))
                 (this-end-line (line-number-at-pos (cdr bounds)))
                 (line-diff (and sib-start-line (- sib-start-line this-end-line))))
            (when (and line-diff
                       (<= 2 line-diff))
              (- line-diff 1)))))
      0))

(defun sptw-open-sibling-forward ()
  (interactive)
  (let* ((same-line t)
         (extra-lines 0)
         (bounds (sptw-bounds-of-sexp-at-point)))
    (when (not bounds)
      (error "No smartparens object at point."))
    (when (not (equal (line-number-at-pos (car bounds))
                      (line-number-at-pos (cdr bounds))))
      ;; When the current sexp is multi-line, the sibling should be on a new line.
      (setq same-line nil))
    (when (and same-line
               (save-mark-and-excursion (goto-char (car bounds)) (bolp)))
      ;; When the current sexp starts at the beginning of the line, if things
      ;; are properly indented, it probably means that the sibling should be on
      ;; the next line.
      (setq same-line nil))
    (when same-line
      (let ((parent-bounds (save-mark-and-excursion
                             (and (tree-walk--motion-moved 'sptw-up-parent-beginning)
                                  (sptw-bounds-of-delimited-sexp-at-point)))))
        (when (and parent-bounds (not (equal (line-number-at-pos (car parent-bounds))
                                             (line-number-at-pos (cdr parent-bounds)))))
          ;; When the parent is on a different line, the next sibling should
          ;; probably be on a new line.
          (setq same-line nil))))
    (when (and same-line
               ;; TODO - make this column number adjustable...
               (<= 80
                   (- (cdr bounds) (save-mark-and-excursion
                                     (beginning-of-line) (point)))))
      (setq same-line nil))

    (when (not same-line)
      (setq extra-lines (sptw--open-sibling-extra-lines bounds)))

    (goto-char (cdr bounds))
    (if same-line
        (progn (insert " "))
      (progn
        (dotimes (i extra-lines) (insert "\n"))
        (newline-and-indent)))))

(defun sptw-open-sibling-backward ()
  (interactive)
  (let* ((same-line t)
         (extra-lines 0)
         (bounds (sptw-bounds-of-sexp-at-point)))
    (when (not bounds)
      (error "No smartparens object at point."))
    (when (not (equal (line-number-at-pos (car bounds))
                      (line-number-at-pos (cdr bounds))))
      (setq same-line nil))
    (when (and same-line
               (save-mark-and-excursion (goto-char (car bounds)) (bolp)))
      (setq same-line nil))
    (when same-line
      (let ((parent-bounds (save-mark-and-excursion
                             (and (tree-walk--motion-moved 'sptw-up-parent-beginning)
                                  (sptw-bounds-of-delimited-sexp-at-point)))))
        (when (and parent-bounds (not (equal (line-number-at-pos (car parent-bounds))
                                             (line-number-at-pos (cdr parent-bounds)))))
          (setq same-line nil))))

    (when (not same-line)
      (setq extra-lines (sptw--open-sibling-extra-lines bounds)))

    (goto-char (car bounds))
    (if same-line
        (progn (insert " ")
               (backward-char 1))
      (let ((indent (current-indentation)))
        (insert "\n")
        (dotimes (i indent) (insert " "))
        (goto-char (car bounds))
        (dotimes (i extra-lines) (insert "\n") (backward-char 1))))))

(provide 'tree-walk-smartparens-integration)
