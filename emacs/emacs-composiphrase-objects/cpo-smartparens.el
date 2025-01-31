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
(require 'cpo-tree-walk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Section for getting needed info out of smartparens settings

(defun cpo-smartparens--delim-list (open?)
  (mapcar (if open? 'car 'cdr) sp-pair-list))

(defun cpo-smartparens--at-delim-p (open? at-end?)
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
              (cpo-smartparens--delim-list open?)
              nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates!

(defun cpo-smartparens--at-open-delimiter-p ()
  "Is point at an opening smartparens delimiter?"
  (cpo-smartparens--at-delim-p t nil))
(defun cpo-smartparens--at-close-delimiter-p ()
  "Is point at the end of a closing smartparens delimiter? IE immediately after the last character."
  (cpo-smartparens--at-delim-p nil t))
(defun cpo-smartparens--before-close-delimiter-p ()
  (cpo-smartparens--at-delim-p nil nil))


(defun cpo-smartparens--advances? (move fwd?)
  ;; does this movement function move the direction we expect?
  (save-excursion
    (let ((cur-point (point))
          (end-point (progn
                       (funcall move)
                       (point))))
      (if fwd?
          (and (> end-point cur-point) end-point)
        (and (< end-point cur-point) end-point)))))

(defun cpo-smartparens--move-if-advances (move fwd?)
  (let ((end (cpo-smartparens--advances? move fwd?)))
    (when end
      (goto-char end)
      end)))

(defun cpo-smartparens--movements-equal? (a b)
  (let ((am (save-excursion (funcall a) (point)))
        (bm (save-excursion (funcall b) (point))))
    (and (eql am bm) am)))


(defun cpo-smartparens--at-end-of-symbol-sexp-probably ()
  "If the character before point is not a space, and after point is an end delimiter or space, we are probably at the end of a sexp."
  (and (not (bobp))
       (save-mark-and-excursion (backward-char 1)
                                (not (looking-at-p (rx space))))
       (or (cpo-smartparens--before-close-delimiter-p)
           (looking-at-p (rx space))
           (looking-at-p "\n"))))

(defun cpo-smartparens--at-sexp-end-probably ()
  (or (cpo-smartparens--at-close-delimiter-p)
      (cpo-smartparens--at-end-of-symbol-sexp-probably)))

(defun cpo-smartparens--bounds-of-sexp-at-point (&optional point prefix-flag direction)
  "Return the bounds of the smartparens sexp at point as (beg . end), or nil if there is no sexp at point.
Note that if point is just before an opening delimiter or just after a closing delimiter, the sexp will be the one for those delimiters.
But if point is both after an end delimiter and before an open delimiter, it will prefer the open delimiter.

PREFX-FLAG can be nil for choice based on point, 'always to include the prefix, 'never to not include the prefix.
DIRECTION can be nil to detect, 'forward, or 'backward.
"
  (let ((point (or point (point))))
    (save-mark-and-excursion
      (goto-char point)
      (let* ((thing (sp-get-thing (cond ((equal direction 'forward) nil)
                                        ((equal direction 'backward) t)
                                        ((cpo-smartparens--at-open-delimiter-p) nil)
                                        ((cpo-smartparens--at-close-delimiter-p) t)
                                        ((cpo-smartparens--at-end-of-symbol-sexp-probably) t)
                                        (t nil))))
             (beg (and thing (plist-get thing ':beg)))
             (end (and thing (plist-get thing ':end)))
             (prefix (and thing (plist-get thing ':prefix)))
             (suffix (and thing (plist-get thing ':suffix)))
             (beg-including-prefix (and beg (if prefix (- beg (length prefix)) beg)))
             (end-including-suffix (and end (if suffix (+ end (length suffix)) end))))
        ;; TODO - suffix
        (cond ((or (not beg) (not end)) nil)
              ((equal prefix-flag 'always) (cons beg-including-prefix end))
              ((equal prefix-flag 'never) (cons beg end))
              ((<= beg point) (cons beg end))
              (t (cons beg-including-prefix end)))))))

(defun cpo-smartparens--bounds-of-delimited-sexp-at-point (&optional point)
  "Like `cpo-smartparens--bounds-of-sexp-at-point' except if the sexp-at-point is a symbol sexp, then use the parent instead."
  (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point point 'never)))
    (and bounds
         (if (save-mark-and-excursion (goto-char (car bounds))
                                      (cpo-smartparens--at-open-delimiter-p))
             bounds
           (save-mark-and-excursion (goto-char (car bounds))
                                    (and (cpo-tree-walk--motion-moved 'cpo-smartparens-up-parent-beginning)
                                         (cpo-smartparens--bounds-of-sexp-at-point (point) 'never)))))))

(defun cpo-smartparens-move-to-current-sexp-beginning ()
  "Move to the beginning of the current sexp at point."
  (interactive)
  (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point)))
    (when bounds
      (goto-char (car bounds)))))

(defun cpo-smartparens--bounds-of-sexp-children-at-point (&optional point)
  "Like `cpo-smartparens--bounds-of-sexp-at-point' but trimmed to only the area inside the delimiters."
  (save-mark-and-excursion
    (when point (goto-char (point)))
    (save-mark-and-excursion
      (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point)))
        (when bounds
          (goto-char (car bounds))))
      (if (cpo-smartparens--at-open-delimiter-p)
          (progn
            (cpo-smartparens-down-first-child-beginning 1)
            (cons (progn (sp-beginning-of-sexp) (point))
                  (progn (sp-end-of-sexp) (point))))
        nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Motion commands

(defmacro cpo-smartparens--command-wrap (name command opposite docs)
  ;; Wrapper code to make a command that takes an optionally negative
  ;; prefix argument, because they're all the same
  `(defun ,name (&optional arg)
     ,docs
     (interactive "p")
     (cond ((not (integerp arg)) (,command))
           ((< arg 0) (,opposite (abs arg)))
           (t (dotimes (_i arg)
                (,command))))))


(defun cpo-smartparens--up-to-prefix ()
  (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point (point) 'always nil)))
    (and bounds
         (goto-char (car bounds)))))

(defun cpo-smartparens--forward-sibling-beginning ()
  (let ((new-point
         (save-mark-and-excursion
           (let* ((thing (sp-get-thing))
                  (beg (and thing (plist-get thing ':beg)))
                  (prefix-beg (and beg (- beg (length (plist-get thing ':prefix))))))
             (and thing
                  (cond
                   ((<= prefix-beg (point) beg)
                    (goto-char (plist-get thing ':end))
                    (let* ((thing (sp-get-thing))
                           (beg (and thing (plist-get thing ':beg))))
                      (and beg (< (point) beg) (- beg (length (plist-get thing ':prefix))))))
                   ((< beg (point))
                    (let ((back-thing (sp-get-thing t)))
                      (and back-thing
                           (if (and (equal beg
                                           (plist-get back-thing ':beg))
                                    (not (cpo-smartparens--before-close-delimiter-p))
                                    (not (looking-at-p (rx space)))
                                    (not (looking-at-p "\n")))
                               (progn
                                 ;; If thing and back-thing have the same beginning, then either point was in the middle of the thing, or point was in an empty list.
                                 (goto-char (plist-get thing ':end))
                                 (let* ((thing (sp-get-thing))
                                        (beg (and thing (plist-get thing ':beg))))
                                   (and beg (< (point) beg)
                                        (- beg (length (plist-get thing ':prefix))))))
                             nil))))
                   (t prefix-beg)))))))
    (when new-point (goto-char new-point) new-point)))

(defun cpo-smartparens--backward-sibling-beginning ()
  (let ((new-point
         (save-mark-and-excursion
           (let* ((thing (sp-get-thing t))
                  (beg (and thing (plist-get thing ':beg)))
                  (prefix-beg (and beg (- beg (length (plist-get thing ':prefix)))))
                  (end (and thing (plist-get thing ':end)))
                  )
             (and thing
                  (if (< (point) end)
                      (let* ((fwd-thing (sp-get-thing)))
                        (and fwd-thing
                             (and (equal end (plist-get fwd-thing ':end))
                                  (not (cpo-smartparens--before-close-delimiter-p))
                                  (not (looking-at-p (rx space)))
                                  (not (looking-at-p "\n"))
                                  prefix-beg)))
                    prefix-beg))))))
    (when new-point (goto-char new-point) new-point)))

(defun cpo-smartparens--forward-sibling-end ()
  (let ((new-point
         (save-mark-and-excursion
           (let* ((thing (sp-get-thing))
                  (beg (and thing (plist-get thing ':beg)))
                  (prefix-beg (and beg (- beg (length (plist-get thing ':prefix)))))
                  (end (and thing (plist-get thing ':end)))
                  )
             (and thing
                  (cond ((<= prefix-beg (point) beg)
                         end)
                        ((<= beg (point) end)
                         (let* ((back-thing (sp-get-thing t)))
                           (and back-thing
                                (equal end (plist-get back-thing ':end))
                                (not (cpo-smartparens--before-close-delimiter-p))
                                (not (looking-at-p (rx space)))
                                (not (looking-at-p "\n"))
                                end)))
                        (t end)))))))
    (when new-point (goto-char new-point) new-point)))

(defun cpo-smartparens--backward-sibling-end ()
  (let ((new-point
         (save-mark-and-excursion
           (let* ((thing (sp-get-thing t))
                  (beg (and thing (plist-get thing ':beg)))
                  (end (and thing (plist-get thing ':end)))
                  (prefix-beg (and beg (- beg (length (plist-get thing ':prefix))))))
             (and thing
                  (cond
                   ((equal end (point))
                    (goto-char prefix-beg)
                    (let* ((thing (sp-get-thing t))
                           (end (and thing (plist-get thing ':end))))
                      (and end (< end (point)) end)))
                   ((< end (point))
                    end)
                   (t
                    (let* ((fwd-thing (sp-get-thing)))
                      (and fwd-thing
                           (equal end (plist-get fwd-thing ':end))
                           (not (cpo-smartparens--before-close-delimiter-p))
                           (not (looking-at-p (rx space)))
                           (not (looking-at-p "\n"))
                           (progn
                             (goto-char prefix-beg)
                             (let* ((thing (sp-get-thing t))
                                    (end (and thing (plist-get thing ':end))))
                               (and end (< end (point)) end))))))))))))
    (when new-point (goto-char new-point) new-point)))

;;;###autoload (autoload 'cpo-smartparens-forward-sibling-beginning "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-forward-sibling-beginning
                    cpo-smartparens--forward-sibling-beginning
                    cpo-smartparens-backward-sibling-beginning
                    "Move forward to the start of the next smartparens sexp sibling.")

;;;###autoload (autoload 'cpo-smartparens-backward-sibling-beginning "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-backward-sibling-beginning
                    cpo-smartparens--backward-sibling-beginning
                    cpo-smartparens-forward-sibling-beginning
                    "Move backward to the start of the next smartparens sexp sibling.  Note that if not at the beginning of the current sexp, it will move to the beginning of the current, not actually to the next sibling.")

;;;###autoload (autoload 'cpo-smartparens-forward-sibling-end "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-forward-sibling-end
                    cpo-smartparens--forward-sibling-end
                    cpo-smartparens-backward-sibling-end
                    "Move forward to the next end of a smartparens sexp sibling.  Note that if not at the end of the current sexp, it will move to the end of the current sexp, not actually to the next sibling.")

;;;###autoload (autoload 'cpo-smartparens-backward-sibling-end "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-backward-sibling-end
                    cpo-smartparens--backward-sibling-end
                    cpo-smartparens-forward-sibling-end
                    "Move backward to the next end of a smartparens sexp sibling.")

(defun cpo-smartparens--up-sexp ()
  (or (cpo-smartparens--move-if-advances 'cpo-smartparens--up-to-prefix nil)
      (sp-backward-up-sexp)))
;;;###autoload (autoload 'cpo-smartparens-up-parent-beginning "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-up-parent-beginning
                    cpo-smartparens--up-sexp
                    cpo-smartparens-down-first-child-beginning
                    "Move up to the start of the containing smartparens sexp.")
(defun cpo-smartparens--up-sexp-end ()
  (let ((start-point (point)))
    (sp-up-sexp)
    (if (> start-point (point))
        ;; If we went backward, then we didn't actually go up to a closer, so go back to start.
        (goto-char start-point))))
;;;###autoload (autoload 'cpo-smartparens-up-parent-end "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-up-parent-end
                    cpo-smartparens--up-sexp-end
                    cpo-smartparens-down-first-child-beginning
                    "Move up to the end of the containing smartparens sexp.")

(defun cpo-smartparens--move-if-within-bounds-of-current-delimited-sexp (movement)
  (let ((start-point (point))
        (bounds (cpo-smartparens--bounds-of-sexp-at-point)))
    (when (not (and bounds
                    (and (save-mark-and-excursion (goto-char (car bounds))
                                                  (cpo-smartparens--at-open-delimiter-p)))
                    (funcall movement)
                    (<= (car bounds) (point) (cdr bounds))))
      (goto-char start-point))))

(defun cpo-smartparens--down-from-prefix ()
  (let ((thing (sp-get-thing)))
    (when thing
      (let* ((beg (plist-get thing ':beg))
             (prefix (plist-get thing ':prefix))
             (pre-len (length prefix)))
        (when (and (not (equal 0 pre-len))
                   (equal (point) (- beg pre-len)))
          (goto-char beg)
          beg)))))

(defun cpo-smartparens--down-sexp (&optional skip-prefix)
  (or (and (not skip-prefix)
           (cpo-smartparens--down-from-prefix))
      (cpo-smartparens--move-if-within-bounds-of-current-delimited-sexp
       (lambda ()
         (let ((close-delim (cpo-smartparens--at-close-delimiter-p))
               (open-delim (cpo-smartparens--at-open-delimiter-p)))
           (cond (open-delim
                  (sp-down-sexp))
                 (close-delim
                  (backward-char (length close-delim))
                  (sp-down-sexp))
                 (t (sp-down-sexp))))))))
(defun cpo-smartparens--down-sexp-skip-prefix ()
  (cpo-smartparens--down-sexp t))

;;;###autoload (autoload 'cpo-smartparens-down-first-child-beginning "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-down-first-child-beginning
                    cpo-smartparens--down-sexp
                    cpo-smartparens-up-parent-beginning
                    "Move down to the beginning of the contained smartparens sexp.")
(defun cpo-smartparens--down-last-sexp-end ()
  (and (cpo-tree-walk--motion-moved 'cpo-smartparens--down-sexp)
       (sp-end-of-sexp)))
(defun cpo-smartparens--down-last-sexp-beginning ()
  (and (cpo-tree-walk--motion-moved 'cpo-smartparens--down-sexp)
       (sp-end-of-sexp)
       (cpo-smartparens-backward-sibling-beginning)))
;;;###autoload (autoload 'cpo-smartparens-down-last-child-end "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-down-last-child-end
                    cpo-smartparens--down-last-sexp-end
                    cpo-smartparens-up-parent-beginning
                    "Move down to the end of the contained smartparens sexp.")
;;;###autoload (autoload 'cpo-smartparens-down-last-child-beginning "cpo-smartparens.el" "" t)
(cpo-smartparens--command-wrap cpo-smartparens-down-last-child-beginning
                    cpo-smartparens--down-last-sexp-beginning
                    cpo-smartparens-up-parent-beginning
                    "Move down to the end of the contained smartparens sexp.")


(defun cpo-smartparens-move-to-other-end-of-sexp ()
  "If at the beginning of a smartparens sexp, move to the end of the same sexp, and vice versa."
  (interactive)
  (cond ((cpo-smartparens--at-open-delimiter-p) (sp-forward-sexp))
        ((cpo-smartparens--at-close-delimiter-p) (sp-backward-sexp))
        ;; TODO - swap on symbols and stuff, too.
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; editing commands!

(defun cpo-smartparens--action-move-wrap (action mod-list action-arg)
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

(defmacro cpo-smartparens--def-action-with-adjusted-bounds (name action)
  "Define NAME that wraps ACTION, but it handles being at the begin/end of a thing differently.
Specifically it moves inside the parens."
  `(defun ,name (&optional interactive-arg)
     (interactive "p")
     (cpo-smartparens--action-move-wrap
      ,action
      (list (list 'cpo-smartparens--at-open-delimiter-p
                  (lambda () (cpo-smartparens--down-sexp t))
                  (lambda () (cpo-smartparens-up-parent-beginning 1)))
            (list 'cpo-smartparens--at-close-delimiter-p
                  (lambda () (cpo-smartparens--down-sexp t))
                  (lambda () (cpo-smartparens-up-parent-end 1))))
      interactive-arg)))


;;;###autoload (autoload 'cpo-smartparens-forward-slurp "cpo-smartparens.el" "" t)
(cpo-smartparens--def-action-with-adjusted-bounds
 cpo-smartparens-forward-slurp
 'sp-forward-slurp-sexp)
;;;###autoload (autoload 'cpo-smartparens-backward-slurp "cpo-smartparens.el" "" t)
(cpo-smartparens--def-action-with-adjusted-bounds
 cpo-smartparens-backward-slurp
 'sp-backward-slurp-sexp)
;;;###autoload (autoload 'cpo-smartparens-forward-barf "cpo-smartparens.el" "" t)
(cpo-smartparens--def-action-with-adjusted-bounds
 cpo-smartparens-forward-barf
 'sp-forward-barf-sexp)
;;;###autoload (autoload 'cpo-smartparens-backward-barf "cpo-smartparens.el" "" t)
(cpo-smartparens--def-action-with-adjusted-bounds
 cpo-smartparens-backward-barf
 'sp-backward-barf-sexp)
;;;###autoload (autoload 'cpo-smartparens-splice "cpo-smartparens.el" "" t)
(cpo-smartparens--def-action-with-adjusted-bounds
 cpo-smartparens-splice
 'sp-splice-sexp)

;;;###autoload (autoload 'cpo-smartparens-kill-sexp "cpo-smartparens.el" "" t)
(defun cpo-smartparens-kill-sexp (&optional arg)
  "Like sp-kill-sexp, except if at the end of a smartparens sexp, kill the sexp that you're at the end of rather than its parent."
  (interactive "p")
  (if (and (cpo-smartparens--at-sexp-end-probably)
           (not (cpo-smartparens--at-open-delimiter-p)))
      (progn
        (backward-char)
        (sp-kill-sexp arg))
    (sp-kill-sexp arg)))


;; sp-split-sexp is defined in a useful way that works for the model that I want, it doesn't need a wrapper.

;;;###autoload (autoload 'cpo-smartparens-join-sexp-forward "cpo-smartparens.el" "" t)
(defun cpo-smartparens-join-sexp-forward (&optional count)
  "Join the delimited sexp at point with the one before it, if they have the same delimiter type."
  (interactive "p")
  (let ((count (or count 1)))
    (let ((bounds (cpo-smartparens--bounds-of-delimited-sexp-at-point)))
      (when bounds
        (goto-char (if (<= 0 count) (cdr bounds) (car bounds)))
        (sp-join-sexp count)))))
;;;###autoload (autoload 'cpo-smartparens-join-sexp-backward "cpo-smartparens.el" "" t)
(defun cpo-smartparens-join-sexp-backward (&optional count)
  "Join the delimited sexp at point with the one after it, if they have the same delimiter type."
  (interactive "p")
  (cpo-smartparens-join-sexp-forward (- (or count 1))))


(cpo-tree-walk-define-operations
 ;; For all of these cpo-tree-walk operations, we move to the beginning of the tree node.
 ;; TODO - the traversal is inconsistent in handling string contents for forward vs backward.
 :def-inorder-forward cpo-smartparens-forward-inorder-traversal
 :def-inorder-backward cpo-smartparens-backward-inorder-traversal
 :def-down-to-last-descendant cpo-smartparens-down-to-last-descendant

 :def-expand-region cpo-smartparens-expand-region
 :def-expand-region-idempotent cpo-smartparens-expand-region-idempotent
 :def-select-children-once cpo-smartparens-select-children-region-idempotent
 :def-expand-region-to-children/ancestor-generation cpo-smartparens-expand-region/children-region
 ;;:def-down-to-last-child cpo-smartparens-down-to-last-child-beginning
 :def-transpose-sibling-forward cpo-smartparens-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-smartparens-transpose-sibling-backward
 :def-ancestor-reorder cpo-smartparens-ancestor-reorder
 :def-up-to-root cpo-smartparens-up-to-root
 :def-select-root cpo-smartparens-select-root

 :use-object-name "smartparens tree"

 :use-down-to-last-child (lambda () (and (cpo-tree-walk--motion-moved 'cpo-smartparens-down-last-child-end)
                                         (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point (point) 'always)))
                                           (when bounds
                                             (goto-char (car bounds))))))

 :use-up-to-parent 'cpo-smartparens-up-parent-beginning
 :use-down-to-first-child 'cpo-smartparens-down-first-child-beginning
 :use-next-sibling 'cpo-smartparens-forward-sibling-beginning
 :use-previous-sibling (lambda () (and (cpo-tree-walk--motion-moved 'cpo-smartparens-backward-sibling-end)
                                       (cpo-smartparens-backward-sibling-beginning)))
 :use-bounds 'cpo-smartparens--bounds-of-sexp-at-point
 :use-children-bounds 'cpo-smartparens--bounds-of-sexp-children-at-point
 )


(defun cpo-smartparens--expand-region-until-predicate (expansion-func predicate &optional count)
  "Call EXPANSION-FUNC until PREDICATE is true.
If the expansion func stops expanding the region before the predicate is ever true, return to the original region and return nil.
If the predicate succeeds, leave the expanded region and return the new bounds."
  (let ((orig-bounds (if (region-active-p) (cons (region-beginning) (region-end)) (cons (point) (point))))
        inner-result
        result)
    (save-mark-and-excursion
      (dotimes (i (or count 1))
        (while (and (not inner-result)
                    (cpo-tree-walk--motion-moved-region expansion-func))
          (if (funcall predicate)
              (setq inner-result (cons (point) (mark)))
            ;; Keep expanding until predicate is true or expansion stops
            nil))
        (setq result inner-result)
        (setq inner-result nil)))
    (if result
        (progn
          ;; Go to the new point/mark, but return the region canonicalized to (beg . end).
          (goto-char (car result))
          (set-mark (cdr result))
          (if (<= (car result) (cdr result))
              result
            (cons (cdr result) (car result))))
      nil)))

(defun cpo-smartparens-expand-region-to-any-delimiter (&optional count)
  "Like `cpo-smartparens-expand-region' but specifically expanding to a delimited region, not just something like a symbol."
  (interactive)
  (cpo-smartparens--expand-region-until-predicate
   'cpo-smartparens-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (if (region-active-p) (region-beginning) (point)))
                     (cpo-smartparens--at-open-delimiter-p))))
   (or count 1)))

(defun cpo-smartparens-expand-region-to-delimiter (delimiter &optional count)
  "DELIMITER must be an opening delimiter used by smartparens.
Expand region until hitting that specific delimiter."
  (cpo-smartparens--expand-region-until-predicate
   'cpo-smartparens-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (region-beginning))
                     (looking-at (regexp-quote delimiter)))))
   (or count 1)))

(defun cpo-smartparens-expand-region-to-delimiter/children-region (delimiter &optional count)
  "DELIMITER must be an opening delimiter used by smartparens.
Expand region until hitting that specific delimiter.
Except only expand to the inner area inside the parens."
  (cpo-smartparens--expand-region-until-predicate
   'cpo-smartparens-expand-region/children-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (region-beginning))
                     (< (length delimiter) (point))
                     (progn (backward-char (length delimiter))
                            (looking-at (regexp-quote delimiter))))))
   (or count 1)))

(defun cpo-smartparens--open-sibling-extra-lines (bounds)
  "Get the number of extra lines to add.
This is already assuming that the sibling will have newlines added.
BOUNDS is the bounds of the current sexp."
  (or (let ((prev-sib-bounds (save-mark-and-excursion
                               (goto-char (car bounds))
                               (and (cpo-tree-walk--motion-moved 'cpo-smartparens-backward-sibling-beginning)
                                    (cpo-smartparens--bounds-of-sexp-at-point)))))
        (if prev-sib-bounds
            (let* ((sib-end-line (line-number-at-pos (cdr prev-sib-bounds)))
                   (this-start-line (line-number-at-pos (car bounds)))
                   (line-diff (- this-start-line sib-end-line)))
              (when (<= 2 line-diff)
                (- line-diff 1)))
          (let* ((next-sib-bounds (save-mark-and-excursion
                                    (goto-char (car bounds))
                                    (and (cpo-tree-walk--motion-moved 'cpo-smartparens-forward-sibling-beginning)
                                         (cpo-smartparens--bounds-of-sexp-at-point))))
                 (sib-start-line (and next-sib-bounds
                                      (line-number-at-pos (car next-sib-bounds))))
                 (this-end-line (line-number-at-pos (cdr bounds)))
                 (line-diff (and sib-start-line (- sib-start-line this-end-line))))
            (when (and line-diff
                       (<= 2 line-diff))
              (- line-diff 1)))))
      0))

(defun cpo-smartparens-open-sibling-forward ()
  (interactive)
  (let* ((same-line t)
         (extra-lines 0)
         (bounds (cpo-smartparens--bounds-of-sexp-at-point)))
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
                             (and (cpo-tree-walk--motion-moved 'cpo-smartparens-up-parent-beginning)
                                  (cpo-smartparens--bounds-of-delimited-sexp-at-point)))))
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
      (setq extra-lines (cpo-smartparens--open-sibling-extra-lines bounds)))

    (goto-char (cdr bounds))
    (if same-line
        (progn (insert " "))
      (progn
        (dotimes (i extra-lines) (insert "\n"))
        (newline-and-indent)))))

(defun cpo-smartparens-open-sibling-backward ()
  (interactive)
  (let* ((same-line t)
         (extra-lines 0)
         (bounds (cpo-smartparens--bounds-of-sexp-at-point)))
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
                             (and (cpo-tree-walk--motion-moved 'cpo-smartparens-up-parent-beginning)
                                  (cpo-smartparens--bounds-of-delimited-sexp-at-point)))))
        (when (and parent-bounds (not (equal (line-number-at-pos (car parent-bounds))
                                             (line-number-at-pos (cdr parent-bounds)))))
          (setq same-line nil))))

    (when (not same-line)
      (setq extra-lines (cpo-smartparens--open-sibling-extra-lines bounds)))

    (goto-char (car bounds))
    (if same-line
        (progn (insert " ")
               (backward-char 1))
      (let ((indent (current-indentation)))
        (insert "\n")
        (dotimes (i indent) (insert " "))
        (goto-char (car bounds))
        (dotimes (i extra-lines) (insert "\n") (backward-char 1))))))

(defun cpo-smartparens--wrap-region-with-delimiter (delimiter beg end)
  (let* ((orig-point (point))
         (pair-for-delim (seq-find (lambda (x)
                                     (or (equal delimiter (car x))
                                         (equal delimiter (cdr x))))
                                   sp-pair-list))
         (open (car pair-for-delim))
         (close (cdr pair-for-delim))
         (change-group (prepare-change-group)))
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)
    (cond ((<= end orig-point)
           (goto-char (+ orig-point (length open) (length close))))
          ((< beg orig-point)
           (goto-char (+ orig-point (length open))))
          (t (goto-char orig-point)))
    (undo-amalgamate-change-group change-group)))

(defun cpo-smartparens-wrap-with-delimiter (delimiter)
  (if (region-active-p)
      (cpo-smartparens--wrap-region-with-delimiter
       delimiter (region-beginning) (region-end))
    (let ((bounds (cpo-smartparens--bounds-of-sexp-at-point)))
      (and bounds
           (cpo-smartparens--wrap-region-with-delimiter
            delimiter (car bounds) (cdr bounds))))))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-smartparens-forward-inorder-traversal 'cpo-smartparens-backward-inorder-traversal)
  (repeatable-motion-define-pair 'cpo-smartparens-forward-sibling-beginning 'cpo-smartparens-backward-sibling-beginning)
  (repeatable-motion-define-pair 'cpo-smartparens-forward-sibling-end 'cpo-smartparens-backward-sibling-end)
  (repeatable-motion-define-pair 'cpo-smartparens-up-parent-beginning 'cpo-smartparens-down-first-child-beginning)
  (repeatable-motion-define 'cpo-smartparens-up-parent-end 'cpo-smartparens-down-first-child-beginning)
  (repeatable-motion-define 'cpo-smartparens-down-last-child-end 'cpo-smartparens-up-parent-beginning)
  ;;(repeatable-motion-define-pair 'cpo-smartparens-forward-sexp-in-supersexp 'cpo-smartparens-backward-sexp-in-supersexp)
  )

(provide 'cpo-smartparens)
