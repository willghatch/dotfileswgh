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

(defun sptw--delim-list (open?)
  (mapcar (if open? 'car 'cdr) sp-pair-list))

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

(defun sptw--at-open-delimiter-p ()
  "Is point at an opening smartparens delimiter?"
  (sptw--at-delim-p t nil))
(defun sptw--at-close-delimiter-p ()
  "Is point at the end of a closing smartparens delimiter? IE immediately after the last character."
  (sptw--at-delim-p nil t))
(defun sptw--before-close-delimiter-p ()
  (sptw--at-delim-p nil nil))


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

(defun sptw--move-if-advances (move fwd?)
  (let ((end (sptw--advances? move fwd?)))
    (when end
      (goto-char end)
      end)))

(defun sptw--movements-equal? (a b)
  (let ((am (save-excursion (funcall a) (point)))
        (bm (save-excursion (funcall b) (point))))
    (and (eql am bm) am)))


(defun sptw--at-end-of-symbol-sexp-probably ()
  "If the character before point is not a space, and after point is an end delimiter or space, we are probably at the end of a sexp."
  (and (not (bobp))
       (save-mark-and-excursion (backward-char 1)
                                (not (looking-at-p (rx space))))
       (or (sptw--before-close-delimiter-p)
           (looking-at-p (rx space))
           (looking-at-p "\n"))))

(defun sptw--at-sexp-end-probably ()
  (or (sptw--at-close-delimiter-p)
      (sptw--at-end-of-symbol-sexp-probably)))

(defun sptw--bounds-of-sexp-at-point (&optional point prefix-flag direction)
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
                                        ((sptw--at-open-delimiter-p) nil)
                                        ((sptw--at-close-delimiter-p) t)
                                        ((sptw--at-end-of-symbol-sexp-probably) t)
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

(defun sptw--bounds-of-delimited-sexp-at-point (&optional point)
  "Like `sptw--bounds-of-sexp-at-point' except if the sexp-at-point is a symbol sexp, then use the parent instead."
  (let ((bounds (sptw--bounds-of-sexp-at-point point 'never)))
    (and bounds
         (if (save-mark-and-excursion (goto-char (car bounds))
                                      (sptw--at-open-delimiter-p))
             bounds
           (save-mark-and-excursion (goto-char (car bounds))
                                    (and (tree-walk--motion-moved 'sptw-up-parent-beginning)
                                         (sptw--bounds-of-sexp-at-point (point) 'never)))))))

(defun sptw-move-to-current-sexp-beginning ()
  "Move to the beginning of the current sexp at point."
  (interactive)
  (let ((bounds (sptw--bounds-of-sexp-at-point)))
    (when bounds
      (goto-char (car bounds)))))

(defun sptw--bounds-of-sexp-children-at-point (&optional point)
  "Like `sptw--bounds-of-sexp-at-point' but trimmed to only the area inside the delimiters."
  (save-mark-and-excursion
    (when point (goto-char (point)))
    (save-mark-and-excursion
      (let ((bounds (sptw--bounds-of-sexp-at-point)))
        (when bounds
          (goto-char (car bounds))))
      (if (sptw--at-open-delimiter-p)
          (progn
            (sptw-down-first-child-beginning 1)
            (cons (progn (sp-beginning-of-sexp) (point))
                  (progn (sp-end-of-sexp) (point))))
        nil))))


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


(defun sptw--up-to-prefix ()
  (let ((bounds (sptw--bounds-of-sexp-at-point (point) 'always nil)))
    (and bounds
         (goto-char (car bounds)))))

(defun sptw--forward-sibling-beginning ()
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
                                    (not (sptw--before-close-delimiter-p))
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

(defun sptw--backward-sibling-beginning ()
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
                                  (not (sptw--before-close-delimiter-p))
                                  (not (looking-at-p (rx space)))
                                  (not (looking-at-p "\n"))
                                  prefix-beg)))
                    prefix-beg))))))
    (when new-point (goto-char new-point) new-point)))

(defun sptw--forward-sibling-end ()
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
                                (not (sptw--before-close-delimiter-p))
                                (not (looking-at-p (rx space)))
                                (not (looking-at-p "\n"))
                                end)))
                        (t end)))))))
    (when new-point (goto-char new-point) new-point)))

(defun sptw--backward-sibling-end ()
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
                           (not (sptw--before-close-delimiter-p))
                           (not (looking-at-p (rx space)))
                           (not (looking-at-p "\n"))
                           (progn
                             (goto-char prefix-beg)
                             (let* ((thing (sp-get-thing t))
                                    (end (and thing (plist-get thing ':end))))
                               (and end (< end (point)) end))))))))))))
    (when new-point (goto-char new-point) new-point)))

;;;###autoload (autoload 'sptw-forward-sibling-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sibling-beginning
                    sptw--forward-sibling-beginning
                    sptw-backward-sibling-beginning
                    "Move forward to the start of the next smartparens sexp sibling.")

;;;###autoload (autoload 'sptw-backward-sibling-beginning "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sibling-beginning
                    sptw--backward-sibling-beginning
                    sptw-forward-sibling-beginning
                    "Move backward to the start of the next smartparens sexp sibling.  Note that if not at the beginning of the current sexp, it will move to the beginning of the current, not actually to the next sibling.")

;;;###autoload (autoload 'sptw-forward-sibling-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-forward-sibling-end
                    sptw--forward-sibling-end
                    sptw-backward-sibling-end
                    "Move forward to the next end of a smartparens sexp sibling.  Note that if not at the end of the current sexp, it will move to the end of the current sexp, not actually to the next sibling.")

;;;###autoload (autoload 'sptw-backward-sibling-end "tree-walk-smartparens-integration.el" "" t)
(sptw--command-wrap sptw-backward-sibling-end
                    sptw--backward-sibling-end
                    sptw-forward-sibling-end
                    "Move backward to the next end of a smartparens sexp sibling.")

(defun sptw--up-sexp ()
  (or (sptw--move-if-advances 'sptw--up-to-prefix nil)
      (sp-backward-up-sexp)))
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
        (bounds (sptw--bounds-of-sexp-at-point)))
    (when (not (and bounds
                    (and (save-mark-and-excursion (goto-char (car bounds))
                                                  (sptw--at-open-delimiter-p)))
                    (funcall movement)
                    (<= (car bounds) (point) (cdr bounds))))
      (goto-char start-point))))

(defun sptw--down-from-prefix ()
  (let ((thing (sp-get-thing)))
    (when thing
      (let* ((beg (plist-get thing ':beg))
             (prefix (plist-get thing ':prefix))
             (pre-len (length prefix)))
        (when (and (not (equal 0 pre-len))
                   (equal (point) (- beg pre-len)))
          (goto-char beg)
          beg)))))

(defun sptw--down-sexp (&optional skip-prefix)
  (or (and (not skip-prefix)
           (sptw--down-from-prefix))
      (sptw--move-if-within-bounds-of-current-delimited-sexp
       (lambda ()
         (let ((close-delim (sptw--at-close-delimiter-p))
               (open-delim (sptw--at-open-delimiter-p)))
           (cond (open-delim
                  (sp-down-sexp))
                 (close-delim
                  (backward-char (length close-delim))
                  (sp-down-sexp))
                 (t (sp-down-sexp))))))))
(defun sptw--down-sexp-skip-prefix ()
  (sptw--down-sexp t))

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


(defun sptw-move-to-other-end-of-sexp ()
  "If at the beginning of a smartparens sexp, move to the end of the same sexp, and vice versa."
  (interactive)
  (cond ((sptw--at-open-delimiter-p) (sp-forward-sexp))
        ((sptw--at-close-delimiter-p) (sp-backward-sexp))
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
      (list (list 'sptw--at-open-delimiter-p
                  (lambda () (sptw--down-sexp t))
                  (lambda () (sptw-up-parent-beginning 1)))
            (list 'sptw--at-close-delimiter-p
                  (lambda () (sptw--down-sexp t))
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
  (if (and (sptw--at-sexp-end-probably)
           (not (sptw--at-open-delimiter-p)))
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
    (let ((bounds (sptw--bounds-of-delimited-sexp-at-point)))
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
 :def-ancestor-reorder sptw-ancestor-reorder
 :def-up-to-root sptw-up-to-root
 :def-select-root sptw-select-root

 :use-object-name "smartparens tree"

 :use-down-to-last-child (lambda () (and (tree-walk--motion-moved 'sptw-down-last-child-end)
                                         (let ((bounds (sptw--bounds-of-sexp-at-point (point) 'always)))
                                           (when bounds
                                             (goto-char (car bounds))))))

 :use-up-to-parent 'sptw-up-parent-beginning
 :use-down-to-first-child 'sptw-down-first-child-beginning
 :use-next-sibling 'sptw-forward-sibling-beginning
 :use-previous-sibling (lambda () (and (tree-walk--motion-moved 'sptw-backward-sibling-end)
                                       (sptw-backward-sibling-beginning)))
 :use-bounds 'sptw--bounds-of-sexp-at-point
 :use-children-bounds 'sptw--bounds-of-sexp-children-at-point
 )


(defun sptw--expand-region-until-predicate (expansion-func predicate &optional count)
  "Call EXPANSION-FUNC until PREDICATE is true.
If the expansion func stops expanding the region before the predicate is ever true, return to the original region and return nil.
If the predicate succeeds, leave the expanded region and return the new bounds."
  (let ((orig-bounds (if (region-active-p) (cons (region-beginning) (region-end)) (cons (point) (point))))
        inner-result
        result)
    (save-mark-and-excursion
      (dotimes (i (or count 1))
        (while (and (not inner-result)
                    (tree-walk--motion-moved-region expansion-func))
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

(defun sptw-expand-region-to-any-delimiter (&optional count)
  "Like `sptw-expand-region' but specifically expanding to a delimited region, not just something like a symbol."
  (interactive)
  (sptw--expand-region-until-predicate
   'sptw-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (if (region-active-p) (region-beginning) (point)))
                     (sptw--at-open-delimiter-p))))
   (or count 1)))

(defun sptw-expand-region-to-delimiter (delimiter &optional count)
  "DELIMITER must be an opening delimiter used by smartparens.
Expand region until hitting that specific delimiter."
  (sptw--expand-region-until-predicate
   'sptw-expand-region
   (lambda () (save-mark-and-excursion
                (and (region-active-p)
                     (goto-char (region-beginning))
                     (looking-at (regexp-quote delimiter)))))
   (or count 1)))

(defun sptw-expand-region-to-delimiter/children-region (delimiter &optional count)
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
                            (looking-at (regexp-quote delimiter))))))
   (or count 1)))

(defun sptw--open-sibling-extra-lines (bounds)
  "Get the number of extra lines to add.
This is already assuming that the sibling will have newlines added.
BOUNDS is the bounds of the current sexp."
  (or (let ((prev-sib-bounds (save-mark-and-excursion
                               (goto-char (car bounds))
                               (and (tree-walk--motion-moved 'sptw-backward-sibling-beginning)
                                    (sptw--bounds-of-sexp-at-point)))))
        (if prev-sib-bounds
            (let* ((sib-end-line (line-number-at-pos (cdr prev-sib-bounds)))
                   (this-start-line (line-number-at-pos (car bounds)))
                   (line-diff (- this-start-line sib-end-line)))
              (when (<= 2 line-diff)
                (- line-diff 1)))
          (let* ((next-sib-bounds (save-mark-and-excursion
                                    (goto-char (car bounds))
                                    (and (tree-walk--motion-moved 'sptw-forward-sibling-beginning)
                                         (sptw--bounds-of-sexp-at-point))))
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
         (bounds (sptw--bounds-of-sexp-at-point)))
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
                                  (sptw--bounds-of-delimited-sexp-at-point)))))
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
         (bounds (sptw--bounds-of-sexp-at-point)))
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
                                  (sptw--bounds-of-delimited-sexp-at-point)))))
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

(defun sptw--wrap-region-with-delimiter (delimiter beg end)
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

(defun sptw-wrap-with-delimiter (delimiter)
  (if (region-active-p)
      (sptw--wrap-region-with-delimiter
       delimiter (region-beginning) (region-end))
    (let ((bounds (sptw--bounds-of-sexp-at-point)))
      (and bounds
           (sptw--wrap-region-with-delimiter
            delimiter (car bounds) (cdr bounds))))))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'sptw-forward-inorder-traversal 'sptw-backward-inorder-traversal)
  (repeatable-motion-define-pair 'sptw-forward-sibling-beginning 'sptw-backward-sibling-beginning)
  (repeatable-motion-define-pair 'sptw-forward-sibling-end 'sptw-backward-sibling-end)
  (repeatable-motion-define-pair 'sptw-up-parent-beginning 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-up-parent-end 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-down-last-child-end 'sptw-up-parent-beginning)
  ;;(repeatable-motion-define-pair 'sptw-forward-sexp-in-supersexp 'sptw-backward-sexp-in-supersexp)
  )

(provide 'tree-walk-smartparens-integration)
