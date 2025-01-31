;;; indent-tree.el --- TODO - description here -*- lexical-binding: t; -*-

;; Note that it will not work gracefully for improper trees (with
;; indentation levels skipped for some nodes).  This can only happen
;; for the first child of any node.
;; eg:
;;   root
;;      indented more than root's next child!
;;     normal child indentation
;;
;; Or more accurately, there can be an arbitrary number of “half-sibling” child
;; nodes for indentation (and org-mode) trees, where an early child is indented
;; more than a later half-sibling.  There can be half-sibling sets.
;; Eg:
;;   root
;;       4 deep
;;       4 deep again, full sibling to above
;;      3 deep, half sibling to above
;;       4 deep, child of 3 deep
;;      3 deep, full sibling to previous 3 deep
;;     2 deep, half sibling of both top 4 deep and both above 3 deep
;;


(require 'cpo-tree-walk)
(require 'repeatable-motion)

(defun cpo-indent-tree--current-line-whitespace-only-p ()
  (string-match-p "^\\s-*$"
                  (buffer-substring (line-beginning-position)
                                    (line-end-position))))

(defun cpo-indent-tree-forward-full-sibling (num)
  "Go forward to full sibling of indent-tree node at point NUM times, going backward for negative NUM.
Return the (positive) number of iterations that could NOT be done (IE returns 0 for full success).
Notably this will stop if it hits a half-sibling boundary."
  (interactive "p")
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        (direction (if (< 0 num) 1 -1))
        (times (abs num))
        (index 0)
        (successful t))
    (while (and (< index times)
                successful)
      (while (and (zerop (forward-line direction))
                  (or (> (current-indentation) start-indent)
                      ;; TODO - maybe this should be configurable as to whether it ignores empty lines and/or lines with indentation but nothing after
                      ;; current-line-empty
                      ;;(equal (line-beginning-position) (line-end-position))
                      ;; white space only line
                      (cpo-indent-tree--current-line-whitespace-only-p)
                      )))
      (if (or (not (= (current-indentation) start-indent))
              ;; TODO - without this, top-level movement can go to a blank line after the last top-level tree.  That could be useful, but violates my intention of making this a tree motion...
              (cpo-indent-tree--current-line-whitespace-only-p))
          (progn
            (setq successful nil)
            (goto-char backtrack-pos))
        (progn
          (back-to-indentation)
          (setq backtrack-pos (point))))
      (when successful (setq index (+ 1 index))))
    (- times index)))

(defun cpo-indent-tree-backward-full-sibling (direction)
  "The reverse of `cpo-indent-tree-forward-full-sibling'."
  (interactive "p")
  (cpo-indent-tree-forward-full-sibling (* -1 (or direction 1))))

(repeatable-motion-define-pair 'cpo-indent-tree-backward-full-sibling
                               'cpo-indent-tree-forward-full-sibling)

(defun cpo-indent-tree--forward-half-sibling ()
  "Go forward to next half sibling iff the current node is the last one before a half-sibling region boundary.
Return new position if moved, return nil on failure."
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        (parent-indentation (save-mark-and-excursion (cpo-indent-tree-up-to-parent 1)
                                                     (current-indentation))))
    (while (and (zerop (forward-line 1))
                (or (> (current-indentation) start-indent)
                    ;; TODO - maybe this should be configurable as to whether it ignores empty lines and/or lines with indentation but nothing after
                    ;; white space only line
                    (cpo-indent-tree--current-line-whitespace-only-p))))
    (if (or (not (< parent-indentation (current-indentation) start-indent))
            ;; TODO - without this, top-level movement can go to a blank line after the last top-level tree.  That could be useful, but violates my intention of making this a tree motion...
            (cpo-indent-tree--current-line-whitespace-only-p))
        (progn
          (goto-char backtrack-pos)
          nil)
      (progn
        (back-to-indentation)
        (point)))))

(defun cpo-indent-tree--backward-half-sibling ()
  "Go backward to next half sibling iff the current node is the last one before a half-sibling region boundary.
Return new position if moved, return nil on failure."
  (let* ((start-indent (current-indentation))
         (start-column (current-column))
         (backtrack-pos (point))
         (parent-info (save-mark-and-excursion (cpo-indent-tree-up-to-parent 1)
                                               (cons (current-indentation)
                                                     (point))))
         (parent-indentation (car parent-info))
         (parent-point (cdr parent-info)))
    (while (and (zerop (forward-line -1))
                (cpo-indent-tree--current-line-whitespace-only-p)))
    (if (< parent-indentation start-indent (current-indentation))
        ;; We have found a possible match, but need to go up the indent tree until one below the original parent.
        (let ((success-backtrack-point (point)))
          (while (< parent-indentation (current-indentation))
            (setq success-backtrack-point (point))
            (cpo-indent-tree-up-to-parent 1))
          (if (eql (point) parent-point)
              (progn
                (goto-char success-backtrack-point)
                (back-to-indentation)
                (point))
            (progn (goto-char backtrack-pos)
                   nil)))
      (progn (goto-char backtrack-pos)
             nil))))

(defun cpo-indent-tree-forward-full-or-half-sibling (num)
  "Move forward NUM full or half sibling nodes from the indent-tree node at point.
Move backward for negative num.
Return the (positive) number of iterations that could NOT be done (IE returns 0 for full success)."
  (interactive "p")
  (let ((fwd (<= 0 num))
        (num-left (abs num))
        (successful t))
    (while (and (< 0 num-left)
                successful)
      (setq num-left (cpo-indent-tree-forward-full-sibling
                      (if fwd num-left (- num-left))))
      (let ((half-sibling-success
             (and (< 0 num-left)
                  (funcall (if fwd
                               #'cpo-indent-tree--forward-half-sibling
                             #'cpo-indent-tree--backward-half-sibling)))))
        (when half-sibling-success
          (setq num-left (- num-left 1)))
        ;; If full siblings were exhausted and half sibling fails, then we must be done.
        ;; But if at least half-sibling-success, then we may still make progress in another loop iteration.
        (setq successful half-sibling-success)))
    num-left))

(defun cpo-indent-tree-backward-full-or-half-sibling (direction)
  "The reverse of `cpo-indent-tree-forward-full-or-half-sibling'."
  (interactive "p")
  (cpo-indent-tree-forward-full-or-half-sibling (* -1 (or direction 1))))

(repeatable-motion-define-pair 'cpo-indent-tree-backward-full-or-half-sibling
                               'cpo-indent-tree-forward-full-or-half-sibling)

(defun cpo-indent-tree--forward-full-sibling-region--only-to-boundary (num)
  (let ((times (abs num))
        (fwd (< 0 num))
        (index 0)
        (successful t)
        (backtrack (point)))
    (while (and (< index times)
                successful)
      (progn
        (if fwd
            (cpo-indent-tree-forward-to-last-full-sibling)
          (cpo-indent-tree-backward-to-first-full-sibling))
        (if (if fwd
                (cpo-indent-tree--forward-half-sibling)
              (cpo-indent-tree--backward-half-sibling))
            (progn
              (setq backtrack (point))
              (setq index (+ 1 index)))
          (progn
            (setq successful nil)
            (goto-char backtrack)))))
    (- times index)))

(defun cpo-indent-tree-forward-full-sibling-region-first (num)
  "Move forward to the next region of full siblings, NUM times.
Move backward for negative NUM.
Return the number of iterations that could NOT be done.

By full-sibling region, I mean a region of full siblings that is delimited by half-sibling breaks.
For example, consider:

```
root
      r1a
      r1b
     r2a
     r2b
    r3a
    r3b
```

If point is on r1a or r1b, moving forward to the next full-sibling region would move to r2a.
If point is on r2a or r2b, moving backward to the next full-sibling region would move to r1a.

It always moves to the FIRST sibling in the full sibling region, regardless of motion direction.
"
  (interactive "p")
  (let ((remaining (cpo-indent-tree--forward-full-sibling-region--only-to-boundary num)))
    (when (and (not (eql (abs num) remaining))
               (< num 0))
      (cpo-indent-tree-backward-to-first-full-sibling))
    remaining))
(defun cpo-indent-tree-backward-full-sibling-region-first (num)
  "The reverse of `cpo-indent-tree-forward-full-sibling-region-first'."
  (interactive "p")
  (cpo-indent-tree-forward-full-sibling-region-first (- num)))

(defun cpo-indent-tree-forward-full-sibling-region-last (num)
  "Like `cpo-indent-tree-forward-full-sibling-region-first', but to the end."
  (interactive "p")
  (let ((remaining (cpo-indent-tree--forward-full-sibling-region--only-to-boundary num)))
    (when (and (not (eql (abs num) remaining))
               (> num 0))
      (cpo-indent-tree-forward-to-last-full-sibling))
    remaining))
(defun cpo-indent-tree-backward-full-sibling-region-last (num)
  "The reverse of `cpo-indent-tree-forward-full-sibling-region-last'."
  (interactive "p")
  (cpo-indent-tree-forward-full-sibling-region-last (- num)))

(repeatable-motion-define-pair 'cpo-indent-tree-forward-full-sibling-region-first
                               'cpo-indent-tree-backward-full-sibling-region-first)
(repeatable-motion-define-pair 'cpo-indent-tree-forward-full-sibling-region-last
                               'cpo-indent-tree-backward-full-sibling-region-last)

(defun cpo-indent-tree-forward-to-last-full-sibling ()
  (interactive)
  (while (cpo-tree-walk--motion-moved (lambda () (cpo-indent-tree-forward-full-sibling 1)))))
(defun cpo-indent-tree-backward-to-first-full-sibling ()
  (interactive)
  (while (cpo-tree-walk--motion-moved (lambda () (cpo-indent-tree-forward-full-sibling -1)))))

(defun cpo-indent-tree--cpo-indent-tree-forward-to-last-full-or-half-sibling ()
  (while (cpo-tree-walk--motion-moved (lambda () (cpo-indent-tree-forward-full-or-half-sibling 1)))))

(defun cpo-indent-tree-up-to-parent (num)
  ;; TODO - this should probably error if num is negative
  (interactive "p")
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        ;;(direction (if (< 0 num) 1 -1))
        (times (abs num))
        (index 0))
    (while (and (< index times)
                (setq index (+ 1 index)))
      (while (and (zerop (forward-line -1))
                  (or (>= (current-indentation) start-indent)
                      (cpo-indent-tree--current-line-whitespace-only-p))))
      (if (or (not (< (current-indentation) start-indent))
              (cpo-indent-tree--current-line-whitespace-only-p))
          (goto-char backtrack-pos)
        (progn
          ;;(evil-goto-column start-column)
          (back-to-indentation)
          (setq backtrack-pos (point))
          (setq start-indent (current-indentation)))))))

(defun cpo-indent-tree-down-to-first-child (num)
  ;; TODO - this should probably error if num is negative
  (interactive "p")
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        ;;(direction (if (< 0 num) 1 -1))
        (times (abs num))
        (index 0)
        (ret-val t))
    (while (and (< index times)
                (setq index (+ 1 index)))
      (while (and (zerop (forward-line 1))
                  (cpo-indent-tree--current-line-whitespace-only-p)))
      (if (or (not (> (current-indentation) start-indent))
              (cpo-indent-tree--current-line-whitespace-only-p))
          (progn
            (setq ret-val nil)
            (goto-char backtrack-pos))
        (progn
          ;;(evil-goto-column start-column)
          (back-to-indentation)
          (setq backtrack-pos (point))
          (setq start-indent (current-indentation)))))
    ret-val))
(repeatable-motion-define-pair 'cpo-indent-tree-up-to-parent
                               'cpo-indent-tree-down-to-first-child)

(defun cpo-indent-tree-down-to-last-child (num)
  ;; TODO - this should probably error if num is negative
  (interactive "p")
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        ;;(direction (if (< 0 num) 1 -1))
        (times (abs num))
        (index 0))
    (while (and (< index times)
                (setq index (+ 1 index)))
      (and (cpo-tree-walk--motion-moved (lambda () (cpo-indent-tree-down-to-first-child 1)))
           (cpo-indent-tree--cpo-indent-tree-forward-to-last-full-or-half-sibling)))))
(repeatable-motion-define 'cpo-indent-tree-down-to-last-child
                          ;; down to last child has the same inverse as down to first child, but up to parent inverses as down to first child
                          'cpo-indent-tree-up-to-parent)

(cpo-tree-walk-define-operations
 :def-inorder-forward cpo-indent-tree-inorder-traversal-forward
 :def-inorder-backward cpo-indent-tree-inorder-traversal-backward
 :def-down-to-last-descendant cpo-indent-tree-down-to-last-descendant

 ;;:def-evil-inner-object-for-tree-with-no-end-delimiter cpo-indent-tree-inner
 ;;:def-evil-outer-object-for-tree-with-no-end-delimiter cpo-indent-tree-outer

 :def-bounds-for-tree-with-no-end-delimiter cpo-indent-tree-bounds
 :def-children-bounds-for-tree-with-no-end-delimiter cpo-indent-tree-children-bounds
 :def-expand-region cpo-indent-tree-expand-region
 :def-expand-region-idempotent cpo-indent-tree-expand-region-idempotent
 :def-select-children-once cpo-indent-tree-region-to-children
 :def-expand-region-to-children/ancestor-generation cpo-indent-tree-expand-region/children-region

 :def-transpose-sibling-forward cpo-indent-tree-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-indent-tree-transpose-sibling-backward

 :def-up-to-root cpo-indent-tree-up-to-root
 :def-select-root cpo-indent-tree-select-root

 :use-object-name "indentation tree"

 :use-up-to-parent (lambda () (cpo-indent-tree-up-to-parent 1))
 :use-down-to-first-child (lambda () (cpo-indent-tree-down-to-first-child 1))
 :use-down-to-last-child (lambda () (cpo-indent-tree-down-to-last-child 1))
 :use-next-sibling (lambda () (cpo-indent-tree-forward-full-or-half-sibling 1))
 :use-previous-sibling (lambda () (cpo-indent-tree-forward-full-or-half-sibling -1))
 :use-left-finalizer-for-tree-with-no-end-delimiter #'line-beginning-position
 :use-right-finalizer-for-tree-with-no-end-delimiter #'line-end-position
 )
(repeatable-motion-define-pair 'cpo-indent-tree-inorder-traversal-forward
                               'cpo-indent-tree-inorder-traversal-backward)
(repeatable-motion-define 'cpo-indent-tree-down-to-last-descendant nil)

;; TODO - for things like python, there is already a variable that this should match, and it should generally be customizable.
;; TODO - also, it might be nice to just check for an existing indentation amount, especially for promoting to promote specifically to parent level.
(setq cpo-indent-tree--indent-amount 2)

(defun cpo-indent-tree--promote-demote-helper (positive-p)
  (let* ((orig-point (point))
         (orig-mark (or (mark) (point)))
         (orig-beg (if (region-active-p) (region-beginning) (point)))
         (orig-end (if (region-active-p) (region-end) (point)))
         (tree-highlighted-p
          (and
           (region-active-p)
           (save-mark-and-excursion
             (goto-char (region-beginning))
             (set-mark (point))
             (cpo-indent-tree-expand-region)
             (and (= orig-beg (region-beginning))
                  (= orig-end (region-end)))))))
    (save-mark-and-excursion
      (unless tree-highlighted-p
        (let ((reg (progn (cpo-indent-tree-expand-region)
                          (cons (region-beginning) (region-end)))))
          (goto-char (car reg))
          (set-mark (cdr reg))))
      (indent-rigidly (region-beginning)
                      (region-end)
                      (funcall (if positive-p
                                   (lambda (x) x)
                                 #'-)
                               cpo-indent-tree--indent-amount)))))
(defun cpo-indent-tree-demote ()
  (interactive)
  (cpo-indent-tree--promote-demote-helper t))
(defun cpo-indent-tree-promote ()
  (interactive)
  (cpo-indent-tree--promote-demote-helper nil))

(defun cpo-indent-tree-open-sibling-forward ()
  (interactive)
  (let ((indentation (current-indentation))
        (reg (cpo-indent-tree-bounds (point))))
    (and reg
         (progn (goto-char (cdr reg))
                (insert "\n")
                (dotimes (i indentation) (insert " "))))))
(defun cpo-indent-tree-open-sibling-backward ()
  (interactive)
  (let ((indentation (current-indentation)))
    (beginning-of-line)
    (insert "\n")
    (backward-char 1)
    (dotimes (i indentation) (insert " "))))

(provide 'cpo-indent-tree)
