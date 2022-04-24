;;; indent-tree.el --- TODO - description here -*- lexical-binding: t; -*-

;; Note that it will not work gracefully for improper trees (with
;; indentation levels skipped for some nodes).  This can only happen
;; for the first child of any node.
;; eg:
;;   root
;;      indented more than root's next child!
;;     normal child indentation

(require 'tree-walk)
(require 'repeatable-motion)

(defun indent-tree--current-line-whitespace-only-p ()
  (string-match-p "^\\s-*$"
                  (buffer-substring (line-beginning-position)
                                    (line-end-position))))

(defun indent-tree-forward-sibling (num)
  (interactive "p")
  (let ((start-indent (current-indentation))
        (start-column (current-column))
        (backtrack-pos (point))
        (direction (if (< 0 num) 1 -1))
        (times (abs num))
        (index 0))
    (while (and (< index times)
                (setq index (+ 1 index)))
      (while (and (zerop (forward-line direction))
                  (or (> (current-indentation) start-indent)
                      ;; TODO - maybe this should be configurable as to whether it ignores empty lines and/or lines with indentation but nothing after
                      ;; current-line-empty
                      ;;(equal (line-beginning-position) (line-end-position))
                      ;; white space only line
                      (indent-tree--current-line-whitespace-only-p)

                      )
                  ))
      (if (or (not (= (current-indentation) start-indent))
              ;; TODO - without this, top-level movement can go to a blank line after the last top-level tree.  That could be useful, but violates my intention of making this a tree motion...
              (indent-tree--current-line-whitespace-only-p))
          (goto-char backtrack-pos)
        (progn
          ;;(evil-goto-column start-column)
          (back-to-indentation)
          (setq backtrack-pos (point)))))))
(defun indent-tree-backward-sibling (direction)
  (interactive "p")
  (indent-tree-forward-sibling (* -1 (or direction 1))))
(repeatable-motion-define-pair 'indent-tree-backward-sibling
                               'indent-tree-forward-sibling)


(defun indent-tree--indent-tree-forward-to-last-sibling ()
  (while (tree-walk--motion-moved (lambda () (indent-tree-forward-sibling 1)))))

(defun indent-tree-up-to-parent (num)
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
                      (indent-tree--current-line-whitespace-only-p))))
      (if (or (not (< (current-indentation) start-indent))
              (indent-tree--current-line-whitespace-only-p))
          (goto-char backtrack-pos)
        (progn
          ;;(evil-goto-column start-column)
          (back-to-indentation)
          (setq backtrack-pos (point))
          (setq start-indent (current-indentation)))))))

(defun indent-tree-down-to-first-child (num)
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
                  (indent-tree--current-line-whitespace-only-p)))
      (if (or (not (> (current-indentation) start-indent))
              (indent-tree--current-line-whitespace-only-p))
          (progn
            (setq ret-val nil)
            (goto-char backtrack-pos))
        (progn
          ;;(evil-goto-column start-column)
          (back-to-indentation)
          (setq backtrack-pos (point))
          (setq start-indent (current-indentation)))))
    ret-val))
(repeatable-motion-define-pair 'indent-tree-up-to-parent
                               'indent-tree-down-to-first-child)

(defun indent-tree-down-to-last-child (num)
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
      (and (tree-walk--motion-moved (lambda () (indent-tree-down-to-first-child 1)))
           (indent-tree--indent-tree-forward-to-last-sibling)))))
(repeatable-motion-define 'indent-tree-down-to-last-child
                          ;; down to last child has the same inverse as down to first child, but up to parent inverses as down to first child
                          'indent-tree-up-to-parent)

(tree-walk-define-operations
 :inorder-forward indent-tree-inorder-traversal-forward
 :inorder-backward indent-tree-inorder-traversal-backward
 :down-to-last-descendant indent-tree-down-to-last-descendant
 :no-end-inner-object indent-tree-inner
 :no-end-outer-object indent-tree-outer

 :up-to-parent (lambda () (indent-tree-up-to-parent 1))
 :down-to-first-child (lambda () (indent-tree-down-to-first-child 1))
 :down-to-last-child (lambda () (indent-tree-down-to-last-child 1))
 :next-sibling (lambda () (indent-tree-forward-sibling 1))
 :previous-sibling (lambda () (indent-tree-forward-sibling -1))
 :no-end-object-left-finalize #'line-beginning-position
 :no-end-object-right-finalize #'line-end-position
 )
(repeatable-motion-define-pair 'indent-tree-inorder-traversal-forward
                               'indent-tree-inorder-traversal-backward)
(repeatable-motion-define 'indent-tree-down-to-last-descendant nil)

;; TODO - for things like python, there is already a variable that this should match, and it should generally be customizable.
(setq indent-tree--indent-amount 2)

(defun indent-tree--promote-demote-helper (positive-p)
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
             (indent-tree-outer 1 (point) (point))
             (and (= orig-beg (region-beginning))
                  (= orig-end (region-end)))))))
    (save-mark-and-excursion
      (unless tree-highlighted-p
        (let ((reg (indent-tree-outer 1 orig-beg orig-end)))
          (goto-char (car reg))
          (set-mark (cadr reg))))
      (indent-rigidly (region-beginning)
                      (region-end)
                      (funcall (if positive-p
                                   (lambda (x) x)
                                 #'-)
                               indent-tree--indent-amount)))))
(defun indent-tree-demote ()
  (interactive)
  (indent-tree--promote-demote-helper t))
(defun indent-tree-promote ()
  (interactive)
  (indent-tree--promote-demote-helper nil))

(provide 'indent-tree)
