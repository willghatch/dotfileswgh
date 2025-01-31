;;; -*- lexical-binding: t; -*-
(require 'cpo-tree-walk)
(require 'outline)

(defun cpo-outline-forward-slurp-heading ()
  ;; TODO - this is broken, outline-demote isn't working right, at least in org-mode
  (interactive)
  (let ((start-line (line-number-at-pos))
        (line-at-next-heading (save-excursion
                                (outline-forward-same-level 1)
                                (line-number-at-pos))))
    (if (equal start-line line-at-next-heading)
        nil
      (save-excursion
        (outline-forward-same-level 1)
        (outline-demote 'subtree)))))

(defun cpo-outline-forward-barf-heading ()
  (interactive)
  (let ((next-heading-loc (save-excursion
                            (outline-forward-same-level 1)
                            (point)))
        (start-line (line-number-at-pos))
        (line-at-next-heading (save-excursion
                                (outline-forward-same-level 1)
                                (line-number-at-pos))))
    (save-excursion
      (outline-next-heading)
      (if (equal start-line line-at-next-heading)
          nil
        ;; go to last child heading
        (letrec ((loop (lambda (pt)
                         (ignore-errors (outline-forward-same-level 1))
                         (if (equal pt (point))
                             nil
                           (funcall loop (point))))))
          (funcall loop (point))
          (outline-promote 'subtree))))))

(defun cpo-outline-add-heading-above ()
  (interactive)
  (outline-back-to-heading)
  (outline-insert-heading)
  (insert " "))

(defun cpo-outline-add-heading-below ()
  (interactive)
  (cpo-outline-add-heading-above)
  (outline-move-subtree-down 1)
  (end-of-line))


(defun cpo-outline-down-to-first-child ()
  (interactive)
  (let ((end-point nil))
    (save-mark-and-excursion
      (outline-back-to-heading)
      (let* ((orig-level (outline-level))
             (orig-heading-point (point))
             (moved (cpo-tree-walk--motion-moved 'outline-next-heading))
             (next-level (outline-level))
             (next-heading-point (point)))
        (when (< orig-level next-level)
          (setq end-point next-heading-point))))
    (when end-point (goto-char end-point))))

;; TODO - switch these to use outline-minor-mode if possible instead of org-mode
(defun wgh/org-down-element ()
  (interactive)
  ;; To not get an error part way through a complex movement where an intermediate movement is allowed to fail
  (ignore-errors (org-down-element)))
;; TODO - use outline-up-heading
(defun wgh/org-up-element ()
  (interactive)
  ;; To not get an error part way through a complex movement where an intermediate movement is allowed to fail
  (ignore-errors (org-up-element)))

(defun wgh/org-forward-to-last-sibling ()
  (while (cpo-tree-walk--motion-moved (lambda () (org-forward-heading-same-level 1)))))
(defun wgh/org-down-to-last-child ()
  ;; TODO - this and others should probably take a number argument
  (interactive)
  (and (cpo-tree-walk--motion-moved #'wgh/org-down-element)
       (wgh/org-forward-to-last-sibling)))

(defun cpo-outline--outline-at-anchor-point-p ()
  (save-mark-and-excursion
    (let ((start-point (point)))
      (outline-previous-heading)
      (outline-next-heading)
      (equal (point) start-point))))

(cpo-tree-walk-define-operations
 :def-inorder-forward cpo-outline-inorder-traversal-forward
 :def-inorder-backward cpo-outline-inorder-traversal-backward
 :def-down-to-last-descendant cpo-outline-down-to-last-descendant
 :def-down-to-last-child cpo-outline-down-to-last-child

 ;;:def-evil-inner-object-for-tree-with-no-end-delimiter cpo-outline-tree-inner
 ;;:def-evil-outer-object-for-tree-with-no-end-delimiter cpo-outline-tree-outer

 :def-bounds-for-tree-with-no-end-delimiter cpo-outline-tree-bounds
 :def-children-bounds-for-tree-with-no-end-delimiter cpo-outline-tree-children-bounds
 :def-expand-region cpo-outline-expand-region
 :def-expand-region-idempotent cpo-outline-expand-region-idempotent
 :def-select-children-once cpo-outline-region-to-children
 :def-expand-region-to-children/ancestor-generation cpo-outline-expand-region/children-region
 :def-transpose-sibling-forward cpo-outline-transpose-sibling-forward
 :def-transpose-sibling-backward cpo-outline-transpose-sibling-backward
 :def-up-to-root cpo-outline-up-to-root
 :def-select-root cpo-outline-select-root

 :use-object-name "outline-mode header (eg. in org-mode or outline-minor-mode)"

 :use-up-to-parent (lambda () (ignore-errors (outline-up-heading 1)))
 :use-down-to-first-child #'cpo-outline-down-to-first-child
 ;; TODO - handle half siblings like I did for indent-tree -- instead of using outline-forward-same-level here, I need to write a forward-sibling-or-half-sibling function.
 :use-next-sibling (lambda () (ignore-errors (outline-forward-same-level 1)))
 :use-previous-sibling (lambda () (ignore-errors (outline-backward-same-level 1)))
 :use-left-finalizer-for-tree-with-no-end-delimiter (lambda () (if (cpo-outline--outline-at-anchor-point-p)
                                                                   (line-beginning-position)
                                                                 (outline-previous-heading)))
 :use-right-finalizer-for-tree-with-no-end-delimiter (lambda ()
                                                       (outline-next-heading)
                                                       (unless (and (eobp) (not (bolp)))
                                                         (beginning-of-line)
                                                         (backward-char 1)))
 )

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'outline-forward-same-level 'outline-backward-same-level)
  (repeatable-motion-define-pair 'cpo-outline-down-to-first-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-child 'outline-up-heading)
  (repeatable-motion-define 'cpo-outline-down-to-last-descendant nil)
  (repeatable-motion-define-pair 'cpo-outline-inorder-traversal-forward 'cpo-outline-inorder-traversal-backward)
  )



(provide 'cpo-outline)
