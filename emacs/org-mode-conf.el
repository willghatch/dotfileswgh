(require 'tree-walk)

(defun wgh/outline-forward-slurp-heading ()
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

(defun wgh/outline-forward-barf-heading ()
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

(defun wgh/outline-add-heading-above ()
  (interactive)
  (outline-back-to-heading)
  (outline-insert-heading)
  (insert " "))

(defun wgh/outline-add-heading-below ()
  (interactive)
  (wgh/outline-add-heading-above)
  (outline-move-subtree-down 1)
  (end-of-line))

(defun wgh/org-update-cookies ()
  (interactive)
  ;; update ALL cookies in the file without the extra argument
  (org-update-statistics-cookies t))

(defun wgh/outline-down-to-first-child ()
  (interactive)
  (let ((end-point nil))
    (save-mark-and-excursion
      (outline-back-to-heading)
      (let* ((orig-level (outline-level))
             (orig-heading-point (point))
             (moved (tree-walk--motion-moved 'outline-next-heading))
             (next-level (outline-level))
             (next-heading-point (point)))
        (when (< orig-level next-level)
          (setq end-point next-heading-point))))
    (when end-point (goto-char end-point))))

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
  (while (tree-walk--motion-moved (lambda () (org-forward-heading-same-level 1)))))
(defun wgh/org-down-to-last-child ()
  ;; TODO - this and others should probably take a number argument
  (interactive)
  (and (tree-walk--motion-moved #'wgh/org-down-element)
       (wgh/org-forward-to-last-sibling)))


(tree-walk-define-operations
 :def-inorder-forward wgh/outline-inorder-traversal-forward
 :def-inorder-backward wgh/outline-inorder-traversal-backward
 :def-down-to-last-descendant wgh/outline-down-to-last-descendant
 :def-down-to-last-child wgh/outline-down-to-last-child

 ;:def-evil-inner-object-for-tree-with-no-end-delimiter wgh/outline-tree-inner
 ;:def-evil-outer-object-for-tree-with-no-end-delimiter wgh/outline-tree-outer

 :def-bounds-for-tree-with-no-end-delimiter wgh/outline-tree-bounds
 :def-children-bounds-for-tree-with-no-end-delimiter wgh/outline-tree-children-bounds
 :def-expand-region wgh/outline-expand-region
 :def-expand-region-idempotent wgh/outline-expand-region-idempotent
 :def-select-children-once wgh/outline-region-to-children
 :def-expand-region-to-children/ancestor-generation wgh/outline-expand-region/children-region

 :use-up-to-parent (lambda () (outline-up-heading 1))
 :use-down-to-first-child #'wgh/outline-down-to-first-child
 ;; TODO - handle half siblings like I did for indent-tree -- instead of using outline-forward-same-level here, I need to write a forward-sibling-or-half-sibling function.
 :use-next-sibling (lambda () (ignore-errors (outline-forward-same-level 1)))
 :use-previous-sibling (lambda () (ignore-errors (outline-backward-same-level 1)))
 :use-left-finalizer-for-tree-with-no-end-delimiter #'line-beginning-position
 ;; TODO - this is wrong, it needs to get the contents of the heading potentially below the line.
 :use-right-finalizer-for-tree-with-no-end-delimiter #'line-end-position
 )

(require 'repeatable-motion)
(repeatable-motion-define-pair 'outline-forward-same-level 'outline-backward-same-level)
(repeatable-motion-define-pair 'wgh/outline-down-to-first-child 'outline-up-heading)
(repeatable-motion-define 'wgh/outline-down-to-last-child 'outline-up-heading)
(repeatable-motion-define 'wgh/outline-down-to-last-descendant nil)
(repeatable-motion-define-pair 'wgh/outline-inorder-traversal-forward 'wgh/outline-inorder-traversal-backward)

(add-hook 'org-mode-hook
          (lambda ()
            (message "doing org-mode-hook")
            ;; (lnkmap "eh" 'rmo/org-forward-heading-same-level)
            ;; (lnkmap "oh" 'rmo/org-backward-heading-same-level)
            ;; ;; outline-up-heading skips the immediate parent heading if called
            ;; ;; from text underneath a heading.
            ;; (lnkmap "og" 'rmo/org-up-element)
            ;; (lnkmap "eg" 'rmo/org-up-element)

            ;; (lnkmap "eH" 'wgh/org-add-heading-below)
            ;; (lnkmap "oH" 'wgh/org-add-heading-above)

            ;; (lnkmap "eus" 'wgh/org-forward-slurp-heading)
            ;; (lnkmap "eub" 'wgh/org-forward-barf-heading)

            ;; This group is a duplicate of the above but using the `m` prefix.
            ;; It would be good if in some mode I could have both org/outline
            ;; functions AND smartparens functions...
            (lnkmap "meh" 'rmo/org-forward-heading-same-level)
            (lnkmap "moh" 'rmo/org-backward-heading-same-level)
            (lnkmap "mog" 'rmo/org-up-element)
            (lnkmap "meg" 'rmo/org-up-element)
            (lnkmap "meo" 'wgh/org-add-heading-below)
            (lnkmap "moo" 'wgh/org-add-heading-above)
            (lnkmap "meus" 'wgh/org-forward-slurp-heading)
            (lnkmap "meub" 'wgh/org-forward-barf-heading)

            (lnkmap "ml" 'org-metaright)
            (lnkmap "mh" 'org-metaleft)
            (lnkmap "mw" 'org-demote-subtree)
            (lnkmap "mb" 'org-promote-subtree)

            (lnkmap "mk" 'org-move-subtree-up)
            (lnkmap "mj" 'org-move-subtree-down)

            (lnkmap "mtt" 'org-todo)


            ;; org-mark-element highlights the current-line and its subtree
                                        ;(lnkmap "mh" 'org-mark-element)

            (define-key org-mode-map (kbd "TAB") nil)
            (define-key org-mode-map (kbd "M-h") nil)

            (setq fold-toggle-wgh-fold-func 'org-cycle)
            (setq fold-toggle-wgh-fold-all-func 'org-shifttab)
            (setq org-cycle-emulate-tab nil)


            (setq org-hide-leading-stars t)
            (setq indent-line-function 'indent-relative)
            ;; Reload theme so that the stupid hidden asterisks are slightly visible
            (run-with-timer 1 nil (lambda ()
                                    ;; It only seems to work when called interactively...
                                    (call-interactively 'current-theme-reapply)))
            ))



