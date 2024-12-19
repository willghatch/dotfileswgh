(require 'tree-walk)

(defun wgh/org-forward-slurp-heading ()
  (interactive)
  (let ((start-line (line-number-at-pos))
        (line-at-next-heading (save-excursion
                                (org-forward-heading-same-level 1)
                                (line-number-at-pos))))
    (if (equal start-line line-at-next-heading)
        nil
      (save-excursion
        (org-forward-heading-same-level 1)
        (org-demote-subtree)))))

(defun wgh/org-forward-barf-heading ()
  (interactive)
  (let ((next-heading-loc (save-excursion
                            (org-forward-heading-same-level 1)
                            (point))))
    (save-excursion
      (outline-next-heading)
      (if (equal (point) next-heading-loc)
          nil
        ;; go to last child heading
        (letrec ((loop (lambda (pt)
                         (org-forward-heading-same-level 1)
                         (if (equal pt (point))
                             nil
                           (funcall loop (point))))))
          (funcall loop (point))
          (org-promote-subtree))))))

(defun wgh/org-add-heading-above ()
  (interactive)
  (when (not (org-at-heading-p))
    (org-up-element))
  (beginning-of-line)
  (org-insert-heading))

(defun wgh/org-add-heading-below ()
  (interactive)
  (wgh/org-add-heading-above)
  (org-metadown))

(defun wgh/org-update-cookies ()
  (interactive)
  ;; update ALL cookies in the file without the extra argument
  (org-update-statistics-cookies t))

(defun wgh/org-down-element ()
  (interactive)
  ;; To not get an error part way through a complex movement where an intermediate movement is allowed to fail
  (ignore-errors (org-down-element)))
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
 :def-inorder-forward wgh/org-inorder-traversal-forward
 :def-inorder-backward wgh/org-inorder-traversal-backward
 :def-down-to-last-descendant wgh/org-down-to-last-descendant

 ;:def-evil-inner-object-for-tree-with-no-end-delimiter wgh/org-tree-inner
 ;:def-evil-outer-object-for-tree-with-no-end-delimiter wgh/org-tree-outer

 :def-bounds-for-tree-with-no-end-delimiter wgh/org-tree-bounds
 :def-children-bounds-for-tree-with-no-end-delimiter wgh/org-tree-children-bounds
 :def-expand-region wgh/org-expand-region
 :def-expand-region-idempotent wgh/org-expand-region-idempotent
 :def-select-children-once wgh/org-region-to-children
 :def-expand-region-to-children/ancestor-generation wgh/org-expand-region/children-region

 :use-up-to-parent #'wgh/org-up-element
 :use-down-to-first-child #'wgh/org-down-element
 :use-down-to-last-child #'wgh/org-down-to-last-child
 :use-next-sibling (lambda () (org-forward-heading-same-level 1))
 :use-previous-sibling (lambda () (org-backward-heading-same-level 1))
 :use-left-finalizer-for-tree-with-no-end-delimiter #'line-beginning-position
 ;; TODO - this is wrong, it needs to get the contents of the heading potentially below the line.
 :use-right-finalizer-for-tree-with-no-end-delimiter #'line-end-position
 )

(require 'repeatable-motion)
(repeatable-motion-define-pair 'org-forward-heading-same-level 'org-backward-heading-same-level)
(repeatable-motion-define-pair 'org-down-element 'org-up-element)
(repeatable-motion-define 'wgh/org-down-to-last-child 'org-up-element)
(repeatable-motion-define 'wgh/org-down-to-last-descendant nil)
(repeatable-motion-define-pair 'wgh/org-inorder-traversal-forward 'wgh/org-inorder-traversal-backward)

(add-hook 'org-mode-hook
          (lambda ()
            (message "doing org-mode-hook")
            (lnkmap "eh" 'rmo/org-forward-heading-same-level)
            (lnkmap "oh" 'rmo/org-backward-heading-same-level)
            ;; outline-up-heading skips the immediate parent heading if called
            ;; from text underneath a heading.
            (lnkmap "og" 'rmo/org-up-element)
            (lnkmap "eg" 'rmo/org-up-element)

            (lnkmap "eH" 'wgh/org-add-heading-below)
            (lnkmap "oH" 'wgh/org-add-heading-above)

            (lnkmap "eus" 'wgh/org-forward-slurp-heading)
            (lnkmap "eub" 'wgh/org-forward-barf-heading)

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



