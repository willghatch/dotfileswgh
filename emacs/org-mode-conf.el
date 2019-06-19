
(defun wgh/org-forward-slurp-heading ()
  (interactive)
  (let ((start-line (line-number-at-pos))
        (line-at-next-heading (save-excursion
                                (org-forward-heading-same-level 1)
                                (line-number-at-pos))))
    (if (equalp start-line line-at-next-heading)
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
                         (if (equalp pt (point))
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

(require 'repeatable-motion)
(repeatable-motion-define-pair 'org-forward-heading-same-level 'org-backward-heading-same-level)
(repeatable-motion-define 'org-up-element nil)

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



