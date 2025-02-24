;;; -*- lexical-binding: t; -*-

(defun wgh/org-update-cookies ()
  (interactive)
  ;; update ALL cookies in the file without the extra argument
  (org-update-statistics-cookies t))

(defun wgh/org-training-wheels-message ()
  (interactive)
  (message "training wheels: use composiphrase bindings"))

(add-hook 'org-mode-hook
          (lambda ()
            (message "doing org-mode-hook")
            (require 'cpo-outline)
            ;; This group is a duplicate of the above but using the `m` prefix.
            ;; It would be good if in some mode I could have both org/outline
            ;; functions AND smartparens functions...
            (lnkmap "meh" 'wgh/org-training-wheels-message)
            (lnkmap "moh" 'wgh/org-training-wheels-message)
            (lnkmap "mog" 'wgh/org-training-wheels-message)
            (lnkmap "meg" 'wgh/org-training-wheels-message)
            (lnkmap "meo" 'wgh/org-training-wheels-message)
            (lnkmap "moo" 'wgh/org-training-wheels-message)
            (lnkmap "meus" 'wgh/org-training-wheels-message)
            (lnkmap "meub" 'wgh/org-training-wheels-message)

            (lnkmap "mw" 'wgh/org-training-wheels-message)
            (lnkmap "mb" 'wgh/org-training-wheels-message)

            (lnkmap "mk" 'wgh/org-training-wheels-message)
            (lnkmap "mj" 'wgh/org-training-wheels-message)

            ;; TODO - I need to add metaright/metaleft (IE promote single heading with no tree) as alternates of promote/demote, or as the disrespect-tree variant
            (lnkmap "ml" 'org-metaright)
            (lnkmap "mh" 'org-metaleft)
            ;; TODO - is there some verb that this could fit in to?  Eg. some generic action verb that can do different things for different objects?  Or is that just trying to kludge more composability in where it doesn't make sense?  This is a key action that I use and want for outline headings.
            (lnkmap "mtt" 'org-todo)


            ;; org-mark-element highlights the current-line and its subtree
            ;; actually - org-mark-subtree marks the subtree, while org-mark-element is the same when on a heading but behaves differently in text under a heading.
            ;;(lnkmap "mh" 'org-mark-element)

            (define-key org-mode-map (kbd "TAB") nil)
            (define-key org-mode-map (kbd "M-h") nil)

            (setq fold-toggle-wgh-fold-func 'org-cycle)
            (setq fold-toggle-wgh-fold-all-func 'org-shifttab)
            (setq org-cycle-emulate-tab nil)


            (setq org-hide-leading-stars t)
            ;; TODO - what indent-line-function do I really want for org-mode?  I think maybe I want no indentation for org-mode.
            (setq indent-line-function 'indent-relative)
            ;; Reload theme so that the stupid hidden asterisks are slightly visible
            (run-with-timer 1 nil (lambda ()
                                    ;; It only seems to work when called interactively...
                                    (call-interactively 'current-theme-reapply)))
            ))

(provide 'org-mode-conf)
