;;; -*- lexical-binding: t; -*-

;; The specifications for this format string is in `man git-log`.
;; %+b adds the commit message body text
;; %+N adds “notes”, which is a git feature that I don't think I've ever seen anybody use.
;; %aN adds author name
;; %aE adds author email
;; %cN adds committer name
;; %cE adds committer email
;(setq magit-log-revision-headers-format "%+b%+N")
;(setq magit-log-revision-headers-format "%+b%+N\nA: %aN, C: %cN")


(defun wgh/magit-checkout-at-point ()
  (interactive)
  (magit-checkout (magit-branch-or-commit-at-point)))
(defun wgh/magit-branch-checkout-at-point ()
  (interactive)
  (magit-checkout (magit-branch-at-point)))
(defun wgh/magit-branch-create-at-point ()
  (interactive)
  (magit-branch-create (read-string "New branch name: ")
                       (magit-commit-at-point)))
(defun wgh/magit-branch-delete-at-point ()
  ;; TODO - this fails to delete unmerged braanches.  I would like it to prompt
  ;; in that case instead.
  (interactive)
  (magit-branch-delete (list (magit-branch-at-point))))
(defun wgh/magit-branch-rename-at-point ()
  (interactive)
  (let ((the-branch (magit-branch-at-point)))
    (magit-branch-rename the-branch
                         (read-string
                          (format "Rename %s: " the-branch)))))
(defun wgh/magit-tag-create-at-point ()
  (interactive)
  (magit-tag-create (read-string "Tag name: ") (magit-commit-at-point)))
(defun wgh/magit-tag-delete-at-point ()
  (interactive)
  (let ((the-tag (magit-tag-at-point)))
    (when (yes-or-no-p (format "Really delete tag %s?: " the-tag))
      (magit-tag-delete (list the-tag)))))

(defun wgh/magit-current-branch-reset-hard-at-point ()
  (interactive)
  (let ((the-commit (magit-branch-or-commit-at-point)))
    (when (yes-or-no-p (format "Really HARD reset to %s: " the-commit))
      (magit-reset-hard the-commit))))
(defun wgh/magit-current-branch-reset-mixed-at-point ()
  (interactive)
  (let ((the-commit (magit-branch-or-commit-at-point)))
    (when (yes-or-no-p (format "Really MIXED reset to %s: " the-commit))
      (magit-reset-mixed the-commit))))
(defun wgh/magit-current-branch-reset-soft-at-point ()
  (interactive)
  (let ((the-commit (magit-branch-or-commit-at-point)))
    (when (yes-or-no-p (format "Really SOFT reset to %s: " the-commit))
      (magit-reset-soft the-commit))))
(defun wgh/magit-current-branch-revert-commit-at-point ()
  (interactive)
  (magit-revert-and-commit (magit-commit-at-point)))

(defun wgh/magit-diff-range-current-commit-to-commit-at-point ()
  (interactive
   (magit-diff-range (format "%s..%s"
                             "HEAD"
                             (magit-commit-at-point)))))
