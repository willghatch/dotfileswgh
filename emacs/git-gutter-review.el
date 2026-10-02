;;; git-gutter-review.el --- Review a branch with git-gutter against its merge base  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; `wgh/git-gutter-review-start' makes git-gutter show changes relative to
;; the merge base of HEAD and the repo's default branch, for every buffer
;; visiting a file in the current worktree, including files opened later.
;; Other worktrees of the same repo are unaffected.
;; `wgh/git-gutter-review-end' returns those buffers to git-gutter's
;; default diff base.  `wgh/git-gutter-refresh-all' re-diffs every
;; git-gutter buffer, for when the gutter goes stale.
;;
;; It also provides motions across changed files that cycle from the last
;; changed file back to the first: `wgh/git-gutter-file-forward' and
;; friends, and hunk motions that continue into other changed files.
;;
;; `git-gutter:start-revision' is buffer-local, and Emacs has no notion of
;; a worktree-local variable, so active reviews are kept in a per-worktree
;; table and copied into each relevant buffer.

;;; Code:

(require 'cl-lib)
(require 'git-gutter)
(require 'cpo-git-gutter)

(defvar wgh/git-gutter-review--active nil
  "Alist of active reviews: (WORKTREE-ROOT . MERGE-BASE-SHA).")

(defun wgh/git-gutter-review--worktree-root (&optional dir)
  "Return the normalized root of the git worktree containing DIR, or nil."
  ;; Linked worktrees and submodules have a .git file at their root, so this
  ;; finds the innermost worktree rather than the main repo.
  (let ((root (locate-dominating-file (or dir default-directory) ".git")))
    (and root (file-name-as-directory (expand-file-name root)))))

(defun wgh/git-gutter-review--buffer-worktree-root ()
  "Return the worktree root of the current buffer's file, or nil.
Returns nil when the buffer is not visiting a file."
  (and (buffer-file-name)
       (wgh/git-gutter-review--worktree-root)))

(defun wgh/git-gutter-review--git-string (&rest args)
  "Run git with ARGS and return trimmed output, or nil on failure."
  (with-temp-buffer
    (when (zerop (apply #'process-file "git" nil t nil args))
      (string-trim (buffer-string)))))

(defun wgh/git-gutter-review-default-branch ()
  "Return the default branch to review against.
Uses the branch that origin/HEAD points to, preferring the local branch
of that name when it exists.  Falls back to main, then master."
  (let* ((remote-head (wgh/git-gutter-review--git-string
                       "symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD"))
         (local-name (and remote-head
                          (string-remove-prefix "origin/" remote-head)))
         (candidates (delq nil (list local-name remote-head "main" "master"))))
    (or (cl-find-if (lambda (ref)
                      (wgh/git-gutter-review--git-string
                       "rev-parse" "--verify" "--quiet" (concat ref "^{commit}")))
                    candidates)
        (user-error "Could not determine a default branch"))))

(defun wgh/git-gutter-review--worktree-buffers (root)
  "Return live buffers visiting files in the worktree at ROOT."
  (cl-remove-if-not
   (lambda (buf)
     (with-current-buffer buf
       (equal root (wgh/git-gutter-review--buffer-worktree-root))))
   (buffer-list)))

(defun wgh/git-gutter-review--apply-to-buffer ()
  "Apply any active review's merge base to the current buffer.
Re-diffs only when the start revision actually changed."
  (let* ((root (wgh/git-gutter-review--buffer-worktree-root))
         (sha (cdr (assoc root wgh/git-gutter-review--active))))
    (when (and sha (not (equal sha git-gutter:start-revision)))
      (setq git-gutter:start-revision sha)
      (when git-gutter-mode
        (wgh/git-gutter-review--rediff)))))

(defun wgh/git-gutter-review--rediff ()
  "Re-diff the current buffer, restarting any diff already in flight."
  ;; `git-gutter' is a no-op while a diff process for the buffer is running,
  ;; so a diff started with an old start revision (eg. by `git-gutter-mode'
  ;; turning on) would otherwise win.  The old process may have already
  ;; exited with its sentinel still pending, so detach the sentinel too, or
  ;; it can later overwrite the new diff results.
  (let ((proc-buf (get-buffer (git-gutter:diff-process-buffer (git-gutter:base-file)))))
    (when proc-buf
      (let ((proc (get-buffer-process proc-buf)))
        (when proc
          (set-process-sentinel proc #'ignore)
          (delete-process proc)))
      (kill-buffer proc-buf)))
  (git-gutter))

(defun wgh/git-gutter-refresh-all ()
  "Re-diff every buffer that has `git-gutter-mode' enabled."
  (interactive)
  (dolist (buf (buffer-list))
    ;; Re-diffing kills in-flight diff process buffers, which may be later
    ;; in the list.
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (bound-and-true-p git-gutter-mode)
          (wgh/git-gutter-review--rediff))))))

(defun wgh/git-gutter-review-start (&optional prompt)
  "Start reviewing the current worktree against its default branch's merge base.
Every buffer visiting a file in this worktree, including ones opened later,
diffs against the merge base of HEAD and the default branch (see
`wgh/git-gutter-review-default-branch').  With PROMPT (prefix arg), read
the branch, with the default prefilled.  Restarting a review recomputes
the merge base."
  (interactive "P")
  (let* ((root (or (wgh/git-gutter-review--worktree-root)
                   (user-error "Not in a git worktree")))
         ;; Don't let-bind `default-directory' here: it is buffer-local, so
         ;; the binding would leak into this buffer's git-gutter diff, which
         ;; runs git with a path relative to the buffer's own directory.
         (default (wgh/git-gutter-review-default-branch))
         (branch (if prompt
                     (read-string (format "Review against branch (default %s): " default)
                                  nil nil default)
                   default))
         (sha (or (wgh/git-gutter-review--git-string "merge-base" branch "HEAD")
                  (user-error "Could not determine merge-base of %s and HEAD" branch))))
    ;; End any previous review of this worktree first, so buffers still on its
    ;; old merge base are reset rather than left behind.
    (when (assoc root wgh/git-gutter-review--active)
      (wgh/git-gutter-review--end root))
    (push (cons root sha) wgh/git-gutter-review--active)
    (add-hook 'git-gutter-mode-hook #'wgh/git-gutter-review--apply-to-buffer)
    (add-hook 'magit-post-refresh-hook #'wgh/git-gutter-refresh-all)
    (dolist (buf (wgh/git-gutter-review--worktree-buffers root))
      (with-current-buffer buf
        (setq git-gutter:start-revision sha)))
    (wgh/git-gutter-refresh-all)
    (message "Reviewing %s against %s (merge-base %s)"
             (abbreviate-file-name root) branch (substring sha 0 (min 12 (length sha))))))

(defun wgh/git-gutter-review--end (root)
  "End the review of the worktree at ROOT, without refreshing."
  (let ((sha (cdr (assoc root wgh/git-gutter-review--active))))
    (dolist (buf (wgh/git-gutter-review--worktree-buffers root))
      (with-current-buffer buf
        ;; Leave alone buffers whose start revision was changed by something
        ;; else, eg. `git-gutter:set-start-revision' or a dir-local.
        (when (equal git-gutter:start-revision sha)
          (setq git-gutter:start-revision nil))))
    (setq wgh/git-gutter-review--active
          (cl-remove root wgh/git-gutter-review--active :key #'car :test #'equal))
    (unless wgh/git-gutter-review--active
      (remove-hook 'git-gutter-mode-hook #'wgh/git-gutter-review--apply-to-buffer)
      (remove-hook 'magit-post-refresh-hook #'wgh/git-gutter-refresh-all))))

(defun wgh/git-gutter-review-end ()
  "End the review of the current worktree.
Its buffers return to git-gutter's default diff base."
  (interactive)
  (let ((root (wgh/git-gutter-review--worktree-root)))
    (unless (assoc root wgh/git-gutter-review--active)
      (user-error "No active review for this worktree"))
    (wgh/git-gutter-review--end root)
    (wgh/git-gutter-refresh-all)
    (message "Ended review of %s" (abbreviate-file-name root))))

;;; Changed-file navigation

;; Like the cross-file motions in cpo-git-gutter, but cycling from the last
;; changed file to the first (and vice versa), and waiting for the target
;; file's diff before jumping to its hunks.

(defun wgh/git-gutter-review--changed-files ()
  "Return sorted absolute paths of files changed vs the current start revision.
Only existing regular files are included, so deleted files and submodules
are skipped."
  (let ((root (wgh/git-gutter-review--worktree-root))
        (rev (and git-gutter:start-revision
                  (not (string-empty-p git-gutter:start-revision))
                  git-gutter:start-revision)))
    (when root
      (with-temp-buffer
        (setq default-directory root)
        (when (zerop (apply #'process-file "git" nil t nil
                            "diff" "--name-only" (and rev (list rev))))
          (sort (cl-remove-if-not
                 #'file-regular-p
                 (mapcar (lambda (f) (expand-file-name f root))
                         (split-string (buffer-string) "\n" t)))
                #'string<))))))

(defun wgh/git-gutter-review--wait-for-diff ()
  "Wait (up to a few seconds) for the current buffer's git-gutter diff to finish."
  (let ((proc-buf-name (git-gutter:diff-process-buffer (git-gutter:base-file)))
        (deadline (+ (float-time) 3)))
    ;; git-gutter's sentinel kills the process buffer after updating hunks.
    (while (and (get-buffer proc-buf-name) (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defun wgh/git-gutter-review--goto-hunk (which end)
  "Go to the first or last hunk (WHICH is `first' or `last') in this buffer.
Go to the hunk's end if END is non-nil, else its beginning."
  (let* ((hunks git-gutter:diffinfos)
         (hunk (if (eq which 'first) (car hunks) (car (last hunks)))))
    (when hunk
      (goto-char (if end
                     (cpo-git-gutter--hunk-end-pos hunk)
                   (cpo-git-gutter--hunk-start-pos hunk))))))

(defun wgh/git-gutter-review--visit-changed-file (direction which end)
  "Visit the next changed file in DIRECTION (1 or -1), cycling at the ends.
Then go to its WHICH (`first' or `last') hunk, at the hunk END if non-nil.
Return non-nil if a file was visited."
  (let* ((files (wgh/git-gutter-review--changed-files))
         (n (length files))
         (cur (and (buffer-file-name)
                   (cl-position (expand-file-name (buffer-file-name)) files
                                :test #'string=)))
         ;; Treat a file not in the list as sitting just outside it, so the
         ;; first step lands on the first (or last) changed file.
         (from (or cur (if (> direction 0) -1 n))))
    (if (zerop n)
        (progn (message "No changed files") nil)
      (find-file (nth (mod (+ from direction) n) files))
      (unless git-gutter-mode
        (git-gutter-mode 1))
      (wgh/git-gutter-review--wait-for-diff)
      (wgh/git-gutter-review--goto-hunk which end)
      t)))

(defun wgh/git-gutter-file-forward (&optional count)
  "Go to the first hunk of the next changed file, COUNT times, cycling.
Changed files are those differing from the current buffer's git-gutter
start revision.  Negative COUNT moves backward."
  (interactive "p")
  (setq count (or count 1))
  (dotimes (_ (abs count))
    (wgh/git-gutter-review--visit-changed-file (if (< count 0) -1 1) 'first nil)))

(defun wgh/git-gutter-file-backward (&optional count)
  "Go to the first hunk of the previous changed file, COUNT times, cycling."
  (interactive "p")
  (wgh/git-gutter-file-forward (- (or count 1))))

(defun wgh/git-gutter-review--hunk-move-cycling (count end)
  "Move COUNT hunks (to hunk END if non-nil, else beginning), crossing files.
When the current buffer has no further hunk, continue in the next (or
previous) changed file, cycling from the last file to the first."
  (dotimes (_ (abs (or count 1)))
    (let ((fwd (>= (or count 1) 0))
          (buf (current-buffer))
          (pos (point)))
      (funcall (if end #'cpo-git-gutter-hunk-forward-end #'cpo-git-gutter-hunk-forward-beginning)
               :count (if fwd 1 -1))
      (when (and (eq buf (current-buffer)) (= pos (point)))
        (wgh/git-gutter-review--visit-changed-file
         (if fwd 1 -1) (if fwd 'first 'last) end)))))

(defun wgh/git-gutter-hunk-forward-beginning-cycling (&optional count)
  "Move to the beginning of the next hunk, continuing into other changed files."
  (interactive "p")
  (wgh/git-gutter-review--hunk-move-cycling count nil))

(defun wgh/git-gutter-hunk-backward-beginning-cycling (&optional count)
  "Move to the beginning of the previous hunk, continuing into other changed files."
  (interactive "p")
  (wgh/git-gutter-review--hunk-move-cycling (- (or count 1)) nil))

(defun wgh/git-gutter-hunk-forward-end-cycling (&optional count)
  "Move to the end of the next hunk, continuing into other changed files."
  (interactive "p")
  (wgh/git-gutter-review--hunk-move-cycling count t))

(defun wgh/git-gutter-hunk-backward-end-cycling (&optional count)
  "Move to the end of the previous hunk, continuing into other changed files."
  (interactive "p")
  (wgh/git-gutter-review--hunk-move-cycling (- (or count 1)) t))

(declare-function repeatable-motion-define-pair "repeatable-motion")
(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'wgh/git-gutter-file-forward
                                 'wgh/git-gutter-file-backward)
  (repeatable-motion-define-pair 'wgh/git-gutter-hunk-forward-beginning-cycling
                                 'wgh/git-gutter-hunk-backward-beginning-cycling)
  (repeatable-motion-define-pair 'wgh/git-gutter-hunk-forward-end-cycling
                                 'wgh/git-gutter-hunk-backward-end-cycling))

(provide 'git-gutter-review)

;;; git-gutter-review.el ends here
