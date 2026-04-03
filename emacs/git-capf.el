;;; git-capf.el --- Completion-at-point functions for git  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides `completion-at-point-functions' (CAPF) entries for various
;; git-related completions:
;;
;;   `wgh/git-branch-capf'     -- branch names in the current repo and
;;                                 submodules (format: BRANCH or
;;                                 repo:SUBREPO_PATH branch:BRANCH)
;;   `wgh/git-file-capf'       -- file paths from git root
;;   `wgh/git-submodule-capf'  -- submodule paths (from the root repo)
;;   `wgh/git-worktree-capf'   -- worktree names / paths
;;   `wgh/git-commit-capf'     -- commit hashes; user types words from
;;                                 the log line or author name and gets
;;                                 candidates in the form
;;                                 commit:HASH (commit log text: LOG_TEXT)
;;
;; Each function follows the Emacs CAPF protocol: it returns either nil
;; (not applicable) or a list (BEG END COLLECTION . PROPS).
;;
;; All git invocations run synchronously via `call-process'.

;;; Code:

(require 'cl-lib)


;;;; Helpers

(defun wgh/git-capf--run (&rest args)
  "Run git with ARGS and return the trimmed stdout, or nil on failure.
The process runs in `default-directory'."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "git" nil t nil args)))
      (if (zerop exit-code)
          (string-trim (buffer-string))
        nil))))

(defun wgh/git-capf--run-lines (&rest args)
  "Run git with ARGS and return a list of non-empty output lines.
Returns nil if the command fails or produces no output."
  (let ((out (apply #'wgh/git-capf--run args)))
    (when (and out (not (string-empty-p out)))
      (split-string out "\n" t "[[:space:]]+"))))

(defun wgh/git-capf--root ()
  "Return the git root for `default-directory', or nil if not in a repo."
  (wgh/git-capf--run "rev-parse" "--show-toplevel"))

(defun wgh/git-capf--bounds-of-word-at-point ()
  "Return (BEG . END) of the word-like token at or before point.
Recognises the characters used in git branch names, paths, and hashes:
letters, digits, hyphens, underscores, forward-slashes, dots, colons,
and at-signs.  Returns (POINT . POINT) when point is not on such a token."
  (save-excursion
    (let* ((beg (progn
                  (skip-chars-backward "A-Za-z0-9/_.:@-")
                  (point)))
           (end (progn
                  (skip-chars-forward "A-Za-z0-9/_.:@-")
                  (point))))
      (cons beg end))))


;;;; Branch name completion

(defun wgh/git-capf--branch-names (root &optional include-remote)
  "Return a list of branch names in the repo at ROOT.
When INCLUDE-REMOTE is non-nil, include remote-tracking branches as well."
  (let ((default-directory (file-name-as-directory root)))
    (apply #'wgh/git-capf--run-lines
           `("branch"
             ,@(when include-remote '("--all"))
             "--format=%(refname:short)"))))

(defun wgh/git-capf--submodule-paths (root)
  "Return a list of submodule paths relative to ROOT."
  (let ((default-directory (file-name-as-directory root)))
    (let ((out (wgh/git-capf--run
                "submodule" "--quiet" "foreach" "--recursive"
                "echo $displaypath")))
      (when (and out (not (string-empty-p out)))
        (split-string out "\n" t "[[:space:]]+")))))

(defun wgh/git-capf--branch-candidates (&optional include-remote)
  "Collect branch completion candidates for the current repo.
Returns a list of strings.  Branches in the root repo are returned as
plain names.  For each submodule, branches are returned as
\"repo:SUBREPO_PATH branch:BRANCH_NAME\".
When INCLUDE-REMOTE is non-nil, remote-tracking branches are included."
  (let ((root (wgh/git-capf--root)))
    (when root
      ;; Branches in the root repo.
      (let* ((root-branches (wgh/git-capf--branch-names root include-remote))
             (submodule-paths (wgh/git-capf--submodule-paths root))
             (candidates (copy-sequence root-branches)))
        ;; Branches in each submodule.
        (dolist (subpath submodule-paths)
          (let* ((subroot (expand-file-name subpath root))
                 (default-directory (file-name-as-directory subroot))
                 (sub-branches (wgh/git-capf--branch-names subroot include-remote)))
            (dolist (branch sub-branches)
              (push (format "repo:%s branch:%s" subpath branch)
                    candidates))))
        (nreverse candidates)))))

;;;###autoload
(cl-defun wgh/git-branch-capf (&key (include-remote t))
  "CAPF function for git branch names.
Completes the word at point against branch names.  By default includes
remote-tracking branches; pass :include-remote nil to restrict to local
branches only.  In repos with submodules, also offers candidates in the
form \"repo:SUBREPO_PATH branch:BRANCH_NAME\"."
  (when (wgh/git-capf--root)
    (let* ((bounds (wgh/git-capf--bounds-of-word-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (candidates (wgh/git-capf--branch-candidates include-remote)))
      (when candidates
        (list beg end candidates
              :annotation-function (lambda (_) " <git-branch>")
              :company-kind (lambda (_) 'keyword)
              :exclusive 'no)))))


;;;; Submodule path completion

(defun wgh/git-capf--submodule-candidates ()
  "Return a list of submodule paths relative to the root repo."
  (let ((root (wgh/git-capf--root)))
    (when root
      (wgh/git-capf--submodule-paths root))))

;;;###autoload
(defun wgh/git-submodule-capf ()
  "CAPF function for git submodule paths.
Completes the word at point against submodule paths relative to the
root (super-project) repository."
  (when (wgh/git-capf--root)
    (let* ((bounds (wgh/git-capf--bounds-of-word-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (candidates (wgh/git-capf--submodule-candidates)))
      (when candidates
        (list beg end candidates
              :annotation-function (lambda (_) " <git-submodule>")
              :company-kind (lambda (_) 'folder)
              :exclusive 'no)))))


;;;; Worktree name/path completion

(defun wgh/git-capf--worktree-candidates ()
  "Return a list of worktree paths for the current repo.
Each entry is the absolute path of a worktree, as reported by
\"git worktree list --porcelain\"."
  (let ((root (wgh/git-capf--root)))
    (when root
      (let ((default-directory (file-name-as-directory root)))
        (let ((out (wgh/git-capf--run "worktree" "list" "--porcelain")))
          (when out
            (let ((paths nil))
              (dolist (line (split-string out "\n" t))
                (when (string-match "\\`worktree \\(.*\\)\\'" line)
                  (push (match-string 1 line) paths)))
              (nreverse paths))))))))

;;;###autoload
(defun wgh/git-worktree-capf ()
  "CAPF function for git worktree paths.
Completes the word at point against the list of worktrees for the
current repository."
  (when (wgh/git-capf--root)
    (let* ((bounds (wgh/git-capf--bounds-of-word-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (candidates (wgh/git-capf--worktree-candidates)))
      (when candidates
        (list beg end candidates
              :annotation-function (lambda (_) " <git-worktree>")
              :company-kind (lambda (_) 'folder)
              :exclusive 'no)))))


;;;; Commit hash completion

(defun wgh/git-capf--commit-candidates (prefix)
  "Return commit candidates whose log line or author matches PREFIX.
PREFIX is searched case-insensitively against the one-line log (subject
and author name).  Each candidate is a string of the form
\"commit:HASH (commit log text: SUBJECT -- AUTHOR)\"."
  (let ((root (wgh/git-capf--root)))
    (when root
      (let* ((default-directory (file-name-as-directory root))
             ;; Format: HASH<TAB>SUBJECT<TAB>AUTHOR
             (out (wgh/git-capf--run
                   "log" "--all"
                   "--pretty=format:%H\t%s\t%aN"
                   "--max-count=500"))
             (candidates nil))
        (when out
          (dolist (line (split-string out "\n" t))
            (let* ((parts (split-string line "\t"))
                   (hash    (nth 0 parts))
                   (subject (nth 1 parts))
                   (author  (nth 2 parts)))
              (when (and hash subject author)
                ;; Include candidate if prefix matches subject or author
                ;; (case-insensitive substring match).
                (when (string-match-p (regexp-quote prefix)
                                      (concat subject " " author))
                  (push (format "commit:%s (commit log text: %s -- %s)"
                                hash subject author)
                        candidates))))))
        (nreverse candidates)))))

;;;###autoload
(defun wgh/git-commit-capf ()
  "CAPF function for git commit hashes.
The text at point is treated as a search query against commit log lines
and author names.  Matching commits are offered as candidates in the
form \"commit:HASH (commit log text: SUBJECT -- AUTHOR)\".

Completion is triggered on any non-empty word-like token at point.
Up to 500 commits from all refs are searched."
  (when (wgh/git-capf--root)
    (let* ((bounds (wgh/git-capf--bounds-of-word-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (prefix (buffer-substring-no-properties beg end)))
      ;; Only activate when the user has typed something.
      (when (> (length prefix) 0)
        (let ((candidates (wgh/git-capf--commit-candidates prefix)))
          (when candidates
            (list beg end candidates
                  :annotation-function (lambda (_) " <git-commit>")
                  :company-kind (lambda (_) 'text)
                  :exclusive 'no)))))))


;;;; File path completion

(defun wgh/git-capf--file-candidates ()
  "Return a list of file paths tracked by git in the current repo.
Paths are relative to the root of the repository."
  (let ((root (wgh/git-capf--root)))
    (when root
      (let ((default-directory (file-name-as-directory root)))
        (wgh/git-capf--run-lines "ls-files")))))

;;;###autoload
(defun wgh/git-file-capf ()
  "CAPF function for git-tracked file paths.
Completes the word at point against the list of files tracked by git,
with paths relative to the repository root."
  (when (wgh/git-capf--root)
    (let* ((bounds (wgh/git-capf--bounds-of-word-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (candidates (wgh/git-capf--file-candidates)))
      (when candidates
        (list beg end candidates
              :annotation-function (lambda (_) " <git-file>")
              :company-kind (lambda (_) 'file)
              :exclusive 'no)))))


(provide 'git-capf)

;;; git-capf.el ends here
