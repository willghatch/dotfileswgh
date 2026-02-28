;;; git-permalink.el --- Generate permalinks for files in git repos  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides commands to generate permalinks (permanent URLs) for the
;; current file position in a git repository.  For GitHub, GitLab, and
;; Sourcehut remotes, generates proper permalink URLs pinned to the
;; current commit.  For other remotes or no remote, generates a
;; generic git-permalink:// URI.

;;; Code:

(defun wgh/git-permalink--shell-command (command &optional dir)
  "Run COMMAND in DIR (or `default-directory') and return trimmed output.
Returns nil if the command fails (non-zero exit)."
  (let ((default-directory (or dir default-directory)))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command command nil t)))
        (if (zerop exit-code)
            (string-trim (buffer-string))
          nil)))))

(defun wgh/git-permalink--git-root ()
  "Return the git repository root for the current buffer's file."
  (let ((root (wgh/git-permalink--shell-command "git rev-parse --show-toplevel")))
    (when root
      (file-name-as-directory root))))

(defun wgh/git-permalink--commit-hash ()
  "Return the current HEAD commit hash."
  (wgh/git-permalink--shell-command "git rev-parse HEAD"))

(defun wgh/git-permalink--relative-path (file git-root)
  "Return FILE's path relative to GIT-ROOT."
  (file-relative-name file git-root))

(defun wgh/git-permalink--file-modified-p (relative-path)
  "Check if RELATIVE-PATH has uncommitted modifications (staged or unstaged).
Returns a list of modification types found, or nil if clean."
  (let ((unstaged (wgh/git-permalink--shell-command
                   (format "git diff --name-only -- %s"
                           (shell-quote-argument relative-path))))
        (staged (wgh/git-permalink--shell-command
                 (format "git diff --cached --name-only -- %s"
                         (shell-quote-argument relative-path))))
        (result nil))
    (when (and staged (not (string-empty-p staged)))
      (push "staged" result))
    (when (and unstaged (not (string-empty-p unstaged)))
      (push "unstaged" result))
    result))

(defun wgh/git-permalink--remote-url (&optional remote)
  "Return the URL for REMOTE (default \"origin\")."
  (wgh/git-permalink--shell-command
   (format "git remote get-url %s" (or remote "origin"))))

;;;; Remote URL parsing

(defun wgh/git-permalink--parse-github-remote (url)
  "Parse a GitHub remote URL and return (OWNER . REPO), or nil if not GitHub.
Handles SSH (git@github.com:owner/repo.git) and
HTTPS (https://github.com/owner/repo.git) formats."
  (cond
   ;; SSH format: git@github.com:owner/repo.git
   ((string-match "git@github\\.com:\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (cons (match-string 1 url) (match-string 2 url)))
   ;; HTTPS format: https://github.com/owner/repo.git
   ((string-match "https?://github\\.com/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (cons (match-string 1 url) (match-string 2 url)))
   ;; SSH format with ssh:// prefix: ssh://git@github.com/owner/repo.git
   ((string-match "ssh://git@github\\.com/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (cons (match-string 1 url) (match-string 2 url)))
   (t nil)))

(defun wgh/git-permalink--parse-gitlab-remote (url)
  "Parse a GitLab remote URL and return (BASE-URL OWNER REPO), or nil.
Handles self-hosted GitLab instances as well as gitlab.com.
Recognizes SSH (git@host:owner/repo.git),
HTTPS (https://host/owner/repo.git), and
ssh:// (ssh://git@host/owner/repo.git) formats.
The heuristic is that any host containing \"gitlab\" is treated as GitLab."
  (cond
   ;; SSH format: git@gitlab.example.com:owner/repo.git
   ((string-match "git@\\([^:]*gitlab[^:]*\\):\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (format "https://%s" (match-string 1 url))
          (match-string 2 url)
          (match-string 3 url)))
   ;; HTTPS format: https://gitlab.example.com/owner/repo.git
   ((string-match "\\(https?://[^/]*gitlab[^/]*\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (match-string 1 url)
          (match-string 2 url)
          (match-string 3 url)))
   ;; SSH format with ssh:// prefix: ssh://git@gitlab.example.com/owner/repo.git
   ((string-match "ssh://git@\\([^/]*gitlab[^/]*\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (format "https://%s" (match-string 1 url))
          (match-string 2 url)
          (match-string 3 url)))
   (t nil)))

(defun wgh/git-permalink--parse-sourcehut-remote (url)
  "Parse a Sourcehut remote URL and return (BASE-URL OWNER REPO), or nil.
Handles git.sr.ht as well as self-hosted Sourcehut instances
whose hostname contains \"sr.ht\".
Recognizes SSH (git@git.sr.ht:~owner/repo),
HTTPS (https://git.sr.ht/~owner/repo), and
ssh:// (ssh://git@git.sr.ht/~owner/repo) formats.
OWNER is returned with its leading tilde (e.g. \"~user\")."
  (cond
   ;; SSH format: git@git.sr.ht:~owner/repo
   ((string-match "git@\\([^:]*sr\\.ht\\):\\(~[^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (format "https://%s" (match-string 1 url))
          (match-string 2 url)
          (match-string 3 url)))
   ;; HTTPS format: https://git.sr.ht/~owner/repo
   ((string-match "\\(https?://[^/]*sr\\.ht\\)/\\(~[^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (match-string 1 url)
          (match-string 2 url)
          (match-string 3 url)))
   ;; SSH format with ssh:// prefix: ssh://git@git.sr.ht/~owner/repo
   ((string-match "ssh://git@\\([^/]*sr\\.ht\\)/\\(~[^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?$" url)
    (list (format "https://%s" (match-string 1 url))
          (match-string 2 url)
          (match-string 3 url)))
   (t nil)))

;;;; URL builders

(defun wgh/git-permalink--github-url (owner repo commit-hash relative-path
                                              line-start &optional line-end)
  "Build a GitHub permalink URL.
OWNER and REPO identify the GitHub repository.
COMMIT-HASH pins the URL to a specific commit.
RELATIVE-PATH is the file path within the repo.
LINE-START is the starting line number.
LINE-END, if non-nil and different from LINE-START, adds a line range."
  (let ((base (format "https://github.com/%s/%s/blob/%s/%s"
                       owner repo commit-hash relative-path)))
    (if (and line-end (not (= line-start line-end)))
        (format "%s#L%d-L%d" base line-start line-end)
      (format "%s#L%d" base line-start))))

(defun wgh/git-permalink--gitlab-url (base-url owner repo commit-hash
                                               relative-path line-start
                                               &optional line-end)
  "Build a GitLab permalink URL.
BASE-URL is the GitLab instance URL (e.g. \"https://gitlab.com\").
OWNER and REPO identify the repository.
COMMIT-HASH pins the URL to a specific commit.
RELATIVE-PATH is the file path within the repo.
LINE-START is the starting line number.
LINE-END, if non-nil and different from LINE-START, adds a line range."
  (let ((base (format "%s/%s/%s/-/blob/%s/%s"
                       base-url owner repo commit-hash relative-path)))
    (if (and line-end (not (= line-start line-end)))
        (format "%s#L%d-%d" base line-start line-end)
      (format "%s#L%d" base line-start))))

(defun wgh/git-permalink--sourcehut-url (base-url owner repo commit-hash
                                                  relative-path line-start
                                                  &optional line-end)
  "Build a Sourcehut permalink URL.
BASE-URL is the Sourcehut instance URL (e.g. \"https://git.sr.ht\").
OWNER is the owner with tilde prefix (e.g. \"~user\").
REPO identifies the repository.
COMMIT-HASH pins the URL to a specific commit.
RELATIVE-PATH is the file path within the repo.
LINE-START is the starting line number.
LINE-END is currently unused, as Sourcehut does not support line ranges."
  (let ((base (format "%s/%s/%s/tree/%s/item/%s"
                       base-url owner repo commit-hash relative-path)))
    (format "%s#L%d" base line-start)))

(defun wgh/git-permalink--generic-url (commit-hash relative-path line column)
  "Build a generic permalink URI.
COMMIT-HASH, RELATIVE-PATH, LINE, and COLUMN identify the location."
  (format "git-permalink://%s/%s#L%d:C%d" commit-hash relative-path line column))

;;;; Core generation logic

(defun wgh/git-permalink--generate (&optional require-forge)
  "Generate a permalink for the current buffer position.
If REQUIRE-FORGE is non-nil, it should be a symbol naming a specific
forge: `github', `gitlab', or `sourcehut'.  If the remote does not
match the requested forge, an error is signaled.
Returns the permalink string."
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((git-root (wgh/git-permalink--git-root)))
    (unless git-root
      (user-error "Not in a git repository"))
    (let* ((file (file-truename buffer-file-name))
           (relative-path (wgh/git-permalink--relative-path file git-root))
           (commit-hash (wgh/git-permalink--commit-hash))
           (modifications (wgh/git-permalink--file-modified-p relative-path))
           (remote-url (wgh/git-permalink--remote-url))
           (line-start (line-number-at-pos (if (use-region-p) (region-beginning) (point))))
           (line-end (when (use-region-p)
                       (line-number-at-pos (region-end))))
           (column (current-column)))
      (unless commit-hash
        (user-error "Could not determine HEAD commit"))
      ;; Warn about modifications
      (when modifications
        (let ((mod-types (string-join modifications " and ")))
          (unless (yes-or-no-p
                   (format "File has %s modifications; permalink may not match.  Continue? "
                           mod-types))
            (user-error "Aborted"))))
      ;; Adjust line-end: if region ends at beginning of a line, don't include
      ;; that line (it's the line after the selection).
      (when (and line-end (use-region-p))
        (let ((end-col (save-excursion
                         (goto-char (region-end))
                         (current-column))))
          (when (and (zerop end-col) (> line-end line-start))
            (setq line-end (1- line-end)))))
      ;; Parse remote URL for each forge type
      (let ((github-info (when remote-url
                           (wgh/git-permalink--parse-github-remote remote-url)))
            (gitlab-info (when remote-url
                           (wgh/git-permalink--parse-gitlab-remote remote-url)))
            (sourcehut-info (when remote-url
                              (wgh/git-permalink--parse-sourcehut-remote remote-url))))
        (cond
         ;; If a specific forge is required, use only that one
         ((eq require-forge 'github)
          (if github-info
              (wgh/git-permalink--github-url
               (car github-info) (cdr github-info)
               commit-hash relative-path line-start line-end)
            (user-error "Remote is not a GitHub repository: %s"
                        (or remote-url "no remote configured"))))
         ((eq require-forge 'gitlab)
          (if gitlab-info
              (wgh/git-permalink--gitlab-url
               (nth 0 gitlab-info) (nth 1 gitlab-info) (nth 2 gitlab-info)
               commit-hash relative-path line-start line-end)
            (user-error "Remote is not a GitLab repository: %s"
                        (or remote-url "no remote configured"))))
         ((eq require-forge 'sourcehut)
          (if sourcehut-info
              (wgh/git-permalink--sourcehut-url
               (nth 0 sourcehut-info) (nth 1 sourcehut-info) (nth 2 sourcehut-info)
               commit-hash relative-path line-start line-end)
            (user-error "Remote is not a Sourcehut repository: %s"
                        (or remote-url "no remote configured"))))
         ;; Auto-detect: try each forge in turn
         (github-info
          (wgh/git-permalink--github-url
           (car github-info) (cdr github-info)
           commit-hash relative-path line-start line-end))
         (gitlab-info
          (wgh/git-permalink--gitlab-url
           (nth 0 gitlab-info) (nth 1 gitlab-info) (nth 2 gitlab-info)
           commit-hash relative-path line-start line-end))
         (sourcehut-info
          (wgh/git-permalink--sourcehut-url
           (nth 0 sourcehut-info) (nth 1 sourcehut-info) (nth 2 sourcehut-info)
           commit-hash relative-path line-start line-end))
         (t
          (wgh/git-permalink--generic-url
           commit-hash relative-path line-start column)))))))

;;;###autoload
(defun wgh/git-permalink ()
  "Generate a permalink for the current file position and copy it to the kill ring.
For GitHub, GitLab, and Sourcehut remotes, generates a forge-specific
permalink URL.  For other remotes, generates a generic
git-permalink:// URI.
If a region is active, the permalink covers the selected line range."
  (interactive)
  (let ((permalink (wgh/git-permalink--generate)))
    (wgh/cpo-copy-string permalink cpo-copy-default-register)))

(provide 'git-permalink)

;;; git-permalink.el ends here
