;;; wade-review-generate.el --- Generate org-mode branch review files -*- lexical-binding: t; -*-

;;; Commentary:

;; The generator behind the `wade-review' CLI.  It turns the diff between
;; a branch and its merge base into an org file with a TODO heading per file
;; and per hunk, each hunk in a diff src block.
;;
;; This file must only depend on built-in Emacs libraries and the git CLI, so
;; the CLI works from `emacs -Q --batch' on any machine.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst wade-review-generate--usage
  "Usage: wade-review [-b BRANCH] -o OUTPUT [--base REF] [--force] [-C DIR]

Generate an org-mode review file for the changes on BRANCH since its merge
base with REF.

  -b, --branch BRANCH  Commit-ish to review (branch, remote branch, SHA).
                       Default: the current branch.
  -o, --output FILE    Org file to write.
      --base REF       Base to diff against.  Default: the default branch
                       named by origin/HEAD (or the only remote's HEAD),
                       local if it exists, else remote-tracking; without a
                       remote HEAD, a local main or master.
      --force          Overwrite FILE if it exists.
  -C DIR               Run as if started in DIR.
  -h, --help           Show this help.
")

(define-error 'wade-review-generate-error "wade-review")

(defun wade-review-generate--fail (fmt &rest args)
  (signal 'wade-review-generate-error (list (apply #'format fmt args))))

(defun wade-review-generate--git-status (&rest args)
  "Run git with ARGS in `default-directory'.  Return (STATUS STDOUT STDERR)."
  (let ((errfile (make-temp-file "wr-git-err")))
    (unwind-protect
        (with-temp-buffer
          (let* ((coding-system-for-read 'utf-8)
                 (status (apply #'call-process "git" nil (list t errfile) nil
                                ;; Paths are passed literally, never as globs
                                ;; or other pathspec magic.
                                "--literal-pathspecs" args)))
            (list status (buffer-string)
                  (with-temp-buffer
                    (insert-file-contents errfile)
                    (string-trim (buffer-string))))))
      (delete-file errfile))))

(defun wade-review-generate--git (&rest args)
  "Run git with ARGS and return its stdout, or fail with its stderr."
  (pcase-let ((`(,status ,out ,err) (apply #'wade-review-generate--git-status args)))
    (unless (eq status 0)
      (wade-review-generate--fail "git %s failed: %s" (string-join args " ") err))
    out))

(defun wade-review-generate--git-line (&rest args)
  "Like `wade-review-generate--git' but return trimmed output."
  (string-trim (apply #'wade-review-generate--git args)))

(defun wade-review-generate--git-ok (&rest args)
  "Run git with ARGS; return trimmed stdout on success, nil on failure."
  (pcase-let ((`(,status ,out ,_err) (apply #'wade-review-generate--git-status args)))
    (and (eq status 0) (string-trim out))))

(defun wade-review-generate--remote-head (remote)
  "Return REMOTE's default branch as (REMOTE . NAME), or nil."
  (let ((ref (wade-review-generate--git-ok
              "symbolic-ref" "-q" (format "refs/remotes/%s/HEAD" remote)))
        (prefix (format "refs/remotes/%s/" remote)))
    (when (and ref (string-prefix-p prefix ref))
      (cons remote (substring ref (length prefix))))))

(defun wade-review-generate--local-branch-p (name)
  (wade-review-generate--git-ok "rev-parse" "--verify" "-q" (concat "refs/heads/" name)))

(defun wade-review-generate-default-base ()
  "Return the ref to use as the review base when none is given.
The default branch's name comes from the remote's HEAD (origin, or the only
remote), so it works whatever that branch is called.  The local branch of
that name is preferred, since the reviewed branch may be based on unpushed
work there; otherwise the remote-tracking branch is used.  Without a remote
HEAD, a local main or master is used."
  (let ((remote-head
         (or (wade-review-generate--remote-head "origin")
             (let ((remotes (split-string (or (wade-review-generate--git-ok "remote") "")
                                          "\n" t)))
               (and (= (length remotes) 1)
                    (wade-review-generate--remote-head (car remotes)))))))
    (or (and remote-head
             (if (wade-review-generate--local-branch-p (cdr remote-head))
                 (cdr remote-head)
               (format "%s/%s" (car remote-head) (cdr remote-head))))
        (cl-find-if #'wade-review-generate--local-branch-p '("main" "master"))
        (wade-review-generate--fail
         "cannot determine the default branch; pass --base REF"))))

;;;; Diff parsing

(defconst wade-review-generate--diff-args
  ;; Explicit options override user git config that changes the output format
  ;; (color, external diff drivers, textconv, noprefix, mnemonicPrefix).
  '("diff" "--no-color" "--no-ext-diff" "--no-textconv" "--no-relative" "-M"
    "--src-prefix=a/" "--dst-prefix=b/"))

(defconst wade-review-generate--hunk-header-re
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@ ?\\(.*\\)$")

(defconst wade-review-generate--status-names
  '((?M . "modified") (?A . "added") (?D . "deleted") (?R . "renamed")
    (?C . "copied") (?T . "type-changed")))

(defun wade-review-generate--changed-files (from to)
  "Return changed files between FROM and TO as plists (:status :file :old-file)."
  (let ((fields (split-string
                 (wade-review-generate--git
                  "diff" "--no-color" "--no-ext-diff" "--no-relative" "-M"
                  "--name-status" "-z" from to)
                 "\0" t))
        files)
    (while fields
      (let* ((code (aref (pop fields) 0))
             (status (or (cdr (assq code wade-review-generate--status-names))
                         (string code))))
        (if (memq code '(?R ?C))
            (let* ((old (pop fields)) (new (pop fields)))
              (push (list :status status :file new :old-file old) files))
          (let ((file (pop fields)))
            (push (list :status status :file file
                        :old-file (unless (eq code ?A) file))
                  files)))))
    (nreverse files)))

(defun wade-review-generate-parse-hunks (diff-text)
  "Parse the hunks of a single-file unified DIFF-TEXT.
Return a list of plists (:old-start :old-count :new-start :new-count
:context :header :lines), where :lines are the hunk body lines.  Return
the symbol `binary' for a binary diff."
  (let ((lines (split-string diff-text "\n"))
        hunks current)
    ;; split-string leaves an empty string after the final newline.
    (when (equal (car (last lines)) "")
      (setq lines (butlast lines)))
    (if (cl-some (lambda (l) (or (string-prefix-p "Binary files " l)
                                 (string= l "GIT binary patch")))
                 lines)
        'binary
      (dolist (line lines)
        (cond
         ((string-match wade-review-generate--hunk-header-re line)
          (when current (push current hunks))
          (setq current
                (list :old-start (string-to-number (match-string 1 line))
                      :old-count (if (match-string 2 line)
                                     (string-to-number (match-string 2 line)) 1)
                      :new-start (string-to-number (match-string 3 line))
                      :new-count (if (match-string 4 line)
                                     (string-to-number (match-string 4 line)) 1)
                      :context (match-string 5 line)
                      :header line
                      :lines nil)))
         (current
          (plist-put current :lines (cons line (plist-get current :lines))))))
      (when current (push current hunks))
      (mapcar (lambda (h) (plist-put h :lines (nreverse (plist-get h :lines))))
              (nreverse hunks)))))

;;;; Org output

(defun wade-review-generate--escape-line (line)
  "Escape LINE for an org src block.
Only lines that are whitespace then `#+' need it: diff lines never start
with `*', and org would end the block early at an indented `#+end_src'."
  (if (string-match "\\`[ \t]*\\(,*#\\+\\)" line)
      (concat (substring line 0 (match-beginning 1)) "," (substring line (match-beginning 1)))
    line))

(defun wade-review-generate--properties (alist)
  (concat ":PROPERTIES:\n"
          (mapconcat (lambda (kv) (format ":%s: %s\n" (car kv) (cdr kv)))
                     (cl-remove-if-not #'cdr alist) "")
          ":END:\n"))

(defun wade-review-generate--file-section (file hunks)
  "Return the org text for FILE (a plist) with its parsed HUNKS."
  (let* ((status (plist-get file :status))
         (path (plist-get file :file))
         (old (plist-get file :old-file))
         (file-props `(("WR_FILE" . ,path)
                       ("WR_OLD_FILE" . ,old)
                       ("WR_STATUS" . ,status)))
         (binary (eq hunks 'binary))
         (hunks (if binary nil hunks))
         (added 0) (removed 0))
    (dolist (h hunks)
      (dolist (l (plist-get h :lines))
        (cond ((string-prefix-p "+" l) (cl-incf added))
              ((string-prefix-p "-" l) (cl-incf removed)))))
    (concat
     (format "** TODO %s (%s)%s\n"
             path
             (string-join
              (delq nil
                    (list (unless (equal status "modified")
                            (if (member status '("renamed" "copied"))
                                (format "%s from %s" status old)
                              status))
                          (if binary "binary" (format "+%d/-%d" added removed))))
              ", ")
             (if hunks " [/]" ""))
     (wade-review-generate--properties file-props)
     (mapconcat
      (lambda (h)
        (concat
         (format "*** TODO %s\n" (plist-get h :header))
         (wade-review-generate--properties
          (append file-props
                  `(("WR_OLD_START" . ,(number-to-string (plist-get h :old-start)))
                    ("WR_NEW_START" . ,(number-to-string (plist-get h :new-start))))))
         "#+begin_src diff\n"
         ;; The file header lets diff-mode pick the language for syntax
         ;; highlighting inside the block.
         (format "--- %s\n" (if old (concat "a/" old) "/dev/null"))
         (format "+++ %s\n" (if (equal status "deleted") "/dev/null" (concat "b/" path)))
         (plist-get h :header) "\n"
         (mapconcat (lambda (l) (concat (wade-review-generate--escape-line l) "\n"))
                    (plist-get h :lines) "")
         "#+end_src\n"))
      hunks ""))))

(defun wade-review-generate--shell-quote-args (args)
  (mapconcat (lambda (a)
               (if (string-match-p "\\`[A-Za-z0-9_./=:@%+,-]+\\'" a)
                   a
                 (concat "'" (replace-regexp-in-string "'" "'\\\\''" a) "'")))
             args " "))

(defun wade-review-generate--current-branch ()
  "Return the checked out branch's name, or \"HEAD\" if HEAD is detached."
  (or (wade-review-generate--git-ok "symbolic-ref" "--short" "-q" "HEAD")
      "HEAD"))

(defun wade-review-generate (&optional branch base command)
  "Return the review org text for BRANCH against BASE in `default-directory'.
BRANCH defaults to the current branch.  It is an error if BRANCH is its own
merge base with BASE, since there would be nothing to review.
COMMAND, if given, is recorded in the file as the command that made it."
  (let* ((toplevel (wade-review-generate--git-ok "rev-parse" "--show-toplevel"))
         ;; Paths from git diff are relative to the top of the working tree,
         ;; but pathspecs are relative to the current directory.
         (default-directory (if toplevel
                                (file-name-as-directory toplevel)
                              default-directory))
         (branch (or branch (wade-review-generate--current-branch)))
         (base (or base (wade-review-generate-default-base)))
         (tip (wade-review-generate--git-line
               "rev-parse" "--verify" "-q" (concat branch "^{commit}")))
         (base-tip (wade-review-generate--git-line
                    "rev-parse" "--verify" "-q" (concat base "^{commit}")))
         (merge-base (wade-review-generate--git-line "merge-base" base-tip tip))
         (_ (when (equal tip merge-base)
              (wade-review-generate--fail
               "%s is the merge base with %s, so there is nothing to review"
               branch base)))
         (git-dir (wade-review-generate--git-line
                   "rev-parse" "--path-format=absolute" "--git-common-dir"))
         (files (wade-review-generate--changed-files merge-base tip)))
    (concat
     "# -*- mode: org; mode: wade-review -*-\n"
     (format "#+TITLE: Review %s\n" branch)
     "#+STARTUP: content\n"
     (mapconcat (lambda (kv) (format "#+WR_%s: %s\n" (car kv) (cdr kv)))
                (cl-remove-if-not
                 #'cdr
                 `(("REPO" . ,toplevel)
                   ("GIT_DIR" . ,git-dir)
                   ("BRANCH" . ,branch)
                   ("BASE" . ,base)
                   ("BASE_TIP" . ,base-tip)
                   ("MERGE_BASE" . ,merge-base)
                   ("TIP" . ,tip)
                   ("GENERATED" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                   ("COMMAND" . ,command)))
                "")
     "\n"
     (format "* Review %s [/]\n" branch)
     (mapconcat
      (lambda (file)
        (wade-review-generate--file-section
         file
         (wade-review-generate-parse-hunks
          (apply #'wade-review-generate--git
                 (append wade-review-generate--diff-args
                         (list merge-base tip "--")
                         (delete-dups (delq nil (list (plist-get file :old-file)
                                                      (plist-get file :file)))))))))
      files ""))))

;;;; CLI

(defun wade-review-generate--parse-args (args)
  "Parse CLI ARGS into a plist."
  (let (opts)
    (while args
      (let ((arg (pop args)))
        (when (string-match "\\`\\(--[a-z]+\\)=\\(.*\\)\\'" arg)
          (push (match-string 2 arg) args)
          (setq arg (match-string 1 arg)))
        (cl-flet ((value () (or (pop args)
                                (wade-review-generate--fail "%s needs a value" arg))))
          (pcase arg
            ((or "-b" "--branch") (setq opts (plist-put opts :branch (value))))
            ((or "-o" "--output") (setq opts (plist-put opts :output (value))))
            ("--base" (setq opts (plist-put opts :base (value))))
            ("-C" (setq opts (plist-put opts :dir (value))))
            ("--force" (setq opts (plist-put opts :force t)))
            ((or "-h" "--help") (setq opts (plist-put opts :help t)))
            (_ (wade-review-generate--fail "unknown argument: %s" arg))))))
    opts))

(defun wade-review-generate-main (args)
  "CLI entry point: generate a review file as directed by ARGS, then exit."
  (condition-case err
      (let ((opts (wade-review-generate--parse-args args)))
        (when (plist-get opts :help)
          (princ wade-review-generate--usage)
          (kill-emacs 0))
        (unless (plist-get opts :output)
          (wade-review-generate--fail
           "--output is required\n\n%s" wade-review-generate--usage))
        (let* ((output (expand-file-name (plist-get opts :output)))
               (default-directory (file-name-as-directory
                                   (expand-file-name (or (plist-get opts :dir) ".")))))
          (when (and (file-exists-p output) (not (plist-get opts :force)))
            (wade-review-generate--fail
             "%s exists; pass --force to overwrite it" output))
          (let ((text (wade-review-generate
                       (plist-get opts :branch) (plist-get opts :base)
                       (wade-review-generate--shell-quote-args
                        (cons "wade-review" args))))
                (coding-system-for-write 'utf-8-unix))
            (make-directory (file-name-directory output) t)
            (write-region text nil output nil 'silent)
            (princ (concat output "\n"))
            (kill-emacs 0))))
    (wade-review-generate-error
     (message "wade-review: %s" (cadr err))
     (kill-emacs 1))))

(provide 'wade-review-generate)

;;; wade-review-generate.el ends here
