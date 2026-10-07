;;; wade-review-generate.el --- Generate org-mode branch review files -*- lexical-binding: t; -*-

;;; Commentary:

;; The generator behind the `wade-review' CLI.  It turns a direct or
;; merge-base comparison into an org file with a TODO heading per file and
;; per hunk, each hunk in a diff src block.
;;
;; This file must only depend on built-in Emacs libraries and the git CLI, so
;; the CLI works from `emacs -Q --batch' on any machine.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst wade-review-generate--usage
  "Usage: wade-review [-b BRANCH] -o OUTPUT [--base REF | --from REF] [--force] [-C DIR]

Generate an org-mode review file for changes ending at BRANCH.  By default,
compare its merge base with the repository's default branch.  Use --from to
compare an exact pair of commits instead.

  -b, --branch BRANCH  Commit-ish to review (branch, remote branch, SHA).
                       Default: the current branch.
  -o, --output FILE    Org file to write.
      --base REF       Diff merge-base(REF, BRANCH) against BRANCH.
                       Default REF: the default branch
                       named by origin/HEAD (or the only remote's HEAD),
                       local if it exists, else remote-tracking; without a
                       remote HEAD, a local main or master.
      --from REF       Diff REF directly against BRANCH, without finding a
                       merge base.  Mutually exclusive with --base.
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

(defun wade-review-generate--changed-files (from &optional to)
  "Return changed files between FROM and TO as plists (:status :file :old-file)."
  (let ((fields (split-string
                 (apply #'wade-review-generate--git
                        (append '("diff" "--no-color" "--no-ext-diff" "--no-relative" "-M"
                                  "--name-status" "-z")
                                (list from) (when to (list to))))
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
:context :header :lines :moved-old :moved-new), where :lines are the hunk
body lines and moved lists are populated by move-aware diff parsing.  Return
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
                      :lines nil
                      :moved-old nil :moved-new nil)))
         (current
          (plist-put current :lines (cons line (plist-get current :lines))))))
      (when current (push current hunks))
      (mapcar (lambda (h) (plist-put h :lines (nreverse (plist-get h :lines))))
              (nreverse hunks)))))

(defconst wade-review-generate--ansi-re "\033\\[[0-9;]*m")

(defun wade-review-generate--move-ranges (numbers)
  "Encode NUMBERS as comma-separated, inclusive line ranges."
  (when numbers
    (let ((start (car numbers)) (end (car numbers)) ranges)
      (dolist (n (cdr numbers))
        (if (= n (1+ end))
            (setq end n)
          (push (if (= start end) (number-to-string start)
                  (format "%d-%d" start end)) ranges)
          (setq start n end n)))
      (push (if (= start end) (number-to-string start)
              (format "%d-%d" start end)) ranges)
      (string-join (nreverse ranges) ","))))

(defun wade-review-generate--moved-hunks (section)
  "Parse a colored Git diff SECTION and mark moved lines in its hunks."
  (let* ((raw-lines (split-string section "\n"))
         (plain-lines (mapcar (lambda (line)
                                (replace-regexp-in-string
                                 wade-review-generate--ansi-re "" line))
                              raw-lines))
         (hunks (wade-review-generate-parse-hunks
                 (string-join plain-lines "\n")))
         (remaining hunks) current old new)
    (cl-mapc
     (lambda (raw plain)
       (cond
        ((string-match wade-review-generate--hunk-header-re plain)
         (setq current (pop remaining)
               old (string-to-number (match-string 1 plain))
               new (string-to-number (match-string 3 plain))))
        ((and current (> (length plain) 0))
         (pcase (aref plain 0)
           (?- (when (string-prefix-p "\033[34m-" raw)
                 (push old (plist-get current :moved-old)))
               (cl-incf old))
           (?+ (when (string-prefix-p "\033[33m+" raw)
                 (push new (plist-get current :moved-new)))
               (cl-incf new))
           (?\s (cl-incf old) (cl-incf new))))))
     raw-lines plain-lines)
    (unless (eq hunks 'binary)
      (dolist (h hunks)
        (plist-put h :moved-old (nreverse (plist-get h :moved-old)))
        (plist-put h :moved-new (nreverse (plist-get h :moved-new)))))
    hunks))

(defun wade-review-generate--review-files (from &optional to zero-context)
  "Return changed files from FROM to TO, with move-aware parsed hunks.
When TO is nil, compare FROM to the working tree.  ZERO-CONTEXT uses -U0."
  (let* ((files (wade-review-generate--changed-files from to))
         (colored (apply #'wade-review-generate--git
                         (append '("-c" "color.diff.oldMoved=blue"
                                   "-c" "color.diff.newMoved=yellow")
                                 wade-review-generate--diff-args
                                 ;; Git's blocks mode ignores isolated trivial
                                 ;; matches, at the cost of missing moved blocks
                                 ;; below its fixed 20-alphanumeric threshold.
                                 '("--color=always" "--color-moved=blocks"
                                   "--color-moved-ws=no")
                                 (when zero-context '("-U0"))
                                 (list from) (when to (list to)))))
         (sections nil) current)
    (dolist (line (split-string colored "\n"))
      (if (string-prefix-p "diff --git "
                           (replace-regexp-in-string wade-review-generate--ansi-re "" line))
          (progn
            (when current (push (string-join (nreverse current) "\n") sections))
            (setq current (list line)))
        (when current (push line current))))
    (when current (push (string-join (nreverse current) "\n") sections))
    (setq sections (nreverse sections))
    (unless (= (length files) (length sections))
      (wade-review-generate--fail "Git diff file count changed while detecting moved code"))
    (cl-mapcar (lambda (file section)
                 (plist-put file :hunks (wade-review-generate--moved-hunks section)))
               files sections)))

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
                    ("WR_NEW_START" . ,(number-to-string (plist-get h :new-start)))
                    ("WR_MOVED_OLD" . ,(wade-review-generate--move-ranges
                                         (plist-get h :moved-old)))
                    ("WR_MOVED_NEW" . ,(wade-review-generate--move-ranges
                                         (plist-get h :moved-new))))))
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

(defun wade-review-generate (&optional branch base from command)
  "Return review org text for BRANCH from a selected comparison start.
With FROM, compare FROM directly to BRANCH.  Otherwise compare the merge base
of BASE and BRANCH to BRANCH, defaulting BASE to the default branch.  BRANCH
defaults to the current branch.  BASE and FROM are mutually exclusive.
COMMAND, if given, is recorded in the file as the command that made it."
  (when (and base from)
    (wade-review-generate--fail "--base and --from are mutually exclusive"))
  (let* ((toplevel (wade-review-generate--git-ok "rev-parse" "--show-toplevel"))
         ;; Paths from git diff are relative to the top of the working tree,
         ;; but pathspecs are relative to the current directory.
         (default-directory (if toplevel
                                (file-name-as-directory toplevel)
                              default-directory))
         (branch (or branch (wade-review-generate--current-branch)))
         (comparison (if from "direct" "merge-base"))
         (base (unless from (or base (wade-review-generate-default-base))))
         (tip (wade-review-generate--git-line
               "rev-parse" "--verify" "-q" (concat branch "^{commit}")))
         (base-tip (and base
                        (wade-review-generate--git-line
                         "rev-parse" "--verify" "-q" (concat base "^{commit}"))))
         (from-tip (if from
                       (wade-review-generate--git-line
                        "rev-parse" "--verify" "-q" (concat from "^{commit}"))
                     (wade-review-generate--git-line "merge-base" base-tip tip)))
         (_ (when (equal tip from-tip)
              (wade-review-generate--fail
               (if from
                   "%s and --from %s resolve to the same commit, so there is nothing to review"
                 "%s is the merge base with %s, so there is nothing to review")
               branch (or from base))))
         (git-dir (wade-review-generate--git-line
                   "rev-parse" "--path-format=absolute" "--git-common-dir"))
         (files (wade-review-generate--review-files from-tip tip)))
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
                   ("COMPARISON" . ,comparison)
                   ("FROM" . ,from-tip)
                   ("FROM_REF" . ,from)
                   ("BASE" . ,base)
                   ("BASE_TIP" . ,base-tip)
                   ("MERGE_BASE" . ,(and base from-tip))
                   ("TIP" . ,tip)
                   ("GENERATED" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                   ("COMMAND" . ,command)))
                "")
     "\n"
     (format "* Review %s [/]\n" branch)
     (mapconcat
      (lambda (file)
        (wade-review-generate--file-section file (plist-get file :hunks)))
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
            ("--from" (setq opts (plist-put opts :from (value))))
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
        (when (and (plist-get opts :base) (plist-get opts :from))
          (wade-review-generate--fail "--base and --from are mutually exclusive"))
        (let* ((output (expand-file-name (plist-get opts :output)))
               (default-directory (file-name-as-directory
                                   (expand-file-name (or (plist-get opts :dir) ".")))))
          (when (and (file-exists-p output) (not (plist-get opts :force)))
            (wade-review-generate--fail
             "%s exists; pass --force to overwrite it" output))
          (let ((text (wade-review-generate
                       (plist-get opts :branch) (plist-get opts :base)
                       (plist-get opts :from)
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
