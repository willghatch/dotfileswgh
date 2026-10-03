;;; wade-review-tests.el --- Tests for wade-review -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with ./run-tests.sh.  The tests build throwaway git repositories, run
;; the real CLI, and drive the Emacs commands against the generated files.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'wade-review)

(defconst wade-review-tests--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst wade-review-tests--cli
  (expand-file-name "wade-review" wade-review-tests--dir))

(defmacro wade-review-tests--with-git-env (&rest body)
  "Run BODY with git isolated from the user's and system git configuration."
  (declare (indent 0))
  `(let ((process-environment
          (append '("GIT_CONFIG_GLOBAL=/dev/null"
                    "GIT_CONFIG_NOSYSTEM=1"
                    "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=t@example.com"
                    "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=t@example.com")
                  process-environment)))
     ,@body))

(defmacro wade-review-tests--with-temp-dir (var &rest body)
  "Bind VAR to a fresh temporary directory (with trailing slash) around BODY."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "wr-test-" t))))
     (unwind-protect
         (wade-review-tests--with-git-env ,@body)
       ;; Buffers visiting files under the temp dir would otherwise leak into
       ;; later tests.
       (dolist (buf (buffer-list))
         (let ((f (buffer-file-name buf)))
           (when (and f (string-prefix-p (file-truename ,var) (file-truename f)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf))))
       (delete-directory ,var t))))

(defun wade-review-tests--git (dir &rest args)
  "Run git with ARGS in DIR, returning trimmed stdout; signal on failure."
  (with-temp-buffer
    (let* ((default-directory dir)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eq status 0)
        (error "git %S failed in %s: %s" args dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun wade-review-tests--write (dir file content)
  (let ((path (expand-file-name file dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert content))))

(defun wade-review-tests--lines (prefix n)
  "Return N numbered lines \"PREFIX K\\n\"."
  (mapconcat (lambda (i) (format "%s %d\n" prefix i)) (number-sequence 1 n) ""))

(defun wade-review-tests--run-cli (dir program &rest args)
  "Run PROGRAM with ARGS in DIR.  Return (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (let* ((default-directory dir)
           (status (apply #'process-file program nil t nil args)))
      (cons status (buffer-string)))))

(defconst wade-review-tests--mod-base
  (concat (wade-review-tests--lines "line" 30)))

(defconst wade-review-tests--mod-tip
  ;; Two distant changes so git produces two hunks:
  ;; - line 3 replaced by two lines,
  ;; - line 25 deleted and a line added after line 27.
  (concat "line 1\nline 2\nchanged 3a\nchanged 3b\n"
          (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 4 24) "")
          "line 26\nline 27\nadded after 27\n"
          (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 28 30) "")))

(defun wade-review-tests--make-repo (dir &optional main-branch)
  "Create a repo in DIR with branch `feature' off MAIN-BRANCH (default main).
Return DIR."
  (let ((main (or main-branch "main")))
    (make-directory dir t)
    (wade-review-tests--git dir "init" "-q" "-b" main)
    (wade-review-tests--write dir "mod.py" wade-review-tests--mod-base)
    (wade-review-tests--write dir "del.txt" "to be deleted\n")
    (wade-review-tests--write dir "old.txt" (wade-review-tests--lines "renamed" 20))
    (wade-review-tests--write dir "bin.dat" "\0\1\2binary\0")
    (wade-review-tests--git dir "add" "-A")
    (wade-review-tests--git dir "commit" "-q" "-m" "base")
    (wade-review-tests--git dir "checkout" "-q" "-b" "feature")
    (wade-review-tests--write dir "mod.py" wade-review-tests--mod-tip)
    (delete-file (expand-file-name "del.txt" dir))
    (wade-review-tests--git dir "mv" "old.txt" "new.txt")
    (wade-review-tests--write
     dir "new.txt"
     (concat (wade-review-tests--lines "renamed" 20) "one more\n"))
    (wade-review-tests--write dir "sub/add.el" "(message \"hi\")\n")
    (wade-review-tests--write dir "bin.dat" "\0\1\2binary changed\0")
    (wade-review-tests--git dir "add" "-A")
    (wade-review-tests--git dir "commit" "-q" "-m" "feature")
    ;; Move the main branch on so the merge base differs from the base tip.
    (wade-review-tests--git dir "checkout" "-q" main)
    (wade-review-tests--write dir "unrelated.txt" "main moved on\n")
    (wade-review-tests--git dir "add" "-A")
    (wade-review-tests--git dir "commit" "-q" "-m" "main moves")
    dir))

(defun wade-review-tests--generate (repo out &rest extra-args)
  "Generate review file OUT for branch feature in REPO; return OUT."
  (let ((result (apply #'wade-review-tests--run-cli repo wade-review-tests--cli
                       "-b" "feature" "-o" out extra-args)))
    (unless (eq (car result) 0)
      (error "CLI failed (%S): %s" (car result) (cdr result)))
    out))

(defun wade-review-tests--keyword (key)
  "Value of #+KEY: in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s: \\(.*\\)$" (regexp-quote key)) nil t)
      (match-string-no-properties 1))))

(defun wade-review-tests--entries ()
  "Return a list of (LEVEL HEADING PROPS) for every heading in the buffer."
  (org-map-entries
   (lambda ()
     (list (org-current-level)
           (org-get-heading t t t t)
           (org-entry-properties nil 'all)))))

(defun wade-review-tests--file-entry (entries file)
  (cl-find-if (lambda (e) (and (= (nth 0 e) 2)
                               (equal (cdr (assoc "WR_FILE" (nth 2 e))) file)))
              entries))

(defun wade-review-tests--goto-heading (regexp)
  (goto-char (point-min))
  (re-search-forward (concat "^\\*+ .*" regexp))
  (org-back-to-heading t))

;;;; Generator

(ert-deftest wade-review-generate-structure ()
  "The CLI, run through a symlink from another directory, generates the
review structure for every kind of file change."
  (wade-review-tests--with-temp-dir tmp
    (let* ((repo (wade-review-tests--make-repo
                  (file-name-as-directory (expand-file-name "repo" tmp))))
           (bindir (expand-file-name "bin/" tmp))
           (link (expand-file-name "wr-link" bindir))
           (out (expand-file-name "out/review.org" tmp)))
      (make-directory bindir t)
      (make-directory (file-name-directory out) t)
      (make-symbolic-link wade-review-tests--cli link)
      (let ((result (wade-review-tests--run-cli
                     tmp link "-C" repo "--branch" "feature" "--output" out)))
        (should (equal (car result) 0)))
      (with-temp-buffer
        (insert-file-contents out)
        (org-mode)
        (should (equal (wade-review-tests--keyword "WR_BRANCH") "feature"))
        (should (equal (wade-review-tests--keyword "WR_BASE") "main"))
        (should (equal (wade-review-tests--keyword "WR_MERGE_BASE")
                       (wade-review-tests--git repo "merge-base" "main" "feature")))
        (should (equal (wade-review-tests--keyword "WR_TIP")
                       (wade-review-tests--git repo "rev-parse" "feature")))
        (should (equal (wade-review-tests--keyword "WR_BASE_TIP")
                       (wade-review-tests--git repo "rev-parse" "main")))
        (let* ((entries (wade-review-tests--entries))
               (status (lambda (file)
                         (cdr (assoc "WR_STATUS"
                                     (nth 2 (wade-review-tests--file-entry entries file)))))))
          (should (equal (funcall status "mod.py") "modified"))
          (should (equal (funcall status "sub/add.el") "added"))
          (should (equal (funcall status "del.txt") "deleted"))
          (should (equal (funcall status "new.txt") "renamed"))
          (should (equal (funcall status "bin.dat") "modified"))
          (should-not (wade-review-tests--file-entry entries "unrelated.txt"))
          (should (equal (cdr (assoc "WR_OLD_FILE"
                                     (nth 2 (wade-review-tests--file-entry entries "new.txt"))))
                         "old.txt"))
          ;; Every file and hunk heading is a TODO.
          (should (cl-every (lambda (e) (equal (cdr (assoc "TODO" (nth 2 e))) "TODO"))
                            (cl-remove-if (lambda (e) (= (nth 0 e) 1)) entries)))
          ;; mod.py has exactly two hunk children, which carry their own
          ;; file and start lines.
          (let ((hunks (cl-remove-if-not
                        (lambda (e) (and (= (nth 0 e) 3)
                                         (equal (cdr (assoc "WR_FILE" (nth 2 e))) "mod.py")))
                        entries)))
            (should (= (length hunks) 2))
            (should (equal (mapcar (lambda (e) (cdr (assoc "WR_NEW_START" (nth 2 e)))) hunks)
                           '("1" "23")))))
        ;; Hunk content is in diff src blocks with file headers.
        (goto-char (point-min))
        (should (search-forward "#+begin_src diff\n--- a/mod.py\n+++ b/mod.py\n@@ -1,6 +1,7 @@" nil t))
        (should (search-forward "-line 3\n+changed 3a\n+changed 3b\n" nil t))
        ;; Binary files get a heading but no block.
        (wade-review-tests--goto-heading "bin\\.dat")
        (should-not (re-search-forward "^#\\+begin_src" (save-excursion (org-end-of-subtree t t)) t))))))

(ert-deftest wade-review-generate-file-activates-mode ()
  "Opening a generated review file turns on `wade-review-mode'."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate repo out)
      (let ((buf (find-file-noselect out)))
        (with-current-buffer buf
          (should (derived-mode-p 'org-mode))
          (should wade-review-mode))))))

(defun wade-review-tests--clone-with-default-branch-dev (tmp)
  "Clone a repo whose default branch is `dev'.  Return the clone directory.
The clone has a local `dev' with an unpushed commit, and a local `main'
that is not the default branch."
  (let ((upstream (wade-review-tests--make-repo
                   (file-name-as-directory (expand-file-name "upstream" tmp)) "dev"))
        (clone (file-name-as-directory (expand-file-name "clone" tmp))))
    (wade-review-tests--git tmp "clone" "-q" upstream clone)
    (wade-review-tests--write clone "local.txt" "unpushed\n")
    (wade-review-tests--git clone "add" "-A")
    (wade-review-tests--git clone "commit" "-q" "-m" "unpushed on dev")
    (wade-review-tests--git clone "branch" "main" "origin/feature~1")
    (wade-review-tests--git clone "branch" "feature" "origin/feature")
    clone))

(ert-deftest wade-review-generate-default-base-local-default-branch ()
  "Without --base, the base is the local branch with the remote default
branch's name (whatever it is), so unpushed work on it isn't reviewed."
  (wade-review-tests--with-temp-dir tmp
    (let ((clone (wade-review-tests--clone-with-default-branch-dev tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate clone out)
      (with-temp-buffer
        (insert-file-contents out)
        (should (equal (wade-review-tests--keyword "WR_BASE") "dev"))
        (should (equal (wade-review-tests--keyword "WR_BASE_TIP")
                       (wade-review-tests--git clone "rev-parse" "dev")))))))

(ert-deftest wade-review-generate-default-base-remote-default-branch ()
  "Without --base or a local default branch, the base is the remote one."
  (wade-review-tests--with-temp-dir tmp
    (let ((clone (wade-review-tests--clone-with-default-branch-dev tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--git clone "checkout" "-q" "feature")
      (wade-review-tests--git clone "branch" "-D" "dev")
      (wade-review-tests--generate clone out)
      (with-temp-buffer
        (insert-file-contents out)
        (should (equal (wade-review-tests--keyword "WR_BASE") "origin/dev"))))))

(ert-deftest wade-review-generate-default-base-local-master ()
  "Without --base or a remote, the base falls back to a local main/master."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp "master"))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate repo out)
      (with-temp-buffer
        (insert-file-contents out)
        (should (equal (wade-review-tests--keyword "WR_BASE") "master"))))))

(ert-deftest wade-review-generate-default-branch-is-current ()
  "Without --branch, the current branch is reviewed."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--git repo "checkout" "-q" "feature")
      (let ((result (wade-review-tests--run-cli repo wade-review-tests--cli "-o" out)))
        (should (equal (car result) 0)))
      (with-temp-buffer
        (insert-file-contents out)
        (should (equal (wade-review-tests--keyword "WR_BRANCH") "feature"))
        (should (equal (wade-review-tests--keyword "WR_TIP")
                       (wade-review-tests--git repo "rev-parse" "feature")))))))

(ert-deftest wade-review-generate-refuses-empty-review ()
  "Reviewing a branch that is its own merge base (eg. running on the base
branch itself) is an error, not an empty review file."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      ;; make-repo leaves main checked out, and main is the default base.
      (let ((result (wade-review-tests--run-cli repo wade-review-tests--cli "-o" out)))
        (should-not (equal (car result) 0))
        (should (string-match-p "merge base" (cdr result))))
      (should-not (file-exists-p out)))))

(ert-deftest wade-review-generate-from-subdirectory ()
  "Running from a subdirectory of the repository reviews the same hunks."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (make-directory (expand-file-name "subdir/" repo))
      (wade-review-tests--run-cli (expand-file-name "subdir/" repo)
                                  wade-review-tests--cli "-b" "feature" "-o" out)
      (with-temp-buffer
        (insert-file-contents out)
        (goto-char (point-min))
        (should (search-forward "+changed 3a" nil t))
        (goto-char (point-min))
        (should (search-forward "-to be deleted" nil t))))))

(ert-deftest wade-review-generate-refuses-overwrite ()
  "An existing review file (which may hold notes) is only replaced with --force."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--write tmp "review.org" "my notes\n")
      (let ((result (wade-review-tests--run-cli
                     repo wade-review-tests--cli "-b" "feature" "-o" out)))
        (should-not (equal (car result) 0)))
      (should (equal (with-temp-buffer (insert-file-contents out) (buffer-string))
                     "my notes\n"))
      (wade-review-tests--generate repo out "--force")
      (should (with-temp-buffer
                (insert-file-contents out)
                (wade-review-tests--keyword "WR_TIP"))))))

;;;; Review buffer

(ert-deftest wade-review-blocks-have-syntax-highlighting ()
  "Diff blocks in a review file show both the diff and the source language."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate repo out)
      (with-current-buffer (find-file-noselect out)
        (font-lock-ensure)
        (goto-char (point-min))
        ;; The string in the added sub/add.el line `(message "hi")'.
        (search-forward "+(message \"h")
        (let ((faces (ensure-list (get-text-property (point) 'face))))
          (should (memq 'font-lock-string-face faces))
          (should (memq 'diff-added faces)))))))

(ert-deftest wade-review-target-after-reorder ()
  "Jump targets come from the heading and block at point, so they survive
reordering subtrees and adding comment sub-headings."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate repo out)
      (with-current-buffer (find-file-noselect out)
        ;; Move the second mod.py hunk above the first, and the mod.py file
        ;; subtree to the end.
        (wade-review-tests--goto-heading "@@ -22,")
        (org-move-subtree-up)
        (wade-review-tests--goto-heading "mod\\.py")
        (dotimes (_ 5) (ignore-errors (org-move-subtree-down)))
        ;; Point on the added line, which follows a removed line in the block.
        (goto-char (point-min))
        (search-forward "\n+added after 27")
        (should (equal (wade-review-target-at-point)
                       '(:file "mod.py" :old-file "mod.py" :status "modified" :line 28)))
        ;; Point on a removed line targets the line where it would have been.
        (goto-char (point-min))
        (search-forward "\n-line 25")
        (should (equal (plist-get (wade-review-target-at-point) :line) 26))
        ;; A comment sub-heading under the first hunk targets the hunk start.
        (wade-review-tests--goto-heading "@@ -1,")
        (org-insert-subheading nil)
        (insert "my comment")
        (should (equal (plist-get (wade-review-target-at-point) :line) 1))
        (should (equal (plist-get (wade-review-target-at-point) :file) "mod.py"))))))

(ert-deftest wade-review-jump-worktree-lifecycle ()
  "Jumping creates (then reuses) a detached review worktree at the tip; closing
removes it."
  (wade-review-tests--with-temp-dir tmp
    (let* ((repo (wade-review-tests--make-repo tmp))
           (out (expand-file-name "review.org" tmp))
           (tip (wade-review-tests--git repo "rev-parse" "feature"))
           (wt-parent (file-truename
                       (expand-file-name ".git/agent-files/wt/user/" repo)))
           worktree)
      (wade-review-tests--generate repo out)
      (with-current-buffer (find-file-noselect out)
        (goto-char (point-min))
        (search-forward "\n+added after 27")
        (wade-review-jump)
        ;; Now in the worktree file at the right line.
        (should (equal (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       "added after 27"))
        (setq worktree (file-name-directory (file-truename buffer-file-name)))
        (should (string-prefix-p wt-parent worktree))
        (should (string-match-p "/review-[a-z0-9]+/\\'" worktree))
        (should (equal (wade-review-tests--git worktree "rev-parse" "HEAD") tip))
        ;; Detached: no branch is checked out there.
        (should (equal (wade-review-tests--git worktree "rev-parse" "--abbrev-ref" "HEAD")
                       "HEAD")))
      (with-current-buffer (find-file-noselect out)
        (should (equal (file-name-as-directory
                        (file-truename (wade-review-tests--keyword "WR_WORKTREE")))
                       worktree))
        ;; The worktree path is saved in the file.
        (should-not (buffer-modified-p))
        (goto-char (point-min))
        (search-forward "\n+changed 3a")
        (wade-review-jump)
        (should (string-prefix-p worktree (file-truename buffer-file-name))))
      (with-current-buffer (find-file-noselect out)
        (wade-review-close-worktree)
        (should-not (file-exists-p worktree))
        (should-not (wade-review-tests--keyword "WR_WORKTREE"))
        (should-not (string-match-p "review-"
                                    (wade-review-tests--git repo "worktree" "list")))))))

;;;; File highlighting

(defun wade-review-tests--faces-on-line (n)
  "Faces of highlight overlays covering line N of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (delq nil (mapcar (lambda (ov) (overlay-get ov 'face))
                      (overlays-at (point))))))

(defun wade-review-tests--before-strings ()
  "Alist of (LINE . BEFORE-STRING) for overlays with a before-string."
  (let (res)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when-let* ((s (overlay-get ov 'before-string)))
        (push (cons (line-number-at-pos (overlay-start ov)) (substring-no-properties s))
              res)))
    res))

(ert-deftest wade-review-highlight-regions ()
  "Files visited from a review show added, changed, and (on request) deleted
code relative to the merge base."
  (wade-review-tests--with-temp-dir tmp
    (let ((repo (wade-review-tests--make-repo tmp))
          (out (expand-file-name "review.org" tmp)))
      (wade-review-tests--generate repo out)
      (with-current-buffer (find-file-noselect out)
        (goto-char (point-min))
        (search-forward "\n+added after 27")
        (wade-review-jump)
        (should wade-review-highlight-mode)
        ;; Lines 3-4 replace old line 3: changed.
        (should (memq 'wade-review-changed (wade-review-tests--faces-on-line 3)))
        (should (memq 'wade-review-changed (wade-review-tests--faces-on-line 4)))
        ;; Line 28 is purely added.
        (should (memq 'wade-review-added (wade-review-tests--faces-on-line 28)))
        ;; Unchanged lines have no highlight.
        (should-not (wade-review-tests--faces-on-line 2))
        (should-not (wade-review-tests--faces-on-line 10))
        ;; Deleted code is hidden by default, and shown by the toggle where it
        ;; was removed: old line 25 sat between new lines 25 and 26.
        (should-not (wade-review-tests--before-strings))
        (wade-review-toggle-deleted)
        (let ((shown (wade-review-tests--before-strings)))
          (should (member '(26 . "line 25\n") shown))
          (should (member '(3 . "line 3\n") shown)))
        (wade-review-toggle-deleted)
        (should-not (wade-review-tests--before-strings))))))

(provide 'wade-review-tests)

;;; wade-review-tests.el ends here
