;;; fzf-conf.el --- fzf alternate actions and configuration -*- lexical-binding: t; -*-

;;; fzf alternate actions
;;
;; When selecting a file in fzf, alternate key bindings trigger different
;; actions on the selected entry:
;;   C-j  -- open a second fzf with a menu of things to do with the file
;;   C-x  -- directly copy the super-repo-relative path
;;   Enter -- perform the original/default action (open file, copy path, etc.)
;;
;; The mechanism uses fzf's --expect option, which causes fzf to print the
;; pressed key on a separate line before the selected entry.  A custom exit
;; handler parses this extra line and dispatches accordingly.
;;
;; With both --print-query (from fzf/args) and --expect, fzf outputs three
;; lines: query, key, selected-entry.  The key line is empty when Enter is
;; pressed (since Enter is not listed in --expect).
;;
;; Because the exit handler runs as :after advice on `term-handle-exit',
;; the buffer already contains an extra "Process <name> <msg>" line
;; appended by `term-handle-exit' itself.  The parsing logic must
;; account for this.

(require 'fzf)
(require 'seq)

(defun wgh/fzf--after-term-handle-exit-with-expect
    (directory action alt-action-alist target-validator extractor-list)
  "Create a term-handle-exit handler that supports fzf --expect keys.
DIRECTORY is prepended to the fzf result.
ACTION is the default action (for Enter).
ALT-ACTION-ALIST is an alist of (KEY-STRING . FUNCTION) for alternate keys.
  Each function receives (FILE-PATH DIRECTORY) as arguments.
TARGET-VALIDATOR and EXTRACTOR-LIST are as in `fzf--after-term-handle-exit'."
  (lambda (process-name msg)
    (let* ((exit-code (fzf--exit-code-from-event msg))
           (text (fzf--text-only-for (point-min) (point-max))))
      ;; Kill the fzf buffer and restore window configuration.
      (kill-buffer fzf/buffer-name)
      (jump-to-register fzf--window-register)
      (when (string= "0" exit-code)
        ;; With --print-query and --expect, fzf outputs exactly 3 lines:
        ;;   query \n key \n entry \n
        ;; where key is empty when Enter is pressed.
        ;;
        ;; However, `term-handle-exit' (which runs BEFORE this :after
        ;; advice) inserts a "Process <name> finished\n" line at the end
        ;; of the buffer.  We must strip that before parsing.
        (let* ((all-lines (split-string text "\n"))
               ;; Strip the fzf cursor prefix "> " from lines.
               (all-lines (mapcar (lambda (s)
                                    (replace-regexp-in-string
                                     "^[[:space:]]*>[[:space:]]+" "" s))
                                  all-lines))
               ;; Remove the "Process <name> <msg>" line that
               ;; `term-handle-exit' inserts, plus trailing empties.
               (all-lines (seq-filter
                           (lambda (s)
                             (not (string-match-p "^Process " s)))
                           all-lines))
               ;; Skip trailing empty lines by reversing and dropping empties.
               (trimmed-rev
                (let ((r (reverse all-lines)))
                  (while (and r (string-empty-p (string-trim (car r))))
                    (setq r (cdr r)))
                  r))
               ;; First non-empty from end is the selected entry.
               (selected-entry (string-trim (or (car trimmed-rev) "")))
               ;; The line before that is the key (may be empty for Enter).
               (key-line (string-trim (or (cadr trimmed-rev) "")))
               (target (string-trim
                        (concat
                         (when directory
                           (file-name-as-directory directory))
                         selected-entry)))
               (target (funcall target-validator target text msg process-name))
               (alt-action (when (not (string-empty-p key-line))
                             (cdr (assoc key-line alt-action-alist)))))
          (if alt-action
              (funcall alt-action target directory)
            ;; Default action (Enter was pressed -- key line was empty)
            (let ((fzf--extractor-list extractor-list))
              (funcall action target))))))
    ;; Remove this advice so it doesn't interfere with other term usage.
    (advice-remove 'term-handle-exit
                   (wgh/fzf--after-term-handle-exit-with-expect
                    directory action alt-action-alist
                    target-validator extractor-list))))

(defun wgh/fzf--start-with-expect (directory action alt-action-alist
                                              &optional custom-args)
  "Like `fzf--start' but adds --expect support for alternate actions.
ALT-ACTION-ALIST is an alist of (KEY-STRING . FUNCTION).
Each function receives (FILE-PATH DIRECTORY)."
  (require 'term)
  (fzf--close)
  (unless (executable-find fzf/executable)
    (user-error "Can't find fzf/executable '%s'" fzf/executable))
  (window-configuration-to-register fzf--window-register)
  (let* ((expect-keys (mapconcat #'car alt-action-alist ","))
         (expect-arg (format " --expect=%s" expect-keys))
         (term-exec-hook nil)
         (buf (get-buffer-create fzf/buffer-name))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (args (concat (or custom-args fzf/args) expect-arg))
         (sh-cmd (concat fzf/executable " " args)))
    (advice-add 'term-handle-exit
                :after (wgh/fzf--after-term-handle-exit-with-expect
                        directory action alt-action-alist
                        fzf--target-validator fzf--extractor-list))
    (with-current-buffer buf
      (setq default-directory (or directory "")))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (make-term (file-name-nondirectory fzf/executable)
               "sh" nil "-c" sh-cmd)
    (switch-to-buffer buf)
    (and (fboundp 'turn-off-evil-mode) (turn-off-evil-mode))
    (when (bound-and-true-p linum-mode) (linum-mode 0))
    (when (bound-and-true-p visual-line-mode) (visual-line-mode 0))
    (when (bound-and-true-p display-line-numbers-mode) (display-line-numbers-mode 0))
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t)
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (setq-local truncate-lines t)
    (face-remap-add-relative 'mode-line '(:box nil))
    (and (fboundp 'term-char-mode) (term-char-mode))
    (setq fzf--hook (wgh/fzf--after-term-handle-exit-with-expect
                     directory action alt-action-alist
                     fzf--target-validator fzf--extractor-list)
          mode-line-format (format "   FZF  %s" (or directory "")))))

(defun wgh/fzf-with-command-and-alt-actions (command action alt-action-alist
                                                      &optional directory)
  "Run fzf on the output of COMMAND with alternate action support.
ACTION is the default action for Enter.
ALT-ACTION-ALIST is an alist of (KEY-STRING . FUNCTION) for alternate keys.
DIRECTORY is the working directory for fzf."
  (let ((fzf--target-validator (or (bound-and-true-p fzf-target-validator)
                                   #'fzf--pass-through)))
    (if command
        (let ((process-environment
               (cons (concat "FZF_DEFAULT_COMMAND=" command)
                     process-environment)))
          (wgh/fzf--start-with-expect directory action alt-action-alist))
      (wgh/fzf--start-with-expect directory action alt-action-alist))))


;;; File path helpers for fzf alt actions

(defun wgh/file-super-repo-relative (file-path)
  "Return FILE-PATH relative to the outermost git superproject root.
If not in a git repo, return the absolute path."
  (let ((root (wgh/git-root-superproject)))
    (if root
        (file-relative-name file-path root)
      (expand-file-name file-path))))

(defun wgh/file-repo-relative (file-path)
  "Return FILE-PATH relative to the nearest git root."
  (let ((git-root (string-trim
                   (let ((default-directory (file-name-directory
                                            (expand-file-name file-path))))
                     (shell-command-to-string
                      "git rev-parse --show-toplevel 2>/dev/null")))))
    (if (or (string-empty-p git-root)
            (string-prefix-p "fatal:" git-root))
        (expand-file-name file-path)
      (file-relative-name (expand-file-name file-path) git-root))))


;;; The fzf action menu (C-j action)

(defun wgh/fzf-file-action-menu (file-path &optional _directory)
  "Present a second fzf menu of actions for FILE-PATH.
The menu offers: open file, copy absolute path, copy repo-relative path,
copy super-repo-relative path, copy basename, and copy directory."
  (let* ((abs-path (expand-file-name file-path))
         (basename (file-name-nondirectory abs-path))
         (dirname (file-name-directory abs-path))
         (repo-rel (wgh/file-repo-relative abs-path))
         (super-rel (wgh/file-super-repo-relative abs-path))
         ;; Build an alist keyed by action label, with value to act on.
         (action-alist
          `(("open"                      . ,abs-path)
            ("copy-absolute-path"        . ,abs-path)
            ("copy-single-repo-relative" . ,repo-rel)
            ("copy-super-repo-relative"  . ,super-rel)
            ("copy-basename"             . ,basename)
            ("copy-dirname"              . ,dirname)
            ))
         ;; Build the display entries: "label: value"
         (entries
          (mapcar (lambda (pair)
                    (format "%-16s %s" (car pair) (cdr pair)))
                  action-alist))
         (action-func
          (lambda (selection)
            ;; Extract the action label (first whitespace-delimited word).
            (let* ((action-name (car (split-string (string-trim selection)
                                                   "[[:space:]]+" t)))
                   (value (cdr (assoc action-name action-alist))))
              (cond
               ((string= action-name "open")
                (when (file-exists-p value)
                  (find-file value)))
               ((string-prefix-p "copy-" action-name)
                (wgh/cpo-copy-string value cpo-copy-default-register))
               (t (message "Unknown action: %s" action-name)))))))
    (fzf-with-entries entries action-func)))


;;; C-x action: copy super-repo-relative path

(defun wgh/fzf-copy-super-repo-relative (file-path &optional _directory)
  "Copy the super-repo-relative path of FILE-PATH to the default register."
  (let* ((abs-path (expand-file-name file-path))
         (super-rel (wgh/file-super-repo-relative abs-path)))
    (wgh/cpo-copy-string super-rel cpo-copy-default-register)))


;;; The standard alt-action-alist for file-picking fzf commands

(defvar wgh/fzf-file-alt-actions
  '(("ctrl-j" . wgh/fzf-file-action-menu)
    ("ctrl-x" . wgh/fzf-copy-super-repo-relative))
  "Alist of alternate fzf key bindings for file-picking commands.
Each entry is (FZF-KEY-NAME . FUNCTION).
Functions receive (FILE-PATH DIRECTORY).")


;;; Git superproject helper

(defun wgh/git-root-superproject ()
  "Find the outermost git superproject working tree.
Returns the root directory path, or nil if not inside a git repo.
Note: `git rev-parse --show-superproject-working-tree' returns empty
with exit code 0 when already at the root (not inside a submodule),
so we must check its output rather than relying on exit codes."
  (let* ((super (string-trim
                 (shell-command-to-string
                  "git rev-parse --show-superproject-working-tree 2>/dev/null")))
         (root (if (string-empty-p super)
                   (string-trim
                    (shell-command-to-string
                     "git rev-parse --show-toplevel 2>/dev/null"))
                 super)))
    (if (string-empty-p root)
        nil
      ;; Walk up to the outermost superproject.
      (let ((dir root))
        (while (let ((parent (string-trim
                              (shell-command-to-string
                               (format "git -C %s rev-parse --show-superproject-working-tree 2>/dev/null"
                                       (shell-quote-argument dir))))))
                 (unless (string-empty-p parent)
                   (setq dir parent)))
          )
        dir))))


;;; fzf file commands with alt-action support

(defun wgh/fzf-git-files ()
  "Open a tracked git file using fzf, with alternate actions (C-j menu, C-x super-rel copy)."
  (interactive)
  (require 'fzf)
  (let* ((fzf--target-validator #'fzf--validate-filename)
         (path (locate-dominating-file (file-truename default-directory) ".git")))
    (if path
        (wgh/fzf-with-command-and-alt-actions
         "git ls-files"
         #'fzf--action-find-file
         wgh/fzf-file-alt-actions
         path)
      (user-error "Not inside a Git repository"))))

(defun wgh/fzf-all-files ()
  "Open a file from the current git repo using fzf, including untracked files.
Lists both tracked and untracked (but not ignored) files.
Supports alternate actions (C-j menu, C-x super-rel copy)."
  (interactive)
  (require 'fzf)
  (let* ((fzf--target-validator #'fzf--validate-filename)
         (path (locate-dominating-file (file-truename default-directory) ".git")))
    (if path
        (wgh/fzf-with-command-and-alt-actions
         "{ git ls-files; git ls-files --others --exclude-standard; } | sort -u"
         #'fzf--action-find-file
         wgh/fzf-file-alt-actions
         path)
      (user-error "Not inside a Git repository"))))

(defun wgh/fzf-untracked-files ()
  "Open an untracked file from the current git repo using fzf.
Lists only untracked (but not ignored) files.
Supports alternate actions (C-j menu, C-x super-rel copy)."
  (interactive)
  (require 'fzf)
  (let* ((fzf--target-validator #'fzf--validate-filename)
         (path (locate-dominating-file (file-truename default-directory) ".git")))
    (if path
        (wgh/fzf-with-command-and-alt-actions
         "git ls-files --others --exclude-standard"
         #'fzf--action-find-file
         wgh/fzf-file-alt-actions
         path)
      (user-error "Not inside a Git repository"))))

(defun wgh/fzf-submodule-files ()
  "Open a file using fzf, recursing into submodules from the root repo.
Walks up to find the outermost git repo (the root superproject) and
lists all tracked files including those in submodules.
Supports alternate actions (C-j menu, C-x super-rel copy)."
  (interactive)
  (require 'fzf)
  (let* ((fzf--target-validator #'fzf--validate-filename)
         (root (wgh/git-root-superproject)))
    (if root
        (wgh/fzf-with-command-and-alt-actions
         "git ls-files --recurse-submodules"
         #'fzf--action-find-file
         wgh/fzf-file-alt-actions
         root)
      (user-error "Not inside a Git repository"))))

(defun wgh/agent-working-dir-fzf-open ()
  "Open a file in the agent-working-directories base path using fzf.
Supports alternate actions (C-j menu, C-x super-rel copy)."
  (interactive)
  (require 'fzf)
  (let ((base (wgh/agent-working-dir-base))
        (fzf--target-validator #'fzf--pass-through))
    (unless (file-directory-p base)
      (user-error "No agent working directories at: %s" base))
    (wgh/fzf-with-command-and-alt-actions
     "find . -type f | sort -r"
     (lambda (x) (find-file (expand-file-name x base)))
     wgh/fzf-file-alt-actions
     base)))


;;; Dotfileswgh mixin path helpers

(defun wgh/dotfileswgh-mixin-paths ()
  "Return the list of dotfileswgh mixin root paths that exist as directories."
  (seq-filter #'file-directory-p dotfileswgh-list))

(defun wgh/dotfileswgh-labeled-mixin-paths ()
  "Return labeled mixin paths that exist as directories.
Each element is (LABEL PATH) from `dotfileswgh-labeled-list', filtered to
only those where PATH is an existing directory."
  (seq-filter (lambda (entry) (file-directory-p (cadr entry)))
              dotfileswgh-labeled-list))

(cl-defun wgh/fzf-mixin-paths (relative-path &key extra-paths extra-mixin-paths action)
  "Search for files with fzf across dotfileswgh mixin directories under RELATIVE-PATH.
Appends RELATIVE-PATH to each mixin root in `dotfileswgh-list', filters to
directories that exist, and runs fzf across all of them.

EXTRA-PATHS is an optional list of additional absolute directory paths to
include in the search (appended after the mixin-derived paths).  Each element
may be a string path, or a function of no arguments that returns a path or nil.
Function entries that return nil are filtered out.

EXTRA-MIXIN-PATHS is an optional list of additional root paths that have
RELATIVE-PATH appended to them, just like the dotfileswgh mixin paths.  Each
element may be a string (used as-is with a label derived from the directory
name), or a list (LABEL PATH) to specify the label explicitly.  Elements may
also be functions of no arguments that return a path, a (LABEL PATH) list,
or nil.

ACTION is an optional function called with the selected file path.  If nil,
defaults to `find-file'.

Each fzf result is displayed as LABEL:RELPATH where LABEL is the short name
of the mixin root (e.g. \"dotfileswgh\", \"pri\", \"dotlocal\") and RELPATH is
the path relative to that mixin's subdirectory.

Supports alternate actions (C-j menu, C-x super-rel copy)."
  (require 'fzf)
  (let* ((action (or action #'find-file))
         ;; Build list of (label . directory) pairs from mixin roots
         (mixin-dirs
          (let ((result nil))
            (dolist (entry (wgh/dotfileswgh-labeled-mixin-paths))
              (let* ((label (car entry))
                     (root (cadr entry))
                     (dir (expand-file-name
                           (concat (file-name-as-directory root)
                                   relative-path))))
                (when (file-directory-p dir)
                  (push (cons label (file-name-as-directory dir))
                        result))))
            (nreverse result)))
         ;; Add extra-mixin-paths (have relative-path appended like mixin dirs)
         (extra-mixin-dirs
          (let ((result nil))
            (dolist (entry (or extra-mixin-paths nil))
              (let* ((resolved (if (functionp entry) (funcall entry) entry))
                     (label (cond
                             ((null resolved) nil)
                             ((listp resolved) (car resolved))
                             ((stringp resolved)
                              (file-name-nondirectory
                               (directory-file-name resolved)))
                             (t nil)))
                     (root (cond
                            ((null resolved) nil)
                            ((listp resolved) (cadr resolved))
                            ((stringp resolved) resolved)
                            (t nil))))
                (when root
                  (let ((dir (expand-file-name
                              (concat (file-name-as-directory root)
                                      relative-path))))
                    (when (file-directory-p dir)
                      (push (cons label (file-name-as-directory dir))
                            result))))))
            (nreverse result)))
         ;; Add extra paths (absolute, no relative-path appended)
         (extra-dirs
          (let ((result nil))
            (dolist (entry (or extra-paths nil))
              (let ((path (if (functionp entry) (funcall entry) entry)))
                (when path
                  (let ((dir (file-name-as-directory (expand-file-name path))))
                    (when (file-directory-p dir)
                      (push (cons (file-name-nondirectory
                                   (directory-file-name dir))
                                  dir)
                            result))))))
            (nreverse result)))
         (all-dirs (append mixin-dirs extra-mixin-dirs extra-dirs)))
    (unless all-dirs
      (user-error "No existing directories found for relative path: %s" relative-path))
    ;; Build a find command that prefixes each result with its label.
    ;; Output format: LABEL:relative/path/to/file
    ;; We pass nil as the directory to fzf-with-command so it does not
    ;; prepend anything to the selection; our fzf-action resolves the
    ;; full path from the LABEL:RELPATH format via label-to-dir.
    (let* ((find-parts
            (mapcar (lambda (pair)
                      (let ((label (car pair))
                            (dir (cdr pair)))
                        (format "find %s -type f -printf '%s:%%P\\n'"
                                (shell-quote-argument dir)
                                label)))
                    all-dirs))
           (command (concat "{ " (mapconcat #'identity find-parts " ; ") " ; } | sort"))
           ;; Build a lookup alist to resolve label back to directory
           (label-to-dir all-dirs)
           (fzf-action
            (lambda (selection)
              (let* ((colon-pos (string-match ":" selection))
                     (label (substring selection 0 colon-pos))
                     (relpath (substring selection (1+ colon-pos)))
                     (dir (cdr (assoc label label-to-dir))))
                (if dir
                    (funcall action (expand-file-name relpath dir))
                  (funcall action selection)))))
           (fzf-alt-action
            (lambda (selection directory)
              (let* ((colon-pos (string-match ":" selection))
                     (label (if colon-pos (substring selection 0 colon-pos) nil))
                     (relpath (if colon-pos (substring selection (1+ colon-pos)) selection))
                     (dir (when label (cdr (assoc label label-to-dir))))
                     (file-path (if dir
                                    (expand-file-name relpath dir)
                                  selection)))
                file-path))))
      ;; Use alt-actions: wrap the alt-action-alist so each alt-action
      ;; receives the resolved file path instead of the LABEL:RELPATH string.
      (let ((resolved-alt-actions
             (mapcar (lambda (pair)
                       (let ((key (car pair))
                             (func (cdr pair)))
                         (cons key (lambda (selection directory)
                                     (funcall func
                                              (funcall fzf-alt-action selection directory)
                                              directory)))))
                     wgh/fzf-file-alt-actions)))
        (wgh/fzf-with-command-and-alt-actions
         command fzf-action resolved-alt-actions nil)))))

(defun wgh/fzf-dotfileswgh-mixin (&optional relative-path extra-paths)
  "Interactively search for files across dotfileswgh mixin directories.
If RELATIVE-PATH is nil, search the mixin roots themselves.
EXTRA-PATHS is an optional list of additional absolute directory paths."
  (interactive)
  (wgh/fzf-mixin-paths (or relative-path "")
                        :extra-paths extra-paths))

(defun wgh/fzf-dotfileswgh-mixin-emacs ()
  "Search for files in emacs/ across dotfileswgh mixin directories."
  (interactive)
  (wgh/fzf-mixin-paths "emacs/"))

(defun wgh/fzf-dotfileswgh-mixin-xdg-config-ro ()
  "Search for files in xdg-config-ro/ across dotfileswgh mixin directories."
  (interactive)
  (wgh/fzf-mixin-paths "xdg-config-ro/"))

(defun wgh/fzf-dotfileswgh-mixin-commands ()
  "Search for files in commands/ across dotfileswgh mixin directories."
  (interactive)
  (wgh/fzf-mixin-paths "commands/"))

(provide 'fzf-conf)
;;; fzf-conf.el ends here
