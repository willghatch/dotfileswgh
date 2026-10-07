;;; wade-review.el --- Review git branches in org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs side of wade-review.  The `wade-review' CLI generates an org
;; file with a TODO heading per changed file and per hunk; see README.md.
;;
;; - `wade-review-mode' is enabled in review files (via their -*- line).
;;   It gives diff blocks language syntax and inline-comment highlighting.
;; - `wade-review-jump' visits the file and line at point, in a detached
;;   review worktree at the reviewed tip, created on first use.
;; - Files in a review worktree get `wade-review-highlight-mode', which
;;   highlights code changed since the comparison start, including refined
;;   changed text, and can show deleted code.
;; - `wade-review-command-map' holds the commands.  It is not bound to
;;   any key; bind it to a prefix of your choice.
;;
;; Commands find everything they need from the heading and block at point and
;; the file's #+WR_ keywords, never from heading order, so review headings
;; can be freely reordered and nested comment headings added.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff-mode)
(require 'xref)
(require 'wade-review-generate)

(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function org-map-entries "org" (func &optional match scope &rest skip))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-before-first-heading-p "org" ())
(defvar org-src-lang-modes)
(defvar wade-review-highlight-mode)
(defvar git-gutter:start-revision)
(defvar git-gutter-mode)
(declare-function git-gutter "git-gutter" ())
(declare-function git-gutter:diff-process-buffer "git-gutter" (curfile))
(declare-function git-gutter-mode "git-gutter" (&optional arg))

(defgroup wade-review nil
  "Review git branches in org-mode."
  :group 'tools)

(defcustom wade-review-comment-prefix "# "
  "Exact prefix that marks inline commentary inside review diffs.
The prefix must be nonempty and starts in the diff indicator column."
  :type 'string
  :group 'wade-review)

(defface wade-review-added
  '((t :inherit diff-added))
  "Face for lines added since the comparison start.")

(defface wade-review-changed
  '((t :inherit diff-changed-unspecified))
  "Face for lines changed since the comparison start.")

(defface wade-review-deleted
  '((t :inherit diff-removed))
  "Face for deleted code shown by `wade-review-toggle-deleted'.")

(defface wade-review-moved-in
  '((default :inherit diff-added)
    (((class color) (background light)) :background "#f3f0db")
    (((class color) (background dark)) :background "#151500"))
  "Background for code moved into its current location.")

(defface wade-review-moved-out
  '((default :inherit diff-removed)
    (((class color) (background light)) :background "#f3f0db")
    (((class color) (background dark)) :background "#151500"))
  "Background for code moved away from its old location.")

(defface wade-review-moved-in-indicator
  '((default :inherit diff-indicator-added)
    (((class color) (background light)) :background "#d5b43c")
    (((class color) (background dark)) :background "#6b5700"))
  "Background for the plus indicator of moved-in code.")

(defface wade-review-moved-out-indicator
  '((default :inherit diff-indicator-removed)
    (((class color) (background light)) :background "#d5b43c")
    (((class color) (background dark)) :background "#6b5700"))
  "Background for the minus indicator of moved-out code.")

(defface wade-review-comment
  '((t :inherit font-lock-comment-face))
  "Face for inline commentary inside review diffs.")

(defface wade-review-comment-indicator
  '((t :inherit font-lock-comment-delimiter-face :weight bold))
  "Face for the prefix of inline commentary inside review diffs.")

(defconst wade-review--marker-file "wade-review"
  "Name of the file, in a review worktree's git dir, that marks it as one.")

;;;; Git helpers

(defun wade-review--git (dir &rest args)
  "Run git with ARGS in DIR and return its stdout; signal on failure."
  (let ((default-directory (file-name-as-directory dir)))
    (condition-case err
        (apply #'wade-review-generate--git args)
      (wade-review-generate-error (user-error "%s" (cadr err))))))

;;;; Review buffer metadata

(defun wade-review--keyword (key)
  "Return the value of the #+WR_KEY: line of the current review buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (when (re-search-forward (format "^#\\+WR_%s: \\(.*\\)$" (regexp-quote key)) nil t)
          (string-trim (match-string-no-properties 1)))))))

(defun wade-review--require-keyword (key)
  (or (wade-review--keyword key)
      (user-error "Not a wade-review file (no #+WR_%s: line)" key)))

(defun wade-review--set-keyword (key value)
  "Set the #+WR_KEY: line to VALUE, or remove it if VALUE is nil.
Save the buffer if it visits a file."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (line (and value (format "#+WR_%s: %s\n" key value))))
        (cond
         ((re-search-forward (format "^#\\+WR_%s: .*\n?" (regexp-quote key)) nil t)
          (replace-match (or line "") t t))
         (line
          (goto-char (point-min))
          (while (re-search-forward "^#\\+WR_[A-Z_]+: .*\n" nil t))
          (insert line))))))
  (when buffer-file-name
    (save-buffer)))

;;;; Review mode

(defun wade-review--comment-line-p (line)
  "Return non-nil when LINE begins with the configured comment prefix."
  (and (not (string-empty-p wade-review-comment-prefix))
       (string-prefix-p wade-review-comment-prefix line)))

(defun wade-review--mask-comments-in-hunk (hunk)
  "Replace commentary in HUNK with whitespace of the same width."
  (mapconcat (lambda (line)
               (if (wade-review--comment-line-p line)
                   (make-string (length line) ?\s)
                 line))
             (split-string hunk "\n") "\n"))

(defun wade-review--filter-diff-hunk-text (args)
  "Hide inline commentary from `diff-hunk-text' in review diff buffers."
  (if (derived-mode-p 'wade-review-diff-mode)
      (cons (wade-review--mask-comments-in-hunk (car args)) (cdr args))
    args))

(advice-add 'diff-hunk-text :filter-args #'wade-review--filter-diff-hunk-text)

(defun wade-review--diff-end-of-hunk (original &rest args)
  "Call ORIGINAL with ARGS, ignoring diff counts in review diff buffers.
Each generated source block contains one hunk, and its user commentary must
not be mistaken for the start of non-diff text."
  (if (and (derived-mode-p 'wade-review-diff-mode)
           (looking-at diff-hunk-header-re))
      (progn
        (forward-line 1)
        (if (re-search-forward diff-hunk-header-re nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max))))
    (apply original args)))

(advice-add 'diff-end-of-hunk :around #'wade-review--diff-end-of-hunk)

(defun wade-review--comment-match (limit)
  "Find inline commentary before LIMIT for font locking."
  (and (not (string-empty-p wade-review-comment-prefix))
       (re-search-forward
        (concat "^\\(" (regexp-quote wade-review-comment-prefix) "\\)\\(.*\\)$")
        limit t)))

(defun wade-review--comment-free-hunk (hunk)
  "Return (TEXT . LINE-MAP) for HUNK with commentary lines removed.
LINE-MAP is a vector mapping each zero-based line in TEXT to its original
zero-based line in HUNK."
  (let (lines line-map)
    (cl-loop for line in (split-string hunk "\n")
             for index from 0
             unless (wade-review--comment-line-p line)
             do (push line lines) (push index line-map))
    (cons (string-join (nreverse lines) "\n")
          (vconcat (nreverse line-map)))))

(defun wade-review--refined-hunk-ranges (hunk)
  "Return fine-change ranges from HUNK with commentary excluded.
Each result is (LINE START END FACE), where LINE is zero-based in the original
HUNK and START and END are columns that include its diff indicator column."
  (pcase-let* ((`(,text . ,line-map) (wade-review--comment-free-hunk hunk))
               (ranges nil))
    (with-temp-buffer
      (let ((default-directory temporary-file-directory)
            (diff-refine nil))
        (insert text)
        (diff-mode)
        (goto-char (point-min))
        (diff-refine-hunk)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (memq (overlay-get ov 'face)
                      '(diff-refine-added diff-refine-removed diff-refine-changed))
            (let ((pos (overlay-start ov))
                  (end (overlay-end ov))
                  (face (overlay-get ov 'face)))
              (while (< pos end)
                (goto-char pos)
                (let* ((line (1- (line-number-at-pos pos)))
                       (line-beg (line-beginning-position))
                       (line-end (line-end-position))
                       (piece-end (min end line-end)))
                  (when (< pos piece-end)
                    (push (list (aref line-map line)
                                (- pos line-beg) (- piece-end line-beg) face)
                          ranges))
                  (setq pos (min end (1+ line-end))))))))))
    (nreverse ranges)))

(defvar-local wade-review--diff-refine nil)

(defun wade-review--diff-font-lock-refined (limit)
  "Create commentary-aware fine-change overlays up to LIMIT."
  (when wade-review--diff-refine
    (when (get-char-property (point) 'diff--font-lock-refined)
      (goto-char (next-single-char-property-change
                  (point) 'diff--font-lock-refined nil limit)))
    (diff--iterate-hunks
     limit
     (lambda (beg end)
       (unless (get-char-property beg 'diff--font-lock-refined)
         (dolist (range (wade-review--refined-hunk-ranges
                         (buffer-substring-no-properties beg end)))
           (save-excursion
             (goto-char beg)
             (forward-line (nth 0 range))
             (let ((ov (make-overlay (+ (point) (nth 1 range))
                                     (+ (point) (nth 2 range)))))
               (overlay-put ov 'diff-mode 'fine)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'modification-hooks
                            '(diff--overlay-auto-delete))
               (overlay-put ov 'face (nth 3 range)))))
         (let ((ov (make-overlay beg end)))
           (overlay-put ov 'diff--font-lock-refined t)
           (overlay-put ov 'diff-mode 'fine)
           (overlay-put ov 'evaporate t)
           (overlay-put ov 'modification-hooks
                        '(diff--overlay-auto-delete)))))))
  (goto-char limit)
  nil)

(defun wade-review--moved-line-p (line ranges)
  "Return non-nil if LINE is in the inclusive RANGES string."
  (when ranges
    (cl-some (lambda (range)
               (when (string-match "\\`\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?\\'" range)
                 (<= (string-to-number (match-string 1 range)) line
                     (string-to-number (or (match-string 2 range)
                                           (match-string 1 range))))))
             (split-string ranges "," t))))

(defun wade-review--mark-moved-line (face indicator-face description)
  "Style the current diff line with FACE and its prefix with INDICATOR-FACE.
DESCRIPTION is shown when hovering over either overlay."
  (let ((line (make-overlay (point) (min (1+ (line-end-position)) (point-max))))
        (indicator (make-overlay (point) (1+ (point)))))
    (dolist (ov (list line indicator))
      (overlay-put ov 'wade-review-move t)
      (overlay-put ov 'help-echo description))
    (overlay-put line 'face face)
    (overlay-put indicator 'face indicator-face)
    (overlay-put indicator 'priority 10)))

(defun wade-review--refresh-move-overlays (&rest _)
  "Style moved lines in this review using their hunk properties."
  (when wade-review-mode
    (remove-overlays (point-min) (point-max) 'wade-review-move t)
    (save-excursion
      (goto-char (point-min))
      (org-map-entries
       (lambda ()
         (save-excursion
          (let ((moved-old (org-entry-get nil "WR_MOVED_OLD"))
               (moved-new (org-entry-get nil "WR_MOVED_NEW")))
           (when (or moved-old moved-new)
             (let ((end (save-excursion (org-end-of-subtree t t))))
               (when (re-search-forward "^@@ -" end t)
                 (beginning-of-line)
                 (when (looking-at wade-review-generate--hunk-header-re)
                   (let ((old (string-to-number (match-string 1)))
                         (new (string-to-number (match-string 3))))
                     (forward-line 1)
                     (while (and (< (point) end)
                                 (not (looking-at "^[ \t]*#\\+end_src")))
                       (pcase (char-after)
                         (?- (when (wade-review--moved-line-p old moved-old)
                               (wade-review--mark-moved-line
                                'wade-review-moved-out
                                'wade-review-moved-out-indicator
                                "Moved out of this location"))
                             (cl-incf old))
                         (?+ (when (wade-review--moved-line-p new moved-new)
                               (wade-review--mark-moved-line
                                'wade-review-moved-in
                                'wade-review-moved-in-indicator
                                "Moved into this location"))
                             (cl-incf new))
                         (?\s (cl-incf old) (cl-incf new)))
                       (forward-line 1))))))))))
       nil 'file))))

(defun wade-review--diff-overlays-to-faces (limit)
  "Copy diff-mode's syntax and fine highlights up to LIMIT into faces.
Org's native src block fontification copies text properties but not overlays.
As a font-lock matcher, this always reports no match."
  (let ((overlays (overlays-in (point) limit)))
    (dolist (kind '(syntax fine))
      (dolist (ov overlays)
        (when (and (eq (overlay-get ov 'diff-mode) kind)
                   (overlay-get ov 'face))
          (add-face-text-property (overlay-start ov) (overlay-end ov)
                                  (overlay-get ov 'face))))))
  (goto-char limit)
  nil)

(define-derived-mode wade-review-diff-mode diff-mode "WR-Diff"
  "Diff mode for fontifying review hunks in org src blocks.
Syntax and fine-change highlighting come from the hunk text alone: a review
file's diff blocks are not tied to files in `default-directory'."
  (setq-local diff-font-lock-syntax 'hunk-only
              wade-review--diff-refine diff-refine
              diff-refine nil)
  (font-lock-add-keywords
   nil
   '((wade-review--diff-font-lock-refined)
     (wade-review--diff-overlays-to-faces)
     (wade-review--comment-match
      (1 'wade-review-comment-indicator t)
      (2 'wade-review-comment t)))
   'append))

;;;###autoload
(define-minor-mode wade-review-mode
  "Minor mode for org files generated by the `wade-review' CLI.
Diff src blocks get language syntax, fine-change, and moved-code highlighting.
The commands are in `wade-review-command-map'."
  :lighter " WR"
  (if wade-review-mode
      (progn
        (setq-local org-src-lang-modes
                    (cons '("diff" . wade-review-diff) org-src-lang-modes))
        (add-hook 'after-change-functions #'wade-review--refresh-move-overlays nil t)
        (wade-review--refresh-move-overlays))
    (remove-hook 'after-change-functions #'wade-review--refresh-move-overlays t)
    (remove-overlays (point-min) (point-max) 'wade-review-move t)
    (kill-local-variable 'org-src-lang-modes))
  (when font-lock-mode
    (font-lock-flush)))

(defun wade-review--src-block-hunk-header ()
  "If point is in a diff src block hunk, return the position of its @@ line."
  (save-excursion
    (let ((bol (line-beginning-position))
          (case-fold-search t))
      (goto-char bol)
      (when (and (re-search-backward "^[ \t]*#\\+\\(begin\\|end\\)_src\\b" nil t)
                 (string-equal (downcase (match-string 1)) "begin")
                 (< (match-beginning 0) bol))
        (let ((block-start (point)))
          (goto-char bol)
          (when (re-search-backward "^@@ -" block-start t)
            (point)))))))

(defun wade-review--block-lines (header-pos)
  "Return (OLD-LINE . NEW-LINE) for point's line in the hunk at HEADER-POS.
Each is the line of that side the point's diff line is at or, for a line
absent from that side, the line it would precede."
  (save-excursion
    (let ((bol (line-beginning-position))
          (old 0) (new 0) old-start new-start)
      (goto-char header-pos)
      (unless (looking-at wade-review-generate--hunk-header-re)
        (user-error "Malformed hunk header"))
      (setq old-start (string-to-number (match-string 1))
            new-start (string-to-number (match-string 3)))
      (forward-line 1)
      (while (< (point) bol)
        (pcase (char-after)
          (?\s (cl-incf old) (cl-incf new))
          (?+ (cl-incf new))
          (?- (cl-incf old)))
        (forward-line 1))
      (cons (+ old-start old) (+ new-start new)))))

(defun wade-review--descendant-min (property)
  "Smallest numeric PROPERTY among the current heading's subtree, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((values (delq nil (org-map-entries
                             (lambda ()
                               (when-let* ((v (org-entry-get nil property)))
                                 (string-to-number v)))
                             nil 'tree))))
      (and values (apply #'min values)))))

(defun wade-review-target-at-point ()
  "Return the review target at point as (:file :old-file :status :line).
FILE is the path at the reviewed tip, relative to the repository root.
LINE is a line of that file, or, for deleted files, of OLD-FILE at the
comparison start.  Inside a diff block it is the line at point; elsewhere it is
the start of the hunk (or the file's first hunk) at point."
  (when (org-before-first-heading-p)
    (user-error "No review heading at point"))
  (let* ((file (or (org-entry-get nil "WR_FILE" t)
                   (user-error "No file for the heading at point")))
         (old-file (org-entry-get nil "WR_OLD_FILE" t))
         (status (org-entry-get nil "WR_STATUS" t))
         (deleted (equal status "deleted"))
         (start-prop (if deleted "WR_OLD_START" "WR_NEW_START"))
         (header (wade-review--src-block-hunk-header))
         (line (if header
                   (let ((lines (wade-review--block-lines header)))
                     (if deleted (car lines) (cdr lines)))
                 (let ((start (org-entry-get nil start-prop t)))
                   (if start
                       (string-to-number start)
                     (or (wade-review--descendant-min start-prop) 1))))))
    (list :file file :old-file old-file :status status :line (max 1 line))))

;;;; Worktrees

(defun wade-review--random-name ()
  (let ((chars "abcdefghijklmnopqrstuvwxyz0123456789"))
    (concat "review-"
            (apply #'string (cl-loop repeat 6 collect (aref chars (random (length chars))))))))

(defun wade-review--worktree ()
  "Return the current review's worktree directory, if it exists."
  (when-let* ((wt (wade-review--keyword "WORKTREE")))
    (and (file-directory-p wt) (file-name-as-directory wt))))

(defun wade-review--ensure-worktree ()
  "Return the current review's worktree directory, creating it if needed.
It is a detached checkout of the reviewed tip, recorded in the review file."
  (or (wade-review--worktree)
      (let* ((git-dir (wade-review--require-keyword "GIT_DIR"))
             (tip (wade-review--require-keyword "TIP"))
             (parent (expand-file-name "agent-files/wt/user/" git-dir))
             (wt (cl-loop for dir = (expand-file-name (wade-review--random-name) parent)
                          unless (file-exists-p dir) return dir)))
        (make-directory parent t)
        (message "Creating review worktree %s..." wt)
        (wade-review--git git-dir "worktree" "add" "--detach" "-q" wt tip)
        (let ((wt-git-dir (string-trim (wade-review--git wt "rev-parse" "--absolute-git-dir")))
              (marker (list :from (wade-review--require-keyword "FROM")
                            :tip tip
                            :review-file buffer-file-name)))
          (with-temp-file (expand-file-name wade-review--marker-file wt-git-dir)
            (let ((print-length nil) (print-level nil))
              (prin1 marker (current-buffer)))))
        (wade-review--set-keyword "WORKTREE" wt)
        (file-name-as-directory wt))))

(defun wade-review--worktree-info (file)
  "If FILE is in a review worktree, return (ROOT . MARKER-PLIST)."
  (when-let* ((root (locate-dominating-file file ".git"))
              (dotgit (expand-file-name ".git" root))
              ((file-regular-p dotgit))
              (gitdir (with-temp-buffer
                        (insert-file-contents dotgit)
                        (and (looking-at "gitdir: \\(.*\\)$")
                             (expand-file-name (match-string 1) root))))
              (marker (expand-file-name wade-review--marker-file gitdir))
              ((file-readable-p marker)))
    (cons (file-name-as-directory root)
          (with-temp-buffer
            (insert-file-contents marker)
            (read (current-buffer))))))

(defun wade-review--review-buffer ()
  "Return the review buffer for the current buffer, or signal."
  (cond
   (wade-review-mode (current-buffer))
   ((and buffer-file-name (wade-review--worktree-info buffer-file-name))
    (let ((review (plist-get (cdr (wade-review--worktree-info buffer-file-name))
                             :review-file)))
      (or (and review (file-exists-p review) (find-file-noselect review))
          (user-error "Review file for this worktree not found: %s" review))))
   (t (user-error "Not in a review file or review worktree"))))

(defun wade-review-close-worktree ()
  "Remove the current review's worktree and kill buffers visiting files in it.
Works from the review file or from a file in its worktree.  Asks before
discarding uncommitted changes in the worktree."
  (interactive)
  (with-current-buffer (wade-review--review-buffer)
    (let ((wt (wade-review--keyword "WORKTREE"))
          (git-dir (wade-review--require-keyword "GIT_DIR")))
      (unless wt
        (user-error "This review has no worktree"))
      (when (file-directory-p wt)
        (let ((root (file-truename (file-name-as-directory wt))))
          (dolist (buf (buffer-list))
            (let ((f (or (buffer-file-name buf)
                         (buffer-local-value 'default-directory buf))))
              (when (and f (string-prefix-p root (file-truename f)))
                (kill-buffer buf)))))
        (let* ((dirty (not (string-empty-p
                            (string-trim (wade-review--git wt "status" "--porcelain")))))
               (force (and dirty
                           (or (yes-or-no-p (format "Worktree %s has changes; discard them? " wt))
                               (user-error "Worktree not removed")))))
          (apply #'wade-review--git git-dir
                 `("worktree" "remove" ,@(and force '("--force")) ,wt))))
      (wade-review--git git-dir "worktree" "prune")
      (wade-review--set-keyword "WORKTREE" nil)
      (message "Removed review worktree %s" wt))))

(defun wade-review-open-worktree ()
  "Open the current review's worktree root in Dired, creating it if needed."
  (interactive)
  (let ((info (and buffer-file-name (wade-review--worktree-info buffer-file-name))))
    (dired (if info
               (car info)
             (with-current-buffer (wade-review--review-buffer)
               (wade-review--ensure-worktree))))))

;;;; Jumping

(defvar-local wade-review--old-file nil
  "For a renamed file, its path at the comparison start, relative to the root.")

(defun wade-review--show-old-file (git-dir rev file line)
  "Show FILE as of REV, read-only, at LINE."
  (let ((buf (get-buffer-create (format "*wade-review: %s@%s*" file (substring rev 0 8)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (wade-review--git git-dir "show" (format "%s:%s" rev file)))
        (let ((buffer-file-name (expand-file-name file)))
          (set-auto-mode))
        (setq buffer-read-only t)))
    (pop-to-buffer-same-window buf)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun wade-review-jump ()
  "Visit the file and line for the review heading or diff line at point.
Files are visited in the review worktree, which is created if needed
(see `wade-review-close-worktree').
Deleted files are shown read-only as of the comparison start.  The jump is
pushed on the xref marker stack, so `xref-go-back' returns."
  (interactive)
  (unless wade-review-mode
    (user-error "Not in a wade-review file"))
  (let* ((target (wade-review-target-at-point))
         (line (plist-get target :line)))
    (if (equal (plist-get target :status) "deleted")
        (let ((git-dir (wade-review--require-keyword "GIT_DIR"))
              (rev (wade-review--require-keyword "FROM")))
          (xref-push-marker-stack)
          (wade-review--show-old-file git-dir rev (plist-get target :old-file) line))
      (let ((wt (wade-review--ensure-worktree)))
        (xref-push-marker-stack)
        (find-file (expand-file-name (plist-get target :file) wt))
        (when (equal (plist-get target :status) "renamed")
          (setq-local wade-review--old-file (plist-get target :old-file)))
        (unless wade-review-highlight-mode
          (wade-review-highlight-mode 1))
        (goto-char (point-min))
        (forward-line (1- line))))))

;;;; Highlighting in review worktree files

(defvar-local wade-review--show-deleted nil
  "Non-nil when deleted code is shown in this buffer.")

(defvar-local wade-review--from nil)
(defvar-local wade-review--root nil)

(defun wade-review--clear-overlays ()
  (remove-overlays (point-min) (point-max) 'wade-review t))

(defun wade-review--line-pos (line)
  "Position of the start of LINE, or `point-max' past the end."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- line))) (point) (point-max))))

(defun wade-review--make-overlay (beg end &rest props)
  (let ((ov (make-overlay beg end nil (= beg end) nil)))
    (overlay-put ov 'wade-review t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defun wade-review--hunk-refinements (hunk)
  "Return fine-change ranges for parsed HUNK, or nil when disabled."
  (when diff-refine
    (wade-review--refined-hunk-ranges
     (concat (plist-get hunk :header) "\n"
             (string-join (plist-get hunk :lines) "\n") "\n"))))

(defun wade-review--line-refinements (ranges line)
  "Return members of RANGES belonging to zero-based hunk LINE."
  (cl-remove-if-not (lambda (range) (= (car range) line)) ranges))

(defun wade-review--refine-worktree-line (ranges hunk-line source-line)
  "Apply RANGES for HUNK-LINE to the worktree's SOURCE-LINE."
  (let ((line-start (wade-review--line-pos source-line)))
    (dolist (range (wade-review--line-refinements ranges hunk-line))
      (let ((start (+ line-start (max 0 (1- (nth 1 range)))))
            (end (+ line-start (max 0 (1- (nth 2 range))))))
        (when (< start end)
          (wade-review--make-overlay
           start (min end (wade-review--line-end-position-at line-start))
           'face (nth 3 range) 'priority -40))))))

(defun wade-review--refine-deleted-string (text ranges hunk-line)
  "Apply RANGES for HUNK-LINE to deleted diff-line TEXT and return it."
  (dolist (range (wade-review--line-refinements ranges hunk-line))
    (let ((start (max 0 (1- (nth 1 range))))
          (end (max 0 (1- (nth 2 range)))))
      (when (< start end)
        (add-face-text-property start (min end (length text))
                                (nth 3 range) t text))))
  text)

(defun wade-review--line-end-position-at (pos)
  "Return the end position of the line containing POS."
  (save-excursion
    (goto-char pos)
    (line-end-position)))

(defun wade-review-refresh ()
  "Recompute change highlights against the review's comparison start.
Highlights include moved code and reflect the file on disk, so they are
refreshed on save."
  (interactive)
  (unless wade-review-highlight-mode
    (user-error "`wade-review-highlight-mode' is not enabled here"))
  (let* ((file (file-relative-name buffer-file-name wade-review--root))
         (default-directory wade-review--root)
         (files (wade-review-generate--review-files wade-review--from nil t))
         (entry (cl-find file files :key (lambda (f) (plist-get f :file)) :test #'equal))
         (hunks (plist-get entry :hunks)))
    (save-restriction
      (widen)
      (wade-review--clear-overlays)
      (unless (eq hunks 'binary)
        (dolist (h hunks)
          (let ((old (plist-get h :old-start))
                (new (plist-get h :new-start))
                (hunk-line 1)
                (refinements (wade-review--hunk-refinements h))
                (changed (cl-some (lambda (l) (string-prefix-p "-" l))
                                  (plist-get h :lines))))
            (dolist (line (plist-get h :lines))
              (pcase (aref line 0)
                (?\s (cl-incf old) (cl-incf new))
                (?+ (let ((source-line new))
                      (wade-review--make-overlay
                       (wade-review--line-pos source-line)
                       (wade-review--line-pos (1+ source-line))
                       'face (if (memq source-line (plist-get h :moved-new))
                                 'wade-review-moved-in
                               (if changed 'wade-review-changed 'wade-review-added))
                       'help-echo (when (memq source-line (plist-get h :moved-new))
                                    "Moved into this location")
                       'priority -50)
                      (wade-review--refine-worktree-line
                       refinements hunk-line source-line))
                    (cl-incf new))
                (?- (when wade-review--show-deleted
                      (let* ((pos (wade-review--line-pos
                                   (if (= (plist-get h :new-count) 0)
                                       (1+ new) new)))
                             (face (if (memq old (plist-get h :moved-old))
                                       'wade-review-moved-out 'wade-review-deleted))
                             (deleted (propertize (concat (substring line 1) "\n")
                                                 'face face)))
                        (wade-review--make-overlay
                         pos pos 'face face
                         'help-echo (when (eq face 'wade-review-moved-out)
                                      "Moved out of this location")
                         'before-string
                         (concat (if (and (= pos (point-max))
                                          (not (eq (char-before pos) ?\n))
                                          (> pos (point-min)))
                                     (propertize "\n" 'face face) "")
                                 (wade-review--refine-deleted-string
                                  deleted refinements hunk-line)))))
                    (cl-incf old)))
              (cl-incf hunk-line))))))))

;;;###autoload
(define-minor-mode wade-review-highlight-mode
  "Highlight changes since a review's start in a review worktree file.
Added, changed, and moved lines get a background highlight; deleted code can be
shown with `wade-review-toggle-deleted'.  If git-gutter is installed,
`git-gutter-mode' is enabled and pointed at the comparison start."
  :lighter " WR-HL"
  (if wade-review-highlight-mode
      (let ((info (and buffer-file-name (wade-review--worktree-info buffer-file-name))))
        (if (not info)
            (progn
              (setq wade-review-highlight-mode nil)
              (user-error "Not a file in a wade-review worktree"))
          (setq wade-review--root (car info)
                wade-review--from (plist-get (cdr info) :from))
          (add-hook 'after-save-hook #'wade-review-refresh nil t)
          (require 'git-gutter nil t)
          ;; Check the feature rather than require's value, which advice on
          ;; `require' may change.
          (when (featurep 'git-gutter)
            (setq git-gutter:start-revision wade-review--from)
            (if (bound-and-true-p git-gutter-mode)
                (wade-review--git-gutter-restart)
              (git-gutter-mode 1)))
          (wade-review-refresh)))
    (remove-hook 'after-save-hook #'wade-review-refresh t)
    (wade-review--clear-overlays)))

(defun wade-review--git-gutter-restart ()
  "Rerun git-gutter's diff, discarding any diff already in progress.
`git-gutter' does nothing while a diff is running, and the diff that
`git-gutter-mode' started when the file was visited predates the start
revision."
  (when-let* ((file (buffer-file-name (buffer-base-buffer)))
              (proc-buf (get-buffer (git-gutter:diff-process-buffer file))))
    ;; The old diff's sentinel would record its (stale or empty) result even
    ;; after the new diff's, so silence it before discarding it.
    (dolist (proc (process-list))
      (when (eq (process-buffer proc) proc-buf)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer proc-buf)))
  (git-gutter))

(defun wade-review-toggle-deleted ()
  "Toggle code deleted since the comparison start in the current file."
  (interactive)
  (unless wade-review-highlight-mode
    (user-error "`wade-review-highlight-mode' is not enabled here"))
  (setq wade-review--show-deleted (not wade-review--show-deleted))
  (wade-review-refresh))

;;;###autoload
(defun wade-review-find-file-hook ()
  "Enable `wade-review-highlight-mode' for files in review worktrees.
Add this to `find-file-hook'."
  (when (and buffer-file-name
             (not wade-review-highlight-mode)
             ;; Searching up the tree for .git is slow on remote files.
             (not (file-remote-p buffer-file-name))
             (wade-review--worktree-info buffer-file-name))
    (wade-review-highlight-mode 1)))

;;;; Keymap

;;;###autoload (autoload 'wade-review-command-map "wade-review" nil nil 'keymap)
(defvar wade-review-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" #'wade-review-jump)
    (define-key map "d" #'wade-review-toggle-deleted)
    (define-key map "r" #'wade-review-refresh)
    (define-key map "k" #'wade-review-close-worktree)
    (define-key map "w" #'wade-review-open-worktree)
    map)
  "Keymap of wade-review commands.  Not bound by default.
Bind it to a prefix key, eg.
  (keymap-global-set \"C-c r\" \\='wade-review-command-map)")
(fset 'wade-review-command-map wade-review-command-map)

(provide 'wade-review)

;;; wade-review.el ends here
