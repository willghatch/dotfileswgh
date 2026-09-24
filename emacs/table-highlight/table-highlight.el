;;; table-highlight.el --- Highlight rows and columns in text tables -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, markdown, org

;;; Commentary:

;; `table-highlight-mode' highlights alternating rows, cycling columns, or
;; both in Org and Markdown pipe tables.  Fontification is demand-driven, so
;; enabling the mode does not scan the entire buffer.

;;; Code:

(require 'cl-lib)
(require 'jit-lock)
(require 'seq)
(require 'subr-x)

(defgroup table-highlight nil
  "Highlight rows and columns in Org and Markdown tables."
  :group 'faces)

(defcustom table-highlight-style 'rows-and-columns
  "Table highlighting enabled automatically in supported buffers.

The value `none' prevents `table-highlight-turn-on' from enabling the
mode.  The other values select alternating rows, cycling columns, or both.
Commands such as `table-highlight-set-style' make this variable local to the
current buffer, leaving the automatic default unchanged."
  :type '(choice (const :tag "No automatic highlighting" none)
                 (const :tag "Alternating rows" rows)
                 (const :tag "Cycling columns" columns)
                 (const :tag "Alternating rows and cycling columns"
                        rows-and-columns))
  :group 'table-highlight)

(defcustom table-highlight-column-face-count 7
  "Number of column faces to cycle through.

For each index below this number, define faces named
`table-highlight-column-even-N' and `table-highlight-column-odd-N'."
  :type 'integer
  :group 'table-highlight)

(defface table-highlight-row-even
  '((t nil))
  "Face for zero-indexed even rows when highlighting rows only.
It is empty by default, so even rows keep the background they would
otherwise have."
  :group 'table-highlight)

(defface table-highlight-row-odd
  '((((background dark)) (:background "#0c0c0c"))
    (((background light)) (:background "#f7eedb")))
  "Face for zero-indexed odd rows when highlighting rows only."
  :group 'table-highlight)

;; The default column colors step through the seven rainbow hues two at a
;; time (red, yellow, blue, violet, orange, green, indigo), so adjacent
;; columns never get neighboring hues.  Seven is prime, so the stepping
;; visits every hue before repeating.
(defface table-highlight-column-even-0
  '((((background dark)) (:background "#180909"))
    (((background light)) (:background "#f9e5e2")))
  "Red column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-1
  '((((background dark)) (:background "#151505"))
    (((background light)) (:background "#f4f0d8")))
  "Yellow column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-2
  '((((background dark)) (:background "#080f1a"))
    (((background light)) (:background "#e3edf8")))
  "Blue column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-3
  '((((background dark)) (:background "#140a19"))
    (((background light)) (:background "#eee6f7")))
  "Violet column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-4
  '((((background dark)) (:background "#181006"))
    (((background light)) (:background "#f8eadb")))
  "Orange column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-5
  '((((background dark)) (:background "#081608"))
    (((background light)) (:background "#e5f2df")))
  "Green column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-even-6
  '((((background dark)) (:background "#0c0b1c"))
    (((background light)) (:background "#e6e5f6")))
  "Indigo column face on zero-indexed even rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-0
  '((((background dark)) (:background "#211010"))
    (((background light)) (:background "#f5dcd8")))
  "Red column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-1
  '((((background dark)) (:background "#1e1e08"))
    (((background light)) (:background "#eee9cc")))
  "Yellow column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-2
  '((((background dark)) (:background "#0c1624"))
    (((background light)) (:background "#d9e6f2")))
  "Blue column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-3
  '((((background dark)) (:background "#1d0e24"))
    (((background light)) (:background "#e6dcf0")))
  "Violet column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-4
  '((((background dark)) (:background "#21160a"))
    (((background light)) (:background "#f3e2cf")))
  "Orange column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-5
  '((((background dark)) (:background "#0c200c"))
    (((background light)) (:background "#dcebd5")))
  "Green column face on zero-indexed odd rows."
  :group 'table-highlight)

(defface table-highlight-column-odd-6
  '((((background dark)) (:background "#121028"))
    (((background light)) (:background "#dcdaf0")))
  "Indigo column face on zero-indexed odd rows."
  :group 'table-highlight)

(defconst table-highlight--owned-face-regexp
  "\\`table-highlight-\\(?:row-\\(?:even\\|odd\\)\\'\\|column-\\(?:even\\|odd\\)-[0-9]+\\'\\)"
  "Match the names of row and column faces that this package applies.")

;; Running as an appended Font Lock rule means the table faces are rebuilt
;; with, and after, the major mode's faces whenever Font Lock refontifies.
(defconst table-highlight--font-lock-keywords
  '((table-highlight--font-lock-matcher (0 nil keep))))

(defvar table-highlight-mode)

(defun table-highlight--column-face (row-parity column)
  "Return the face for ROW-PARITY and COLUMN."
  (intern (format "table-highlight-column-%s-%d"
                  row-parity
                  (% column table-highlight-column-face-count))))

(defun table-highlight--validate-faces ()
  "Signal a user error when the configured faces are unavailable."
  (unless (and (integerp table-highlight-column-face-count)
               (> table-highlight-column-face-count 0))
    (user-error "Table highlight column face count must be a positive integer"))
  (let ((faces
         (pcase table-highlight-style
           ('rows '(table-highlight-row-even table-highlight-row-odd))
           ('columns
            (mapcar (lambda (column)
                      (table-highlight--column-face 'even column))
                    (number-sequence 0 (1- table-highlight-column-face-count))))
           ('rows-and-columns
            (append
             (mapcar (lambda (column)
                       (table-highlight--column-face 'even column))
                     (number-sequence 0 (1- table-highlight-column-face-count)))
             (mapcar (lambda (column)
                       (table-highlight--column-face 'odd column))
                     (number-sequence 0 (1- table-highlight-column-face-count))))))))
    (dolist (face faces)
      (unless (facep face)
        (user-error "Table highlight face `%s' is not defined" face)))))

(defun table-highlight--owned-face-p (face)
  "Return non-nil when FACE belongs to table-highlight."
  (and (symbolp face)
       (string-match-p table-highlight--owned-face-regexp (symbol-name face))))

(defun table-highlight--without-owned-faces (value)
  "Return face VALUE without faces owned by table-highlight."
  (cond
   ((table-highlight--owned-face-p value) nil)
   ((and (listp value) (not (keywordp (car-safe value))))
    (let ((remaining (seq-remove #'table-highlight--owned-face-p value)))
      (pcase remaining
        ('nil nil)
        (`(,only) only)
        (_ remaining))))
   (t value)))

(defun table-highlight--remove-property-faces (start end property)
  "Remove table-highlight faces from PROPERTY between START and END."
  (with-silent-modifications
    (let ((position start))
      (while (< position end)
        (let* ((next (next-single-property-change position property nil end))
               (face (get-text-property position property))
               (remaining (table-highlight--without-owned-faces face)))
          (if remaining
              (put-text-property position next property remaining)
            (remove-text-properties position next (list property nil)))
          (setq position next))))))

(defun table-highlight--remove-faces (start end)
  "Remove table-highlight faces between START and END."
  ;; Clean `font-lock-face' too, which an earlier version of this package
  ;; used, so live upgrades do not leave stale faces behind.
  (table-highlight--remove-property-faces start end 'face)
  (table-highlight--remove-property-faces start end 'font-lock-face))

(defun table-highlight--escaped-pipe-p (position)
  "Return non-nil when the pipe at POSITION is backslash escaped."
  (let ((slashes 0)
        (cursor (1- position)))
    (while (and (>= cursor (line-beginning-position))
                (eq (char-after cursor) ?\\))
      (setq slashes (1+ slashes)
            cursor (1- cursor)))
    (= 1 (% slashes 2))))

(defun table-highlight--pipe-positions ()
  "Return unescaped pipe positions on the current line."
  (let ((end (line-end-position))
        positions)
    (save-excursion
      (beginning-of-line)
      (while (search-forward "|" end t)
        (let ((position (1- (point))))
          (unless (table-highlight--escaped-pipe-p position)
            (push position positions)))))
    (nreverse positions)))

(defun table-highlight--kind ()
  "Return the supported table syntax for the current buffer."
  (cond
   ((derived-mode-p 'org-mode) 'org)
   ((derived-mode-p 'markdown-mode 'gfm-mode) 'markdown)))

(defun table-highlight--candidate-line-p (kind)
  "Return non-nil when the current line may be a KIND table row."
  (save-excursion
    (beginning-of-line)
    (pcase kind
      ('org (looking-at-p "[ \t]*|"))
      ('markdown
       ;; Table bounds scans call this for every row, so check the common
       ;; leading-pipe form before searching the whole line for a pipe.
       (or (looking-at-p " \\{0,3\\}|")
           (let ((line-start (point)))
             (skip-chars-forward " ")
             (and (<= (- (point) line-start) 3)
                  (table-highlight--pipe-positions))))))))

(defun table-highlight--line-cell-spans (kind)
  "Return cell spans on the current KIND table row."
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (content-start
          (save-excursion
            (goto-char line-start)
            (skip-chars-forward " \t" line-end)
            (point)))
         (content-end
          (save-excursion
            (goto-char line-end)
            (skip-chars-backward " \t" line-start)
            (point)))
         (separators (table-highlight--pipe-positions)))
    (when (and (eq kind 'org)
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p "[ \t]*|[-+]+|[ \t]*$")))
      (save-excursion
        (goto-char content-start)
        (while (search-forward "+" content-end t)
          (push (1- (point)) separators)))
      (setq separators (sort separators #'<)))
    (when separators
      (let ((start (if (= (car separators) content-start)
                       (1+ (pop separators))
                     content-start))
            spans)
        (dolist (separator separators)
          (when (> separator start)
            (push (cons start separator) spans))
          (setq start (1+ separator)))
        (when (< start content-end)
          (push (cons start content-end) spans))
        (nreverse spans)))))

(defun table-highlight--markdown-delimiter-row-p ()
  "Return non-nil when the current line is a Markdown delimiter row."
  (let ((spans (table-highlight--line-cell-spans 'markdown)))
    (and (> (length spans) 1)
         (seq-every-p
          (lambda (span)
            (string-match-p
             "\\`:?-\\{3,\\}:?\\'"
             (string-trim
              (buffer-substring-no-properties (car span) (cdr span)))))
          spans))))

(defun table-highlight--markdown-in-fence-p ()
  "Return non-nil when point is in a Markdown fenced code block."
  (if (fboundp 'markdown-code-block-at-point-p)
      (condition-case nil
          (markdown-code-block-at-point-p)
        (error nil))
    ;; This fallback keeps the package useful with lightweight Markdown modes.
    ;; Full markdown-mode provides the parser-backed predicate above.
    (let ((limit (point))
          (fences 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ ]*\\(```+\\|~~~+\\)" limit t)
          (when (<= (- (match-beginning 1) (line-beginning-position)) 3)
            (setq fences (1+ fences)))))
      (= 1 (% fences 2)))))

(defun table-highlight--org-in-block-p ()
  "Return non-nil when point is in an Org literal block."
  (and (fboundp 'org-in-block-p)
       (org-in-block-p '("src" "example" "export" "comment" "verse"))))

(defun table-highlight--excluded-p (kind)
  "Return non-nil when the current KIND table candidate is literal text."
  (pcase kind
    ('org (table-highlight--org-in-block-p))
    ('markdown (table-highlight--markdown-in-fence-p))))

(defun table-highlight--table-end (kind limit)
  "Return the end of consecutive KIND table candidates before LIMIT."
  (save-excursion
    (while (and (< (point) limit)
                (table-highlight--candidate-line-p kind))
      (forward-line 1))
    (point)))

(defun table-highlight--table-start (kind)
  "Return the start of the KIND table candidate containing point."
  (save-excursion
    (beginning-of-line)
    (while (and (> (point) (point-min))
                (save-excursion
                  (forward-line -1)
                  (table-highlight--candidate-line-p kind)))
      (forward-line -1))
    (point)))

(defun table-highlight--markdown-table-p (start end)
  "Return non-nil when START through END contain a Markdown table."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (and (< (point) end)
         (table-highlight--markdown-delimiter-row-p))))

(defun table-highlight--apply-face (start end face)
  "Give FACE precedence over existing faces between START and END."
  ;; Table faces only set backgrounds, so taking precedence lets them show
  ;; over a major-mode table background without hiding other attributes.
  (when (< start end)
    (with-silent-modifications
      (font-lock-prepend-text-property start end 'face face))))

(defun table-highlight--fontify-row (kind row)
  "Fontify the current KIND table row at zero-indexed ROW."
  (let* ((even (= 0 (% row 2)))
         (parity (if even 'even 'odd))
         (row-face (if even
                       'table-highlight-row-even
                     'table-highlight-row-odd))
         (line-start (line-beginning-position))
         (line-end (line-end-position)))
    (pcase table-highlight-style
      ('rows
       (table-highlight--apply-face line-start line-end row-face))
      ('columns
       (cl-loop for span in (table-highlight--line-cell-spans kind)
                for column from 0
                do (table-highlight--apply-face
                    (car span) (cdr span)
                    (table-highlight--column-face 'even column))))
      ('rows-and-columns
       (table-highlight--apply-face line-start line-end row-face)
       (cl-loop for span in (table-highlight--line-cell-spans kind)
                for column from 0
                do (table-highlight--apply-face
                    (car span) (cdr span)
                    (table-highlight--column-face parity column)))))))

(defun table-highlight--fontify-table (kind start end)
  "Fontify a KIND table between START and END."
  (save-excursion
    (goto-char start)
    (let ((row 0))
      (while (< (point) end)
        (table-highlight--fontify-row kind row)
        (setq row (1+ row))
        (forward-line 1)))))

(defun table-highlight--line-bounds (start end)
  "Expand START and END to complete lines."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let ((line-start (point)))
      (goto-char end)
      (unless (bolp)
        (forward-line 1))
      (cons line-start (point)))))

(defun table-highlight--fontify (start end)
  "Fontify supported tables intersecting START through END."
  (when-let ((kind (table-highlight--kind)))
    (save-restriction
      (widen)
      (pcase-let* ((`(,scan-start . ,scan-end)
                    (table-highlight--line-bounds start end)))
        (table-highlight--remove-faces scan-start scan-end)
        (save-excursion
          (goto-char scan-start)
          (while (< (point) scan-end)
            (if (not (table-highlight--candidate-line-p kind))
                (forward-line 1)
              (let* ((visible-start (point))
                     (table-start (table-highlight--table-start kind))
                     (table-end (table-highlight--table-end kind scan-end))
                     (valid
                      (and (not (table-highlight--excluded-p kind))
                           (or (eq kind 'org)
                               (table-highlight--markdown-table-p
                                table-start (point-max))))))
                (when valid
                  (if (= visible-start table-start)
                      (table-highlight--fontify-table
                       kind visible-start table-end)
                    ;; Preserve parity when JIT starts in the middle of a table.
                    (save-excursion
                      (goto-char visible-start)
                      (let ((row (count-lines table-start visible-start)))
                        (while (< (point) table-end)
                          (table-highlight--fontify-row kind row)
                          (setq row (1+ row))
                          (forward-line 1))))))
                (goto-char table-end)))))))))

(defun table-highlight--font-lock-matcher (limit)
  "Apply table faces through point up to font-lock LIMIT.

Return one dummy match because this matcher applies its faces directly."
  (when (< (point) limit)
    (table-highlight--fontify (point) limit)
    (goto-char limit)
    (set-match-data (list (1- limit) limit))
    t))

(defun table-highlight--after-change (start end _old-length)
  "Defer refontification of the tables around the change from START to END."
  (when-let ((kind (and table-highlight-mode (table-highlight--kind))))
    (save-excursion
      (save-restriction
        (widen)
        ;; Start one line early, since a new Markdown delimiter row turns the
        ;; line above it into a table header.  Continue past the change to the
        ;; end of the following table, whose row parity the change may shift.
        (let ((from (progn
                      (goto-char start)
                      (forward-line -1)
                      (table-highlight--table-start kind)))
              (to (progn
                    (goto-char end)
                    (forward-line 1)
                    (table-highlight--table-end kind (point-max)))))
          (jit-lock-refontify from to))))))

(defun table-highlight--refresh ()
  "Refontify table highlighting without scanning synchronously."
  (when font-lock-mode
    (font-lock-flush (point-min) (point-max))))

;;;###autoload
(define-minor-mode table-highlight-mode
  "Highlight table rows and columns in the current buffer."
  :lighter " TblHi"
  (if table-highlight-mode
      (condition-case error-data
          (progn
            (table-highlight--validate-faces)
            (font-lock-add-keywords
             nil table-highlight--font-lock-keywords 'append)
            (add-hook 'after-change-functions
                      #'table-highlight--after-change nil t)
            (table-highlight--refresh))
        (error
         (setq table-highlight-mode nil)
         (signal (car error-data) (cdr error-data))))
    (font-lock-remove-keywords nil table-highlight--font-lock-keywords)
    (remove-hook 'after-change-functions #'table-highlight--after-change t)
    (table-highlight--remove-faces (point-min) (point-max))
    (table-highlight--refresh)))

;;;###autoload
(defun table-highlight-turn-on ()
  "Enable `table-highlight-mode' according to `table-highlight-style'."
  (unless (eq table-highlight-style 'none)
    (table-highlight-mode 1)))

;;;###autoload
(defun table-highlight-set-style (style)
  "Set table highlighting STYLE in the current buffer."
  (interactive
   (list (intern
          (completing-read "Table highlighting: "
                           '(none rows columns rows-and-columns)
                           nil t nil nil
                           (symbol-name table-highlight-style)))))
  (setq-local table-highlight-style style)
  (if (eq style 'none)
      (table-highlight-mode -1)
    (table-highlight--validate-faces)
    (table-highlight-mode 1)
    (table-highlight--refresh)))

;;;###autoload
(defun table-highlight-toggle-rows ()
  "Toggle alternating row highlighting in the current buffer."
  (interactive)
  (table-highlight-set-style
   (pcase table-highlight-style
     ('none 'rows)
     ('rows 'none)
     ('columns 'rows-and-columns)
     (_ 'columns))))

;;;###autoload
(defun table-highlight-toggle-columns ()
  "Toggle column highlighting in the current buffer."
  (interactive)
  (table-highlight-set-style
   (pcase table-highlight-style
     ('none 'columns)
     ('columns 'none)
     ('rows 'rows-and-columns)
     (_ 'rows))))

(provide 'table-highlight)

;;; table-highlight.el ends here
