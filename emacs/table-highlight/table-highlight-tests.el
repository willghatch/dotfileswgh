;;; table-highlight-tests.el --- Tests for table-highlight -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'seq)
(require 'table-highlight)

(unless (fboundp 'markdown-mode)
  (define-derived-mode markdown-mode text-mode "Markdown"))

;; Themes commonly give Markdown code and tables a background, so the
;; stand-in table face has one too, along with attributes that the table
;; highlighting must leave visible.
(defface table-highlight-test-table-face
  '((t (:foreground "#abcdef" :background "#123456" :weight bold)))
  "Major-mode table face used to test composed display attributes.")

(define-derived-mode table-highlight-test-markdown-mode markdown-mode "MdTest"
  "Markdown mode that faces whole table lines as `markdown-mode' does."
  (font-lock-add-keywords
   nil '(("^|.*\n?" (0 'table-highlight-test-table-face append)))))

(defun table-highlight-test--position-at (text occurrence)
  "Return the position of OCCURRENCE of TEXT in the current buffer."
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (dotimes (_ occurrence)
      (search-forward text)))
  (- (point) (length text)))

(defun table-highlight-test--faces-at (text occurrence)
  "Return faces at OCCURRENCE of TEXT in the current buffer."
  (let* ((position (table-highlight-test--position-at text occurrence))
         (face (get-char-property position 'face))
         (font-lock-face (get-char-property position 'font-lock-face)))
    (append (if (listp face) face (list face))
            (if (listp font-lock-face)
                font-lock-face
              (list font-lock-face)))))

(defun table-highlight-test--face-attribute (face attribute)
  "Return FACE's ATTRIBUTE, following inheritance, or `unspecified'."
  (cond
   ((facep face) (face-attribute face attribute nil t))
   ((and (listp face) (keywordp (car-safe face)))
    (let ((value (plist-get face attribute))
          (inherit (plist-get face :inherit)))
      (cond
       ((and value (not (eq value 'unspecified))) value)
       (inherit (table-highlight-test--first-attribute
                 (if (listp inherit) inherit (list inherit)) attribute))
       (t 'unspecified))))
   (t 'unspecified)))

(defun table-highlight-test--first-attribute (faces attribute)
  "Return the first specified ATTRIBUTE among FACES, or `unspecified'.
Earlier faces take precedence, as they do in the `face' property."
  (or (seq-some
       (lambda (face)
         (let ((value (table-highlight-test--face-attribute face attribute)))
           (unless (eq value 'unspecified) value)))
       faces)
      'unspecified))

(defun table-highlight-test--effective-attribute-at (text occurrence attribute)
  "Return the displayed ATTRIBUTE at OCCURRENCE of TEXT.
This resolves the face that redisplay would use, including the
`font-lock-face' alias, which only applies where `face' is absent."
  (let* ((position (table-highlight-test--position-at text occurrence))
         (face (get-char-property position 'face))
         (value (table-highlight-test--first-attribute
                 (if (and face (or (symbolp face) (keywordp (car-safe face))))
                     (list face)
                   face)
                 attribute)))
    (if (eq value 'unspecified)
        (face-attribute 'default attribute nil t)
      value)))

(defun table-highlight-test--fontify (mode text style)
  "Enable MODE for TEXT and fontify with table highlighting STYLE."
  (funcall mode)
  (insert text)
  (setq-local table-highlight-style style)
  (table-highlight-mode 1)
  (font-lock-ensure (point-min) (point-max)))

(ert-deftest table-highlight-row-only-alternates-from-even ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode "| A | B |\n|---+---|\n| C | D |\n" 'rows)
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "A" 1)))
    (should (memq 'table-highlight-row-odd
                  (table-highlight-test--faces-at "---" 1)))
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "C" 1)))
    (should-not (seq-some
                 (lambda (face)
                   (string-prefix-p "table-highlight-column-"
                                    (symbol-name face)))
                 (table-highlight-test--faces-at "A" 1)))))

(ert-deftest table-highlight-column-only-cycles-even-faces ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode
     "| A | B | C | D | E | F | G | H |\n| I | J | K | L | M | N | O | P |\n"
     'columns)
    (should (memq 'table-highlight-column-even-0
                  (table-highlight-test--faces-at "A" 1)))
    (should (memq 'table-highlight-column-even-1
                  (table-highlight-test--faces-at "B" 1)))
    (should (memq 'table-highlight-column-even-0
                  (table-highlight-test--faces-at "H" 1)))
    (should (memq 'table-highlight-column-even-6
                  (table-highlight-test--faces-at "G" 1)))
    (should-not (memq 'table-highlight-row-even
                      (table-highlight-test--faces-at "A" 1)))))

(ert-deftest table-highlight-combined-varies-column-face-by-row ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode "| A | B |\n| C | D |\n" 'rows-and-columns)
    (should (memq 'table-highlight-column-even-0
                  (table-highlight-test--faces-at "A" 1)))
    (should (memq 'table-highlight-column-even-1
                  (table-highlight-test--faces-at "B" 1)))
    (should (memq 'table-highlight-column-odd-0
                  (table-highlight-test--faces-at "C" 1)))
    (should (memq 'table-highlight-column-odd-1
                  (table-highlight-test--faces-at "D" 1)))))

(ert-deftest table-highlight-custom-column-count-and-missing-face-error ()
  (with-temp-buffer
    (let ((table-highlight-column-face-count 8))
      (org-mode)
      (insert "| A |\n")
      (should-error (table-highlight-mode 1) :type 'user-error)))
  (let ((new-face 'table-highlight-column-even-7)
        (new-odd-face 'table-highlight-column-odd-7))
    (unwind-protect
        (progn
          (make-face new-face)
          (make-face new-odd-face)
          (with-temp-buffer
            (let ((table-highlight-column-face-count 8))
              (table-highlight-test--fontify
               #'org-mode "| A | B | C | D | E | F | G | H |\n" 'columns)
              (should (memq new-face
                            (table-highlight-test--faces-at "H" 1))))))
      (face-spec-reset-face new-face)
      (face-spec-reset-face new-odd-face))))

(ert-deftest table-highlight-markdown-requires-delimiter-and-handles-escapes ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'markdown-mode
     "| prose | with pipes |\n\n| A | B |\n| --- | --- |\n| C \\| D | E |\n"
     'columns)
    (should-not (seq-some
                 (lambda (face)
                   (and (symbolp face)
                        (string-prefix-p "table-highlight-"
                                         (symbol-name face))))
                 (table-highlight-test--faces-at "prose" 1)))
    (should (memq 'table-highlight-column-even-0
                  (table-highlight-test--faces-at "C" 1)))
    (should (memq 'table-highlight-column-even-1
                  (table-highlight-test--faces-at "E" 1)))))

;; The major mode faces whole table lines, and redisplay must still show the
;; table backgrounds, even after the major mode refontifies the table.
(ert-deftest table-highlight-composes-visible-background-after-refontification ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'table-highlight-test-markdown-mode
     (concat
      "| Finding | Evidence | Impact | Recommendation | Owner | Priority | Follow-up |\n"
      "| --- | --- | --- | --- | --- | --- | --- |\n"
      "| The generated report contains a table wider than the current window | "
      "Several cells contain complete explanatory sentences that wrap | "
      "Readers can lose track | Use highlighting | Documentation team | High | "
      "Recheck in a narrow window |\n")
     'rows-and-columns)
    (font-lock-flush (point-min) (point-max))
    (font-lock-ensure (point-min) (point-max))
    (dolist (case '(("Finding" table-highlight-column-even-0)
                    ("Evidence" table-highlight-column-even-1)
                    ("---" table-highlight-column-odd-0)
                    ("The generated report" table-highlight-column-even-0)
                    ("Several cells" table-highlight-column-even-1)))
      (should (equal
               (list (car case)
                     (table-highlight-test--effective-attribute-at
                      (car case) 1 :background))
               (list (car case)
                     (face-attribute (cadr case) :background nil t)))))
    (should (equal (table-highlight-test--effective-attribute-at
                    "Finding" 1 :foreground)
                   "#abcdef"))
    (should (eq (table-highlight-test--effective-attribute-at
                 "Finding" 1 :weight)
                'bold))))

;; Row highlighting must show over a major-mode table background without
;; hiding the major mode's other attributes on either row parity.
(ert-deftest table-highlight-rows-compose-visibly-with-major-mode-faces ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'table-highlight-test-markdown-mode
     "| Head | B |\n| --- | --- |\n| Body | D |\n"
     'rows)
    (should (equal (table-highlight-test--effective-attribute-at
                    "---" 1 :background)
                   (face-attribute 'table-highlight-row-odd :background nil t)))
    (dolist (text '("Head" "---" "Body"))
      (should (equal (list text (table-highlight-test--effective-attribute-at
                                 text 1 :foreground))
                     (list text "#abcdef")))
      (should (equal (list text (table-highlight-test--effective-attribute-at
                                 text 1 :weight))
                     (list text 'bold))))))

(ert-deftest table-highlight-org-table-includes-horizontal-rule ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode "before\n| A | B |\n|---+---|\n| C | D |\nafter\n" 'rows)
    (should-not (get-char-property (point-min) 'face))
    (should (memq 'table-highlight-row-odd
                  (table-highlight-test--faces-at "---" 1)))
    (goto-char (point-max))
    (search-backward "after")
    (should-not (memq 'table-highlight-row-even
                      (table-highlight-test--faces-at "after" 1)))))

(ert-deftest table-highlight-skips-markdown-fences-and-org-blocks ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'markdown-mode
     "```text\n| A | B |\n| --- | --- |\n```\n\n| C | D |\n| --- | --- |\n"
     'rows)
    (should-not (memq 'table-highlight-row-even
                      (table-highlight-test--faces-at "A" 1)))
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "C" 1))))
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode
     "#+begin_example\n| A | B |\n#+end_example\n\n| C | D |\n"
     'rows)
    (should-not (memq 'table-highlight-row-even
                      (table-highlight-test--faces-at "A" 1)))
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "C" 1)))))

(ert-deftest table-highlight-edit-refreshes-table ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode "| A | B |\n| C | D |\n" 'rows)
    (goto-char (point-min))
    (insert "| X | Y |\n")
    (font-lock-ensure (point-min) (point-max))
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "X" 1)))
    (should (memq 'table-highlight-row-odd
                  (table-highlight-test--faces-at "A" 1)))))

(defun table-highlight-test--refreshed-p (text occurrence face)
  "Return non-nil when OCCURRENCE of TEXT shows FACE or awaits refontification."
  (or (null (get-text-property
             (table-highlight-test--position-at text occurrence) 'fontified))
      (memq face (table-highlight-test--faces-at text occurrence))))

(defun table-highlight-test--mark-fontified ()
  "Mark the whole buffer as already fontified, as after redisplay."
  (with-silent-modifications
    (put-text-property (point-min) (point-max) 'fontified t)))

;; An edit can change the parity of every later row in its table, and a new
;; Markdown delimiter row makes the line above it a header, so those must be
;; refreshed.  Invalidating unrelated later text would make every keystroke
;; cost time proportional to the rest of the buffer.
(ert-deftest table-highlight-edit-invalidates-only-surrounding-tables ()
  (with-temp-buffer
    (table-highlight-test--fontify
     #'org-mode "| A |\n| B |\n| C |\n\nprose\n\n| Far |\n" 'rows)
    (table-highlight-test--mark-fontified)
    (goto-char (point-min))
    (insert "| X |\n")
    (should (table-highlight-test--refreshed-p
             "C" 1 'table-highlight-row-odd))
    (should (get-text-property
             (table-highlight-test--position-at "Far" 1) 'fontified)))
  (with-temp-buffer
    (table-highlight-test--fontify
     #'markdown-mode "| H | I |\n| a | b |\n\nprose\n\n| Far |\n| --- |\n" 'rows)
    (table-highlight-test--mark-fontified)
    (goto-char (point-min))
    (forward-line 1)
    (insert "| --- | --- |\n")
    (should (table-highlight-test--refreshed-p
             "H" 1 'table-highlight-row-even))
    (should (table-highlight-test--refreshed-p
             "a" 1 'table-highlight-row-even))
    (should (get-text-property
             (table-highlight-test--position-at "Far" 1) 'fontified))))

(ert-deftest table-highlight-partial-fontification-preserves-table-parity ()
  (with-temp-buffer
    (org-mode)
    (insert "| A |\n| B |\n| C |\n| D |\n")
    (setq-local table-highlight-style 'rows)
    (table-highlight-mode 1)
    (goto-char (point-min))
    (forward-line 1)
    (font-lock-fontify-region (line-beginning-position) (line-end-position))
    (should (memq 'table-highlight-row-odd
                  (table-highlight-test--faces-at "B" 1)))
    (should-not (memq 'table-highlight-row-even
                      (table-highlight-test--faces-at "C" 1)))))

(ert-deftest table-highlight-disable-removes-only-package-faces ()
  (with-temp-buffer
    (table-highlight-test--fontify #'org-mode "| A | B |\n" 'rows)
    (should (memq 'table-highlight-row-even
                  (table-highlight-test--faces-at "A" 1)))
    (goto-char (point-min))
    (search-forward "A")
    (add-face-text-property (1- (point)) (point) 'bold t)
    (table-highlight-mode -1)
    (let ((faces (table-highlight-test--faces-at "A" 1)))
      (should (memq 'bold faces))
      (should-not (memq 'table-highlight-row-even faces)))))

(ert-deftest table-highlight-auto-style-controls-mode ()
  (dolist (case '((none nil)
                  (rows t)
                  (columns t)
                  (rows-and-columns t)))
    (with-temp-buffer
      (org-mode)
      (let ((table-highlight-style (car case)))
        (table-highlight-turn-on)
        (should (eq (and table-highlight-mode t) (cadr case)))))))

(provide 'table-highlight-tests)

;;; table-highlight-tests.el ends here
