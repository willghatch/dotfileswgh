
; taken from emacswiki
(defun set-tab-stop-width (width)
      "Set all tab stops to WIDTH in current buffer.
    This updates `tab-stop-list', but not `tab-width'.
    By default, `indent-for-tab-command' uses tabs to indent, see
    `indent-tabs-mode'."
      (interactive "nTab width: ")
      (let* ((max-col (car (last tab-stop-list)))
             ;; If width is not a factor of max-col,
             ;; then max-col could be reduced with each call.
             (n-tab-stops (/ max-col width)))
        (set (make-local-variable 'tab-stop-list)
             (mapcar (lambda (x) (* width x))
                     (number-sequence 1 n-tab-stops)))
        ;; So preserve max-col, by adding to end.
        (unless (zerop (% max-col width))
          (setcdr (last tab-stop-list)
                  (list max-col)))))

; adapted from emacswiki
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, include time.
   Two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y-%m-%d %H:%M")
                 ((equal prefix '(16)) "%A, %d %B %Y"))))
    (insert (format-time-string format))))
(defun insert-date-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d_%H:%M")))


;; adapted from ergoemacs site
(defun color-hex-syntax ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

