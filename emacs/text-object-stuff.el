;; Working on some infrastructure to define motions and selections by text objects, but without using evil-mode, and thus not dealing with the major error of vi: positioning the cursor ON characters in command mode instead of BETWEEN characters.  That's been a constant pain since switching to emacs, and it would be the same issue for integration with any editor that has most motions defined the normal way, with the cursor between characters.

;; One issue with thing-at-point is that point can be in multiple things if they nest, or between two things if one point can be at once the end of one and the beginning of another.  In those cases, it prefers the thing whose bounds extend ahead of the cursor.  So, eg. between two close parens it is at the end of a symex and just inside another, and it will choose to work on the symex that it is inside rather than the symex it is at but after.

(defun wgh/at-thing-beginning-p (thing)
  "If at the beginning of a thing, return its bounds, else nil."
  (let ((b (bounds-of-thing-at-point thing)))
    (and b (= (car b) (point)) b)))
(defun wgh/at-thing-end-p (thing)
  "If at the end of a thing, return its bounds, else nil."
  (let ((p-orig (point)))
    (ignore-errors
      (save-mark-and-excursion
        (backward-char)
        ;;(forward-thing thing -1)
        (let ((b (bounds-of-thing-at-point thing)))
          (and b (= (cdr b) p-orig) b))))))

(defun wgh/forward-thing (strict thing &optional count)
  "Like forward-thing, but add strict param. If strict, it only goes forward/back if at the end it is actually on a thing.  Return the number of things left to move."
  (setq count (or count 1))
  (let ((keep-going t)
        (fwd-p (> count 0))
        (count (abs count)))
    (while (and keep-going (> count 0))
      (let ((pos (point))
            (bounds-begin-l (wgh/at-thing-end-p thing))
            (bounds-begin-r (wgh/at-thing-beginning-p thing))
            (bounds-begin (bounds-of-thing-at-point thing))
            (bounds-extend (lambda (old new)
                             (and old new
                                  (and
                                   ;; The new bounds include the old bounds.
                                   (and (<= (car new) (car old))
                                        (<= (cdr old) (cdr new)))
                                   ;; At least one of the new bounds goes beyond
                                   ;; the old ones.
                                   (or (< (car new) (car old))
                                       (< (cdr old) (cdr new))))))))
        (if fwd-p
            (forward-thing thing 1)
          (forward-thing thing -1))
        (setq count (- count 1))
        (let ((bounds-end (if fwd-p
                              (wgh/at-thing-end-p thing)
                            (wgh/at-thing-beginning-p thing))))
          (message "bounds begin: %s, bounds end %s" bounds-begin bounds-end)
          (when (or
                 ;; If there are no bounds, we have not actually moved to a thing, just moved without finding one.
                 (not bounds-end)
                 ;; If the new bounds of the thing include the old bounds and more, we have gone up in a tree of things, which is not a strict behavior.
                 (funcall bounds-extend bounds-begin-l bounds-end)
                 (funcall bounds-extend bounds-begin-r bounds-end)
                 (funcall bounds-extend bounds-begin bounds-end))
            (setq keep-going nil)
            (when strict (goto-char pos))))))
    (if keep-going 0 (+ count 1))))

;; Forward end and backward beginning are the easy cases, the ones normally supported by forward-thing.
;; IE forward-thing goes to the end of the thing with a positive number, and the beginning with a negative number.
(defun wgh/forward-thing-end (strict thing &optional count)
  (setq count (or count 1))
  (if (< count 0)
      (wgh/backward-thing-end strict thing count)
    (wgh/forward-thing strict thing count)))
(defun wgh/backward-thing-beginning (strict thing &optional count)
  (setq count (or count 1))
  (if (< count 0)
      (wgh/forward-thing-beginning strict thing count)
    (wgh/forward-thing strict thing (- count))))

(defun -wgh/fwd-beg_or_bwd-end_thing (strict thing count fwd-beg-p)
  (setq count (or count 1))
  (if (< count 0)
      ((if fwd-beg-p #'wgh/backward-thing-beginning #'wgh/forward-thing-end)
       strict thing count)
    (let* ((orig-point (point))
           (bounds (or (if fwd-beg-p
                           (wgh/at-thing-beginning-p thing)
                         (wgh/at-thing-end-p thing))
                       (bounds-of-thing-at-point thing))))
      (when (and bounds (<= (car bounds) orig-point (cdr bounds)))
        (goto-char (if fwd-beg-p (cdr bounds) (car bounds))))
      (let ((n-left (wgh/forward-thing strict thing (if fwd-beg-p count (- count)))))
        (when (or strict
                  (= 0 n-left))
          ;; I would use beginning/end-of-thing, but forward-thing is more robust due to nesting or adjacent things.
          (if fwd-beg-p (forward-thing thing -1) (forward-thing thing 1)))
        ))))
(defun wgh/forward-thing-beginning (strict thing &optional count)
  (-wgh/fwd-beg_or_bwd-end_thing strict thing count t))
(defun wgh/backward-thing-end (strict thing &optional count)
  (-wgh/fwd-beg_or_bwd-end_thing strict thing count nil))

;;;;;;

(cl-defmacro wgh/def-move-thing (thing &key (strict t))
  (let ((fwd-beg (intern (format "wgh/forward-%s-beginning" thing)))
        (fwd-end (intern (format "wgh/forward-%s-end" thing)))
        (bwd-end (intern (format "wgh/backward-%s-end" thing)))
        (bwd-beg (intern (format "wgh/backward-%s-beginning" thing)))
        )
    `(progn
       (defun ,fwd-beg (&optional count)
         (interactive "p")
         (wgh/forward-thing-beginning ,strict ',thing count))
       (defun ,fwd-end (&optional count)
         (interactive "p")
         (wgh/forward-thing-end ,strict ',thing count))
       (defun ,bwd-end (&optional count)
         (interactive "p")
         (wgh/backward-thing-end ,strict ',thing count))
       (defun ,bwd-beg (&optional count)
         (interactive "p")
         (wgh/backward-thing-beginning ,strict ',thing count))
       (repeatable-motion-define-pair ',fwd-beg ',bwd-beg)
       (repeatable-motion-define-pair ',fwd-end ',bwd-end)
       )))

;;;;;;

;; TODO - delete this, but it was useful as a test, so I want it to end up in at least one commit for potential future use.
;;(defun forward-pair-test (&optional count)
;;  (setq count (or count 1))
;;  (let* ((fwd (< 0 count))
;;         (count (abs count))
;;         (done nil))
;;    (ignore-errors
;;      (while (and (not done) (< 0 count))
;;        (progn
;;          (condition-case nil (if fwd (forward-char) (backward-char)) (setq done t))
;;          (if fwd
;;              (when (and (< 2 (point))
;;                         (save-mark-and-excursion (backward-char 2) (looking-at "()")))
;;                (setq count (- count 1)))
;;            (when (looking-at "()")
;;              (setq count (- count 1)))))))))
;;
;;(put 'pair-strict 'forward-op 'forward-pair-test)
;;(put 'pair-lax 'forward-op 'forward-pair-test)
;;(wgh/def-move-thing pair-strict :strict t)
;;(wgh/def-move-thing pair-lax :strict nil)

(wgh/def-move-thing word :strict nil)
(wgh/def-move-thing symbol)
(wgh/def-move-thing sentence)
(wgh/def-move-thing paragraph)
(put 'symex 'forward-op 'sp-forward-sexp)
(wgh/def-move-thing symex)

