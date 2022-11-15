;; Working on some infrastructure to define motions and selections by text objects, but without using evil-mode, and thus not dealing with the major error of vi: positioning the cursor ON characters in command mode instead of BETWEEN characters.  That's been a constant pain since switching to emacs, and it would be the same issue for integration with any editor that has most motions defined the normal way, with the cursor between characters.

;; One issue with thing-at-point is that point can be in multiple things if they nest, or between two things if one point can be at once the end of one and the beginning of another.  In those cases, it prefers the thing whose bounds extend ahead of the cursor.  So, eg. between two close parens it is at the end of a symex and just inside another, and it will choose to work on the symex that it is inside rather than the symex it is at but after.

(defun wgh/at-thing-beginning-p (thing)
  "If at the beginning of a thing, return its bounds, else nil."
  (let ((b (bounds-of-thing-at-point thing)))
    (and b (= (car b) (point)) b)))
(defun wgh/at-thing-end-p (thing)
  "If at the end of a thing, return its bounds, else nil."
  (let ((p-orig (point))
        (b-orig (bounds-of-thing-at-point thing)))
    (or (and b-orig
             (= p-orig (cdr b-orig))
             b-orig)
        (ignore-errors
          (save-mark-and-excursion
            (backward-char)
            ;;(forward-thing thing -1)
            (let ((b (bounds-of-thing-at-point thing)))
              (and b (= (cdr b) p-orig) b)))))))

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

(defun wgh/transpose-thing-forward-once (thing)
  (let ((bounds-1 (bounds-of-thing-at-point thing)))
    (and bounds-1
         (let ((bounds-2 (save-mark-and-excursion
                           (goto-char (cdr bounds-1))
                           (wgh/forward-thing-beginning t thing 1)
                           (bounds-of-thing-at-point thing))))
           (when (and bounds-2
                      (<= (cdr bounds-1) (car bounds-2)))
             (let ((s1 (buffer-substring-no-properties (car bounds-1)
                                                       (cdr bounds-1)))
                   (s2 (buffer-substring-no-properties (car bounds-2)
                                                       (cdr bounds-2))))
               ;; swap regions
               (atomic-change-group
                 (delete-region (car bounds-2) (cdr bounds-2))
                 (goto-char (car bounds-2))
                 (insert s1)
                 (delete-region (car bounds-1) (cdr bounds-1))
                 (goto-char (car bounds-1))
                 (insert s2))
               ;; put cursor at beginning of later region
               (let ((len-diff (- (length s2) (length s1))))
                 (goto-char (+ len-diff (car bounds-2)))
                 (undo-boundary))))))))
(defun wgh/transpose-thing-backward-once (thing)
  (let ((bounds-1 (bounds-of-thing-at-point thing)))
    (and bounds-1
         (let ((bounds-2 (save-mark-and-excursion
                           (wgh/backward-thing-beginning t thing 1)
                           (bounds-of-thing-at-point thing))))
           (when (and bounds-2
                      (< (cdr bounds-2) (car bounds-1)))
             (goto-char (car bounds-2))
             (wgh/transpose-thing-forward-once thing)
             (goto-char (car bounds-2)))))))

(defun wgh/transpose-thing-forward (thing &optional count)
  (setq count (or count 1))
  (let ((fwd (< 0 count))
        (count (abs count)))
    (while (< 0 count)
      (if fwd
          (wgh/transpose-thing-forward-once thing)
        (wgh/transpose-thing-backward-once thing))
      (setq count (- count 1)))))
(defun wgh/transpose-thing-backward (thing &optional count)
  (setq count (or count 1))
  (wgh/transpose-thing-forward thing (- count)))

(cl-defmacro wgh/def-transpose-thing (thing)
  (let ((fwd (intern (format "wgh/transpose-%s-forward" thing)))
        (bwd (intern (format "wgh/transpose-%s-backward" thing))))
    `(progn
       (defun ,fwd (&optional count)
         (interactive "p")
         (wgh/transpose-thing-forward ',thing count))
       (defun ,bwd (&optional count)
         (interactive "p")
         (wgh/transpose-thing-backward ',thing count)))))

;;;;;


(wgh/def-move-thing word :strict nil)
(wgh/def-transpose-thing word)
(wgh/def-move-thing symbol)
(wgh/def-transpose-thing symbol)
(wgh/def-move-thing sentence)
(wgh/def-transpose-thing sentence)
(wgh/def-move-thing paragraph)
(wgh/def-transpose-thing paragraph)
(put 'symex 'forward-op 'sp-forward-sexp)
(wgh/def-move-thing symex)
(wgh/def-transpose-thing symex)


;;;;;


;; I'm used to vi/evil word definition, it's strange to me that emacs' native word definition skips over punctuation and symbols, as well as blank lines.
(defun forward-vi-like-word (&optional count)
  (setq count (or count 1))
  (rx-let ((blank-line-regexp "^\\s*$")
           (vi-like-word-regexp-forward/non-blank-line
            (or (+ word)
                (+ (not (any word space))))))
    (let ((vi-like-word-regexp-forward
           (rx (or vi-like-word-regexp-forward/non-blank-line
                   blank-line-regexp)))
          (vi-like-word-regexp-backward
           (rx (or (seq space
                        vi-like-word-regexp-forward/non-blank-line)
                   (seq word (+ (not (any word space))))
                   (seq (not (any word space)) (+ word))
                   blank-line-regexp)))
          (blank-line-regexp (rx blank-line-regexp))
          (point-orig (point))
          (fwd (< 0 count))
          (count (abs count)))
      (while (< 0 count)
        (if fwd
            (re-search-forward vi-like-word-regexp-forward)
          ;; Backward requires special handling, because searching a regexp backward isn't a mirror of regexp searching forward...
          ;; This is going to be really inefficient, but I think the easiest way is...
          (progn
            (or
             (let ((searched  (ignore-errors
                                (re-search-backward vi-like-word-regexp-backward))))
               (and searched
                    (progn (unless (looking-at blank-line-regexp)
                             (forward-char 1))
                           t)))
             (beginning-of-buffer))))
        (setq count (- count 1))))))
(wgh/def-move-thing vi-like-word)

(defun -wgh/-vi-like-word-looking-at-category ()
  (cond ((looking-at (rx word)) 'word)
        ((looking-at (rx space)) 'space)
        (t 'sym)))
(defun forward-vi-like-word-2 (&optional count)
  (setq count (or count 1))
  (let ((fwd (< 0 count))
        (count (abs count)))
    (while (< 0 count)
      (if fwd
          (re-search-forward (rx (or (+ word)
                                     (+ (not (any word space)))
                                     (seq bol (* space) eol))))
        (let* ((at-cat (lambda () (save-mark-and-excursion
                                    (unless (bobp)
                                      (backward-char 1)
                                      (-wgh/-vi-like-word-looking-at-category)))))
               (orig-cat (funcall at-cat))
               (moved nil))
          (while (not (or (bobp)
                          (and moved (looking-at (rx (seq bol (* space) eol))))
                          (let ((new-cat (funcall at-cat)))
                            (or (and (not (equal new-cat orig-cat))
                                     (not (equal orig-cat 'space)))))))
            (backward-char 1)
            (setq moved t)
            (when (equal orig-cat 'space)
              (setq orig-cat (funcall at-cat))))))
      (setq count (- count 1)))))
(wgh/def-move-thing vi-like-word-2)
;; TODO - both of these attempts got me something kinda close... but wrong.  I should probably give up and do something else for now.  How much does it matter that my word movement behaves just like vim except in the specific way I want it not to?
