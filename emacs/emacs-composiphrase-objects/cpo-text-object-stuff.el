;;; -*- lexical-binding: t; -*-
;;; This file provides motions explicitly to the beginning/end of non-tree objects, as well as region expansion and transposition for those same objects.
;;; TODO - at least list the public exports of this file, which are basically all macro outputs.

(defun cpo-text-object-stuff--at-thing-beginning-p (thing)
  "If at the beginning of a thing, return its bounds, else nil."
  (let ((b (bounds-of-thing-at-point thing)))
    (and b (= (car b) (point)) b)))
(defun cpo-text-object-stuff--at-thing-end-p (thing)
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

(defun cpo-text-object-stuff--bounds-extend (old new)
  ;; TODO - I have basically this same function defined elsewhere, too.  I should deduplicate.
  (and old new
       (and
        ;; The new bounds include the old bounds.
        (and (<= (car new) (car old))
             (<= (cdr old) (cdr new)))
        ;; At least one of the new bounds goes beyond
        ;; the old ones.
        (or (< (car new) (car old))
            (< (cdr old) (cdr new))))))

(defun cpo-text-object-stuff--forward-thing (strict thing &optional count)
  "Like forward-thing, but add strict param. If strict, it only goes forward/back if at the end it is actually on a thing.  Return the number of things left to move."
  (setq count (or count 1))
  (let ((keep-going t)
        (fwd-p (> count 0))
        (count (abs count)))
    (while (and keep-going (> count 0))
      (let ((pos (point))
            (bounds-begin-l (cpo-text-object-stuff--at-thing-end-p thing))
            (bounds-begin-r (cpo-text-object-stuff--at-thing-beginning-p thing))
            (bounds-begin (bounds-of-thing-at-point thing)))
        (if fwd-p
            (forward-thing thing 1)
          (forward-thing thing -1))
        (setq count (- count 1))
        (let ((bounds-end (if fwd-p
                              (cpo-text-object-stuff--at-thing-end-p thing)
                            (cpo-text-object-stuff--at-thing-beginning-p thing))))
          (when (or
                 ;; If there are no bounds, we have not actually moved to a thing, just moved without finding one.
                 (not bounds-end)
                 ;; If the new bounds of the thing include the old bounds and more, we have gone up in a tree of things, which is not a strict behavior.
                 (cpo-text-object-stuff--bounds-extend bounds-begin-l bounds-end)
                 (cpo-text-object-stuff--bounds-extend bounds-begin-r bounds-end)
                 (cpo-text-object-stuff--bounds-extend bounds-begin bounds-end))
            (setq keep-going nil)
            (when strict (goto-char pos))))))
    (if keep-going 0 (+ count 1))))

;; Forward end and backward beginning are the easy cases, the ones normally supported by forward-thing.
;; IE forward-thing goes to the end of the thing with a positive number, and the beginning with a negative number.
(defun cpo-text-object-stuff--forward-thing-end (strict thing &optional count)
  (setq count (or count 1))
  (if (< count 0)
      (cpo-text-object-stuff--backward-thing-end strict thing count)
    (cpo-text-object-stuff--forward-thing strict thing count)))
(defun cpo-text-object-stuff--backward-thing-beginning (strict thing &optional count)
  (setq count (or count 1))
  (if (< count 0)
      (cpo-text-object-stuff--forward-thing-beginning strict thing count)
    (cpo-text-object-stuff--forward-thing strict thing (- count))))

(defun cpo-text-object-stuff--fwd-beg_or_bwd-end_thing (strict thing count fwd-beg-p)
  (setq count (or count 1))
  (if (< count 0)
      ((if fwd-beg-p #'cpo-text-object-stuff--backward-thing-beginning #'cpo-text-object-stuff--forward-thing-end)
       strict thing count)
    (let* ((orig-point (point))
           (bounds (or (if fwd-beg-p
                           (cpo-text-object-stuff--at-thing-beginning-p thing)
                         (cpo-text-object-stuff--at-thing-end-p thing))
                       (bounds-of-thing-at-point thing))))
      (when (and bounds (<= (car bounds) orig-point (cdr bounds)))
        (goto-char (if fwd-beg-p (cdr bounds) (car bounds))))
      (let ((n-left (cpo-text-object-stuff--forward-thing strict thing (if fwd-beg-p count (- count)))))
        (when (or strict
                  (= 0 n-left))
          (let ((bounds (if fwd-beg-p
                            (cpo-text-object-stuff--at-thing-end-p thing)
                          (cpo-text-object-stuff--at-thing-beginning-p thing))))
            (if bounds (goto-char (if fwd-beg-p (car bounds) (cdr bounds)))))
          ;; I would use beginning/end-of-thing, but forward-thing is more robust due to nesting or adjacent things.
          ;;(if fwd-beg-p (forward-thing thing -1) (forward-thing thing 1))
          )
        ))))
(defun cpo-text-object-stuff--forward-thing-beginning (strict thing &optional count)
  (cpo-text-object-stuff--fwd-beg_or_bwd-end_thing strict thing count t))
(defun cpo-text-object-stuff--backward-thing-end (strict thing &optional count)
  (cpo-text-object-stuff--fwd-beg_or_bwd-end_thing strict thing count nil))


(defun cpo-text-object-stuff--expanded-region-to-bounds-of-thing-at-point
    (strictly-grow sloppy-grow thing &optional region)
  "Returns the new bounds or nil.
If STRICTLY-GROW, only return the bounds if they are strictly greater than the original region.
If SLOPPY-GROW is true, grows the region to include both the original region and the region of the thing at point.
If REGION is not given, uses `region-bounds`, but either way the region must be a single contiguous region.
If no region is active, it will use (point . point)."
  (let* ((orig-regions (or (and region (list region))
                           (if (region-active-p)
                               (region-bounds)
                             (list (cons (point) (point))))))
         (orig-region (and (listp orig-regions)
                           (null (cdr orig-regions))
                           (car orig-regions)))
         (bounds (and orig-region (save-excursion
                                    (cpo-text-object-stuff--set-region orig-region)
                                    (bounds-of-thing-at-point thing)))))
    (if (and orig-region bounds)
        (let* ((left-ok (<= (car bounds) (car orig-region)))
               (left-grow (< (car bounds) (car orig-region)))
               (right-ok (<= (cdr orig-region) (cdr bounds)))
               (right-grow (< (cdr orig-region) (cdr bounds)))
               (nonstrict-ok (and left-ok right-ok))
               (strict-ok (and nonstrict-ok (or left-grow right-grow))))
          (cond ((or (and strictly-grow strict-ok)
                     (and (not strictly-grow) nonstrict-ok))
                 bounds)
                (sloppy-grow
                 (let ((combined (cons (min (car orig-region) (car bounds))
                                       (max (cdr orig-region) (cdr bounds)))))
                   (if strictly-grow
                       (and (cpo-text-object-stuff--bounds-extend orig-region combined) combined)
                     combined)))
                (t nil)))
      nil)))

(defun cpo-text-object-stuff--set-region (bounds)
  "Set the active region to BOUNDS.  BOUNDS must be a single pair."
  (set-mark (car bounds))
  (goto-char (cdr bounds)))

(defun cpo-text-object-stuff--expand-region-to-thing (thing &optional sloppy-grow)
  (interactive)
  ;; TODO - handle trees and count?
  ;; TODO - do I want sloppy expansion?  I definitely do for lines, but I'm not yet certain about other things...  I should revisit this.
  (let ((new-bounds (cpo-text-object-stuff--expanded-region-to-bounds-of-thing-at-point t sloppy-grow thing)))
    (when new-bounds
      (cpo-text-object-stuff--set-region new-bounds))))


(cl-defmacro cpo-text-object-stuff--def-expand-region-to-thing (thing)
  (let ((sym (intern (format "cpo-expand-region-to-%s" thing))))
    `(progn
       (defun ,sym ()
         (interactive)
         (cpo-text-object-stuff--expand-region-to-thing ',thing t)))))

;;;;;;

(cl-defmacro cpo-text-object-stuff--def-move-thing (thing &key (strict t))
  (let ((fwd-beg (intern (format "cpo-forward-%s-beginning" thing)))
        (fwd-end (intern (format "cpo-forward-%s-end" thing)))
        (bwd-end (intern (format "cpo-backward-%s-end" thing)))
        (bwd-beg (intern (format "cpo-backward-%s-beginning" thing)))
        )
    `(progn
       (defun ,fwd-beg (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--forward-thing-beginning ,strict ',thing count))
       (defun ,fwd-end (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--forward-thing-end ,strict ',thing count))
       (defun ,bwd-end (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--backward-thing-end ,strict ',thing count))
       (defun ,bwd-beg (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--backward-thing-beginning ,strict ',thing count))
       (with-eval-after-load 'repeatable-motion
         (repeatable-motion-define-pair ',fwd-beg ',bwd-beg)
         (repeatable-motion-define-pair ',fwd-end ',bwd-end)))))

;;;;;;

(defun cpo-text-object-stuff--transpose-thing-forward-once (thing)
  (let ((bounds-1 (bounds-of-thing-at-point thing)))
    (and bounds-1
         (let ((bounds-2 (save-mark-and-excursion
                           (goto-char (cdr bounds-1))
                           (let ((bounds-at-end (bounds-of-thing-at-point thing)))
                             (if (not (equal bounds-1 bounds-at-end))
                                 bounds-at-end
                               (progn (cpo-text-object-stuff--forward-thing-beginning t thing 1)
                                      (bounds-of-thing-at-point thing)))))))
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
(defun cpo-text-object-stuff--transpose-thing-backward-once (thing)
  (let ((bounds-1 (bounds-of-thing-at-point thing)))
    (and bounds-1
         (let ((bounds-2 (save-mark-and-excursion
                           (goto-char (car bounds-1))
                           (cpo-text-object-stuff--backward-thing-beginning t thing 1)
                           (bounds-of-thing-at-point thing))))
           (when (and bounds-2
                      (<= (cdr bounds-2) (car bounds-1)))
             (goto-char (car bounds-2))
             (cpo-text-object-stuff--transpose-thing-forward-once thing)
             (goto-char (car bounds-2)))))))

(defun cpo-text-object-stuff--transpose-thing-forward (thing &optional count)
  (setq count (or count 1))
  (let ((fwd (< 0 count))
        (count (abs count)))
    (while (< 0 count)
      (if fwd
          (cpo-text-object-stuff--transpose-thing-forward-once thing)
        (cpo-text-object-stuff--transpose-thing-backward-once thing))
      (setq count (- count 1)))))
(defun cpo-text-object-stuff--transpose-thing-backward (thing &optional count)
  (setq count (or count 1))
  (cpo-text-object-stuff--transpose-thing-forward thing (- count)))

(cl-defmacro cpo-text-object-stuff--def-transpose-thing (thing)
  (let ((fwd (intern (format "cpo-transpose-%s-forward" thing)))
        (bwd (intern (format "cpo-transpose-%s-backward" thing))))
    `(progn
       (defun ,fwd (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--transpose-thing-forward ',thing count))
       (defun ,bwd (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--transpose-thing-backward ',thing count)))))

;;;;;

;;;;;

(cpo-text-object-stuff--def-move-thing word :strict nil)
(cpo-text-object-stuff--def-transpose-thing word)
(cpo-text-object-stuff--def-expand-region-to-thing word)
(cpo-text-object-stuff--def-move-thing symbol)
(cpo-text-object-stuff--def-transpose-thing symbol)
(cpo-text-object-stuff--def-expand-region-to-thing symbol)
(cpo-text-object-stuff--def-move-thing sentence)
(cpo-text-object-stuff--def-transpose-thing sentence)
(cpo-text-object-stuff--def-expand-region-to-thing sentence)
(cpo-text-object-stuff--def-move-thing paragraph)
(cpo-text-object-stuff--def-transpose-thing paragraph)
(cpo-text-object-stuff--def-expand-region-to-thing paragraph)

(cpo-text-object-stuff--def-move-thing line)
(cpo-text-object-stuff--def-transpose-thing line)
(cpo-text-object-stuff--def-expand-region-to-thing line)


;;;;;


(defun cpo-text-object-stuff--move-thing-with-bounds-but-no-motion-single (thing fwd-p beg-p)
  "Move until hitting a THING with a bounds func but no motion func.  Extremely naive and inefficient. FWD-P determines whether to move forward or backward.  BEG-P determines whether it goes to the beginning or end of the thing."
  (let* ((orig-point (point))
         (bounds-orig (bounds-of-thing-at-point thing))
         (bounds nil))
    (cond ((and fwd-p
                (not beg-p)
                bounds-orig
                (not (equal orig-point (cdr bounds-orig))))
           (goto-char (cdr bounds-orig)))
          ((and (not fwd-p)
                beg-p
                bounds-orig
                (not (equal orig-point (car bounds-orig))))
           (goto-char (car bounds-orig)))
          (t
           (save-mark-and-excursion
             (when bounds-orig
               (goto-char (if fwd-p (cdr bounds-orig) (car bounds-orig)))
               (setq bounds (bounds-of-thing-at-point thing)))
             (while (and (not (if fwd-p (eobp) (bobp)))
                         (or (not bounds)
                             (equal bounds bounds-orig)))
               (if fwd-p (forward-char 1) (backward-char 1))
               (setq bounds (bounds-of-thing-at-point thing))))
           (when bounds
             (goto-char (if beg-p (car bounds) (cdr bounds))))))))

(defun cpo-text-object-stuff--move-thing-with-bounds-but-no-motion (thing fwd-p beg-p &optional count)
  (let* ((count (or count 1))
         (fwd-p (if (<= 0 count) fwd-p (not fwd-p))))
    (dotimes (i (abs count))
      (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion-single thing fwd-p beg-p))))

(cl-defmacro cpo-text-object-stuff--def-move-thing-with-bounds-but-no-motion (thing)
  (let ((fwd-beg (intern (format "cpo-forward-%s-beginning" thing)))
        (fwd-end (intern (format "cpo-forward-%s-end" thing)))
        (bwd-end (intern (format "cpo-backward-%s-end" thing)))
        (bwd-beg (intern (format "cpo-backward-%s-beginning" thing)))
        )
    `(progn
       (defun ,fwd-beg (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion ',thing t t count))
       (defun ,fwd-end (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion ',thing t nil count))
       (defun ,bwd-end (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion ',thing nil nil count))
       (defun ,bwd-beg (&optional count)
         (interactive "p")
         (cpo-text-object-stuff--move-thing-with-bounds-but-no-motion ',thing nil t count))
       (with-eval-after-load 'repeatable-motion
         (repeatable-motion-define-pair ',fwd-beg ',bwd-beg)
         (repeatable-motion-define-pair ',fwd-end ',bwd-end)))))

(cpo-text-object-stuff--def-move-thing-with-bounds-but-no-motion url)
(cpo-text-object-stuff--def-expand-region-to-thing url)
;; TODO - add keyword args for transpose functions to use different movement func.  But that said, these are so inefficient, they would be extremely frustrating if the things aren't quite close.  These movements really can't be generic and any good.
;;(cpo-text-object-stuff--def-transpose-thing url)

(cpo-text-object-stuff--def-move-thing-with-bounds-but-no-motion email)
(cpo-text-object-stuff--def-expand-region-to-thing email)
;;(cpo-text-object-stuff--def-transpose-thing email)



;;;;;

(defun cpo-text-object-stuff--forward-line-no-newline (&optional count)
  "Movement function for `thing-at-point' for lines that treat the newline character as a separator and not part of the line.
IE going forward go to the end of the line (character before newline), going backward go to the first character of the line (character after newline)."
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (left (abs count)))
    (while (< 0 left)
      (if fwd
          (if (eolp)
              (progn (forward-char 1)
                     (end-of-line))
            (end-of-line))
        (if (bolp)
            (progn (backward-char 1)
                   (beginning-of-line))
          (beginning-of-line)))
      (setq left (- left 1)))))
(defun cpo-text-object-stuff--line-no-newline-bounds-at-point (&optional pt)
  "Get the bounds of the line at point, but not including the newline character."
  (let ((pt (or pt (point))))
    (cons (save-mark-and-excursion (beginning-of-line) (point))
          (save-mark-and-excursion (end-of-line) (point)))))
(put 'cpo-line-no-newline 'bounds-of-thing-at-point 'cpo-text-object-stuff--line-no-newline-bounds-at-point)
(put 'cpo-line-no-newline 'forward-op 'cpo-text-object-stuff--forward-line-no-newline)
(cpo-text-object-stuff--def-move-thing cpo-line-no-newline)



(defun cpo-next-line (&optional arg)
  "`next-line', but with line-move-visual always nil"
  (interactive "p")
  (let ((line-move-visual nil))
    (next-line arg)))
(defun cpo-prev-line (&optional arg)
  "`previous-line', but with line-move-visual always nil"
  (interactive "p")
  (let ((line-move-visual nil))
    (previous-line arg)))
(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-next-line 'cpo-prev-line))


(defun cpo-expand-region-to-fill-lines (&optional include-final-newline)
  "Expand the current region to include the full first and last line.
If INCLUDE-FINAL-NEWLINE, expands to include the newline as well, which makes the function non-idempotent."
  (when (not (region-active-p))
    (set-mark (point)))
  (let ((point-first (< (point) (mark)))
        (goto-end (if include-final-newline
                      (lambda () (goto-char (line-end-position)) (when (not (eobp)) (forward-char 1)))
                    (lambda () (goto-char (line-end-position))))))
    (if point-first (goto-char (line-beginning-position)) (funcall goto-end))
    (exchange-point-and-mark)
    (if (not point-first) (goto-char (line-beginning-position)) (funcall goto-end))
    (exchange-point-and-mark)))

(defun cpo-open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun cpo-open-line-above ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (open-line 1)
    (progn (backward-char 1)
           (newline-and-indent))))


;;;;;


;; I'm used to vi/evil word definition, it's strange to me that emacs' native word definition skips over punctuation and symbols, as well as blank lines.
(defun cpo-text-object-stuff--forward-vi-like-word (&optional count)
  (setq count (or count 1))
  (rx-let ((blank-line-regexp (seq bol (* blank) eol))
           (vi-like-word-regexp-forward/non-blank-line
            (or
             (seq (+ word) eol)
             (+ word)
             (seq (+ (not (any word space))) eol)
             (+ (not (any word space)))
             )))
    (let ((vi-like-word-regexp-forward
           (rx (or vi-like-word-regexp-forward/non-blank-line
                   blank-line-regexp
                   )))
          (vi-like-word-regexp-backward-no-bol
           (rx (or
                (seq blank (+ word))
                (seq blank (+ (not (any word blank))))
                (seq word (+ (not (any word blank "\n"))))
                (seq (not (any word blank)) (+ word))
                )))
          (vi-like-word-regexp-backward-yes-bol
           (rx (or
                (seq blank (+ word))
                (seq blank (+ (not (any word blank))))
                (seq blank
                     vi-like-word-regexp-forward/non-blank-line)
                (seq word (+ (not (any word blank))))
                (seq (not (any word blank)) (+ word))
                (seq bol
                     vi-like-word-regexp-forward/non-blank-line)
                (seq bol (+ (not (any word blank))))
                (seq bol (+ word))
                (seq bol (* blank) eol)
                )))
          (blank-line-regexp (rx blank-line-regexp))
          (point-orig (point))
          (fwd (< 0 count))
          (count (abs count)))
      (while (and (< 0 count)
                  (not (if fwd (eobp) (bobp))))
        (if fwd
            (progn (re-search-forward vi-like-word-regexp-forward
                                      (save-mark-and-excursion (forward-line) (point))))
          ;; Backward requires special handling, because searching a regexp backward isn't a mirror of regexp searching forward...
          ;; This is going to be really inefficient, but I think the easiest way is...
          (progn
            (or
             (let* ((point-orig (point))
                    (searched-1 (ignore-errors
                                  (re-search-backward vi-like-word-regexp-backward-no-bol)))
                    (search-point-1 (point))
                    (RESET (goto-char point-orig))
                    (searched-2 (ignore-errors
                                  (re-search-backward vi-like-word-regexp-backward-yes-bol)))
                    (search-point-2 (point))
                    (RESET (goto-char point-orig))
                    )
               (cond ((and (or (and searched-1 searched-2 (< search-point-1 search-point-2))
                               (and (not searched-1) searched-2))
                           (not (looking-at (rx (seq bol (* blank) eol)))))
                      ;; The BOL case hit first, so go to it
                      (goto-char search-point-2)
                      t)
                     (searched-1
                      ;; The BOL case hit last, go to the other one, but fix it up
                      (goto-char (+ 1 search-point-1))
                      t)
                     (t nil)))
             (beginning-of-buffer))))
        (setq count (- count 1)))
      count)))
(put 'cpo-vi-like-word 'forward-op 'cpo-text-object-stuff--forward-vi-like-word)

(cpo-text-object-stuff--def-move-thing cpo-vi-like-word :strict nil)
(cpo-text-object-stuff--def-transpose-thing cpo-vi-like-word)
(cpo-text-object-stuff--def-expand-region-to-thing cpo-vi-like-word)

(provide 'cpo-text-object-stuff)
