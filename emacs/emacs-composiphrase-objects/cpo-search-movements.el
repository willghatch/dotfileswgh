;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; isearch-stuff

;; State for repeating isearch -- my key to start isearch sets this so that the repeat keys go the right direction.
(setq wgh/isearch-repeat-forward-p t)
;; There is an issue where the first repeat backward to beginning after going forward or the first repeat to end forward after going backward skips one.  Keep track of some state to hack around it.
(setq wgh/isearch-last-direction-absolute nil)
;; State for isearch repetition -- values 'beginning or 'end.
;; TODO - I would like a system for writing an escape in the search for where the cursor should go.
(setq wgh/isearch-go-part 'beginning)

(defun wgh/isearch-start-forward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p t)
  (setq wgh/isearch-last-direction-absolute 'forward)
  (funcall 'isearch-forward))
(defun wgh/isearch-start-backward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p nil)
  (setq wgh/isearch-last-direction-absolute 'backward)
  (funcall 'isearch-backward))

;; TODO - this has an issue when changing direction.  The first time it repeats backward after going forward, it skips one.
(defun wgh/isearch-repeat-forward (&optional count)
  (interactive "p")
  (require 'tree-walk) ;; for tree-walk--motion-moved
  (let* ((count-fwd (<= 0 (or count 1)))
         (count-num (abs (or count 1)))
         (fwd (not (xor count-fwd wgh/isearch-repeat-forward-p)))
         (repeat-func (if fwd #'isearch-repeat-forward #'isearch-repeat-backward))
         ;; count-adjusted to hack around skipping in these cases...
         (count-adjusted (cond ((and (not fwd)
                                     (equal wgh/isearch-go-part 'beginning)
                                     (equal wgh/isearch-last-direction-absolute
                                            'forward))
                                (- count-num 1))
                               ((and fwd
                                     (equal wgh/isearch-go-part 'end)
                                     (equal wgh/isearch-last-direction-absolute
                                            'backward))
                                (- count-num 1))
                               (t count-num)))

         (moved (tree-walk--motion-moved
                 (lambda () (funcall repeat-func count-adjusted)))))
    (setq wgh/isearch-last-direction-absolute (if fwd 'forward 'backward))
    (cond ((not moved) nil)
          ((and (equal wgh/isearch-go-part 'beginning)
                (< isearch-other-end (point)))
           (goto-char isearch-other-end)
           )
          ((and (equal wgh/isearch-go-part 'end)
                (>= isearch-other-end (point)))
           (goto-char isearch-other-end)
           ))))
(defun wgh/isearch-repeat-backward (&optional count)
  (interactive "p")
  (wgh/isearch-repeat-forward (- (or count 1))))


(defun my-isearch-bor-exit ()
  "Ensure point is at beginning of isearch result and exit."
  ;; Copied from https://emacs.stackexchange.com/questions/74339/how-to-leave-cursor-at-beginning-of-searched-text-in-isearch
  (interactive)
  (when (< isearch-other-end (point))
    (goto-char isearch-other-end))
  (call-interactively 'isearch-exit))

(defun my-isearch-eor-exit ()
  "Ensure point is at end of isearch result and exit."
  ;; Copied from https://emacs.stackexchange.com/questions/74339/how-to-leave-cursor-at-beginning-of-searched-text-in-isearch
  (interactive)
  (when (>= isearch-other-end (point))
    (goto-char isearch-other-end))
  (call-interactively 'isearch-exit))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; find-character

(setq -wgh/find-char-in-line/impl-last-char nil)
(setq -wgh/find-char-in-line/impl-last-style 'reverse-emacs)
(defun wgh/find-char-in-line/impl (char count style)
  "STYLE is 'emacs, 'reverse-emacs, 'beginning, or 'end.
Emacs style is to go to the end of the character when going forward, and
to the beginning of the character when going backward.  Reverse-emacs style
is the opposite."
  (let ((r (regexp-quote (if (stringp char) char (string char))))
        (fwd (< 0 count))
        (count (abs count))
        (keep-going t))
    (when (and fwd
               (or (equal style 'beginning)
                   (equal style 'reverse-emacs))
               (looking-at r))
      (forward-char 1))
    (when (and (not fwd)
               (or (equal style 'end)
                   (equal style 'reverse-emacs))
               (save-mark-and-excursion
                 (backward-char 1)
                 (looking-at r)))
      (backward-char 1))
    (while (and keep-going (< 0 count))
      (setq count (- count 1))
      (let ((p (point))
            (l (line-number-at-pos (point))))
        (ignore-errors
          (if fwd
              (re-search-forward r)
            (re-search-backward r)))
        (when (not (equal l (line-number-at-pos (point))))
          (setq keep-going nil)
          (goto-char p))))
    (when (and fwd
               (or (equal style 'beginning)
                   (equal style 'reverse-emacs)))
      (backward-char 1))
    (when (and (not fwd)
               (or (equal style 'end)
                   (equal style 'reverse-emacs))
               (forward-char 1))
      (backward-char 1))))
(defun wgh/find-char-in-line-forward-repeat (&optional n)
  (interactive "p")
  (wgh/find-char-in-line/impl -wgh/find-char-in-line/impl-last-char
                              n
                              -wgh/find-char-in-line/impl-last-style))
(defun wgh/find-char-in-line-backward-repeat (&optional n)
  (interactive "p")
  (wgh/find-char-in-line-forward-repeat (- n)))

;; The evil-mode movements I'm used to are basically like emacs style and reverse-emacs style of what I've defined... so maybe I'll use them...
(defun wgh/find-char-beginning-in-line-forward (&optional n)
  (interactive "p")
  (let ((c (read-char "char to find:")))
    (setq -wgh/find-char-in-line/impl-last-char c)
    (setq -wgh/find-char-in-line/impl-last-style 'beginning)
    (wgh/find-char-in-line/impl c n 'beginning)))
(defun wgh/find-char-beginning-in-line-backward (&optional n)
  (interactive "p")
  (wgh/find-char-beginning-in-line-forward (- n)))
(repeatable-motion-define 'wgh/find-char-beginning-in-line-forward 'wgh/find-char-in-line-backward-repeat :repeat 'wgh/find-char-in-line-forward-repeat)
(repeatable-motion-define 'wgh/find-char-beginning-in-line-backward 'wgh/find-char-in-line-forward-repeat :repeat 'wgh/find-char-in-line-backward-repeat)
(defun wgh/find-char-end-in-line-forward (&optional n)
  (interactive "p")
  (let ((c (read-char "char to find:")))
    (setq -wgh/find-char-in-line/impl-last-char c)
    (setq -wgh/find-char-in-line/impl-last-style 'end)
    (wgh/find-char-in-line/impl c n 'end)))
(defun wgh/find-char-end-in-line-backward (&optional n)
  (interactive "p")
  (wgh/find-char-end-in-line-forward (- n)))
(repeatable-motion-define 'wgh/find-char-end-in-line-forward 'wgh/find-char-in-line-backward-repeat :repeat 'wgh/find-char-in-line-forward-repeat)
(repeatable-motion-define 'wgh/find-char-end-in-line-backward 'wgh/find-char-in-line-forward-repeat :repeat 'wgh/find-char-in-line-backward-repeat)



(provide 'cpo-search-movements)
