;; TODO - find homes for these

;; State for repeating isearch -- my key to start isearch sets this so that the repeat keys go the right direction.
(setq wgh/isearch-repeat-forward-p t)
;; State for isearch repetition -- values 'beginning or 'end.
;; TODO - I would like a system for writing an escape in the search for where the cursor should go.
(setq wgh/isearch-go-part 'beginning)

(defun wgh/isearch-start-forward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p t)
  (funcall 'isearch-forward))
(defun wgh/isearch-start-backward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p nil)
  (funcall 'isearch-backward))

;; TODO - this has an issue when changing direction.  The first time it repeats backward after going forward, it skips one.
(defun wgh/isearch-repeat-forward (&optional count)
  (interactive "p")
  (let* ((count-fwd (<= 0 (or count 1)))
         (count-num (abs (or count 1)))
         (fwd (not (xor count-fwd wgh/isearch-repeat-forward-p)))
         (repeat-func (if fwd #'isearch-repeat-forward #'isearch-repeat-backward))
         (moved (tree-walk--motion-moved (lambda () (funcall repeat-func count-num)))))
    (cond ((not moved) nil)
          ((and (equal wgh/isearch-go-part 'beginning)
                (< isearch-other-end (point)))
           (goto-char isearch-other-end))
          ((and (equal wgh/isearch-go-part 'end)
                (>= isearch-other-end (point)))
           (goto-char isearch-other-end)))))
(defun wgh/isearch-repeat-backward (&optional count)
  (interactive "p")
  (wgh/isearch-repeat-forward (- (or count 1))))



;; isearch-wrap-pause can be t (default) to signal an error, then actually wrap the next time, 'no to wrap immediately but flash, 'no-ding to wrap immediately but not flash, or nil to disallow wrapping entirely.
(setq isearch-wrap-pause 'no)



(cl-defun wgh/forward-line-keep-column/qd (&optional count)
  ;; Well, not as good as evil mode... maybe I'll do this properly later?
  ;; TODO - look at goal-column variable
  (interactive "p")
  (let ((col (current-column)))
    (forward-line (or count 1))
    (move-to-column col)))
(cl-defun wgh/backward-line-keep-column/qd (&optional count)
  (interactive "p")
  (wgh/forward-line-keep-column/qd (- (or count 1))))


(defun wgh/next-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (next-line arg)))
(defun wgh/prev-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (previous-line arg)))
(repeatable-motion-define-pair 'wgh/next-line 'wgh/prev-line)




(defun goto-column (&optional n)
  (interactive "P")
  (move-to-column (or n 0)))
(defun goto-column/default-end (&optional n)
  (interactive "P")
  (if n (move-to-column n) (end-of-line)))



(defmacro with-evil (func)
  `(lambda ()
     (interactive)
     (require 'evil)
     (call-interactively ,func)))

(defun wgh/evil-goto-marker ()
  (interactive)
  (require 'evil)
  (call-interactively 'evil-goto-mark))
(defun wgh/evil-goto-marker-line ()
  (interactive)
  (require 'evil)
  (call-interactively 'evil-goto-mark-line))



(progn
  (repeatable-motion-define-pair 'sptw-forward-inorder-traversal 'sptw-backward-inorder-traversal)
  (repeatable-motion-define-pair 'sptw-forward-sibling-beginning 'sptw-backward-sibling-beginning)
  (repeatable-motion-define-pair 'sptw-forward-sibling-end 'sptw-backward-sibling-end)
  (repeatable-motion-define-pair 'sptw-up-parent-beginning 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-up-parent-end 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-down-last-child-end 'sptw-up-parent-beginning)
  ;;(repeatable-motion-define-pair 'sptw-forward-sexp-in-supersexp 'sptw-backward-sexp-in-supersexp)
  )






(defun wgh/expand-region-to-fill-lines (&optional include-final-newline)
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




;; call-keymap is from https://stackoverflow.com/questions/24914202/elisp-call-keymap-from-code
(defun call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP."
  ;; Note: MAP must be a symbol so we can trick `describe-bindings' into giving
  ;; us a nice help text.
  (let* ((overriding-local-map `(keymap (,map . ,map)))
         (help-form `(describe-bindings ,(vector map)))
         (key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (if (functionp cmd)
        (call-interactively cmd)
      (user-error "%s is undefined" key))))



(defun vilish/goto-line/beginning (n)
  (interactive "p")
  (if (= n 0) (goto-line 1) (goto-line n)))
(defun vilish/goto-line/end (n)
  (interactive "P")
  (cond ((null n) (goto-line (line-number-at-pos (point-max))))
        ((numberp n) (goto-line n))
        ((consp n) (goto-line (car n)))
        (t (error))))



;; TODO - move these
(defun vilish-open-line-below ()
  ;; TODO - evil takes a numeric argument, and when you are done entering text it copies that line N times.  I never use that feature though, so... maybe I don't care.
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (end-of-line)
                                    ;;(open-line 1)
                                    ;;(forward-line)
                                    (newline-and-indent)
                                    )))
(defun vilish-open-line-above ()
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (beginning-of-line)
                                    (if (bobp)
                                        (open-line 1)
                                      (progn (backward-char 1)
                                             (newline-and-indent))))))



(defun wgh/forward-char/no-line-wrap (&optional count)
  (interactive "p")
  (let ((fwd (< 0 count))
        (count (abs count))
        (keep-going t))
    (while (and keep-going (< 0 count))
      (if fwd
          (if (eolp) (setq keep-going nil) (forward-char 1))
        (if (bolp) (setq keep-going nil) (backward-char 1)))
      (setq count (- count 1)))))
(defun wgh/backward-char/no-line-wrap (&optional count)
  (interactive "p")
  (wgh/forward-char/no-line-wrap (- count)))
(repeatable-motion-define-pair 'wgh/forward-char/no-line-wrap
                               'wgh/backward-char/no-line-wrap)


(defun join-line/default-forward (arg)
  (interactive "P")
  (join-line (not arg)))

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


(defun delete-char-backward (&optional n)
  (interactive "p")
  (delete-char (- n)))

