
(defun whitespace ()
  (interactive)
  ;; These two highlighters kill the formatting of
  ;; list-faces-display... and I don't know why
  (hc-toggle-highlight-tabs)
  (hc-toggle-highlight-trailing-whitespace))

(defun np-buffer-no-star (next-buffer-func)
  "Cycle buffers ignoring ** buffers.  If it circles back to the first buffer
it calls the next function one more time."
  (let ((cbuf (buffer-name)))
    (funcall next-buffer-func)
    (while (and (string= "*" (substring (buffer-name) 0 1))
                (not (string= cbuf (buffer-name))))
      (funcall next-buffer-func))
    (when (string= cbuf (buffer-name))
      (funcall next-buffer-func))))
(defun next-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'next-buffer))
(defun prev-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'previous-buffer))


(defun backward-symbol (n)
  "this doesn't work right..."
  (interactive "p")
  (forward-word (- n)))

(defalias 'nop 'ignore) ; returns nil

(defun ido-ffap-no ()
  (interactive)
  (let ((ido-use-filename-at-point nil))
    (call-interactively 'ido-find-file)))
(defun ido-ffap-yes ()
  (interactive)
  (let ((ido-use-filename-at-point 'guess))
    (call-interactively 'ido-find-file)))

(defun ish (cmd) (interactive (list (read-shell-command "$ ")))
  (insert-string (shell-command-to-string cmd)))

(require 'cl-lib)
(defun kill-buffer-or-quit-emacs ()
  "Kill buffer and, if there are no more file buffers and scratch is unmodified,
quit emacs."
  (interactive)
  (kill-buffer (current-buffer))
  (when (and
       (or (not (buffer-modified-p (get-buffer "*scratch*")))
           (not (get-buffer "*scratch*")))
       (cl-notany 'buffer-file-name (buffer-list)))
      (evil-quit-all)))

(defun save-and-kill-buffer-and-maybe-quit-emacs ()
  "Save buffer and kill it using kill-buffer-or-quit-emacs"
  (interactive)
  (save-buffer)
  (kill-buffer-or-quit-emacs))
