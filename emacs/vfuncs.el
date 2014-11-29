
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


(defun myslime () (interactive)
  "pulls in slime (in elpa) and my config"
  (load-library "wghconf-slime"))

(defun myac () (interactive)
  "pulls in auto-complete package (in elpa) with my config"
  (load-library "wghconf-auto-complete"))

(evil-define-command wevil-quit ()
  "Close buffer, primarily"
  (kill-buffer))
(evil-define-command wevil-save-and-quit ()
  "Close buffer, primarily"
  (save-buffer)
  (kill-buffer))

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

