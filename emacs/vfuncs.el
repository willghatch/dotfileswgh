
(defun whitespace ()
  (interactive)
  ;; These two highlighters kill the formatting of
  ;; list-faces-display... and I don't know why
  (hc-toggle-highlight-tabs)
  (hc-toggle-highlight-trailing-whitespace))

(defun file-visiting-buffer-list ()
  (remove-if-not 'buffer-file-name (buffer-list)))

(defun np-buffer-conditional (next-buffer-func ignore-func advance-on-failure-func)
  "Cycle buffers until one isn't ignored by the ignore-func.  If in reaches the starting
buffer, it will call the next-buffer-func once more if advance-on-failure-p."
  (let ((cbuf (buffer-name)))
    (funcall next-buffer-func)
    (while (and (not (string= cbuf (buffer-name)))
                (funcall ignore-func))
      (funcall next-buffer-func))
    (when (and (string= cbuf (buffer-name)) advance-on-failure-func)
      (funcall advance-on-failure-func))))

(defun np-buffer-no-star (next-buffer-func advance-on-failure-func)
  (let ((ignore (lambda () (string= "*" (substring (buffer-name) 0 1)))))
    (np-buffer-conditional next-buffer-func ignore advance-on-failure-func)))
(defun next-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'next-buffer 'next-buffer))
(defun prev-buffer-no-star ()
  (interactive)
  (np-buffer-no-star 'previous-buffer 'previous-buffer))

(defun np-dirty-buffer (next-buffer-func advance-on-failure-func)
  (let ((ignore (lambda () (not (buffer-modified-p)))))
    (np-buffer-conditional next-buffer-func ignore advance-on-failure-func)))
(defun prev-dirty-buffer-no-star ()
  (interactive)
  (np-dirty-buffer (lambda () (np-buffer-no-star 'previous-buffer nil))
                   'prev-buffer-no-star))
(defun next-dirty-buffer-no-star ()
  (interactive)
  (np-dirty-buffer (lambda () (np-buffer-no-star 'next-buffer nil))
                   'next-buffer-no-star))

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
(defun ido-find-file-from-pwd ()
  (interactive)
  (let* ((pwd (getenv "PWD"))
         (default-directory (if (string-suffix-p "/" pwd)
                                pwd
                              (concat pwd "/"))))
    (call-interactively 'ido-ffap-no)))
(defun chdir (dir)
  (interactive "Dcd:")
  (if (file-directory-p dir)
      (setenv "PWD" dir)
    (message (format "%s is not a directory!" dir))))

(defun ish (cmd) (interactive (list (read-shell-command "$ ")))
  (insert-string (shell-command-to-string cmd)))

(require 'cl-lib)
(defun do-kill-buffer-or-quit-emacs ()
  (kill-buffer (current-buffer))
  (when (and
         (or (not (buffer-modified-p (get-buffer "*scratch*")))
             (not (get-buffer "*scratch*")))
         (cl-notany 'buffer-file-name (buffer-list)))
    (evil-quit-all)))
(defun kill-buffer-or-quit-emacs ()
  "Kill buffer and, if there are no more file buffers and scratch is unmodified,
quit emacs."
  (interactive)
  (if (and (fboundp 'server-edit) server-buffer-clients)
      ;; Run server-edit to close buffer if I'm in a server and clients are using it.
      ;; If server-edit returns non-nil, it means that there are no more
      ;; server-buffers, so just kill normally.
      (when (server-edit) (do-kill-buffer-or-quit-emacs))
    (do-kill-buffer-or-quit-emacs)))

(defun kill-buffer-or-quit-emacs-ignore-dirty ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer-or-quit-emacs))

(defun save-and-kill-buffer-and-maybe-quit-emacs ()
  "Save buffer and kill it using kill-buffer-or-quit-emacs"
  (interactive)
  (save-buffer)
  (kill-buffer-or-quit-emacs))

(defmacro setq-not (symbol)
  `(setq ,symbol (not ,symbol)))

(defun toggle-wrap-scan ()
  (interactive)
  (setq-not evil-search-wrap))

(defun reformat-file ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (call-interactively 'indent-region)
    (mark-whole-buffer)
    (call-interactively 'delete-trailing-whitespace)))

(defun file->string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun file->lines (file)
  (split-string (file->string file) "\n"))

(defun pscroll-up-line (&optional arg)
  (interactive "p")
  (scroll-down-line arg)
  (let ((wline (+ (window-text-height) (line-number-at-pos (window-start)))))
    (while (and (> (window-text-height) scroll-margin)
                (> scroll-margin (- wline (line-number-at-pos))))
      (message (format "wline: %d, cur: %d" wline (line-number-at-pos)))
      (previous-line))))
(defun pscroll-down-line (&optional arg)
  (interactive "p")
  (scroll-up-line arg)
  (let ((wline (line-number-at-pos (window-start))))
    (while (and (> (window-text-height) scroll-margin)
                (> scroll-margin (- (line-number-at-pos) wline)))
      (next-line))))
(defun pscroll-down-half (&optional arg)
  (interactive "p")
  (pscroll-down-line (/ (window-text-height) 2)))
(defun pscroll-up-half (&optional arg)
  (interactive "p")
  (pscroll-up-line (/ (window-text-height) 2)))
(defun pscroll-up-full (&optional arg)
  (interactive "p")
  (pscroll-up-line (window-text-height)))
(defun pscroll-down-full (&optional arg)
  (interactive "p")
  (pscroll-down-line (window-text-height)))

(defun ansi-color-buffer ()
  (interactive)
  (ansi-format-decode (point-min) (point-max))
  (backspace-overstrike-decode (point-min) (point-max)))

(defun light-theme ()
  (interactive)
  (load-theme 'tango)
  (disable-theme 'wgh))
(defun dark-theme ()
  (interactive)
  (load-theme 'wgh)
  (disable-theme 'tango))

(provide 'vfuncs)
