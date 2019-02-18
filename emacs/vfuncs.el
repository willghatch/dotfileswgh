

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

(defun ffap/no-confirm ()
  (interactive)
  (find-file-at-point (ffap-file-at-point)))

(defun ido-ffap-no ()
  (interactive)
  (let ((ido-use-filename-at-point nil))
    (call-interactively 'ido-find-file)
    ;; Say that all clients are using it.
    ;; This is so that premacs won't close when I close the original buffer I
    ;; opened if I still have other open buffers that I opened later.
    (mapcar (lambda (p) (process-put p 'buffers (cons (current-buffer)
                                                      (process-get p 'buffers))))
            server-clients)
    (setq server-buffer-clients server-clients)))
(defun ido-ffap-yes ()
  (interactive)
  (let ((ido-use-filename-at-point 'guess))
    (call-interactively 'ido-find-file)
    (mapcar (lambda (p) (process-put p 'buffers (cons (current-buffer)
                                                      (process-get p 'buffers))))
            server-clients)
    (setq server-buffer-clients server-clients)))
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

(defun copy-buffer ()
  (interactive)
  (copy-region-as-kill 1 (+ 1 (buffer-size))))

(defun file->string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun file->lines (file)
  (split-string (file->string file) "\n"))

(defun pscroll-up-line (&optional arg)
  (interactive "p")
  (scroll-down-line arg)
  (let ((wline (+ (window-text-height) (line-number-at-pos (window-start))))
        (margin (max scroll-margin smooth-scroll-margin)))
    ;; there is some fighting about setting these...
    (setq scroll-margin margin)
    (setq smooth-scroll-margin margin)
    (while (and (> (window-text-height) (* 2 margin))
                (> margin (- wline (line-number-at-pos))))
      (previous-line))))
(defun pscroll-down-line (&optional arg)
  (interactive "p")
  (scroll-up-line arg)
  (let ((wline (line-number-at-pos (window-start)))
        (margin (max scroll-margin smooth-scroll-margin)))
    ;; there is some fighting about setting these...
    (setq scroll-margin margin)
    (setq smooth-scroll-margin margin)
    (while (and (> (window-text-height) (* 2 margin))
                (> margin (- (line-number-at-pos) wline)))
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

(require 'repeatable-motion)
(repeatable-motion-define-pair 'pscroll-down-line 'pscroll-up-line)
(repeatable-motion-define-pair 'pscroll-down-half 'pscroll-up-half)
(repeatable-motion-define-pair 'pscroll-down-full 'pscroll-up-full)

(defun ansi-color-buffer ()
  (interactive)
  (ansi-format-decode (point-min) (point-max))
  (backspace-overstrike-decode (point-min) (point-max)))

(defun racket-launch-macro-stepper ()
  (interactive)
  (call-process "raco" nil 0 nil "macro-step" (buffer-file-name)))

(defvar current-theme-reapply-var 'dark-theme)
(defun light-theme ()
  (interactive)
  (setq current-theme-reapply-var 'light-theme)
  (load-theme 'tango t)
  (disable-theme 'wgh))
(defun dark-theme ()
  (interactive)
  (setq current-theme-reapply-var 'dark-theme)
  (load-theme 'wgh t)
  (disable-theme 'tango))
(defun current-theme-reapply ()
  (interactive
   (funcall current-theme-reapply-var)))

(defun quick-in-block ()
  (interactive)
  (when (not (evil-visual-state-p))
    (evil-visual-state))
  (call-interactively 'evil-textobj-anyblock-inner-block))
(defun quick-a-block ()
  (interactive)
  (when (not (evil-visual-state-p))
    (evil-visual-state))
  (call-interactively 'evil-textobj-anyblock-a-block))

(defun isearch-abort-abort-gosh-darn-it ()
  "If the start of a search query matches but the end doesn't, a single abort will
just backspace to the part that does match, which is never what I want when I say
abort."
  (interactive)
  (isearch-abort)
  (isearch-abort))

;;; Folding
(setq-default fold-toggle-wgh-fold-func 'yafolding-toggle-element)
(make-variable-buffer-local 'fold-toggle-wgh-fold-func)
(setq-default fold-toggle-wgh-fold-all-func 'yafolding-toggle-all)
(make-variable-buffer-local 'fold-toggle-wgh-fold-all-func)
(defun fold-toggle-wgh ()
  (interactive)
  (call-interactively fold-toggle-wgh-fold-func))
(defun fold-toggle-wgh-all ()
  (interactive)
  (call-interactively fold-toggle-wgh-fold-all-func))

(provide 'vfuncs)
