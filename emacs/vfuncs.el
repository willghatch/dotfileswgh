;; -*- lexical-binding: t -*-

(defun file-visiting-buffer-list ()
  (cl-remove-if-not 'buffer-file-name (buffer-list)))

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
  (let ((ignore (lambda () (or (string= "*" (substring (buffer-name) 0 1))
                               ;; I hate this buffer name.  Why doesn't it have stars?
                               (string= "racket-mode-back-end-stderr" (buffer-name))))))
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
  (require 'ffap)
  (find-file-at-point (ffap-file-at-point)))

(defun ido-ffap-no ()
  (interactive)
  (require 'ido)
  (let ((ido-use-filename-at-point nil))
    (ido-mode 1)
    (call-interactively 'ido-find-file)
    ;; Say that all clients are using it.
    ;; This is so that premacs won't close when I close the original buffer I
    ;; opened if I still have other open buffers that I opened later.
    (when (boundp 'server-clients)
      (mapcar (lambda (p) (process-put p 'buffers (cons (current-buffer)
                                                        (process-get p 'buffers))))
              server-clients)
      (setq server-buffer-clients server-clients))))
(defun ido-ffap-yes ()
  (interactive)
  (require 'ido)
  (let ((ido-use-filename-at-point 'guess))
    (ido-mode 1)
    (call-interactively 'ido-find-file)
    (when (boundp 'server-clients)
      (mapcar (lambda (p) (process-put p 'buffers (cons (current-buffer)
                                                        (process-get p 'buffers))))
              server-clients)
      (setq server-buffer-clients server-clients))))
(defun ido-find-file-from-pwd ()
  (interactive)
  (require 'ido)
  (ido-mode 1)
  (let* ((pwd (getenv "PWD"))
         (default-directory (if (string-suffix-p "/" pwd)
                                pwd
                              (concat pwd "/"))))
    (call-interactively 'ido-ffap-no)))
(defun wgh/ido-switch-buffer ()
  (interactive)
  (require 'ido)
  (ido-mode 1)
  (ido-switch-buffer))
(defun chdir (dir)
  (interactive "Dcd:")
  (if (file-directory-p dir)
      (setenv "PWD" dir)
    (message (format "%s is not a directory!" dir))))

(defun ish (cmd) (interactive (list (read-shell-command "$ ")))
  (insert-string (shell-command-to-string cmd)))

(require 'cl-lib)
;; List of buffers that will keep emacs from closing when I use kill-buffer-or-quit-emacs
(setq wgh/kill-block-buffer-list nil)
(defun wgh/add-buffer-to-kill-block-list (buffer)
  (setq wgh/kill-block-buffer-list
        (cons buffer wgh/kill-block-buffer-list)))
(defun do-kill-buffer-or-quit-emacs ()
  (kill-buffer (current-buffer))
  (when (and
         (or (not (buffer-modified-p (get-buffer "*scratch*")))
             (not (get-buffer "*scratch*")))
         (cl-notany 'buffer-file-name (buffer-list))
         (cl-notany 'buffer-live-p wgh/kill-block-buffer-list))
    ;(evil-quit-all)
    (save-buffers-kill-terminal)
    ))
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


(defun get-background-mode ()
  (frame-parameter nil 'background-mode))
(defun set-background-mode (type)
  (if (equal type 'auto)
      (setq frame-background-mode nil)
    (setq frame-background-mode type))
  ;; When setting frame-background-mode, you need to call the function
  ;; frame-set-background-mode for each frame for the update to actually
  ;; take effect.
  (mapc 'frame-set-background-mode (frame-list)))

(defvar current-theme-reapply-var 'dark-theme)
(defun light-theme ()
  (interactive)
  (set-background-mode 'light)
  (setq current-theme-reapply-var 'light-theme)
  (load-theme 'wgh t)
  ;(disable-theme 'wgh)
  )
(defun dark-theme ()
  (interactive)
  (set-background-mode 'dark)
  (setq current-theme-reapply-var 'dark-theme)
  (load-theme 'wgh t)
  ;(disable-theme 'wgh-light)
  )
(defun current-theme-reapply ()
  (interactive
   (funcall current-theme-reapply-var)))
(defun lightdark-update-theme ()
  (interactive)
  (if (string-match "light"
                    (shell-command-to-string "lightdark-status"))
      (light-theme)
    (dark-theme)))
(defun lightdark-update-theme-watch ()
  (file-notify-add-watch (concat
                          ;; Keep this path synced with the lightdark-status script
                          ;;(getenv "XDG_RUNTIME_DIR")
                          "/tmp"
                          "/lightdark")
                         (list 'change)
                         (lambda (event)
                           (lightdark-update-theme)))
  (message "Watching theme file for light/dark changes..."))


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
  (require 'yafolding)
  (call-interactively fold-toggle-wgh-fold-func))
(defun fold-toggle-wgh-all ()
  (interactive)
  (require 'yafolding)
  (call-interactively fold-toggle-wgh-fold-all-func))

;;; checklist helpers
(defun wgh/increment-number-at-end-of-line ()
  (interactive)
  (require 'org)
  (save-excursion
    (end-of-line)
    (org-increase-number-at-point)))

(defun wgh/lisp-indent-region ()
  (interactive)
  (if (< (point) (mark))
      (lisp-indent-region (point) (mark))
    (lisp-indent-region (mark) (point))))

(defun wgh/racket-indent-region ()
  (interactive)
  (require 'racket-mode)
  (let ((mm major-mode)
        (reverse (< (mark) (point))))
    (racket-mode)
    (if reverse
        (indent-region (mark) (point))
      (indent-region (point) (mark)))
    (funcall mm)))

(defun wgh/face-at-point ()
  (interactive)
  ;; This gives a bunch of info, including face info.
  ;; So the face-at-point name is not quite right, but
  ;; I think I'll mostly use it when I want to know a face,
  ;; so I'll remember (or find) the name.
  (what-cursor-position t))

(defun wgh/fzf-repo ()
  ;; This more-or-less implements the cool fuzzy find from all files in the repo feature that Jay used in the demo video he showed of his vim setup.  His only searches files of the same extension, which maybe I should try to add at some point.  But this is probably good enough for now.
  (interactive)
  (require 'fzf)
  (require 'projectile)
  (let ((fzf-rg-prefix "ripgrep --vimgrep --follow --one-file-system --color=always --smart-case")
        (initial-query "")
        (action (lambda (line)
                  (let* ((parts (split-string line ":"))
                         (full-path (car parts))
                         (line-number (cadr parts))
                         (column-number (caddr parts)))
                    (message "line: %s, full-path: %s, line-number: %s" line full-path line-number)
                    (find-file full-path)
                    (goto-line (string-to-number line-number))
                    (move-to-column (1- (string-to-number column-number)))))))
    (let ((fzf/args (format "--ansi --print-query --bind \"change:reload:%s {q} || true\" --query \"%s\"" fzf-rg-prefix initial-query))
          (process-environment
           (cons (concat "FZF_DEFAULT_COMMAND=" fzf-rg-prefix initial-query)
                 process-environment))
          ;; The validator strips extra info before this point.
          (fzf--target-validator #'fzf--pass-through)
          )
      ;;(fzf/start (fzf/resolve-directory) action)
      ;; The package seems not to be as stable as I had hoped -- the author changed a bunch of public things to private and made other changes to how the fzf.el package works.  This is frustrating and annoying.  If this happens again I will fork rather than figuring out what changed.
      (fzf--start (fzf--resolve-directory) action)
      )))

(defun shell-command-to-string-with-stdin (command-string stdin-string)
  (with-temp-buffer
    (insert stdin-string)
    (shell-command-on-region (point-min) (point-max) command-string (current-buffer) t)
    (buffer-string)))

(defun wgh/trivial-if-braces-remove ()
  "Remove braces for “trivial” if statements (single statement) for
languages with C-like syntax.  I prefer to have them personally
(having them leads to more consistent syntax, less editing churn,
lower chance of mistakes), but for working where the style disagrees,
I'm sick of doing this manually."
  (interactive)
  (save-mark-and-excursion
    (when (looking-at-p (regexp-quote "}"))
      ;; Go to the start brace.
      (evil-jump-item))
    (when (not (looking-at-p (regexp-quote "{")))
      (error "Must be called on braces to be removed"))
    (let* ((begin-point (point))
           (end-point (save-mark-and-excursion (evil-jump-item) (point)))
           (begin-on-end-of-line (save-mark-and-excursion
                                   (forward-char)
                                   (looking-at-p "[:blank:]*$")))
           (end-on-end-of-line (save-mark-and-excursion
                                 (evil-jump-item)
                                 (forward-char)
                                 (looking-at-p "[:blank:]*$"))))
      (goto-char end-point)
      (delete-char 1)
      (when end-on-end-of-line
        (call-interactively 'evil-join))
      (goto-char begin-point)
      (delete-char 1)
      (indent-region begin-point end-point))))

(defun wgh/wgrep-start ()
  (interactive)
  (require 'wgrep)
  ;; Functions to use:
  ;; wgrep-finish-edit
  ;; wgrep-save-all-buffers
  (wgrep-change-to-wgrep-mode))

(with-eval-after-load 'gptel
  (require 'gptel-curl))
(defun wgh/gptel-init ()
  (require 'gptel))
;;(autoload 'gptel "gptel" "gptel" t)
;;(autoload 'gptel-send "gptel" "gptel-send -- send text to point or in region" t)

(defun wgh/start-copilot ()
    (interactive)
  (require 'copilot)

  (require 'request)
  (require 'org)
  (require 'markdown-mode)
  (require 'shell-maker)
  (require 'magit)
  (require 'copilot-chat)

  (copilot-mode))


(defun refresh-buffer-from-file ()
  "Alias for revert-buffer since I can never remember its name."
  (interactive)
  (revert-buffer))

(defun quiet-ignore-warnings ()
  "Function that I can find when searching M-x to shut up annoying warnings or errors."
  (interactive)
  (setq warning-minimum-level :error))
(defun quiet-ignore-errors ()
  "Function that I can find when searching M-x to shut up annoying warnings or errors."
  (interactive)
  (setq warning-minimum-level :emergency))

(defun wgh/isearch-forward-for-text-in-region (beg end)
  ;; TODO - none of this is working, but I'll leave it here to look at the next time I try to do this.
  (interactive "r")
  (when (region-active-p)
    (deactivate-mark))
  (let* ((text (buffer-substring-no-properties beg end))
         (set-string-func (lambda () (setq isearch-string text))))
    (setq isearch-string text)
    (isearch-repeat-forward)
    ;;(isearch-resume text nil nil t text 'case-insensitive)
    ;; (unwind-protect
    ;;     (progn
    ;;       (add-hook 'isearch-mode-hook set-string-func -100)
    ;;       (isearch-forward))
    ;;   (remove-hook 'isearch-mode-hook set-string-func))
    ;;(setq isearch-string text)
    ;;(isearch-forward)
    ))

(provide 'vfuncs)
