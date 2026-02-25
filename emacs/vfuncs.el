;; -*- lexical-binding: t -*-

;;; Miscellaneous helpers and utilities that I use, or did at some point, or thought I would, or something.

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

(defun wgh/find-file-no-ffap ()
  (interactive)
  (require 'minad-stack-conf)
  (nobreak (wgh/init-minad))
  (let ((file-name-at-point-functions nil))
    (call-interactively 'find-file)))

;; TODO - I have now stopped using ido, I should probably just delete these.
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
    ;;(evil-quit-all)
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

;; TODO - I like having config in def.el and functions here, but this is sort of a mix and I want them together...
;; isearch-wrap-pause can be t (default) to signal an error, then actually wrap the next time, 'no to wrap immediately but flash, 'no-ding to wrap immediately but not flash, or nil to disallow wrapping entirely.
(setq isearch-wrap-pause 'no)
(defun wgh/toggle-search-wrap ()
  (interactive)
  (if (equal isearch-wrap-pause nil)
      (setq isearch-wrap-pause 'no)
    (setq isearch-wrap-pause nil)))

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
  ;;(disable-theme 'wgh)
  )
(defun dark-theme ()
  (interactive)
  (set-background-mode 'dark)
  (setq current-theme-reapply-var 'dark-theme)
  (load-theme 'wgh t)
  ;;(disable-theme 'wgh-light)
  )
(defun current-theme-reapply ()
  (interactive
   (funcall current-theme-reapply-var)))
(defun lightdark-update-theme ()
  (interactive)
  (if (string-prefix-p "light"
                       (shell-command-to-string "lightdark-status"))
      (light-theme)
    (dark-theme)))
(defun lightdark-update-theme-watch ()
  ;; Keep this path synced with the lightdark-status script
  (let ((lightdark-dir (or (getenv "LIGHTDARK_DIR") "/tmp/lightdark")))
    (when (not (file-directory-p lightdark-dir))
      (make-directory lightdark-dir t))
    (file-notify-add-watch (concat
                            lightdark-dir
                            "/lightdark")
                           (list 'change)
                           (lambda (event)
                             (lightdark-update-theme))))
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


(defun wgh/send-line-to-bottom-of-buffer ()
  (interactive)
  (save-excursion
    (let* ((beg (line-beginning-position))
           (end (+ 1 (line-end-position)))
           (text (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (goto-char (point-max))
      (when (not (bolp))
        (insert "\n"))
      (insert text))))

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
  (let* ((ripgrep-command (if (executable-find "ripgrep")
                              "ripgrep"
                            "rg"))
         (fzf-rg-prefix (concat ripgrep-command " --vimgrep --follow --one-file-system --color=always --smart-case"))
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
  (nobreak
   (require 'copilot)

   (define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
   (define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)
   (define-key copilot-completion-map (kbd "C-i") 'copilot-accept-completion)
   (define-key copilot-completion-map (kbd "C-m") 'copilot-hydra/body)

   (require 'request)
   (require 'org)
   (require 'markdown-mode)
   ;;(require 'shell-maker)
   (require 'magit)
   (require 'copilot-chat)

   (copilot-mode)))


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

(defun debug-message-and-ret (msg val)
  (message "debug-message-and-ret: %s --- %s" msg val)
  val)

(defun wgh/ripgrep ()
  (interactive)
  (require 'rg)
  (call-interactively 'rg))





(defun goto-column (&optional n)
  (interactive "P")
  (move-to-column (or n 0)))
(defun goto-column/default-end (&optional n)
  (interactive "P")
  (if n (move-to-column n) (end-of-line)))




;;;; TODO - stuff I don't use.  Do I want to use it, or delete it?

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

;;;;;;;;;;;;;

(defun wgh/set-terminal-cursor-style (style &optional blink)
  "Set terminal cursor to STYLE and optionally BLINK.
STYLE must be one of 'block, 'underline, or 'bar.

This works in most graphical terminals, I think, as DECSCUSR CSI code.
"
  ;; TODO - maybe I want to use this with estate-mode.  If I do, I need hooks on state change or buffer change to set the current cursor style.  I also would need to set it on exit to hopefully restore the previous state.  But do I really even care about this?  Unclear.
  (let ((style-number (cond
                       ((and (eq style 'block) blink) 1)
                       ((and (eq style 'block) (not blink)) 2)
                       ((and (eq style 'underline) blink) 3)
                       ((and (eq style 'underline) (not blink)) 4)
                       ((and (eq style 'bar) blink) 5)
                       ((and (eq style 'bar) (not blink)) 6)
                       ;; Default to block non-blinking, I guess?
                       (t 2))))
    (send-string-to-terminal
     (concat "\e[" (number-to-string style-number) " q"))))

(autoload 'magit-status "magit" "magit-status" t)
(defalias 'mgs 'magit-status)

(defun whisper-record-and-transcribe ()
  "Record audio and transcribe it using the whisper-record-and-transcribe script.
Wait for the user to press Enter or for the script to timeout, then insert
the transcription at point."
  (interactive)
  (let* ((output-buffer (generate-new-buffer " *whisper-record-and-transcribe*"))
         (process (start-process "whisper-record-and-transcribe"
                                output-buffer
                                "whisper-record-and-transcribe"
                                "--quiet")))
    ;; Set a no-op sentinel to prevent "Process finished" messages
    (set-process-sentinel process 'ignore)
    (message "Recording... Press ENTER to stop.")
    (read-event)  ; Wait for user to press a key
    (process-send-string process "\n")  ; Send Enter to the process
    ;; Wait for the process to finish
    (while (process-live-p process)
      (accept-process-output process 0.1))
    ;; Get the output and insert it
    (let ((output (with-current-buffer output-buffer
                    (buffer-string))))
      (kill-buffer output-buffer)
      (insert output))))

(defun wgh/cpo-copy-string (str register)
  "Store STR to REGISTER following cpo-copy conventions.
Also syncs to kill ring if REGISTER matches cpo-copy-sync-with-kill-ring-register."
  (let ((reg (if (functionp register) (funcall register) register)))
    (set-register reg str)
    (when (equal reg cpo-copy-sync-with-kill-ring-register)
      (kill-new str))
    (message "Copied: %s" str)))

(defun wgh/file-name-full-path ()
  "Return the full absolute path of the current buffer's file, or signal an error."
  (or (buffer-file-name) (user-error "Buffer has no file")))

(defun wgh/file-name-basename ()
  "Return the basename of the current buffer's file, or signal an error."
  (file-name-nondirectory (wgh/file-name-full-path)))

(defun wgh/file-name-git-relative ()
  "Return the path of the current buffer's file relative to its git root, or signal an error."
  (let* ((path (wgh/file-name-full-path))
         (git-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel"))))
    (file-relative-name path git-root)))

;;; Agent working directory helpers

(defun wgh/agent-working-dir-base ()
  "Return the base path for agent-working-directories.
Uses GIT_COMMON_DIR/agent-files/work/ if in a git repo, or agent-files/work/
relative to `default-directory' otherwise.
When git-common-dir falls inside .git/modules/... or .git/worktrees/...,
walks back up to the top-level .git directory."
  (let* ((git-common-dir (string-trim
                          (shell-command-to-string
                           "git rev-parse --git-common-dir 2>/dev/null")))
         (git-dir
          (when (and (not (string-empty-p git-common-dir))
                     (not (string-prefix-p "fatal:" git-common-dir)))
            (let ((abs-dir (expand-file-name git-common-dir)))
              ;; If inside .git/modules/... or .git/worktrees/..., strip back
              ;; to the .git directory itself.
              (if (string-match "\\(.*\\.git\\)/\\(?:modules\\|worktrees\\)" abs-dir)
                  (match-string 1 abs-dir)
                abs-dir)))))
    (if git-dir
        (concat git-dir "/agent-files/work/")
      (concat (expand-file-name default-directory) "agent-files/work/"))))

(defun wgh/agent-working-dir-fzf-open ()
  "Open a file in the agent-working-directories base path using fzf."
  (interactive)
  (require 'fzf)
  (let ((base (wgh/agent-working-dir-base)))
    (unless (file-directory-p base)
      (user-error "No agent working directories at: %s" base))
    (fzf-with-command "find . -type f | sort -r"
                      (lambda (x) (find-file (expand-file-name x base)))
                      base)))

(defun wgh/fzf-all-files ()
  "Open a file from the current git repo using fzf, including untracked files.
Lists both tracked and untracked (but not ignored) files."
  (interactive)
  (require 'fzf)
  (let* ((path (locate-dominating-file (file-truename default-directory) ".git")))
    (if path
        (fzf-with-command "{ git ls-files; git ls-files --others --exclude-standard; } | sort -u"
                          #'fzf--action-find-file
                          path)
      (user-error "Not inside a Git repository"))))

(defun wgh/fzf-untracked-files ()
  "Open an untracked file from the current git repo using fzf.
Lists only untracked (but not ignored) files."
  (interactive)
  (require 'fzf)
  (let* ((path (locate-dominating-file (file-truename default-directory) ".git")))
    (if path
        (fzf-with-command "git ls-files --others --exclude-standard"
                          #'fzf--action-find-file
                          path)
      (user-error "Not inside a Git repository"))))

(defun wgh/git-root-superproject ()
  "Find the outermost git superproject working tree.
Returns the root directory path, or nil if not inside a git repo.
Note: `git rev-parse --show-superproject-working-tree' returns empty
with exit code 0 when already at the root (not inside a submodule),
so we must check its output rather than relying on exit codes."
  (let* ((super (string-trim
                 (shell-command-to-string
                  "git rev-parse --show-superproject-working-tree 2>/dev/null")))
         (root (if (string-empty-p super)
                   (string-trim
                    (shell-command-to-string
                     "git rev-parse --show-toplevel 2>/dev/null"))
                 super)))
    (if (string-empty-p root)
        nil
      ;; Walk up to the outermost superproject.
      (let ((dir root))
        (while (let ((parent (string-trim
                              (shell-command-to-string
                               (format "git -C %s rev-parse --show-superproject-working-tree 2>/dev/null"
                                       (shell-quote-argument dir))))))
                 (unless (string-empty-p parent)
                   (setq dir parent)))
          )
        dir))))

(defun wgh/fzf-submodule-files ()
  "Open a file using fzf, recursing into submodules from the root repo.
Walks up to find the outermost git repo (the root superproject) and
lists all tracked files including those in submodules."
  (interactive)
  (require 'fzf)
  (let* ((root (wgh/git-root-superproject)))
    (if root
        (fzf-with-command "git ls-files --recurse-submodules"
                          #'fzf--action-find-file
                          root)
      (user-error "Not inside a Git repository"))))

(defun wgh/agent-make-working-dir (topic)
  "Create and return a fresh agent-working-directory path for TOPIC."
  (let* ((base (wgh/agent-working-dir-base))
         (timestamp (format-time-string "%Y-%m-%dT%H-%M"))
         (agentid (string-trim
                   (shell-command-to-string
                    "head -c 10 /dev/random | md5sum | head -c 6")))
         (dir (concat base timestamp "_" topic "_" agentid "/")))
    (make-directory dir t)
    dir))

(defun wgh/agent-prompt-org (topic)
  "Prompt for TOPIC, create an agent-working-directory, and open prompt.org."
  (interactive "sTopic (hyphenated words): ")
  (let* ((dir (wgh/agent-make-working-dir topic))
         (file (concat dir "prompt.org")))
    (find-file file)))

(defun wgh/agent-prompt-from-region (beg end topic)
  "Create an agent-working-directory, write region to prompt.txt, and copy a message.
The copied message contains the directory path and prompt file path."
  (interactive "r\nsTopic (hyphenated words): ")
  (let* ((text (buffer-substring-no-properties beg end))
         (dir (wgh/agent-make-working-dir topic))
         (prompt-file (concat dir "prompt.txt")))
    (with-temp-file prompt-file
      (insert text))
    (let ((msg (format "Your agent-working-directory path is %s.  Read %s for instructions."
                       dir prompt-file)))
      (wgh/terminal-copy-osc-string msg)
      (message "Copied: %s" msg))))

(provide 'vfuncs)
