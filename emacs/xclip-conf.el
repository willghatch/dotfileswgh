;;; -*- lexical-binding: t; -*-
(setq init-xclip-conf-done nil)
(defun init-xclip-conf ()
  (unless init-xclip-conf-done
    (require 'xclip)
    (cond ((equal system-type 'darwin)
           (setq xclip-method 'pbpaste))
          ((equal (getenv "XDG_SESSION_TYPE") "wayland")
           (setq xclip-method 'wl-copy))
          (t (setq xclip-method 'xclip)))
    (setq init-xclip-conf-done t)))

(defun xcopy (beg end) (interactive "r")
       (init-xclip-conf)
       ;; the xclip package doesn't seem to be working on wayland right now, let's hack around it.
       (if (equal xclip-method 'wl-copy)
           (call-process-region (region-beginning) (region-end) "wl-copy")
         (xclip-set-selection 'clipboard
                              (filter-buffer-substring beg end)))
       (let ((eol (eolp))
             (point-at-region-end (equal end (point))))
         ;;(call-interactively 'evil-normal-state)
         (if (and point-at-region-end (not eol))
             (backward-char))))

(defun xcopy-buffer () (interactive)
       (init-xclip-conf)
       (xclip-set-selection 'clipboard
                            (filter-buffer-substring 1 (+ 1 (buffer-size)))))

(defun xpaste () (interactive)
       (init-xclip-conf)
       (let ((xclip-select-enable-clipboard t))
         ;; the xclip package doesn't seem to be working on wayland right now, let's hack around it.
         (if (equal xclip-method 'wl-copy)
             (call-process "wl-paste" nil t nil "-n")
           (insert (xclip-get-selection 'clipboard)))))

(defun xpaste-after-char () (interactive)
       (init-xclip-conf)
       (unless (eolp) (forward-char))
       (xpaste)
       (backward-char))

(defun wgh/tmux-copy ()
  (interactive)
  (call-process-region (region-beginning)
                       (region-end)
                       "tmux"
                       nil nil nil
                       "load-buffer"
                       "-"
                       ))
(defun wgh/tmux-paste ()
  (interactive)
  (call-process "tmux"
                nil t nil
                "show-buffer"
                ))

(defun wgh/terminal-copy-osc-string (str)
  ;; TODO - maybe check length, if it is too long the terminal might abort.
  (send-string-to-terminal
   (concat "\e]52;c;"
           (base64-encode-string (encode-coding-string str 'utf-8))
           ;; BEL character
           "\07")))
(defun wgh/terminal-copy-osc ()
  (interactive)
  (wgh/terminal-copy-osc-string
   (buffer-substring-no-properties (region-beginning) (region-end))))

(provide 'xclip-conf)
