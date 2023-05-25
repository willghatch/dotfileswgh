(setq init-xclip-conf-done nil)
(defun init-xclip-conf ()
  (unless init-xclip-conf-done
    (require 'xclip)
    (setq xclip-method 'xclip)
    (when (equal system-type 'darwin)
      (setq xclip-method 'pbpaste))
    (setq init-xclip-conf-done t)))

(defun xcopy (beg end) (interactive "r")
       (init-xclip-conf)
       (xclip-set-selection 'clipboard
                            (filter-buffer-substring beg end))
       (let ((eol (eolp))
             (point-at-region-end (equal end (point))))
         (call-interactively 'evil-normal-state)
         (if (and point-at-region-end (not eol))
             (backward-char))))

(defun xcopy-buffer () (interactive)
       (init-xclip-conf)
       (xclip-set-selection 'clipboard
                            (filter-buffer-substring 1 (+ 1 (buffer-size)))))

(defun xpaste () (interactive)
       (init-xclip-conf)
       (let ((xclip-select-enable-clipboard t))
         (insert (xclip-get-selection 'clipboard))))

(defun xpaste-after-char () (interactive)
       (init-xclip-conf)
       (unless (eolp) (forward-char))
       (xpaste)
       (backward-char))
