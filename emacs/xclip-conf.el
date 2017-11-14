
(require 'xclip)

(defun xcopy (beg end) (interactive "r")
       (xclip-set-selection 'clipboard
                            (filter-buffer-substring beg end))
       (let ((eol (eolp))
             (point-at-region-end (equal end (point))))
         (call-interactively 'evil-normal-state)
         (if (and point-at-region-end (not eol))
             (backward-char))))
(defun xpaste () (interactive)
       (let ((xclip-select-enable-clipboard t))
         (insert (xclip-selection-value))))

(defun xpaste-after-char () (interactive)
       (unless (eolp) (forward-char))
       (xpaste)
       (backward-char))
