
(require 'xclip)

(defun xcopy (beg end) (interactive "r")
       (xclip-set-selection 'clipboard
                            (filter-buffer-substring beg end)))
(defun xpaste () (interactive)
       (insert (shell-command-to-string "xclip -o -clipboard")))
