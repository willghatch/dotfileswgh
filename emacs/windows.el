;;; -*- lexical-binding: t; -*-
(defun window-swap (backwards-p)
  "Put the buffer from the selected window in next window, and vice versa"
  (let* ((this (selected-window))
         (other (if backwards-p (previous-window) (next-window)))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    (select-window other)))

(defun window-swap-next ()
  (interactive)
  (window-swap nil))
(defun window-swap-prev ()
  (interactive)
  (window-swap t))
