;;; -*- lexical-binding: t; -*-


;; TODO - definitions that I'm using in my key bindings that I want in a composiphrase demo, but that aren't related to composing things, and don't really fit anywhere.

(defun delete-char-backward (&optional n)
  (interactive "p")
  (delete-char (- n)))


(defun cpo-goto-line-default-first (n)
  (interactive "p")
  (if (= n 0) (goto-line 1) (goto-line n)))
(defun cpo-goto-line-default-last (n)
  (interactive "P")
  (cond ((null n) (goto-line (line-number-at-pos (point-max))))
        ((numberp n) (goto-line n))
        ((consp n) (goto-line (car n)))
        (t (error))))


(defun keyboard-quit-and-clear-command-sentence ()
  (interactive)
  (setq command-sentence-current-sentence nil)
  (keyboard-quit))


(provide 'cpo-helpers)

