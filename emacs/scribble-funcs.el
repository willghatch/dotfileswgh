
(defun scribble-pdf ()
  (interactive)
  (start-process "scribble" "*scribble-output*" "scribble" "--pdf" (buffer-file-name)))
