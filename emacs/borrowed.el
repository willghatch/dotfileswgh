
; taken from emacswiki
(defun set-tab-stop-width (width)
      "Set all tab stops to WIDTH in current buffer.
    
    This updates `tab-stop-list', but not `tab-width'.
    
    By default, `indent-for-tab-command' uses tabs to indent, see
    `indent-tabs-mode'."
      (interactive "nTab width: ")
      (let* ((max-col (car (last tab-stop-list)))
             ;; If width is not a factor of max-col,
             ;; then max-col could be reduced with each call.
             (n-tab-stops (/ max-col width)))
        (set (make-local-variable 'tab-stop-list)
             (mapcar (lambda (x) (* width x))
                     (number-sequence 1 n-tab-stops)))
        ;; So preserve max-col, by adding to end.
        (unless (zerop (% max-col width))
          (setcdr (last tab-stop-list)
                  (list max-col)))))

