;; alternate-region.el  -*- lexical-binding: t -*-
;;
;;
;; Implements an alternate region.
;;
;; Core functionality:
;; `alternate-region-set' takes a (cons beginning end) region or nil, and optional buffer (which defaults to the current buffer).  It accordingly activates or deactivates the alternate-region overlay.  Non-interactive.
;; `alternate-region-activate' is interactive.  If `region-active-p', it sets the current region as the alternate region, and deactivates the region.  If not `region-active-p', deactivate the alternate region (setting it to nil).
;; `alternate-region-cycle' is interactive.  If `region-active-p' and there is an alternate region, it sets the current region as the alternate region and sets the previous alternate region as the new region.  Otherwise error.
;; `alternate-region-swap' is interactive.  If `region-active-p' and there is an alterante region, it swaps the contents of the region and the alternate region.  It updates the ranges of the region and alternate region, so they are both still active.  If the region and alternate region are not both active, error.

;; TODO - I would like to have multiple alternate regions in theory, but my main use case is just highlighting and swapping things.

(defvar alternate-region--current nil
  "Nil when there is no alternate region.
When there is an alternate region, it is a list (BUFFER BEGIN END).")

(defface alternate-region-face
  '((t (:inherit 'highlight)))
  "Face for alternate-region.")

(defvar alternate-region--overlay nil
  "Overlay for the alternate region.")



(defun alternate-region-set (region &optional buffer)
  "Set the alternate region to REGION (a cons of BEGIN and END) in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if region
        (let ((begin (car region))
              (end (cdr region)))
          (setq alternate-region--current (list (current-buffer) begin end))
          (when alternate-region--overlay
            (delete-overlay alternate-region--overlay)
            (setq alternate-region--overlay nil))
          (setq alternate-region--overlay (make-overlay begin end))
          (overlay-put alternate-region--overlay 'face 'alternate-region-face))
      (setq alternate-region--current nil)
      (when alternate-region--overlay
        (delete-overlay alternate-region--overlay)
        (setq alternate-region--overlay nil)))))

(defun alternate-region-activate ()
  "Activate the alternate region by setting it to the current active region."
  (interactive)
  (if (region-active-p)
      (let ((current-region (cons (region-beginning) (region-end))))
        (alternate-region-set current-region)
        (deactivate-mark))
    (alternate-region-set nil)))

(defun alternate-region-cycle ()
  "Cycle the current region and the alternate region."
  (interactive)
  (if (and (region-active-p) alternate-region--current)
      (let ((current-region (cons (region-beginning) (region-end)))
            (alt-region (cons (cadr alternate-region--current)
                              (caddr alternate-region--current)))
            (alt-buffer (car alternate-region--current)))
        (alternate-region-set current-region)
        (when (not (eq (current-buffer) alt-buffer))
          (switch-to-buffer alt-buffer))
        (goto-char (car alt-region))
        (set-mark (cdr alt-region)))
    (error "Both current and alternate regions must be active.")))



(defun alternate-region-swap ()
  "Swap the contents of the current region and the alternate region."
  (interactive)
  (if (and (region-active-p) alternate-region--current)
      (let* ((current-region (cons (region-beginning) (region-end)))
             (alt-buffer (car alternate-region--current))
             (alt-region (cons (cadr alternate-region--current) (caddr alternate-region--current)))
             (current-start (car current-region))
             (current-end (cdr current-region))
             (alt-start (car alt-region))
             (alt-end (cdr alt-region)))
        (if (equal (current-buffer) alt-buffer)
            ;; If both regions are in the same buffer
            (let ((current-text (buffer-substring-no-properties current-start current-end))
                  (alt-text (buffer-substring-no-properties alt-start alt-end)))
              (delete-region current-start current-end)
              (goto-char current-start)
              (insert alt-text)
              ;; Update positions based on the current length
              (let* ((length-difference (- (length alt-text) (length current-text)))
                     (new-alt-start (+ (if (< current-start alt-start) length-difference 0) alt-start))
                     (new-alt-end (+ new-alt-start (length alt-text)))
                     (final-alt-end (+ new-alt-start (length current-text)))
                     (current-alt-length (length current-text)))
                (delete-region new-alt-start new-alt-end)
                (goto-char new-alt-start)
                (insert current-text)
                ;; Adjust alternate buffer region positions
                (alternate-region-set (cons new-alt-start final-alt-end) alt-buffer)
                (goto-char (+ current-start (length alt-text)))
                (set-mark current-start)))
          ;; If regions are in different buffers
          (let ((current-text (buffer-substring-no-properties current-start current-end))
                (alt-text (with-current-buffer alt-buffer
                             (buffer-substring-no-properties alt-start alt-end))))
            ;; Swap text in the current buffer
            (delete-region current-start current-end)
            (goto-char current-start)
            (insert alt-text)
            (goto-char (+ current-start (length alt-text)))
            (set-mark current-start)
            ;; Swap text in the alternate buffer
            (with-current-buffer alt-buffer
              (delete-region alt-start alt-end)
              (goto-char alt-start)
              (insert current-text))
            ;; Update the alternate region
            (alternate-region-set (cons alt-start (+ alt-start (length current-text))) alt-buffer))))
    (error "Both current and alternate regions must be active.")))

(provide 'alternate-region)
