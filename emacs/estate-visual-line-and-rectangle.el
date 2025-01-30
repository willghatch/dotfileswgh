(require 'estate-default-states)

(defun estate--activate-initialize-extra-visual-states ()
  (add-hook 'estate-visual-line-state-enter-hook #'estate--visual-line-on 0 t)
  (add-hook 'estate-visual-line-state-exit-hook #'estate--visual-line-off 0 t)
  )
(defun estate--deactivate-initialize-extra-visual-states ()
  (remove-hook 'estate-visual-line-state-enter-hook #'estate--visual-line-on t)
  (remove-hook 'estate-visual-line-state-exit-hook #'estate--visual-line-off t)
  )
(add-hook 'estate-activate-hook #'estate--activate-initialize-extra-visual-states)
(add-hook 'estate-deactivate-hook #'estate--deactivate-initialize-extra-visual-states)



;; Visual states.
;; I've made visual states use a hook so that commands that activate the region will activate visual modes.
;; All of these visual modes presuppose that you are using and want to use transient-mark-mode.
(estate-define-state visual estate-normal-state-keymap)
(estate-define-state visual-rectangle estate-visual-state-keymap)
(estate-define-state visual-line estate-visual-state-keymap)
;; TODO - the more I think about it, the more I think visual-line mode is more complexity than it is worth.  I'm going to expand the line selection text object behavior to expand the region to fill both begin and end lines for the region, then disable visual line mode for a while to see if I'm sure about it.

(defvar-local estate--visual-line nil)

(defun estate--mark-activate-hook ()
  (estate--visual-state-change 'activate))
(defun estate--mark-deactivate-hook ()
  (estate--visual-state-change 'deactivate))

(defvar-local estate--pre-visual-state nil)
(defvar-local estate--previous-kind-of-visual-state nil)

(defun estate--visual-state-change (reason)
  "Set estate state based on region and such."
  (if (member estate-state '(visual visual-rectangle visual-line))
      (setq-local estate--previous-kind-of-visual-state estate-state)
    (setq-local estate--pre-visual-state estate-state))
  (let ((previous-state-was-visual (member estate-state '(visual visual-rectangle visual-line))))
    (cond
     ((and (eq reason 'explicit) (not (region-active-p)))
      (estate-activate-state estate--pre-visual-state))
     ((and (region-active-p) (boundp 'rectangle-mark-mode) rectangle-mark-mode)
      (estate-activate-state 'visual-rectangle))
     ((and (region-active-p) estate--visual-line)
      (estate-activate-state 'visual-line))
     ((region-active-p)
      (estate-activate-state 'visual))
     (t
      ;; This is from something deactivating the mark.
      (estate-activate-state estate--pre-visual-state)))))

(defun estate--visual-state-helper (line-mode)
  (setq-local estate--visual-line line-mode)
  (let ((changed nil))
    (when (and (boundp 'rectangle-mark-mode) rectangle-mark-mode)
      (setq changed t)
      (rectangle-mark-mode -1))
    (when (not (region-active-p))
      (setq changed t)
      (set-mark-command nil))
    (when (not changed)
      (estate--visual-state-change 'explicit))))

(defun estate-visual-state ()
  (interactive)
  (setq-local estate--visual-line nil)
  (estate--visual-state-helper nil))

(defun estate-visual-line-state ()
  (interactive)
  (estate--visual-state-helper t))

(defun estate-visual-rectangle-state ()
  (interactive)
  (setq-local estate--visual-line nil)
  ;; Rely on the region activation hooks to set this.
  (when (not (region-active-p))
    (set-mark-command nil))
  (require 'rect)
  (rectangle-mark-mode 1))


(defun estate--visual-line-region (point-first)
  (and (region-active-p)
       (let* ((beg (region-beginning))
              (end (region-end))
              (beg-use (save-mark-and-excursion
                         (goto-char beg)
                         (if (bolp)
                             beg
                           (progn (move-beginning-of-line 1)
                                  (point)))))
              (end-use (save-mark-and-excursion
                         (goto-char end)
                         (if (eolp)
                             end
                           (progn (move-end-of-line 1)
                                  (unless (eobp) (forward-char 1))
                                  (point))))))
         (if (and point-first (< (mark) (point)))
             (cons end-use beg-use)
           (cons beg-use end-use)))))

(defun estate-visual-bounds ()
  "Like `visual-bounds', except in estate-visual-line-state it gives a region that covers full lines."
  (and (region-active-p)
       (if (eq estate-state 'visual-line)
           (list (estate--visual-line-region nil))
         (region-bounds))))

(defun estate-visual-execution-helper (thunk)
  "Execute THUNK, but if visual-line state is active, extend region first."
  (if (eq estate-state 'visual-line)
      (progn
        (let ((bounds (estate--visual-line-region t)))
          (set-mark (cdr bounds))
          (goto-char (car bounds))
          (funcall thunk)))
    (funcall thunk)))


(defun estate-restore-region ()
  (interactive)
  (let ((last-state estate-previous-state))
    (activate-mark)
    (when (member last-state '(visual-line visual-rectangle))
      (estate-activate-state last-state))))

(defvar-local estate--visual-line-overlays '())
(defun estate--visual-line-overlay-helper ()
  "Create overlays to show the difference between actual region and line-based region in visual-line mode.
Creates overlays for the areas that would be included in the line-based selection but aren't in the actual region."
  (mapc #'delete-overlay estate--visual-line-overlays)
  (setq-local estate--visual-line-overlays nil)
  (when (and (region-active-p) (eq estate-state 'visual-line))
    (let* ((region-bounds (region-bounds))
           (line-bounds (estate--visual-line-region nil))
           (region-start (caar region-bounds))
           (region-end (cdar region-bounds))
           (line-start (car line-bounds))
           (line-end (cdr line-bounds)))
      ;; Create overlay for start of line if needed
      (when (< line-start region-start)
        (let ((ov (make-overlay line-start region-start)))
          (overlay-put ov 'face 'region)
          (push ov estate--visual-line-overlays)))
      ;; Create overlay for end of line if needed
      (when (> line-end region-end)
        (let ((ov (make-overlay region-end line-end)))
          (overlay-put ov 'face 'region)
          (push ov estate--visual-line-overlays))))))
(defun estate--visual-line-on ()
  (add-hook 'post-command-hook #'estate--visual-line-overlay-helper nil t)
  (estate--visual-line-overlay-helper))
(defun estate--visual-line-off ()
  (estate--visual-line-overlay-helper)
  (remove-hook 'post-command-hook #'estate--visual-line-overlay-helper t)
  (setq estate--visual-line nil))



(provide 'estate-visual-line-and-rectangle-states)
