;;; -*- lexical-binding: t -*-
;;; alternate-region.el --- Make a second region, swap regions.

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-alternate-region
;;; Git-Repository: git://github.com/willghatch/emacs-alternate-region.git
;;; Keywords: region
;;; Package-Requires: ((emacs "28"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;; I wrote this package to support a workflow that I wanted: Often there is
;;; some piece of code that I want to lift out into a definition.  But then I
;;; need to decide where to put it.  I can cut the text first, put in the
;;; identifier I use, find a place to put it, start writing a definition and
;;; hope I use the same identifier, then paste it.  But maybe I'll get
;;; distracted along the way to finding the appropriate place.  And probably I
;;; won't remember the exact name I used.  So instead, with this package, I can
;;; set the region that I want to lift out as the alternate region with
;;; `alternate-region-activate', which gives it highlighting, then move on to
;;; find the right place to put the definition, write the name, copy the name,
;;; and paste the name on the right hand side, then select the name and use
;;; `alternate-region-swap' to move the regions.  Then fix indentation,
;;; probably.
;;;
;;; The API:
;;; • `alternate-region-activate' is interactive.  If `region-active-p', it sets the current region as the alternate region, and deactivates the region.  If not `region-active-p', deactivate the alternate region (setting it to nil).
;;; • `alternate-region-cycle' is interactive.  If `region-active-p' and there is an alternate region, it sets the current region as the alternate region and sets the previous alternate region as the new region.  Otherwise error.
;;; • `alternate-region-swap' is interactive.  If `region-active-p' and there is an alterante region, it swaps the contents of the region and the alternate region.  It updates the ranges of the region and alternate region, so they are both still active.  If the region and alternate region are not both active, error.
;;; • `alternate-region-set' takes a (cons beginning end) region or nil, and optional buffer (which defaults to the current buffer).  It accordingly activates or deactivates the alternate-region overlay.  Non-interactive.  Probably just use `alternate-region-activate' instead, but this is the function to use programatically.
;;; • 'alternate-region-face' is a face for the highlighted alternate region.

;;; Code:


;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.
;; TODO - I would like to have multiple alternate regions in theory, but my main use case is just highlighting and swapping things.

(defvar alternate-region--current nil
  "Nil when there is no alternate region.
When there is an alternate region, it is a list (BUFFER BEGIN END).")

(defface alternate-region-face
  '((default (:inherit region))
    (((background dark)) (:background "#105010"))
    (((background light)) (:background "#a0cfaf")))
  "Face for alternate-region.")

(defvar alternate-region--overlay nil
  "Overlay for the alternate region.")



(defun alternate-region-set (region &optional buffer)
  "Set the alternate region to REGION (a cons of BEGIN and END) in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if region
        (let ((begin (car region))
              (end (cdr region))
              (prev-buffer (and alternate-region--current
                                (car alternate-region--current))))
          (setq alternate-region--current (list (current-buffer) begin end))
          (when (and prev-buffer (not (eq prev-buffer (current-buffer))))
            (with-current-buffer prev-buffer
              (remove-hook 'after-change-functions 'alternate-region--update t)))
          (when alternate-region--overlay
            (delete-overlay alternate-region--overlay)
            (setq alternate-region--overlay nil))
          (setq alternate-region--overlay (make-overlay begin end))
          (overlay-put alternate-region--overlay 'face 'alternate-region-face)
          (add-hook 'after-change-functions 'alternate-region--update 10 t))
      (setq alternate-region--current nil)
      (remove-hook 'after-change-functions 'alternate-region--update t)
      (when alternate-region--overlay
        (delete-overlay alternate-region--overlay)
        (setq alternate-region--overlay nil)))))

(defun alternate-region--update (beg end prev-length)
  "Update the alternate region when a change is made in its buffer.
BEG and END indicate the boundaries of the changed region.
PREV-LENGTH is the length of the text that was in the modified region."
  (when alternate-region--current
    (let* ((alt-buffer (car alternate-region--current))
           (alt-start (cadr alternate-region--current))
           (alt-end (caddr alternate-region--current))
           (change-length (- (- end beg) prev-length)))

      (when (eq (current-buffer) alt-buffer)
        (cond
         ((<= alt-end beg)
          ;; Do nothing
          )
         ((<= end alt-start)
          (let ((new-alt-start (+ alt-start change-length))
                (new-alt-end (+ alt-end change-length)))
            (alternate-region-set (cons new-alt-start new-alt-end) alt-buffer)))
         (t
          ;; Change overlaps with the alternate region, so just clear the alternate-region.
          (alternate-region-set nil)))))))

(defun alternate-region-activate ()
  "Activate the alternate region by setting it to the current active region.  If not `region-active-p', deactivate the alternate region."
  (interactive)
  (if (region-active-p)
      (let ((current-region (cons (region-beginning) (region-end))))
        (alternate-region-set current-region)
        (deactivate-mark))
    (alternate-region-set nil)))

(defun alternate-region-cycle ()
  "Don't move the text between the regions, but changen which region is the current region and the alternate region."
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
  "Swap the contents of the current region and the alternate region.  IE move the text between the two regions."
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
