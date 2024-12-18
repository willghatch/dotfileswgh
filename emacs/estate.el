;; -*- lexical-binding: t -*-

;; This is basically intended as a modal editing package, like evil-mode, vaguely vim-inspired, except:
;; * All cursor positions are between characters, always.  No cursor-on-character nonsense like vi and its descendants.
;; * I want it to be lighter-weight than evil-mode.
;; * It won't come with bindings out-of-the-box, or at least won't provide them without calling some function.
;; * estate-mode is mostly about having states, and only provides a few helpers for a few concerns that interact with states, eg. visual mode concerns.

;; TODO - make state definition macro, clean up to be “publishable”, add current state variable and indicator, how to deal with repeating commands (should it be integrated or separate), ...

;; TODO - what vim/evil functionality do I want to replicate?  Marks?  Registers?  I want something that (1) doesn't have the on-character location issue of vim, and (2) is lighter-weight than evil-mode.  What features belong in estate vs in other files?

(setq -estate-original-global-map (current-global-map))
(defvar-local -estate-mode-current-keymap (make-sparse-keymap))
(defvar-local -estate-mode-map-alist `((estate-local-mode . ,-estate-mode-current-keymap)))

(defun estate--activate-mark-hook ()
  (add-hook 'activate-mark-hook #'estate--mark-activate-hook 0 t)
  (add-hook 'deactivate-mark-hook #'estate--mark-deactivate-hook 0 t))
(defun estate--deactivate-mark-hook ()
  (remove-hook 'activate-mark-hook #'estate--mark-activate-hook t)
  (remove-hook 'deactivate-mark-hook #'estate--mark-deactivate-hook t))

(define-minor-mode estate-local-mode
  "TODO docstring here..."
  :init-value nil
  :lighter " estate "
  ;; If this has a keymap value, the keymap is stored in a non-buffer-local variable.
  ;; The solution is to use emulation-mod-map-alist, which can consult a buffer-local variable, and is read before minor-mode maps.
  (if estate-local-mode
      (progn
        (setq-local -estate-mode-current-keymap (make-sparse-keymap))
        (add-to-list 'emulation-mode-map-alists '-estate-mode-map-alist)
        (estate-command-state)
        (estate--activate-mark-hook)
        (add-hook 'estate-visual-line-state-enter-hook #'estate--visual-line-on 0 t)
        (add-hook 'estate-visual-line-state-leave-hook #'estate--visual-line-off 0 t)
        )
    (progn
      (estate--deactivate-mark-hook)
      ))
  )
(defun -estate-mode-initialize ()
  (unless (minibufferp)
    ;; TODO - maybe I should make a minibuffer-specific map instead?
    (estate-local-mode 1)))
(define-globalized-minor-mode estate-mode
  estate-local-mode -estate-mode-initialize)

(defvar estate-state-change-hook '())

(defun estate--keymap-name (state)
  (intern (format "estate-%s-keymap" state)))
(defun estate--enter-hook-name (state)
  (intern (format "estate-%s-state-enter-hook" state)))
(defun estate--leave-hook-name (state)
  (intern (format "estate-%s-state-leave-hook" state)))

(defmacro estate-define-state (state-name parent-keymap)
  "Define a new state with STATE-NAME.
Defines estate-X-keymap, estate-X-state-enter-hook, estate-X-state-leave-hook.
The keymap gets the specified parent."
  (let ((keymap (estate--keymap-name state-name))
        (enter-hook (estate--enter-hook-name state-name))
        (leave-hook (estate--leave-hook-name state-name)))
    `(progn
       (defvar ,keymap (make-sparse-keymap))
       (set-keymap-parent ,keymap ,parent-keymap)
       (defvar ,enter-hook '())
       (defvar ,leave-hook '())
       )))

(estate-define-state motion nil)
(suppress-keymap estate-motion-keymap)

(estate-define-state command estate-motion-keymap)
(estate-define-state visual estate-motion-keymap)
(estate-define-state visual-rectangle estate-visual-keymap)
(estate-define-state visual-line estate-visual-keymap)
(estate-define-state pager estate-motion-keymap)
(estate-define-state insert -estate-original-global-map)

;; Let's set one key here so the estate keymap isn't a death trap if not configured further.
(define-key estate-command-keymap "i" 'estate-insert-state)

(defvar-local estate-state nil)
(defvar-local estate--previous-state nil)
(defvar-local estate--pre-visual-state nil)

;; TODO - this undo grouping is terrible.  I should re-work it.  It should be probably an insert-state hook.
(defun estate-state-activate (state &optional skip-undo-grouping)
  (unless (eq estate-state state)
    (let ((keymap-sym (estate--keymap-name state))
          (enter-hook (estate--enter-hook-name state))
          (leave-hook (and estate--previous-state
                           (estate--leave-hook-name estate--previous-state))))
      (if (boundp keymap-sym)
          (progn
            (setq-local estate--previous-state estate-state)
            (setq-local estate-state state)

            (setq-local -estate-mode-map-alist `((estate-local-mode . ,(symbol-value keymap-sym))))
            ;(set-keymap-parent -estate-mode-current-keymap (symbol-value keymap-sym))
            (unless skip-undo-grouping
              (estate-mode-with-change-group-handler))
            (when (symbol-value leave-hook)
              (run-hooks leave-hook))
            (run-hooks 'estate-state-change-hook)
            (when (symbol-value enter-hook)
              (run-hooks enter-hook)))
        (error "bad estate state: %s" state)))))

(defun estate-command-state ()
  (interactive)
  (deactivate-mark)
  (estate-state-activate 'command))

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
      (estate-state-activate estate--pre-visual-state 'skip-undo-group))
     ((and (region-active-p) (boundp 'rectangle-mark-mode) rectangle-mark-mode)
      (estate-state-activate 'visual-rectangle))
     ((and (region-active-p) estate--visual-line)
      (estate-state-activate 'visual-line))
     ((region-active-p)
      (estate-state-activate 'visual))
     (t
      ;; This is from something deactivating the mark.
      ;(message "mark deactivation state change...")
      (estate-state-activate estate--pre-visual-state 'skip-undo-group)
      ;nil
      )
     ;(t (error "estate--visual-state-change fall through"))
     )))

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

(defvar-local estate--mode-change-with-group-marker nil)
(defun estate-mode-with-change-group-handler ()
  (when estate--mode-change-with-group-marker
    (undo-amalgamate-change-group estate--mode-change-with-group-marker)
    (setq-local estate--mode-change-with-group-marker nil)))

(defun estate-mode-with-change-group (new-state pre-change-thunk)
  "Do all operations in pre-change-thunk and new-state as one change group (undo group)."
  (estate-mode-with-change-group-handler)
  (let ((estate-change-group-marker (prepare-change-group)))
    (setq-local estate--mode-change-with-group-marker estate-change-group-marker)
    (funcall pre-change-thunk)
    (estate-state-activate new-state 'skip-undo-group)))

(defun estate-insert-state ()
  (interactive)
  (estate-mode-with-change-group 'insert (lambda () nil)))

(defun estate-insert-state-with-thunk (thunk)
  "enter insert state, but execute thunk as part of the change group"
  (estate-mode-with-change-group 'insert thunk))


(defvar estate--registers (make-hash-table :test #'equal)
  "Hash table for “registers”, a la vim, where each register value must be a cons pair where the first element is a string and the second element has metadata for line mode.  For now, the metadata will either be null or the symbol 'line")
(setq estate-current-register "default")
(defun estate--get-register (reg)
  (gethash reg estate--registers (cons "" nil)))

(defun estate-copy ()
  "TODO - copy but with extra handling"
  (interactive)
  (let ((copy-metadata (and (eq estate-state 'visual-line) 'line)))
    (save-mark-and-excursion
      (estate-visual-execution-helper
       (lambda ()
         (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
           (puthash estate-current-register (cons str copy-metadata) estate--registers)
           (when (equal estate-current-register "default")
             (kill-new str))))))))

(defun estate--paste-helper (copy-from-active-region)
  "TODO - paste but with extra handling"
  ;; TODO - currently this also copies if region is active.  Do I want to do that?
  (let* ((to-paste (estate--get-register estate-current-register))
         (to-paste-text (car to-paste))
         (to-paste-metadata (cdr to-paste)))
    (cond
     ((and (eq estate-state 'visual-line) (eq to-paste-metadata 'line))
      (when copy-from-active-region
        (estate-copy))
      (estate-visual-execution-helper
       (lambda () (replace-region-contents (region-beginning) (region-end)
                                           (lambda () to-paste-text)))))
     ((and (region-active-p) (eq to-paste-metadata 'line))
      (error "Paste of line text in non-line visual state not yet supported"))
     ((eq to-paste-metadata 'line)
      ;; No region active with line mode.
      (save-mark-and-excursion
        (atomic-change-group
          (end-of-line)
          (insert "\n")
          (insert to-paste-text))))
     ((eq estate-state 'visual-line)
      (error "Paste of non-line text in visual-line state not yet supported"))
     ((region-active-p)
      (when copy-from-active-region
        (estate-copy))
      (replace-region-contents (region-beginning) (region-end)
                               (lambda () to-paste-text)))
     (t (insert to-paste-text)))))

(defun estate-paste ()
  (interactive)
  (estate--paste-helper nil))
(defun estate-paste/swap ()
  (interactive)
  (estate--paste-helper t))

(defun estate-restore-region ()
  (interactive)
  ;; TODO - this is not quite right for “restore last region” but is... close enough for now.
  (let ((last-state estate--previous-state))
    (activate-mark)
    (when (member last-state '(visual-line visual-rectangle))
      (estate-state-activate last-state))))

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
  (remove-hook 'post-command-hook #'estate--visual-line-overlay-helper t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro stuff, this really should go elsewhere
(defvar-local wgh/-macro-recording nil)
(defun wgh/macro-record (char)
  (interactive "cmacro char:\n")
  (setq-local wgh/-macro-recording char)
  (kmacro-start-macro nil))
(defun wgh/macro-finish-recording ()
  (interactive)
  (kmacro-end-macro nil)
  (puthash wgh/-macro-recording last-kbd-macro estate--registers)
  (setq-local wgh/-macro-recording nil))
(defun wgh/macro-toggle ()
  (interactive)
  (if wgh/-macro-recording
      (call-interactively 'wgh/macro-finish-recording)
    (call-interactively 'wgh/macro-record)))
(defun wgh/call-macro-by-name (count char)
  (interactive "p\ncmacro char:\n")
  (execute-kbd-macro (gethash char estate--registers "") count))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(set-keymap-parent -estate-mode-current-keymap estate-command-keymap)
(estate-command-state)

(provide 'estate)
