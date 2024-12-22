;; -*- lexical-binding: t -*-

;; Definitions for states that are similar to vim and evil-mode in spirit, though of course with some different details.

;; TODO - decide what I want to be here vs elsewhere.  Anything that lives here should have the estate- prefix, no wgh/ prefix.
;; TODO - clean up and document.  Especially document the wonky limitations of the different visual modes -- I am implementing just enough so that I get what I want out of visual-line, etc.
;; TODO - estate-vim-like-states, or at least a demo of bindings, needs a text object library.  So if I actually publish estate outside of my dotfiles, I need to also publish a text object library for it to use.

(require 'estate-core)

(defun estate--activate-initialize-core-states ()
  (estate-command-state)
  (add-hook 'activate-mark-hook #'estate--mark-activate-hook 0 t)
  (add-hook 'deactivate-mark-hook #'estate--mark-deactivate-hook 0 t)
  (add-hook 'estate-visual-line-state-enter-hook #'estate--visual-line-on 0 t)
  (add-hook 'estate-visual-line-state-leave-hook #'estate--visual-line-off 0 t)
  )
(defun estate--deactivate-initialize-core-states ()
  (remove-hook 'activate-mark-hook #'estate--mark-activate-hook t)
  (remove-hook 'deactivate-mark-hook #'estate--mark-deactivate-hook t)
  (remove-hook 'estate-visual-line-state-enter-hook #'estate--visual-line-on t)
  (remove-hook 'estate-visual-line-state-leave-hook #'estate--visual-line-off t)
  )
(add-hook 'estate-activate-hook #'estate--activate-initialize-core-states)
(add-hook 'estate-deactivate-hook #'estate--deactivate-initialize-core-states)

;; Motion map is a core map that suppresses everything not bound, and is the basis for most non-insert modes.
(estate-define-state motion nil)
(suppress-keymap estate-motion-keymap)



;; Command state is like “normal” mode in vim/evil-mode.
(estate-define-state command estate-motion-keymap)

;; Let's set one key here so the estate keymap isn't a death trap if not configured further.
(define-key estate-command-keymap "i" 'estate-insert-state)

(defun estate-command-state ()
  (interactive)
  (deactivate-mark)
  (estate-state-activate 'command))

;; A pager state is nice to have -- it's basically motion state but maybe with some extra keys.  The idea is that it's like command mode but you won't accidentally hit editing keys or such.
(estate-define-state pager estate-motion-keymap)
(defun estate-pager-state ()
  (interactive)
  (deactivate-mark)
  (estate-state-activate 'pager))

;; Insert state inherits the global keymap.  It's basically emacs mode.
(estate-define-state insert -estate-original-global-map)

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


;; Visual states.
;; I've made visual states use a hook so that commands that activate the region will activate visual modes.
;; All of these visual modes presuppose that you are using and want to use transient-mark-mode.
(estate-define-state visual estate-command-keymap)
(estate-define-state visual-rectangle estate-visual-keymap)
(estate-define-state visual-line estate-visual-keymap)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now some helpers for operations that I want.  IE functions and infrastructure to have some things that are vim-like.

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
          (if (eobp)
              (insert "\n")
            (forward-char 1))
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
  (estate--visual-line-overlay-helper)
  (remove-hook 'post-command-hook #'estate--visual-line-overlay-helper t)
  (setq estate--visual-line nil))


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

;; TODO - also shouldn't be in estate, but markers -- eg. evil-mode stores markers with buffer, line, and column, so you can hop between them.
;; TODO - not stuff to actually go in this file, but a checklist of things I want to replace evil-mode.  Note that many of these I can just fall back to requiring evil-mode and just using it...
;; * quick-in-block/quick-a-block re-implementation
;; * et/ot/ef/of - make them be “find to char begin/end” instead
;; * markers
;; * surround region - add delimiter
;; * surround - change delimiter, maybe
;; * . repeat, maybe, could just use macros more
;; * select register, maybe
;; * d/c/y operators, maybe, could just get used to visual first -- I only use the operators for trivial cases
;; * select object modifiers to get surrounding whitespace
;; * isearch land on start of search string -- actually I would like both, but with a way to control it.  Maybe this already exists?
;;
;; Differences from vim/evil to note:
;; * cursor always between parens
;; * movements are probably a bit different, not trying to slavishly copy vim, just get the spirit of my favorite features.  This isn't vim emulation, it's a start to replace evil-mode with something simpler that I can use as a springboard to broader exploration of better modal editing paradigms.
;; * copy doesn't leave visual mode (you can bind a key to do both, but by default it doesn't)
;; * delete doesn't copy (there is a copy/swap and a delete without copy).  Together with delete not copying, you can do copy-then-delete to get the same effect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'estate-vim-like-states)
