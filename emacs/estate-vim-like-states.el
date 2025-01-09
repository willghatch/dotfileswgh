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
(suppress-keymap estate-motion-state-keymap)



;; Command state is like “normal” mode in vim/evil-mode.
(estate-define-state command estate-motion-state-keymap)

;; Let's set one key here so the estate keymap isn't a death trap if not configured further.
;;(define-key estate-command-state-keymap "i" 'estate-insert-state)

(defun estate-command-state ()
  (interactive)
  (deactivate-mark)
  (estate-activate-state 'command))

;; A pager state is nice to have -- it's basically motion state but maybe with some extra keys.  The idea is that it's like command mode but you won't accidentally hit editing keys or such.
(estate-define-state pager estate-motion-state-keymap)
(defun estate-pager-state ()
  (interactive)
  (deactivate-mark)
  (estate-activate-state 'pager))

;; Insert state inherits the global keymap.  It's basically emacs mode.
(estate-define-state insert -estate-original-global-map)

(defvar-local estate--state-change-with-group-marker nil)

(defun estate--state-with-change-group-handler ()
  (when estate--state-change-with-group-marker
    (undo-amalgamate-change-group estate--state-change-with-group-marker)
    (setq-local estate--state-change-with-group-marker nil)))

(defun estate-state-with-change-group (new-state pre-change-thunk)
  "Do all operations in pre-change-thunk and new-state as one change group (undo group)."
  (estate--state-with-change-group-handler)
  (let ((estate-change-group-marker (prepare-change-group)))
    (setq-local estate--state-change-with-group-marker estate-change-group-marker)
    (funcall pre-change-thunk)
    (estate-activate-state new-state)))

(add-hook 'estate-insert-state-leave-hook 'estate--state-with-change-group-handler)

(defun estate-insert-state ()
  (interactive)
  (estate-state-with-change-group 'insert (lambda () nil)))

(defun estate-insert-state-with-thunk (thunk)
  "enter insert state, but execute thunk as part of the change group"
  (estate-state-with-change-group 'insert thunk))


;; Visual states.
;; I've made visual states use a hook so that commands that activate the region will activate visual modes.
;; All of these visual modes presuppose that you are using and want to use transient-mark-mode.
(estate-define-state visual estate-command-state-keymap)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now some helpers for operations that I want.  IE functions and infrastructure to have some things that are vim-like.

(defvar estate--registers (make-hash-table :test #'equal)
  "Hash table for “registers”, a la vim, where each register value must be a cons pair where the first element is a string and the second element has metadata for line mode.  For now, the metadata will either be null or the symbol 'line")
(setq estate-current-register "default")
(defun estate--get-register (reg)
  (gethash reg estate--registers (cons "" nil)))

;; TODO - I probably want to just get rid of extra visual states like visual-line and visual-block for now.
(defun estate-copy (&optional region)
  "TODO - copy but with extra handling, REGION is nil or a cons pair (beg . end)."
  (interactive)
  (let ((copy-metadata (and (eq estate-state 'visual-line) 'line)))
    (save-mark-and-excursion
      (estate-visual-execution-helper
       (lambda ()
         (let ((str (buffer-substring-no-properties (if region (car region) (region-beginning))
                                                    (if region (cdr region) (region-end)))))
           (puthash estate-current-register (cons str copy-metadata) estate--registers)
           (when (equal estate-current-register "default")
             (kill-new str))))))))

(defun estate--paste-helper (copy-from-active-region)
  "TODO - paste but with extra handling"
  ;; TODO - currently this also copies if region is active.  Do I want to do that?
  (let* ((to-paste (estate--get-register estate-current-register))
         (to-paste-text (if (stringp to-paste) to-paste (car to-paste)))
         (to-paste-metadata (if (stringp to-paste) nil (cdr to-paste))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro stuff
;; TODO - after I decide what I want, rename and finalize things.
;; TODO - don't use name kmacro -- kmacro is a specific library for handling keyboard macros.  The base emacs term is “kbd-macro”, so use that instead, or come up with some better term.
;; TODO - I want to have some kind of nested kmacro recording.  Eg. I want to be able to start recording a specific named macro, then start recording another specific named macro inside, finish the inner one and probably call it, then end the outer macro.  In part I want to use this because I want to have various commands that implicitly record named macros, mostly as an implementation detail, then allow calling them outside.
;;      - it looks like start-kbd-macro with an arg lets you start editing.  So If I keep state of a list of macro tags, and change behavior based on tag state, I can juggle multiple macro definitions.

(defvar-local estate--nested-keyboard-macro-states-alist nil
  "Alist of tags to nested keyboard macro recording state.
For each (potentially) nested macro recording, the list stores the prefix length of the kbd macro when the nested macro recording started.
When a macro recording ends, it ends recording, reads the state of last-kbd-macro, and removes the prefix as appropriate.
Then, if there are other macros being recorded, it starts a macro edit so that the other macro can continue.
")

(defun estate-nestable-keyboard-macro-start (tag)
  "Start recording a nestable keyboard macro.
This is presumably incompatible with any other packages that wrap kbd macro recording.
TAG must be a unique key as tested by `equal', and it is an error to start recording again with the same tag.

TODO - maybe I should add a counter like kmacro has, that is tag-specific?
"
  (cond ((assoc tag estate--nested-keyboard-macro-states-alist)
         (error "estate-nestable-keyboard-macro-start error: tag already in use: %s"
                tag))
        ((and (null estate--nested-keyboard-macro-states-alist)
              defining-kbd-macro)
         (error "estate-nestable-keyboard-macro-start error: already defining kbd macro outside of this framework"))
        ((null estate--nested-keyboard-macro-states-alist)
         ;; Non-nested call.
         (setq estate--nested-keyboard-macro-states-alist (list (list tag 0)))
         (start-kbd-macro nil))
        (t
         ;; Nested call.
         (end-kbd-macro)
         (setq estate--nested-keyboard-macro-states-alist
               (cons (list tag (length last-kbd-macro))
                     estate--nested-keyboard-macro-states-alist))
         (start-kbd-macro t t))))

(defun estate-nestable-keyboard-macro-end (tag)
  "Stop recording a nestable keyboard macro for TAG.
Return the keyboard macro as a string or vector.
TODO - maybe I should also save the recording to some data structure?
"
  (let* ((found (assoc tag estate--nested-keyboard-macro-states-alist))
         (prefix-length (and found (cadr found))))
    (cond ((not found)
           (error "estate-nestable-keyboard-macro-end: tag not found: %s" tag))
          (t
           (let ((not-defining-error nil))
             (if defining-kbd-macro
                 (end-kbd-macro)
               (setq not-defining-error t)
               )
             (let ((last-val last-kbd-macro))
               (setq estate--nested-keyboard-macro-states-alist
                     (assoc-delete-all tag estate--nested-keyboard-macro-states-alist))
               (when not-defining-error
                 (error "estate-nestable-keyboard-macro-end: not currently defining kbd macro"))
               (when (not (null estate--nested-keyboard-macro-states-alist))
                 (start-kbd-macro t t))
               (seq-drop last-val prefix-length)))))))


(setq-local estate--kmacro-to-buffer-change-state nil)

(defun estate-record-quick-keyboard-macro-to-buffer-change ()
  "Start recording a nestable keyboard macro that will stop recording automatically once the buffer has been changed and estate is back in command state.
Uses `estate-nestable-keyboard-macro-start', and thus is incompatible with other keyboard macro tools that don't.
"
  ;; TODO - integrate this better with record-to-register, and make the register it records to be configurable.
  (interactive)
  (when estate--kmacro-to-buffer-change-state
    (error "estate--kmacro-to-buffer-change-state error, alread: recording in %s state"
           estate--kmacro-too-buffer-change-state))
  ;;(kmacro-start-macro nil)
  (estate-nestable-keyboard-macro-start 'estate--to-change-keyboard-macro)
  (setq-local estate--kmacror-to-buffer-change-state 'recording)
  (letrec ((kmacro-record-until-buffer-change-func
            (lambda (changed-beg changed-end old-length)
              (remove-hook 'after-change-functions
                           kmacro-record-until-buffer-change-func
                           t)
              (if (equal estate-state 'command)
                  (progn
                    (setq-local estate--kmacro-to-buffer-change-state nil)
                    ;;(end-kbd-macro)
                    (puthash "kmacro-to-buffer-change"
                             (estate-nestable-keyboard-macro-end
                              'estate--to-change-keyboard-macro)
                             estate--registers)
                    (setq estate--most-recent-register-macro-recorded "kmacro-to-buffer-change"))
                (progn
                  (setq-local estate--kmacro-to-buffer-change-state 'changed)
                  (letrec ((command-state-kmacro-hook
                            (lambda ()
                              (remove-hook 'estate-command-state-enter-hook
                                           command-state-kmacro-hook
                                           t)
                              (setq-local estate--kmacro-to-buffer-change-state nil)
                              ;;(end-kbd-macro)
                              (puthash "kmacro-to-buffer-change"
                                       ;; When using the
                                       ;; estate-command-state-enter-hook, the
                                       ;; kmacro has not yet recorded the
                                       ;; current keys.  I don't love this, but
                                       ;; I'm not sure a better way right now.
                                       ;; TODO - this doesn't work when nested under recording a macro to a particular register.
                                       (vconcat (estate-nestable-keyboard-macro-end
                                                 'estate--to-change-keyboard-macro)
                                                (this-command-keys-vector))
                                       estate--registers)
                              (setq estate--most-recent-register-macro-recorded "kmacro-to-buffer-change"))))
                    (add-hook 'estate-command-state-enter-hook
                              command-state-kmacro-hook
                              0 t)))))))
    (add-hook 'after-change-functions
              kmacro-record-until-buffer-change-func
              0 t)))


(setq estate--most-recent-register-macro-recorded nil)

(defun estate-keyboard-macro-to-register-start (char)
  "Start recording a keyboard macro to register CHAR."
  (interactive "cmacro register char:\n")
  (estate-nestable-keyboard-macro-start
   (cons 'estate--keyboard-macro-to-register char)))

(defun estate--keyboard-macro-to-register-end (char)
  "Finish recording a keyboard macro to register CHAR."
  (let ((value (estate-nestable-keyboard-macro-end
                (cons 'estate--keyboard-macro-to-register char))))
    (puthash char value estate--registers)
    ;; TODO - I've been writing this macro stuff with setq-local and maybe defvar-local, but actually it should probably be global, not local.
    (setq estate--most-recent-register-macro-recorded char)
    value))

(defun estate--keyboard-macro-to-register-assoc-helper (alist-car key)
  (and (consp alist-car)
       (equal (car alist-car) key)))

(defun estate-keyboard-macro-to-register-end-most-recent ()
  "Finish recording a keyboard macro to a register, the most recent one started."
  (interactive)
  (let* ((found (assoc 'estate--keyboard-macro-to-register
                       estate--nested-keyboard-macro-states-alist
                       'estate--keyboard-macro-to-register-assoc-helper)))
    (when (not found)
      (error "estate-keyboard-macro-to-register-end-most-recent: not currently recording any"))
    (estate--keyboard-macro-to-register-end (cdar found))))

(defun estate-keyboard-macro-execute-from-register (count char)
  (interactive "p\ncregister char:\n")
  (execute-kbd-macro (gethash char estate--registers "") count))

(defun estate-keyboard-macro-execute-from-most-recently-macro-recorded-register
    (count)
  "TODO"
  (interactive "p")
  (estate-keyboard-macro-execute-from-register
   count
   estate--most-recent-register-macro-recorded))

(defun estate-keyboard-macro-to-register-end-most-recent-or-start-default ()
  "If currently recording from an estate-keyboard-macro-to-register-* function, end the most recently started one.
Otherwise, start recording to the register \"default\".
"
  (interactive)
  (let* ((found (assoc 'estate--keyboard-macro-to-register
                       estate--nested-keyboard-macro-states-alist
                       'estate--keyboard-macro-to-register-assoc-helper)))
    (if found
        (puthash (cdar found)
                 (estate--keyboard-macro-to-register-end (cdar found))
                 estate--registers)
      (estate-keyboard-macro-to-register-start "default"))))
;; TODO - probably use the character for default register a la vim, so it can still be selected by char.

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
