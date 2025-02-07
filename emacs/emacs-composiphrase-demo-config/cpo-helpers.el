;;; -*- lexical-binding: t; -*-


;; TODO - definitions that I'm using in my key bindings that I want in a composiphrase demo, but that aren't related to composing things, and don't really fit anywhere.

(defun delete-char-backward (&optional n)
  (interactive "p")
  (delete-char (- n)))


(defun cpo-goto-line-default-first (&optional n)
  "Go to line n.  But also handle negative n with -1 as the last line, etc."
  (interactive "p")
  (cond ((null n) (goto-line 1))
        ((= n 0) (goto-line 1))
        ((< n 0) (goto-line (+ 1 (line-number-at-pos (point-max)) n)))
        (t (goto-line n))))
(defun cpo-goto-line-default-last (&optional n)
  "Go to line n.  But also handle negative n with -1 as the last line, etc.
If no argument is given, go to the last line."
  (interactive "P")
  (cond ((null n) (goto-line (line-number-at-pos (point-max))))
        ((numberp n) (cpo-goto-line-default-first n))
        ((consp n) (cpo-goto-line-default-first (car n)))
        (t (error))))



(defun keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state ()
  "Like `keyboard-quit', but also clear 'composiphrase-current-sentence' and quit `estate-visual-state'."
  (interactive)
  (setq composiphrase-current-sentence nil)
  (when (equal estate-state 'visual)
    (estate-normal-state))
  (keyboard-quit))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now some helpers for operations that I want.  IE functions and infrastructure to have some things that are vim-like.




;; TODO - get rid of this or make a working version
(defun estate-restore-region ()
  "Restore previous visual region.
TODO this is broken.  Implement correctly by having a visual state hook that saves the last region, and then after-change-functions hook to update the last region."
  (interactive)
  (let ((last-state estate-previous-state))
    (activate-mark)))





(defun cpo-paste (register-for-paste register-to-copy-old-region)
  "Paste from REGISTER-FOR-PASTE.
If there is an active region, save it to REGISTER-TO-COPY-OLD-REGION if non-nil.
Returns the size of the pasted text (minus size of replaced text).
"
  (let* ((use-register (or register-for-paste cpo-paste-default-register))
         (use-register (if (functionp use-register) (funcall use-register) use-register))
         (to-paste (get-register use-register))
         (to-paste-text (cond
                         ((not to-paste) "")
                         ((stringp to-paste) to-paste)
                         (t (format "%s" to-paste))))
         (region-size (and (region-active-p) (- (region-end) (region-beginning))))
         (paste-diff-size (- (length to-paste-text) (or region-size 0))))
    (if (region-active-p)
        (progn
          (when register-to-copy-old-region
            (set-register (if (functionp register-to-copy-old-region)
                              (funcall register-to-copy-old-region)
                            register-to-copy-old-region)
                          (buffer-substring-no-properties (region-beginning) (region-end))))
          (replace-region-contents (region-beginning) (region-end)
                                   (lambda () to-paste-text)))
      (insert to-paste-text))
    paste-diff-size))

(defun cpo-move-paste-sentence-execute (sentence-with-defaults)
  "Takes a command sentence for move-paste, executes a move with it, then does paste.  Depends on the specific composiphrase config from the demo setup..."
  (let* ((orig-point (point))
         (new-point (save-mark-and-excursion
                      (composiphrase-execute
                       ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                       (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                       composiphrase-current-configuration)
                      (point)))
         (sentence-modifiers (composiphrase-sentence-modifiers
                              sentence-with-defaults))
         (register (cdr (assq 'register sentence-modifiers)))
         (register-for-old (cdr (assq 'register-for-old sentence-modifiers))))
    (progn
      (goto-char new-point)
      (let ((pasted-length (cpo-paste register register-for-old)))
        (goto-char (if (< orig-point new-point)
                       orig-point
                     (+ orig-point pasted-length)))))))


(defun cpo-copy (register &optional region)
  "REGION is nil or a cons pair (beg . end)."
  (let ((register-use (if (functionp register)
                          (funcall register)
                        register))
        (str (buffer-substring-no-properties (if region (car region) (region-beginning))
                                             (if region (cdr region) (region-end)))))
    (set-register register-use str)
    (when (equal register-use
                 cpo-copy-sync-with-kill-ring-register)
      (kill-new str))))

(defun cpo-delete (register &optional region)
  (let ((register-use (if (functionp register)
                          (funcall register)
                        register))
        (beg (if region (car region) (region-beginning)))
        (end (if region (cdr region) (region-end))))
    (when register-use
      (cpo-copy register (cons beg end)))
    (delete-region beg end)))

(defun cpo-change (register &optional region)
  (let ((register-use (if (functionp register)
                          (funcall register)
                        register))
        (beg (if region (car region) (region-beginning)))
        (end (if region (cdr region) (region-end))))
    (when register-use
      (cpo-copy register (cons beg end)))
    (estate-insert-state-with-thunk (lambda ()
                                      (delete-region beg end)))))


;; Default registers a la vim...
(defvar cpo-paste-default-register ?\©
  "Default register to read from for cpo-paste.
To have Vim-like behavior, set to ?\".")
(defvar cpo-copy-default-register ?\©
  "Default register to write to for cpo-copy.
To have Vim-like behavior, set to ?\".")
(defvar cpo-copy-sync-with-kill-ring-register ?\©
  "When this register is equal to the register used for a copy, also put the copy in the kill ring.")
(defvar cpo-delete-default-register ?\␡
  "Default register to write to for cpo-delete.
To have Vim-like behavior, set to ?\".")
(defvar cpo-change-default-register ?\␡
  "Default register to write to for cpo-change.
To have Vim-like behavior, set to ?\".")
(defvar cpo-paste-copy-old-default-register ?\␡
  "Default register to write to for cpo-copy when replacing a region.")


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
    (set-register char value)
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
  (execute-kbd-macro
   (get-register char)
   count))

(defun estate-keyboard-macro-execute-from-most-recently-macro-recorded-register
    (count)
  "TODO"
  (interactive "p")
  (estate-keyboard-macro-execute-from-register
   count
   estate--most-recent-register-macro-recorded))

(defvar estate-default-keyboard-macro-register ?\⌨
  "Register used to save macro for `estate-keyboard-macro-to-register-end-most-recent-or-start-default'.  Wow that's a terrible name.  This is demo-ware, after all.")

(defun estate-keyboard-macro-to-register-end-most-recent-or-start-default ()
  "If currently recording from an estate-keyboard-macro-to-register-* function, end the most recently started one.
Otherwise, start recording to the register 'estate-default-keyboard-macro-register'.
"
  (interactive)
  (let* ((found (assoc 'estate--keyboard-macro-to-register
                       estate--nested-keyboard-macro-states-alist
                       'estate--keyboard-macro-to-register-assoc-helper)))
    (if found
        (set-register (cdar found)
                      (estate--keyboard-macro-to-register-end (cdar found)))
      (estate-keyboard-macro-to-register-start
       estate-default-keyboard-macro-register))))
;; TODO - probably use the character for default register a la vim, so it can still be selected by char.



;; TODO - not stuff to actually go in this file, but a checklist of things I want to replace evil-mode.  Note that many of these I can just fall back to requiring evil-mode and just using it...
;; * surround region - add delimiter
;; * surround - change delimiter, maybe
;; * . repeat, maybe, could just use macros more
;; * select register, maybe
;; * select object modifiers to get surrounding whitespace
;; * isearch land on start of search string -- actually I would like both, but with a way to control it.  Maybe this already exists?
;;
;; Differences from vim/evil to note:
;; * cursor always between characters
;; * movements are probably a bit different, not trying to slavishly copy vim, just get the spirit of my favorite features.  This isn't vim emulation, it's a start to replace evil-mode with something simpler that I can use as a springboard to broader exploration of better modal editing paradigms.
;; * copy doesn't leave visual mode (you can bind a key to do both, but by default it doesn't)
;; * delete doesn't copy (there is a copy/swap and a delete without copy).  Together with delete not copying, you can do copy-then-delete to get the same effect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(provide 'cpo-helpers)

