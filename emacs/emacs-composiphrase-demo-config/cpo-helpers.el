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



(defun keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state ()
  (interactive)
  (setq composiphrase-current-sentence nil)
  (when (equal estate-state 'visual)
    (estate-normal-state))
  (keyboard-quit))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now some helpers for operations that I want.  IE functions and infrastructure to have some things that are vim-like.


(defvar estate-default-register ?\🄯)
(defvar estate-copy-sync-with-kill-ring-register estate-default-register)
(setq estate-current-register estate-default-register)

;; TODO - I probably want to just get rid of extra visual states like visual-line and visual-block for now.
(defun estate-copy (&optional region)
  "TODO - copy but with extra handling, REGION is nil or a cons pair (beg . end)."
  (interactive)
  (save-mark-and-excursion
    (let ((str (buffer-substring-no-properties (if region (car region) (region-beginning))
                                               (if region (cdr region) (region-end)))))
      (set-register estate-default-register str)
      (when (equal estate-current-register
                   estate-copy-sync-with-kill-ring-register)
        (kill-new str)))))

(defun estate--paste-helper (copy-from-active-region)
  "TODO - paste but with extra handling"
  ;; TODO - currently this also copies if region is active.  Do I want to do that?
  (let* ((to-paste (get-register estate-current-register))
         ;; TODO - use emacs register instead of my register stuff that I wrote thinking that registers were an evil-mode thing instead of an emacs feature.
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

;; TODO - get rid of this or make a working version
(defun estate-restore-region ()
  (interactive)
  (let ((last-state estate-previous-state))
    (activate-mark)))




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

(defvar estate-default-keyboard-macro-register ?\⌨)

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

