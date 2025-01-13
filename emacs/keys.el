;; TODO - working on replacing evil-mode with something that keeps cursor BETWEEN instead of ON characters in all modes.  Also hopefully a little lighter.
;; TODO - what major features do I really want from vim/evil?
;; * composable language for composing operations, motions, etc.  But I have something more ambitious than what vim does in mind, much more composable to learn a smaller number of keys yet have way more motions than vim.  But it may take me some time to fully flesh it out and build it, given how often I spend time working towards it...
;; * visual mode
;; * line-based visual mode - with proper insert/append support -- IE when inserting or appending, add to each line
;; * block-based visual mode - with proper insert/append/edit support -- IE insert/append/replace on each line
;; * text objects, object/inner/around/etc with growing for nestable objects, and my easy bindings on () keys
;; * port my tree operations, in particular on-parens, but switching to normal smartparens now should be easy
;; * change surrounding delimiters
;; * move to opposite delimiter (eg. % in evil-mode)
;; * repeat command
;; * marks (set mark, go to mark or line of mark) -- note that evil-mode marks are per-buffer, but I wonder if I would prefer marks that record buffer and position...
;; * registers -- IE copy/paste to/from register, record/replay keyboard macro to/from register
;; * overwrite mode?
;; * replace single character?
;; * TODO - what else?

;; * TODO - improvements (that I could even put in my current config before switching to this)

;; * TODO - I now have a bunch of definitions in here that I should move out


;; Keys I don't really use:

;; Insert mode:
;; C-o -- I read that this is bound to something useful in readline, I should look into it
;; C-q
;; maybes
;; C-r
;; C-w is close window on certain un-customizable browsers and I accidentally
;;     hit it all the time, so I should not use it.
;; non-character symbols?
;; M- combos? (I like and use them less)
;; M-a,e,g,i
;; M-j is something that maybe I should use, but I don't
;; M-k,l,m,n,o,p,q
;; M-s,u,v
;; maybe M-y
;; M-z

;; M-t -- I think I want to save this for windowing in terminal multiplexer with C-t

;; G- H- (sup- hyp-) combos?  (I need conventions for terminal encodings of these...)

;; Normal mode:
;; B,W I don't really use, but maybe I should?
;; E
;; h,l I have on ec/oc which I'm mostly happy with
;; H,L,M -- move to top,bottom,middle of window.  I don't really use these.  Should I?
;; J,K -- aliases for H,L, also not really used
;; m -- I think I wanted this to be mode-specific -- eg. a fallback to normal emacs keys for things like dired
;; O -- I have it as oo
;; S,T,U,Y -- all no-ops or duplicates, or things I don't use.
;; Z -- currently sub-map, but I don't use it for anything.
;; non-character symbols?
;; #, $, comma, +
;; & I don't really use, maybe I should
;; also maybe I should use *
;; []{}
;; any weird unicode that I have in convenient places


(message "\n\nIn keys.el\n\n")

(require 'estate-core)
(require 'estate-vim-like-states)
(estate-mode 1)
(require 'command-sentence)

(defun cs/verb (name)
  `((word-type . verb)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cs/obj (name)
  `((word-type . object)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cs/mod (name contents)
  `((word-type . modifier)
    (parameter-name . ,name)
    (contents . ,contents)
    (ui-hint . ,contents)))
(defun cs/ae (&rest words)
  (apply 'command-sentence-add-to-current-with-numeric-handling
         'exec-after
         words))
(defun cs/add (&rest words)
  (apply 'command-sentence-add-to-current-with-numeric-handling
         nil
         words))


(load-library "text-object-stuff")
(load-library "tree-walk-smartparens-integration.el")

;; State for repeating isearch -- my key to start isearch sets this so that the repeat keys go the right direction.
(setq wgh/isearch-repeat-forward-p t)
;; State for isearch repetition -- values 'beginning or 'end.
;; TODO - I would like a system for writing an escape in the search for where the cursor should go.
(setq wgh/isearch-go-part 'beginning)

(defun wgh/isearch-start-forward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p t)
  (funcall 'isearch-forward))
(defun wgh/isearch-start-backward ()
  (interactive)
  (setq wgh/isearch-repeat-forward-p nil)
  (funcall 'isearch-backward))

;; TODO - this has an issue when changing direction.  The first time it repeats backward after going forward, it skips one.
(defun wgh/isearch-repeat-forward (&optional count)
  (interactive "p")
  (let* ((count-fwd (<= 0 (or count 1)))
         (count-num (abs (or count 1)))
         (fwd (not (xor count-fwd wgh/isearch-repeat-forward-p)))
         (repeat-func (if fwd #'isearch-repeat-forward #'isearch-repeat-backward))
         (moved (tree-walk--motion-moved (lambda () (funcall repeat-func count-num)))))
    (cond ((not moved) nil)
          ((and (equal wgh/isearch-go-part 'beginning)
                (< isearch-other-end (point)))
           (goto-char isearch-other-end))
          ((and (equal wgh/isearch-go-part 'end)
                (>= isearch-other-end (point)))
           (goto-char isearch-other-end)))))
(defun wgh/isearch-repeat-backward (&optional count)
  (interactive "p")
  (wgh/isearch-repeat-forward (- (or count 1))))



;; isearch-wrap-pause can be t (default) to signal an error, then actually wrap the next time, 'no to wrap immediately but flash, 'no-ding to wrap immediately but not flash, or nil to disallow wrapping entirely.
(setq isearch-wrap-pause 'no)


(defun emmap (keys func)
  (nobreak-define-key estate-motion-state-keymap keys func))
(defun ecmap (keys func)
  (nobreak-define-key estate-command-state-keymap keys func))
(defun evmap (keys func)
  (nobreak-define-key estate-visual-state-keymap keys func))
(defun evrmap (keys func)
  (nobreak-define-key estate-visual-state-rectangle-keymap keys func))
(defun epmap (keys func)
  (nobreak-define-key estate-pager-state-keymap keys func))
(defun eimap (keys func)
  (nobreak-define-key estate-insert-state-keymap keys func))
;; TODO - pager mode/state


;; TODO - These are defined in init-helpers, but I'm redefining them for now for compatibility until I finish the a full transition to the new config...
;; TODO - I should move the above definitions into init-helpers instead and replace uses of these everywhere.  At least a bit in mode-hooks.
(defun mkmap (keys func)
  (nobreak-define-key estate-motion-state-keymap keys func))
(defun nkmap (keys func)
  (nobreak-define-key estate-command-state-keymap keys func))
(defun vkmap (keys func)
  (nobreak-define-key estate-visual-state-keymap keys func))
(defun ikmap (keys func)
  (nobreak-define-key estate-insert-state-keymap keys func))
(defun pkmap (keys func)
  (nobreak-define-key estate-pager-state-keymap keys func))

;; TODO - do I want to add something like evil-normal-state-local-map?  The only place I have used this is in org-mode to overwrite bindings to be org-specific.  Only I haven't liked that, because then I can't use the bindings that they shadow, which I also sometimes want.  This is part of the motivation to design a more composable and prefix-heavy keymap -- so that I can have more things bound in a way that I can remember them, and use all of the commands together.
(defun lnkmap (keys func)
  (when (not estate-command-state-buffer-local-keymap)
    (setq-local estate-command-state-buffer-local-keymap (make-sparse-keymap)))
  (nobreak-define-key estate-command-state-buffer-local-keymap keys func))




;; TODOs
;; * forward/back line keeping column position (including when going through a short line)
;; * repeat commands
;; * visual mode
;; * text objects
;; ... a lot more

(defmacro with-evil (func)
  `(lambda ()
     (interactive)
     (require 'evil)
     (call-interactively ,func)))

(defun wgh/evil-goto-marker ()
  (interactive)
  (require 'evil)
  (call-interactively 'evil-goto-mark))
(defun wgh/evil-goto-marker-line ()
  (interactive)
  (require 'evil)
  (call-interactively 'evil-goto-mark-line))

(cl-defun wgh/forward-line-keep-column/qd (&optional count)
  ;; Well, not as good as evil mode... maybe I'll do this properly later?
  ;; TODO - look at goal-column variable
  (interactive "p")
  (let ((col (current-column)))
    (forward-line (or count 1))
    (move-to-column col)))
(cl-defun wgh/backward-line-keep-column/qd (&optional count)
  (interactive "p")
  (wgh/forward-line-keep-column/qd (- (or count 1))))

;;(ecmap "j" 'wgh/forward-line-keep-column/qd)
;;(ecmap "k" 'wgh/backward-line-keep-column/qd)
;;(repeatable-motion-define-pair 'forward-line 'previous-line)
(defun wgh/next-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (next-line arg)))
(defun wgh/prev-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (previous-line arg)))
(repeatable-motion-define-pair 'wgh/next-line 'wgh/prev-line)

(defun goto-column (&optional n)
  (interactive "P")
  (move-to-column (or n 0)))
(defun goto-column/default-end (&optional n)
  (interactive "P")
  (if n (move-to-column n) (end-of-line)))


(ecmap "\M-c" 'execute-extended-command)
(ecmap "\M-r" 'estate-mode)

(evmap "\C-c" 'estate-command-state)

(eimap "\C-c" 'estate-command-state)
(eimap "\C-l" 'estate-command-state)
(eimap "\M-c" 'execute-extended-command)
(eimap "\C-w" 'ignore) ;; by default bound to kill-region.  I might map it to delete-backward-word, but I also don't want to get used to using it such that I accidentally use it in a web browser, since browsers are both extremely necessary, in fact using multiple browsers is more-or-less necessary, and none of them have reasonable allowance for user customization of keyboard shortcuts.
(eimap "\C-t" 'ignore)


;; Wow.  I've stopped using this organically over time due to inherent issues in timing based key chords.  But it turns out that removing this single require cut my load time by nearly half.
;;(require 'key-chord)
;;(key-chord-mode 1)
;;(key-chord-define estate-insert-keymap (kbd "kj") 'estate-command-state)


(progn
  (repeatable-motion-define-pair 'sptw-forward-inorder-traversal 'sptw-backward-inorder-traversal)
  (repeatable-motion-define-pair 'sptw-forward-sibling-beginning 'sptw-backward-sibling-beginning)
  (repeatable-motion-define-pair 'sptw-forward-sibling-end 'sptw-backward-sibling-end)
  (repeatable-motion-define-pair 'sptw-up-parent-beginning 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-up-parent-end 'sptw-down-first-child-beginning)
  (repeatable-motion-define 'sptw-down-last-child-end 'sptw-up-parent-beginning)
                                        ;(repeatable-motion-define-pair 'sptw-forward-sexp-in-supersexp 'sptw-backward-sexp-in-supersexp)
  )



(defun wgh/expand-region-to-fill-lines (&optional include-final-newline)
  (when (not (region-active-p))
    (set-mark (point)))
  (let ((point-first (< (point) (mark)))
        (goto-end (if include-final-newline
                      (lambda () (goto-char (line-end-position)) (when (not (eobp)) (forward-char 1)))
                    (lambda () (goto-char (line-end-position))))))
    (if point-first (goto-char (line-beginning-position)) (funcall goto-end))
    (exchange-point-and-mark)
    (if (not point-first) (goto-char (line-beginning-position)) (funcall goto-end))
    (exchange-point-and-mark)))


(setq -wgh/find-char-in-line/impl-last-char nil)
(setq -wgh/find-char-in-line/impl-last-style 'reverse-emacs)
(defun wgh/find-char-in-line/impl (char count style)
  "STYLE is 'emacs, 'reverse-emacs, 'beginning, or 'end.
Emacs style is to go to the end of the character when going forward, and
to the beginning of the character when going backward.  Reverse-emacs style
is the opposite."
  (let ((r (regexp-quote (if (stringp char) char (string char))))
        (fwd (< 0 count))
        (count (abs count))
        (keep-going t))
    (when (and fwd
               (or (equal style 'beginning)
                   (equal style 'reverse-emacs))
               (looking-at r))
      (forward-char 1))
    (when (and (not fwd)
               (or (equal style 'end)
                   (equal style 'reverse-emacs))
               (save-mark-and-excursion
                 (backward-char 1)
                 (looking-at r)))
      (backward-char 1))
    (while (and keep-going (< 0 count))
      (setq count (- count 1))
      (let ((p (point))
            (l (line-number-at-pos (point))))
        (ignore-errors
          (if fwd
              (re-search-forward r)
            (re-search-backward r)))
        (when (not (equal l (line-number-at-pos (point))))
          (setq keep-going nil)
          (goto-char p))))
    (when (and fwd
               (or (equal style 'beginning)
                   (equal style 'reverse-emacs)))
      (backward-char 1))
    (when (and (not fwd)
               (or (equal style 'end)
                   (equal style 'reverse-emacs))
               (forward-char 1))
      (backward-char 1))))
(defun wgh/find-char-in-line-forward-repeat (&optional n)
  (interactive "p")
  (wgh/find-char-in-line/impl -wgh/find-char-in-line/impl-last-char
                              n
                              -wgh/find-char-in-line/impl-last-style))
(defun wgh/find-char-in-line-backward-repeat (&optional n)
  (interactive "p")
  (wgh/find-char-in-line-forward-repeat (- n)))

;; The evil-mode movements I'm used to are basically like emacs style and reverse-emacs style of what I've defined... so maybe I'll use them...
(defun wgh/find-char-beginning-in-line-forward (&optional n)
  (interactive "p")
  (let ((c (read-char "char to find:")))
    (setq -wgh/find-char-in-line/impl-last-char c)
    (setq -wgh/find-char-in-line/impl-last-style 'beginning)
    (wgh/find-char-in-line/impl c n 'beginning)))
(defun wgh/find-char-beginning-in-line-backward (&optional n)
  (interactive "p")
  (wgh/find-char-beginning-in-line-forward (- n)))
(repeatable-motion-define 'wgh/find-char-beginning-in-line-forward 'wgh/find-char-in-line-backward-repeat :repeat 'wgh/find-char-in-line-forward-repeat)
(repeatable-motion-define 'wgh/find-char-beginning-in-line-backward 'wgh/find-char-in-line-forward-repeat :repeat 'wgh/find-char-in-line-backward-repeat)
(defun wgh/find-char-end-in-line-forward (&optional n)
  (interactive "p")
  (let ((c (read-char "char to find:")))
    (setq -wgh/find-char-in-line/impl-last-char c)
    (setq -wgh/find-char-in-line/impl-last-style 'end)
    (wgh/find-char-in-line/impl c n 'end)))
(defun wgh/find-char-end-in-line-backward (&optional n)
  (interactive "p")
  (wgh/find-char-end-in-line-forward (- n)))
(repeatable-motion-define 'wgh/find-char-end-in-line-forward 'wgh/find-char-in-line-backward-repeat :repeat 'wgh/find-char-in-line-forward-repeat)
(repeatable-motion-define 'wgh/find-char-end-in-line-backward 'wgh/find-char-in-line-forward-repeat :repeat 'wgh/find-char-in-line-backward-repeat)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dump of my old keys to gradually massage them and see if I will like this...

(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))

;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun tkmap (keys func)
  (nobreak-define-key temp-key-map keys func))

;; call-keymap is from https://stackoverflow.com/questions/24914202/elisp-call-keymap-from-code
(defun call-keymap (map &optional prompt)
  "Read a key sequence and call the command it's bound to in MAP."
  ;; Note: MAP must be a symbol so we can trick `describe-bindings' into giving
  ;; us a nice help text.
  (let* ((overriding-local-map `(keymap (,map . ,map)))
         (help-form `(describe-bindings ,(vector map)))
         (key (read-key-sequence prompt))
         (cmd (lookup-key map key t)))
    (if (functionp cmd)
        (call-interactively cmd)
      (user-error "%s is undefined" key))))



;;(ecmap "a" (lambda () (interactive) (message "don't append, use i for insert")))
;;(ecmap "a" expand-region-outer-map)

;;(evmap "a" expand-region-outer-map)
;;(evmap "i" expand-region-inner-map)



(ecmap "A" (lambda () (interactive)
             (progn (move-end-of-line nil) (estate-insert-state))))
(evmap "A" (lambda () (interactive) (progn (goto-char (region-end))
                                           (deactivate-mark)
                                           (estate-insert-state))))

(ecmap "C" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "C" (lambda () (interactive (progn (kill-region (region-beginning)
                                                       (region-end))
                                          (estate-insert-state)))))

(ecmap "D" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "D" (lambda () (interactive) (progn (kill-region nil nil t))))
(ecmap "I" (lambda () (interactive)
             (progn (back-to-indentation) (estate-insert-state))))
(evmap "I" (lambda () (interactive) (progn (goto-char (region-beginning))
                                           (deactivate-mark)
                                           (estate-insert-state))))


;;(ecmap "O" 'baddd-open-above)
;; I really prefer vim's terminology for copy/paste...
;;(ecmap "p" 'yank)
(ecmap "p" 'estate-paste)
(evmap "p" 'estate-paste)
(ecmap "P" 'estate-paste/swap)
(evmap "P" 'estate-paste/swap)
(ecmap "r" 'baddd-replace)
(ecmap "R" (lambda () (interactive) (message "don't use replace-state")))

;; (ecmap "q" 'wgh/macro-toggle)
;; (ecmap "Q" 'kmacro-call-macro)
;; TODO - figure out good keys to use, especially for the “quick” ones that record to change.
(ecmap "q" 'estate-keyboard-macro-to-register-end-most-recent-or-start-default)
(ecmap "Q" 'estate-keyboard-macro-to-register-start)
(ecmap "r" 'estate-keyboard-macro-execute-from-most-recently-macro-recorded-register)
(ecmap "R" 'estate-keyboard-macro-execute-from-register)
;;(ecmap "h" 'estate-record-quick-keyboard-macro-to-buffer-change)

(ecmap "@" 'wgh/call-macro-by-name)
(ecmap "x" 'delete-char)
(defun delete-char-backward (&optional n)
  (interactive "p")
  (delete-char (- n)))
(ecmap "X" 'delete-char-backward)

;; TODO - I have this in the command map, but I find that I care more about having quick access to copy than I do about most commands.
(ecmap "y" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cs/ae (cs/verb 'copy)
                                 (cs/obj 'region))
                          n)
               (funcall (cs/add (cs/verb 'copy))
                        n))))

(ecmap "Y" (lambda () (interactive) (message "Y not yet implemented")))
;;(ecmap "&" 'baddd-ex-repeat-substitute)
(ecmap "gq" (with-evil 'evil-fill-and-move))
(ecmap "gw" (with-evil 'evil-fill))
;;(ecmap "zo" 'baddd-open-fold)
;;(ecmap "zc" 'baddd-close-fold)
;;(ecmap "za" 'baddd-toggle-fold)
;;(ecmap "zr" 'baddd-open-folds)
;;(ecmap "zm" 'baddd-close-folds)
;;(ecmap "z=" 'ispell-word)
;;(ecmap "\C-n" 'baddd-paste-pop-next)
;;(ecmap "\C-p" 'baddd-paste-pop)
(ecmap "\C-t" 'pop-tag-mark)
;;(ecmap (kbd "C-.") 'baddd-repeat-pop)
;;(ecmap (kbd "M-.") 'baddd-repeat-pop-next)
(ecmap "." 'baddd-repeat) ;; TODO - repeat is an important evil-mode thing to replace!
(ecmap "\"" 'baddd-use-register)
(ecmap "~" (with-evil 'evil-invert-char))
;;(ecmap "=" 'baddd-indent)
(ecmap "<" (with-evil 'evil-shift-left))
(ecmap ">" (with-evil 'evil-shift-right))
;;(ecmap "ZZ" 'baddd-save-modified-and-close)
;;(ecmap "ZQ" 'baddd-quit)
(ecmap (kbd "DEL") 'rmo/backward-char)
(ecmap (kbd "<deletechar>") 'rmo/forward-char)

(ecmap "=" 'indent-region)
(ecmap "≠" 'wgh/racket-indent-region)
(ecmap "\M-\C-\\" (lambda () (interactive) (message "use =")))
(eimap "\C-s" 'backward-kill-word)
(eimap (kbd "M-DEL") (lambda () (interactive) (message "use C-s")))

;; undo
(ecmap "u" 'undo)
(ecmap "\C-r" 'undo-tree-redo)


;;; Motion state

;; "0" is a special command when called first
;;(baddd-redirect-digit-argument baddd-motion-state-map "0" 'baddd-beginning-of-line)
;;(emmap "0" 'baddd-beginning-of-line)
(emmap "0" 'digit-argument)
(emmap "1" 'digit-argument)
(emmap "2" 'digit-argument)
(emmap "3" 'digit-argument)
(emmap "4" 'digit-argument)
(emmap "5" 'digit-argument)
(emmap "6" 'digit-argument)
(emmap "7" 'digit-argument)
(emmap "8" 'digit-argument)
(emmap "9" 'digit-argument)
(emmap "-" 'negative-argument)
(defun vilish/goto-line/beginning (n)
  (interactive "p")
  (if (= n 0) (goto-line 1) (goto-line n)))
(defun vilish/goto-line/end (n)
  (interactive "P")
  (cond ((null n) (goto-line (line-number-at-pos (point-max))))
        ((numberp n) (goto-line n))
        ((consp n) (goto-line (car n)))
        (t (error))))
(emmap "G" 'vilish/goto-line/end)
;;(emmap "H" 'baddd-window-top)
(emmap "j" (cs/ae (cs/mod 'direction 'forward)
                  (cs/mod 'location-within 'keep-if-possible)
                  (cs/obj 'line)))
(emmap "k" (cs/ae (cs/mod 'direction 'backward)
                  (cs/mod 'location-within 'keep-if-possible)
                  (cs/obj 'line)))
;;(emmap "K" 'baddd-lookup)
;;(emmap "L" 'baddd-window-bottom)
;;(emmap "M" 'baddd-window-middle)
;;(emmap "w" 'rmo/wgh/forward-word-beginning)
;;(emmap "b" 'rmo/wgh/backward-word-beginning)
;;(emmap "w" 'rmo/wgh/forward-vi-like-word-beginning)
(emmap "w" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'vi-like-word)))
;;(emmap "b" 'rmo/wgh/backward-vi-like-word-beginning)
(emmap "b" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'vi-like-word)))

;;(emmap "W" 'rmo/baddd-forward-WORD-begin)
;;(emmap "B" 'rmo/baddd-backward-WORD-begin)
;;(emmap "ge" 'rmo/baddd-backward-word-end)
;;(emmap "gE" 'rmo/baddd-backward-WORD-end)
(emmap "gg" 'vilish/goto-line/beginning)
;;(emmap "g_" 'baddd-last-non-blank)
(emmap "g\C-]" 'find-tag)
(emmap "{" 'rmo/wgh/backward-paragraph-beginning)
(emmap "}" 'rmo/wgh/forward-paragraph-beginning)
(emmap "#" 'rmo/baddd-search-word-backward)
(emmap "g#" 'rmo/baddd-search-unbounded-word-backward)
;;(emmap "$" 'end-of-line)
(emmap "%" 'sptw-move-to-other-end-of-sexp)
(emmap "`" (with-evil (cs/ae (cs/obj 'goto-marker))))
(emmap "'" (with-evil (cs/ae (cs/obj 'goto-marker-line))))
;;(emmap "]]" 'rmo/baddd-forward-section-begin)
;;(emmap "][" 'rmo/baddd-forward-section-end)
;;(emmap "[[" 'rmo/baddd-backward-section-begin)
;;(emmap "[]" 'rmo/baddd-backward-section-end)
;;(emmap "[(" 'rmo/baddd-previous-open-paren)
;;(emmap "])" 'rmo/baddd-next-close-paren)
;;(emmap "[{" 'rmo/baddd-previous-open-brace)
;;(emmap "]}" 'rmo/baddd-next-close-brace)
(emmap "*" 'rmo/baddd-search-word-forward)
(emmap "g*" 'rmo/baddd-search-unbounded-word-forward)
;;(emmap "," 'baddd-repeat-find-char-reverse)

(emmap "/" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'isearch-new)))
(emmap "?" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'isearch-new)))
(emmap "n" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'isearch-repeat)))
(emmap "N" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'isearch-repeat)))



(emmap ";" 'er/expand-region)
(emmap "^" 'baddd-first-non-blank) ;; TODO - re-impl using back-to-indentation...
;;(emmap "+" 'baddd-next-line-first-non-blank)
;;(emmap "_" 'baddd-next-line-1-first-non-blank)
;;(emmap "-" 'baddd-previous-line-first-non-blank)
(emmap "\C-w" 'baddd-window-map)
(emmap "\C-]" 'baddd-jump-to-tag)
(emmap (kbd "C-b") 'baddd-scroll-page-up)
(emmap (kbd "C-d") 'baddd-scroll-down)
(emmap (kbd "C-e") 'baddd-scroll-line-down)
(emmap (kbd "C-f") 'baddd-scroll-page-down)
(emmap (kbd "C-o") 'baddd-jump-backward)
(emmap (kbd "C-y") 'baddd-scroll-line-up)
(emmap "\\" 'baddd-execute-in-emacs-state)
;;(emmap "z^" 'baddd-scroll-top-line-to-bottom)
;;(emmap "z+" 'baddd-scroll-bottom-line-to-top)
;;(emmap "zt" 'baddd-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
;;(emmap (vconcat "z" [return]) "zt^")
;;(emmap (kbd "z RET") (vconcat "z" [return]))
;;(emmap "zz" 'baddd-scroll-line-to-center)
;;(emmap "z." "zz^")
;;(emmap "zb" 'baddd-scroll-line-to-bottom)
;;(emmap "z-" "zb^")
(emmap "v" 'estate-visual-state)
(emmap "V" 'estate-visual-line-state)
(emmap "\C-v" 'estate-visual-rectangle-state)
(emmap "gv" 'estate-restore-region)
(emmap [left] 'rmo/backward-char)
(emmap [right] 'rmo/forward-char)
(emmap [up] 'rmo/previous-line)
(emmap [down] 'rmo/next-line)

;;(evmap [escape] 'baddd-exit-visual-state)

;;; Replace state

;;(define-key baddd-replace-state-map (kbd "DEL") 'baddd-replace-backspace)
;;(define-key baddd-replace-state-map [escape] 'baddd-normal-state)

;;; Minibuffer

;;(define-key minibuffer-local-map "\C-p" 'baddd-complete-next)
;;(define-key minibuffer-local-map "\C-n" 'baddd-complete-previous)
;;(define-key minibuffer-local-map "\C-x\C-p" 'baddd-complete-next-line)
;;(define-key minibuffer-local-map "\C-x\C-n" 'baddd-complete-previous-line)

;; Ex
(emmap ":" (lambda () (interactive)
             (require 'evil)
             (if (region-active-p)
                 (evil-ex (format "%s,%s "
                                  (line-number-at-pos (region-beginning))
                                  (line-number-at-pos (region-end))))
               (evil-ex))
             ;; The below isn't working, but it would be nicer to have symbolic '<,'> for region bounds so that I can just repeat a replacement command.
             ;; (assq-delete-all 60 evil-markers-alist) ;; by default has evil-visual-beginning
             ;; (assq-delete-all 62 evil-markers-alist) ;; by default has evil-visual-goto-end
             ;; (setq evil-markers-alist (cons
             ;;                           (cons 62 #'region-end)
             ;;                           (cons (cons 60 #'region-beginning) evil-markers-alist)))

             ;; (evil-ex (if (region-active-p) "'<,'> " nil))
             ))
(emmap "!" (lambda () (interactive)
             (require 'evil) (call-interactively 'evil-shell-command)))
                                        ;
;;; search command line
;;(define-key baddd-ex-search-keymap "\d" #'baddd-ex-delete-backward-char)
                                        ;


;;; CUSTOM BINDINGS SECTION

;; window commands
(defhydra my-window-map (:foreign-keys warn) "WM:"
  ("v" split-window-horizontally nil)
  ("s" split-window-vertically nil)
  ("j" (lambda (&optional n) (interactive "p") (other-window n)) nil)
  ("k" (lambda (&optional n) (interactive "p") (other-window (- n))) nil)
  ("c" delete-window nil)
  ("h" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'shrink-window-horizontally))) "skinny")
  ("l" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'enlarge-window-horizontally))) "fat")
  ("H" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'enlarge-window))) "tall")
  ("L" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'shrink-window))) "short")
  ("f" delete-other-windows "full")
  ;; Space will be for layout concerns
                                        ;(" f" delete-other-windows)
                                        ;(" u" winner-undo)
                                        ;(" r" winner-redo)
                                        ;(" j" window-swap-next)
                                        ;(" k" window-swap-prev)
                                        ;("p" popwin-map)
  ("=" balance-windows "balance")
  ("g" elscreen-create)
  ("G" elscreen-kill)
  ("w" elscreen-next)
  ("b" elscreen-previous)
  ;; TODO - I want m to be "mode" -- IE I want a key to go into the map, but then
  ;;        have a key inside the map to make it sticky (switch modes).
  ("m" nil)
  ("e" nil)
  )



;; function aliases
(defalias 'er 'eval-region)
(defalias 'eb 'eval-buffer)
(defalias 'ee 'eval-expression)
(defalias 'shr 'shell-command-on-region)
(defalias 'trunc 'toggle-truncate-lines)
(defalias 'sc 'flyspell-mode)
(defalias 'sc-prog 'flyspell-prog-mode)
(defalias 'fc 'flycheck-mode)

;; More text objects!
;;(define-key baddd-inner-text-objects-map "a" 'baddd-inner-arg)
;;(define-key baddd-outer-text-objects-map "a" 'baddd-outer-arg)
;;(define-key baddd-inner-text-objects-map "c" 'baddd-cp-inner-comment)
;;(define-key baddd-outer-text-objects-map "c" 'baddd-cp-a-comment)
;;(define-key baddd-inner-text-objects-map "d" 'baddd-cp-inner-defun)
;;(define-key baddd-outer-text-objects-map "d" 'baddd-cp-a-defun)
;;(define-key baddd-inner-text-objects-map "b" 'baddd-textobj-anyblock-inner-block)
;;(define-key baddd-outer-text-objects-map "b" 'baddd-textobj-anyblock-a-block)
;;(define-key baddd-inner-text-objects-map "i" 'indent-tree-inner)
;;(define-key baddd-outer-text-objects-map "i" 'indent-tree-outer)
;;; These are also on t, which I'm already used to, but to fix the tree thing...
;;(define-key baddd-inner-text-objects-map "x" 'baddd-inner-tag)
;;(define-key baddd-outer-text-objects-map "x" 'baddd-a-tag)

;; Normal state switch!
(eimap (kbd "C-c") 'estate-command-state)
(emmap (kbd "C-c") 'ignore)

;; Keys unbound and reserved for future use - bind to nop so they don't input
(emmap (kbd "RET") 'ignore)
(emmap "S" 'ignore)
(emmap "T" 'ignore)


;; I'm not sure a better place to put this...
(ecmap (kbd "TAB") 'sp-indent-defun)

;; g map
(emmap "gr" 'baddd-ace-jump-word-mode)
(emmap "gc" 'baddd-ace-jump-char-mode)
(emmap "gf" 'baddd-ace-jump-line-mode)
(emmap "gdd" 'xref-find-definitions)
(emmap "gdD" 'xref-find-definitions-other-window)
(emmap "gdr" 'xref-find-references)
(emmap "gdi" 'lsp-describe-thing-at-point)
(emmap "gdp" 'pop-tag-mark)
(ecmap " yc" 'xcopy)
(ecmap " Pc" 'xpaste)
(ecmap " pc" 'xpaste)
(evmap " yc" 'xcopy)
(evmap " Pc" 'xpaste)
(evmap " pc" 'xpaste)
(emmap "gl" 'baddd-scroll-right)
(emmap "gh" 'baddd-scroll-left)
(emmap "gn" 'baddd-next-match)
(emmap "gN" 'baddd-previous-match)

(ecmap "g&" 'baddd-ex-repeat-global-substitute)
(ecmap "gi" 'baddd-insert-resume) ; insert mode at ins. mode cursor point
(ecmap "gu" 'baddd-downcase)
(ecmap "gU" 'baddd-upcase)
(ecmap "g~" 'baddd-invert-case)
(ecmap "g;" 'goto-last-change)
(ecmap "g," 'goto-last-change-reverse)

;; TODO - move these
(defun vilish-open-line-below ()
  ;; TODO - evil takes a numeric argument, and when you are done entering text it copies that line N times.  I never use that feature though, so... maybe I don't care.
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (end-of-line)
                                    ;;(open-line 1)
                                    ;;(forward-line)
                                    (newline-and-indent)
                                    )))
(defun vilish-open-line-above ()
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (beginning-of-line)
                                    (if (bobp)
                                        (open-line 1)
                                      (progn (backward-char 1)
                                             (newline-and-indent))))))

(defun wgh/forward-char/no-line-wrap (&optional count)
  (interactive "p")
  (let ((fwd (< 0 count))
        (count (abs count))
        (keep-going t))
    (while (and keep-going (< 0 count))
      (if fwd
          (if (eolp) (setq keep-going nil) (forward-char 1))
        (if (bolp) (setq keep-going nil) (backward-char 1)))
      (setq count (- count 1)))))
(defun wgh/backward-char/no-line-wrap (&optional count)
  (interactive "p")
  (wgh/forward-char/no-line-wrap (- count)))
(repeatable-motion-define-pair 'wgh/forward-char/no-line-wrap
                               'wgh/backward-char/no-line-wrap)

;; Commands for command-sentence
(defhydra command-select (:foreign-keys warn :exit nil) "Cmd:"
  ("c" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'change)
                             (cs/obj 'region))
                      n)
           (funcall (cs/add (cs/verb 'change))
                    n)))
   "change" :exit t)
  ("d" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'delete)
                             (cs/obj 'region))
                      n)
           (funcall (cs/add (cs/verb 'delete))
                    n)))
   "delete" :exit t)
  ("y" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'copy)
                             (cs/obj 'region))
                      n)
           (funcall (cs/add (cs/verb 'copy))
                    n)))
   "copy" :exit t)
  ;; TODO - handle the fact that some verbs take region, and other verbs don't work well in visual state.  Except that some should be able to take an object and behave differently with region active.  Eg. transpose with a region containing multiple tree siblings or lines should transpose a group.
  ("j" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'join)) n))
   "join" :exit t)
  ("J" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'split)) n))
   "split" :exit t)
  ("o" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'open)) n))
   "open" :exit t)
  ("t" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'transpose)) n))
   "transpose" :exit t)
  ("s" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'slurp)) n))
   "slurp" :exit t)
  ("b" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'barf)) n))
   "barf" :exit t)
  ("u" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'downcase)) n))
   "downcase" :exit t)
  ("U" (lambda (n) (interactive "p")
         (funcall (cs/add (cs/verb 'upcase)) n))
   "upcase" :exit t)
  ;; TODO - need to make C-g clear the current-command-sentence, or something like that

  )

(ecmap "c" 'command-select/body)

;; o and e maps - o is left/back, e is right/forward

(defhydra object-select (:foreign-keys warn :exit nil) "Obj:"
  ;; TODO - I need these bodies to be evaluated, but hydra does some macro magic.  Maybe I should use something else instead of hydra... Or I can wrap them all in funcall...
  ("c" (funcall (cs/ae (cs/obj 'character))) "character" :exit t)
  ("f" (funcall (cs/ae (cs/mod 'specific t)
                       (cs/obj 'character)))
   "character-specific" :exit t)
  ("l" (funcall (cs/ae (cs/obj 'line))) "line" :exit t)
  ("w" (funcall (cs/ae (cs/obj 'vi-like-word))) "vi-like-word" :exit t)
  ("W" (funcall (cs/ae (cs/obj 'word))) "word" :exit t)
  ("s" (funcall (cs/ae (cs/obj 'sptw))) "smartparens" :exit t)
  ("i" (funcall (cs/ae (cs/obj 'indent-tree))) "indent-tree" :exit t)
  ;; TODO - I want this one, but I keep using this accidentally due to my old key bindings, and it is so frustrating.  So I'll leave it as a no-op for now.
  ;;("o" (funcall (cs/ae (cs/obj 'outline))) "outline" :exit t)
  ("o" (funcall (cs/ae (cs/obj 'NOOP-STOP-USING-THIS-BINDING-FOR-OLD-PURPOSE))) "break habit!" :exit t)
  ("O" (funcall (cs/ae (cs/obj 'outline))) "outline" :exit t)
  ("t" (funcall (cs/ae (cs/obj 'tstw-qd))) "treesitter-thumb" :exit t)
  ("x" (funcall (cs/ae (cs/obj 'xml))) "xml" :exit t)
  ("y" (funcall (cs/ae (cs/obj 'symbol))) "symbol" :exit t)
  ("p" (funcall (cs/ae (cs/obj 'paragraph))) "paragraph" :exit t)
  ("S" (funcall (cs/ae (cs/obj 'sentence))) "sentence" :exit t)
  ("B" (funcall (cs/ae (cs/obj 'buffer))) "buffer" :exit t)
  ;; TODO - more objects -- defun/definition, comment, function arg, need to actually implement tree-sitter stuff, ...


  ;; Modifiers -- maybe these should have a separate map, but that adds verbosity, and I'm not yet certain there are enough objects and modifiers to warrant splitting -- I can have a prefix within this map for infrequent things, and I can always add another separateprefix map.
  ("e" (funcall (cs/add (cs/mod 'location-within 'end))) "end" :exit nil)
  ("I" (funcall (cs/add (cs/mod 'tree-inner t))) "tree-inner" :exit nil)
  ("u" (funcall (cs/add (cs/mod 'tree-vertical 'up))) "up" :exit nil)
  ("d" (funcall (cs/add (cs/mod 'tree-vertical 'down))) "down" :exit nil)
  ("r" TODO_register-select "register" :exit nil)
  (" " (funcall (cs/add (cs/mod 'surrounding-space 'surrounding-space))) "surrounding-space" :exit nil)
  ("g" (funcall (cs/add (cs/mod 'current-line-only 'current-line-only))) "current-line-only" :exit nil)


  ("\"" (funcall (cs/ae (cs/mod 'delimiter "\"") (cs/obj 'sptw))) "\"" :exit t)
  ("(" (funcall (cs/ae (cs/mod 'delimiter "(") (cs/obj 'sptw))) "()" :exit t)
  (")" (funcall (cs/ae (cs/mod 'delimiter "(") (cs/obj 'sptw))) "()" :exit t)
  ("[" (funcall (cs/ae (cs/mod 'delimiter "[") (cs/obj 'sptw))) "[]" :exit t)
  ("]" (funcall (cs/ae (cs/mod 'delimiter "[") (cs/obj 'sptw))) "[]" :exit t)
  ("{" (funcall (cs/ae (cs/mod 'delimiter "{") (cs/obj 'sptw))) "{}" :exit t)
  ("}" (funcall (cs/ae (cs/mod 'delimiter "{") (cs/obj 'sptw))) "{}" :exit t)
  ("«" (funcall (cs/ae (cs/mod 'delimiter "«") (cs/obj 'sptw))) "«»" :exit t)
  ("»" (funcall (cs/ae (cs/mod 'delimiter "«") (cs/obj 'sptw))) "«»" :exit t)
  ("“" (funcall (cs/ae (cs/mod 'delimiter "“") (cs/obj 'sptw))) "“”" :exit t)
  ("”" (funcall (cs/ae (cs/mod 'delimiter "“") (cs/obj 'sptw))) "“”" :exit t)
  ("⟅" (funcall (cs/ae (cs/mod 'delimiter "⟅") (cs/obj 'sptw))) "⟅⟆" :exit t)
  ("⟆" (funcall (cs/ae (cs/mod 'delimiter "⟅") (cs/obj 'sptw))) "⟅⟆" :exit t)

  ("q" (funcall (cs/ae (cs/obj 'non-matching-bad-object-just-to-clear-things-out)))
   "quit-cancel-sentence" :exit t)

  )




(emmap "e" (lambda (n) (interactive "p")
             (funcall (cs/add (cs/mod 'direction 'forward)) n)
             (object-select/body)))
(emmap "o" (lambda (n) (interactive "p")
             (funcall (cs/add (cs/mod 'direction 'backward)) n)
             (object-select/body)))


(emmap "i" (lambda (n) (interactive "p")
             (if (null command-sentence-current-sentence)
                 (estate-insert-state) ;; TODO - handle number
               (progn
                 (funcall (cs/add (cs/mod 'direction nil)
                                  (cs/mod 'expand-region 'inner))
                          n)
                 (object-select/body)))))
(evmap "i" (lambda (n) (interactive "p")
             (progn
               (funcall (cs/add (cs/mod 'direction nil)
                                (cs/mod 'expand-region 'inner))
                        n)
               (object-select/body))))
(emmap "a" (lambda (n) (interactive "p")
             (funcall (cs/add (cs/mod 'direction nil)
                              (cs/mod 'expand-region t))
                      n)
             (object-select/body)))

;; Quick select any delimiter
(emmap "(" (cs/ae (cs/mod 'direction nil)
                  (cs/mod 'expand-region t)
                  (cs/mod 'delimiter 'any)
                  (cs/obj 'sptw)))
(emmap ")" (cs/ae (cs/mod 'direction nil)
                  (cs/mod 'expand-region 'inner)
                  (cs/mod 'delimiter 'any)
                  (cs/obj 'sptw)))


;; TODO - still need: goto-column, back-to-indentation, line start, line end, stay-in-line, line-end-but-before-trailing-space, splice, promote, demote, function arg object, ...




;; Mouse keys...
;; mouse-1 is left-click.  There is also drag-mouse-1, double-mouse-1, triple-mouse-1
;; drag-mouse-1 is left-click drag.
;; mouse-3 is right-click
;; mouse-4 is scroll-wheel up
;; mouse-5 is scroll-wheel down
;; You can also do combos with special regions where the mouse is, eg. [mode-line mouse-1] to bind to something different when clicking in the mode-line.
(emmap [mouse-4] 'mwheel-scroll)
(emmap [mouse-5] 'mwheel-scroll)
;;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
;;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))


(emmap "f" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'repeatable-motion-repeat)))
(emmap "F" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'repeatable-motion-repeat)))

;; t map
(emmap "tt" 'temp-key-map)

(emmap "tia" 'switch-to-buffer)
(emmap "tic" 'kill-buffer-or-quit-emacs)
(emmap " tica" 'save-buffers-kill-terminal)
(emmap "tis" 'save-buffer)
(emmap " tisa" 'baddd-write-all)
(emmap "tie" 'save-and-kill-buffer-and-maybe-quit-emacs)
(emmap " tiea" 'baddd-save-and-quit)
(emmap "tip" 'ffap/no-confirm)
;; TODO - I want to switch from "tif" and friends to "tf*" for the variety of ways I want to find files.
(emmap "tif" 'ido-ffap-no)
(emmap " tifd" 'ido-find-file-from-pwd)
(emmap " tiff" 'ffap/no-confirm)
(emmap "tiw" 'next-buffer-no-star)
(emmap "tib" 'prev-buffer-no-star)
(emmap " tiwd" 'next-dirty-buffer-no-star)
(emmap " tibd" 'prev-dirty-buffer-no-star)
(emmap "tff" 'ido-ffap-no)
(emmap "tfp" 'ffap/no-confirm)
(emmap "tfh" 'ff-find-other-file) ; IE switch between header and source file for C/C++
(emmap "tfd" 'ido-find-file-from-pwd)
(emmap "tfg" (lambda () (interactive) (require 'helm-projectile) (helm-projectile)))

(emmap "th" 'my-window-map/body)
(autoload 'projectile-command-map "projectile-conf" "" t 'keymap)
(emmap "tp" 'projectile-command-map)
(emmap "tr" 'baddd-use-register)

;; "ta" will be an assortment of handy stuff...
(ecmap "tac" 'comment-region)
(ecmap "taC" 'uncomment-region)
(ecmap "tam" (lambda () (interactive) (exchange-point-and-mark)))
(ecmap "tad" 'insert-date)
(ecmap "taD" 'insert-date-time)
(ecmap "tara" (lambda () (interactive) (require 'alternate-region) (alternate-region-activate)))
(ecmap "tars" (lambda () (interactive) (require 'alternate-region) (alternate-region-swap)))
(ecmap "tarc" (lambda () (interactive) (require 'alternate-region) (alternate-region-cycle)))

;; "ts" will stand for "toggle setting"
(emmap "ts"
       (defhydra settings-toggle (:foreign-keys warn :exit t) "Toggle:"
         ("p" smartparens-mode "smartparens")
         ("b" (lambda () (interactive) (require 'blamer (blamer-mode))) "git blame")
         ("w" whitespace "whitespace")
         ("C" (lambda () (interactive) (require 'rainbow-mode) (rainbow-mode)) "#aabbcc")
         ("c" company-mode "company")
         ("t" toggle-truncate-lines "trunc")
         ("i" toggle-case-fold-search "/? case")
         ("W" toggle-wrap-scan "search-wrap")
         ("f" flycheck-mode "flycheck")
         ("F" display-fill-column-indicator-mode "fill-col")
         ("s" flyspell-mode "flyspell")
         ("S" flyspell-prog-mode "flyspell-prog")
                                        ;("e" electric-indent-mode "el.indent")
         ("d" rainbow-delimiters-mode "rainbow{}")
         ("r" linum-relative-toggle "linum-rel")
         ("h" isearch-exit "clear-search-highlight")
         ;;("h" baddd-search-highlight-persist-remove-all "search-highlight-now")
         ;;("H" baddd-search-highlight-persist "search-highlight-ever")
         ("M" menu-bar-mode "menu-bar")
         ("l" lsp-lens-mode "lsp-lens") ;; lsp-lens is the thing that shows eg. haskell imports in an overlay
         ("m" (lambda () (interactive) (menu-bar-mode 1) (menu-bar-open)) "menu-open")
         ("n" display-line-numbers-mode "line-numbers")
         ("I" indent-guide-mode "indent-guide")
         ("x" wgh/racket-xp-pre-redisplay-toggle "racket-xp-hl")
         ))

(emmap "tl"
       (defhydra list-stuff-map (:foreign-keys warn :exit t) "List:"
         ("b" list-buffers "buffers")
         ("m" (with-evil 'evil-show-marks) "marks")
         ("M" bookmark-bmenu-list "bookmarks")
                                        ;("tlk" 'list-keymaps) ; TODO - make this function
         ("c" list-colors-display "colors")
         ("f" list-faces-display "faces")
         ("r" (with-evil 'evil-show-registers) "registers")
         ))
;; TODO - list jumps, maybe

;; s map
;;(ecmap "ss" 'baddd-substitute)
;;(ecmap "sS" 'baddd-change-whole-line)
;;(baddd-define-key 'visual baddd-surround-mode-map "s" nil)
;;(baddd-define-key 'visual baddd-surround-mode-map "S" 'ignore)
;;(defun surround-region-with-parens (beg end)
;;  (interactive "r")
;;  (baddd-surround-region beg end nil ?\)))
(evmap "ss" 'baddd-surround-region)
(evmap "sS" 'baddd-Surround-region)
(evmap "sh" 'shell-command-on-region)
(ecmap "sh" 'shell-command)
(emmap "sa" 'baddd-ex)
(ecmap "s)" 'eval-last-sexp)
(evmap "s)" 'eval-region)
(evmap "s/" (kbd ":s/ ")) ; TODO - fix this...
(ecmap "sm" (with-evil 'evil-set-marker))
(ecmap "sM" 'bookmark-set)
(ecmap "sg" (cs/ae (cs/obj 'goto-marker)))
(ecmap "sG" 'bookmark-jump)
(eimap (kbd "M-c") 'helm-M-x)
(emmap (kbd "M-c") 'helm-M-x)
(global-set-key (kbd "M-c") 'helm-M-x)
(emmap "sx" 'eval-expression)

;;(evmap (kbd "C-s") 'yas-insert-with-region)

;; command modes and macros

;; (emmap "-" (lambda (n)
;;              (interactive "p")
;;              (message "use z")
;;              ;;(call-interactively 'helm-M-x)
;;              ))
(emmap "z" 'helm-M-x)
;;(emmap "|" 'execute-extended-command)
(emmap "|" 'eval-expression)
(emmap "_" 'eval-expression)
(emmap "Q" 'call-last-kbd-macro)
;; Movement
(ecmap "m" nil) ;;;;;;;;;; m will be my prefix for mode-specific bindings
;; everything in motion state is pulled into normal state
(emmap "+" 'baddd-repeat-find-char)
(emmap "~" 'baddd-repeat-find-char-reverse)
(emmap "J" 'baddd-window-bottom)
(emmap "K" 'baddd-window-top)
(emmap "{" 'backward-sexp)
(emmap "}" 'forward-sexp)
(emmap "[" 'backward-list)
(emmap "]" 'forward-list)
(emmap (kbd "C-z") 'suspend-frame)
(eimap (kbd "C-z") 'suspend-frame)

(epmap " " 'rmo/pscroll-down-half)
(epmap "j" 'rmo/pscroll-down-half)
(epmap "k" 'rmo/pscroll-up-full)
(epmap "J" 'rmo/pscroll-down-line)
(epmap "K" 'rmo/pscroll-up-line)
(epmap "sj" 'rmo/pscroll-down-full)
(epmap "sk" 'rmo/pscroll-up-full)
(epmap "e" 'estate-command-state)
;; if I start in pager mode, this gets remapped to quit
(epmap "q" 'estate-command-state)
(emmap "to" 'estate-pager-state)


;; space map
(emmap "sj" 'rmo/pscroll-down-half)
(emmap "sk" 'rmo/pscroll-up-half)
(emmap "sf" 'fold-toggle-wgh)
(emmap "sF" 'fold-toggle-wgh-all)
(emmap " /"
       (myhydradef search-hydra
                   ("s" helm-swoop "swoop")
                   ("a" helm-multi-swoop-all "multi-swoop all")
                   ("m" helm-multi-swoop "multi-swoop")
                   ("r" wgh/fzf-repo "fzf-repo")
                   ))
(emmap " -h" 'helm-M-x)
(emmap " &g" 'baddd-ex-repeat-global-substitute)
(emmap " va" 'mark-whole-buffer)

;; Joining
;; TODO -- make a better mapping for this.  I should make my prefixes be mnemonic or something...
;;         for instance, g<key> is mostly navigation... t... mosty has window stuff in th... space is mostly one handed
;;         navigation aside from this one
(defun join-line/default-forward (arg)
  (interactive "P")
  (join-line (not arg)))
(emmap " j"
       (myhydradef j-hydra
                   ("l" join-line/default-forward "join lines")
                   ("w" baddd-join-whitespace "join whitespace")
                   ))


;; input mode
(eimap (kbd "DEL") 'delete-backward-char) ; remap away from the baddd-version backspace
(eimap "\C-v" #'quoted-insert) ; more vim-like
;;(eimap "\M-r" 'baddd-paste-from-register)
(eimap "\M-w" 'baddd-window-map)

;; helm map to match what I've got going in zsh with zaw...
(define-prefix-command 'meta-space-map)
(global-set-key (kbd "M-SPC") 'meta-space-map)
(define-key meta-space-map " " 'helm-helm-commands)
(define-key meta-space-map (kbd "RET") 'helm-helm-commands)
(define-key meta-space-map "c" 'helm-M-x)
(define-key meta-space-map "p" 'helm-browse-project)
(define-key meta-space-map "g" 'helm-do-grep)

(myhydradef completer-map
            ("h" hippie-expand "hippie")
            ("n" baddd-complete-next "vim-n")
            ("p" baddd-complete-previous "vim-p")
            ("f" he-expand-file-name "file")
            ("l" he-expand-lisp-symbol "lisp")
            ("s" yas-expand "yas")
            )
(eimap "\M-h" 'completer-map/body)
(eimap (kbd "C-SPC TAB") 'completer-map/body)
(eimap (kbd "C-@ TAB") 'completer-map/body)
(eimap (kbd "TAB") 'company-complete-common-wgh)
;; put indentation on something...
;;(define-key 'completer-map (kbd "TAB") 'indent-for-tab-command)
(eimap (kbd "<backtab>") 'indent-for-tab-command)

(global-set-key (kbd "C-\\") 'baddd-execute-in-normal-state)

;; Default mode settings
;;(setq baddd-normal-state-modes (append baddd-emacs-state-modes baddd-normal-state-modes))
;;(setq baddd-emacs-state-modes nil)
;;(setq baddd-insert-state-modes (cons 'racket-repl-mode baddd-insert-state-modes))


(define-key tty-menu-navigation-map "j" 'tty-menu-next-item)
(define-key tty-menu-navigation-map "k" 'tty-menu-prev-item)
(define-key tty-menu-navigation-map "h" 'tty-menu-prev-menu)
(define-key tty-menu-navigation-map "l" 'tty-menu-next-menu)

(define-key help-map "\C-h" 'describe-prefix-bindings)

;;(define-key baddd-ex-completion-map "\C-a" 'move-beginning-of-line)
;;(define-key baddd-ex-completion-map "\C-e" 'move-end-of-line)
;;(define-key baddd-ex-completion-map "\C-d" 'delete-char)
;;(define-key baddd-ex-completion-map "\C-k" 'kill-line)
;;(define-key baddd-ex-completion-map "\C-p" 'previous-complete-history-element)
;;(define-key baddd-ex-completion-map "\C-n" 'next-complete-history-element)
;;(define-key baddd-ex-completion-map "\C-f" 'forward-char)
;;(define-key baddd-ex-completion-map "\C-b" 'backward-char)
;;(define-key baddd-ex-completion-map "\M-r" 'baddd-paste-from-register)


(defun my-isearch-bor-exit ()
  "Ensure point is at beginning of isearch result and exit."
  ;; Copied from https://emacs.stackexchange.com/questions/74339/how-to-leave-cursor-at-beginning-of-searched-text-in-isearch
  (interactive)
  (when (< isearch-other-end (point))
    (goto-char isearch-other-end))
  (call-interactively 'isearch-exit))

(defun my-isearch-eor-exit ()
  "Ensure point is at end of isearch result and exit."
  ;; Copied from https://emacs.stackexchange.com/questions/74339/how-to-leave-cursor-at-beginning-of-searched-text-in-isearch
  (interactive)
  (when (>= isearch-other-end (point))
    (goto-char isearch-other-end))
  (call-interactively 'isearch-exit))

(define-key isearch-mode-map "\C-g" 'isearch-abort-abort-gosh-darn-it)
(define-key isearch-mode-map (kbd "RET") (lambda () (interactive) (setq wgh/isearch-go-part 'beginning) (call-interactively 'my-isearch-bor-exit)))
(define-key isearch-mode-map "\C-j" (lambda () (interactive) (setq wgh/isearch-go-part 'end) (call-interactively 'my-isearch-eor-exit)))


;; Without defining these, I get terminal bell events when I move the mouse around on the header line / mode line.
(global-set-key (kbd "<header-line><mouse-movement>") 'ignore)
(global-set-key (kbd "<mode-line><mouse-movement>") 'ignore)


(message "at end of keys.el")
