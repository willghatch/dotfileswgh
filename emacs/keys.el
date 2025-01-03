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

(load-library "text-object-stuff")
(load-library "tree-walk-smartparens-integration.el")

;; State for repeating isearch -- my key to start isearch sets this so that the repeat keys go the right direction.
(setq wgh/isearch-repeat-forward-p t)
;; State for isearch repetition -- values 'beginning or 'end.
;; TODO - I would like a system for writing an escape in the search for where the cursor should go.
(setq wgh/isearch-go-part 'beginning)

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

;(ecmap "j" 'wgh/forward-line-keep-column/qd)
;(ecmap "k" 'wgh/backward-line-keep-column/qd)
;(repeatable-motion-define-pair 'forward-line 'previous-line)
(defun wgh/next-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (next-line arg)))
(defun wgh/prev-line (&optional arg)
  (interactive "p")
  (let ((line-move-visual nil))
    (previous-line arg)))
(repeatable-motion-define-pair 'wgh/next-line 'wgh/prev-line)
(ecmap "i" 'estate-insert-state)
(ecmap "\M-c" 'execute-extended-command)
(ecmap "\M-r" 'estate-mode)

(evmap "\C-c" 'estate-command-state)

(eimap "\C-c" 'estate-command-state)
(eimap "\C-l" 'estate-command-state)
(eimap "\M-c" 'execute-extended-command)
(eimap "\C-w" 'ignore) ;; by default bound to kill-region.  I might map it to delete-backward-word, but I also don't want to get used to using it such that I accidentally use it in a web browser, since browsers are both extremely necessary, in fact using multiple browsers is more-or-less necessary, and none of them have reasonable allowance for user customization of keyboard shortcuts.
(eimap "\C-t" 'ignore)


;; Wow.  I've stopped using this organically over time due to inherent issues in timing based key chords.  But it turns out that removing this single require cut my load time by nearly half.
;(require 'key-chord)
;(key-chord-mode 1)
;(key-chord-define estate-insert-keymap (kbd "kj") 'estate-command-state)


(progn
     (repeatable-motion-define-pair 'sptw-forward-sibling-beginning 'sptw-backward-sibling-beginning)
     (repeatable-motion-define-pair 'sptw-forward-sibling-end 'sptw-backward-sibling-end)
     (repeatable-motion-define-pair 'sptw-up-parent-beginning 'sptw-down-first-child-beginning)
     (repeatable-motion-define 'sptw-up-parent-end 'sptw-down-first-child-beginning)
     (repeatable-motion-define 'sptw-down-last-child-end 'sptw-up-parent-beginning)
     ;(repeatable-motion-define-pair 'sptw-forward-sexp-in-supersexp 'sptw-backward-sexp-in-supersexp)
     )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dump of my old keys to gradually massage them and see if I will like this...

(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))

;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun tkmap (keys func)
  (nobreak-define-key temp-key-map keys func))

(ecmap "a" (lambda () (interactive) (message "don't append, use i for insert")))
(evmap "aw" 'wgh/expand-region-to-vi-like-word)
(evmap "aW" 'wgh/expand-region-to-word) ;; TODO - also define forward/back emacs-style word
(evmap "as" 'wgh/expand-region-to-sentence)
(evmap "aS" 'wgh/expand-region-to-symbol)
(evmap "ap" 'wgh/expand-region-to-paragraph)
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
(evmap "al" (lambda () (interactive) (wgh/expand-region-to-fill-lines t)))
(evmap "il" (lambda () (interactive) (wgh/expand-region-to-fill-lines nil)))
(evmap "ab" 'sptw-expand-region)
(evmap "ib" 'sptw-expand-region/children-region)
(evmap "a\"" (lambda () (interactive) (sptw-expand-region-to-delimiter "\"")))
(evmap "i\"" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "\"")))
(evmap "a)" (lambda () (interactive) (sptw-expand-region-to-delimiter "(")))
(evmap "i)" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "(")))
(evmap "a(" (lambda () (interactive) (sptw-expand-region-to-delimiter "(")))
(evmap "i(" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "(")))
(evmap "a]" (lambda () (interactive) (sptw-expand-region-to-delimiter "[")))
(evmap "i]" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "[")))
(evmap "a[" (lambda () (interactive) (sptw-expand-region-to-delimiter "[")))
(evmap "i[" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "[")))
(evmap "a}" (lambda () (interactive) (sptw-expand-region-to-delimiter "{")))
(evmap "i}" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "{")))
(evmap "a{" (lambda () (interactive) (sptw-expand-region-to-delimiter "{")))
(evmap "i{" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "{")))
(evmap "a“" (lambda () (interactive) (sptw-expand-region-to-delimiter "“")))
(evmap "i“" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "“")))
(evmap "a”" (lambda () (interactive) (sptw-expand-region-to-delimiter "“")))
(evmap "i”" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "“")))
(evmap "a«" (lambda () (interactive) (sptw-expand-region-to-delimiter "«")))
(evmap "i«" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "«")))
(evmap "a»" (lambda () (interactive) (sptw-expand-region-to-delimiter "«")))
(evmap "i»" (lambda () (interactive) (sptw-expand-region-to-delimiter/children-region "«")))
(emmap "(" 'sptw-expand-region-to-any-delimiter)
(emmap ")" 'sptw-expand-region/children-region)
(evmap "ai" 'indent-tree-expand-region)
(evmap "ii" 'indent-tree-expand-region/children-region)
(ecmap "A" (lambda () (interactive)
             (progn (move-end-of-line nil) (estate-insert-state))))
(evmap "A" (lambda () (interactive) (progn (goto-char (region-end))
                                           (deactivate-mark)
                                           (estate-insert-state))))
(ecmap "c" (lambda () (interactive) (message "c not really implemented yet")))
(evmap "c" (lambda () (interactive) (estate-state-with-change-group
                                     'insert
                                     (lambda () (kill-region (region-beginning)
                                                             (region-end))))))
(ecmap "C" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "C" (lambda () (interactive (progn (kill-region (region-beginning)
                                                       (region-end))
                                          (estate-insert-state)))))
(ecmap "d" (lambda () (interactive) (message "d not really implemented yet")))
(evmap "d" (lambda () (interactive)
             (estate-visual-execution-helper (lambda () (delete-region (region-beginning) (region-end))))
             ;; Even without putting this switch to command state, kill-region disables the region and exits... I'm not sure whether that's a bug or a feature.
             ;;(estate-command-state)
             ))
(ecmap "D" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "D" (lambda () (interactive) (progn (kill-region nil nil t))))
(ecmap "i" (lambda () (interactive) (estate-insert-state)))
(ecmap "I" (lambda () (interactive)
             (progn (back-to-indentation) (estate-insert-state))))
(evmap "I" (lambda () (interactive) (progn (goto-char (region-beginning))
                                           (deactivate-mark)
                                           (estate-insert-state))))

(emmap "h" (lambda () (interactive) (message "use oc")))
(emmap "l" (lambda () (interactive) (message "use ec")))

;(ecmap "O" 'baddd-open-above)
;; I really prefer vim's terminology for copy/paste...
;(ecmap "p" 'yank)
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
(ecmap "h" 'estate-record-quick-keyboard-macro-to-buffer-change)

(ecmap "@" 'wgh/call-macro-by-name)
(ecmap "x" 'delete-char)
(defun delete-char-backward (&optional n)
  (interactive "p")
  (delete-char (- n)))
(ecmap "X" 'delete-char-backward)
(emmap "y" (lambda () (interactive)
             (message "y not fully implemented yet")))
;(evmap "y" (lambda () (interactive)
;             (save-excursion
;               (estate-visual-execution-helper
;                (lambda () (kill-ring-save (region-beginning) (region-end)))))))
(evmap "y" 'estate-copy)
(ecmap "Y" (lambda () (interactive) (message "Y not yet implemented")))
;(ecmap "&" 'baddd-ex-repeat-substitute)
(ecmap "gq" (with-evil 'evil-fill-and-move))
(ecmap "gw" (with-evil 'evil-fill))
;;(ecmap "zo" 'baddd-open-fold)
;;(ecmap "zc" 'baddd-close-fold)
;;(ecmap "za" 'baddd-toggle-fold)
;;(ecmap "zr" 'baddd-open-folds)
;;(ecmap "zm" 'baddd-close-folds)
;;(ecmap "z=" 'ispell-word)
;(ecmap "\C-n" 'baddd-paste-pop-next)
;(ecmap "\C-p" 'baddd-paste-pop)
(ecmap "\C-t" 'pop-tag-mark)
;(ecmap (kbd "C-.") 'baddd-repeat-pop)
;(ecmap (kbd "M-.") 'baddd-repeat-pop-next)
(ecmap "." 'baddd-repeat) ;; TODO - repeat is an important evil-mode thing to replace!
(ecmap "\"" 'baddd-use-register)
(ecmap "~" (with-evil 'evil-invert-char))
;(ecmap "=" 'baddd-indent)
(ecmap "<" (with-evil 'evil-shift-left))
(ecmap ">" (with-evil 'evil-shift-right))
;(ecmap "ZZ" 'baddd-save-modified-and-close)
;(ecmap "ZQ" 'baddd-quit)
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
;(baddd-redirect-digit-argument baddd-motion-state-map "0" 'baddd-beginning-of-line)
;(emmap "0" 'baddd-beginning-of-line)
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
;(emmap "H" 'baddd-window-top)
(emmap "j" 'rmo-c/wgh/next-line)
(emmap "k" 'rmo-c/wgh/prev-line)
;(emmap "K" 'baddd-lookup)
;(emmap "L" 'baddd-window-bottom)
;(emmap "M" 'baddd-window-middle)
(defun wgh/isearch-repeat (&optional count)
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
(emmap "n" (lambda (&optional n) (interactive "p") (wgh/isearch-repeat (or n 1))))
(emmap "N" (lambda (&optional n) (interactive "p") (wgh/isearch-repeat (- (or n 1)))))

;(emmap "w" 'rmo/wgh/forward-word-beginning)
;(emmap "b" 'rmo/wgh/backward-word-beginning)
(emmap "w" 'rmo/wgh/forward-vi-like-word-beginning)
(emmap "b" 'rmo/wgh/backward-vi-like-word-beginning)

;(emmap "W" 'rmo/baddd-forward-WORD-begin)
;(emmap "B" 'rmo/baddd-backward-WORD-begin)
;(emmap "ge" 'rmo/baddd-backward-word-end)
;(emmap "gE" 'rmo/baddd-backward-WORD-end)
(emmap "gg" 'vilish/goto-line/beginning)
;(emmap "g_" 'baddd-last-non-blank)
(emmap "g\C-]" 'find-tag)
(emmap "{" 'rmo/wgh/backward-paragraph-beginning)
(emmap "}" 'rmo/wgh/forward-paragraph-beginning)
(emmap "#" 'rmo/baddd-search-word-backward)
(emmap "g#" 'rmo/baddd-search-unbounded-word-backward)
;(emmap "$" 'end-of-line)
(emmap "%" 'sptw-move-to-other-end-of-sexp)
(emmap "`" (with-evil 'evil-goto-mark))
(emmap "'" (with-evil 'evil-goto-mark-line))
;(emmap "]]" 'rmo/baddd-forward-section-begin)
;(emmap "][" 'rmo/baddd-forward-section-end)
;(emmap "[[" 'rmo/baddd-backward-section-begin)
;(emmap "[]" 'rmo/baddd-backward-section-end)
;(emmap "[(" 'rmo/baddd-previous-open-paren)
;(emmap "])" 'rmo/baddd-next-close-paren)
;(emmap "[{" 'rmo/baddd-previous-open-brace)
;(emmap "]}" 'rmo/baddd-next-close-brace)
(emmap "*" 'rmo/baddd-search-word-forward)
(emmap "g*" 'rmo/baddd-search-unbounded-word-forward)
;(emmap "," 'baddd-repeat-find-char-reverse)

;; TODO - how to configure isearch to be more like evil-mode search without importing all of evil-mode
(emmap "/" (lambda () (interactive) (setq wgh/isearch-repeat-forward-p t) (call-interactively 'isearch-forward)))
(emmap "?" (lambda () (interactive) (setq wgh/isearch-repeat-forward-p nil) (call-interactively 'isearch-backward)))
(emmap ";" 'er/expand-region)
(emmap "^" 'baddd-first-non-blank) ;; TODO - re-impl using back-to-indentation...
;(emmap "+" 'baddd-next-line-first-non-blank)
;(emmap "_" 'baddd-next-line-1-first-non-blank)
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

;(define-key baddd-replace-state-map (kbd "DEL") 'baddd-replace-backspace)
;(define-key baddd-replace-state-map [escape] 'baddd-normal-state)

;;; Minibuffer

;(define-key minibuffer-local-map "\C-p" 'baddd-complete-next)
;(define-key minibuffer-local-map "\C-n" 'baddd-complete-previous)
;(define-key minibuffer-local-map "\C-x\C-p" 'baddd-complete-next-line)
;(define-key minibuffer-local-map "\C-x\C-n" 'baddd-complete-previous-line)

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
;(define-key baddd-ex-search-keymap "\d" #'baddd-ex-delete-backward-char)
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
;(define-key baddd-inner-text-objects-map "a" 'baddd-inner-arg)
;(define-key baddd-outer-text-objects-map "a" 'baddd-outer-arg)
;(define-key baddd-inner-text-objects-map "c" 'baddd-cp-inner-comment)
;(define-key baddd-outer-text-objects-map "c" 'baddd-cp-a-comment)
;(define-key baddd-inner-text-objects-map "d" 'baddd-cp-inner-defun)
;(define-key baddd-outer-text-objects-map "d" 'baddd-cp-a-defun)
;(define-key baddd-inner-text-objects-map "b" 'baddd-textobj-anyblock-inner-block)
;(define-key baddd-outer-text-objects-map "b" 'baddd-textobj-anyblock-a-block)
;(define-key baddd-inner-text-objects-map "i" 'indent-tree-inner)
;(define-key baddd-outer-text-objects-map "i" 'indent-tree-outer)
;;; These are also on t, which I'm already used to, but to fix the tree thing...
;(define-key baddd-inner-text-objects-map "x" 'baddd-inner-tag)
;(define-key baddd-outer-text-objects-map "x" 'baddd-a-tag)

;; Normal state switch!
(eimap (kbd "C-c") 'estate-command-state)
(emmap (kbd "C-c") 'ignore)

; Keys unbound and reserved for future use - bind to nop so they don't input
(emmap (kbd "RET") 'ignore)
(emmap "S" 'ignore)
(emmap "T" 'ignore)


;; I'm not sure a better place to put this...
(ecmap (kbd "TAB") 'sp-indent-defun)

; g map
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

; o and e maps - o is left/back, e is right/forward
;(emmap "ee" 'rmo/wgh/forward-word-end)
;(emmap "oe" 'rmo/wgh/backward-word-end)
(emmap "ee" 'rmo/wgh/forward-vi-like-word-end)
(emmap "oe" 'rmo/wgh/backward-vi-like-word-end)
(emmap "eE" 'rmo/baddd-forward-WORD-end)
(emmap "oE" 'rmo/baddd-backward-WORD-end)
(defun vilish-open-line-below ()
  ;; TODO - evil takes a numeric argument, and when you are done entering text it copies that line N times.  I never use that feature though, so... maybe I don't care.
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (end-of-line)
                                    (open-line 1)
                                    (forward-line))))
(defun vilish-open-line-above ()
  (interactive)
  (estate-insert-state-with-thunk (lambda ()
                                    (beginning-of-line)
                                    (open-line 1))))
(evmap "eo" (lambda () (interactive) (exchange-point-and-mark)))
(ecmap "eo" (lambda () (interactive) (vilish-open-line-below)))
(evmap "oo" (lambda () (interactive) (exchange-point-and-mark)))
(ecmap "oo" (lambda () (interactive) (vilish-open-line-above)))
;(evmap "o" nil)
;(evmap "oo" 'exchange-point-and-mark)
;(evmap "eo" 'exchange-point-and-mark)
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
(emmap "ec" 'rmo/wgh/forward-char/no-line-wrap)
(emmap "oc" 'rmo/wgh/backward-char/no-line-wrap)
;(emmap "ec" 'rmo/forward-char)
;(emmap "oc" 'rmo/backward-char)
;(emmap "ed" 'rmo/baddd-next-close-paren)
;(emmap "od" 'rmo/baddd-previous-open-paren)
;(emmap "ed" 'rmo/baddd-next-close-brace)
;(emmap "od" 'rmo/baddd-previous-open-brace)
(emmap "ed" 'rmo/sptw-down-last-child-end)
(emmap "od" 'rmo/sptw-down-first-child-beginning)
(emmap "eg" 'rmo/sptw-up-parent-end)
(emmap "og" 'rmo/sptw-up-parent-beginning)
;; Note that these two are just sp, not wrapped
(emmap "eG" 'rmo/sp-end-of-sexp)
(emmap "oG" 'rmo/sp-beginning-of-sexp)
(emmap "eh" 'rmo/sptw-forward-sibling-beginning)
(emmap "oh" 'rmo/sptw-backward-sibling-beginning)
(emmap "em" 'rmo/sptw-forward-sibling-end)
(emmap "om" 'rmo/sptw-backward-sibling-end)
;(emmap "eH" 'rmo/sptw-forward-sexp-in-supersexp)
;(emmap "oH" 'rmo/sptw-backward-sexp-in-supersexp)
(emmap "ea" 'rmo/baddd-forward-arg)
(emmap "oa" 'rmo/baddd-backward-arg)
(emmap "ew" 'rmo/baddd-forward-little-word-begin)
(emmap "ow" 'rmo/baddd-backward-little-word-begin)
(emmap "eW" 'rmo/baddd-forward-little-word-end)
(emmap "oW" 'rmo/baddd-backward-little-word-end)
(emmap "es" 'rmo/wgh/forward-sentence-beginning)
(emmap "os" 'rmo/wgh/backward-sentence-beginning)
(emmap "ep" 'rmo/wgh/forward-paragraph-beginning)
(emmap "op" 'rmo/wgh/backward-paragraph-beginning)
;(emmap "eS" 'rmo/baddd-forward-section-begin)
;(emmap "oS" 'rmo/baddd-backward-section-begin)
;(emmap "eP" 'rmo/baddd-forward-section-end)
;(emmap "oP" 'rmo/baddd-backward-section-end)
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

(emmap "et" 'rmo/wgh/find-char-beginning-in-line-forward)
(emmap "ot" 'rmo/wgh/find-char-beginning-in-line-backward)
(emmap "ef" 'rmo/wgh/find-char-end-in-line-forward)
(emmap "of" 'rmo/wgh/find-char-end-in-line-backward)

(defun goto-column (&optional n)
  (interactive "P")
  (move-to-column (or n 0)))
(defun goto-column/default-end (&optional n)
  (interactive "P")
  (if n (move-to-column n) (end-of-line)))
(emmap "ol" 'goto-column)
(emmap "o l" 'back-to-indentation)
(emmap "el" 'goto-column/default-end)
(emmap "oL" 'baddd-beginning-of-visual-line)
(emmap "eL" 'baddd-end-of-visual-line)
(emmap "eb" 'baddd-next-line-first-non-blank)
(emmap "ob" 'baddd-previous-line-first-non-blank)
(emmap "oB" 'baddd-first-non-blank)
(emmap "eB" 'baddd-next-line-1-first-non-blank)
;; TODO - I never use this binding, but symbol will be a useful thing when I make a more composable system...
(emmap "eO" 'rmo/wgh/forward-symbol-beginning)
(emmap "oO" 'rmo/wgh/backward-symbol-beginning)
(emmap "ei" 'rmo/indent-tree-forward-full-or-half-sibling)
(emmap "oi" 'rmo/indent-tree-backward-full-or-half-sibling)
(emmap "eI" 'rmo/indent-tree-down-to-first-child)
(emmap "oI" 'rmo/indent-tree-up-to-parent)
(emmap "ej" 'rmo/baddd-jump-forward)
(emmap "oj" 'rmo/baddd-jump-backward)
;; eu/ou for destructive subcommands!
(emmap "eus" 'sptw-forward-slurp)
(emmap "ous" 'sptw-backward-slurp)
(emmap "eub" 'sptw-forward-barf)
(emmap "oub" 'sptw-backward-barf)
;(emmap "euj" 'sptw-join-neighbor-sexp)
(emmap "ouj" 'sp-split-sexp)
;; this one doesn't really belong...
(emmap "eup" 'sptw-splice)
(emmap "oup" 'sptw-splice)

;; tree operations "en<op><tree-type>"
;; TODO - I should have a wrapper function defined that has various arguments to compose, and the keybinding section should just use that one function composed.  eg. (tree-op 'OP 'TREE-TYPE 'DIRECTION 'ANY-OTHER-INFO) but probably with keyword args, then probably have an extensible table that it looks up.  For any entries that aren't there, instead of using the ignore function I can print a descriptive message about which entries are not yet filled out.
;; TODO - rearrange this in whatever way is necessary to get hints as I go to remember what the options are
;; TODO - slurp/barf are not the only tree mutation operations I should have, eg. above I have "eu_" as "mutate forward" with things like join, split, splice, ... I should consider other tree operations and how they should fit in.
;; TODO - add insertion functions, eg. org-mode insert sibling header below/above.
;; "ens_" forward slurp
(emmap "ensp" 'sptw-forward-slurp)
(emmap "onsp" 'sptw-backward-slurp)
; TODO - indent tree slurp
(emmap "ensi" 'ignore)
(emmap "onsi" 'ignore)
(emmap "enso" 'wgh/outline-forward-slurp-heading)
(emmap "onso" 'ignore)
(emmap "ensx" 'ignore)
(emmap "onsx" 'ignore)
;; "enb_" forward barf
(emmap "enbp" 'sptw-forward-barf)
(emmap "onbp" 'sptw-backward-barf)
; TODO - indent tree barf
(emmap "enbi" 'ignore)
(emmap "onbi" 'ignore)
(emmap "enbo" 'wgh/outline-forward-barf-heading)
(emmap "onbo" 'ignore)
(emmap "enbx" 'ignore)
(emmap "onbx" 'ignore)
;; "enh_" forward sibling
(emmap "enhp" 'rmo/sptw-forward-sexp)
(emmap "onhp" 'rmo/sptw-backward-sexp)
(emmap "enhi" 'rmo/indent-tree-forward-sibling)
(emmap "onhi" 'rmo/indent-tree-backward-sibling)
(emmap "enho" 'rmo/outline-forward-same-level)
(emmap "onho" 'rmo/outline-backward-same-level)
;; TODO - make rmo versions
(emmap "enhx" 'on-xml-forward)
(emmap "onhx" 'on-xml-backward)
;; "enm_" forward sibling end
(emmap "enmp" 'rmo/sptw-forward-sexp-end)
(emmap "onmp" 'rmo/sptw-backward-sexp-end)
; TODO - indent tree moving by end of line
(emmap "enmi" 'ignore)
(emmap "onmi" 'ignore)
(emmap "enmo" 'ignore)
(emmap "onmo" 'ignore)
(emmap "enmx" 'on-xml-forward-end)
(emmap "onmx" 'on-xml-backward-end)
;; "enp_" up to parent start/end
(emmap "enpp" 'rmo/sptw-up-sexp-end)
(emmap "onpp" 'rmo/sptw-up-sexp)
(emmap "enpi" 'ignore)
(emmap "onpi" 'rmo/indent-tree-up-to-parent)
(emmap "enpo" 'ignore)
(emmap "onpo" 'rmo/outline-up-heading)
(emmap "enpx" 'on-xml-up-end)
(emmap "onpx" 'on-xml-up)
;; "enc_" down to first child / "onc_" down to last child
(emmap "encp" 'rmo/sptw-down-sexp)
(emmap "oncp" 'rmo/sptw-down-sexp-end)
(emmap "enci" 'rmo/indent-tree-down-to-first-child)
(emmap "onci" 'rmo/indent-tree-down-to-last-child)
(emmap "enco" 'rmo/wgh/outline-down-to-first-child)
(emmap "onco" 'rmo/wgh/outline-down-to-last-child)
(emmap "encx" 'ignore)
(emmap "oncx" 'ignore)
;; TODO - down to end of last child (on-parens-down-sexp-end)
;; TODO - to end/beginning of current tree element (IE swap between delimiters when available)
;; TODO - forward/back in parent sibling? (on-parens-forward-sexp-in-supersexp)
;; "ent_" inorder traversal
;; TODO - write inorder traversal for symex and xml
(emmap "entp" 'ignore)
(emmap "ontp" 'ignore)
(emmap "enti" 'rmo/indent-tree-inorder-traversal-forward)
(emmap "onti" 'rmo/indent-tree-inorder-traversal-backward)
(emmap "ento" 'rmo/wgh/outline-inorder-traversal-forward)
(emmap "onto" 'rmo/wgh/outline-inorder-traversal-backward)
(emmap "entx" 'ignore)
(emmap "ontx" 'ignore)
;; "end_" down to last descendant
(emmap "endp" 'ignore)
(emmap "ondp" 'ignore)
(emmap "endi" 'rmo/indent-tree-down-to-last-descendant)
(emmap "ondi" 'ignore)
(emmap "endo" 'rmo/wgh/outline-down-to-last-descendant)
(emmap "ondo" 'ignore)
;; "enw_" wrap/demote unwrap/promote
(emmap "enwp" 'ignore) ;; TODO - wrap with paren, the default wrapper
(emmap "onwp" 'ignore) ;; TODO - delete outer paren (of any type?  It's not symmetric, but maybe more useful?)
(emmap "enwi" 'indent-tree-demote)
(emmap "onwi" 'indent-tree-promote)
(emmap "enwo" 'outline-demote-subtree)
(emmap "onwo" 'outline-promote-subtree)
;; TODO - insertions, like wgh/org-add-heading-above/below

;; TODO - other useful tree operations:
;; * make new next sibling (useful in particular for trees without an end delimiter like indent trees or org-mode, especially to easily make a next sibling for the last sibling, because you can't go to its end delimiter.)  I think this means it goes to the sibling's location, enters any necessary stuff (eg. indentation or org-mode header bullets), and maybe enters insert mode.

;; TODO - tree text objects
;(define-key baddd-inner-text-objects-map "np" 'inner-parens-textobj)
;(define-key baddd-outer-text-objects-map "np" 'outer-parens-textobj)
;(define-key baddd-inner-text-objects-map "ni" 'indent-tree-inner)
;(define-key baddd-outer-text-objects-map "ni" 'indent-tree-outer)
;(define-key baddd-inner-text-objects-map "no" 'wgh/org-tree-inner)
;(define-key baddd-outer-text-objects-map "no" 'wgh/org-tree-outer)
;;;;


;; Mouse keys...
;; mouse-1 is left-click.  There is also drag-mouse-1, double-mouse-1, triple-mouse-1
;; drag-mouse-1 is left-click drag.
;; mouse-3 is right-click
;; mouse-4 is scroll-wheel up
;; mouse-5 is scroll-wheel down
;; You can also do combos with special regions where the mouse is, eg. [mode-line mouse-1] to bind to something different when clicking in the mode-line.
(emmap [mouse-4] 'mwheel-scroll)
(emmap [mouse-5] 'mwheel-scroll)
;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))


(emmap "f" 'repeatable-motion-forward)
(emmap "F" 'repeatable-motion-backward)

; t map
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
;; "ts" will stand for "toggle setting"
(ecmap "tac" 'comment-region)
(ecmap "taC" 'uncomment-region)
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
; TODO - list jumps, maybe

;; s map
;(ecmap "ss" 'baddd-substitute)
;(ecmap "sS" 'baddd-change-whole-line)
;(baddd-define-key 'visual baddd-surround-mode-map "s" nil)
;(baddd-define-key 'visual baddd-surround-mode-map "S" 'ignore)
;(defun surround-region-with-parens (beg end)
;  (interactive "r")
;  (baddd-surround-region beg end nil ?\)))
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
(ecmap "sg" (with-evil 'evil-goto-mark))
(ecmap "sG" 'bookmark-jump)
(eimap (kbd "M-c") 'helm-M-x)
(emmap (kbd "M-c") 'helm-M-x)
(global-set-key (kbd "M-c") 'helm-M-x)
(emmap "sx" 'eval-expression)

;(evmap (kbd "C-s") 'yas-insert-with-region)

;; command modes and macros

;; (emmap "-" (lambda (n)
;;              (interactive "p")
;;              (message "use z")
;;              ;;(call-interactively 'helm-M-x)
;;              ))
(emmap "z" 'helm-M-x)
;(emmap "|" 'execute-extended-command)
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


; space map
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
;(eimap "\M-r" 'baddd-paste-from-register)
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
;(define-key 'completer-map (kbd "TAB") 'indent-for-tab-command)
(eimap (kbd "<backtab>") 'indent-for-tab-command)

(global-set-key (kbd "C-\\") 'baddd-execute-in-normal-state)

;; Default mode settings
;(setq baddd-normal-state-modes (append baddd-emacs-state-modes baddd-normal-state-modes))
;(setq baddd-emacs-state-modes nil)
;(setq baddd-insert-state-modes (cons 'racket-repl-mode baddd-insert-state-modes))


(define-key tty-menu-navigation-map "j" 'tty-menu-next-item)
(define-key tty-menu-navigation-map "k" 'tty-menu-prev-item)
(define-key tty-menu-navigation-map "h" 'tty-menu-prev-menu)
(define-key tty-menu-navigation-map "l" 'tty-menu-next-menu)

(define-key help-map "\C-h" 'describe-prefix-bindings)

;(define-key baddd-ex-completion-map "\C-a" 'move-beginning-of-line)
;(define-key baddd-ex-completion-map "\C-e" 'move-end-of-line)
;(define-key baddd-ex-completion-map "\C-d" 'delete-char)
;(define-key baddd-ex-completion-map "\C-k" 'kill-line)
;(define-key baddd-ex-completion-map "\C-p" 'previous-complete-history-element)
;(define-key baddd-ex-completion-map "\C-n" 'next-complete-history-element)
;(define-key baddd-ex-completion-map "\C-f" 'forward-char)
;(define-key baddd-ex-completion-map "\C-b" 'backward-char)
;(define-key baddd-ex-completion-map "\M-r" 'baddd-paste-from-register)


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
