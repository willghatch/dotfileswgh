;;; -*- lexical-binding: t; -*-
;; TODO - things to think about re-mapping: folds (toggling, moving between visible ones), spell check (for individual words or for the buffer),
;; TODO - repeat a la vim dot, accessing old repetitions, register choice
;; TODO - search forward/backward for word or symbol or such at point, that's a nice single character function that vim has.
;; TODO - generic er/expand-region?
;; TODO - single command in insert state?  single command in command state while inserting?
;; TODO - ace jump?
;; TODO - Related to ace jump, Helix uses "g" as a prefix for goto commands, including an ace-jump-like thing.  I use it for going to first/last line a la vim, and I have go-to-definition under g, and I used to have ace jump under g (but just never got in the habit of using it), so maybe I should use g as a goto prefix, too.
;; TODO - I currently have some LSP functions under g, because I thought “this is related to go-to-definition”, so I put them there, but I would like to re-think command groupings some time soon.
;; TODO - functions for going to changes in order of recency (maybe under g goto map), as well as just forward/back to changes in change history.
;; TODO - surround region with paren of given type, surround region with xml tag of given type, delete enclosing paren or xml tag of given type, change delimiter or xml tag from given type to other type, a la evil-surround
;; TODO - give yasnippet another try, and figure out good keys for it
;; TODO - reconsider keys for completion, and maybe approach to completion generally.
;; TODO - still need: goto-column, back-to-indentation, line start, line end, stay-in-line, line-end-but-before-trailing-space, splice, promote, demote, function arg object, ...
;; TODO - consider motion vs command state bindings -- I have not been careful about keeping them straight.  Maybe that means that motion state is redundant?

(message "started loading keys.el")

(require 'estate)
(require 'estate-default-states)
(setq estate-set-initial-state-function
      (lambda ()
        (cond
         ((minibufferp) (estate-insert-state))
         ((equal major-mode 'term-mode) (estate-insert-state))
         (t (estate-normal-state)))))
(estate-mode 1)
(require 'composiphrase)
(require 'composiphrase-demo-match-config)
;; Note that I'm extending this composiphrase configuration below.
(setq composiphrase-current-configuration composiphrase-demo-match-config)
(require 'aggreact)
(require 'composiphrase-estate-aggreact-config)
(aggreact-mode 1)

(require 'cpo-search-movements)
(require 'cpo-helpers)

;; TODO - I don't know whether I want to require this up-front, but for the advice it adds, it needs to be loaded before creating all of the lambdas that modify the current command sentence.
(require 'estate-visual-modifier-composiphrase-integration)

(setq cpo-paste-default-register ?P)
(setq cpo-copy-default-register ?P)
(setq cpo-copy-sync-with-kill-ring-register ?P)
(setq cpo-delete-default-register ?D)
(setq cpo-change-default-register ?D)
(setq cpo-paste-copy-old-default-register ?C)



(defun cp/verb (name)
  "Construct a composiphrase word alist for verb NAME."
  `((word-type . verb)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cp/obj (name)
  "Construct a composiphrase word alist for object NAME."
  `((word-type . object)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cp/mod (name contents &optional ui-hint)
  "Construct a composiphrase word alist for modifier NAME with value CONTENTS.
UI-HINT is used for the ui-hint field if non-nil, else CONTENTS is also used as the hint."
  `((word-type . modifier)
    (parameter-name . ,name)
    (contents . ,contents)
    (ui-hint . ,(or ui-hint contents))))

(defun cp/add (&rest words)
  "Return a command that adds WORDS to current command sentence (and handles numeric argument, adding it to sentence).
"
  (apply 'composiphrase-add-to-current-sentence-with-numeric-handling
         nil
         words))
(defun cp/ae (&rest words)
  "Return a command that adds WORDS to current command sentence (and handles numeric argument, adding it to sentence).
The command then executes the sentence.
"
  (apply 'composiphrase-add-to-current-sentence-with-numeric-handling
         'exec-after
         words))
(defun cp/ar (&rest words)
  "Return a command that adds WORDS to current command sentence (and handles numeric argument, adding it to sentence).
The command also executes the sentence, with region as the object, if the region is active.
"
  (apply 'composiphrase-add-to-current-sentence-with-numeric-handling
         (region-active-p)
         (if (region-active-p)
             (cons (cp/obj 'region)
                   words)
           words)))


(require 'cpo-text-object-stuff)

(autoload 'rg "rg" "" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nobreak-define-key (map keys func)
  (nobreak (define-key map keys func)))
(defun emmap (keys func)
  (nobreak-define-key estate-motion-state-keymap keys func))
(defun enmap (keys func)
  (nobreak-define-key estate-normal-state-keymap keys func))
(defun evmap (keys func)
  (nobreak-define-key estate-visual-state-keymap keys func))
(defun evrmap (keys func)
  (nobreak-define-key estate-visual-state-rectangle-keymap keys func))
(defun epmap (keys func)
  (nobreak-define-key estate-pager-state-keymap keys func))
;; insert state binding -- I'm used to using the function eimap, but for most bindings in “insert mode” I actually want to change the global map, to still allow other maps to override it depending on the mode.  But there are a few things that I DO need in the actual insert state keymap, at least if I don't want the hassle of unbinding things then rebinding, for prefix keys.
(defun eImap (keys func)
  (nobreak-define-key estate-insert-state-keymap keys func))
(defun eimap (keys func)
  (nobreak-define-key global-map keys func))
(defun egmap (keys func)
  (nobreak-define-key global-map keys func))


(defun lnkmap (keys func)
  (when (not estate-normal-state-buffer-local-keymap)
    (setq-local estate-normal-state-buffer-local-keymap (make-sparse-keymap)))
  (nobreak-define-key estate-normal-state-buffer-local-keymap keys func))


;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun etmap (keys func)
  (nobreak-define-key temp-key-map keys func))


(require 'hydra)

(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))
;; Hydra note: add more bindings with (defhydra+ NAME HYDRA-OPTIONS HEAD ...), where HYDRA-OPTIONS can be nil to not change the options.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core things that I hope work even if something later in this file breaks...

(emmap "\M-c" 'execute-extended-command)
(emmap "\M-x" 'execute-extended-command)
(eimap "\M-c" 'execute-extended-command)
(eimap "\M-x" 'execute-extended-command)


;; Normal state switch!
(emmap (kbd "C-c") 'ignore)
(evmap "\C-c" 'estate-normal-state)
(eimap "\C-c" 'estate-normal-state)
(eImap "\C-c" 'estate-normal-state)

(emmap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)
(eimap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)
(evmap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc things that cross states but that I want together for review...

(emmap (kbd "C-z") 'suspend-frame)
(eimap (kbd "C-z") 'suspend-frame)

(autoload 'helm-M-x "helm-command" "" t)
(emmap "z" 'helm-M-x)
(eimap (kbd "M-c") 'helm-M-x)
(emmap (kbd "M-c") 'helm-M-x)
(global-set-key (kbd "M-c") 'helm-M-x)

(autoload 'wgh/init-minad "minad-stack-conf" "" t)
(emmap "z" (lambda () (interactive) (nobreak (wgh/init-minad)) (call-interactively 'execute-extended-command)))
(eimap (kbd "M-c") (lambda () (interactive) (nobreak (wgh/init-minad)) (call-interactively 'execute-extended-command)))
(emmap (kbd "M-c") (lambda () (interactive) (nobreak (wgh/init-minad)) (call-interactively 'execute-extended-command)))
(global-set-key (kbd "M-c") (lambda () (interactive) (nobreak (wgh/init-minad)) (call-interactively 'execute-extended-command)))


(enmap "=" 'indent-region)
(enmap "≠" 'wgh/racket-indent-region)
(enmap (kbd "TAB") 'sp-indent-defun)
(eimap (kbd "<backtab>") 'indent-for-tab-command)

(emmap (kbd "RET") 'ignore)
(emmap "\C-m" 'ignore)
(emmap "\C-j" 'ignore)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert state

(eimap "\C-w" 'ignore) ;; by default bound to kill-region.  I might map it to delete-backward-word, but I also don't want to get used to using it such that I accidentally use it in a web browser, since browsers are both extremely necessary, in fact using multiple browsers is more-or-less necessary, and none of them have reasonable allowance for user customization of keyboard shortcuts.
(eimap "\C-t" 'ignore) ;; I use this as a prefix for tmux, so preferably ignore it if it accidentally comes through.
(eimap "\M-t" 'ignore) ;; M-t -- I think I want to save this for windowing in terminal multiplexer with C-t

(defun wgh/backward-delete-word (arg)
  "like backward-kill-word, except don't add to kill ring"
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(eimap "\C-s" 'wgh/backward-delete-word) ;; TODO - this is problematic in two ways.  One, C-s is save in most programs, so maybe I don't want a habit of using something that often means save.  Also, it is a key for terminal flow control that stops printing, which is super annoying when accidentally used.
(eimap "\C-q" 'ignore) ;; C-q - don't use, it is the terminal flow control binding to restart flow.
(eimap "\C-v" 'quoted-insert)
;; C-o -- in readline the default action for this is “operate-and-get-next”, which executes the command, finds the command in the history, and sets the buffer to the next command in history.  So it is useful for those times when you go back in history 5 commands, hit enter, then go up in history 5 commands, hit enter, etc, you can just go back 5 commands, then hit C-o 5 times.

;; TODO - make this add a hook to stay in normal state if the current composiphrase sentence is non-empty.
(eimap "\C-l" 'estate-normal-state-keymap)

;;(eimap "\M-h" 'completer-map/body)
(eimap "\M-h" (lambda () (interactive) (require 'minad-stack-conf) (nobreak (wgh/init-corfu)) (completer-map/body)))
(eimap "\C-r" (lambda () (interactive) (require 'minad-stack-conf) (nobreak (wgh/init-corfu)) (completer-map/body)))
;;(eimap (kbd "C-SPC TAB") 'completer-map/body)
;;(eimap (kbd "C-@ TAB") 'completer-map/body)
;;(eimap (kbd "TAB") 'company-complete-common-wgh)
;;(eimap (kbd "TAB") (lambda () (interactive) (require 'minad-stack-conf) (wgh/init-corfu) (completion-at-point)))
(eimap (kbd "TAB") (lambda () (interactive) (require 'minad-stack-conf) (wgh/completion-at-point-start)))


;;(eimap (kbd "<tab>") 'ignore) ;; This will bind tab in gui but not terminal.  In GUI and terminal C-i is always equivalent to TAB.  In GUI tab sends <tab>, in terminal tab sends C-i.
;; TODO - should I even use things like this?
(eimap (kbd "M-+ M-+ i") (lambda () (interactive) (message "C-i remapped")))
(eimap (kbd "M-+ M-+ M-i") (lambda () (interactive) (message "C-M-i remapped")))
(eimap (kbd "M-+ M-+ I") (lambda () (interactive) (message "C-I remapped")))
(eimap (kbd "M-+ M-+ m") (lambda () (interactive) (message "C-m remapped")))
(eimap (kbd "M-+ M-+ M-m") (lambda () (interactive) (message "C-M-m remapped")))
(eimap (kbd "M-+ M-+ M") (lambda () (interactive) (message "C-M remapped")))
(eimap (kbd "M-+ M-+ N") (lambda () (interactive) (message "C-N remapped")))
(eimap (kbd "H-i") (lambda () (interactive) (message "H-i mapping")))
(eimap (kbd "s-i") (lambda () (interactive) (message "s-i mapping")))

;; save some key bindings that I'm overwriting
(eimap (kbd "C-x C-r") 'isearch-backward)
(eimap (kbd "C-x C-s") 'isearch-forward)
(eimap (kbd "C-x C-l") 'recenter-top-bottom)
(eimap (kbd "C-x C-t") 'transpose-chars)
(eimap (kbd "C-x C-o") 'open-line)
(eimap (kbd "C-x C-@") 'set-mark-command)
(eimap (kbd "C-x C-SPC") 'set-mark-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim-like keys to reconsider since I'm moving to composiphrase

;; TODO - consider A/I to open object selection, so that they are longer commands in terms of keys but dot repetition captures the movement and the insertion for arbitrary movements instead of just end of line / back-to-indentation...
(enmap "A" (lambda () (interactive)
             (progn (move-end-of-line nil) (estate-insert-state))))
(evmap "A" (lambda () (interactive) (progn (goto-char (region-end))
                                           (deactivate-mark)
                                           (estate-insert-state))))
(enmap "I" (lambda () (interactive)
             (progn (back-to-indentation) (estate-insert-state))))
(evmap "I" (lambda () (interactive) (progn (goto-char (region-beginning))
                                           (deactivate-mark)
                                           (estate-insert-state))))

;; (enmap "C" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
;; (evmap "C" (lambda () (interactive (progn (kill-region (region-beginning)
;;                                                        (region-end))
;;                                           (estate-insert-state)))))

;; (enmap "D" (lambda () (interactive) (progn (kill-line))))
;; (evmap "D" (lambda () (interactive) (progn (kill-region nil nil t))))
(enmap "C" (lambda () (interactive) (message "Training wheels: use long composed keys")))
(enmap "D" (lambda () (interactive) (message "Training wheels: use long composed keys")))

(enmap "Y" (lambda () (interactive) (message "Y not mapped...")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most key bindings...

;; Hmm, should I keep these around?  Do they serve a purpose?  Maybe better keep them just in case.
(emmap [left] 'rmo/backward-char)
(emmap [right] 'rmo/forward-char)
(emmap [up] 'rmo/previous-line)
(emmap [down] 'rmo/next-line)


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



(emmap "v" 'estate-visual-state)
;;(emmap "V" 'estate-visual-line-state)
(enmap "V" (lambda () (interactive)
             (funcall (cp/add (cp/verb 'activate-visual-modifier-state)))
             (wgh/object-select/body)))
(emmap "\C-v" 'estate-visual-rectangle-state)

(emmap "|" 'eval-expression)
(emmap "_" 'eval-expression)

;; TODO - figure out good keys to use, especially for the “quick” ones that record to change.
(emmap "q" 'estate-keyboard-macro-to-register-end-most-recent-or-start-default)
(emmap "Q" 'estate-keyboard-macro-to-register-start)
(emmap "r" 'estate-keyboard-macro-execute-from-most-recently-macro-recorded-register)
(emmap "R" 'estate-keyboard-macro-execute-from-register)
;;(enmap "   h" 'estate-record-quick-keyboard-macro-to-buffer-change)


(emmap "m" nil) ;;;;;;;;;; m will be my prefix for mode-specific bindings

(enmap "." 'cp-demo-aggreact-repeat-latest-editing)

;; TODO - use command sentence
(enmap "x" 'delete-char)
(enmap "X" 'delete-char-backward)

;; TODO - I have this in the command map, but I find that I care more about having quick access to copy than I do about most commands.
(emmap "y" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cp/ae (cp/verb 'copy)
                                 (cp/obj 'region))
                          n)
               (funcall (cp/add (cp/verb 'copy))
                        n))))
(emmap " yc" 'xcopy)
(emmap " yt" 'wgh/tmux-copy)
(emmap " yo" 'wgh/terminal-copy-osc)
(emmap " yff" (cp/ae (cp/verb 'copy-file-name-full-path)))
(emmap " yfg" (cp/ae (cp/verb 'copy-file-name-git-relative)))
(emmap " yfb" (cp/ae (cp/verb 'copy-file-name-basename)))
(emmap " yfz" (cp/ae (cp/verb 'copy-git-fzf-file-name)))
(emmap " yT" (cp/ae (cp/verb 'paste-to-terminal-osc)))
(enmap "p" (cp/ae (cp/verb 'paste-to-region-from-move)
                  (cp/obj 'region)))
(enmap " pc" 'xpaste)
(enmap " pt" 'wgh/tmux-paste)

(emmap "\"" (lambda (n) (interactive "p")
              (funcall (cp/add (let ((reg (read-key "Register: ")))
                                 (cp/mod 'register reg (format "r:%c" reg))))
                       n)))

(enmap "u" 'undo)
(enmap "\C-r" 'undo-tree-redo)

(defmacro with-evil (func)
  `(lambda ()
     (interactive)
     (require 'evil)
     (call-interactively ,func)))

(enmap "<" (with-evil 'evil-shift-left))
(enmap ">" (with-evil 'evil-shift-right))
(emmap "%" 'cpo-smartparens-move-to-other-end-of-sexp) ;; TODO - maybe just stop using this?  I can accomplish it with forward/backward beg/end.  Turn it into training wheels message binding.

(emmap (kbd "DEL") 'rmo/backward-char)
(emmap (kbd "<deletechar>") 'rmo/forward-char)



;; (emmap "f" (cp/ae (cp/mod 'direction 'forward)
;;                   (cp/obj 'repeatable-motion-repeat)))
;; (emmap "F" (cp/ae (cp/mod 'direction 'backward)
;;                   (cp/obj 'repeatable-motion-repeat)))
(emmap "h" (cp/ae (cp/mod 'direction 'forward)
                  (cp/obj 'repeatable-motion-repeat)))
(emmap "H" (cp/ae (cp/mod 'direction 'backward)
                  (cp/obj 'repeatable-motion-repeat)))


;; Enter prefix maps for composiphrase
(emmap "c" (lambda ()
             (interactive)
             (funcall (cp/add `((word-type . ignore))))
             (wgh/verb-select/body)))

(emmap "e" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'forward)) n)
             (wgh/object-select/body)))
(emmap "o" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'backward)) n)
             (wgh/object-select/body)))


(emmap "i" (lambda (n) (interactive "p")
             (if (null composiphrase-current-sentence)
                 (estate-insert-state) ;; TODO - handle number
               (progn
                 (funcall (cp/add (cp/mod 'direction 'expand-region)
                                  (cp/mod 'inner 'inner "inner"))
                          n)
                 (wgh/object-select/body)))))
(evmap "i" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'expand-region)
                              (cp/mod 'inner 'inner "inner"))
                      n)
             (wgh/object-select/body)))
(emmap "a" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'expand-region))
                      n)
             (wgh/object-select/body)))



(emmap "j" (cp/ae (cp/mod 'direction 'forward)
                  (cp/mod 'location-within 'keep-if-possible)
                  (cp/obj 'line)))
(emmap "k" (cp/ae (cp/mod 'direction 'backward)
                  (cp/mod 'location-within 'keep-if-possible)
                  (cp/obj 'line)))

;; TODO - w/b is a lazy duplicate with no difference from what exists compositionally.  I've kept w/b as the “quick mash for movement” keys to just move for a while.  I would replace that with using ew/eb then mashing f.  I should turn these into training wheels message binds to unlearn their use.
;; (emmap "w" (cp/ae (cp/mod 'direction 'forward)
;;                   (cp/obj 'cpo-vi-like-word)))
;; (emmap "b" (cp/ae (cp/mod 'direction 'backward)
;;                   (cp/obj 'cpo-vi-like-word)))
(emmap "w" (lambda () (interactive) (message "training wheels: use ew")))
(emmap "b" (lambda () (interactive) (message "training wheels: use eb")))

(emmap "`" (cp/ae (cp/obj 'jump-to-register)))
(emmap "'" (cp/ae (cp/obj 'jump-to-register))) ;; in vim, this jumps to the LINE of the given marker, keeping the current column.  But... I dunno, I'll map both for now.

(emmap "/" (cp/ae (cp/mod 'direction 'forward)
                  (cp/obj 'isearch-new)))
(emmap "?" (cp/ae (cp/mod 'direction 'backward)
                  (cp/obj 'isearch-new)))
(emmap "n" (cp/ae (cp/mod 'direction 'forward)
                  (cp/obj 'isearch-repeat)))
(emmap "N" (cp/ae (cp/mod 'direction 'backward)
                  (cp/obj 'isearch-repeat)))

(autoload 'helm-swoop "helm-swoop" "" t)
(autoload 'helm-multi-swoop-all "helm-swoop" "" t)
(autoload 'helm-multi-swoop "helm-swoop" "" t)
(emmap " /"
       (myhydradef search-hydra
                   ("C-g" nil "quit")
                   ("s" helm-swoop "swoop")
                   ("a" helm-multi-swoop-all "multi-swoop all")
                   ("m" helm-multi-swoop "multi-swoop")
                   ("r" wgh/fzf-repo "fzf-repo")
                   ))



;; Ex
(emmap ":" (lambda () (interactive)
             (require 'evil) ;; Note that if I just require evil-ex here, it doesn't have any commands and I just get stuck in the ex buffer.
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
(evmap "!" (lambda () (interactive)
             (require 'evil) (call-interactively 'evil-shell-command)))



;; Quick select any delimiter
(emmap "(" (lambda (n)
             (interactive "p")
             (require 'cpo-smartparens)
             (funcall (cp/ae (cp/mod 'direction 'expand-region)
                             (cp/mod 'delimiter 'any)
                             (cp/obj 'cpo-smartparens))
                      n)))
(emmap ")" (lambda (n)
             (interactive "p")
             (require 'cpo-smartparens)
             (funcall (cp/ae (cp/mod 'direction 'expand-region)
                             (cp/mod 'inner 'inner "inner")
                             (cp/mod 'delimiter 'any)
                             (cp/obj 'cpo-smartparens))
                      n)))


;; TODO - obviously I want to integrate these into composiphrase, but I need to add keys for going forward/back to objects, including delimiters, without regard for tree boundaries.
(emmap "{" 'backward-sexp)
(emmap "}" 'forward-sexp)
(emmap "[" 'backward-list)
(emmap "]" 'forward-list)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; composiphrase wgh/verb-select map
(defhydra wgh/verb-select (:foreign-keys warn :exit nil) "Verb:"
  ("C-g" keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state "quit" :exit t)

  ;; Verbs that automatically take region if the region is active.
  ;; These are the ones where you maybe should select first anyway.
  ("a" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'action)) n))
   "action" :exit t)
  ("c" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'change)) n))
   "change" :exit t)
  ("d" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'delete)) n))
   "delete" :exit t)
  ("y" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'copy)) n))
   "copy" :exit t)
  ("u" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'downcase)) n))
   "downcase" :exit t)
  ("U" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'upcase)) n))
   "upcase" :exit t)
  ("~" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'toggle-case)) n))
   "upcase" :exit t)
  ("hu" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'capitalize)) n))
   "capitalize" :exit t)
  ("n" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'initiate-isearch)) n))
   "isearch-thing" :exit t)
  ("f" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'format)) n))
   "format" :exit t)
  ("i" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'indent)) n))
   "indent" :exit t)
  ("I" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'dedent)) n))
   "dedent" :exit t)
  ("C" (lambda (n) (interactive "p")
         (funcall (cp/ar (cp/verb 'toggle-comment)) n))
   "toggle-comment" :exit t)
  ("hc" (lambda (n) (interactive "p")
          (funcall (cp/ar (cp/verb 'comment)) n))
   "comment" :exit t)
  ("hC" (lambda (n) (interactive "p")
          (funcall (cp/ar (cp/verb 'uncomment)) n))
   "uncomment" :exit t)
  ;; Note that I'm reserving h as a prefix here...
  ("hp" (lambda (n) (interactive "p")
          ;; Note that I'm putting this on a prefix map because it is just less useful than move-paste.
          (funcall (cp/add (cp/verb 'paste-to-region-from-move)) n))
   "paste-over-region" :exit t)

  ;; Verbs that don't automatically take region -- they still need an object argument to decide what to do.
  ;; Some of these should still work with a region -- eg. transpose with a region should transpose based on the text object given, but with multiple of the object based on the current region.  Eg. transpose multiple lines together, multiple s-expressions together, multiple indent trees together, etc.
  ;; But some of these... should probably just error if given a region.

  ;; Move is the default, but let's add it to the map anyway.
  ("m" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'move)) n))
   "move" :exit t)
  ("p" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'move-paste)) n))
   "move-paste" :exit t)
  ("A" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'move-insert)) n))
   "move-insert" :exit t)
  ("j" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'join)) n))
   "join" :exit t)
  ("J" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'split)) n))
   "split" :exit t)
  ("o" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'open)) n))
   "open" :exit t)
  ("t" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'transpose)) n))
   "transpose" :exit t)
  ("s" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'slurp)) n))
   "slurp" :exit t)
  ("S" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'barf)) n))
   "barf" :exit t)
  ("b" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'barf)) n))
   "barf" :exit t)
  ("W" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'promote)) n))
   "promote" :exit t)
  ("w" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/verb 'demote)) n))
   "demote" :exit t)
  ("hw" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/verb 'change-delimiter)) n))
   "change-delimiter" :exit t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; composiphrase object-select map

(defun with-cpo-smartparens-req (x)
  (require 'cpo-smartparens)
  x)


(defhydra wgh/object-select (:foreign-keys warn :exit nil) "Obj:"
  ;; TODO - Hydra handles lambda specially.  I would love to just use higher order functions, but to work with Hydra I seem to need to use lambdas...  Maybe I should do this without hydra.  But I really want the “stay in the map” functionality, and I like the documentation popup, too.
  ("C-g" keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state "quit" :exit t)

  ("c" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'character)) n)) "character" :exit t)
  ;; This encoding is sketchy... but oh well.
  ("f" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'specific t)
                                                     (cp/obj 'character))
                                              n))
   "character-specific" :exit t)
  ;;("l" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'line)) n)) "line" :exit t)
  ("l" (lambda (n) (interactive "p") (message "use g, less mnemonic but works better for keyboard layout")) "line-norm" :exit t) ;; TODO - I want to leave this here and not bind something new to l, since l is the good mnemonic for line.  But I keep going to “end line”, and I want that to just be nicer to type.  Dvorak put L in a terrible place, a big mistake of the layout.
  ("g" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'line)) n)) "line" :exit t)
  ;; TODO - consider whether to keep using this vi-like word, or the emacs word, or something else.  Also how to deal with sub-words in symbols.
  ("w" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'cpo-vi-like-word)) n)) "vi-like-word" :exit t)
  ("W" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'word)) n)) "word" :exit t)
  ("y" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'symbol)) n)) "symbol" :exit t)
  ("Y" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'sexp)) n)) "sexp" :exit t)
  ("P" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'paragraph)) n)) "paragraph" :exit t)
  ("S" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'sentence)) n)) "sentence" :exit t)
  ("B" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'buffer)) n)) "buffer" :exit t)
  ("s" (lambda (n) (interactive "p") (funcall (cp/ae (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "smartparens" :exit t)
  ("e" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-indent-tree) (cp/obj 'cpo-indent-tree))) n)) "indent-tree" :exit t)
  ;; TODO - I want this one, but I keep using this accidentally due to my old key bindings, and it is so frustrating.  So I'll leave it as a no-op for now.
  ;;("o" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'NOOP-STOP-USING-THIS-BINDING-FOR-OLD-PURPOSE)) n)) "break habit!" :exit t)
  ("o" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-outline) (cp/obj 'outline))) n)) "outline" :exit t)
  ("t" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-treesitter-qd)
                                                            (wgh/initialize-treesit-for-buffer)
                                                            ;; TODO - also need to initialize treesitter in the buffer before first use...
                                                            (cp/obj 'cpo-treesitter-qd)))
                                              n))
   "treesitter-qd" :exit t)
  ("x" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'xml)) n)) "xml" :exit t)
  ("X" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'xml-tag)) n)) "xml-tag" :exit t)
  ("j" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'json)) n)) "json" :exit t)

  ;; It's hard to decide priorities for myself, especially for things not yet implemented.  I want to reserve space in my map for future things.
  ;;("g" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'argument)) n)) "argument" :exit t)
  ("G" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'argument)) n)) "argument" :exit t)
  ("D" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'definition)) n)) "definition" :exit t)
  ("F" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'function)) n)) "function" :exit t)
  ("M" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'statement)) n)) "statement" :exit t)
  ("C" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'comment)) n)) "comment" :exit t)
  ("L" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'list)) n)) "list" :exit t)
  ("hL" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'linter-warning)) n)) "linter-warning" :exit t)
  ("p" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'proposed-change)) n)) "proposed-change" :exit t)
  ("hC" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'class)) n)) "class" :exit t)
  ("hX" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'test)) n)) "test" :exit t)
  ("hc" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'buffer-change)) n)) "buffer-change" :exit t)
  ("hg" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'vcs-change)) n)) "vcs-change" :exit t)
  ("h SPC" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'whitespace)) n)) "whitespace" :exit t)
  ("hu" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-url-object) (cp/obj 'url))) n)) "url" :exit t)
  ("hd" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-date-object) (cp/obj 'date-yyyy-mm-dd))) n)) "date" :exit t)
  ("he" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'cpo-url-object) (cp/obj 'email))) n)) "email" :exit t)
  ("hP" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'phone-number)) n)) "phone-number" :exit t)
  ("hf" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'file-name)) n)) "file-name" :exit t)
  ("ht" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'tempel-conf) (cp/obj 'tempel-snippet-hole))) n)) "tempel" :exit t)
  ("hT" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'yasnippet-conf) (cp/obj 'yasnippet-snippet-hole))) n)) "yasnippet" :exit t)
  ("hF" (lambda (n) (interactive "p") (funcall (cp/ae (progn (require 'yafolding) (cp/obj 'yafold))) n)) "yafolding" :exit t)


  ;; Specific delimiters, use smartparens for them.
  ("\"" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "\"") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "\"" :exit t)
  ("'" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "'") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "'" :exit t)
  ("`" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "`") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "`" :exit t)
  ("(" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "(") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "()" :exit t)
  (")" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "(") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "()" :exit t)
  ("[" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "[") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "[]" :exit t)
  ("]" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "[") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "[]" :exit t)
  ("{" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "{") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "{}" :exit t)
  ("}" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "{") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "{}" :exit t)
  ("«" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "«") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "«»" :exit t)
  ("»" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "«") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "«»" :exit t)
  ("“" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "“") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "“”" :exit t)
  ("”" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "“") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "“”" :exit t)
  ("⟅" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "⟅") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "⟅⟆" :exit t)
  ("⟆" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "⟅") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "⟅⟆" :exit t)
  ("#" (lambda (n) (interactive "p") (funcall (cp/ae (cp/mod 'delimiter "#|") (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n)) "#||#" :exit t)

  ;; Modifiers -- maybe these should have a separate map, but that adds verbosity, and I'm not yet certain there are enough objects and modifiers to warrant splitting -- I can have a prefix within this map for infrequent things, and I can always add another separate prefix map.
  ("n" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'location-within 'end)) n)) "end" :exit nil)
  ;;("hb" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'location-within 'beginning)) n)) "beginning" :exit nil) ;; Default, but let's add it anyway.
  ;;("he" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'location-within 'emacs-style)) n)) "emacs-style" :exit nil)
  ("i" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'inner 'inner)) n)) "inner" :exit nil)
  ("u" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'tree-vertical 'up)) n)) "up" :exit nil)
  ("d" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'tree-vertical 'down)) n)) "down" :exit nil)
  ("T" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'tree-traversal 'inorder)) n)) "inorder" :exit nil)
  ("r" (lambda (n) (interactive "p") (let ((reg (read-key "register: ")))
                                       (funcall (cp/add (cp/mod 'register reg (format "r:%c" reg))) n)))
   "register" :exit nil)
  (" " (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'surrounding-space 'surrounding-space)) n)) "surrounding-space" :exit nil)
  ("b" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'absolute 'absolute)) n)) "absolute" :exit nil) ;; For absolute numbering (within tree if respect tree is on).  Ignore forward/backward direction.
  ("m" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'matching 'matching)) n)) "matching" :exit nil) ;; Eg. for finding the next matching word, symbol, whatever.
  ("a" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'alternate 'alternate)) n)) "alternate" :exit nil) ;; For object-specific alternate behavior...
  ("A" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'alternate 2)) n)) "alternate-2" :exit nil)

  ("hR" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'respect-tree 'respect-tree)) n)) "respect-tree" :exit nil)
  ("hr" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'respect-tree nil)) n)) "DISrespect-tree" :exit nil)
  ("hD" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'direction nil)) n)) "No direction" :exit nil) ;; Is this useful?  Maybe.  Currently I have no way to get into the map without specifying a direction...
  ("hi" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'idempotent 'idempotent)) n)) "idempotent" :exit nil) ;; Eg. only move if not at a place where this movement would have moved to.  Useful for keyboard macros, maybe.
  ("hl" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'current-line-only 'current-line-only)) n)) "current-line-only" :exit nil) ;; Eg. do something on the current line only.  Uncertain how useful this is.

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big prefix maps

;; g map
;; TODO - try to figure out a theme for this prefix map.  Maybe goto, which fits some of my bindings as well as what the helix editor does.  This map has grown by accretion over time and I would like to organize my accreted maps better.  That said, is it worth re-learning all of these miscellaneous key bindings, esp. those that are relatively low-use but that I have memorized?
;; TODO - if I turn this into a goto-themed map, I should put jump-to-register functions here.
(emmap "gg" 'cpo-goto-line-default-first)
(emmap "G" 'cpo-goto-line-default-last)
(emmap "gdd" 'xref-find-definitions)
(emmap "gdD" 'xref-find-definitions-other-window)
(emmap "gdr" 'xref-find-references)
(emmap "gdi" 'lsp-describe-thing-at-point)
(emmap "gdp" 'pop-tag-mark)
(enmap "gq" (with-evil 'evil-fill-and-move))
(enmap "gw" (with-evil 'evil-fill))
(emmap "gv" 'estate-restore-region)
;; TODO - avy can be customized with different targets.  Maybe add it as a composiphrase verb.
(emmap "gac" (lambda () (interactive) (require 'avy) (call-interactively 'avy-goto-char)))
(emmap "gan" (lambda () (interactive) (require 'avy) (call-interactively 'avy-next)))
(emmap "gap" (lambda () (interactive) (require 'avy) (call-interactively 'avy-prev)))
(emmap "gal" (lambda () (interactive) (require 'avy) (call-interactively 'avy-goto-line))) ;; This is maybe worse than just looking at the line number and using a command to jump to the line number...


;; t map
(emmap "tt" 'temp-key-map)

;;(emmap "tia" 'wgh/ido-switch-buffer)
(emmap "tia" (lambda () (interactive)
               (nobreak (wgh/init-minad))
               (call-interactively 'switch-to-buffer)))
(emmap "tic" 'kill-buffer-or-quit-emacs)
(emmap " tica" 'save-buffers-kill-terminal)
(emmap "tis" 'save-buffer)
(emmap " tisa" 'TODO-write-all)
(emmap "tie" 'save-and-kill-buffer-and-maybe-quit-emacs)
(emmap " tiea" 'TODO-save-all-and-quit)
(emmap "tip" 'ffap/no-confirm)
;; TODO - I want to switch from "tif" and friends to "tf*" for the variety of ways I want to find files.
;;(emmap "tif" 'ido-ffap-no)
(emmap "tif" 'wgh/find-file-no-ffap)
;;(emmap " tifd" 'ido-find-file-from-pwd)
(emmap " tiff" 'ffap/no-confirm)
(emmap "tiw" 'next-buffer-no-star)
(emmap "tib" 'prev-buffer-no-star)
(emmap " tiwd" 'next-dirty-buffer-no-star)
(emmap " tibd" 'prev-dirty-buffer-no-star)
;;(emmap "tff" 'ido-ffap-no)
(emmap "tff" 'wgh/find-file-no-ffap)
(emmap "tfp" 'ffap/no-confirm)
(emmap "tfh" 'ff-find-other-file) ; IE switch between header and source file for C/C++
;;(emmap "tfd" 'ido-find-file-from-pwd)
(emmap "tfg" (lambda () (interactive) (require 'helm-projectile) (helm-projectile)))
(autoload 'fzf-git-files "fzf" "" t)
(emmap "tfz" 'fzf-git-files)

(emmap "th" 'my-window-map/body)
(autoload 'projectile-command-map "projectile-conf" "" t 'keymap)
(emmap "tp" 'projectile-command-map)
(emmap "tr" 'TODO-select-register)
(emmap "to" 'estate-pager-state)


;; "ts" will stand for "toggle setting"
(emmap "ts"
       (defhydra settings-toggle (:foreign-keys warn :exit t) "Toggle:"
         ("C-g" nil "quit")
         ("p" (lambda () (interactive) (require 'smartparens) (smartparens-mode)) "smartparens")
         ("b" (lambda () (interactive) (require 'blamer (blamer-mode))) "git blame")
         ("B" (lambda () (interactive) (if (equal browse-url-browser-function 'browse-url-firefox)
                                           (setq browse-url-browser-function 'eww)
                                         (setq browse-url-browser-function 'browse-url-firefox)))
          "browser_ffx_eww")
         ("w" whitespace-mode "whitespace")
         ("C" (lambda () (interactive) (require 'rainbow-mode) (rainbow-mode)) "#aabbcc")
         ("c" company-mode "company")
         ("t" toggle-truncate-lines "trunc")
         ("i" toggle-case-fold-search "/? case")
         ("W" wgh/toggle-search-wrap "search-wrap")
         ("f" flycheck-mode "flycheck")
         ("F" display-fill-column-indicator-mode "fill-col")
         ("s" flyspell-mode "flyspell")
         ("S" flyspell-prog-mode "flyspell-prog")
         ;;("e" electric-indent-mode "el.indent")
         ("d" rainbow-delimiters-mode "rainbow{}")
         ("r" linum-relative-toggle "linum-rel")
         ("h" isearch-exit "clear-search-highlight")
         ("M" menu-bar-mode "menu-bar")
         ("l" lsp-lens-mode "lsp-lens") ;; lsp-lens is the thing that shows eg. haskell imports in an overlay
         ("m" (lambda () (interactive) (menu-bar-mode 1) (menu-bar-open)) "menu-open")
         ("n" display-line-numbers-mode "line-numbers")
         ("I" indent-guide-mode "indent-guide")
         ("x" wgh/racket-xp-pre-redisplay-toggle "racket-xp-hl")
         ("k" (lambda () (interactive) (require 'which-key) (which-key-mode)) "which-key")
         ))

(emmap "tl"
       (defhydra list-stuff-map (:foreign-keys warn :exit t) "List:"
         ("C-g" nil "quit")
         ("b" list-buffers "buffers")
         ("m" (with-evil 'evil-show-marks) "marks")
         ("M" bookmark-bmenu-list "bookmarks")
         ;;("tlk" 'list-keymaps) ; TODO - make this function
         ("c" list-colors-display "colors")
         ("f" list-faces-display "faces")
         ("r" (with-evil 'evil-show-registers) "registers")
         ;; TODO - list jumps, maybe
         ))


;; "ta" will be an assortment of handy stuff...
(enmap "tac" 'comment-region) ;; TODO - maybe put comment/uncomment and/or toggle-comment in composiphrase verb map, behind some prefix for less common verbs.
(enmap "taC" 'uncomment-region)
(emmap "tam" (lambda () (interactive) (exchange-point-and-mark)))
(enmap "tad" 'insert-date)
(enmap "taD" 'insert-date-time)
(autoload 'alternate-region-activate "alternate-region" "" t)
(autoload 'alternate-region-swap "alternate-region" "" t)
(autoload 'alternate-region-cycle "alternate-region" "" t)
(autoload 'alternate-region-pop-go "alternate-region" "" t)
(enmap "tara" 'alternate-region-activate)
(enmap "tars" 'alternate-region-swap)
(enmap "tarc" 'alternate-region-cycle)
(enmap "tarp" 'alternate-region-pop-go)
(enmap "tav" 'sp-convolute-sexp)
(enmap "tag" 'gptel-send)
(emmap "tayp" (cons "symbol-overlay-put" (lambda () (interactive) (require 'symbol-overlay) (symbol-overlay-put)))) ;; TODO - symbol overlay seems like it could be helpful.  I should add a modifier to symbol to have forward/back symbol motions go to the next instance of the highlighted symbol at point, or to the next highlighted symbol.  Also think about keys for marking, this binding is terrible.  Maybe symbol-overlay can share a verb with marking alternate region?  Are there other ways one might mark something that would be useful to group with these?  Maybe also marking char can be setting point-to-register?
(with-eval-after-load 'symbol-overlay
  ;; I don't appreciate symbol-overlay taking over my keymap.
  (setcdr symbol-overlay-map nil))




;; s map
;; TODO - what is the theme of this map?  Does it need one?
(evmap "sh" 'shell-command-on-region)
(enmap "sh" 'shell-command)
(enmap "s)" 'eval-last-sexp)
(evmap "s)" 'eval-region)
(evmap "s/" (kbd ":s/ ")) ; TODO - fix this...
(emmap "sm" 'point-to-register)
(emmap "sM" 'bookmark-set)
(emmap "sg" (cp/ae (cp/obj 'jump-to-register)))
(emmap "sG" 'bookmark-jump)
(emmap "sx" 'eval-expression)
(emmap "sj" 'rmo/pscroll-down-half)
(emmap "sk" 'rmo/pscroll-up-half)
(emmap "sf" 'fold-toggle-wgh)
(emmap "sF" 'fold-toggle-wgh-all)

;; Documentation
;; TODO - these are also related to LSP doc functionality that I have bound to gdi, related to gdd go-to-def.  I would like to consolidate, but looking up docs is not always related to a symbol at point, though it usually is.
;; TODO - also I use C-h f, C-h v, and C-h o to look up elisp function, value, and function+value docs.  It would be nice to have all docs together in some useful way.
;; TODO - also bindings for racket-xp-describe and racket-xp-documentation

;; TODO - can set devdocs-current-docs for buffer.  Using devdocs-lookup will set it on first run, but if you want multiple doc sources at once, you need to set the variable manually.
(emmap "sdd" (cons "devdocs" (lambda () (interactive) (require 'devdocs) (wgh/init-minad) (devdocs-lookup)))) ;; TODO - use symbol at point
;; TODO - must set dash-docs-common-docsets or dash-doc-docsets, or run dash-docs-activate-docset
;; TODO - can set dash-docs-browser-func to 'eww, but it maybe doesn't work beautifully.
;; TODO - can I switch counsel-dash here to just dash-docs, and have a useful function to call at the end, maybe filtered with vertico?  I didn't find the function after a brief search, but I should look again some time.
(emmap "sdh" (cons "dash-docs" (lambda () (interactive) (require 'counsel-dash) (counsel-dash-at-point))))
(emmap "sdH" (cons "dash-docs-eww" (lambda () (interactive) (require 'counsel-dash) (let ((counsel-dash-browser-func 'eww)) (counsel-dash-at-point)))))
(emmap "sdb" 'eldoc-doc-buffer)
(emmap "sdt" (cons "tldr" (lambda () (interactive) (require 'tldr) (wgh/init-minad) (tldr))))
(emmap "sdm" (cons "(wo)man" (lambda () (interactive) (wgh/init-minad) (woman))))
(emmap "sdM" (cons "man" (lambda () (interactive) (wgh/init-minad) (man))))
(emmap "sdii" (cons "info-top" (lambda () (interactive) (wgh/init-minad) (info)))) ;; TODO - this is also C-h i, should I move other doc stuff under C-h?
(emmap "sdis" (cons "info-search" (lambda () (interactive) (wgh/init-minad) (info-search))))
(emmap "sdi/" (cons "info-search" (lambda () (interactive) (wgh/init-minad) (info-search))))
(emmap "sdb" 'browse-url-at-point)
;; TODO - C-h r is the emacs manual, and there are several other related things, eg. for packages, etc.  Should I try to put them all under one umbrella here, or should I move what I'm doing under C-h?







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc maps

(myhydradef completer-map
            ;;("h" hippie-expand "hippie")
            ;;("f" he-expand-file-name "file")
            ;;("F" (progn (company-conf-init) (require 'company-files) (call-interactively 'company-files)) "file (company)")
            ;;("l" he-expand-lisp-symbol "lisp")
            ("f" (cape-interactive 'cape-file) "file")
            ("l" (cape-interactive 'elisp-completion-at-point) "elisp")
            ("y" (cape-interactive 'cape-elisp-symbol) "cape-elisp")
            ("d" (cape-interactive 'cape-dabbrev) "dabbrev -- text-in-buffer")
            ("s"
             (progn (require 'yasnippet-conf)
                    (call-interactively 'yas-expand))
             "yas")
            ("t"
             (progn (require 'tempel-conf)
                    (call-interactively 'tempel-complete))
             "tempel")
            ("c" copilot-complete "copilot")
            ("C-g" 'ignore "quit")
            )

(defhydra copilot-hydra (:foreign-keys warn) "CP:"
  ("tab" copilot-accept-completion "accept")
  ("a" copilot-accept-completion "accept")
  ("w" copilot-accept-completion-by-word "accept-word")
  ("l" copilot-accept-completion-by-line "accept-line")
  ("p" copilot-accept-completion-by-paragraph "accept-para")
  ("j" copilot-next-completion "next")
  ("k" copilot-previous-completion "prev")
  ("s" copilot-complete "start")
  ("C-g" nil "quit")
  )


(defhydra my-window-map (:foreign-keys warn) "WM:"
  ("C-g" nil "quit")
  ("v" split-window-horizontally nil)
  ("s" split-window-vertically nil)
  ("j" (lambda (&optional n) (interactive "p") (other-window n)) nil)
  ("k" (lambda (&optional n) (interactive "p") (other-window (- n))) nil)
  ("c" delete-window nil)
  ("h" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'shrink-window-horizontally)))
   "skinny")
  ("l" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'enlarge-window-horizontally)))
   "fat")
  ("H" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'enlarge-window)))
   "tall")
  ("L" (lambda () (interactive)
         (let ((current-prefix-arg '(5)))
           (call-interactively 'shrink-window)))
   "short")
  ("f" delete-other-windows "full")
  ;; Space will be for layout concerns
  ;;(" f" delete-other-windows)
  ;;(" u" winner-undo)
  ;;(" r" winner-redo)
  ;;(" j" window-swap-next)
  ;;(" k" window-swap-prev)
  ;;("p" popwin-map)
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



(define-key tty-menu-navigation-map "j" 'tty-menu-next-item)
(define-key tty-menu-navigation-map "k" 'tty-menu-prev-item)
(define-key tty-menu-navigation-map "h" 'tty-menu-prev-menu)
(define-key tty-menu-navigation-map "l" 'tty-menu-next-menu)



(define-key help-map "\C-h" 'describe-prefix-bindings)



(with-eval-after-load 'evil
  (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
  (define-key evil-ex-completion-map "\C-e" 'move-end-of-line)
  (define-key evil-ex-completion-map "\C-d" 'delete-char)
  (define-key evil-ex-completion-map "\C-k" 'kill-line)
  (define-key evil-ex-completion-map "\C-p" 'previous-complete-history-element)
  (define-key evil-ex-completion-map "\C-n" 'next-complete-history-element)
  (define-key evil-ex-completion-map "\C-f" 'forward-char)
  (define-key evil-ex-completion-map "\C-b" 'backward-char)
  (define-key evil-ex-completion-map "\M-r" 'evil-paste-from-register)
  )




(define-key isearch-mode-map "\C-g" 'isearch-abort-abort-gosh-darn-it)

(define-key isearch-mode-map (kbd "RET") 'cpo-isearch-bor-exit)
(define-key isearch-mode-map "\C-j" 'cpo-isearch-eor-exit)



;; helm map to match what I've got going in zsh with zaw...
;;(define-prefix-command 'meta-space-map)
;;(global-set-key (kbd "M-SPC") 'meta-space-map)
;;(define-key meta-space-map " " 'helm-helm-commands)
;;(define-key meta-space-map (kbd "RET") 'helm-helm-commands)
;;(define-key meta-space-map "c" 'helm-M-x)
;;(define-key meta-space-map "p" 'helm-browse-project)
;;(define-key meta-space-map "g" 'helm-do-grep)





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


;; Without defining these, I get terminal bell events when I move the mouse around on the header line / mode line.
(global-set-key (kbd "<header-line><mouse-movement>") 'ignore)
(global-set-key (kbd "<mode-line><mouse-movement>") 'ignore)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pager state
(epmap " " 'rmo/pscroll-down-half)
(epmap "j" 'rmo/pscroll-down-half)
(epmap "k" 'rmo/pscroll-up-full)
(epmap "J" 'rmo/pscroll-down-line)
(epmap "K" 'rmo/pscroll-up-line)
(epmap "sj" 'rmo/pscroll-down-full)
(epmap "sk" 'rmo/pscroll-up-full)
(epmap "e" 'estate-normal-state)
;; if I start in pager mode, this gets remapped to quit
(epmap "q" 'estate-normal-state)




;; Keys I don't really use:

;; Insert mode:
;; C-r
;; non-character symbols?
;; M- combos? (I like and use them less)
;; M-a,e,g,i
;; M-j is something that maybe I should use, but I don't
;; M-k,l,m,n,o,p,q
;; M-s,u,v
;; maybe M-y
;; M-z


;; G- H- (sup- hyp-) combos?  (I need conventions for terminal encodings of these...)

;; Normal mode:
;; B,W I don't really use, but maybe I should?
;; E
;; h,l I have on ec/oc which I'm mostly happy with
;; H,L,M -- move to top,bottom,middle of window.  I don't really use these.  Should I?
;; J,K -- aliases for H,L, also not really used
;; m -- I want this to be mode-specific -- eg. a fallback to normal emacs keys for things like dired
;; O
;; S,T,U,Y -- all no-ops or duplicates, or things I don't use.
;; Z -- currently sub-map, but I don't use it for anything.
;; non-character symbols?
;; #, $, comma, +
;; & I don't really use, maybe I should
;; also maybe I should use *
;; []{}
;; any weird unicode that I have in convenient places


;; Training wheels: prevent repeating the same move command instead of using repeat
(defvar-local wgh/composiphrase-training-wheels--last-sentence nil
  "Last sentence executed, for training wheels duplicate detection.")
(defun wgh/composiphrase-training-wheels-advice (orig-func sentence config)
  "Advice to prevent repeated move commands, encouraging use of repeat key instead."
  (let* ((verb (seq-find (lambda (word) (eq 'verb (cdr (assq 'word-type word))))
                         sentence))
         (object (seq-find (lambda (word) (eq 'object (cdr (assq 'word-type word))))
                           sentence))
         (verb-name (when verb (cdr (assq 'contents verb))))
         (object-name (when object (cdr (assq 'contents object))))
         (is-move-command (or (eq verb-name 'move)
                              (null verb-name))) ; no verb defaults to move, typically
         (is-exempt-object (memq object-name '(repeatable-motion-repeat
                                               line
                                               isearch-repeat
                                               isearch-new
                                               cpo-smartparens ;; mostly for paren-bound quick key
                                               )))
         (same-as-previous (equal sentence wgh/composiphrase-training-wheels--last-sentence)))
    (if (and is-move-command same-as-previous wgh/composiphrase-training-wheels--last-sentence
             (not is-exempt-object))
        (progn
          (composiphrase-clear-current-sentence)
          (message "training wheels: use repeat key (now on h)"))
      (progn
        (setq wgh/composiphrase-training-wheels--last-sentence sentence)
        (funcall orig-func sentence config)))))

;;(advice-add 'composiphrase-execute :around #'wgh/composiphrase-training-wheels-advice)


(repeatable-motion-define-pair 'tempel-next 'tempel-previous)

(let* ((verbs
        (append
         `(
           (copy-file-name-full-path    (register . ,(lambda () cpo-copy-default-register))  (default-object . dummy-object))
           (copy-file-name-git-relative (register . ,(lambda () cpo-copy-default-register))  (default-object . dummy-object))
           (copy-file-name-basename     (register . ,(lambda () cpo-copy-default-register))  (default-object . dummy-object))
           (copy-git-fzf-file-name      (register . ,(lambda () cpo-copy-default-register))  (default-object . dummy-object))
           (paste-to-terminal-osc       (register . ,(lambda () cpo-paste-default-register)) (default-object . dummy-object))
           )
         (cdr (assq 'verbs composiphrase-current-configuration))))
       (objects
        (append
         `(
           (dummy-object) ;; dummy object for verbs that operate without a buffer text object
           (tempel-snippet-hole (default-verb . move) (location-within . beginning))
           (yasnippet-snippet-hole (default-verb . move) (location-within . beginning))
           ;; TODO - yafold doesn't have motions to go to folded regions.  Anyway, I never use folding, why am I bothering with this?
           (yafold (default-verb . move) (location-within . beginning))
           ;; TODO - org-mode begin_src object, or maybe begin_* object
           )
         (cdr (assq 'objects composiphrase-current-configuration))))
       (match-table
        (append
         `(
           (open tempel-snippet-hole () (,(lambda () (estate-insert-state-with-thunk (lambda () (call-interactively 'tempel-insert)))) ()))
           (move tempel-snippet-hole ((direction forward) (alternate ,nil)) (,(lambda (n) (rmo/tempel-next (or n 1))) (num)))
           (move tempel-snippet-hole ((direction forward) (alternate alternate)) (tempel-end ()))
           (move tempel-snippet-hole ((direction backward) (alternate ,nil)) (,(lambda (n) (rmo/tempel-previous (or n 1))) (num)))
           (move tempel-snippet-hole ((direction backward) (alternate alternate)) (tempel-beginning ()))
           (action tempel-snippet-hole ((direction forward)) (tempel-done ()))
           (action tempel-snippet-hole ((direction backward)) (tempel-abort ()))

           (action yafold ((alternate ,nil)) (yafolding-toggle-element ()))
           (action yafold ((alternate alternate)) (yafolding-show-all ()))

           ;; TODO - add yasnippet matchers

           ;; These are kind of dummy verbs.  I want them to take registers, but for the moment I just want to bind keys for these.  Maybe later I'll think more and work them in as proper objects and such.  But for now, expedience to get something that I want working.
           (copy-file-name-full-path
            ,(lambda (x) t) ()
            (,(lambda (register) (wgh/cpo-copy-string (wgh/file-name-full-path) register))
             (register)))
           (copy-file-name-git-relative
            ,(lambda (x) t) ()
            (,(lambda (register) (wgh/cpo-copy-string (wgh/file-name-git-relative) register))
             (register)))
           (copy-file-name-basename
            ,(lambda (x) t) ()
            (,(lambda (register) (wgh/cpo-copy-string (wgh/file-name-basename) register))
             (register)))
           (copy-git-fzf-file-name
            ,(lambda (x) t) ()
            (,(lambda (register)
                (require 'fzf)
                (let ((fzf--target-validator #'fzf--pass-through)
                      (path (locate-dominating-file (file-truename default-directory) ".git")))
                  (if path
                      (fzf-with-command "git ls-files"
                                        (lambda (x) (wgh/cpo-copy-string (file-relative-name x path) register))
                                        path)
                    (user-error "Not inside a Git repository"))))
             (register)))
           (paste-to-terminal-osc
            ,(lambda (x) t) ()
            (,(lambda (register)
                (require 'xclip-conf)
                (let* ((reg (if (functionp register) (funcall register) register))
                       (str (get-register reg)))
                  (if (stringp str)
                      (wgh/terminal-copy-osc-string str)
                    (user-error "Register %c does not contain a string" reg))))
             (register)))


           )
         (cdr (assq 'match-table composiphrase-current-configuration)))))
  (setq composiphrase-current-configuration
        `((verbs . ,verbs)
          (objects . ,objects)
          (match-table . ,match-table)
          )))

(message "finished loading keys.el")
