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

(require 'estate-core)
(require 'estate-vim-like-states)
(estate-mode 1)
(require 'command-sentence)
(require 'estate-and-command-sentence-repeat)

(defun cs/verb (name)
  `((word-type . verb)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cs/obj (name)
  `((word-type . object)
    (contents . ,name)
    (ui-hint . ,name)))
(defun cs/mod (name contents &optional ui-hint)
  `((word-type . modifier)
    (parameter-name . ,name)
    (contents . ,contents)
    (ui-hint . ,(or ui-hint contents))))
(defun cs/ae (&rest words)
  (apply 'command-sentence-add-to-current-with-numeric-handling
         'exec-after
         words))
(defun cs/add (&rest words)
  (apply 'command-sentence-add-to-current-with-numeric-handling
         nil
         words))


(require 'text-object-stuff)

(load-library "keys-funcs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nobreak-define-key (map keys func)
  (nobreak (define-key map keys func)))
(defun emmap (keys func)
  (nobreak-define-key estate-motion-state-keymap keys func))
(defun ecmap (keys func)
  (nobreak-define-key estate-normal-state-keymap keys func))
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
  (nobreak-define-key estate-normal-state-keymap keys func))
(defun vkmap (keys func)
  (nobreak-define-key estate-visual-state-keymap keys func))
(defun ikmap (keys func)
  (nobreak-define-key estate-insert-state-keymap keys func))
(defun pkmap (keys func)
  (nobreak-define-key estate-pager-state-keymap keys func))

(defun lnkmap (keys func)
  (when (not estate-normal-state-buffer-local-keymap)
    (setq-local estate-normal-state-buffer-local-keymap (make-sparse-keymap)))
  (nobreak-define-key estate-normal-state-buffer-local-keymap keys func))

;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun tkmap (keys func)
  (nobreak-define-key temp-key-map keys func))


(require 'hydra)

(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))


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
(eimap "\C-l" 'estate-normal-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc things that cross states but that I want together for review...

(emmap (kbd "C-z") 'suspend-frame)
(eimap (kbd "C-z") 'suspend-frame)

(autoload 'helm-M-x "helm-command" "" t)
(emmap "z" 'helm-M-x)
(eimap (kbd "M-c") 'helm-M-x)
(emmap (kbd "M-c") 'helm-M-x)
(global-set-key (kbd "M-c") 'helm-M-x)

(emmap "z" (lambda () (interactive) (require 'minad-stack-conf) (wgh/init-minad) (call-interactively 'execute-extended-command)))
(eimap (kbd "M-c") (lambda () (interactive) (require 'minad-stack-conf) (wgh/init-minad) (call-interactively 'execute-extended-command)))
(emmap (kbd "M-c") (lambda () (interactive) (require 'minad-stack-conf) (wgh/init-minad) (call-interactively 'execute-extended-command)))
(global-set-key (kbd "M-c") (lambda () (interactive) (require 'minad-stack-conf) (wgh/init-minad) (call-interactively 'execute-extended-command)))


(ecmap "=" 'indent-region)
(ecmap "≠" 'wgh/racket-indent-region)
(ecmap (kbd "TAB") 'sp-indent-defun)
(eimap (kbd "<backtab>") 'indent-for-tab-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert state

(eimap "\C-w" 'ignore) ;; by default bound to kill-region.  I might map it to delete-backward-word, but I also don't want to get used to using it such that I accidentally use it in a web browser, since browsers are both extremely necessary, in fact using multiple browsers is more-or-less necessary, and none of them have reasonable allowance for user customization of keyboard shortcuts.
(eimap "\C-t" 'ignore) ;; I use this as a prefix for tmux, so preferably ignore it if it accidentally comes through.
(eimap "\M-t" 'ignore) ;; M-t -- I think I want to save this for windowing in terminal multiplexer with C-t

(eimap "\C-s" 'backward-kill-word) ;; TODO - this is problematic in two ways.  One, C-s is save in most programs, so maybe I don't want a habit of using something that often means save.  Also, it is a key for terminal flow control that stops printing, which is super annoying when accidentally used.
(eimap "\C-q" 'ignore) ;; C-q - don't use, it is the terminal flow control binding to restart flow.
(eimap "\C-v" 'quoted-insert)
;; C-o -- in readline the default action for this is “operate-and-get-next”, which executes the command, finds the command in the history, and sets the buffer to the next command in history.  So it is useful for those times when you go back in history 5 commands, hit enter, then go up in history 5 commands, hit enter, etc, you can just go back 5 commands, then hit C-o 5 times.
(eimap "\C-l" 'estate-normal-state-keymap)

(eimap "\M-h" 'completer-map/body)
(eimap (kbd "C-SPC TAB") 'completer-map/body)
(eimap (kbd "C-@ TAB") 'completer-map/body)
(eimap (kbd "TAB") 'company-complete-common-wgh)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim-like keys to reconsider since I'm moving to command-sentence

(ecmap "A" (lambda () (interactive)
             (progn (move-end-of-line nil) (estate-insert-state))))
(evmap "A" (lambda () (interactive) (progn (goto-char (region-end))
                                           (deactivate-mark)
                                           (estate-insert-state))))

(ecmap "C" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "C" (lambda () (interactive (progn (kill-region (region-beginning)
                                                       (region-end))
                                          (estate-insert-state)))))

(ecmap "D" (lambda () (interactive) (progn (kill-line))))
(evmap "D" (lambda () (interactive) (progn (kill-region nil nil t))))
(ecmap "I" (lambda () (interactive)
             (progn (back-to-indentation) (estate-insert-state))))
(evmap "I" (lambda () (interactive) (progn (goto-char (region-beginning))
                                           (deactivate-mark)
                                           (estate-insert-state))))
(ecmap "Y" (lambda () (interactive) (message "Y not yet implemented")))


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
(emmap "V" 'estate-visual-line-state)
(emmap "\C-v" 'estate-visual-rectangle-state)

(emmap "|" 'eval-expression)
(emmap "_" 'eval-expression)

;; TODO - figure out good keys to use, especially for the “quick” ones that record to change.
(ecmap "q" 'estate-keyboard-macro-to-register-end-most-recent-or-start-default)
(ecmap "Q" 'estate-keyboard-macro-to-register-start)
(ecmap "r" 'estate-keyboard-macro-execute-from-most-recently-macro-recorded-register)
(ecmap "R" 'estate-keyboard-macro-execute-from-register)
;;(ecmap "h" 'estate-record-quick-keyboard-macro-to-buffer-change)


(emmap "m" nil) ;;;;;;;;;; m will be my prefix for mode-specific bindings

(ecmap "." 'eacsr-repeat-latest-editing)

;; TODO - use command sentence
(ecmap "x" 'delete-char)
(ecmap "X" 'delete-char-backward)

;; TODO - I have this in the command map, but I find that I care more about having quick access to copy than I do about most commands.
(emmap "y" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cs/ae (cs/verb 'copy)
                                 (cs/obj 'region))
                          n)
               (funcall (cs/add (cs/verb 'copy))
                        n))))
(emmap " yc" 'xcopy)
(ecmap "p" 'estate-paste)
(ecmap "P" 'estate-paste/swap)
(ecmap " pc" 'xpaste)

(ecmap "u" 'undo)
(ecmap "\C-r" 'undo-tree-redo)

(ecmap "<" (with-evil 'evil-shift-left))
(ecmap ">" (with-evil 'evil-shift-right))
(emmap "%" 'sptw-move-to-other-end-of-sexp) ;; TODO - maybe just stop using this?  I can accomplish it with forward/backward beg/end.  Turn it into training wheels message binding.

(ecmap (kbd "DEL") 'rmo/backward-char)
(ecmap (kbd "<deletechar>") 'rmo/forward-char)



(emmap "f" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'repeatable-motion-repeat)))
(emmap "F" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'repeatable-motion-repeat)))


;; Enter prefix maps for command-sentence
(ecmap "c" (lambda ()
             (interactive)
             (funcall (cs/add `((word-type . ignore))))
             (command-select/body)))

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
             (funcall (cs/add (cs/mod 'direction nil)
                              (cs/mod 'expand-region 'inner))
                      n)
             (object-select/body)))
(emmap "a" (lambda (n) (interactive "p")
             (funcall (cs/add (cs/mod 'direction nil)
                              (cs/mod 'expand-region t "select"))
                      n)
             (object-select/body)))



(emmap "j" (cs/ae (cs/mod 'direction 'forward)
                  (cs/mod 'location-within 'keep-if-possible)
                  (cs/obj 'line)))
(emmap "k" (cs/ae (cs/mod 'direction 'backward)
                  (cs/mod 'location-within 'keep-if-possible)
                  (cs/obj 'line)))

;; TODO - w/b is a lazy duplicate with no difference from what exists compositionally.  I've kept w/b as the “quick mash for movement” keys to just move for a while.  I would replace that with using ew/eb then mashing f.  I should turn these into training wheels message binds to unlearn their use.
(emmap "w" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'vi-like-word)))
(emmap "b" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'vi-like-word)))

(emmap "`" (with-evil (cs/ae (cs/obj 'goto-marker))))
(emmap "'" (with-evil (cs/ae (cs/obj 'goto-marker-line))))

(emmap "/" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'isearch-new)))
(emmap "?" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'isearch-new)))
(emmap "n" (cs/ae (cs/mod 'direction 'forward)
                  (cs/obj 'isearch-repeat)))
(emmap "N" (cs/ae (cs/mod 'direction 'backward)
                  (cs/obj 'isearch-repeat)))

(autoload 'helm-swoop "helm-swoop" "" t)
(autoload 'helm-multi-swoop-all "helm-swoop" "" t)
(autoload 'helm-multi-swoop "helm-swoop" "" t)
(emmap " /"
       (myhydradef search-hydra
                   ("s" helm-swoop "swoop")
                   ("a" helm-multi-swoop-all "multi-swoop all")
                   ("m" helm-multi-swoop "multi-swoop")
                   ("r" wgh/fzf-repo "fzf-repo")
                   ))


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
(evmap "!" (lambda () (interactive)
             (require 'evil) (call-interactively 'evil-shell-command)))



;; Quick select any delimiter
(emmap "(" (lambda (n)
             (interactive "p")
             (require 'tree-walk-smartparens-integration)
             (funcall (cs/ae (cs/mod 'direction nil)
                             (cs/mod 'expand-region t "select")
                             (cs/mod 'delimiter 'any)
                             (cs/obj 'sptw))
                      n)))
(emmap ")" (lambda (n)
             (interactive "p")
             (require 'tree-walk-smartparens-integration)
             (funcall (cs/ae (cs/mod 'direction nil)
                             (cs/mod 'expand-region 'inner "select")
                             (cs/mod 'delimiter 'any)
                             (cs/obj 'sptw))
                      n)))


;; TODO - obviously I want to integrate these into command-sentence, but I need to add keys for going forward/back to objects, including delimiters, without regard for tree boundaries.
(emmap "{" 'backward-sexp)
(emmap "}" 'forward-sexp)
(emmap "[" 'backward-list)
(emmap "]" 'forward-list)








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command-sentence command-select map
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
  ("u" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'downcase)
                             (cs/obj 'region)))
           (funcall (cs/add (cs/verb 'downcase)) n)))
   "downcase" :exit t)
  ("U" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'upcase)
                             (cs/obj 'region)))
           (funcall (cs/add (cs/verb 'upcase)) n)))
   "upcase" :exit t)
  ("C" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'capitalize)
                             (cs/obj 'region)))
           (funcall (cs/add (cs/verb 'capitalize)) n)))
   "capitalize" :exit t)
  ("i" (lambda (n) (interactive "p")
         (if (region-active-p)
             (funcall (cs/ae (cs/verb 'initiate-isearch)
                             (cs/obj 'region))))
         (funcall (cs/add (cs/verb 'initiate-isearch)) n))
   "isearch-thing" :exit t)

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
  ;; TODO - need to make C-g clear the current-command-sentence, or something like that

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command-sentence object-select map

(defun with-sptw-req (x)
  (require 'tree-walk-smartparens-integration)
  x)
(defhydra object-select (:foreign-keys warn :exit nil) "Obj:"
  ;; TODO - I need these bodies to be evaluated, but hydra does some macro magic.  Maybe I should use something else instead of hydra... Or I can wrap them all in funcall...
  ("c" (funcall (cs/ae (cs/obj 'character))) "character" :exit t)
  ("f" (funcall (cs/ae (cs/mod 'specific t)
                       (cs/obj 'character)))
   "character-specific" :exit t)
  ("l" (funcall (cs/ae (cs/obj 'line))) "line" :exit t)
  ("w" (funcall (cs/ae (cs/obj 'vi-like-word))) "vi-like-word" :exit t)
  ("W" (funcall (cs/ae (cs/obj 'word))) "word" :exit t)
  ("s" (funcall (cs/ae (with-sptw-req (cs/obj 'sptw)))) "smartparens" :exit t)
  ("i" (funcall (cs/ae (progn (require 'indent-tree) (cs/obj 'indent-tree)))) "indent-tree" :exit t)
  ;; TODO - I want this one, but I keep using this accidentally due to my old key bindings, and it is so frustrating.  So I'll leave it as a no-op for now.
  ;;("o" (funcall (cs/ae (cs/obj 'outline))) "outline" :exit t)
  ("o" (funcall (cs/ae (cs/obj 'NOOP-STOP-USING-THIS-BINDING-FOR-OLD-PURPOSE))) "break habit!" :exit t)
  ("O" (funcall (cs/ae (progn (require 'tree-walk-outline-integration) (cs/obj 'outline)))) "outline" :exit t)
  ("t" (funcall (cs/ae (progn (require 'tree-walk-treesitter-integration)
                              (wgh/initialize-treesit-for-buffer)
                              ;; TODO - also need to initialize treesitter in the buffer before first use...
                              (cs/obj 'tstw-qd))))
   "treesitter-thumb" :exit t)
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
  ("T" (funcall (cs/add (cs/mod 'tree-traversal 'inorder))) "inorder" :exit nil)
  ("r" TODO_register-select "register" :exit nil)
  (" " (funcall (cs/add (cs/mod 'surrounding-space 'surrounding-space))) "surrounding-space" :exit nil)
  ("g" (funcall (cs/add (cs/mod 'current-line-only 'current-line-only))) "current-line-only" :exit nil)

  ("hu" (funcall (cs/ae (cs/obj 'url))) "url" :exit t)
  ("he" (funcall (cs/ae (cs/obj 'email))) "email" :exit t)

  ("\"" (funcall (cs/ae (cs/mod 'delimiter "\"") (with-sptw-req (cs/obj 'sptw)))) "\"" :exit t)
  ("(" (funcall (cs/ae (cs/mod 'delimiter "(") (with-sptw-req (cs/obj 'sptw)))) "()" :exit t)
  (")" (funcall (cs/ae (cs/mod 'delimiter "(") (with-sptw-req (cs/obj 'sptw)))) "()" :exit t)
  ("[" (funcall (cs/ae (cs/mod 'delimiter "[") (with-sptw-req (cs/obj 'sptw)))) "[]" :exit t)
  ("]" (funcall (cs/ae (cs/mod 'delimiter "[") (with-sptw-req (cs/obj 'sptw)))) "[]" :exit t)
  ("{" (funcall (cs/ae (cs/mod 'delimiter "{") (with-sptw-req (cs/obj 'sptw)))) "{}" :exit t)
  ("}" (funcall (cs/ae (cs/mod 'delimiter "{") (with-sptw-req (cs/obj 'sptw)))) "{}" :exit t)
  ("«" (funcall (cs/ae (cs/mod 'delimiter "«") (with-sptw-req (cs/obj 'sptw)))) "«»" :exit t)
  ("»" (funcall (cs/ae (cs/mod 'delimiter "«") (with-sptw-req (cs/obj 'sptw)))) "«»" :exit t)
  ("“" (funcall (cs/ae (cs/mod 'delimiter "“") (with-sptw-req (cs/obj 'sptw)))) "“”" :exit t)
  ("”" (funcall (cs/ae (cs/mod 'delimiter "“") (with-sptw-req (cs/obj 'sptw)))) "“”" :exit t)
  ("⟅" (funcall (cs/ae (cs/mod 'delimiter "⟅") (with-sptw-req (cs/obj 'sptw)))) "⟅⟆" :exit t)
  ("⟆" (funcall (cs/ae (cs/mod 'delimiter "⟅") (with-sptw-req (cs/obj 'sptw)))) "⟅⟆" :exit t)
  ("#" (funcall (cs/ae (cs/mod 'delimiter "#|") (with-sptw-req (cs/obj 'sptw)))) "#||#" :exit t)

  ("q" (funcall (cs/ae (cs/obj 'non-matching-bad-object-just-to-clear-things-out)))
   "quit-cancel-sentence" :exit t)

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big prefix maps

;; g map
;; TODO - try to figure out a theme for this prefix map.  Maybe goto, which fits some of my bindings as well as what the helix editor does.  This map has grown by accretion over time and I would like to organize my accreted maps better.  That said, is it worth re-learning all of these miscellaneous key bindings, esp. those that are relatively low-use but that I have memorized?
;; TODO - if I turn this into a goto-themed map, I should put goto-marker functions here.
(emmap "gg" 'vilish/goto-line/beginning)
(emmap "G" 'vilish/goto-line/end)
(emmap "gdd" 'xref-find-definitions)
(emmap "gdD" 'xref-find-definitions-other-window)
(emmap "gdr" 'xref-find-references)
(emmap "gdi" 'lsp-describe-thing-at-point)
(emmap "gdp" 'pop-tag-mark)
(ecmap "gq" (with-evil 'evil-fill-and-move))
(ecmap "gw" (with-evil 'evil-fill))
(emmap "gv" 'estate-restore-region)


;; t map
(emmap "tt" 'temp-key-map)

(emmap "tia" 'wgh/ido-switch-buffer)
(emmap "tic" 'kill-buffer-or-quit-emacs)
(emmap " tica" 'save-buffers-kill-terminal)
(emmap "tis" 'save-buffer)
(emmap " tisa" 'TODO-write-all)
(emmap "tie" 'save-and-kill-buffer-and-maybe-quit-emacs)
(emmap " tiea" 'TODO-save-all-and-quit)
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
(emmap "tr" 'TODO-select-register)
(emmap "to" 'estate-pager-state)


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
(ecmap "tac" 'comment-region)
(ecmap "taC" 'uncomment-region)
(ecmap "tam" (lambda () (interactive) (exchange-point-and-mark)))
(ecmap "tad" 'insert-date)
(ecmap "taD" 'insert-date-time)
(ecmap "tara" (lambda () (interactive) (require 'alternate-region) (alternate-region-activate)))
(ecmap "tars" (lambda () (interactive) (require 'alternate-region) (alternate-region-swap)))
(ecmap "tarc" (lambda () (interactive) (require 'alternate-region) (alternate-region-cycle)))
(ecmap "tav" 'sp-convolute-sexp)
(ecmap "tag" 'gptel-send)




;; s map
;; TODO - what is the theme of this map?  Does it need one?
(evmap "sh" 'shell-command-on-region)
(ecmap "sh" 'shell-command)
(ecmap "s)" 'eval-last-sexp)
(evmap "s)" 'eval-region)
(evmap "s/" (kbd ":s/ ")) ; TODO - fix this...
(ecmap "sm" (with-evil 'evil-set-marker))
(ecmap "sM" 'bookmark-set)
(ecmap "sg" (cs/ae (cs/obj 'goto-marker)))
(ecmap "sG" 'bookmark-jump)
(emmap "sx" 'eval-expression)
(emmap "sj" 'rmo/pscroll-down-half)
(emmap "sk" 'rmo/pscroll-up-half)
(emmap "sf" 'fold-toggle-wgh)
(emmap "sF" 'fold-toggle-wgh-all)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc maps

(myhydradef completer-map
            ("h" hippie-expand "hippie")
            ("f" he-expand-file-name "file")
            ("l" he-expand-lisp-symbol "lisp")
            ("s" yas-expand "yas")
            ("F" (progn (company-conf-init) (require 'company-files) (call-interactively 'company-files)) "file (company)")
            )

(myhydradef copilot-map
            ("a" copilot-accept-completion "accept")
            ("j" copilot-next-completion "next")
            ("k" copilot-previous-completion "prev")
            ("s" copilot-complete "start")
            )


(defhydra my-window-map (:foreign-keys warn) "WM:"
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

(define-key isearch-mode-map (kbd "RET") (lambda () (interactive) (setq wgh/isearch-go-part 'beginning) (call-interactively 'my-isearch-bor-exit)))
(define-key isearch-mode-map "\C-j" (lambda () (interactive) (setq wgh/isearch-go-part 'end) (call-interactively 'my-isearch-eor-exit)))



;; helm map to match what I've got going in zsh with zaw...
(define-prefix-command 'meta-space-map)
(global-set-key (kbd "M-SPC") 'meta-space-map)
(define-key meta-space-map " " 'helm-helm-commands)
(define-key meta-space-map (kbd "RET") 'helm-helm-commands)
(define-key meta-space-map "c" 'helm-M-x)
(define-key meta-space-map "p" 'helm-browse-project)
(define-key meta-space-map "g" 'helm-do-grep)





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

(message "finished loading keys.el")
