;;; -*- lexical-binding: t; -*-

;; TODO - review motion vs normal state bindings -- these accreted over time with little thought, and maybe I should just remove motion map?

;; Repeatable-motion config.  Long ago I set the default for these to be some
;; annoyingly long thing that also doesn't have a slash, for Melpa coding
;; standards, but I always have used these shorter prefixes.
(setq repeatable-motion-count-needed-prefix "rmo-c/")
(setq repeatable-motion-definition-prefix "rmo/")
(setq repeatable-motion-training-wheels-p nil)
(require 'repeatable-motion)
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
(setq composiphrase-current-configuration composiphrase-demo-match-config)
(require 'aggreact)
(require 'composiphrase-estate-aggreact-config)
(aggreact-mode 1)

(require 'cpo-search-movements)
(require 'cpo-helpers)

;; (setq cpo-paste-default-register ?P)
;; (setq cpo-copy-default-register ?P)
;; (setq cpo-copy-sync-with-kill-ring-register ?P)
;; (setq cpo-delete-default-register ?D)
;; (setq cpo-change-default-register ?D)
;; (setq cpo-paste-copy-old-default-register ?C)



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nobreak-define-key (map keys func)
  (with-demoted-errors "Error: %s" (define-key map keys func)))
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




(require 'hydra)

;; Hydra note: add more bindings with (defhydra+ NAME HYDRA-OPTIONS HEAD ...), where HYDRA-OPTIONS can be nil to not change the options.



(emmap "\M-x" 'execute-extended-command)
(eimap "\M-x" 'execute-extended-command)


;; Normal state switch!
(emmap (kbd "C-c") 'ignore)
(evmap "\C-c" 'estate-normal-state)
(eimap "\C-c" 'estate-normal-state)
(eImap "\C-c" 'estate-normal-state)
(eimap "\C-l" 'estate-normal-state)
(eImap (kbd "<escape>") 'estate-normal-state)
(evmap (kbd "<escape>") 'estate-normal-state)
(eImap (kbd "ESC") 'estate-normal-state)
(evmap (kbd "ESC") 'estate-normal-state)

(emmap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)
(eimap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)
(evmap "\C-g" 'keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert state

(eimap "\C-v" 'quoted-insert)


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

(emmap "q" 'estate-keyboard-macro-to-register-end-most-recent-or-start-default)
(emmap "Q" 'estate-keyboard-macro-to-register-start)
(emmap "r" 'estate-keyboard-macro-execute-from-most-recently-macro-recorded-register)
(emmap "R" 'estate-keyboard-macro-execute-from-register)

(enmap "." 'cp-demo-aggreact-repeat-latest-editing)

;; TODO - use command sentence
(enmap "x" 'delete-char)
(enmap "X" 'delete-char-backward)


;; TODO - should I re-implement these in terms of composiphrase?  I wrote these
;; implementations after I wrote estate-mode but before I wrote composiphrase.
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

(enmap "C" (lambda () (interactive) (progn (kill-line) (estate-insert-state))))
(evmap "C" (lambda () (interactive (progn (kill-region (region-beginning)
                                                       (region-end))
                                          (estate-insert-state)))))

(enmap "D" (lambda () (interactive) (progn (kill-line))))
(evmap "D" (lambda () (interactive) (progn (kill-region nil nil t))))

(emmap "Y" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cp/ae (cp/verb 'copy)
                                 (cp/obj 'region))
                          n)
               (funcall (cp/ae (cp/verb 'copy)
                               (cp/obj 'line)
                               (cp/mod 'direction 'expand-region))
                        n))))

(enmap "c" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cp/ae (cp/verb 'change)
                                 (cp/obj 'region))
                          n)
               (funcall (cp/add (cp/verb 'change))
                        n))))
(enmap "d" (lambda (n)
             (interactive "p")
             (if (region-active-p)
                 (funcall (cp/ae (cp/verb 'delete)
                                 (cp/obj 'region))
                          n)
               (funcall (cp/add (cp/verb 'delete))
                        n))))



(emmap "y" (lambda (n) (interactive "p")
             (if (region-active-p)
                 (funcall (cp/ae (cp/verb 'copy)
                                 (cp/obj 'region))
                          n)
               (funcall (cp/add (cp/verb 'copy))
                        n))))
(enmap "p" (cp/ae (cp/verb 'paste-to-region-from-move)
                  (cp/obj 'region)))

(emmap "\"" (lambda (n) (interactive "p")
              (funcall (cp/add (let ((reg (read-key "Register: ")))
                                 (cp/mod 'register reg (format "r:%c" reg))))
                       n)))

(enmap "u" 'undo)
(enmap "\C-r" 'undo-tree-redo)



(emmap (kbd "DEL") 'rmo/backward-char)
(emmap (kbd "<deletechar>") 'rmo/forward-char)



(emmap "f" (cp/ae (cp/mod 'direction 'forward)
                  (cp/obj 'repeatable-motion-repeat)))
(emmap "F" (cp/ae (cp/mod 'direction 'backward)
                  (cp/obj 'repeatable-motion-repeat)))
(emmap ";" (cp/ae (cp/mod 'direction 'forward)
                  (cp/obj 'repeatable-motion-repeat)))
(emmap "," (cp/ae (cp/mod 'direction 'backward)
                  (cp/obj 'repeatable-motion-repeat)))


;; Enter prefix maps for composiphrase
(emmap "s" (lambda ()
             (interactive)
             (funcall (cp/add `((word-type . ignore))))
             (cpo/verb-select/body)))

(emmap "l" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'forward)) n)
             (cpo/object-select/body)))
(emmap "h" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'backward)) n)
             (cpo/object-select/body)))


(emmap "i" (lambda (n) (interactive "p")
             (if (null composiphrase-current-sentence)
                 (estate-insert-state) ;; TODO - handle number
               (progn
                 (funcall (cp/add (cp/mod 'direction 'expand-region)
                                  (cp/mod 'inner t "inner"))
                          n)
                 (cpo/object-select/body)))))
(evmap "i" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'expand-region)
                              (cp/mod 'inner t "inner"))
                      n)
             (cpo/object-select/body)))
(emmap "a" (lambda (n) (interactive "p")
             (funcall (cp/add (cp/mod 'direction 'expand-region))
                      n)
             (cpo/object-select/body)))



(emmap "j" (cp/ae (cp/mod 'direction 'forward)
                  (cp/mod 'location-within 'keep-if-possible)
                  (cp/obj 'line)))
(emmap "k" (cp/ae (cp/mod 'direction 'backward)
                  (cp/mod 'location-within 'keep-if-possible)
                  (cp/obj 'line)))

;; (emmap "w" (cp/ae (cp/mod 'direction 'forward)
;;                   (cp/obj 'cpo-vi-like-word)))
;; (emmap "b" (cp/ae (cp/mod 'direction 'backward)
;;                   (cp/obj 'cpo-vi-like-word)))

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
                             (cp/mod 'inner t "inner")
                             (cp/mod 'delimiter 'any)
                             (cp/obj 'cpo-smartparens))
                      n)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; composiphrase cpo/verb-select map
(defhydra cpo/verb-select (:foreign-keys warn :exit nil) "Verb:"
  ;; Hydra handles commands specially, such that I need a defun for each one or
  ;; I need to wrap with a literal lambda.  So it adds a lot of verbosity.
  ;; Maybe I should write a macro to reduce boilerplate.  But at any rate, Hydra
  ;; provides the “stay in map until hitting an object” behavior.
  ("C-g" keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state "quit" :exit t)

  ;; Verbs that automatically take region if the region is active.
  ;; These are the ones where you maybe should select first anyway.
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

  ;; Verbs that don't automatically take region -- they still need an object
  ;; argument to decide what to do.
  ;; Some of these should still work with a region -- eg. transpose with a
  ;; region should transpose based on the text object given, but with multiple
  ;; of the object based on the current region.  Eg. transpose multiple lines
  ;; together, multiple s-expressions together, multiple indent trees together,
  ;; etc.
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


(defhydra cpo/object-select (:foreign-keys warn :exit nil) "Obj:"
  ;; Hydra handles commands specially, such that I need a defun for each one or
  ;; I need to wrap with a literal lambda.  So it adds a lot of verbosity.
  ;; Maybe I should write a macro to reduce boilerplate.  But at any rate, Hydra
  ;; provides the “stay in map until hitting an object” behavior.
  ("C-g" keyboard-quit-and-clear-composiphrase-and-maybe-leave-visual-state
   "quit" :exit t)

  ("c" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'character)) n))
   "character" :exit t)
  ;; This encoding is sketchy... but oh well.
  ("f" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'specific t)
                         (cp/obj 'character))
                  n))
   "character-specific" :exit t)
  ("l" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'line)) n))
   "line" :exit t)
  ;; TODO - consider whether to keep using this vi-like word, or the emacs word, or something else.  Also how to deal with sub-words in symbols.
  ;;("w" (lambda (n) (interactive "p") (funcall (cp/ae (cp/obj 'cpo-vi-like-word)) n)) "vi-like-word" :exit t)
  ("w" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'word)) n))
   "word" :exit t)
  ("y" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'symbol)) n))
   "symbol" :exit t)
  ("Y" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'sexp)) n))
   "sexp" :exit t)
  ("P" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'paragraph)) n))
   "paragraph" :exit t)
  ("S" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'sentence)) n))
   "sentence" :exit t)
  ("B" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'buffer)) n))
   "buffer" :exit t)
  ("s" (lambda (n) (interactive "p")
         (funcall (cp/ae (with-cpo-smartparens-req (cp/obj 'cpo-smartparens))) n))
   "smartparens" :exit t)
  ("e" (lambda (n) (interactive "p")
         (funcall (cp/ae (progn (require 'cpo-indent-tree) (cp/obj 'cpo-indent-tree))) n))
   "indent-tree" :exit t)
  ("o" (lambda (n) (interactive "p")
         (funcall (cp/ae (progn (require 'cpo-outline) (cp/obj 'outline))) n))
   "outline" :exit t)
  ("t" (lambda (n) (interactive "p")
         (funcall (cp/ae (progn (require 'cpo-treesitter-qd)
                                ;;(cpo/initialize-treesit-for-buffer)
                                ;; TODO - also need to initialize treesitter in the buffer before first use...
                                (cp/obj 'cpo-treesitter-qd)))
                  n))
   "treesitter-qd" :exit t)
  ("x" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'xml)) n))
   "xml" :exit t)
  ("X" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'xml-tag)) n))
   "xml-tag" :exit t)
  ("j" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'json)) n))
   "json" :exit t)

  ;; It's hard to decide priorities for myself, especially for things not yet implemented.  I want to reserve space in my map for future things.

  ("g" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'argument)) n))
   "argument" :exit t)
  ("D" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'definition)) n))
   "definition" :exit t)
  ("F" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'function)) n))
   "function" :exit t)
  ("M" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'statement)) n))
   "statement" :exit t)
  ("C" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'comment)) n))
   "comment" :exit t)
  ("L" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'list)) n))
   "list" :exit t)
  ("hL" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'linter-warning)) n))
   "linter-warning" :exit t)
  ("p" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/obj 'proposed-change)) n))
   "proposed-change" :exit t)
  ("hC" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'class)) n))
   "class" :exit t)
  ("hX" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'test)) n))
   "test" :exit t)
  ("hc" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'buffer-change)) n))
   "buffer-change" :exit t)
  ("hg" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'vcs-change)) n))
   "vcs-change" :exit t)
  ("h SPC" (lambda (n) (interactive "p")
             (funcall (cp/ae (cp/obj 'whitespace)) n))
   "whitespace" :exit t)
  ("hu" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'url)) n))
   "url" :exit t)
  ("he" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'email)) n))
   "email" :exit t)
  ("hP" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'phone-number)) n))
   "phone-number" :exit t)
  ("hf" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/obj 'file-name)) n))
   "file-name" :exit t)


  ;; Specific delimiters, use smartparens for them.

  ("\"" (lambda (n) (interactive "p")
          (funcall (cp/ae (cp/mod 'delimiter "\"")
                          (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                   n))
   "\"" :exit t)
  ("'" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "'")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "'" :exit t)
  ("`" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "`")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "`" :exit t)
  ("(" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "(")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "()" :exit t)
  (")" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "(")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "()" :exit t)
  ("[" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "[")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "[]" :exit t)
  ("]" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "[")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "[]" :exit t)
  ("{" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "{")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "{}" :exit t)
  ("}" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "{")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "{}" :exit t)
  ("«" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "«")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "«»" :exit t)
  ("»" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "«")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "«»" :exit t)
  ("“" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "“")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "“”" :exit t)
  ("”" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "“")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "“”" :exit t)
  ("⟅" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "⟅")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "⟅⟆" :exit t)
  ("⟆" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "⟅")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "⟅⟆" :exit t)
  ("#" (lambda (n) (interactive "p")
         (funcall (cp/ae (cp/mod 'delimiter "#|")
                         (with-cpo-smartparens-req (cp/obj 'cpo-smartparens)))
                  n))
   "#||#" :exit t)

  ;; Modifiers -- maybe these should have a separate map, but that adds
  ;; verbosity, and I'm not yet certain there are enough objects and modifiers
  ;; to warrant splitting -- I can have a prefix within this map for infrequent
  ;; things, and I can always add another separate prefix map.
  ("n" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'location-within 'end)) n))
   "end" :exit nil)
  ;;("hb" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'location-within 'beginning)) n)) "beginning" :exit nil) ;; Default, but let's add it anyway.
  ;;("he" (lambda (n) (interactive "p") (funcall (cp/add (cp/mod 'location-within 'emacs-style)) n)) "emacs-style" :exit nil)
  ("i" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'inner 'inner)) n))
   "inner" :exit nil)
  ("u" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'tree-vertical 'up)) n))
   "up" :exit nil)
  ("d" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'tree-vertical 'down)) n))
   "down" :exit nil)
  ("T" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'tree-traversal 'inorder)) n))
   "inorder" :exit nil)
  ("r" (lambda (n) (interactive "p")
         (let ((reg (read-key "register: ")))
           (funcall (cp/add (cp/mod 'register reg (format "r:%c" reg))) n)))
   "register" :exit nil)
  (" " (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'surrounding-space 'surrounding-space)) n))
   "surrounding-space" :exit nil)
  ("b" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'absolute 'absolute)) n))
   "absolute" :exit nil) ;; For absolute numbering (within tree if respect tree is on).  Ignore forward/backward direction.
  ("m" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'matching 'matching)) n))
   "matching" :exit nil) ;; Eg. for finding the next matching word, symbol, whatever.
  ("a" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'alternate 'alternate)) n))
   "alternate" :exit nil) ;; For object-specific alternate behavior...
  ("A" (lambda (n) (interactive "p")
         (funcall (cp/add (cp/mod 'alternate 2)) n))
   "alternate-2" :exit nil)

  ("hR" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/mod 'respect-tree 'respect-tree)) n))
   "respect-tree" :exit nil)
  ("hr" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/mod 'respect-tree nil)) n))
   "DISrespect-tree" :exit nil)
  ("hD" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/mod 'direction nil)) n))
   "No direction" :exit nil) ;; Is this useful?  Maybe.  Currently I have no way to get into the map without specifying a direction...
  ("hi" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/mod 'idempotent 'idempotent)) n))
   "idempotent" :exit nil) ;; Eg. only move if not at a place where this movement would have moved to.  Useful for keyboard macros, maybe.
  ("hl" (lambda (n) (interactive "p")
          (funcall (cp/add (cp/mod 'current-line-only 'current-line-only)) n))
   "current-line-only" :exit nil) ;; Eg. do something on the current line only.  Uncertain how useful this is.

  )



;; g map

;; TODO - try to figure out a theme for this prefix map.  Maybe goto, which fits
;; some of my bindings as well as what the helix editor does.  This map has
;; grown by accretion over time and I would like to organize my accreted maps
;; better.  That said, is it worth re-learning all of these miscellaneous key
;; bindings, esp. those that are relatively low-use but that I have memorized?
;; TODO - if I turn this into a goto-themed map, I should put jump-to-register
;; functions here.
(emmap "gg" 'cpo-goto-line-default-first)
(emmap "G" 'cpo-goto-line-default-last)

(emmap "m" 'point-to-register)
(emmap "z" 'execute-extended-command)



(define-key isearch-mode-map (kbd "RET") 'cpo-isearch-bor-exit)
(define-key isearch-mode-map "\C-j" 'cpo-isearch-eor-exit)


