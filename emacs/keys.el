;;; Evil package configuration

;; the following isn't working...
;; these have to be customized.  I'm leaving these comments here too, as an important note.
;(setq-default evil-overriding-maps nil
;              evil-intercept-maps nil)

;;; First, blow up maps so they don't map things like t/T and
;;; insert mode stuff
(setcdr evil-insert-state-map nil)
(setcdr evil-normal-state-map nil)
(setcdr evil-motion-state-map nil)

(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))

;;; define some short-named functions for the most common types of mappings
(defun nobreak-define-key (map keys func)
  (nobreak (define-key map keys func)))
(defun mkmap (keys func)
  (nobreak-define-key evil-motion-state-map keys func))
(defun nkmap (keys func)
  (nobreak-define-key evil-normal-state-map keys func))
(defun vkmap (keys func)
  (nobreak-define-key evil-visual-state-map keys func))
(defun ikmap (keys func)
  (nobreak-define-key evil-insert-state-map keys func))

;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun tkmap (keys func)
  (nobreak-define-key temp-key-map keys func))


;;; DEFAULT BINDINGS SECTION
;;; these are mostly taken from the default evil config, and just put back in
;;; after blowing everything up

;;; Normal state
(nkmap "a" 'evil-append)
(nkmap "A" 'evil-append-line)
(nkmap "c" 'evil-change)
(nkmap "C" 'evil-change-line)
(nkmap "d" 'evil-delete)
(nkmap "D" 'evil-delete-line)
(nkmap "i" 'evil-insert)
(nkmap "I" 'evil-insert-line)
(nkmap "O" 'evil-open-above)
(nkmap "p" 'evil-paste-after)
(nkmap "P" 'evil-paste-before)
(nkmap "q" 'evil-record-macro)
(nkmap "r" 'evil-replace)
(nkmap "R" 'evil-replace-state)
(nkmap "x" 'evil-delete-char)
(nkmap "X" 'evil-delete-backward-char)
(nkmap "y" 'evil-yank)
(nkmap "Y" 'evil-yank-line)
(nkmap "&" 'evil-ex-repeat-substitute)
(nkmap "gq" 'evil-fill-and-move)
(nkmap "gw" 'evil-fill)
(nkmap "zo" 'evil-open-fold)
(nkmap "zc" 'evil-close-fold)
(nkmap "za" 'evil-toggle-fold)
(nkmap "zr" 'evil-open-folds)
(nkmap "zm" 'evil-close-folds)
(nkmap "z=" 'ispell-word)
(nkmap "\C-n" 'evil-paste-pop-next)
(nkmap "\C-p" 'evil-paste-pop)
(nkmap "\C-t" 'pop-tag-mark)
(nkmap (kbd "C-.") 'evil-repeat-pop)
(nkmap (kbd "M-.") 'evil-repeat-pop-next)
(nkmap "." 'evil-repeat)
(nkmap "@" 'evil-execute-macro)
(nkmap "\"" 'evil-use-register)
(nkmap "~" 'evil-invert-char)
(nkmap "=" 'evil-indent)
(nkmap "<" 'evil-shift-left)
(nkmap ">" 'evil-shift-right)
(nkmap "ZZ" 'evil-save-modified-and-close)
(nkmap "ZQ" 'evil-quit)
(nkmap (kbd "DEL") 'repeatable-motion-evil-backward-char)
(nkmap [escape] 'evil-force-normal-state)
(nkmap [remap cua-paste-pop] 'evil-paste-pop)
(nkmap [remap yank-pop] 'evil-paste-pop)

;; undo
(nkmap "u" 'undo)
(nkmap "\C-r" 'redo)

;;; Motion state

;; "0" is a special command when called first
(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(mkmap "1" 'digit-argument)
(mkmap "2" 'digit-argument)
(mkmap "3" 'digit-argument)
(mkmap "4" 'digit-argument)
(mkmap "5" 'digit-argument)
(mkmap "6" 'digit-argument)
(mkmap "7" 'digit-argument)
(mkmap "8" 'digit-argument)
(mkmap "9" 'digit-argument)
(mkmap "b" 'repeatable-motion-evil-backward-word-begin)
(mkmap "B" 'repeatable-motion-evil-backward-WORD-begin)
(mkmap "E" 'repeatable-motion-evil-forward-WORD-end)
(mkmap "f" 'repeatable-motion-evil-find-char)
(mkmap "F" 'repeatable-motion-evil-find-char-backward)
(mkmap "G" 'evil-goto-line)
(mkmap "h" (lambda () (interactive) (message "use oc")))
(mkmap "H" 'evil-window-top)
(mkmap "j" 'repeatable-motion-evil-next-line)
(mkmap "k" 'repeatable-motion-evil-previous-line)
(mkmap "l" (lambda () (interactive) (message "use ec")))
(mkmap "K" 'evil-lookup)
(mkmap "L" 'evil-window-bottom)
(mkmap "M" 'evil-window-middle)
(mkmap "n" 'evil-search-next)
(mkmap "N" 'evil-search-previous)
(mkmap "w" 'repeatable-motion-evil-forward-word-begin)
(mkmap "W" 'repeatable-motion-evil-forward-WORD-begin)
(mkmap "ge" 'repeatable-motion-evil-backward-word-end)
(mkmap "gE" 'repeatable-motion-evil-backward-WORD-end)
(mkmap "gg" 'evil-goto-first-line)
(mkmap "gj" 'evil-next-visual-line)
(mkmap "gk" 'evil-previous-visual-line)
(mkmap "g0" 'evil-beginning-of-visual-line)
(mkmap "g_" 'evil-last-non-blank)
(mkmap "g^" 'evil-first-non-blank-of-visual-line)
(mkmap "gm" 'evil-middle-of-visual-line)
(mkmap "g$" 'evil-end-of-visual-line)
(mkmap "g\C-]" 'find-tag)
(mkmap "{" 'repeatable-motion-evil-backward-paragraph)
(mkmap "}" 'repeatable-motion-evil-forward-paragraph)
(mkmap "#" 'repeatable-motion-evil-search-word-backward)
(mkmap "g#" 'repeatable-motion-evil-search-unbounded-word-backward)
(mkmap "$" 'evil-end-of-line)
(mkmap "%" 'evil-jump-item)
(mkmap "`" 'evil-goto-mark)
(mkmap "'" 'evil-goto-mark-line)
(mkmap "(" 'repeatable-motion-evil-backward-sentence)
(mkmap ")" 'repeatable-motion-evil-forward-sentence)
(mkmap "]]" 'repeatable-motion-evil-forward-section-begin)
(mkmap "][" 'repeatable-motion-evil-forward-section-end)
(mkmap "[[" 'repeatable-motion-evil-backward-section-begin)
(mkmap "[]" 'repeatable-motion-evil-backward-section-end)
(mkmap "[(" 'repeatable-motion-evil-previous-open-paren)
(mkmap "])" 'repeatable-motion-evil-next-close-paren)
(mkmap "[{" 'repeatable-motion-evil-previous-open-brace)
(mkmap "]}" 'repeatable-motion-evil-next-close-brace)
(mkmap "*" 'repeatable-motion-evil-search-word-forward)
(mkmap "g*" 'repeatable-motion-evil-search-unbounded-word-forward)
(mkmap "," 'evil-repeat-find-char-reverse)
(mkmap "/" 'repeatable-motion-evil-search-forward)
(mkmap ";" 'repeatable-motion-evil-repeat-find-char)
(mkmap "?" 'repeatable-motion-evil-search-backward)
(mkmap "|" 'evil-goto-column)
(mkmap "^" 'evil-first-non-blank)
(mkmap "+" 'evil-next-line-first-non-blank)
(mkmap "_" 'evil-next-line-1-first-non-blank)
(mkmap "-" 'evil-previous-line-first-non-blank)
(mkmap "\C-w" 'evil-window-map)
(mkmap "\C-]" 'evil-jump-to-tag)
(mkmap (kbd "C-b") 'evil-scroll-page-up)
(mkmap (kbd "C-d") 'evil-scroll-down)
(mkmap (kbd "C-e") 'evil-scroll-line-down)
(mkmap (kbd "C-f") 'evil-scroll-page-down)
(mkmap (kbd "C-o") 'evil-jump-backward)
(mkmap (kbd "C-y") 'evil-scroll-line-up)
(mkmap "\\" 'evil-execute-in-emacs-state)
(mkmap "z^" 'evil-scroll-top-line-to-bottom)
(mkmap "z+" 'evil-scroll-bottom-line-to-top)
(mkmap "zt" 'evil-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(mkmap (vconcat "z" [return]) "zt^")
(mkmap (kbd "z RET") (vconcat "z" [return]))
(mkmap "zz" 'evil-scroll-line-to-center)
(mkmap "z." "zz^")
(mkmap "zb" 'evil-scroll-line-to-bottom)
(mkmap "z-" "zb^")
(mkmap "v" 'evil-visual-char)
(mkmap "V" 'evil-visual-line)
(mkmap "\C-v" 'evil-visual-block)
(mkmap "gv" 'evil-visual-restore)
(mkmap [left] 'repeatable-motion-evil-backward-char)
(mkmap [right] 'repeatable-motion-evil-forward-char)
(mkmap [up] 'repeatable-motion-evil-previous-line)
(mkmap [down] 'repeatable-motion-evil-next-line)

(vkmap [escape] 'evil-exit-visual-state)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Minibuffer

;(define-key minibuffer-local-map "\C-p" 'evil-complete-next)
;(define-key minibuffer-local-map "\C-n" 'evil-complete-previous)
;(define-key minibuffer-local-map "\C-x\C-p" 'evil-complete-next-line)
;(define-key minibuffer-local-map "\C-x\C-n" 'evil-complete-previous-line)

;; Ex
(mkmap ":" 'evil-ex)
(mkmap "!" 'evil-shell-command)
;
;;; search command line
;(define-key evil-ex-search-keymap "\d" #'evil-ex-delete-backward-char)
;


;;; CUSTOM BINDINGS SECTION

;; window commands
(defhydra evil-window-map (:foreign-keys warn) "WM:"
  ("v" evil-window-vsplit nil)
  ("s" evil-window-split nil)
  ("j" evil-window-next nil)
  ("k" evil-window-prev nil)
  ("c" delete-window nil)
  ("h" (lambda () (interactive)
     (let ((current-prefix-arg '(5)))
       (call-interactively 'evil-window-decrease-width))) "skinny")
  ("l" (lambda () (interactive)
     (let ((current-prefix-arg '(5)))
       (call-interactively 'evil-window-increase-width))) "fat")
  ("H" (lambda () (interactive)
     (let ((current-prefix-arg '(5)))
       (call-interactively 'evil-window-increase-height))) "tall")
  ("L" (lambda () (interactive)
     (let ((current-prefix-arg '(5)))
       (call-interactively 'evil-window-decrease-height))) "short")
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
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-inner-text-objects-map "e" 'er/expand-region)

;; Normal state switch!
(key-chord-define evil-insert-state-map (kbd "kj") 'evil-normal-state)
(key-chord-define evil-replace-state-map (kbd "kj") 'evil-normal-state)

; Keys unbound and reserved for future use - bind to nop so they don't input
(mkmap (kbd "RET") 'ignore)
(mkmap "S" 'ignore)
(mkmap "T" 'ignore)

; g map
(mkmap "gr" 'evil-ace-jump-word-mode)
(mkmap "gc" 'evil-ace-jump-char-mode)
(mkmap "gf" 'evil-ace-jump-line-mode)
(mkmap "gdd" 'evil-goto-definition)
(mkmap "gdp" 'pop-tag-mark)
(nkmap " yc" 'xcopy)
(nkmap " Pc" 'xpaste)
(nkmap " pc" 'xpaste-after-char)
(mkmap "gl" 'evil-scroll-right)
(mkmap "gh" 'evil-scroll-left)
(mkmap "gn" 'evil-next-match)
(mkmap "gN" 'evil-previous-match)

(nkmap "g&" 'evil-ex-repeat-global-substitute)
(nkmap "gi" 'evil-insert-resume) ; insert mode at ins. mode cursor point
(nkmap "gu" 'evil-downcase)
(nkmap "gU" 'evil-upcase)
(nkmap "g~" 'evil-invert-case)
(nkmap "g;" 'goto-last-change)
(nkmap "g," 'goto-last-change-reverse)

; o and e maps - o is left/back, e is right/forward
(mkmap "ee" 'repeatable-motion-evil-forward-word-end)
(mkmap "oe" 'repeatable-motion-evil-backward-word-end)
(mkmap "eE" 'repeatable-motion-evil-forward-WORD-end)
(mkmap "oE" 'repeatable-motion-evil-backward-WORD-end)
(nkmap "eo" 'evil-open-below)
(vkmap "o" nil)
(vkmap "oo" 'exchange-point-and-mark)
(vkmap "eo" 'exchange-point-and-mark)
(nkmap "oo" 'evil-open-above)
(mkmap "ec" 'repeatable-motion-evil-forward-char)
(mkmap "oc" 'repeatable-motion-evil-backward-char)
;(mkmap "ed" 'repeatable-motion-evil-next-close-paren)
;(mkmap "od" 'repeatable-motion-evil-previous-open-paren)
;(mkmap "ed" 'repeatable-motion-evil-next-close-brace)
;(mkmap "od" 'repeatable-motion-evil-previous-open-brace)
(mkmap "ed" 'repeatable-motion-on-parens-down-sexp)
(mkmap "od" 'repeatable-motion-on-parens-up-sexp)
(mkmap "eD" 'repeatable-motion-on-parens-down-sexp-end)
(mkmap "oD" 'repeatable-motion-on-parens-up-sexp-end)
(mkmap "eh" 'repeatable-motion-on-parens-forward-sexp)
(mkmap "oh" 'repeatable-motion-on-parens-backward-sexp)
(mkmap "eH" 'repeatable-motion-on-parens-forward-sexp-end)
(mkmap "oH" 'repeatable-motion-on-parens-backward-sexp-end)
(mkmap "en" 'repeatable-motion-on-parens-forward-sexp-in-supersexp)
(mkmap "on" 'repeatable-motion-on-parens-backward-sexp-in-supersexp)
(mkmap "ea" 'repeatable-motion-evil-forward-arg)
(mkmap "oa" 'repeatable-motion-evil-backward-arg)
(mkmap "ew" 'repeatable-motion-evil-forward-little-word-begin)
(mkmap "ow" 'repeatable-motion-evil-backward-little-word-begin)
(mkmap "eW" 'repeatable-motion-evil-forward-little-word-end)
(mkmap "oW" 'repeatable-motion-evil-backward-little-word-end)
(mkmap "ep" 'repeatable-motion-evil-forward-sentence-begin)
(mkmap "op" 'repeatable-motion-evil-backward-sentence-begin)
(mkmap "eP" 'repeatable-motion-evil-forward-paragraph)
(mkmap "oP" 'repeatable-motion-evil-backward-paragraph)
(mkmap "es" 'repeatable-motion-evil-forward-section-begin)
(mkmap "os" 'repeatable-motion-evil-backward-section-begin)
(mkmap "eS" 'repeatable-motion-evil-forward-section-end)
(mkmap "oS" 'repeatable-motion-evil-backward-section-end)
(mkmap "et" 'repeatable-motion-evil-find-char-to)
(mkmap "ot" 'repeatable-motion-evil-find-char-to-backward)
(mkmap "ef" 'repeatable-motion-evil-find-char)
(mkmap "of" 'repeatable-motion-evil-find-char-backward)
(mkmap "ol" 'evil-goto-column)
(mkmap "o l" 'evil-first-non-blank)
(mkmap "el" 'evil-end-of-line)
(mkmap "oL" 'evil-beginning-of-visual-line)
(mkmap "eL" 'evil-end-of-visual-line)
(mkmap "eb" 'evil-next-line-first-non-blank)
(mkmap "ob" 'evil-previous-line-first-non-blank)
(mkmap "oB" 'evil-first-non-blank)
(mkmap "eB" 'evil-next-line-1-first-non-blank)
(mkmap "eO" 'repeatable-motion-forward-symbol)
(mkmap "oO" 'repeatable-motion-backward-symbol)
(mkmap "ej" 'repeatable-motion-evil-jump-forward)
(mkmap "oj" 'repeatable-motion-evil-jump-backward)
;; eu/ou for destructive subcommands!
(mkmap "eus" 'on-parens-forward-slurp)
(mkmap "ous" 'on-parens-backward-slurp)
(mkmap "eub" 'on-parens-forward-barf)
(mkmap "oub" 'on-parens-backward-barf)
(mkmap "euj" 'on-parens-join-neighbor-sexp)
(mkmap "ouj" 'on-parens-split-supersexp)
;; this one doesn't really belong...
(mkmap "eup" 'on-parens-splice)
(mkmap "oup" 'on-parens-splice)

(mkmap "f" 'repeatable-motion-forward)
(mkmap "F" 'repeatable-motion-backward)

; t map
(mkmap "tt" 'temp-key-map)

(mkmap "tia" 'switch-to-buffer)
(mkmap "tic" 'kill-buffer-or-quit-emacs)
(mkmap " tica" 'evil-quit-all)
(mkmap "tis" 'save-buffer)
(mkmap " tisa" 'evil-write-all)
(mkmap "tie" 'save-and-kill-buffer-and-maybe-quit-emacs)
(mkmap " tiea" 'evil-save-and-quit)
(mkmap "tif" 'ido-ffap-no)
(mkmap " tifd" 'ido-find-file-from-pwd)
(mkmap " tiff" 'ido-ffap-yes)
(mkmap "tiw" 'next-buffer-no-star)
(mkmap "tib" 'prev-buffer-no-star)
(mkmap " tiwd" 'next-dirty-buffer-no-star)
(mkmap " tibd" 'prev-dirty-buffer-no-star)

(mkmap "th" 'evil-window-map/body)
(mkmap "tp" 'projectile-command-map)
(mkmap "tr" 'evil-use-register)
;; "ts" will stand for "toggle setting"
(mkmap "ts"
  (defhydra settings-toggle (:foreign-keys warn :exit t) "Toggle:"
    ("p" smartparens-mode "smartparens")
    ("w" whitespace "whitespace")
    ("C" rainbow-mode "#aabbcc")
    ("c" company-mode "company")
    ("t" toggle-truncate-lines "trunc")
    ("i" toggle-case-fold-search "/? case")
    ("W" toggle-wrap-scan "search-wrap")
    ("f" flycheck-mode "flycheck")
    ("F" fci-mode-toggle "fill-col")
    ("s" flyspell-mode "flyspell")
    ("S" flyspell-prog-mode "flyspell-prog")
    ("e" electric-indent-mode "el.indent")
    ("d" rainbow-delimiters-mode "rainbow{}")
    ("r" linum-relative-toggle "linum-rel")
    ("h" evil-search-highlight-persist-remove-all "search-highlight-now")
    ("H" evil-search-highlight-persist "search-highlight-ever")
    ("M" menu-bar-mode "menu-bar")
    ("m" (lambda () (interactive)
           (menu-bar-mode 1) (menu-bar-open))
     "menu-open")
    ("I" indent-guide-mode "indent-guide")
    ))

(mkmap "tl"
  (defhydra list-stuff-map (:foreign-keys warn :exit t) "List:"
    ("b" list-buffers "buffers")
    ("m" evil-show-marks "marks")
    ("M" bookmark-bmenu-list "bookmarks")
                                        ;("tlk" 'list-keymaps) ; TODO - make this function
    ("c" list-colors-display "colors")
    ("f" list-faces-display "faces")
    ("r" evil-show-registers "registers")
    ))
; TODO - list jumps, maybe

;; s map
;(nkmap "ss" 'evil-substitute)
;(nkmap "sS" 'evil-change-whole-line)
(evil-define-key 'visual evil-surround-mode-map "s" nil)
(evil-define-key 'visual evil-surround-mode-map "S" 'ignore)
(defun surround-region-with-parens (beg end)
  (interactive "r")
  (evil-surround-region beg end nil ?\)))
(vkmap ")" 'surround-region-with-parens)
(vkmap "ss" 'evil-surround-region)
(vkmap "sS" 'evil-Surround-region)
(vkmap "sh" 'shell-command-on-region)
(nkmap "sh" 'shell-command)
(mkmap "sa" 'evil-ex)
(nkmap "s)" 'eval-last-sexp)
(vkmap "s)" 'eval-region)
(vkmap "s/" (kbd ":s/ ")) ; TODO - fix this...
(nkmap "sm" 'evil-set-marker)
(nkmap "sM" 'bookmark-set)
(nkmap "sg" 'evil-goto-mark)
(nkmap "sG" 'bookmark-jump)
(ikmap (kbd "M-c") 'smex)
(mkmap (kbd "M-c") 'smex)
(global-set-key (kbd "M-c") 'smex)
(mkmap "sx" 'eval-expression)

(vkmap (kbd "C-s") 'yas-insert-with-region)

;; command modes and macros
(mkmap "-" 'smex)
;(mkmap "|" 'execute-extended-command)
(mkmap "|" 'eval-expression)
(mkmap "_" 'eval-expression)
(mkmap "Q" 'call-last-kbd-macro)
;; Movement
(nkmap "mm" 'evil-set-marker) ;;;;;;;;;; m will be my prefix for mode-specific bindings
;; everything in motion state is pulled into normal state
(mkmap "+" 'evil-repeat-find-char)
(mkmap "~" 'evil-repeat-find-char-reverse)
(mkmap "J" 'evil-window-bottom)
(mkmap "K" 'evil-window-top)
(mkmap "{" 'backward-sexp)
(mkmap "}" 'forward-sexp)
(mkmap "[" 'backward-list)
(mkmap "]" 'forward-list)
(mkmap (kbd "C-z") 'suspend-frame)
(ikmap (kbd "C-z") 'suspend-frame)

; space map
(mkmap "sj" 'scroll-up)
(mkmap "sk" 'scroll-down)
(mkmap "sf" 'yafolding-toggle-element)
(mkmap " sfa" 'yafolding-toggle-all)
(mkmap " /"
  (myhydradef search-hydra
              ("s" helm-swoop "swoop")
              ("a" helm-multi-swoop-all "multi-swoop all")
              ("m" helm-multi-swoop "multi-swoop")
  ))
(mkmap " -h" 'helm-M-x)
(mkmap " &g" 'evil-ex-repeat-global-substitute)
(mkmap " va" 'mark-whole-buffer)

;; Joining
;; TODO -- make a better mapping for this.  I should make my prefixes be mnemonic or something...
;;         for instance, g<key> is mostly navigation... t... mosty has window stuff in th... space is mostly one handed
;;         navigation aside from this one
(mkmap " j"
  (myhydradef j-hydra
              ("l" evil-join "join lines")
              ("w" evil-join-whitespace "join whitespace")
              ))


;; input mode
(ikmap (kbd "DEL") 'delete-backward-char) ; remap away from the evil-version backspace
(ikmap "\C-v" #'quoted-insert) ; more vim-like
(ikmap "\M-r" 'evil-paste-from-register)

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
            ("n" evil-complete-next "vim-n")
            ("p" evil-complete-previous "vim-p")
            ("f" he-expand-file-name "file")
            ("l" he-expand-lisp-symbol "lisp")
            ("s" yas-expand "yas")
)
(ikmap "\M-h" 'completer-map/body)
(ikmap (kbd "C-SPC TAB") completer-map/body)
(ikmap (kbd "C-@ TAB") completer-map/body)
(ikmap (kbd "TAB") 'company-complete-common-wgh)
;; put indentation on something...
(define-key completer-map (kbd "TAB") 'indent-for-tab-command)
(ikmap (kbd "<backtab>") 'indent-for-tab-command)

(global-set-key (kbd "C-\\") 'evil-execute-in-normal-state)

;; Default mode settings
(setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
(setq evil-emacs-state-modes nil)


;; Mouse keys...
;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))

(define-key tty-menu-navigation-map "j" 'tty-menu-next-item)
(define-key tty-menu-navigation-map "k" 'tty-menu-prev-item)
(define-key tty-menu-navigation-map "h" 'tty-menu-prev-menu)
(define-key tty-menu-navigation-map "l" 'tty-menu-next-menu)

(define-key help-map "\C-h" 'describe-prefix-bindings)

(define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
(define-key evil-ex-completion-map "\C-e" 'move-end-of-line)
(define-key evil-ex-completion-map "\C-d" 'delete-char)
(define-key evil-ex-completion-map "\C-k" 'kill-line)
(define-key evil-ex-completion-map "\C-p" 'previous-complete-history-element)
(define-key evil-ex-completion-map "\C-n" 'next-complete-history-element)
(define-key evil-ex-completion-map "\C-f" 'forward-char)
(define-key evil-ex-completion-map "\C-b" 'backward-char)
(define-key evil-ex-completion-map "\M-r" 'evil-paste-from-register)

