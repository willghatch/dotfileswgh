;;; Key config

;; Keys I don't really use:

;; Insert mode:
;; C-o
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
;; m -- I think I wanted this to be mode-specific -- eg. a fallback to normal emacs keys
;; O -- I have it as oo
;; S,T,U,Y -- all no-ops or duplicates, or things I don't use.
;; z,Z -- these are currently sub-maps, but I don't use them for anything.
;; non-character symbols?
;; #, $, comma, +
;; & I don't really use, maybe I should
;; also maybe I should use *
;; []{}
;; any weird unicode that I have in convenient places


;; the following isn't working...
;; these have to be customized.  I'm leaving these comments here too, as an important note.
;(setq-default evil-overriding-maps nil
;              evil-intercept-maps nil)

;;; First, blow up maps so they don't map things like t/T and
;;; insert mode stuff
(setcdr evil-insert-state-map nil)
(setcdr evil-replace-state-map nil)
(set-keymap-parent evil-replace-state-map evil-insert-state-map)
(setcdr evil-normal-state-map nil)
(setcdr evil-motion-state-map nil)

(evil-define-state pager
  "Pager State"
  :tag "<P>"
  :message "-- PAGER STATE --"
  :enable (motion)
  :suppress-keymap t
  )


(defmacro myhydradef (hydra-name &rest hydra-keys)
  `(defhydra ,hydra-name (:exit t :foreign-keys warn)
     ,@hydra-keys))


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
;(nkmap "=" 'evil-indent)
(nkmap "<" 'evil-shift-left)
(nkmap ">" 'evil-shift-right)
(nkmap "ZZ" 'evil-save-modified-and-close)
(nkmap "ZQ" 'evil-quit)
(nkmap (kbd "DEL") 'rmo/evil-backward-char)
(nkmap (kbd "<deletechar>") 'rmo/evil-forward-char)
(nkmap [escape] 'evil-force-normal-state)
(nkmap [remap cua-paste-pop] 'evil-paste-pop)
(nkmap [remap yank-pop] 'evil-paste-pop)

(nkmap "=" 'indent-region)
(nkmap "≠" 'wgh/racket-indent-region)
(nkmap "\M-\C-\\" (lambda () (interactive) (message "use =")))
(ikmap "\C-s" 'backward-kill-word)
(ikmap (kbd "M-DEL") (lambda () (interactive) (message "use C-s")))

;; undo
(nkmap "u" 'undo)
(nkmap "\C-r" 'evil-redo)

;;; Motion state

;; "0" is a special command when called first
;(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(mkmap "0" 'evil-beginning-of-line)
(mkmap "1" 'digit-argument)
(mkmap "2" 'digit-argument)
(mkmap "3" 'digit-argument)
(mkmap "4" 'digit-argument)
(mkmap "5" 'digit-argument)
(mkmap "6" 'digit-argument)
(mkmap "7" 'digit-argument)
(mkmap "8" 'digit-argument)
(mkmap "9" 'digit-argument)
(mkmap "b" 'rmo/evil-backward-word-begin)
(mkmap "B" 'rmo/evil-backward-WORD-begin)
(mkmap "E" 'rmo/evil-forward-WORD-end)
(mkmap "f" 'rmo/evil-find-char)
(mkmap "F" 'rmo/evil-find-char-backward)
(mkmap "G" 'evil-goto-line)
(mkmap "h" (lambda () (interactive) (message "use oc")))
(mkmap "H" 'evil-window-top)
(mkmap "j" 'rmo-c/evil-next-line)
(mkmap "k" 'rmo-c/evil-previous-line)
(mkmap "l" (lambda () (interactive) (message "use ec")))
(mkmap "K" 'evil-lookup)
(mkmap "L" 'evil-window-bottom)
(mkmap "M" 'evil-window-middle)
(mkmap "n" 'evil-search-next)
(mkmap "N" 'evil-search-previous)
(mkmap "w" 'rmo/evil-forward-word-begin)
(mkmap "W" 'rmo/evil-forward-WORD-begin)
(mkmap "ge" 'rmo/evil-backward-word-end)
(mkmap "gE" 'rmo/evil-backward-WORD-end)
(mkmap "gg" 'evil-goto-first-line)
(mkmap "gj" 'evil-next-visual-line)
(mkmap "gk" 'evil-previous-visual-line)
(mkmap "g0" 'evil-beginning-of-visual-line)
(mkmap "g_" 'evil-last-non-blank)
(mkmap "g^" 'evil-first-non-blank-of-visual-line)
(mkmap "gm" 'evil-middle-of-visual-line)
(mkmap "g$" 'evil-end-of-visual-line)
(mkmap "g\C-]" 'find-tag)
(mkmap "{" 'rmo/evil-backward-paragraph)
(mkmap "}" 'rmo/evil-forward-paragraph)
(mkmap "#" 'rmo/evil-search-word-backward)
(mkmap "g#" 'rmo/evil-search-unbounded-word-backward)
(mkmap "$" 'evil-end-of-line)
(mkmap "%" 'evil-jump-item)
(mkmap "`" 'evil-goto-mark)
(mkmap "'" 'evil-goto-mark-line)
(mkmap "(" 'quick-a-block)
(mkmap ")" 'quick-in-block)
(mkmap "]]" 'rmo/evil-forward-section-begin)
(mkmap "][" 'rmo/evil-forward-section-end)
(mkmap "[[" 'rmo/evil-backward-section-begin)
(mkmap "[]" 'rmo/evil-backward-section-end)
(mkmap "[(" 'rmo/evil-previous-open-paren)
(mkmap "])" 'rmo/evil-next-close-paren)
(mkmap "[{" 'rmo/evil-previous-open-brace)
(mkmap "]}" 'rmo/evil-next-close-brace)
(mkmap "*" 'rmo/evil-search-word-forward)
(mkmap "g*" 'rmo/evil-search-unbounded-word-forward)
(mkmap "," 'evil-repeat-find-char-reverse)
(mkmap "/" 'rmo/evil-search-forward)
(mkmap ";" 'er/expand-region)
(mkmap "?" 'rmo/evil-search-backward)
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
(mkmap [left] 'rmo/evil-backward-char)
(mkmap [right] 'rmo/evil-forward-char)
(mkmap [up] 'rmo/evil-previous-line)
(mkmap [down] 'rmo/evil-next-line)

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
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-inner-text-objects-map "c" 'evil-cp-inner-comment)
(define-key evil-outer-text-objects-map "c" 'evil-cp-a-comment)
(define-key evil-inner-text-objects-map "d" 'evil-cp-inner-defun)
(define-key evil-outer-text-objects-map "d" 'evil-cp-a-defun)
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)

;; Normal state switch!
(key-chord-define evil-insert-state-map (kbd "kj") 'evil-normal-state)
(key-chord-define evil-replace-state-map (kbd "kj") 'evil-normal-state)
(ikmap (kbd "C-c") 'evil-normal-state)

; Keys unbound and reserved for future use - bind to nop so they don't input
(mkmap (kbd "RET") 'ignore)
(mkmap "S" 'ignore)
(mkmap "T" 'ignore)

;; make v a no-op in visual-char-mode, so I don't accidentally type vi)vi) and exit
;; visual state
(vkmap "v" (lambda (&optional mark point type message)
             (interactive)
             (if (and (equal evil-state 'visual)
                      (equal evil-visual-selection 'char))
                 (ignore)
               (evil-visual-char evil-visual-mark evil-visual-point type message))))

;; I'm not sure a better place to put this...
(nkmap (kbd "TAB") 'sp-indent-defun)

; g map
(mkmap "gr" 'evil-ace-jump-word-mode)
(mkmap "gc" 'evil-ace-jump-char-mode)
(mkmap "gf" 'evil-ace-jump-line-mode)
(mkmap "gdd" 'xref-find-definitions)
(mkmap "gdD" 'xref-find-definitions-other-window)
(mkmap "gdr" 'xref-find-references)
(mkmap "gdi" 'lsp-describe-thing-at-point)
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
(mkmap "ee" 'rmo/evil-forward-word-end)
(mkmap "oe" 'rmo/evil-backward-word-end)
(mkmap "eE" 'rmo/evil-forward-WORD-end)
(mkmap "oE" 'rmo/evil-backward-WORD-end)
(nkmap "eo" 'evil-open-below)
(vkmap "o" nil)
(vkmap "oo" 'exchange-point-and-mark)
(vkmap "eo" 'exchange-point-and-mark)
(nkmap "oo" 'evil-open-above)
(mkmap "ec" 'rmo/evil-forward-char)
(mkmap "oc" 'rmo/evil-backward-char)
;(mkmap "ed" 'rmo/evil-next-close-paren)
;(mkmap "od" 'rmo/evil-previous-open-paren)
;(mkmap "ed" 'rmo/evil-next-close-brace)
;(mkmap "od" 'rmo/evil-previous-open-brace)
(mkmap "ed" 'rmo/on-parens-down-sexp-end)
(mkmap "od" 'rmo/on-parens-down-sexp)
(mkmap "eg" 'rmo/on-parens-up-sexp-end)
(mkmap "og" 'rmo/on-parens-up-sexp)
(mkmap "eh" 'rmo/on-parens-forward-sexp)
(mkmap "oh" 'rmo/on-parens-backward-sexp)
(mkmap "em" 'rmo/on-parens-forward-sexp-end)
(mkmap "om" 'rmo/on-parens-backward-sexp-end)
(mkmap "en" 'rmo/on-parens-forward-sexp-in-supersexp)
(mkmap "on" 'rmo/on-parens-backward-sexp-in-supersexp)
(mkmap "ea" 'rmo/evil-forward-arg)
(mkmap "oa" 'rmo/evil-backward-arg)
(mkmap "ew" 'rmo/evil-forward-little-word-begin)
(mkmap "ow" 'rmo/evil-backward-little-word-begin)
(mkmap "eW" 'rmo/evil-forward-little-word-end)
(mkmap "oW" 'rmo/evil-backward-little-word-end)
(mkmap "es" 'rmo/evil-forward-sentence-begin)
(mkmap "os" 'rmo/evil-backward-sentence-begin)
(mkmap "ep" 'rmo/evil-forward-paragraph)
(mkmap "op" 'rmo/evil-backward-paragraph)
(mkmap "eS" 'rmo/evil-forward-section-begin)
(mkmap "oS" 'rmo/evil-backward-section-begin)
(mkmap "eP" 'rmo/evil-forward-section-end)
(mkmap "oP" 'rmo/evil-backward-section-end)
(mkmap "et" 'rmo/evil-find-char-to)
(mkmap "ot" 'rmo/evil-find-char-to-backward)
(mkmap "ef" 'rmo/evil-find-char)
(mkmap "of" 'rmo/evil-find-char-backward)
(mkmap "ol" 'evil-goto-column)
(mkmap "o l" 'evil-first-non-blank)
(mkmap "el" 'evil-end-of-line)
(mkmap "oL" 'evil-beginning-of-visual-line)
(mkmap "eL" 'evil-end-of-visual-line)
(mkmap "eb" 'evil-next-line-first-non-blank)
(mkmap "ob" 'evil-previous-line-first-non-blank)
(mkmap "oB" 'evil-first-non-blank)
(mkmap "eB" 'evil-next-line-1-first-non-blank)
(mkmap "eO" 'rmo/forward-symbol)
(mkmap "oO" 'rmo/backward-symbol)
(mkmap "ei" 'rmo/wgh/next-line-same-indent-in-block)
(mkmap "oi" 'rmo/wgh/previous-line-same-indent-in-block)
(mkmap "eI" 'wgh/indent-tree-down-to-first-child)
(mkmap "oI" 'wgh/indent-tree-up-to-parent)
(mkmap "ej" 'rmo/evil-jump-forward)
(mkmap "oj" 'rmo/evil-jump-backward)
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


;; Mouse keys...
;; mouse-1 is left-click.  There is also drag-mouse-1, double-mouse-1, triple-mouse-1
;; drag-mouse-1 is left-click drag.
;; mouse-3 is right-click
;; mouse-4 is scroll-wheel up
;; mouse-5 is scroll-wheel down
;; You can also do combos with special regions where the mouse is, eg. [mode-line mouse-1] to bind to something different when clicking in the mode-line.
(mkmap [mouse-4] 'mwheel-scroll)
(mkmap [mouse-5] 'mwheel-scroll)
;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))


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
(mkmap "tip" 'ffap/no-confirm)
(mkmap "tif" 'ido-ffap-no)
(mkmap " tifd" 'ido-find-file-from-pwd)
(mkmap " tiff" 'ffap/no-confirm)
(mkmap "tiw" 'next-buffer-no-star)
(mkmap "tib" 'prev-buffer-no-star)
(mkmap " tiwd" 'next-dirty-buffer-no-star)
(mkmap " tibd" 'prev-dirty-buffer-no-star)

(mkmap "th" 'evil-window-map/body)
(autoload 'projectile-command-map "projectile-conf" "" t 'keymap)
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
    ("l" lsp-lens-mode "lsp-lens") ;; lsp-lens is the thing that shows eg. haskell imports in an overlay
    ("m" (lambda () (interactive)
           (menu-bar-mode 1) (menu-bar-open))
     "menu-open")
    ("I" indent-guide-mode "indent-guide")
    ("x" wgh/racket-xp-pre-redisplay-toggle "racket-xp-hl")
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
(vkmap "(" 'quick-a-block)
(vkmap ")" 'quick-in-block)
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
(ikmap (kbd "M-c") 'helm-M-x)
(mkmap (kbd "M-c") 'helm-M-x)
(global-set-key (kbd "M-c") 'helm-M-x)
(mkmap "sx" 'eval-expression)

(vkmap (kbd "C-s") 'yas-insert-with-region)

;; command modes and macros
(mkmap "-" 'helm-M-x)
;(mkmap "|" 'execute-extended-command)
(mkmap "|" 'eval-expression)
(mkmap "_" 'eval-expression)
(mkmap "Q" 'call-last-kbd-macro)
;; Movement
(nkmap "m" nil) ;;;;;;;;;; m will be my prefix for mode-specific bindings
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

(pkmap " " 'rmo/pscroll-down-half)
(pkmap "j" 'rmo/pscroll-down-half)
(pkmap "k" 'rmo/pscroll-up-full)
(pkmap "J" 'rmo/pscroll-down-line)
(pkmap "K" 'rmo/pscroll-up-line)
(pkmap "sj" 'rmo/pscroll-down-full)
(pkmap "sk" 'rmo/pscroll-up-full)
(pkmap "e" 'evil-normal-state)
;; if I start in pager mode, this gets remapped to quit
(pkmap "q" 'evil-normal-state)
(mkmap "to" 'evil-pager-state)


; space map
(mkmap "sj" 'rmo/pscroll-down-half)
(mkmap "sk" 'rmo/pscroll-up-half)
(mkmap "sf" 'fold-toggle-wgh)
(mkmap "sF" 'fold-toggle-wgh-all)
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
(ikmap "\M-w" 'evil-window-map)

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
(ikmap (kbd "C-SPC TAB") 'completer-map/body)
(ikmap (kbd "C-@ TAB") 'completer-map/body)
(ikmap (kbd "TAB") 'company-complete-common-wgh)
;; put indentation on something...
;(define-key 'completer-map (kbd "TAB") 'indent-for-tab-command)
(ikmap (kbd "<backtab>") 'indent-for-tab-command)

(global-set-key (kbd "C-\\") 'evil-execute-in-normal-state)

;; Default mode settings
(setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes (cons 'racket-repl-mode evil-insert-state-modes))


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

(define-key isearch-mode-map "\C-g" 'isearch-abort-abort-gosh-darn-it)

