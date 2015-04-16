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
(defun kmap-m (keys func)
  (define-key evil-motion-state-map keys func))
(defun kmap-n (keys func)
  (define-key evil-normal-state-map keys func))
(defun kmap-i (keys func)
  (define-key evil-insert-state-map keys func))
(defun kmap-v (keys func)
  (define-key evil-visual-state-map keys func))
(defun kmap-w (keys func)
  (define-key evil-window-map keys func))

;; for temporary on-the-fly bindings
(define-prefix-command 'temp-key-map)
(defun kmap-t (keys func)
  (define-key temp-key-map keys func))


;;; DEFAULT BINDINGS SECTION
;;; these are mostly taken from the default evil config, and just put back in
;;; after blowing everything up

;;; Normal state
(define-key evil-normal-state-map "a" 'evil-append)
(define-key evil-normal-state-map "A" 'evil-append-line)
(define-key evil-normal-state-map "c" 'evil-change)
(define-key evil-normal-state-map "C" 'evil-change-line)
(define-key evil-normal-state-map "d" 'evil-delete)
(define-key evil-normal-state-map "D" 'evil-delete-line)
(define-key evil-normal-state-map "i" 'evil-insert)
(define-key evil-normal-state-map "I" 'evil-insert-line)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "p" 'evil-paste-after)
(define-key evil-normal-state-map "P" 'evil-paste-before)
(define-key evil-normal-state-map "q" 'evil-record-macro)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)
(define-key evil-normal-state-map "x" 'evil-delete-char)
(define-key evil-normal-state-map "X" 'evil-delete-backward-char)
(define-key evil-normal-state-map "y" 'evil-yank)
(define-key evil-normal-state-map "Y" 'evil-yank-line)
(define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute)
(define-key evil-normal-state-map "gq" 'evil-fill-and-move)
(define-key evil-normal-state-map "gw" 'evil-fill)
(define-key evil-normal-state-map "zo" 'evil-open-fold)
(define-key evil-normal-state-map "zc" 'evil-close-fold)
(define-key evil-normal-state-map "za" 'evil-toggle-fold)
(define-key evil-normal-state-map "zr" 'evil-open-folds)
(define-key evil-normal-state-map "zm" 'evil-close-folds)
(define-key evil-normal-state-map "z=" 'ispell-word)
(define-key evil-normal-state-map "\C-n" 'evil-paste-pop-next)
(define-key evil-normal-state-map "\C-p" 'evil-paste-pop)
(define-key evil-normal-state-map "\C-t" 'pop-tag-mark)
(define-key evil-normal-state-map (kbd "C-.") 'evil-repeat-pop)
(define-key evil-normal-state-map (kbd "M-.") 'evil-repeat-pop-next)
(define-key evil-normal-state-map "." 'evil-repeat)
(define-key evil-normal-state-map "@" 'evil-execute-macro)
(define-key evil-normal-state-map "\"" 'evil-use-register)
(define-key evil-normal-state-map "~" 'evil-invert-char)
(define-key evil-normal-state-map "=" 'evil-indent)
(define-key evil-normal-state-map "<" 'evil-shift-left)
(define-key evil-normal-state-map ">" 'evil-shift-right)
(define-key evil-normal-state-map "ZZ" 'evil-save-modified-and-close)
(define-key evil-normal-state-map "ZQ" 'evil-quit)
(define-key evil-normal-state-map (kbd "DEL") 'evil-backward-char)
(define-key evil-normal-state-map [escape] 'evil-force-normal-state)
(define-key evil-normal-state-map [remap cua-paste-pop] 'evil-paste-pop)
(define-key evil-normal-state-map [remap yank-pop] 'evil-paste-pop)

;; undo
(define-key evil-normal-state-map "u" 'undo)
(define-key evil-normal-state-map "\C-r" 'redo)

;;; Motion state

;; "0" is a special command when called first
(evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)
(define-key evil-motion-state-map "1" 'digit-argument)
(define-key evil-motion-state-map "2" 'digit-argument)
(define-key evil-motion-state-map "3" 'digit-argument)
(define-key evil-motion-state-map "4" 'digit-argument)
(define-key evil-motion-state-map "5" 'digit-argument)
(define-key evil-motion-state-map "6" 'digit-argument)
(define-key evil-motion-state-map "7" 'digit-argument)
(define-key evil-motion-state-map "8" 'digit-argument)
(define-key evil-motion-state-map "9" 'digit-argument)
(define-key evil-motion-state-map "b" 'evil-backward-word-begin)
(define-key evil-motion-state-map "B" 'evil-backward-WORD-begin)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
(define-key evil-motion-state-map "K" 'evil-lookup)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "n" 'evil-search-next)
(define-key evil-motion-state-map "N" 'evil-search-previous)
(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "g\C-]" 'find-tag)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "%" 'evil-jump-item)
(define-key evil-motion-state-map "`" 'evil-goto-mark)
(define-key evil-motion-state-map "'" 'evil-goto-mark-line)
(define-key evil-motion-state-map "(" 'evil-backward-sentence)
(define-key evil-motion-state-map ")" 'evil-forward-sentence)
(define-key evil-motion-state-map "]]" 'evil-forward-section-begin)
(define-key evil-motion-state-map "][" 'evil-forward-section-end)
(define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
(define-key evil-motion-state-map "[]" 'evil-backward-section-end)
(define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
(define-key evil-motion-state-map "])" 'evil-next-close-paren)
(define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
(define-key evil-motion-state-map "]}" 'evil-next-close-brace)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
(define-key evil-motion-state-map "," 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "/" 'evil-search-forward)
(define-key evil-motion-state-map ";" 'evil-repeat-find-char)
(define-key evil-motion-state-map "?" 'evil-search-backward)
(define-key evil-motion-state-map "|" 'evil-goto-column)
(define-key evil-motion-state-map "^" 'evil-first-non-blank)
(define-key evil-motion-state-map "+" 'evil-next-line-first-non-blank)
(define-key evil-motion-state-map "_" 'evil-next-line-1-first-non-blank)
(define-key evil-motion-state-map "-" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "\C-w" 'evil-window-map)
(define-key evil-motion-state-map "\C-]" 'evil-jump-to-tag)
(define-key evil-motion-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "C-d") 'evil-scroll-down)
(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)
(define-key evil-motion-state-map "\\" 'evil-execute-in-emacs-state)
(define-key evil-motion-state-map "z^" 'evil-scroll-top-line-to-bottom)
(define-key evil-motion-state-map "z+" 'evil-scroll-bottom-line-to-top)
(define-key evil-motion-state-map "zt" 'evil-scroll-line-to-top)
;; TODO: z RET has an advanced form taking an count before the RET
;; but this requires again a special state with a single command
;; bound to RET
(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))
(define-key evil-motion-state-map "zz" 'evil-scroll-line-to-center)
(define-key evil-motion-state-map "z." "zz^")
(define-key evil-motion-state-map "zb" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "z-" "zb^")
(define-key evil-motion-state-map "v" 'evil-visual-char)
(define-key evil-motion-state-map "V" 'evil-visual-line)
(define-key evil-motion-state-map "\C-v" 'evil-visual-block)
(define-key evil-motion-state-map "gv" 'evil-visual-restore)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)

(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Minibuffer

;(define-key minibuffer-local-map "\C-p" 'evil-complete-next)
;(define-key minibuffer-local-map "\C-n" 'evil-complete-previous)
;(define-key minibuffer-local-map "\C-x\C-p" 'evil-complete-next-line)
;(define-key minibuffer-local-map "\C-x\C-n" 'evil-complete-previous-line)

;; Ex
(define-key evil-motion-state-map ":" 'evil-ex)
(define-key evil-motion-state-map "!" 'evil-shell-command)
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
(define-key evil-motion-state-map (kbd "RET") 'ignore)
(define-key evil-motion-state-map "S" 'ignore)
(define-key evil-motion-state-map "T" 'ignore)
(define-key evil-motion-state-map "f" 'ignore)
(define-key evil-motion-state-map "F" 'ignore)

; g map
(define-key evil-motion-state-map "gr" 'evil-ace-jump-word-mode)
(define-key evil-motion-state-map "gc" 'evil-ace-jump-char-mode)
(define-key evil-motion-state-map "gf" 'evil-ace-jump-line-mode)
(define-key evil-motion-state-map "gdd" 'evil-goto-definition)
(define-key evil-motion-state-map "gdp" 'pop-tag-mark)
(define-key evil-normal-state-map " yc" 'xcopy)
(define-key evil-normal-state-map " Pc" 'xpaste)
(define-key evil-normal-state-map " pc" 'xpaste-after-char)
(define-key evil-motion-state-map "gl" 'evil-scroll-right)
(define-key evil-motion-state-map "gh" 'evil-scroll-left)
(define-key evil-motion-state-map "gn" 'evil-next-match)
(define-key evil-motion-state-map "gN" 'evil-previous-match)

(define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
(define-key evil-normal-state-map "gi" 'evil-insert-resume) ; insert mode at ins. mode cursor point
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)
(define-key evil-normal-state-map "g~" 'evil-invert-case)
(define-key evil-normal-state-map "g;" 'goto-last-change)
(define-key evil-normal-state-map "g," 'goto-last-change-reverse)

; o and e maps - o is left/back, e is right/forward
(define-key evil-motion-state-map "ee" 'evil-forward-word-end)
(define-key evil-motion-state-map "oe" 'evil-backward-word-end)
(define-key evil-motion-state-map "eE" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "oE" 'evil-backward-WORD-end)
(define-key evil-normal-state-map "eo" 'evil-open-below)
(define-key evil-visual-state-map "o" nil)
(define-key evil-visual-state-map "oo" 'exchange-point-and-mark)
(define-key evil-visual-state-map "eo" 'exchange-point-and-mark)
(define-key evil-normal-state-map "oo" 'evil-open-above)
(define-key evil-motion-state-map "ea" 'evil-forward-arg)
(define-key evil-motion-state-map "oa" 'evil-backward-arg)
(define-key evil-motion-state-map "ew" 'evil-forward-little-word-begin)
(define-key evil-motion-state-map "ow" 'evil-backward-little-word-begin)
(define-key evil-motion-state-map "eW" 'evil-forward-little-word-end)
(define-key evil-motion-state-map "oW" 'evil-backward-little-word-end)
(define-key evil-motion-state-map "es" 'evil-forward-sentence)
(define-key evil-motion-state-map "os" 'evil-backward-sentence)
(define-key evil-motion-state-map "ep" 'evil-forward-paragraph)
(define-key evil-motion-state-map "op" 'evil-backward-paragraph)
(define-key evil-motion-state-map "ec" 'evil-forward-section-begin)
(define-key evil-motion-state-map "oc" 'evil-backward-section-begin)
(define-key evil-motion-state-map "eC" 'evil-forward-section-end)
(define-key evil-motion-state-map "oC" 'evil-backward-section-end)
(define-key evil-motion-state-map "eh" 'evil-search-forward)
(define-key evil-motion-state-map "oh" 'evil-search-backward)
(define-key evil-motion-state-map "et" 'evil-find-char-to)
(define-key evil-motion-state-map "ot" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "ef" 'evil-find-char)
(define-key evil-motion-state-map "of" 'evil-find-char-backward)
(define-key evil-motion-state-map "em" 'evil-repeat-find-char)
(define-key evil-motion-state-map "om" 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "en" 'evil-search-next)
(define-key evil-motion-state-map "on" 'evil-search-previous)
(define-key evil-motion-state-map "en" 'evil-search-next)
(define-key evil-motion-state-map "on" 'evil-search-previous)
(define-key evil-motion-state-map "ol" 'evil-goto-column)
(define-key evil-motion-state-map "o l" 'evil-first-non-blank)
(define-key evil-motion-state-map "el" 'evil-end-of-line)
(define-key evil-motion-state-map "oL" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "eL" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "eb" 'evil-next-line-first-non-blank)
(define-key evil-motion-state-map "ob" 'evil-previous-line-first-non-blank)
(define-key evil-motion-state-map "oB" 'evil-first-non-blank)
(define-key evil-motion-state-map "eB" 'evil-next-line-1-first-non-blank)
(define-key evil-motion-state-map "eO" 'forward-symbol)
(define-key evil-motion-state-map "oO" 'backward-symbol)
(define-key evil-motion-state-map "ej" 'evil-jump-forward)
(define-key evil-motion-state-map "oj" 'evil-jump-backward)

(define-key evil-motion-state-map "f" 'evil-repeat-find-char)
(define-key evil-motion-state-map "F" 'evil-repeat-find-char-reverse)

; t map
(define-key evil-motion-state-map "tt" 'temp-key-map)

(define-key evil-motion-state-map "tia" 'switch-to-buffer)
(define-key evil-motion-state-map "tic" 'kill-buffer-or-quit-emacs)
(define-key evil-motion-state-map " tica" 'evil-quit-all)
(define-key evil-motion-state-map "tis" 'save-buffer)
(define-key evil-motion-state-map " tisa" 'evil-write-all)
(define-key evil-motion-state-map "tie" 'save-and-kill-buffer-and-maybe-quit-emacs)
(define-key evil-motion-state-map " tiea" 'evil-save-and-quit)
(define-key evil-motion-state-map "tif" 'ido-ffap-no)
(define-key evil-motion-state-map " tifd" 'ido-find-file-from-pwd)
(define-key evil-motion-state-map " tiff" 'ido-ffap-yes)
(define-key evil-motion-state-map "tiw" 'next-buffer-no-star)
(define-key evil-motion-state-map "tib" 'prev-buffer-no-star)
(define-key evil-motion-state-map " tiwd" 'next-dirty-buffer-no-star)
(define-key evil-motion-state-map " tibd" 'prev-dirty-buffer-no-star)

(define-key evil-motion-state-map "th" 'evil-window-map/body)
(define-key evil-motion-state-map "tp" 'projectile-command-map)
(define-key evil-motion-state-map "tr" 'evil-use-register)
;; "ts" will stand for "toggle setting"
(define-key evil-motion-state-map "ts"
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

(define-key evil-motion-state-map "tl"
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
;(define-key evil-normal-state-map "ss" 'evil-substitute)
;(define-key evil-normal-state-map "sS" 'evil-change-whole-line)
(evil-define-key 'visual evil-surround-mode-map "s" nil)
(evil-define-key 'visual evil-surround-mode-map "S" 'ignore)
(define-key evil-visual-state-map "ss" 'evil-surround-region)
(define-key evil-visual-state-map "sS" 'evil-Surround-region)
(define-key evil-visual-state-map "sh" 'shell-command-on-region)
(define-key evil-normal-state-map "sh" 'shell-command)
(define-key evil-motion-state-map "sa" 'evil-ex)
(define-key evil-normal-state-map "s)" 'eval-last-sexp)
(define-key evil-visual-state-map "s)" 'eval-region)
(define-key evil-visual-state-map "s/" (kbd ":s/ ")) ; TODO - fix this...
(define-key evil-normal-state-map "sm" 'evil-set-marker)
(define-key evil-normal-state-map "sM" 'bookmark-set)
(define-key evil-normal-state-map "sg" 'evil-goto-mark)
(define-key evil-normal-state-map "sG" 'bookmark-jump)
(define-key evil-insert-state-map (kbd "M-c") 'smex)
(define-key evil-motion-state-map (kbd "M-c") 'smex)
(define-key evil-motion-state-map "sx" 'eval-expression)

(define-key evil-visual-state-map (kbd "C-s") 'yas-insert-with-region)

;; command modes and macros
(define-key evil-motion-state-map "-" 'smex)
;(define-key evil-motion-state-map "|" 'execute-extended-command)
(define-key evil-motion-state-map "|" 'eval-expression)
(define-key evil-motion-state-map "_" 'eval-expression)
(define-key evil-motion-state-map "Q" 'call-last-kbd-macro)
;; Movement
(define-key evil-normal-state-map "mm" 'evil-set-marker) ;;;;;;;;;; m will be my prefix for mode-specific bindings
;; everything in motion state is pulled into normal state
(define-key evil-motion-state-map "+" 'evil-repeat-find-char)
(define-key evil-motion-state-map "~" 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map "J" 'evil-window-bottom)
(define-key evil-motion-state-map "K" 'evil-window-top)
(define-key evil-motion-state-map "{" 'backward-sexp)
(define-key evil-motion-state-map "}" 'forward-sexp)
(define-key evil-motion-state-map "[" 'backward-list)
(define-key evil-motion-state-map "]" 'forward-list)
(define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)
(define-key evil-insert-state-map (kbd "C-z") 'suspend-frame)

; space map
(define-key evil-motion-state-map "sj" 'scroll-up)
(define-key evil-motion-state-map "sk" 'scroll-down)
(define-key evil-motion-state-map "sf" 'yafolding-toggle-element)
(define-key evil-motion-state-map " sfa" 'yafolding-toggle-all)
(define-key evil-motion-state-map " /"
  (myhydradef search-hydra
              ("s" helm-swoop "swoop")
              ("a" helm-multi-swoop-all "multi-swoop all")
              ("m" helm-multi-swoop "multi-swoop")
  ))
(define-key evil-motion-state-map " -h" 'helm-M-x)
(define-key evil-motion-state-map " &g" 'evil-ex-repeat-global-substitute)
(define-key evil-motion-state-map " va" 'mark-whole-buffer)

;; Joining
;; TODO -- make a better mapping for this.  I should make my prefixes be mnemonic or something...
;;         for instance, g<key> is mostly navigation... t... mosty has window stuff in th... space is mostly one handed
;;         navigation aside from this one
(define-key evil-normal-state-map " sjj" 'evil-join)
(define-key evil-normal-state-map " sjw" 'evil-join-whitespace)


;; input mode
(define-key evil-insert-state-map (kbd "DEL") 'delete-backward-char) ; remap away from the evil-version backspace
(define-key evil-insert-state-map "\C-v" #'quoted-insert) ; more vim-like
(define-key evil-insert-state-map "\M-r" 'evil-paste-from-register)

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
(define-key evil-insert-state-map "\M-h" 'completer-map/body)
(define-key evil-insert-state-map (kbd "C-SPC TAB") completer-map/body)
(define-key evil-insert-state-map (kbd "C-@ TAB") completer-map/body)
(define-key evil-insert-state-map (kbd "TAB") 'company-complete-common-wgh)
;; put indentation on something...
(define-key completer-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "<backtab>") 'indent-for-tab-command)

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

