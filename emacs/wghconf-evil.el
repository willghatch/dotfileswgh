;;; Evil package configuration

(require 'evil)
(evil-mode 1)


(evil-define-command wevil-quit ()
  "Close buffer, primarily"
  (kill-buffer))
(evil-define-command wevil-save-and-quit ()
  "Close buffer, primarily"
  (save-buffer)
  (kill-buffer))

;;;; Ace Jump installed here as well...
(load-library "wghconf-ace-jump-mode")
(load-library "wghconf-xclip")
;;;; and key-chord...
(require 'key-chord)
(key-chord-mode 1)

;; the following isn't working...
;(setq-default evil-overriding-maps nil
;              evil-intercept-maps nil)

(require 'evil-args)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-little-word)
(require 'evil-textobj-between)

(defun backward-symbol (n)
  "this doesn't work right..."
  (interactive "p")
  (forward-word (- n)))


;;; First, blow up maps so they don't map things like t/T and
;;; insert mode stuff

(setcdr evil-insert-state-map nil)
(setcdr evil-normal-state-map nil)
(setcdr evil-motion-state-map nil)
(setcdr evil-ex-commands nil)

;; TODO -- replace this map-killing with
;;(define-key <keymap> <key> nil)
;; for every key I don't want.




;;;;;;; Here is basically a copy of evil-maps.el from the package
;;;;;;; I'm only commenting things out here, not making serious changes.


;;; Normal state

(define-key evil-normal-state-map "a" 'evil-append)
(define-key evil-normal-state-map "A" 'evil-append-line)
(define-key evil-normal-state-map "c" 'evil-change)
(define-key evil-normal-state-map "C" 'evil-change-line)
(define-key evil-normal-state-map "d" 'evil-delete)
(define-key evil-normal-state-map "D" 'evil-delete-line)
(define-key evil-normal-state-map "i" 'evil-insert)
(define-key evil-normal-state-map "I" 'evil-insert-line)
;(define-key evil-normal-state-map "J" 'evil-join)
;(define-key evil-normal-state-map "m" 'evil-set-marker)
(define-key evil-normal-state-map "o" 'evil-open-below)
(define-key evil-normal-state-map "O" 'evil-open-above)
(define-key evil-normal-state-map "p" 'evil-paste-after)
(define-key evil-normal-state-map "P" 'evil-paste-before)
(define-key evil-normal-state-map "q" 'evil-record-macro)
(define-key evil-normal-state-map "r" 'evil-replace)
(define-key evil-normal-state-map "R" 'evil-replace-state)
;(define-key evil-normal-state-map "s" 'evil-substitute)
;(define-key evil-normal-state-map "S" 'evil-change-whole-line)
(define-key evil-normal-state-map "x" 'evil-delete-char)
(define-key evil-normal-state-map "X" 'evil-delete-backward-char)
(define-key evil-normal-state-map "y" 'evil-yank)
(define-key evil-normal-state-map "Y" 'evil-yank-line)
(define-key evil-normal-state-map "&" 'evil-ex-repeat-substitute)
;(define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
;(define-key evil-normal-state-map "g8" 'what-cursor-position)
;(define-key evil-normal-state-map "ga" 'what-cursor-position)
;(define-key evil-normal-state-map "gi" 'evil-insert-resume)
;(define-key evil-normal-state-map "gJ" 'evil-join-whitespace)
;(define-key evil-normal-state-map "gq" 'evil-fill-and-move)
;(define-key evil-normal-state-map "gw" 'evil-fill)
;(define-key evil-normal-state-map "gu" 'evil-downcase)
;(define-key evil-normal-state-map "gU" 'evil-upcase)
;(define-key evil-normal-state-map "gf" 'find-file-at-point)
;(define-key evil-normal-state-map "gF" 'evil-find-file-at-point-with-line)
;(define-key evil-normal-state-map "g?" 'evil-rot13)
;(define-key evil-normal-state-map "g~" 'evil-invert-case)
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

;; go to last change
;(define-key evil-normal-state-map "g;" 'goto-last-change)
;(define-key evil-normal-state-map "g," 'goto-last-change-reverse)

;; undo
(define-key evil-normal-state-map "u" 'undo)
(define-key evil-normal-state-map "\C-r" 'redo)

;; window commands
(define-prefix-command 'evil-window-map)
(define-key evil-window-map "b" 'evil-window-bottom-right)
;(define-key evil-window-map "c" 'evil-window-delete)
(define-key evil-window-map "h" 'evil-window-left)
(define-key evil-window-map "H" 'evil-window-move-far-left)
(define-key evil-window-map "j" 'evil-window-down)
(define-key evil-window-map "J" 'evil-window-move-very-bottom)
(define-key evil-window-map "k" 'evil-window-up)
(define-key evil-window-map "K" 'evil-window-move-very-top)
(define-key evil-window-map "l" 'evil-window-right)
(define-key evil-window-map "L" 'evil-window-move-far-right)
;(define-key evil-window-map "n" 'evil-window-new)
;(define-key evil-window-map "o" 'delete-other-windows)
(define-key evil-window-map "p" 'evil-window-mru)
(define-key evil-window-map "r" 'evil-window-rotate-downwards)
(define-key evil-window-map "R" 'evil-window-rotate-upwards)
(define-key evil-window-map "s" 'evil-window-split)
(define-key evil-window-map "S" 'evil-window-split)
(define-key evil-window-map "t" 'evil-window-top-left)
(define-key evil-window-map "v" 'evil-window-vsplit)
;(define-key evil-window-map "w" 'evil-window-next)
;(define-key evil-window-map "W" 'evil-window-prev)
;(define-key evil-window-map "+" 'evil-window-increase-height)
;(define-key evil-window-map "-" 'evil-window-decrease-height)
;(define-key evil-window-map "_" 'evil-window-set-height)
;(define-key evil-window-map "<" 'evil-window-decrease-width)
;(define-key evil-window-map ">" 'evil-window-increase-width)
(define-key evil-window-map "=" 'balance-windows)
;(define-key evil-window-map "|" 'evil-window-set-width)
(define-key evil-window-map "\C-b" 'evil-window-bottom-right)
(define-key evil-window-map "\C-c" 'evil-window-delete)
(define-key evil-window-map "\C-H" 'evil-window-move-far-left)
(define-key evil-window-map "\C-h" 'evil-window-left)
(define-key evil-window-map "\C-J" 'evil-window-move-very-bottom)
(define-key evil-window-map "\C-j" 'evil-window-down)
(define-key evil-window-map "\C-K" 'evil-window-move-very-top)
(define-key evil-window-map "\C-k" 'evil-window-up)
(define-key evil-window-map "\C-L" 'evil-window-move-far-right)
(define-key evil-window-map "\C-l" 'evil-window-right)
(define-key evil-window-map "\C-n" 'evil-window-new)
(define-key evil-window-map "\C-o" 'delete-other-windows)
(define-key evil-window-map "\C-p" 'evil-window-mru)
(define-key evil-window-map "\C-r" 'evil-window-rotate-downwards)
(define-key evil-window-map "\C-R" 'evil-window-rotate-upwards)
(define-key evil-window-map "\C-s" 'evil-window-split)
(define-key evil-window-map "\C-S" 'evil-window-split)
(define-key evil-window-map "\C-t" 'evil-window-top-left)
(define-key evil-window-map "\C-v" 'evil-window-vsplit)
(define-key evil-window-map "\C-w" 'evil-window-next)
(define-key evil-window-map "\C-W" 'evil-window-prev)
(define-key evil-window-map "\C-_" 'evil-window-set-height)
(define-key evil-window-map "\C-f" 'ffap-other-window)

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
(define-key evil-motion-state-map "e" 'evil-forward-word-end)
(define-key evil-motion-state-map "E" 'evil-forward-WORD-end)
(define-key evil-motion-state-map "f" 'evil-find-char)
(define-key evil-motion-state-map "F" 'evil-find-char-backward)
(define-key evil-motion-state-map "G" 'evil-goto-line)
(define-key evil-motion-state-map "h" 'evil-backward-char)
(define-key evil-motion-state-map "H" 'evil-window-top)
(define-key evil-motion-state-map "j" 'evil-next-line)
(define-key evil-motion-state-map "k" 'evil-previous-line)
(define-key evil-motion-state-map "l" 'evil-forward-char)
;(define-key evil-motion-state-map " " 'evil-forward-char)
(define-key evil-motion-state-map "K" 'evil-lookup)
(define-key evil-motion-state-map "L" 'evil-window-bottom)
(define-key evil-motion-state-map "M" 'evil-window-middle)
(define-key evil-motion-state-map "n" 'evil-search-next)
(define-key evil-motion-state-map "N" 'evil-search-previous)
;(define-key evil-motion-state-map "t" 'evil-find-char-to)
;(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "w" 'evil-forward-word-begin)
(define-key evil-motion-state-map "W" 'evil-forward-WORD-begin)
;(define-key evil-motion-state-map "gd" 'evil-goto-definition)
;(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
;(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
;(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
;(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
;(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
;(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
;(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
;(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
;(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
;(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
;(define-key evil-motion-state-map "g\C-]" 'find-tag)
(define-key evil-motion-state-map "{" 'evil-backward-paragraph)
(define-key evil-motion-state-map "}" 'evil-forward-paragraph)
(define-key evil-motion-state-map "#" 'evil-search-word-backward)
;(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward)
(define-key evil-motion-state-map "$" 'evil-end-of-line)
(define-key evil-motion-state-map "%" 'evil-jump-item)
(define-key evil-motion-state-map "`" 'evil-goto-mark)
(define-key evil-motion-state-map "'" 'evil-goto-mark-line)
(define-key evil-motion-state-map "(" 'evil-backward-sentence)
(define-key evil-motion-state-map ")" 'evil-forward-sentence)
;(define-key evil-motion-state-map "]]" 'evil-forward-section-begin)
;(define-key evil-motion-state-map "][" 'evil-forward-section-end)
;(define-key evil-motion-state-map "[[" 'evil-backward-section-begin)
;(define-key evil-motion-state-map "[]" 'evil-backward-section-end)
;(define-key evil-motion-state-map "[(" 'evil-previous-open-paren)
;(define-key evil-motion-state-map "])" 'evil-next-close-paren)
;(define-key evil-motion-state-map "[{" 'evil-previous-open-brace)
;(define-key evil-motion-state-map "]}" 'evil-next-close-brace)
(define-key evil-motion-state-map "*" 'evil-search-word-forward)
;(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward)
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
;(define-key evil-motion-state-map (kbd "RET") 'evil-ret)
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
;(define-key evil-motion-state-map "gv" 'evil-visual-restore)
;(define-key evil-motion-state-map (kbd "C-^") 'evil-buffer)
(define-key evil-motion-state-map [left] 'evil-backward-char)
(define-key evil-motion-state-map [right] 'evil-forward-char)
(define-key evil-motion-state-map [up] 'evil-previous-line)
(define-key evil-motion-state-map [down] 'evil-next-line)
;(define-key evil-motion-state-map "zl" 'evil-scroll-column-right)
;(define-key evil-motion-state-map [?z right] "zl")
;(define-key evil-motion-state-map "zh" 'evil-scroll-column-left)
;(define-key evil-motion-state-map [?z left] "zh")
;(define-key evil-motion-state-map "zL" 'evil-scroll-right)
;(define-key evil-motion-state-map "zH" 'evil-scroll-left)
(define-key evil-motion-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;
;(when evil-want-C-i-jump
;  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))
;
;(when evil-want-C-u-scroll
;  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
;
(define-key evil-visual-state-map [escape] 'evil-exit-visual-state)


;;; Insert state

;(define-key evil-insert-state-map "\C-k" 'evil-insert-digraph)
;(define-key evil-insert-state-map "\C-o" 'evil-execute-in-normal-state)
;(define-key evil-insert-state-map "\C-r" 'evil-paste-from-register)
;(define-key evil-insert-state-map "\C-y" 'evil-copy-from-above)
;(define-key evil-insert-state-map "\C-e" 'evil-copy-from-below)
;(define-key evil-insert-state-map "\C-n" 'evil-complete-next)
;(define-key evil-insert-state-map "\C-p" 'evil-complete-previous)
;(define-key evil-insert-state-map "\C-x\C-n" 'evil-complete-next-line)
;(define-key evil-insert-state-map "\C-x\C-p" 'evil-complete-previous-line)
;(define-key evil-insert-state-map "\C-t" 'evil-shift-right-line)
;(define-key evil-insert-state-map "\C-d" 'evil-shift-left-line)
;(define-key evil-insert-state-map [remap delete-backward-char] 'evil-delete-backward-char-and-join)
;(define-key evil-insert-state-map [delete] 'delete-char)
;(define-key evil-insert-state-map [remap newline] 'evil-ret)
;(define-key evil-insert-state-map [remap newline-and-indent] 'evil-ret-and-indent)
;(define-key evil-insert-state-map [escape] 'evil-normal-state)
;(define-key evil-insert-state-map
  ;(read-kbd-macro evil-toggle-key) 'evil-emacs-state)

;(if evil-want-C-w-delete
;    (define-key evil-insert-state-map "\C-w" 'evil-delete-backward-word)
;  (define-key evil-insert-state-map "\C-w" 'evil-window-map))

;;; Replace state

(define-key evil-replace-state-map (kbd "DEL") 'evil-replace-backspace)
(define-key evil-replace-state-map [escape] 'evil-normal-state)

;;; Emacs state

(define-key evil-emacs-state-map
  (read-kbd-macro evil-toggle-key) 'evil-exit-emacs-state)

(when evil-want-C-w-in-emacs-state
  (define-key evil-emacs-state-map "\C-w" 'evil-window-map))

;;; Minibuffer

;(define-key minibuffer-local-map "\C-p" 'evil-complete-next)
;(define-key minibuffer-local-map "\C-n" 'evil-complete-previous)
;(define-key minibuffer-local-map "\C-x\C-p" 'evil-complete-next-line)
;(define-key minibuffer-local-map "\C-x\C-n" 'evil-complete-previous-line)

;;; Mouse
;(define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)
;(define-key evil-visual-state-map [mouse-2] 'evil-exit-visual-and-repeat)
;(define-key evil-normal-state-map [mouse-2] 'mouse-yank-primary)
;(define-key evil-insert-state-map [mouse-2] 'mouse-yank-primary)

;; Ex
(define-key evil-motion-state-map ":" 'evil-ex)
(define-key evil-motion-state-map "!" 'evil-shell-command)

;(evil-ex-define-cmd "e[dit]" 'evil-edit)
;(evil-ex-define-cmd "w[rite]" 'evil-write)
;(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
;(evil-ex-define-cmd "sav[eas]" 'evil-save)
;(evil-ex-define-cmd "r[ead]" 'evil-read)
;(evil-ex-define-cmd "b[uffer]" 'evil-buffer)
;(evil-ex-define-cmd "bn[ext]" 'evil-next-buffer)
;(evil-ex-define-cmd "bp[revious]" 'evil-prev-buffer)
;(evil-ex-define-cmd "bN[ext]" "bprevious")
;(evil-ex-define-cmd "sb[uffer]" 'evil-split-buffer)
;(evil-ex-define-cmd "sbn[ext]" 'evil-split-next-buffer)
;(evil-ex-define-cmd "sbp[revious]" 'evil-split-prev-buffer)
;(evil-ex-define-cmd "sbN[ext]" "sbprevious")
;(evil-ex-define-cmd "buffers" 'buffer-menu)
;(evil-ex-define-cmd "files" 'evil-show-files)
;(evil-ex-define-cmd "ls" "buffers")
;
;(evil-ex-define-cmd "c[hange]" 'evil-change)
;(evil-ex-define-cmd "co[py]" 'evil-copy)
;(evil-ex-define-cmd "t" "copy")
;(evil-ex-define-cmd "m[ove]" 'evil-move)
;(evil-ex-define-cmd "d[elete]" 'evil-delete)
;(evil-ex-define-cmd "y[ank]" 'evil-yank)
;(evil-ex-define-cmd "go[to]" 'evil-goto-char)
;(evil-ex-define-cmd "j[oin]" 'evil-join)
;(evil-ex-define-cmd "le[ft]" 'evil-align-left)
;(evil-ex-define-cmd "ri[ght]" 'evil-align-right)
;(evil-ex-define-cmd "ce[nter]" 'evil-align-center)
;(evil-ex-define-cmd "sp[lit]" 'evil-window-split)
;(evil-ex-define-cmd "vs[plit]" 'evil-window-vsplit)
;(evil-ex-define-cmd "new" 'evil-window-new)
;(evil-ex-define-cmd "vne[w]" 'evil-window-vnew)
;(evil-ex-define-cmd "clo[se]" 'evil-window-delete)
;(evil-ex-define-cmd "on[ly]" 'delete-other-windows)
;(evil-ex-define-cmd "q[uit]" 'evil-quit)
;(evil-ex-define-cmd "wq" 'evil-save-and-close)
;(evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
;(evil-ex-define-cmd "qa[ll]" "quitall")
;(evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
;(evil-ex-define-cmd "xa[ll]" "wqall")
;(evil-ex-define-cmd "x[it]" 'evil-save-modified-and-close)
;(evil-ex-define-cmd "exi[t]" 'evil-save-modified-and-close)
;(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
;(evil-ex-define-cmd "g[lobal]" 'evil-ex-global)
;(evil-ex-define-cmd "v[global]" 'evil-ex-global-inverted)
;(evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
;(evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
;(evil-ex-define-cmd "&" 'evil-ex-repeat-substitute)
;(evil-ex-define-cmd "&&" 'evil-ex-repeat-substitute-with-flags)
;(evil-ex-define-cmd "~" 'evil-ex-repeat-substitute-with-search)
;(evil-ex-define-cmd "~&" 'evil-ex-repeat-substitute-with-search-and-flags)
;(evil-ex-define-cmd "registers" 'evil-show-registers)
;(evil-ex-define-cmd "marks" 'evil-show-marks)
;(evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)
;(evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)
;(evil-ex-define-cmd "f[ile]" 'evil-show-file-info)
;(evil-ex-define-cmd "<" 'evil-shift-left)
;(evil-ex-define-cmd ">" 'evil-shift-right)
;(evil-ex-define-cmd "=" 'evil-ex-line-number)
;(evil-ex-define-cmd "!" 'evil-shell-command)
;(evil-ex-define-cmd "@:" 'evil-ex-repeat)
;(evil-ex-define-cmd "set-initial-state" 'evil-ex-set-initial-state)
;(evil-ex-define-cmd "show-digraphs" 'evil-ex-show-digraphs)
;
;;; search command line
;(define-key evil-ex-search-keymap "\d" #'evil-ex-delete-backward-char)
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; My normal config here...

(evil-define-state window
  "Window management state"
  :tag " <W> "
  :supress-keymap t)

(define-key evil-window-map (kbd "RET") 'evil-normal-state)
(define-key evil-window-map [return] 'evil-normal-state)
(define-key evil-window-map "m" 'evil-window-state)


(define-key evil-window-map "j" 'evil-window-next)
(define-key evil-window-map "k" 'evil-window-prev)
(define-key evil-window-map "c" 'delete-window)
(define-key evil-window-map "h"
  (lambda () (interactive)
    (let ((current-prefix-arg '(5)))
      (call-interactively 'evil-window-decrease-width))))
(define-key evil-window-map "l"
  (lambda () (interactive)
    (let ((current-prefix-arg '(5)))
      (call-interactively 'evil-window-increase-width))))
(define-key evil-window-map "H"
  (lambda () (interactive)
    (let ((current-prefix-arg '(5)))
      (call-interactively 'evil-window-increase-height))))
(define-key evil-window-map "L"
  (lambda () (interactive)
    (let ((current-prefix-arg '(5)))
      (call-interactively 'evil-window-decrease-height))))
(define-key evil-window-map "f" 'delete-other-windows)
(define-key evil-window-map "a" 'switch-to-buffer)
(define-key evil-window-map "q" 'kill-buffer)
;; Space will be for layout concerns
(define-key evil-window-map " f" 'delete-other-windows)
(define-key evil-window-map " u" 'winner-undo)
(define-key evil-window-map " r" 'winner-redo)
(define-key evil-window-map "w" 'next-buffer-no-star)
(define-key evil-window-map "b" 'prev-buffer-no-star)

(set-keymap-parent evil-window-state-map evil-window-map)
(define-key evil-window-state-map "m" 'evil-normal-state)



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

; g map
(define-key evil-motion-state-map "gt" 'next-buffer)
(define-key evil-motion-state-map "gT" 'previous-buffer)
(define-key evil-motion-state-map "gb" 'evil-backward-arg)
(define-key evil-motion-state-map "gw" 'evil-forward-arg)
;(define-key evil-motion-state-map "gq" 'kill-buffer)
(define-key evil-normal-state-map "g&" 'evil-ex-repeat-global-substitute)
(define-key evil-normal-state-map "gi" 'evil-insert-resume) ; insert mode at ins. mode cursor point
;(define-key evil-normal-state-map "gJ" 'evil-join-whitespace)
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)
(define-key evil-normal-state-map "g~" 'evil-invert-case)
(define-key evil-normal-state-map "gy" 'xcopy)
(define-key evil-normal-state-map "gP" 'xpaste)
(define-key evil-normal-state-map "gp" 'xpaste-after-char)
(define-key evil-motion-state-map "gdd" 'evil-goto-definition)
(define-key evil-motion-state-map "gdp" 'pop-tag-mark)
(define-key evil-motion-state-map "ge" 'evil-backward-word-end)
(define-key evil-motion-state-map "gE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "gg" 'evil-goto-first-line)
(define-key evil-motion-state-map "gj" 'evil-next-visual-line)
(define-key evil-motion-state-map "gk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "gl" 'evil-scroll-right)
(define-key evil-motion-state-map "gh" 'evil-scroll-left)
(define-key evil-motion-state-map "g0" 'evil-beginning-of-visual-line)
(define-key evil-motion-state-map "g_" 'evil-last-non-blank)
(define-key evil-motion-state-map "g^" 'evil-first-non-blank-of-visual-line)
;(define-key evil-motion-state-map "gm" 'evil-middle-of-visual-line)
(define-key evil-motion-state-map "g$" 'evil-end-of-visual-line)
(define-key evil-motion-state-map "g\C-]" 'find-tag)
(define-key evil-normal-state-map "g;" 'goto-last-change)
(define-key evil-normal-state-map "g," 'goto-last-change-reverse)
(define-key evil-motion-state-map "g#" 'evil-search-unbounded-word-backward) ; /<word under point
(define-key evil-motion-state-map "g*" 'evil-search-unbounded-word-forward) ; ?<word under point
(define-key evil-motion-state-map "gv" 'evil-visual-restore) ; re-highlight region
(define-key evil-motion-state-map "gn" 'evil-next-match)
(define-key evil-motion-state-map "gN" 'evil-previous-match)
;; these bookmarks are persistent... and a little more serious than I was thinking
;(define-key evil-motion-state-map "gq" 'bookmark-set)
;(define-key evil-motion-state-map "gQ" 'bookmark-delete)
;(define-key evil-motion-state-map "ga" 'bookmark-jump)
;(define-key evil-motion-state-map "gA" 'bookmark-bmenu-list)
(define-key evil-motion-state-map "gr" 'evil-ace-jump-word-mode)
(define-key evil-motion-state-map "gc" 'evil-ace-jump-char-mode)
(define-key evil-motion-state-map "gf" 'evil-ace-jump-line-mode)

;(define-key evil-motion-state-map "go" 'nop) ; TODO - make object movement map for uncommon ones (paragraph, etc)
(define-key evil-motion-state-map "gob" 'nop) ; TODO - make another map that is the same but for going backwards


; t map
(define-key evil-motion-state-map "th" 'evil-window-map)
(define-key evil-motion-state-map "tt" 'evil-find-char-to)
(define-key evil-motion-state-map "T" 'evil-find-char-to-backward)
(define-key evil-motion-state-map "tp" 'projectile-command-map)
;; "ts" will stand for "toggle setting"
(define-key evil-motion-state-map "tsp" 'smartparens-mode)
(define-key evil-motion-state-map "tsw" 'whitespace)
(define-key evil-motion-state-map "tsc" 'color-hex-syntax)
(define-key evil-motion-state-map "tst" 'toggle-truncate-lines)
(define-key evil-motion-state-map "tsf" 'flycheck-mode)
(define-key evil-motion-state-map "tsF" 'fci-mode-toggle)
(define-key evil-motion-state-map "tss" 'flyspell-mode)
(define-key evil-motion-state-map "tsS" 'flyspell-prog-mode)
(define-key evil-motion-state-map "tse" 'electric-indent-mode)
(define-key evil-motion-state-map "tsr" 'rainbow-delimiters-mode)

;; s map
;(define-key evil-normal-state-map "ss" 'evil-substitute)
;(define-key evil-normal-state-map "sS" 'evil-change-whole-line)
(evil-define-key 'visual evil-surround-mode-map "s" nil)
(evil-define-key 'visual evil-surround-mode-map "S" nil)
(define-key evil-visual-state-map "ss" 'evil-surround-region)
(define-key evil-visual-state-map "sS" 'evil-Surround-region)
(define-key evil-visual-state-map "sh" 'shell-command-on-region)
(define-key evil-normal-state-map "sh" 'shell-command)
(define-key evil-normal-state-map "s)" 'eval-last-sexp)
(define-key evil-visual-state-map "s)" 'eval-region)
(define-key evil-visual-state-map (kbd "C-s") 'yas-insert-with-region)
(define-key evil-normal-state-map "sm" 'evil-set-marker)
(define-key evil-insert-state-map (kbd "M-c") 'smex)
(define-key evil-motion-state-map (kbd "M-c") 'smex)
(define-key evil-motion-state-map "sx" 'eval-expression)


;; command modes and macros
(define-key evil-motion-state-map "-" 'evil-ex)
;(define-key evil-motion-state-map "|" 'execute-extended-command)
(define-key evil-motion-state-map "|" 'smex)
(define-key evil-motion-state-map "_" 'eval-expression)
(define-key evil-motion-state-map "Q" 'call-last-kbd-macro)
;; Movement
(define-key evil-normal-state-map "mm" 'evil-set-marker) ;;;;;;;;;; m will be my prefix for mode-specific bindings
;; everything in motion state is pulled into normal state
(define-key evil-motion-state-map "+" 'evil-repeat-find-char)
(define-key evil-motion-state-map "~" 'evil-repeat-find-char-reverse)
(define-key evil-motion-state-map " h" 'scroll-up)
(define-key evil-motion-state-map " t" 'scroll-down)
(define-key evil-motion-state-map " j" 'scroll-up)
(define-key evil-motion-state-map " k" 'scroll-down)
(define-key evil-motion-state-map " o" 'forward-symbol)
(define-key evil-motion-state-map " eo" 'backward-symbol)
(define-key evil-motion-state-map " f" 'yafolding-toggle-element)
(define-key evil-motion-state-map "  f" 'yafolding-toggle-all)
(define-key evil-motion-state-map " /" 'helm-swoop)
(define-key evil-motion-state-map "  /" 'helm-multi-swoop-all)
(define-key evil-motion-state-map "   /" 'helm-multi-swoop)
(define-key evil-motion-state-map " w" 'save-buffer)
(define-key evil-motion-state-map " -" 'helm-M-x)
(define-key evil-motion-state-map "J" 'evil-window-bottom)
(define-key evil-motion-state-map "K" 'evil-window-top)
(define-key evil-motion-state-map "{" 'backward-sexp)
(define-key evil-motion-state-map "}" 'forward-sexp)
(define-key evil-motion-state-map "[" 'backward-list)
(define-key evil-motion-state-map "]" 'forward-list)

;; Joining
;; TODO -- make a better mapping for this.  I should make my prefixes be mnemonic or something...
;;         for instance, g<key> is mostly navigation... t... mosty has window stuff in th... space is mostly one handed
;;         navigation aside from this one
(define-key evil-normal-state-map " J" 'evil-join)



;; input mode
(define-key evil-insert-state-map (kbd "DEL") 'delete-backward-char) ; remap away from the evil-version backspace
(define-key evil-insert-state-map "\C-v" #'quoted-insert) ; more vim-like

;; helm map to match what I've got going in zsh with zaw...
(define-prefix-command 'meta-space-map)
(global-set-key (kbd "M-SPC") 'meta-space-map)
(define-key meta-space-map " " 'helm-helm-commands)
(define-key meta-space-map (kbd "RET") 'helm-helm-commands)
(define-key meta-space-map "c" 'helm-M-x)
(define-key meta-space-map "p" 'helm-browse-project)
(define-key meta-space-map "g" 'helm-do-grep)

(define-prefix-command 'completer-map)
(define-key evil-insert-state-map "\M-h" 'completer-map)
(define-key completer-map "h" 'hippie-expand)
(define-key completer-map "\M-h" 'hippie-expand)
(define-key completer-map "n" 'evil-complete-next)
(define-key completer-map "\M-n" 'evil-complete-next)
(define-key completer-map "p" 'evil-complete-previous)
(define-key completer-map "\M-p" 'evil-complete-previous)
(define-key completer-map "f" 'he-expand-file-name)
(define-key completer-map "\M-f" 'he-expand-file-name)
(define-key completer-map "l" 'he-expand-lisp-symbol)
(define-key completer-map "\M-l" 'he-expand-lisp-symbol)
(define-key completer-map "s" 'yas-expand)
(define-key completer-map "\M-s" 'yas-expand)
(define-key evil-insert-state-map (kbd "TAB") 'company-complete-common-wgh)
;; put indentation on something...
(define-key completer-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "<backtab>") 'indent-for-tab-command)

(global-set-key (kbd "C-\\") 'evil-execute-in-normal-state)

;; Default mode settings
(setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
(setq evil-emacs-state-modes nil)


;; ex-style commands
(evil-ex-define-cmd "w[rite]" 'evil-write)
(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
(evil-ex-define-cmd "wn" 'evil-save) ;writes file and changes buffer name to name given
(evil-ex-define-cmd "q[uit]" 'wevil-quit)
(evil-ex-define-cmd "wq" 'wevil-save-and-quit)
(evil-ex-define-cmd "qa[ll]" "quitall")
(evil-ex-define-cmd "quita[ll]" 'evil-quit-all)
(evil-ex-define-cmd "wqa[ll]" 'evil-save-and-quit)
;(evil-ex-define-cmd "xa[ll]" "wqall")
(evil-ex-define-cmd "bd[elete]" 'evil-delete-buffer)
(evil-ex-define-cmd "s[ubstitute]" 'evil-ex-substitute)
;(evil-ex-define-cmd "!" 'evil-shell-command)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(defun ido-ffap-no ()
  (interactive)
  (let ((ido-use-filename-at-point nil))
    (call-interactively 'ido-find-file)))
(defun ido-ffap-yes ()
  (interactive)
  (let ((ido-use-filename-at-point 'guess))
    (call-interactively 'ido-find-file)))
(evil-ex-define-cmd "ff" 'ido-ffap-yes)
(evil-ex-define-cmd "f" 'ido-ffap-no)


(defun ish (cmd) (interactive (list (read-shell-command "$ ")))
  (insert-string (shell-command-to-string cmd)))


;; Mouse keys...
;(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
;(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))

