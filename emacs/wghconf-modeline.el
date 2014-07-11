
;; first define some faces for the mode line stuff, especially
;; for evil states
(defface wevil-normal-face
  '((((background dark)) (:foreground "black" :background "cyan"))
    (((background light)) (:foreground "black" :background "cyan")))
  "normal state marker face")

(defface wevil-motion-face
  '((((background dark)) (:foreground "black" :background "blue"))
    (((background light)) (:foreground "black" :background "blue")))
  "motion state marker face")

(defface wevil-insert-face
  '((((background dark)) (:foreground "black" :background "magenta"))
    (((background light)) (:foreground "black" :background "magenta")))
  "insert state marker face")

(defface wevil-replace-face
  '((((background dark)) (:foreground "black" :background "red"))
    (((background light)) (:foreground "black" :background "red")))
  "replace state marker face")

(defface wevil-visual-face
  '((((background dark)) (:foreground "black" :background "brightcyan"))
    (((background light)) (:foreground "black" :background "brightcyan")))
  "visual state marker face")

(defface wevil-emacs-face
  '((((background dark)) (:foreground "black" :background "yellow"))
    (((background light)) (:foreground "black" :background "yellow")))
  "emacs state marker face")

(defface wevil-bufname-face
  '((((background dark)) (:foreground "magenta" :bold t))
    (((background light)) (:foreground "magenta" :bold t)))
  "face for buffer name")

(defface wevil-dirty-face
  '((((background dark)) (:foreground "color52" :background "red" :bold t))
    (((background light)) (:foreground "color52" :background "red" :bold t)))
  "buffer dirty (unsaved) face")

(defface wevil-ro-face
  '((((background dark)) (:foreground "black" :background "brightyellow" :bold t))
    (((background light)) (:foreground "black" :background "brightyellow" :bold t)))
  "read only tag face")


;; You can set both header-line-format and mode-line-format

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
      (list
       ;; Evil state
       '(:eval (cond
                ((eq evil-state 'impossible)
                 (propertize "IMPOSS" 'face 'wevil-emacs-face))
                ((eq evil-state 'visual)
                 (propertize "< VI >" 'face 'wevil-visual-face))
                ((eq evil-state 'normal)
                 (propertize "<NORM>" 'face 'wevil-normal-face))
                ((eq evil-state 'motion)
                 (propertize "<MOTN>" 'face 'wevil-motion-face))
                ((eq evil-state 'insert)
                 (propertize "<INST>" 'face 'wevil-insert-face))
                ((eq evil-state 'replace)
                 (propertize "<REPL>" 'face 'wevil-replace-face))
                ((eq evil-state 'emacs)
                 (propertize "<EMCS>" 'face 'wevil-emacs-face))
                (t (propertize "WHAT STATE??" 'face 'wevil-emacs-face))))

       '(:eval (propertize " %b " 'face 'wevil-bufname-face))

       ;; line and column
       " (" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%03l" 'face 'font-lock-constant-face) ","
       (propertize "%02c" 'face 'font-lock-constant-face)
       ") "

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "
       ;; add the time, with the date and the emacs uptime in the tooltip
       '(:eval (propertize (format-time-string "%H:%M")
                           'face 'font-lock-type-face))
       " %-"
       
       ))

(setq-default header-line-format
              (list
       ;; the full file name
       '(:eval (propertize (buffer-file-name) 'face 'font-lock-keyword-face))

       " [ "
       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (propertize "DIRTY" 'face 'wevil-dirty-face)))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat " "  (propertize " RO "
                                          'face 'wevil-ro-face))))
       "] "

       ;; the current major mode for the buffer.
       "["

       '(:eval (propertize "%m" 'face 'font-lock-string-face))

       "] "
       " --"
       minor-mode-alist  ;; list of minor modes
       " %-" ;; fill with '-'
       ))
