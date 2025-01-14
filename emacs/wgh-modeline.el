
;; first define some faces for the mode line stuff, especially
;; for evil states
(defface wevil-normal-face
  '((t (:foreground "black" :background "cyan")))
  "normal state marker face")

(defface wevil-motion-face
  '((t (:foreground "black" :background "blue")))
  "motion state marker face")

(defface wevil-insert-face
  '((t (:foreground "black" :background "magenta")))
  "insert state marker face")

(defface wevil-replace-face
  '((t (:foreground "black" :background "red")))
  "replace state marker face")

(defface wevil-visual-face
  '((t (:foreground "black" :background "green")))
  "visual state marker face")

(defface wevil-emacs-face
  '((t (:foreground "black" :background "white")))
  "emacs state marker face")

(defface wevil-operator-face
  '((t (:foreground "black" :background "yellow")))
  "operator state marker face")

(defface wevil-bufname-face
  '((t (:foreground "magenta" :bold t)))
  "face for buffer name")

(defface wevil-unsaved-face
  '((t (:foreground "#5f0000" :background "red" :bold t)))
  "buffer unsaved (unsaved) face")

(defface wevil-unsaved-global-face
  '((t (:foreground "#5f0000" :background "magenta" :bold t)))
  "face for marker for when any file-visiting buffer is unsaved")

(defface wevil-ro-face
  '((t (:foreground "black" :background "yellow" :bold t)))
  "read only tag face")

(defface wevil-pager-face
  '((t (:foreground "black" :background "purple")))
  "pager state marker face")

;; You can set both header-line-format and mode-line-format

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
      (list
       ;; Evil state
       '(:eval (cond ((and (boundp 'estate-state) estate-local-mode)
                      (cond
                       ((eq estate-state 'command)
                        (propertize "<CMD >" 'face 'wevil-normal-face))
                       ((eq estate-state 'visual)
                        (propertize "< VI >" 'face 'wevil-visual-face))
                       ((eq estate-state 'visual-rectangle)
                        (propertize "<RECT>" 'face 'wevil-visual-face))
                       ((eq estate-state 'visual-line)
                        (propertize "<LINE>" 'face 'wevil-visual-face))
                       ((eq estate-state 'insert)
                        (propertize "<INST>" 'face 'wevil-insert-face))
                       ((eq estate-state 'pager)
                        (propertize "<PAGE>" 'face 'wevil-pager-face))
                       (t (propertize "WHAT ESTATE??" 'face 'wevil-emacs-face))))
                     ((boundp 'evil-state)
                      (cond
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
                       ((eq evil-state 'operator)
                        (propertize "<OPER>" 'face 'wevil-operator-face))
                       ((eq evil-state 'pager)
                        (propertize "<PAGE>" 'face 'wevil-pager-face))
                       (t (propertize "WHAT STATE??" 'face 'wevil-emacs-face))))
                     (t (propertize "<NO STATE INFO>" 'face 'wevil-emacs-face))))

       '(:eval (propertize " %b " 'face 'wevil-bufname-face))

       ;; line and column
       '(:eval
         (let ((nlines (line-number-at-pos (point-max)))
               (curline (line-number-at-pos (point))))
           (concat
            (propertize (format "%3d" (/ (* 100 curline)
                                         nlines))
                        'face 'font-lock-constant-face)
            "%% "
            (propertize "%03l" 'face 'font-lock-constant-face)
            "/"
            (propertize (number-to-string nlines) 'face 'font-lock-constant-face)
            " "
            )))
       " C0:"
       '(:eval (propertize "%03c" 'face 'font-lock-constant-face))
       " P1:"
       '(:eval (propertize (let* ((ps (format "%s" (point)))
                                  (len (length (format "%s" (point-max))))
                                  (lendiff (- len (length ps))))
                             (concat (make-string lendiff (aref " " 0)) ps))
                           'face 'font-lock-constant-face))
       " M1:"
       '(:eval (propertize (let* ((ps (format "%s" (mark)))
                                  (len (length (format "%s" (point-max))))
                                  (lendiff (- len (length ps))))
                             (concat (make-string lendiff (aref " " 0)) ps))
                           'face 'font-lock-constant-face))

       ;; add the time, with the date and the emacs uptime in the tooltip
       ;; Well, I have plenty of clocks, I never look at this one.
       ;;'(:eval (propertize (format-time-string " %H:%M ")
       ;;                    'face 'font-lock-type-face))
       '(:eval (let ((coding buffer-file-coding-system))
                 (unless (or (equal coding 'undecided-unix)
                             (equal coding 'utf-8-unix)
                             (equal coding 'prefer-utf-8-unix))
                   (propertize (symbol-name coding) 'face 'font-lock-warning-face))))
       "%]%["
       '(:eval (or (and (boundp 'command-sentence-current-sentence)
                        (let ((ui-hints (seq-filter
                                         #'identity
                                         (mapcar (lambda (x) (cdr (assq 'ui-hint x)))
                                                 command-sentence-current-sentence))))
                          (and ui-hints
                               (format " %s" ui-hints))))
                   ""))
       " {"
       '(:eval (mapconcat (lambda (buffer)
                            (buffer-name buffer))
                          (cl-remove-if (lambda (buffer)
                                          (equal (buffer-name buffer) (buffer-name)))
                                        (file-visiting-buffer-list))
                          " "))
       "}"
       "%-" ; dashes to end
       ))

(setq-default header-line-format
              (list
       "["
       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (propertize "EDITED" 'face 'wevil-unsaved-face)))

       ;; was any buffer modified since the last save?
       '(:eval (when
                   (and
                    (not (buffer-modified-p))
                    (cl-reduce
                     (lambda (old buffer)
                       (or old (with-current-buffer buffer
                                 (and (buffer-file-name)
                                      (buffer-modified-p)))))
                     (buffer-list) :initial-value nil))
                 (propertize "EB" 'face 'wevil-unsaved-global-face)))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (propertize "RO" 'face 'wevil-ro-face)))
       "%e"
       "] "
       ;; the full file name
       '(:eval (propertize (or (buffer-file-name) "~NoFile~") 'face 'font-lock-preprocessor-face))


       ;; the current major mode for the buffer.
       " ["

       '(:eval (propertize "%m" 'face 'font-lock-string-face))

       "]"
       "%-" ;; fill with '-'
       ))


(mapcar (lambda (b) (progn
                      (set-buffer b)
                      (setq mode-line-format (default-value 'mode-line-format))))
        (buffer-list))

(provide 'wgh-modeline)
