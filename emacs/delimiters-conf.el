(require 'smartparens)
(require 'evil)
(require 'evil-textobj-anyblock)
(require 'evil-surround)
(require 'cl-lib)

;; remove the pairs that won't be good for lisp
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;; treat quotes correctly
(setq sp-sexp-prefix '((racket-mode regexp "#?[`',]@?")))

(defun make-inner-textobj-name (name)
  (intern (concat "inner-" name "-textobj")))
(defun make-outer-textobj-name (name)
  (intern (concat "outer-" name "-textobj")))

;; this is to do with text objects, not smartparens exactly, but it's related
(defmacro define-text-object-with-name (start-regex end-regex name)
  (let ((inner-name (make-inner-textobj-name name))
        (outer-name (make-outer-textobj-name name))
        ;; use characters instead of regex to avoid block behavior
        (pstart (if (= (length start-regex) 1)
                    (string-to-char start-regex)
                  start-regex))
        (pend (if (= (length end-regex) 1)
                  (string-to-char end-regex)
                end-regex)))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,pstart ,pend beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,pstart ,pend beg end type count t)))))

(defun bind-textobj-key-maybe (key textobj-name)
  (when (= 1 (length key))
    (define-key evil-inner-text-objects-map key (make-inner-textobj-name textobj-name))
    (define-key evil-outer-text-objects-map key (make-outer-textobj-name textobj-name))))

(defun make-evil-surround-pair-maybe (key l r)
  (if (= 1 (length key))
      `(,(string-to-char key) . (,l . ,r))
    nil))

;;;; TODO -- macro that takes lists of parens and their name, adds them to sp-pair, and makes evil-textobjs with them, binds them the closing and opening paren in the keymaps, and adds them to evil-any-textobj's map, and also add stuff for `cs»｣` or `ds»`
(defmacro add-delimiter-pairs (&rest delimiter-lists)
  `(progn
     ,@(mapcar (lambda (delim-list)
                 `(sp-pair ,(car delim-list) ,(cadr delim-list)))
               delimiter-lists)
     (setq evil-textobj-anyblock-blocks
           '(,@(mapcar (lambda (delim-list)
                         (cons (car delim-list) (cadr delim-list)))
                       delimiter-lists)))
     ,@(mapcar (lambda (delim-list)
                 `(define-text-object-with-name ,@delim-list))
               delimiter-lists)
     ,@(mapcar (lambda (delim-list)
                 `(bind-textobj-key-maybe ,(cadr delim-list)
                                          ,(caddr delim-list)))
               delimiter-lists)
     (setq
      evil-surround-pairs-alist-addendum
      '(,@(remove-if 'null
                     (append
                      ;; make pairs with both the left and the right delimiters as keys
                      (mapcar (lambda (delim-list)
                                (make-evil-surround-pair-maybe (car delim-list)
                                                               (car delim-list)
                                                               (cadr delim-list)))
                              delimiter-lists)
                      (mapcar (lambda (delim-list)
                                (make-evil-surround-pair-maybe (cadr delim-list)
                                                               (car delim-list)
                                                               (cadr delim-list)))
                              delimiter-lists))))
     )))


(add-delimiter-pairs

 ;; don't run this on the parens with good built-in support...
 ;; the built-in support is better.
 ;;("(" ")" "parens")
 ;;("{" "}" "braces")
 ;;("\\[" "\\]" "brackets")

 ;; quotes are handled weird, and are basically horrible.
 ;;("\"" . "\"")

 ;; single quotes aren't text object material in lispy languages
 ;; as well as having the issues that double quotes have.
 ;;("'" . "'")

 ;; fancy delimiters!
 ("#|" "|#" "racket-multiline-comments")
 ("«" "»" "guillemets")
 ("‹" "›" "single-guillemets")
 ("｢" "｣" "cjk-angle-quotes")
 ("『" "』" "white-cjk-angle-quotes")
 ("“" "”" "curved-double-quotes")
 ("‘" "’" "curved-single-quotes")
 ;;("❛" "❜")
 ;;("❝" "❞")
 ("⌈" "⌉" "ceiling")
 ("⌊" "⌋" "floor")
 ("⦗" "⦘" "tortoise-shells")
 ("﴾" "﴿" "ornate-parens")
 ("⸢" "⸣" "upper-corner-brackets")
 ("⸤" "⸥" "lower-corner-brackets")
 ("⟅" "⟆" "bag-delimiters")
 ("⦓" "⦔" "inequality-brackets")
 ("⦕" "⦖" "double-inequality-brackets")
 ("🌜" "🌛" "moon-faces")
 ;;("〖" "〗" "weird-bracket-1")
 ;;("⟦" "⟧")
 ;;("⦇" "⦈")
 ;;("⦉" "⦊")
 ;;("〈" "〉")
 ;;("⦑" "⦒")
 ;;("⧼" "⧽")
 ;;("⁅" "⁆")
 ;;("❰" "❱")
 ;;("❮" "❯")
 ;;("⸦" "⸧")
 ;;("᚛" "᚜")
 ;;("༺" "༻")
 ;;("༼" "༽")
 )

;; add the normal parens back to anyblock
(setq evil-textobj-anyblock-blocks
      (append '(("(" . ")")
                ("{" . "}")
                ("\\[" . "\\]"))
              evil-textobj-anyblock-blocks))

;; bind # to the textobj maps
(bind-textobj-key-maybe "#" "racket-multiline-comments")

(setq-default evil-surround-pairs-alist
              (append
               (list '(?# . ("#|" . "|#"))
                     ;; don't use the versions with spaces, I hate those
                     '(40 . ("(" . ")"))
                     '(41 . ("[" . "]"))
                     '(123 . ("{" . "}"))
                     )
               evil-surround-pairs-alist-addendum
               (remove-if (lambda (amem)
                            (or (= (car amem) ?#)
                                ;; (
                                (= (car amem) 40)
                                ;; [
                                (= (car amem) 41)
                                ;; {
                                (= (car amem) 123)))
                          evil-surround-pairs-alist)))
