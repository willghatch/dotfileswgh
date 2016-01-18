(require 'smartparens)

;; remove the pairs that won't be good for lisp
(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;; add some pairs that seem to be lacking
(sp-local-pair 'racket-mode "#|" "|#")
(setq sp-sexp-prefix '((racket-mode regexp "#?[`',]@?")))

;; add other types of pairs things
;; a lot of these sorts of things are at:
;; http://xahlee.info/comp/unicode_matching_brackets.html
;;
;; TODO - make these work with text object selection, `cs<delimiter>` replacement, etc
(sp-pair "«" "»")
(sp-pair "‹" "›")
(sp-pair "｢" "｣")
(sp-pair "『" "』")
(sp-pair "“" "”")
(sp-pair "‘" "’")
(sp-pair "❛" "❜")
(sp-pair "❝" "❞")
(sp-pair "⦗" "⦘")
(sp-pair "〖" "〗")
(sp-pair "⟦" "⟧")
(sp-pair "⌈" "⌉")
(sp-pair "⌊" "⌋")
(sp-pair "⦇" "⦈")
(sp-pair "⦉" "⦊")
(sp-pair "⸢" "⸣")
(sp-pair "⸤" "⸥")
(sp-pair "〈" "〉")
(sp-pair "⦑" "⦒")
(sp-pair "⧼" "⧽")
(sp-pair "⁅" "⁆")
(sp-pair "❰" "❱")
(sp-pair "❮" "❯")
(sp-pair "⟅" "⟆")
(sp-pair "⦓" "⦔")
(sp-pair "⦕" "⦖")
(sp-pair "⸦" "⸧")
(sp-pair "᚛" "᚜")
(sp-pair "༺" "༻")
(sp-pair "༼" "༽")

