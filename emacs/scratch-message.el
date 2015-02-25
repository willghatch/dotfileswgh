(setq scratch-ascii-banner
";;      ___           ___           ___           ___           ___     
;;     /  /\\         /__/\\         /  /\\         /  /\\         /  /\\    
;;    /  /:/_       |  |::\\       /  /::\\       /  /:/        /  /:/_   
;;   /  /:/ /\\      |  |:|:\\     /  /:/\\:\\     /  /:/        /  /:/ /\\  
;;  /  /:/ /:/_   __|__|:|\\:\\   /  /:/~/::\\   /  /:/  ___   /  /:/ /::\\ 
;; /__/:/ /:/ /\\ /__/::::| \\:\\ /__/:/ /:/\\:\\ /__/:/  /  /\\ /__/:/ /:/\\:\\
;; \\  \\:\\/:/ /:/ \\  \\:\\~~\\__\\/ \\  \\:\\/:/__\\/ \\  \\:\\ /  /:/ \\  \\:\\/:/~/:/
;;  \\  \\::/ /:/   \\  \\:\\        \\  \\::/       \\  \\:\\  /:/   \\  \\::/ /:/ 
;;   \\  \\:\\/:/     \\  \\:\\        \\  \\:\\        \\  \\:\\/:/     \\__\\/ /:/  
;;    \\  \\::/       \\  \\:\\        \\  \\:\\        \\  \\::/        /__/:/   
;;     \\__\\/         \\__\\/         \\__\\/         \\__\\/         \\__\\/    
")

(setq scratch-useful-message
"

;; Useful notes for run-time definitions:
;;(define-key evil-normal-state-map \"a\" 'evil-append)
;;(defalias 'eb 'eval-buffer)
;; kmap-[m,n,i,v,w,t]
;;(kmap-t \"p\" 'previous-something)
;;
;;(defun foo (n)
;;  (interactive)
;;  (foo)) ; so I remember defun format
;;
;;(mapcar (lambda (b)
;;        (with-current-buffer b
;;          (reformat-file)))
;;      (file-visiting-buffer-list))

")


(setq initial-scratch-message (concat scratch-ascii-banner scratch-useful-message))
