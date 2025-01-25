
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

(define-prefix-command 'popwin-map) ; because for some reason the one defined by
                                        ; popwin itself won't play nice with evil
(define-key popwin-map "b"    'popwin:popup-buffer)
(define-key popwin-map "l"    'popwin:popup-last-buffer)
(define-key popwin-map "o"    'popwin:display-buffer)
(define-key popwin-map "\C-b" 'popwin:switch-to-last-buffer)
(define-key popwin-map "\C-p" 'popwin:original-pop-to-last-buffer)
(define-key popwin-map "\C-o" 'popwin:original-display-last-buffer)
(define-key popwin-map " "    'popwin:select-popup-window)
(define-key popwin-map "s"    'popwin:stick-popup-window)
(define-key popwin-map "0"    'popwin:close-popup-window)
(define-key popwin-map "f"    'popwin:find-file)
(define-key popwin-map "\C-f" 'popwin:find-file)
(define-key popwin-map "e"    'popwin:messages)
(define-key popwin-map "\C-u" 'popwin:universal-display)
(define-key popwin-map "1"    'popwin:one-window)
