
;; settings and extensions to company mode

(require 'company)
(company-mode 1)

(defvar-local company-completing-wgh nil)

(defun company-pseudo-tooltip-wgh-frontend (command)
  "show tooltip when I want to search through options"
  (when company-completing-wgh (company-pseudo-tooltip-frontend command)))

(defun company-complete-common-wgh () (interactive)
  (if company-completing-wgh
      (company-complete-common)
    (setq company-completing-wgh t)))

(defadvice company-cancel (after company-wgh-style activate protect)
  (setq company-completing-wgh nil))

(define-key company-active-map (kbd "TAB") 'company-complete-common-wgh)
(define-key company-active-map [tab] 'company-complete-common-wgh)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-next)
(define-key company-active-map (kbd "C-h") nil)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

(setq company-frontends '(company-preview-frontend company-pseudo-tooltip-wgh-frontend company-echo-metadata-frontend))

(setq company-minimum-prefix-length 2)

