;;; -*- lexical-binding: t; -*-

(require 'helm-projectile)
(helm-projectile-on)
(define-prefix-command 'helm-projectile-map)
(define-key projectile-command-map "h" 'helm-projectile-map)
(define-key helm-projectile-map (kbd "h") 'helm-projectile)
(define-key helm-projectile-map (kbd "f") 'helm-projectile-find-file-dwim)
(define-key helm-projectile-map (kbd "F") 'helm-projectile-find-file)

