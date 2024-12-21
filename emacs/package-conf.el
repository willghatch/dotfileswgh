;; Load this AFTER env-conf.

; load local settings first, in case of proxy or something
(let ((local-f "~/dotfileswgh-dotlocal/emacs/package-conf.el"))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))

(let ()
  (make-directory local-emacs.d-path t)
  (setq package-user-dir (concat local-emacs.d-path "elpa"))

  (let ((default-directory local-emacs.d-path))
    (normal-top-level-add-subdirs-to-load-path))
  (let ((default-directory "~/dotfileswgh-dotlocal/emacs/"))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))
  ;(let ((default-directory "~/.emacs.d/"))
  ;  (normal-top-level-add-subdirs-to-load-path))
  )

(setq package-archives '(
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
;(package-initialize)


(defun package-upgrade ()
  (interactive)
  (require 'package)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute))
