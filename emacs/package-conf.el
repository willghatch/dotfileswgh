
; load local settings first, in case of proxy or something
(let ((local-f "~/dotfileswgh/dotlocal/emacs"))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))

(let ((local-e-d (concat (getenv "DOTFILESWGH") "/dotlocal/emacs.d")))
  (make-directory local-e-d t)
  (setq package-user-dir (concat local-e-d "/elpa"))

  (let ((default-directory local-e-d))
    (normal-top-level-add-subdirs-to-load-path))
  (let ((default-directory "~/.emacs.d"))
    (normal-top-level-add-subdirs-to-load-path))
  )

(require 'package)
(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)


(defun package-upgrade ()
  (interactive)
  (package-list-packages)
  (package-menu-mark-upgrades)
  (package-menu-execute))
