
; load local settings first, in case of proxy or something
(let ((local-f "~/dotfileswgh/dotlocal/emacs"))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))

(setq wgh/local-emacs.d-path
      (let ((emacs.d-path (getenv "EMACS_DOT_D_PATH")))
        (if emacs.d-path
            emacs.d-path
          (concat (getenv "DOTFILESWGH") "/dotlocal/emacs.d/"))))

(let ()
  (make-directory wgh/local-emacs.d-path t)
  (setq package-user-dir (concat wgh/local-emacs.d-path "elpa"))

  (let ((default-directory wgh/local-emacs.d-path))
    (normal-top-level-add-subdirs-to-load-path))
  (let ((default-directory "~/.emacs.d/"))
    (normal-top-level-add-subdirs-to-load-path))
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
