;; Environment setup

(setq dotfileswgh (concat (getenv "DOTFILESWGH") "/"))

(setq local-emacs.d-path
      (let ((emacs.d-path (getenv "EMACS_DOT_D_PATH")))
        (if emacs.d-path
            emacs.d-path
          (concat dotfileswgh "/dotlocal/emacs.d"))))
;; Set the name that emacs actually knows...
(setq user-emacs-directory local-emacs.d-path)

(let ((local-f (concat dotfileswgh "dotlocal/emacs/env-conf.el")))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))
