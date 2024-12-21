;; Environment setup

(setq dotfileswgh (concat (getenv "DOTFILESWGH") "/"))

(setq local-emacs.d-path
      (let ((emacs.d-path (getenv "EMACS_DOT_D_PATH")))
        (if emacs.d-path
            emacs.d-path
          "~/dotfileswgh-dotlocal/emacs.d/")))
(when (not (string-suffix-p "/" local-emacs.d-path))
  (setq local-emacs.d-path (concat local-emacs.d-path "/")))
;; Set the name that emacs actually knows...
(setq user-emacs-directory local-emacs.d-path)

(let ((local-f "~/dotfileswgh-dotlocal/emacs/env-conf.el"))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))
