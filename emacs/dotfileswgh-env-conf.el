;; Environment setup

(defun ensure-final-slash (str)
  "If the string doesn't end in a final slash, add one."
  (if (string-suffix-p "/" str)
      str
    (concat str "/")))

(setq dotfileswgh (ensure-final-slash (getenv "DOTFILESWGH")))
(setq dotfileswgh-dotlocal (ensure-final-slash (getenv "DOTFILESWGH_DOTLOCAL")))
(setq dotfileswgh-pri (ensure-final-slash (getenv "DOTFILESWGH_PRI")))

(setq local-emacs.d-path
      (ensure-final-slash
       (let ((emacs.d-path (getenv "EMACS_DOT_D_PATH")))
         (if emacs.d-path
             emacs.d-path
           (concat dotfileswgh-dotlocal "emacs.d/")))))

;; Set the name that emacs actually knows...
(setq user-emacs-directory local-emacs.d-path)

(setq native-comp-eln-load-path (list (concat local-emacs.d-path "eln-cache/")))

;; Straight.el config
(defvar bootstrap-version)
(setq straight-base-dir (concat local-emacs.d-path "straight/"))
(setq wgh/straight-first-install-p (not (file-exists-p straight-base-dir)))
;; The default for this has find-at-startup, which runs find(1) and is slow.
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-profiles `((nil . ,(concat dotfileswgh "emacs/straight-lockfile-for-dotfileswgh.el"))))
(setq straight-vc-git-default-clone-depth 1)


(let ((local-f (concat dotfileswgh-dotlocal "/emacs/env-conf.el")))
  (when (file-exists-p local-f)
    (with-demoted-errors "Warning: %S" (load-file local-f))))

(setq package-enable-at-startup nil)
(setq package-user-dir (concat local-emacs.d-path "elpa/"))

;; Set up load path for requires
(let ((default-directory (concat dotfileswgh "emacs/")))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat dotfileswgh "external/emacs/")))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (concat dotfileswgh-pri "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(let ((default-directory (concat dotfileswgh-dotlocal "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(setq load-path (cons (concat local-emacs.d-path "single-files/") load-path))
(setq load-path (cons (concat dotfileswgh "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-pri "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-dotlocal "emacs/") load-path))
;; Add straight build dirs, so that emacs can search in them, rather than using
;; straight on every emacs load.  It won't process autoloads, but I can wrap
;; key bindings with requires or otherwise force autoload evaluation for things
;; that I care about.
(let ((default-directory (concat straight-base-dir "straight/build")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(provide 'dotfileswgh-env-conf)
