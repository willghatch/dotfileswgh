;;; -*- lexical-binding: t; -*-
;; Environment setup

(defun ensure-final-slash (str)
  "If the string doesn't end in a final slash, add one."
  (if (string-suffix-p "/" str)
      str
    (concat str "/")))

(setq dotfileswgh (ensure-final-slash (getenv "DOTFILESWGH")))
(setq dotfileswgh-dotlocal (ensure-final-slash (getenv "DOTFILESWGH_DOTLOCAL")))
(setq dotfileswgh-rootgit (ensure-final-slash (getenv "DOTFILESWGH_ROOTGIT")))
(setq dotfileswgh-pri (ensure-final-slash (getenv "DOTFILESWGH_PRI")))
(setq dotfileswgh-pri-dotlocal (ensure-final-slash (getenv "DOTFILESWGH_PRI_DOTLOCAL")))
(setq dotfileswgh-ghp (ensure-final-slash (getenv "DOTFILESWGH_GHP")))
(setq dotfileswgh-list
      (list dotfileswgh
            dotfileswgh-ghp
            dotfileswgh-pri
            dotfileswgh-rootgit
            dotfileswgh-dotlocal
            dotfileswgh-pri-dotlocal
            ))
(setq dotfileswgh-labeled-list
      (list (list "dotfileswgh" dotfileswgh)
            (list "ghp" dotfileswgh-ghp)
            (list "pri" dotfileswgh-pri)
            (list "rootgit" dotfileswgh-rootgit)
            (list "dotlocal" dotfileswgh-dotlocal)
            (list "pri-dotlocal" dotfileswgh-pri-dotlocal)
            ))


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
(let ((default-directory (concat dotfileswgh-ghp "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(let ((default-directory (concat dotfileswgh-pri "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(let ((default-directory (concat dotfileswgh-dotlocal "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(let ((default-directory (concat dotfileswgh-pri-dotlocal "emacs/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
(setq load-path (cons (concat local-emacs.d-path "single-files/") load-path))
(setq load-path (cons (concat dotfileswgh "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-ghp "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-pri "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-pri-dotlocal "emacs/") load-path))
(setq load-path (cons (concat dotfileswgh-dotlocal "emacs/") load-path))
;; Add straight build dirs, so that emacs can search in them, rather than using
;; straight on every emacs load.  It won't process autoloads, but I can wrap
;; key bindings with requires or otherwise force autoload evaluation for things
;; that I care about.
(let ((default-directory (concat straight-base-dir "straight/build")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
;; Some packages ship with Emacs but at a version older than what straight installs.
;; Previously I moved the system paths to the end of the load path (normal-top-level-add-subdirs-to-load-path adds to the end).
;; But doing so broke things.
;; It turns out that there are many conflicts between system paths and straight-install paths, and at least some need to get the system version, or I get serious breakage in some situations.
;; But we still need SOME straight install packages to win, so explicitly promote their straight build dirs to the front so our version wins.
(let ((straight-priority-packages '(compat)))
  (dolist (pkg straight-priority-packages)
    (let ((pkg-dir (concat straight-base-dir "straight/build/" (symbol-name pkg) "/")))
      (when (file-exists-p pkg-dir)
        (setq load-path (cons pkg-dir load-path))))))

(provide 'dotfileswgh-env-conf)
