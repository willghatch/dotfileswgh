;;; -*- lexical-binding: t; -*-


;; Use the demo repo dir as the user directory.
(setq user-emacs-directory (file-name-directory load-file-name))

;;; straight.el bootstrap code, from the straight.el readme file:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Install packages with straight.el
;; Install third-party packages from Melpa, I guess.
(straight-use-package 'undo-tree) ;; for simpler undo interface
;; Hydra implements key map that sticks around until one of the bindings exits,
;; for object select map.
(straight-use-package 'hydra)
(straight-use-package 'smartparens) ;; for core demo motions
(straight-use-package 'evil) ;; for evil-ex
(straight-use-package
 '(estate
   :type git :host nil
   :repo "https://github.com/willghatch/emacs-estate"))
(straight-use-package
 '(repeatable-motion
   :type git :host nil
   :repo "https://github.com/willghatch/emacs-repeatable-motion"))
(straight-use-package
 '(composiphrase
   :type git :host nil
   :repo "https://github.com/willghatch/emacs-composiphrase"))
(straight-use-package
 '(composiphrase-objects
   :type git :host nil
   :repo "https://github.com/willghatch/emacs-composiphrase-objects"))
