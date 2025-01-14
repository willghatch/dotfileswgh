;; Install files with straight.el
(let ((conf-dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "./env-conf.el" conf-dir))
  ;;(load-file (expand-file-name "./package-conf.el" conf-dir))
  )

(defun in (package)
  (with-demoted-errors "Error installing: %S" (straight-use-package package)))

(message "Current user-emacs-directory: %S" user-emacs-directory)
(message "Current package-user-dir: %S" package-user-dir)
(message "Installing packages...")


(progn
  (let ((bootstrap-file
         (concat dotfileswgh "external/emacs/straight.el/bootstrap.el"))
        (bootstrap-version 7))
    (load bootstrap-file nil 'nomessage))

  (straight-thaw-versions)

  ;;; Load core packages most critical to my config running at all.
  ;;; These are now dotfileswgh submodules
  ;;(straight-use-package 'repeatable-motion)
  ;;(straight-use-package 'hydra)
  ;;(straight-use-package 'undo-tree) ;; NOTE: this depends on the queue package, which is resolved by straight, but I want to remember why I have the queue package as a submodule now
  ;; The above are the most critical to getting my config running at all.
  )

;;; Next most important group
;;; These are also now submodules
;;(in 'smartparens)
;;(in 'rainbow-delimiters)
;;(in 'rainbow-identifiers)
;;(in 'company)
;;(in 'rainbow-mode) ; for #123456 colors
;;(in 'xclip)
;;(in 'magit)
;;(in 'evil)
;;(in 'systemd)
;;(in 'nix-mode)

(in 'yafolding)
(in 'smooth-scrolling)
(in 'git-gutter)
(in 'blamer) ;; show git blame on line
;;(in 'guide-key)
;;(in 'key-chord)



(in 'ace-jump-mode)
(in 'markchars)

(in 'elscreen)
(in 'ag)
(in 'smex)
(in 'flycheck)
(in 'highlight-chars)
(in 'elisp-slime-nav)
;;(in 'projectile)
(in 'flx-ido)
(in 'expand-region)
;;(in 'popwin)
;;(in 'linum-relative)

(in 'helm)
(in 'helm-lsp)
(in 'helm-swoop)
(in 'helm-ag)
(in 'helm-ag-r)
(in 'helm-projectile)
;;(in 'helm-package)
(in 'helm-mode-manager)
(in 'helm-helm-commands)
;;(in 'helm-c-yasnippet)
;;(in 'helm-company)
(in 'helm-descbinds)

;; Let's try ivy and friends
;; On second thought, ivy requires that the snippets you type are in order, which is usually not what I want.  So I prefer Helm's functionality.
;;(in 'ivy)
;;(in 'swiper)
;;(in 'counsel)


(in 'hlinum)
(in 'auto-compile)
(in 'js2-mode)
(in 'js2-refactor)
(in 'typescript-mode)
;; prettier code formatter integration
(in 'prettier)
(in 'haskell-mode)
(in 'lsp-haskell)
(in 'lsp-ui)
(in 'dap-mode)
(in 'all-the-icons) ;; used by dap-mode...
(in 'eglot)
(in 'racket-mode)
(in 'scribble-mode)
(in 'clojure-mode)
(in 'markdown-mode)
(in 'slime)
(in 'lua-mode)
;;(in 'vimrc-mode)
(in 'anaconda-mode)
(in 'company-anaconda)
(in 'web-mode)
;;(in 'indent-guide)
(in 'highlight-indent-guides)
;;(in 'hl-todo)
;;(in 'git-commit)
(in 'lsp-java)
(in 'solidity-mode)

(in 'rust-mode)
;; Rustic is recommended as being better, but also has more dependencies and I don't want to sort it out right now, I really just want syntax highlighting.
;;(in 'rustic)

(in 'keyfreq)

(in 'fzf)

(in 'scad-mode)

;; some other packages I don't currently use, but might want to later:

;; figlet -- make ascii banners

;; Link programs -- to give pentadactyl/vimium style hints to links on page
;; The most important thing for links is that I can define new ones and new ways of handling them.
;; Getting to the links I can use any movement command in emacs, so moving to the link is less of
;; an issue than recognizing and handling the link.
;;
;; Ace link doesn't support many modes (eg. it doesn't work in fundamental or markdown mode)
;; (in ace-link) ; -- ace jump to navigation links in info, help, eww, or compilation mode
;; link-hint -- I'm not sure there's a good way of defining new link types (eg. define a regex and a handler function).
;;              Also, it auto-follows a link if there is only one, which I don't like (may be configurable).
;; (in 'link-hint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Single files to install without straight.el.

(defun file-to-string (file-path)
  (with-temp-buffer (insert-file-contents file-path) (buffer-string)))
(defun hash-file (file-path secure-hash-name)
  (secure-hash secure-hash-name (file-to-string file-path)))

(defun download-checked (url local-path secure-hash-name hash-value check-if-exists-p)
  "Convenince function for installing a single file from a URL and
checking that we get the right file.  Useful when you want a single
file from a large repository."
  (defun check-hash ()
    (let ((actual-hash-value (hash-file local-path secure-hash-name)))
      (when (not (equal actual-hash-value
                        hash-value))
        (error "Installing hash mismatch for %s, expected %s, got %s"
               local-path hash-value actual-hash-value))))
  (if (file-exists-p local-path)
      (when check-if-exists-p
        (check-hash))
    (progn
      (url-copy-file url local-path)
      (check-hash))))

(defun install-file-checked (path-in-install-dir url hash-name hash-value)
  "Specialization of downoad-checked to be more convenient to write in this file."
  (let ((install-dir (concat local-emacs.d-path "single-files/")))
    (make-directory install-dir t)
    (download-checked url (concat install-dir path-in-install-dir)
                      hash-name
                      hash-value
                      'check-if-exists-p)))

(install-file-checked "tablegen-mode.el"
                      "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-16.0.6/llvm/utils/emacs/tablegen-mode.el"
                      'sha256
                      "2c8b17f091a23572deb09649323b21bd5d870d48cdc1fd14a03891958d5b2bce")
(install-file-checked "llvm-mode.el"
                      "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-16.0.6/llvm/utils/emacs/llvm-mode.el"
                      'sha256
                      "9d66cbae84e9868eaef841462d0f9faf877c5380b196d4414d048f69ae03cb38")
(install-file-checked "mlir-mode.el"
                      "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-16.0.6/mlir/utils/emacs/mlir-mode.el"
                      'sha256
                      "598a1b7fb3a9e23682ca120e30392a68ecb4486599d5cd8f239d3167bc29b5b6")
(install-file-checked "mlir-lsp-client.el"
                      "https://raw.githubusercontent.com/llvm/llvm-project/llvmorg-16.0.6/mlir/utils/emacs/mlir-lsp-client.el"
                      'sha256
                      "37761e19d08895298bbb04882a9d9810a718a2fb776e1ab4ef85877bf0886762")

(straight-thaw-versions)
