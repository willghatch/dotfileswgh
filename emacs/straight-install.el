;; Install files with straight.el

(straight-use-package 'evil)
(straight-use-package 'on-parens)
(straight-use-package 'key-chord)
(straight-use-package 'auto-compile)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-search-highlight-persist)
;; TODO - eventually targets.el (https://github.com/noctuid/targets.el) should
;; replace evil-textobj-anyblock.  But it's not ready yet, apparently.
(straight-use-package 'evil-textobj-anyblock)
(straight-use-package 'evil-cleverparens)
(straight-use-package 'evil-terminal-cursor-changer)
(straight-use-package 'evil-anzu)
(straight-use-package 'yafolding)
(straight-use-package 'company)

(straight-use-package 'evil-args)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'rainbow-identifiers)
(straight-use-package 'rainbow-mode) ; for #123456 colors
(straight-use-package 'ace-jump-mode)
(straight-use-package 'markchars)

(straight-use-package 'elscreen)
(straight-use-package 'xclip)
(straight-use-package 'ag)
(straight-use-package 'smex)
(straight-use-package 'flycheck)
(straight-use-package 'highlight-chars)
(straight-use-package 'hlinum)
(straight-use-package 'elisp-slime-nav)
;(straight-use-package 'projectile)
(straight-use-package 'flx-ido)
(straight-use-package 'expand-region)
;(straight-use-package 'popwin)
;(straight-use-package 'linum-relative)
(straight-use-package 'smooth-scrolling)
(straight-use-package 'guide-key)
(straight-use-package 'magit)
(straight-use-package 'git-gutter)
(straight-use-package 'blamer) ;; show git blame on line

(straight-use-package 'helm)
(straight-use-package 'helm-lsp)
(straight-use-package 'helm-swoop)
(straight-use-package 'helm-ag)
(straight-use-package 'helm-ag-r)
(straight-use-package 'helm-projectile)
;(straight-use-package 'helm-package)
(straight-use-package 'helm-mode-manager)
(straight-use-package 'helm-helm-commands)
;(straight-use-package 'helm-c-yasnippet)
;(straight-use-package 'helm-company)
(straight-use-package 'helm-descbinds)

;; Let's try ivy and friends
;; On second thought, ivy requires that the snippets you type are in order, which is usually not what I want.  So I prefer Helm's functionality.
;(straight-use-package 'ivy)
;(straight-use-package 'swiper)
;(straight-use-package 'counsel)


(straight-use-package 'js2-mode)
(straight-use-package 'js2-refactor)
(straight-use-package 'typescript-mode)
;; prettier code formatter integration
(straight-use-package 'prettier)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'lsp-ui)
(straight-use-package 'dap-mode)
(straight-use-package 'all-the-icons) ;; used by dap-mode...
(straight-use-package 'eglot)
(straight-use-package 'racket-mode)
(straight-use-package 'scribble-mode)
(straight-use-package 'clojure-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'slime)
(straight-use-package 'lua-mode)
;(straight-use-package 'vimrc-mode)
(straight-use-package 'anaconda-mode)
(straight-use-package 'company-anaconda)
(straight-use-package 'web-mode)
;(straight-use-package 'indent-guide)
(straight-use-package 'highlight-indent-guides)
;(straight-use-package 'hl-todo)
;(straight-use-package 'git-commit)
(straight-use-package 'systemd)
(straight-use-package 'lsp-java)
(straight-use-package 'solidity-mode)

(straight-use-package 'rust-mode)
;; Rustic is recommended as being better, but also has more dependencies and I don't want to sort it out right now, I really just want syntax highlighting.
;;(straight-use-package 'rustic)

(straight-use-package 'keyfreq)

(straight-use-package 'fzf)

(straight-use-package 'scad-mode)

;; some other packages I don't currently use, but might want to later:

;; figlet -- make ascii banners

;; Link programs -- to give pentadactyl/vimium style hints to links on page
;; The most important thing for links is that I can define new ones and new ways of handling them.
;; Getting to the links I can use any movement command in emacs, so moving to the link is less of
;; an issue than recognizing and handling the link.
;;
;; Ace link doesn't support many modes (eg. it doesn't work in fundamental or markdown mode)
;; (straight-use-package ace-link) ; -- ace jump to navigation links in info, help, eww, or compilation mode
;; link-hint -- I'm not sure there's a good way of defining new link types (eg. define a regex and a handler function).
;;              Also, it auto-follows a link if there is only one, which I don't like (may be configurable).
;; (straight-use-package 'link-hint)

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
                      hash-name hash-value nil)))

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

