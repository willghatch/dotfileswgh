;; TODO - switch to a package management system that lets me pin versions.  Ideally I'd really like to write git repository paths for each of these and be able to pin versions inline as well as have a lock file.

(load-file "~/dotfileswgh/emacs/package-conf.el")


(defun file-to-string (file-path)
  (with-temp-buffer (insert-file-contents file-path) (buffer-string)))
(defun hash-file (file-path secure-hash-name)
  (secure-hash secure-hash-name (file-to-string file-path)))

(defun download-checked (url local-path secure-hash-name hash-value)
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
      (check-hash)
    (progn
      (url-copy-file url local-path)
      (check-hash))))

(defun install-file-checked (path-in-install-dir url hash-name hash-value)
  "Specialization of downoad-checked to be more convenient to write in this file."
  (let ((install-dir (concat wgh/local-emacs.d-path "single-files/")))
    (make-directory install-dir t)
    (download-checked url (concat install-dir path-in-install-dir)
                      hash-name hash-value)))

(package-initialize)
(message "Updating package lists...")
(package-refresh-contents)

(defun in (package)
  (with-demoted-errors "Error installing: %S" (package-install package)))

(message "Installing packages...")

(in 'evil)
(in 'repeatable-motion)
(in 'on-parens)
(in 'key-chord)
(in 'undo-tree)
(in 'hydra)
(in 'auto-compile)
(in 'evil-surround)
(in 'evil-search-highlight-persist)
;; TODO - eventually targets.el (https://github.com/noctuid/targets.el) should
;; replace evil-textobj-anyblock.  But it's not ready yet, apparently.
(in 'evil-textobj-anyblock)
(in 'evil-cleverparens)
(in 'evil-terminal-cursor-changer)
(in 'evil-anzu)
(in 'yafolding)
(in 'company)

(in 'evil-args)
(in 'rainbow-delimiters)
(in 'rainbow-identifiers)
(in 'rainbow-mode) ; for #123456 colors
(in 'smartparens)
(in 'ace-jump-mode)
(in 'markchars)

(in 'elscreen)
(in 'xclip)
(in 'ag)
(in 'smex)
(in 'flycheck)
(in 'fill-column-indicator)
(in 'highlight-chars)
(in 'hlinum)
(in 'elisp-slime-nav)
;(in 'projectile)
(in 'flx-ido)
(in 'expand-region)
;(in 'popwin)
;(in 'linum-relative)
(in 'smooth-scrolling)
(in 'guide-key)
(in 'magit)

(in 'helm)
(in 'helm-lsp)
(in 'helm-swoop)
(in 'helm-ag)
(in 'helm-ag-r)
(in 'helm-projectile)
;(in 'helm-package)
(in 'helm-mode-manager)
(in 'helm-helm-commands)
;(in 'helm-c-yasnippet)
;(in 'helm-company)
(in 'helm-descbinds)

;; Let's try ivy and friends
;; On second thought, ivy requires that the snippets you type are in order, which is usually not what I want.  So I prefer Helm's functionality.
;(in 'ivy)
;(in 'swiper)
;(in 'counsel)


(in 'js2-mode)
(in 'js2-refactor)
(in 'typescript-mode)
;; prettier code formatter integration
(in 'prettier)
(in 'haskell-mode)
(in 'lsp-haskell)
(in 'lsp-ui)
(in 'racket-mode)
(in 'scribble-mode)
(in 'clojure-mode)
(in 'markdown-mode)
(in 'slime)
(in 'lua-mode)
;(in 'vimrc-mode)
(in 'anaconda-mode)
(in 'company-anaconda)
(in 'web-mode)
(in 'indent-guide)
(in 'hl-todo)
(in 'git-commit)
(in 'systemd)
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

;; 

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

(message "Done installing packages!")

