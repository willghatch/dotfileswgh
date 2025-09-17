;;; -*- lexical-binding: t; -*-
;; Install files with straight.el
(let ((conf-dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "./dotfileswgh-env-conf.el" conf-dir))
  ;;(load-file (expand-file-name "./package-conf.el" conf-dir))
  )

(defun in (package)
  (with-demoted-errors "Error installing: %S" (straight-use-package package)))

(message "Current user-emacs-directory: %S" user-emacs-directory)
(message "Current package-user-dir: %S" package-user-dir)
(message "Installing packages...")

(defun submod (name)
  ;; :type nil to make it not clone it
  `(,name :local-repo ,(concat dotfileswgh (format "external/emacs/%s/" name)) :type nil))

(with-demoted-errors
    "Error: %s"

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
    (straight-use-package (submod 'estate))
    (straight-use-package (submod 'repeatable-motion))
    (straight-use-package (submod 'composiphrase))
    (straight-use-package (submod 'composiphrase-objects))
    (straight-use-package (submod 'composiphrase-demo))

    (straight-use-package (submod 'hydra))
    (straight-use-package (submod 'queue))
    (straight-use-package (submod 'undo-tree))
    ;; The above are the most critical to getting my config running at all.
    )

  (straight-use-package (submod 'sexp-rewrite))
  (straight-use-package (submod 'whitespace-final-newline))
  (straight-use-package (submod 'alternate-region))

;;; Next most important group
  (straight-use-package (submod 'dash))
  (straight-use-package (submod 'smartparens))
  (straight-use-package (submod 'rainbow-delimiters))
  (straight-use-package (submod 'rainbow-identifiers))
  (straight-use-package (submod 'company))
  (straight-use-package (submod 'rainbow-mode))
  (straight-use-package (submod 'xclip))
  (straight-use-package (submod 'magit))
  (straight-use-package (submod 'evil))
  (straight-use-package (submod 'nix-mode))

;;; Tertiary packages
  (in 'rg)
  (in 'systemd)
  (in 'yafolding)
  (in 'smooth-scrolling)
  (in 'git-gutter)
  (in 'blamer) ;; show git blame on line
  ;;(in 'guide-key)
  (in 'which-key)
  ;;(in 'key-chord)

  (in 'wgrep)

  (in 'vertico)
  (in 'marginalia)
  (in 'consult)
  (in 'embark)
  (in 'orderless)
  (in 'corfu)
  (in 'corfu-terminal)
  (in 'cape)


  ;; TODO - try avy as an ace-jump-mode replacement
  ;;(in 'ace-jump-mode)

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
  ;;(in '(kkp :type git :repo "https://github.com/benotn/kkp"))
  (in '(eat :type git :repo "https://codeberg.org/akib/emacs-eat")) ;; "emulate a terminal"

  (in 'helm)
  (in 'helm-lsp)
  (in 'helm-swoop)
  (in 'helm-ag)
  (in 'helm-ag-r)
  (in 'helm-projectile)
  ;;(in 'helm-package)
  ;;(in 'helm-mode-manager)
  (in 'helm-helm-commands)
  ;;(in 'helm-c-yasnippet)
  ;;(in 'helm-company)
  (in 'helm-descbinds)

  (in 'tempel)
  ;;(in 'yasnippet)

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

  (in 'lsp-pyright)

  (in 'gptel)
  (in 'copilot)
  (in 'copilot-chat)
  (in '(aidermacs :type git :repo "https://github.com/MatthewZMD/aidermacs"))
  (in '(aider :type git :repo "https://github.com/tninja/aider.el"))

  (in 'tldr)
  (in 'devdocs) ;; Has very nice documentation formatting in terminal emacs with hyperlinks
  ;;(in 'devdocs-browser) ;; Seems about the same as devdocs...
  (in 'dash-docs) ;; Has some different doc sources than devdocs, though lots of overlap.  This will open in browser, which can be set to 'eww, but which doesn't necessarily work great in eww.
  (in 'counsel-dash) ;; requires dash-docs, provides counsel integration

  ;;;;;;;; trying new packages, or at least looking at some to consider trying in the future.
  (in 'magit-delta) ;; run with magit-delta-mode, adds syntax highlighting to magit diff views, including highlighting the changes within similar lines.
  (in 'avy) ;; jump to char, like ace-jump, or like pentadactyl link highlighting.  I never used it back in the day when I had ace-jump installed, but maybe worth another try.
  (in 'dumb-jump) ;; go to definition heuristically using ripgrep, I hear good things about it.  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate), also can set xref-show-definitions-function for completing-read.
  (in 'symbol-overlay) ;; use symbol-overlay-put to put an overlay on the symbol at point.  Has symbol-overlay-switch-forward/backward to move between highlighted symbols, symbol-overlay-remove-all to remove highlights.  Has some more features, but these look the most interesting.
  ;;(in 'visual-regexp) (in 'visual-regexp-steroids) ;; has vr/replace, vr/query-replace, vr/mc-mark (multiple-cursors), steroids version adds pcre2el/python support for “normal” regexps.  The steroids package also lets you use pcre for isearch.
  ;;(in 'devdocs) ;; uses the devdocs.io documentation website, but views things in editor.  But maybe requires gui emacs?  Unclear from 2 minute look.  But maybe makes looking at other language documentation more like emacs documentation, which is convenient.
  ;; (in 'git-time-machine) ;; use M-x git-time-machine to enter the time machine, then it has bindings to go back/forward between versions, copy the hash, go to revision by commit message, view magit-blame on the old version, etc.
  ;; link-hint ;; provides pentadactyl-like hints for “links”, and it supports many different kinds of “links” including file paths, xref, org agenda items... but does this provide something better than moving point to the thing and then using some act-on-thing-at-point library (maybe embark? maybe hyperbole?) or such?  Eg. this is like a less general avy plus action-on-thing-at-point.  Maybe worth a try, but probably not right now.
  ;; inspector ;; inspect elisp data, some kind of improved data viewer
  ;; vlf ;; for viewing very large files in chunks
  ;; deadgrep ;; another ripgrep frontend, maybe it is better than the rg  package.
  ;; dirvish ;; alternate UI for dired.
  ;; puni ;; alternative for smartparens, hooks in to various modes to give tree motions to siblings and parents (but not down!), doesn't have automatic paren balancing like smartparens.  But its big selling point is that it works better for xml, it seems.
  ;; jinx ;; spell checker, I hear it is good.  It only works on the visible part of the buffer.  Handles camelCase.  Highlights misspelled words.  Requires libenchant dynamic library, needs special handling in NixOS probably, but can be pulled in with jinx from elpa-packages.nix.
  ;; iedit ;; an alternative to multiple-cursors.  Maybe worth trying this and/or mc again.  iedit supports visible rectangles.
  ;; dape ;; dap-mode alternative for Debug Adapter Protocol.
  ;; hyperbole ;; referenced as being like plan9 acme text buttons, or recognizing text patterns in arbitrary butters to become buttons that can do things (eg. arbitrary lisp).

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


  )
