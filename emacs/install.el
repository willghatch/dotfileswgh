;;; -*- lexical-binding: t; -*-
;; Install/manage packages with straight.el

;;; Parse command-line arguments passed after --
;;; Consume them immediately so Emacs doesn't complain about unknown args.
(let ((args command-line-args-left))
  (setq command-line-args-left nil)
  (when (and args (string= (car args) "--"))
    (setq args (cdr args)))
  (defvar install--mode (or (car args) "install"))
  (defvar install--packages (cdr args)))

(let ((conf-dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "./dotfileswgh-env-conf.el" conf-dir)))

(defun in (package)
  (with-demoted-errors "Error installing: %S"
    (straight-use-package package)))

(message "Current user-emacs-directory: %S" user-emacs-directory)
(message "Current package-user-dir: %S" package-user-dir)

(defun submod (name)
  ;; :type nil to make it not clone it
  `(,name :local-repo ,(concat dotfileswgh (format "external/emacs/%s/" name)) :type nil))

(defun install--register-packages ()
  "Register and build all packages with straight.
Populates straight's internal caches and installs transitive dependencies."

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

  (in 'compat)
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

  ;;(in 'gptel)
  ;;(in 'copilot)
  ;;(in 'copilot-chat)

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
  ;;(in 'visual-regexp) (in 'visual-regexp-steroids) ;; has vr/replace, vr/query-replace, vr/mc-mark (multiple-cursors), steroids version adds pcre2el/python support for "normal" regexps.  The steroids package also lets you use pcre for isearch.
  ;;(in 'devdocs) ;; uses the devdocs.io documentation website, but views things in editor.  But maybe requires gui emacs?  Unclear from 2 minute look.  But maybe makes looking at other language documentation more like emacs documentation, which is convenient.
  ;; (in 'git-time-machine) ;; use M-x git-time-machine to enter the time machine, then it has bindings to go back/forward between versions, copy the hash, go to revision by commit message, view magit-blame on the old version, etc.
  ;; link-hint ;; provides pentadactyl-like hints for "links", and it supports many different kinds of "links" including file paths, xref, org agenda items... but does this provide something better than moving point to the thing and then using some act-on-thing-at-point library (maybe embark? maybe hyperbole?) or such?  Eg. this is like a less general avy plus action-on-thing-at-point.  Maybe worth a try, but probably not right now.
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
  )

(defun install--repo-versions ()
  "Return a sorted alist of (REPO . COMMIT) for every cloned straight repo.
Scans the repos directory directly, so transitive dependencies are
included regardless of straight's profile cache."
  (cl-sort
   (delq nil
         (mapcar (lambda (repo)
                   (when (file-directory-p
                          (expand-file-name ".git" (straight--repos-dir repo)))
                     (when-let ((commit (straight-vc-get-commit 'git repo)))
                       (cons repo commit))))
                 (directory-files (straight--repos-dir) nil "^[^.]")))
   #'string-lessp :key #'car))

(defun install--read-lockfile ()
  "Read the existing straight lockfile and return an alist, or nil if absent."
  (let ((path (straight--versions-file (cdr (car straight-profiles)))))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (condition-case nil
            (read (current-buffer))
          (error nil))))))

(defun install--write-lockfile (versions-alist)
  "Write VERSIONS-ALIST to the straight lockfile."
  (let ((path (straight--versions-file (cdr (car straight-profiles)))))
    (make-directory (file-name-directory path) 'parents)
    (with-temp-file path
      (insert (format "(%s)\n:gamma\n"
                      (mapconcat (apply-partially #'format "%S")
                                 versions-alist
                                 "\n "))))
    (message "Wrote %s" path)))

(defun install--freeze-merge ()
  "Merge current repo HEAD commits into the lockfile.
New commits overwrite old entries; absent repos are kept."
  (let* ((new-versions (install--repo-versions))
         (old-versions (or (install--read-lockfile) nil))
         (merged (let ((result (copy-alist old-versions)))
                   (dolist (entry new-versions)
                     (let ((cell (assoc (car entry) result)))
                       (if cell
                           (setcdr cell (cdr entry))
                         (push entry result))))
                   (cl-sort result #'string-lessp :key #'car))))
    (install--write-lockfile merged)))

(defun install--quarantine-repo (pkg-name commit)
  "Move PKG-NAME's repo out of the straight repos dir into repos-quarantined/.
The build dir symlinks become dangling, preventing the package from loading.
The repo content is preserved for inspection."
  (let* ((repo-dir (straight--repos-dir pkg-name))
         (quarantine-base (expand-file-name
                           "repos-quarantined"
                           (file-name-directory (directory-file-name
                                                 (file-name-directory repo-dir)))))
         (quarantine-dir (expand-file-name pkg-name quarantine-base)))
    (make-directory quarantine-base t)
    (when (file-exists-p quarantine-dir)
      (delete-directory quarantine-dir t))
    (rename-file repo-dir quarantine-dir)
    (message (concat "ERROR: Pinned commit %.8s for package %s could not be obtained.\n"
                     "  The repo has been moved to %s.\n"
                     "  Inspect the repo, resolve the issue, and re-run the installer.")
             commit pkg-name quarantine-dir)))

(defun install--ensure-pinned-commits ()
  "Fetch pinned commits that are missing from shallow repos.
For each lockfile entry, if the repo is a shallow clone and does not
already contain the pinned commit, fetches that commit at depth 1.
Falls back to unshallowing if the targeted fetch fails.
If the pinned commit still cannot be obtained, quarantines the repo by
moving it to repos-quarantined/, leaving build/ symlinks dangling so the
package cannot load."
  (let ((lockfile-versions (install--read-lockfile)))
    (dolist (entry lockfile-versions)
      (let* ((pkg-name (car entry))
             (commit (cdr entry))
             (repo-dir (straight--repos-dir pkg-name))
             (shallow-file (expand-file-name ".git/shallow" repo-dir)))
        (when (and (file-directory-p repo-dir)
                   (file-exists-p shallow-file))
          (let ((default-directory repo-dir))
            (unless (= 0 (call-process "git" nil nil nil "cat-file" "-t" commit))
              (message "Fetching pinned commit %.8s for %s..." commit pkg-name)
              (unless (= 0 (call-process "git" nil nil nil
                                         "fetch" "--depth=1" "origin" commit))
                (message "Targeted fetch failed for %s, falling back to unshallow..." pkg-name)
                (install--unshallow-if-needed pkg-name))
              ;; Verify the commit is now present; quarantine if not.
              (unless (= 0 (call-process "git" nil nil nil "cat-file" "-t" commit))
                (install--quarantine-repo pkg-name commit)))))))))

(defun install--unshallow-if-needed (pkg-name)
  "If the repo for PKG-NAME is a shallow clone, fetch --unshallow it.
PKG-NAME is a string matching the straight repo directory name."
  (let* ((repo-dir (straight--repos-dir pkg-name))
         (shallow-file (expand-file-name ".git/shallow" repo-dir)))
    (when (file-exists-p shallow-file)
      (message "Unshallowing %s..." pkg-name)
      (let* ((default-directory repo-dir)
             (exit-code (call-process "git" nil nil nil "fetch" "--unshallow")))
        (if (= exit-code 0)
            (message "Unshallowed %s." pkg-name)
          (message "Warning: failed to unshallow %s (exit code %d)"
                   pkg-name exit-code))))))

(defun install--update-repo (pkg-name)
  "Update PKG-NAME's repo to the latest commit on origin's default branch.
Unshallows first if shallow.  Uses origin/HEAD so no tracking config is needed."
  (let ((default-directory (straight--repos-dir pkg-name)))
    (install--unshallow-if-needed pkg-name)
    ;; Resolve origin/HEAD so we can reference it without a tracking branch
    (call-process "git" nil nil nil "remote" "set-head" "origin" "--auto")
    (let ((fetch-exit (call-process "git" nil nil nil "fetch" "origin")))
      (if (not (= fetch-exit 0))
          (message "Warning: fetch failed for %s (exit %d)" pkg-name fetch-exit)
        (let ((checkout-exit (call-process "git" nil nil nil
                                           "checkout" "--detach" "origin/HEAD")))
          (if (= checkout-exit 0)
              (message "Updated %s." pkg-name)
            (message "Warning: checkout --detach origin/HEAD failed for %s (exit %d)"
                     pkg-name checkout-exit)))))))

(defun install--do-install ()
  "Register and install all packages."
  (with-demoted-errors
      "Error: %s"

    (install--ensure-pinned-commits)
    (straight-thaw-versions)
    (install--register-packages)

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

    (install--ensure-pinned-commits)
    (straight-thaw-versions)))

(let ((bootstrap-file
       (concat dotfileswgh "external/emacs/straight.el/bootstrap.el"))
      (bootstrap-version 7))
  (load bootstrap-file nil 'nomessage))

(pcase install--mode

  ("install"
   (message "Installing packages...")
   (install--do-install))

  ("full-clone"
   (message "Installing packages with full (non-shallow) git clones...")
   (setq straight-vc-git-default-clone-depth 'full)
   (install--do-install))

  ("freeze"
   ;; Default: merge new hashes into the existing lockfile.  Entries not
   ;; present in the current repos are kept so that previously-trusted
   ;; versions are not silently discarded.  New commits overwrite old ones
   ;; for repos that are still cloned.
   (message "Freezing package versions to lockfile (add-only)...")
   (install--freeze-merge)
   (message "Done."))

  ("freeze-remove-unused"
   ;; Write only currently-cloned repos, removing any lockfile entries
   ;; for repos that are no longer present.
   (message "Freezing package versions to lockfile (replacing)...")
   (install--write-lockfile (install--repo-versions))
   (message "Done."))

  ("print-hashes"
   (if (null install--packages)
       (error "No packages specified for --print-hashes")
     (let ((repo-commits (install--repo-versions)))
       (dolist (pkg install--packages)
         (if-let ((commit (cdr (assoc pkg repo-commits))))
             (message "%s: %s" pkg commit)
           (message "%s: not found (repo dir may differ from package name)" pkg))))))

  ("update-packages"
   (if (null install--packages)
       (error "No packages specified for --straight-update-packages")
     (dolist (pkg install--packages)
       (message "Updating package: %s" pkg)
       (install--update-repo pkg))
     (message "Done.")))

  (_
   (error "Unknown mode: %s" install--mode)))
