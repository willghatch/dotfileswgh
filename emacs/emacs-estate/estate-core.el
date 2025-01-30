;; -*- lexical-binding: t -*-

;; This is basically intended as a modal editing package, like evil-mode, vaguely vim-inspired.
;; Some notes:
;; * estate-mode just provides modal editing (or stateful editing since emacs uses the word mode for something else).
;; * estate-core just provides the infrastructure for defining states.  You can define as many states as you want, but if a state should represent anything more than a set of key bindings, you must write the code to support that.
;; * Each estate state comes with hooks, so you can run code on entering or leaving any particular state.
;; * No part of estate provides support for vim-style on-character cursor positioning, which I consider to be a bad feature.  All cursor positions are between characters, always.
;; * It won't come with bindings out-of-the-box.  Estate can be used as a DIY modal editing package, or can be used in combination with other packages that may provide key bindings.
;; * I want it to be lighter-weight than evil-mode.  But I may expand features as I find I need more.  The evil-core.el file already feels too big and complicated to me.

;; TODO - clean up and document to be “publishable”, add current state variable and indicator, how to deal with repeating commands (should it be integrated or separate), ...

;; Notes: each state buffer-local-keymap must be initialized to an actual keymap (instead of nil) before use.

(defvar-local estate--estate-mode-map-alist `())
(defvar-local estate--estate-mode-map-local-alist `())

(defvar estate-activate-hook '()
  "Hook used when estate-mode is first activated in a buffer.")
(defvar estate-deactivate-hook '()
  "Hook used when estate-mode is deactivated in a buffer.")

(defvar estate-set-initial-state-function nil
  "Function called when activating estate-mode to set the initial state for the buffer.
Should probably check things like 'major-mode' variable or `minibufferp', etc.
By default this is unset with just estate-core, so you had better set it if you want to do a DIY modal editing setup.
The estate-vim-like-states file will set it if it has not been set yet, to something that puts every buffer into normal state unless it is a minibuffer.
You probably want more fine grained control over this, so you should write such a function yourself, probably.")

(define-minor-mode estate-local-mode
  "Buffer-local mode for estate-mode."
  :init-value nil
  :lighter " estate "
  ;; If a minor mode has a keymap value, the keymap is stored in a non-buffer-local variable.
  ;; The solution is to use emulation-mod-map-alist, which can consult a buffer-local variable, and is read before minor-mode maps.
  ;; This allows estate-mode to have a buffer-local state, as well as have a buffer-local keymap for each state in each buffer.
  (if estate-local-mode
      (progn
        (add-to-list 'emulation-mode-map-alists 'estate--estate-mode-map-alist)
        ;; Add local alist after so that the buffer-local keys are earlier in the list.
        (add-to-list 'emulation-mode-map-alists 'estate--estate-mode-map-local-alist)
        (when estate-set-initial-state-function
          (funcall estate-set-initial-state-function))
        (run-hooks 'estate-activate-hook)
        )
    (progn
      (setq emulation-mode-map-alists
            (assoc-delete-all 'estate--estate-mode-map-alist emulation-mode-map-alists))
      (setq emulation-mode-map-alists
            (assoc-delete-all 'estate--estate-mode-map-local-alist emulation-mode-map-alists))
      (run-hooks 'estate-deactivate-hook)
      )))

(defvar estate-mode-activate-predicate nil
  "Predicate for whether to turn on estate-mode in a buffer.
Can be nil to always enable, or a function that returns non-nil when the mode should be enabled.
The function takes no arguments, and probably should inspect the 'major-mode' variable or `minibufferp' or such.")

(defun estate-mode--initialize ()
  (when (or (not estate-mode-activate-predicate)
            (funcall estate-mode-activate-predicate))
    (estate-local-mode 1)))

(define-globalized-minor-mode estate-mode
  estate-local-mode estate-mode--initialize)

(defvar estate-state-change-hook '()
  "Hook called for every estate state change.
Called after the exit-hook of the previous state, but before the enter-hook of the new state.
The previous state is already set in 'estate-previous-state', and the new state is already set in 'estate-state'.")

(defun estate--keymap-name (state)
  (intern (format "estate-%s-state-keymap" state)))
(defun estate--local-keymap-name (state)
  (intern (format "estate-%s-state-buffer-local-keymap" state)))
(defun estate--enter-hook-name (state)
  (intern (format "estate-%s-state-enter-hook" state)))
(defun estate--exit-hook-name (state)
  (intern (format "estate-%s-state-exit-hook" state)))

(defmacro estate-define-state (state-name parent-keymap)
  "Define a new state with STATE-NAME.
STATE-NAME should be a symbol.
Defines estate-X-keymap, estate-X-buffer-local-keymap, estate-X-state-enter-hook, estate-X-state-exit-hook.
The keymap gets the specified parent.
The buffer-local-keymap has default value of nil, and so must be initialized to a keymap (eg. with `make-sparse-keymap') before first use in each buffer.
States can be switched with `estate-activate-state' using STATE-NAME.
"
  (let ((keymap (estate--keymap-name state-name))
        (local-keymap (estate--local-keymap-name state-name))
        (enter-hook (estate--enter-hook-name state-name))
        (exit-hook (estate--exit-hook-name state-name)))
    `(progn
       (define-prefix-command ',keymap)
       (defvar-local ,local-keymap nil
         ;; Set the default value as nil to force a buffer-local initialization.
         ;; If the default is a keymap, it is too easy to just access and mutate
         ;; that global default keymap instead of mutating an actually local
         ;; keymap.
         ,(format "Buffer-local keymap for estate %s state.  Starts nil, must be initialized (eg. with `make-sparse-keymap') before binding anything." state-name))
       (set-keymap-parent ,keymap ,parent-keymap)
       (defvar ,enter-hook '()
         ,(format "Hook for entering estate %s state.
Called after the exit-hook for the previous state and after the generic 'estate-state-change-hook'.
The variables 'estate-previous-state' and 'estate-state' are already set, so 'estate-state' is the new state."
                  state-name))
       (defvar ,exit-hook '()
         ,(format "Hook for exiting estate %s state.
Called before the enter-hook for the new state and before the generic 'estate-state-change-hook'.
The variables 'estate-previous-state' and 'estate-state' are already set, so 'estate-previous-state' is the state that the hook is running for."
                  state-name)))))

(defvar-local estate-state nil
  "The current estate-mode state.
Don't manually set this variable, use `estate-activate-state' instead if you want to change states.")
(defvar-local estate-previous-state nil
  "The previous estate-mode state.
Do not manually set this variable.")


(defun estate-activate-state (state)
  "Activate the estate state STATE, which should be a symbol."
  (unless (eq estate-state state)
    (let ((keymap-sym (estate--keymap-name state))
          (local-keymap-sym (estate--local-keymap-name state))
          (enter-hook (estate--enter-hook-name state))
          (exit-hook (and estate-state
                           (estate--exit-hook-name estate-state))))
      (if (boundp keymap-sym)
          (progn
            (setq-local estate-previous-state estate-state)
            (setq-local estate-state state)
            (setq-local estate--estate-mode-map-alist `((estate-local-mode . ,(symbol-value keymap-sym))))
            (setq-local estate--estate-mode-map-local-alist `((estate-local-mode . ,(symbol-value local-keymap-sym))))
            (when (symbol-value exit-hook)
              (run-hooks exit-hook))
            (run-hooks 'estate-state-change-hook)
            (when (symbol-value enter-hook)
              (run-hooks enter-hook)))
        (error "bad estate state: %s" state)))))


(provide 'estate-core)
