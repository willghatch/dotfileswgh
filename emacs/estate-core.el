;; -*- lexical-binding: t -*-

;; This is basically intended as a modal editing package, like evil-mode, vaguely vim-inspired, except:
;; * All cursor positions are between characters, always.  No cursor-on-character nonsense like vi and its descendants.
;; * I want it to be lighter-weight than evil-mode.
;; * It won't come with bindings out-of-the-box, or at least won't provide them without calling some function.
;; * estate-mode is mostly about having states, and only provides a few helpers for a few concerns that interact with states, eg. visual mode concerns.

;; TODO - clean up and document to be “publishable”, add current state variable and indicator, how to deal with repeating commands (should it be integrated or separate), ...
;; TODO - what vim/evil functionality do I want to replicate?  Marks?  Registers?  I want something that (1) doesn't have the on-character location issue of vim, and (2) is lighter-weight than evil-mode.  What features belong in estate vs in other files?
;; TODO - split into estate-core.el that has the minor mode definition and the infrastructure to define modes, but with no modes defined, and estate-vim-like-states.el that defines the core states that I want and their hooks and such, but with no bindings.  Add maybe another file with function definitions.  Add one more with vim-like bindings as a demo.
;; Notes: each state buffer-local-keymap must be initialized to an actual keymap (instead of nil) before use.

(setq -estate-original-global-map (current-global-map))
(defvar-local estate--estate-mode-map-alist `())
(defvar-local estate--estate-mode-map-local-alist `())

(defvar estate-activate-hook '())
(defvar estate-deactivate-hook '())

(define-minor-mode estate-local-mode
  "TODO docstring here..."
  :init-value nil
  :lighter " estate "
  ;; If this has a keymap value, the keymap is stored in a non-buffer-local variable.
  ;; The solution is to use emulation-mod-map-alist, which can consult a buffer-local variable, and is read before minor-mode maps.
  (if estate-local-mode
      (progn
        (add-to-list 'emulation-mode-map-alists 'estate--estate-mode-map-alist)
        ;; Add local alist after so that the buffer-local keys are earlier in the list.
        (add-to-list 'emulation-mode-map-alists 'estate--estate-mode-map-local-alist)
        (run-hooks 'estate-activate-hook)
        )
    (progn
      (setq emulation-mode-map-alists
            (assoc-delete-all 'estate--estate-mode-map-alist emulation-mode-map-alists))
      (setq emulation-mode-map-alists
            (assoc-delete-all 'estate--estate-mode-map-local-alist emulation-mode-map-alists))
      (run-hooks 'estate-deactivate-hook)
      )))

(defun -estate-mode-initialize ()
  ;; TODO - use a defvar for predicate
  (unless (minibufferp)
    ;; TODO - maybe I should make a minibuffer-specific map instead?
    (estate-local-mode 1)))
(define-globalized-minor-mode estate-mode
  estate-local-mode -estate-mode-initialize)

(defvar estate-state-change-hook '())

(defun estate--keymap-name (state)
  (intern (format "estate-%s-state-keymap" state)))
(defun estate--local-keymap-name (state)
  (intern (format "estate-%s-state-buffer-local-keymap" state)))
(defun estate--enter-hook-name (state)
  (intern (format "estate-%s-state-enter-hook" state)))
(defun estate--leave-hook-name (state)
  (intern (format "estate-%s-state-leave-hook" state)))

(defmacro estate-define-state (state-name parent-keymap)
  "Define a new state with STATE-NAME.
Defines estate-X-keymap, estate-X-state-enter-hook, estate-X-state-leave-hook.
The keymap gets the specified parent."
  (let ((keymap (estate--keymap-name state-name))
        (local-keymap (estate--local-keymap-name state-name))
        (enter-hook (estate--enter-hook-name state-name))
        (leave-hook (estate--leave-hook-name state-name)))
    `(progn
       (defvar ,keymap (make-sparse-keymap)
         ,(format "Keymap for estate %s state." state-name))
       (defvar-local ,local-keymap nil
         ;; Set the default value as nil to force a buffer-local initialization.
         ;; If the default is a keymap, it is too easy to just access and mutate
         ;; that global default keymap instead of mutating an actually local
         ;; keymap.
         ,(format "Buffer-local keymap for estate %s state.  Starts nil, must be initialized (eg. with `make-sparse-keymap') before binding anything." state-name))
       (set-keymap-parent ,keymap ,parent-keymap)
       (defvar ,enter-hook '()
         ,(format "Hook for entering estate %s state." state-name))
       (defvar ,leave-hook '()
         ,(format "Hook for leaving estate %s state." state-name)))))

(defvar-local estate-state nil)
(defvar-local estate--previous-state nil)

;; TODO - this undo grouping is terrible.  I should re-work it.  It should be probably an insert-state hook.
(defun estate-state-activate (state &optional skip-undo-grouping)
  (unless (eq estate-state state)
    (let ((keymap-sym (estate--keymap-name state))
          (local-keymap-sym (estate--local-keymap-name state))
          (enter-hook (estate--enter-hook-name state))
          (leave-hook (and estate-state
                           (estate--leave-hook-name estate-state))))
      (if (boundp keymap-sym)
          (progn
            (setq-local estate--previous-state estate-state)
            (setq-local estate-state state)
            (setq-local estate--estate-mode-map-alist `((estate-local-mode . ,(symbol-value keymap-sym))))
            (setq-local estate--estate-mode-map-local-alist `((estate-local-mode . ,(symbol-value local-keymap-sym))))
            (unless skip-undo-grouping
              (estate-mode-with-change-group-handler))
            (when (symbol-value leave-hook)
              (run-hooks leave-hook))
            (run-hooks 'estate-state-change-hook)
            (when (symbol-value enter-hook)
              (run-hooks enter-hook)))
        (error "bad estate state: %s" state)))))


(provide 'estate-core)
