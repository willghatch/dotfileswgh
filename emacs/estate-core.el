;; -*- lexical-binding: t -*-

;; This is basically intended as a modal editing package, like evil-mode, vaguely vim-inspired, except:
;; * All cursor positions are between characters, always.  No cursor-on-character nonsense like vi and its descendants.
;; * I want it to be lighter-weight than evil-mode.
;; * It won't come with bindings out-of-the-box, or at least won't provide them without calling some function.
;; * estate-mode is mostly about having states, and only provides a few helpers for a few concerns that interact with states, eg. visual mode concerns.

;; TODO - clean up and document to be “publishable”, add current state variable and indicator, how to deal with repeating commands (should it be integrated or separate), ...
;; TODO - what vim/evil functionality do I want to replicate?  Marks?  Registers?  I want something that (1) doesn't have the on-character location issue of vim, and (2) is lighter-weight than evil-mode.  What features belong in estate vs in other files?
;; TODO - split into estate-core.el that has the minor mode definition and the infrastructure to define modes, but with no modes defined, and estate-vim-like-states.el that defines the core states that I want and their hooks and such, but with no bindings.  Add maybe another file with function definitions.  Add one more with vim-like bindings as a demo.

(setq -estate-original-global-map (current-global-map))
(defvar-local -estate-mode-current-keymap (make-sparse-keymap))
(defvar-local -estate-mode-map-alist `((estate-local-mode . ,-estate-mode-current-keymap)))

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
        (setq-local -estate-mode-current-keymap (make-sparse-keymap))
        (add-to-list 'emulation-mode-map-alists '-estate-mode-map-alist)
        (run-hooks 'estate-activate-hook)
        )
    (progn
      (run-hooks 'estate-activate-hook)
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
  (intern (format "estate-%s-keymap" state)))
(defun estate--enter-hook-name (state)
  (intern (format "estate-%s-state-enter-hook" state)))
(defun estate--leave-hook-name (state)
  (intern (format "estate-%s-state-leave-hook" state)))

(defmacro estate-define-state (state-name parent-keymap)
  "Define a new state with STATE-NAME.
Defines estate-X-keymap, estate-X-state-enter-hook, estate-X-state-leave-hook.
The keymap gets the specified parent."
  (let ((keymap (estate--keymap-name state-name))
        (enter-hook (estate--enter-hook-name state-name))
        (leave-hook (estate--leave-hook-name state-name)))
    `(progn
       (defvar ,keymap (make-sparse-keymap))
       (set-keymap-parent ,keymap ,parent-keymap)
       (defvar ,enter-hook '())
       (defvar ,leave-hook '()))))

(defvar-local estate-state nil)
(defvar-local estate--previous-state nil)

;; TODO - this undo grouping is terrible.  I should re-work it.  It should be probably an insert-state hook.
(defun estate-state-activate (state &optional skip-undo-grouping)
  (unless (eq estate-state state)
    (let ((keymap-sym (estate--keymap-name state))
          (enter-hook (estate--enter-hook-name state))
          (leave-hook (and estate--previous-state
                           (estate--leave-hook-name estate--previous-state))))
      (if (boundp keymap-sym)
          (progn
            (setq-local estate--previous-state estate-state)
            (setq-local estate-state state)

            (setq-local -estate-mode-map-alist `((estate-local-mode . ,(symbol-value keymap-sym))))
            ;(set-keymap-parent -estate-mode-current-keymap (symbol-value keymap-sym))
            (unless skip-undo-grouping
              (estate-mode-with-change-group-handler))
            (when (symbol-value leave-hook)
              (run-hooks leave-hook))
            (run-hooks 'estate-state-change-hook)
            (when (symbol-value enter-hook)
              (run-hooks enter-hook)))
        (error "bad estate state: %s" state)))))


(provide 'estate-core)
