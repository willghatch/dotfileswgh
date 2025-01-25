;;; -*- lexical-binding: t; -*-

(defun lsp-common-setup ()
  (require 'lsp)
  (setq lsp-headerline-breadcrumb-enable nil)
  (require 'lsp-ui)
  (require 'lsp-lens)
  (require 'lsp-modeline)

  ;; Unbind <mouse-movement> from showing documentation pop-ups.
  ;; It's not that it's a bad idea, but that it catches “movements” spuriously, and makes it so I constantly have to move the mouse cursor to stop getting pop-ups when I'm not using the mouse.
  (setcdr lsp-ui-mode-map nil)
  ;; The above isn't working, so let's...
  (defun lsp-ui-doc--handle-mouse-movement (event)
    (interactive "e")
    nil)

  ;; I could enable it here, but let's wait until after requiring mode-specific things.
  ;;(lsp)
  )
