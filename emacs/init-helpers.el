
;;; define some short-named functions for the most common types of mappings
(defun nobreak-define-key (map keys func)
  (nobreak (define-key map keys func)))
(defun mkmap (keys func)
  (nobreak-define-key evil-motion-state-map keys func))
(defun nkmap (keys func)
  (nobreak-define-key evil-normal-state-map keys func))
(defun lnkmap (keys func)
  (nobreak-define-key evil-normal-state-local-map keys func))
(defun vkmap (keys func)
  (nobreak-define-key evil-visual-state-map keys func))
(defun ikmap (keys func)
  (nobreak-define-key evil-insert-state-map keys func))
(defun pkmap (keys func)
  (nobreak-define-key evil-pager-state-map keys func))

