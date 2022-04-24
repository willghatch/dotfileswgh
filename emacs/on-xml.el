;;; on-xml.el --- TODO - description here -*- lexical-binding: t; -*-

;; Similar to on-parens for smartparens, except this will probably be lower effort.  I just want more tree operations to work with my tree system.

(defun on-xml-on-open-p ()
  (looking-at-p "<[^/]"))
(defun on-xml-on-close-p ()
  (and (looking-at-p ">")
       (or (save-excursion (backward-char) (looking-at-p "/"))
           (save-excursion (search-backward "<") (looking-at-p "</")))))

(defun on-xml--forward-end-from-on-close ()
  (when (save-excursion
          (forward-char)
          (on-parens--advances? #'nxml-forward-element t))
    (nxml-forward-element)
    ;; back to be ON tag...
    (backward-char)))

(defun on-xml--forward-from-on-close ()
  (when (save-excursion
          (forward-char)
          (on-parens--advances? #'nxml-forward-element t))
    (nxml-forward-element)
    ;; get back to start location
    (nxml-backward-element)))

(defun on-xml--forward-from-on-open ()
  (when (save-excursion
          (nxml-forward-element)
          (on-parens--advances? #'nxml-forward-element t))
    (nxml-forward-element)
    (backward-char)
    (on-xml--forward-from-on-close)))

(defun on-xml--forward-end-from-on-open ()
  (when (tree-walk--motion-moved #'nxml-forward-element)
    (backward-char)))

(defun on-xml--forward ()
  (cond ((on-xml-on-open-p) (on-xml--forward-from-on-open))
        ((on-xml-on-close-p) (on-xml--forward-from-on-close))
        (t nil)))
(defun on-xml--forward-end ()
  (cond ((on-xml-on-open-p) (on-xml--forward-end-from-on-open))
        ((on-xml-on-close-p) (on-xml--forward-end-from-on-close))
        (t nil)))

(on-parens--command-wrap on-xml-forward
                         on-xml--forward
                         on-xml-backward
                         "Move forward to the start of the next tag.")
(on-parens--command-wrap on-xml-forward-end
                         on-xml--forward-end
                         on-xml-backward-end
                         "Move forward to the end of the next tag.")

(defun on-xml--backward-from-on-open ()
  (nxml-backward-element))
(defun on-xml--backward-end-from-on-open ()
  (when (tree-walk--motion-moved #'nxml-backward-element)
    (nxml-forward-element)
    (backward-char)))
(defun on-xml--backward-from-on-close ()
  (let ((target (save-excursion
                  (forward-char)
                  (nxml-backward-element)
                  (point))))
    (when (< target (point))
      (goto-char target))))
(defun on-xml--backward-end-from-on-close ()
  (when (save-excursion
          (forward-char)
          (nxml-backward-element)
          (on-parens--advances? #'nxml-backward-element nil))
    (forward-char)
    (nxml-backward-element)
    (on-xml--backward-end-from-on-open)))

(defun on-xml--backward ()
  (cond ((on-xml-on-open-p) (on-xml--backward-from-on-open))
        ((on-xml-on-close-p) (on-xml--backward-from-on-close))
        (t nil)))
(defun on-xml--backward-end ()
  (cond ((on-xml-on-open-p) (on-xml--backward-end-from-on-open))
        ((on-xml-on-close-p) (on-xml--backward-end-from-on-close))
        (t nil)))

(on-parens--command-wrap on-xml-backward
                         on-xml--backward
                         on-xml-forward
                         "Move backward to the start of the next tag.")
(on-parens--command-wrap on-xml-backward-end
                         on-xml--backward-end
                         on-xml-forward-end
                         "Move backward to the end of the next tag.")

(defun on-xml--up-from-on-open ()
  (nxml-backward-up-element))
(defun on-xml--up-end-from-on-open ()
  (when (tree-walk--motion-moved #'nxml-up-element)
    (backward-char)))

(defun on-xml--up-from-on-close ()
  (let ((start (point)))
    (on-xml-backward 1)
    (unless (tree-walk--motion-moved #'on-xml--up-from-on-open)
      (goto-char start))))
(defun on-xml--up-end-from-on-close ()
  (let ((start (point)))
    (on-xml-backward 1)
    (unless (tree-walk--motion-moved #'on-xml--up-end-from-on-open)
      (goto-char start))))

(defun on-xml--up ()
  (cond ((on-xml-on-open-p) (on-xml--up-from-on-open))
        ((on-xml-on-close-p) (on-xml--up-from-on-close))
        (t (nxml-backward-up-element))))
(defun on-xml--up-end ()
  (cond ((on-xml-on-open-p) (on-xml--up-end-from-on-open))
        ((on-xml-on-close-p) (on-xml--up-end-from-on-close))
        (t (when (tree-walk--motion-moved #'nxml-up-element)
             (backward-char)))))

(on-parens--command-wrap on-xml-up
                         on-xml--up
                         on-xml-down
                         "Move up to the start of the parent tag.")
(on-parens--command-wrap on-xml-up-end
                         on-xml--up-end
                         on-xml-down-end
                         "Move up to the end of the parent tag.")

;; TODO - on-xml-down/on-xml-down-end

(provide 'on-xml)
