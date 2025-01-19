
(require 'tree-walk-smartparens-integration)
(require 'ert)

;; TODO - actually write a bunch of tests
;; TODO - maybe use smartparens testing infrastructure, it has a lot of convenience definitions.  They are probably mostly internal, but they are probably sufficiently stable.

(defmacro should/region-equal (region-cons)
  `(progn
     (should (region-active-p))
     (should (equal ,region-cons
                    (cons (region-beginning) (region-end))))))
(defmacro should/looking-at (at-string)
  `(progn
     (let* ((str ,at-string)
            (result (looking-at-p str)))
       (when (not result)
         (message "Test failure, should look at: %s" str)
         (message "actually looking at: %s" (buffer-substring (point) (min (point-max) (+ (point) 10)))))
       (should result))))
(defmacro should/mark-looking-at (at-string)
  `(progn
     (save-mark-and-excursion
       (exchange-point-and-mark)
       (should/looking-at ,at-string))))

(ert-deftest test-expand-region-to-any-delimiter-after-last-child ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (transient-mark-mode 1)
    (emacs-lisp-mode)
    (goto-char 1)
    (search-forward "))")
    (backward-char 5)
    (message "point pre: %s, mark %s, active: %s" (point) (mark) (region-active-p))
    (sptw-expand-region-to-any-delimiter)
    (message "point post: %s, mark: %s, active: %s" (point) (mark) (region-active-p))
    (message "point at: %s" (buffer-substring (region-beginning) (point-max)))
    (should/mark-looking-at "(inner")
    ;;(should/region-equal (cons 8 56))
    )

  )

(ert-deftest misc-sptw-tests ()
  (with-temp-buffer
    (insert "
(outer (inner foo
              bar baz
              ))
")
    (setq-local sp-pair-list '(("(" . ")")))
    (goto-char 1)
    (transient-mark-mode 1)
    (emacs-lisp-mode)

    (search-forward "foo")
    (sptw-expand-region)
    (should/mark-looking-at "foo")
    (sptw-expand-region-to-any-delimiter)
    (should/mark-looking-at "(inner")

    (set-mark nil)
    (goto-char 1)
    (search-forward "foo")
    (backward-char 1)
    (sptw-expand-region-to-any-delimiter)
    (should/mark-looking-at "(inner")

    ;; TODO - enable this, I didn't think this was failing, too, but it is.
    ;; (set-mark nil)
    ;; (goto-char 1)
    ;; (search-forward "foo")
    ;; (sptw-expand-region-to-any-delimiter)
    ;; (should/mark-looking-at "(inner")
    )

    )
