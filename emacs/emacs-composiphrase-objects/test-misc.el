;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'text-object-stuff)

(setq some-words "The quick brown fox jumps over the lazy dog.")
(ert-deftest misc-tests ()
  (with-temp-buffer
    (insert some-words)
    (goto-char 6)
    (message "region active in test: %s" (region-active-p))
    (should (equal (cons 5 10)
                   (wgh/-expanded-region-to-bounds-of-thing-at-point t nil 'word (cons 7 9))))))

