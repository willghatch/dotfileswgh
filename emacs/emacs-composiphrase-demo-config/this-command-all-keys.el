;;; -*- lexical-binding: t -*-
;;; this-command-all-keys.el --- get all keys for the command, not just the latest read-key-sequence.

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-this-command-all-keys
;;; Git-Repository: git://github.com/willghatch/emacs-this-command-all-keys.git
;;; Keywords: keyboard-macro
;;; Package-Requires: ((emacs "28"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;;
;;; The `this-command-keys' (and `this-single-command-keys', and `this-single-command-raw-keys') get the keys for the command.
;;; Except after `read-key-sequence' or `read-key-sequence-vector' are called, at which point the previous keys are lost.
;;; This package provides a function to get all of the keys, including each `read-key-sequence' call, either as a flat vector or as a list of vectors (one for each of the original command and each call to read keys).
;;; The function requires advice, so it provides a minor mode which activates the advice.

;;; Code:

;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record all keys for each command.

(setq this-command-all-keys--current-command-key-groups nil)
(setq this-command-all-keys--current-single-command-key-groups nil)
(setq this-command-all-keys--current-single-command-raw-key-groups nil)

(defun this-command-all-keys--read-key-sequence-advice (&rest args)
  (setq this-command-all-keys--current-single-command-raw-key-groups
        (cons (this-single-command-raw-keys)
              this-command-all-keys--current-single-command-raw-key-groups))
  (setq this-command-all-keys--current-single-command-key-groups
        (cons (this-single-command-keys)
              this-command-all-keys--current-single-command-key-groups))
  (setq this-command-all-keys--current-command-key-groups
        (cons (this-command-keys)
              this-command-all-keys--current-command-key-groups)))

(defun this-command-all-keys--pre-command ()
  (setq this-command-all-keys--current-single-command-raw-key-groups nil)
  (setq this-command-all-keys--current-single-command-key-groups nil)
  (setq this-command-all-keys--current-command-key-groups nil))

(defun this-command-all-keys (cook-style as-groups)
  "Return a vector of keys used for this command, including any uses of `read-key-sequence', at least up to the point where this function is used.
Returns a vector, or if AS-GROUPS is non-nil, return a list of vectors where each vector represents a different call to `read-key-sequence' or a command loop read.
COOK-STYLE can be 'raw, 'single, or 'tck to receive keys as in `this-single-command-raw-keys', `this-single-command-keys', or `this-command-keys', respectively.

Requires this-command-all-keys-mode to be enabled, since it depends on `read-key-sequence' advice to not lose key sequences.
"
  (when (not this-command-all-keys-mode)
    (error "aggreact-this-command-all-keys requires aggreact-mode be enabled"))
  (let ((data (reverse
               (cons (cond ((eq cook-style 'raw) (this-single-command-raw-keys))
                           ((eq cook-style 'single) (this-single-command-keys))
                           ((eq cook-style 'tck) (this-command-keys))
                           (t (error "bad value for cook-style: %s" cook-style)))
                     (cond ((eq cook-style 'raw) this-command-all-keys--current-single-command-raw-key-groups)
                           ((eq cook-style 'single) this-command-all-keys--current-single-command-key-groups)
                           ((eq cook-style 'tck) this-command-all-keys--current-command-key-groups)
                           (t (error "bad value for cook-style: %s" cook-style)))))))
    (if as-groups
        data
      (apply 'seq-concatenate 'vector data))))

(define-minor-mode this-command-all-keys-mode
  "A minor-mode for fixing a specific flaw in emacs:  using `read-key-sequence' makes `this-command-keys' not have all keys available.
It adds advice to `read-key-sequence' so that all keys for a given command can be recorded.
This is all in support of the function `this-command-all-keys'.
"
  :global t
  (if this-command-all-keys-mode
      (progn
        (add-hook 'pre-command-hook 'this-command-all-keys--pre-command)
        (advice-add 'read-key-sequence
                    :before
                    'this-command-all-keys--read-key-sequence-advice)
        (advice-add 'read-key-sequence-vector
                    :before
                    'this-command-all-keys--read-key-sequence-advice))
    (progn
      (remove-hook 'pre-command-hook 'this-command-all-keys--pre-command)
      (advice-remove 'read-key-sequence
                     'this-command-all-keys--read-key-sequence-advice)
      (advice-remove 'read-key-sequence-vector
                     'this-command-all-keys--read-key-sequence-advice))))

(provide 'this-command-all-keys)
