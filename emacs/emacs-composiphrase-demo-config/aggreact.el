;;; -*- lexical-binding: t; -*-
;;; aggreact.el --- Record commands with rich history, and group them.

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-aggreact
;;; Git-Repository: git://github.com/willghatch/emacs-aggreact.git
;;; Keywords: keyboard-macro repeat
;;; Package-Requires: ((emacs "28") (this-command-all-keys "0.0"))

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
;;; See docstring for aggreact-mode.

;;; Code:

;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.

;; TODO - this is fundamentally broken when considering recursive editing.  I'll probably re-think how I'm doing this.

(require 'this-command-all-keys)

(setq aggreact--current-groups nil)

(defvar aggreact-command-group-split-predicate nil
  "Predicate for when to split a command group.
Splits when the predicate returns true.
If the predicate itself is nil instead of a function, always split.
Predicate takes a single argument, which is a (reversed) list of commands so far, as alists of details about the commands.
")

(defvar aggreact-command-history-enrichment-functions nil
  "List of functions for enriching command history.
Each function takes a single argument, the command data alist so far, and returns either nil to make no change, or an alist to extend the history with.
IE if you want to add one field, you can return an alist with the one field, which will be appended to the other alist in the enrichment loop.
")

(defvar aggreact-command-group-split-functions nil
  "List of functions to run once a command group is split (according to 'aggreact-command-group-split-predicate').
Each function receives a single argument: the new command group.
The command group is a list of alists, where each alist contains details of a command in the group.
This is useful to eg. keep histories of commands with interesting properties.
")

(defun aggreact--post-command ()
  (let* ((new-command-details
          `((command . ,this-command)
            ;; TODO - command arguments
            (keys-vectors . ,(this-command-all-keys 'tck t))
            (single-keys-vectors . ,(this-command-all-keys 'single t))
            (raw-keys-vectors . ,(this-command-all-keys 'raw t)))))
    (setq new-command-details
          (seq-reduce (lambda (accum enrich-func)
                        (let ((enrich-result
                               (with-demoted-errors
                                   "error during command history enrichment: %s"
                                 (funcall enrich-func accum))))
                          (if enrich-result
                              (append enrich-result accum)
                            accum)))
                      aggreact-command-history-enrichment-functions
                      new-command-details))
    (setq aggreact--current-groups (cons new-command-details aggreact--current-groups))
    (when (if aggreact-command-group-split-predicate
              (funcall aggreact-command-group-split-predicate aggreact--current-groups)
            t)
      (let ((finalized-group (reverse aggreact--current-groups)))
        (setq aggreact--current-groups nil)
        (with-demoted-errors
            "error during aggreact-command-group-split-functions: %s"
          (mapcar (lambda (func) (funcall func finalized-group))
                  aggreact-command-group-split-functions))))))

(defun aggreact--get-keys-for-command-group (command-group)
  "Return the raw keys used for the entire COMMAND-GROUP as a flat vector."
  (apply 'seq-concatenate 'vector
         (mapcar (lambda (x) (apply 'seq-concatenate 'vector
                                    (cdr (assq 'keys-vectors x))))
                 command-group)))

(defun aggreact-execute-command-group-as-keyboard-macro (command-group)
  "Use the recorded keys of COMMAND-GROUP to execute as keyboard macro.
This may cause issues if not run in a state where the keys will do the same thing...
"
  (execute-kbd-macro (aggreact--get-keys-for-command-group command-group)))

(setq aggreact--this-command-all-keys-mode-state-before-aggreact nil)

(define-minor-mode aggreact-mode
  "A minor-mode for recording commands.
For each command, it records an alist that includes the the keys:
* command - the command executed
* keys-vectors - list of vectors of keys, as from `this-single-command-keys'
* raw-keys-vectors - list of vectors of keys, as from `this-single-command-raw-keys'

Additionally, it uses 'aggreact-command-history-enrichment-functions' to add more keys to the alist.

Commands are also grouped.
The variable 'aggreact-command-group-split-predicate' determines when command groups are split.
When a group is split, the 'aggreact-command-gorup-split-functions' list is run.
You can add a function to the list to eg. keep a list of interesting command groups.

This mode also requires this-command-all-keys mode.

The original motivation behind this mode was to provide command recording and replay that can work for my other packages, estate-mode and composiphrase.
That requires replaying groups of commands, and filtering to decide which groups are interesting for replaying.
See the composiphrase demo config at TODO to see an example setup using aggreact-mode.
"
  :global t
  (if aggreact-mode
      (progn
        (setq aggreact--this-command-all-keys-mode-state-before-aggreact
              this-command-all-keys-mode)
        (this-command-all-keys-mode 1)
        (add-hook 'post-command-hook 'aggreact--post-command))
    (progn
      (remove-hook 'post-command-hook 'aggreact--post-command)
      (when (not aggreact--this-command-all-keys-mode-state-before-aggreact)
        (this-command-all-keys-mode -1)))))

(provide 'aggreact)
