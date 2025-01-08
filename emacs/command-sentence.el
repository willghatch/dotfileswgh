;; command-sentence.el  -*- lexical-binding: t -*-
;;
;;
;; The command-sentence library provides a way of interpreting sentence objects based on a configuration.
;; A command-sentence is a list of command-sentence-words.
;; The `command-sentence-execute' function takes a command-sentence SENTENCE and a command-sentence-configuration CONFIG, and maps the sentence, using the config, to a specific function to call.
;;
;; A command-sentence-word is a dictionary imlemented as an alist, following a schema that maps a known set of keys to values.
;; * word-type: can be 'verb, 'object, 'modifier
;; * parameter-name: (only on 'modifier words) a symbol, name of appropriate parameter of object or verb
;; * contents: for modifier it can be anything, for verb or object it should be a symbol
;; * ui-hint: a string that can be displayed on the modeline or in some other way to show the current state (optional)
;; * keys: a string or vector of the keys used when entering the word (optional)
;;
;; A command-sentence-configuration is an alist with the following top-level fields:
;; * verbs - has a list of verb specifications
;; * objects - has a list of object specifications
;; * match-table - has a list of match-table specifications
;;
;; A verb specification is a list (VERB-NAME MODIFIER-SPEC ...)
;; An object specification is a list (OBJECT-NAME MODIFIER-SPEC ...)
;; A modifier spec is a list (PARAM-NAME DEFAULT-VALUE)
;;
;; A match-table specification is an list with the following fields:
;; * verb: a symbol
;; * object: a symbol
;; * modifiers: an alist mapping modifier name to a list (VALUE OPTIONAL-COMPARATOR), if the optional comparator is not given, the `equal' function is used.  If `t` is given as the comparitor, the value always matches.  If a comparator function is given, it takes the given value first, then the table entry value.
;; * executor: a list of function, then executor-argument-spec
;;
;; An executor-argument-spec is one of:
;; * a list of parameter names in the order which they are to be given to the function
;; * the symbol 'alist to pass all parameters in as an alist.
;; * the symbol 'sentence-with-defaults to pass the the command sentence, but with default modifiers added.  This is useful to define verbs that are wrappers around others.
;; * the symbol 'original-sentence to pass the original command sentence.
;;

(defun command-sentence--get-spec-from-symbol (given-v-or-o given-verb-p config)
  "Takes a symbol name for a verb or an object, returns the spec from the config."
  (cdr (assq given-v-or-o
             (cdr (assq (if given-verb-p 'verbs 'objects) config)))))
(defun command-sentence--get-verb-or-obj-name (given-v-or-o)
  "Takes a command-sentence-word or a symbol."
  (if (symbolp given-v-or-o)
      given-v-or-o
    (cdr (assq 'contents given-v-or-o))))
(defun command-sentence--get-default-verb-or-obj (given-v-or-o given-verb-p config)
  "Given a verb or object (as a command-sentence-word), get the symbol for the default of the other one."
  (let* ((given-name (command-sentence--get-verb-or-obj-name given-v-or-o))
         (given-spec (command-sentence--get-spec-from-symbol given-name given-verb-p config)))
    (cdr (assq (if given-verb-p 'default-object 'default-verb) given-spec))))

(defun command-sentence--match (sentence config)
  "Find a match for SENTENCE using the CONFIG.
Return nil if no match is found.
Otherwise, return a cons pair (PARAMS . EXECUTOR), containing the final parameters and executor from the match.
"
  (let* ((verb (seq-find (lambda (word) (eq 'verb (cdr (assq 'word-type word))))
                         sentence))
         (object (seq-find (lambda (word) (eq 'object (cdr (assq 'word-type word))))
                           sentence))
         (given-modifiers (mapcar
                           (lambda (word) (cons (cdr (assq 'parameter-name word))
                                                (cdr (assq 'contents word))))
                           (cl-remove-if-not
                            (lambda (word) (eq 'modifier
                                                (cdr (assq 'word-type word))))
                            sentence))))
    (when (and (not verb) (not object))
      (error "command-sentence: sentence lacks both verb and object: %s" sentence))
    ;; TODO - deeper validation of the structure of command sentence words, to be sure all parts are there.
    (let* ((verb (or verb
                     (command-sentence--get-default-verb-or-obj object nil config)
                     (error "command-sentence: can't resolve a verb for sentence: %s" sentence)))
           (object (or object
                       (command-sentence--get-default-verb-or-obj verb t config)
                       (error "command-sentence: can't resolve an object for sentence: %s" sentence)))
           (verb-name (command-sentence--get-verb-or-obj-name verb))
           (object-name (command-sentence--get-verb-or-obj-name object))
           (verb-spec (command-sentence--get-spec-from-symbol verb-name t config))
           (object-spec (command-sentence--get-spec-from-symbol object-name nil config))
           ;; TODO - check for duplicate param names, and probably error.
           (full-default-params (append verb-spec object-spec))
           (params (mapcar (lambda (param-spec)
                             (or (assq (car param-spec) given-modifiers)
                                 (assq (car param-spec) full-default-params)))
                           full-default-params))
           (match-table (cdr (assq 'match-table config)))
           (matched nil))
      (while (and (not matched)
                  match-table)
        (let* ((entry (car match-table))
               (verb-sym-match (eq verb-name (car entry)))
               (object-sym-match (eq object-name (cadr entry)))
               (verb-match (or verb-sym-match
                               (and (not (symbolp (car entry)))
                                    (functionp (car entry))
                                    ;; Don't try to match predicate if object
                                    ;; match already failed symbol eq
                                    (or object-sym-match (functionp (cadr entry)))
                                    (funcall (car entry) verb-name))))
               (object-match (or object-sym-match
                                 (and verb-match
                                      (not (symbolp (cadr entry)))
                                      (functionp (cadr entry))
                                      (funcall (cadr entry) object-name))))
               (vo-match (and verb-match object-match))
               (entry-mods (and vo-match
                                (caddr entry)))
               (full-match (and vo-match
                                (command-sentence--match-table-modifiers-match-p
                                 params
                                 entry-mods))))
          (when full-match
            (setq matched entry))
          (setq match-table (cdr match-table))))
      (and matched
           (cons params (seq-elt matched 3))))))


(defun command-sentence--match-table-modifiers-match-p
    (given-params-alist match-table-modifiers)
  "Check if the GIVEN-PARAMS-ALIST matches the MATCH-TABLE-MODIFIERS."
  (let ((match-failed nil))
    (while (and given-params-alist
                (not match-failed))
      (let* ((param-name (caar given-params-alist))
             (param-value (cdar given-params-alist))
             (matcher (assq param-name match-table-modifiers)))
        (when matcher
          (let* ((expected-value (cadr matcher))
                 (optional-comparator (caddr matcher))
                 (comparator (cond ((eq optional-comparator t)
                                    (lambda (a b) t))
                                   (optional-comparator optional-comparator)
                                   (t #'equal))))
            (when (not (funcall comparator
                                param-value
                                expected-value))
              (setq match-failed t)))))
      (setq given-params-alist (cdr given-params-alist)))
    (not match-failed)))

(defun command-sentence--execute-match (orig-sentence params executor)
  "PARAMS and EXECUTOR should match what is returned from command-sentence--match."
  (let ((spec (cadr executor))
        (func (car executor)))
    (cond ((eq spec 'alist) (funcall func params))
          ((eq spec 'original-sentence) (funcall func orig-sentence))
          ((eq spec 'sentence-with-defaults)
           (let ((new-sentence (command-sentence--apply-params-to-sentence
                                orig-sentence params)))
             (funcall func new-sentence)))
          ((listp spec) (apply func (mapcar (lambda (name)
                                              (cdr (assq name params)))
                                            spec)))
          (t (error "bad executor spec: %s" spec)))))

(defun command-sentence-execute (sentence config)
  (let ((match (command-sentence--match sentence config)))
    (if match
        (command-sentence--execute-match sentence (car match) (cdr match))
      (error "No executor found for command sentence: %s" sentence))))

(defun command-sentence--apply-params-to-sentence (old-sentence params)
  "For each param in PARAMS that is not in OLD-SENTENCE, add the param to a new sentence (whose tail is the old sentence)."
  (let ((new-sentence old-sentence))
    (dolist (param params)
      (let ((param-name (car param))
            (param-value (cdr param)))
        (unless (seq-find TODO-PRED old-sentence)
          (push `((word-type . modifier)
                  (parameter-name . ,param-name)
                  (contents . ,param-value))
                new-sentence))))
    new-sentence))

(defvar-local command-sentence-current-sentence nil)
(defvar command-sentence-current-configuration nil)

(defun command-sentence-clear-current ()
  (interactive)
  (setq command-sentence-current-sentence nil))

(defun command-sentence-add-to-current (&rest words)
  "add words to current-command-sentence (and also add the keys used to add the command (WIP))"
  (let ((word1 (car words))
        (words-rest (cdr words)))
    (unless nil ;;no-keys
      ;; TODO - how can I get the keys for numeric arguments and uses of M-x, etc?
      (setq word1 (cons (cons 'keys (this-command-keys))
                        word1)))
    (setq command-sentence-current-sentence (append (list word1) words-rest command-sentence-current-sentence))))

(defun command-sentence-execute-current ()
  "Executes command-sentence-current-sentence and clears it."
  (interactive)
  (let ((sentence command-sentence-current-sentence))
    (command-sentence-clear-current)
    (command-sentence-execute sentence command-sentence-current-configuration)))

(defun command-sentence-keyboard-macro-from-sentence (sentence)
  "Get a vector or string of keys used to create SENTENCE."
  (apply append (map (lambda (word) (cdr (assq 'keys word)))
                     (reverse sentence))))

;; TODO - add command-sentence-configuration-compose that can merge configs
;; TODO - add macro for use as key bind RHS such that you write a list of words to add, an option to execute, and it implicitly takes a numeric argument.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests

;; TODO - move tests into some other file.

(setq command-sentence--test-config
      `((verbs
         .
         ((move (default-object . word) (direction . forward) (count . 1))
          (delete (default-object . word) (direction . forward))
          ))
        (objects
         .
         ((word (default-verb . move) (location-within . beginning))
          (sentence (default-verb . move) (location-within . beginning))
          ))
        (match-table
         .
         ((move word
                ((direction forward) (location-within beginning eq))
                (forward-word (count)))
          (delete ,(lambda (x) t)
                  ()
                  (function-to-delete-after-moving sentence-with-defaults)))
         )))


(require 'ert)
(ert-deftest command-sentence--match-test ()
  "Test matching functionality of `command-sentence--match`."
  (let ((mv '((word-type . verb)
              (contents . move)))
        (del '((word-type . verb)
               (contents . delete)))
        (wd '((word-type . object)
              (contents . word)))
        (fwd '((word-type . modifier)
               (parameter-name . direction)
               (contents . forward)))
        (beg '((word-type . modifier)
               (parameter-name . location-within)
               (contents . beginning))))

    (let ((result1 (command-sentence--match (list mv fwd beg wd) command-sentence--test-config)))
      (should result1)
      (let ((params (car result1))
            (executor (cdr result1)))
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
        ))

    ;; Same as above, but with default values
    (let ((result2 (command-sentence--match (list mv wd) command-sentence--test-config)))
      (should result2)
      (let ((params (car result2))
            (executor (cdr result2)))
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
        ))

    ;; Predicate matcher
    (let ((result3 (command-sentence--match (list del wd) command-sentence--test-config)))
      (should result3)
      (let ((params (car result3))
            (executor (cdr result3)))
        ;;(should (equal 'forward (cdr (assq 'direction params))))
        ;;(should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'function-to-delete-after-moving (car executor)))
        ))
    )


  (let ((non-matching-sentence '(((word-type . verb)
                                  (contents . jump))
                                 ((word-type . object)
                                  (contents . word))
                                 ((word-type . modifier)
                                  (parameter-name . direction)
                                  (contents . backward)))))
    (should-not (command-sentence--match non-matching-sentence command-sentence--test-config)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a config, maybe to be default

;; TODO - put this somewhere else
;; TODO - add convenient commands to add to a config, especially the default config
(defun command-sequence--config-add (config section-key spec)
  "Update the given config, mutating it."
  (let ((section (assq section-key config)))
    (setcdr section (cons spec (cdr section)))))

(setq command-sentence--my-config
      `((verbs
         .
         ((move (direction . forward) (num . 1))
          ;; TODO - add registers for delete and change-move to copy their old contents.  Also to copy-move.
          ;; TODO - what arguments do most of these mean?  Eg. tree movement also needs arguments about up/down, when going down you can have a child index, etc.  I generally want an idempotence argument for movements, though maybe it's really only useful for things like “go to the end of the line” without going to the next line, but it's likely a useful option in principle especially for keyboard macros.  Also want movement to sibling vs strict full/half sibling (indent/org trees) vs unbound by tree (eg. move to next s-expression beginning whether or not it moves out of the current tree).
          ;; TODO - should I have “select” separate from “move”?  And maybe deleting, changing, or copying could be an action parameter for moving, rather than a separate action?
          (delete (direction . forward) (num . 1))
          (change (direction . forward) (num . 1))
          (copy (direction . forward) (num . 1))
          (transpose (direction . forward) (num . 1))
          (join (direction . forward) (num . 1))
          (split)
          (slurp (direction . forward) (num . 1))
          (barf (direction . forward) (num . 1))
          (open (direction . forward)) ;; IE new sibling
          ;; TODO - what verbs?  Tree promote/demote, but eg. for paren trees we care about which kind of paren/bracket/brace/etc is used, or for xml we need a specific tag.  Tree splice - works for symex and xml, but less clearly useful for outline-mode or indent trees.  Tree change node type, eg. symex change paren type, xml change tag.  Tree raise - IE replace parent with child, except I'm used to the workflow of select-element, copy, expand to parent, paste.
          ))
        (objects
         .
         ((character (default-verb . move) (location-within . beginning) (specific . nil))
          (word (default-verb . move) (location-within . beginning))
          (sentence (default-verb . move) (location-within . beginning))
          (line (default-verb . move) (location-within . beginning))
          (sptw (default-verb . move) (location-within . beginning))
          (outline (default-verb . move) (location-within . beginning))
          (region)
          ))
        (match-table
         .
         (
          ;; TODO - I just wrote this encoding but it's not quite what I want and doesn't actually work right now the way I was thinking when writing it, either.  I need to decide how I want this.
          (move character
                ((direction forward) (specific t ,(lambda (actual expected) actual)))
                (rmo/wgh/find-char-beginning-in-line-forward) (num))
          (move character
                ((direction backward) (specific t ,(lambda (actual expected) actual)))
                (rmo/wgh/find-char-beginning-in-line-backward) (num))
          (move character
                ((direction forward) (specific nil))
                (rmo/forward-char (num)))
          (move character
                ((direction backward) (specific nil))
                (rmo/forward-char (num)))

          (move word
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-word-beginning (num)))
          (move word
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-word-beginning (num)))
          (move word
                ((direction forward) (location-within end))
                (rmo/wgh/forward-word-end (num)))
          (move word
                ((direction backward) (location-within end))
                (rmo/wgh/backward-word-end (num)))

          (move vi-like-word
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-vi-like-word-beginning (num)))
          (move vi-like-word
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-vi-like-word-beginning (num)))
          (move vi-like-word
                ((direction forward) (location-within end))
                (rmo/wgh/forward-vi-like-word-end (num)))
          (move vi-like-word
                ((direction backward) (location-within end))
                (rmo/wgh/backward-vi-like-word-end (num)))

          (move symbol
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-symbol-beginning (num)))
          (move symbol
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-symbol-beginning (num)))
          (move symbol
                ((direction forward) (location-within end))
                (rmo/wgh/forward-symbol-end (num)))
          (move symbol
                ((direction backward) (location-within end))
                (rmo/wgh/backward-symbol-end (num)))

          (move sentence
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-sentence-beginning (num)))
          (move sentence
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-sentence-beginning (num)))
          (move sentence
                ((direction forward) (location-within end))
                (rmo/wgh/forward-sentence-end (num)))
          (move sentence
                ((direction backward) (location-within end))
                (rmo/wgh/backward-sentence-end (num)))

          (move paragraph
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-paragraph-beginning (num)))
          (move paragraph
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-paragraph-beginning (num)))
          (move paragraph
                ((direction forward) (location-within end))
                (rmo/wgh/forward-paragraph-end (num)))
          (move paragraph
                ((direction backward) (location-within end))
                (rmo/wgh/backward-paragraph-end (num)))

          (move line
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-line-beginning (num)))
          (move line
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-line-beginning (num)))
          (move line
                ((direction forward) (location-within end))
                (rmo/wgh/forward-line-end (num)))
          (move line
                ((direction backward) (location-within end))
                (rmo/wgh/backward-line-end (num)))

          (move sptw
                ((direction forward) (location-within beginning))
                (rmo/sptw-forward-sibling-beginning (num)))
          (move sptw
                ((direction backward) (location-within beginning))
                (rmo/sptw-backward-sibling-beginning (num)))
          (move sptw
                ((direction forward) (location-within end))
                (rmo/sptw-forward-sibling-end (num)))
          (move sptw
                ((direction backward) (location-within end))
                (rmo/sptw-backward-sibling-end (num)))

          (move outline
                ((direction forward) (location-within beginning))
                (rmo/outline-forward-same-level (num)))
          (move outline
                ((direction backward) (location-within beginning))
                (rmo/outline-backward-same-level (num)))
          ;; TODO - end of outline?
          (move indent-tree
                ((direction forward) (location-within beginning))
                (indent-tree-forward-full-or-half-sibling (num)))
          (move indent-tree
                ((direction backward) (location-within beginning))
                (indent-tree-backward-full-or-half-sibling (num)))


          ;; TODO - for transpose character, implement something that follows the character explicitly forward/backward.
          (transpose word ((direction forward)) (wgh/transpose-word-forward (num)))
          (transpose word ((direction backward)) (wgh/transpose-word-backward (num)))
          (transpose vi-like-word ((direction forward)) (wgh/transpose-vi-like-word-forward (num)))
          (transpose vi-like-word ((direction backward)) (wgh/transpose-vi-like-word-backward (num)))
          (transpose symbol ((direction forward)) (wgh/transpose-symbol-forward (num)))
          (transpose symbol ((direction backward)) (wgh/transpose-symbol-backward (num)))
          (transpose sentence ((direction forward)) (wgh/transpose-sentence-forward (num)))
          (transpose sentence ((direction backward)) (wgh/transpose-sentence-backward (num)))
          (transpose paragraph ((direction forward)) (wgh/transpose-paragraph-forward (num)))
          (transpose paragraph ((direction backward)) (wgh/transpose-paragraph-backward (num)))
          (transpose line ((direction forward)) (wgh/transpose-line-forward (num)))
          (transpose line ((direction backward)) (wgh/transpose-line-backward (num)))
          (transpose sptw ((direction forward)) (sptw-transpose-sibling-forward (num)))
          (transpose sptw ((direction backward)) (sptw-transpose-sibling-backward (num)))
          (transpose outline ((direction forward)) (wgh/outline-transpose-sibling-forward (num)))
          (transpose outline ((direction backward)) (wgh/outline-transpose-sibling-backward (num)))
          (transpose indent-tree ((direction forward)) (indent-tree-transpose-sibling-forward (num)))
          (transpose indent-tree ((direction backward)) (indent-tree-transpose-sibling-backward (num)))

          ;; TODO - rename and put this vilish-open-line stuff... somewhere reasonable
          (open line ((direction forward)) (vilish-open-line-below))
          (open line ((direction backward)) (vilish-open-line-above))
          (open outline ((direction forward)) (wgh/outline-add-heading-below))
          (open outline ((direction backward)) (wgh/outline-add-heading-above))
          ;; TODO - indent tree open
          ;; TODO - symex open - ignore unwrapped forms and open a sibling form with the same paren type, hopefully matching indentation...

          (split line () (open-line))
          ;; TODO - is there something useful to do for split for outline or indent tree?  For symex or XML it has obvious meaning, but is used in the middle of a thing.  Maybe for outline it means to split the parent on the current header, inserting a new header above at the parent level.  And similar for indent tree.  Need to implement this...
          (split sptw () (sp-split-sexp))
          ;; TODO - split for non-tree objects has reasonably defined meaning, I suppose, but isn't very interesting.

          ;; TODO - I need to fix my join-line implementation to take a numerical argument
          (join line ((direction forward)) (join-line/default-forward))
          (join line ((direction forward)) (,(lambda () (join-line/default-forward -1))))
          ;; TODO - sptw - make a join-sexp function that takes a forward or backward argument


          ;; TODO - optional register for delete to be delete-copy
          (delete region
                  ()
                  (,(lambda () (delete-region (region-beginning) (region-end)))))
          (delete ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(lambda (sentence-with-defaults)
                      (let ((orig-point (point)))
                        (command-sentence-execute
                         ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                         (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                         command-sentence-current-configuration)
                        (delete-region (min (point) orig-point)
                                       (max (point) orig-point))))
                   sentence-with-defaults))

          (change region
                  ()
                  (,(lambda ()
                      (delete-region (region-beginning) (region-end))
                      (estate-insert-state))))
          (change ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(lambda (sentence-with-defaults)
                      (let ((orig-point (point)))
                        (command-sentence-execute
                         ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                         (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                         command-sentence-current-configuration)
                        (delete-region (min (point) orig-point)
                                       (max (point) orig-point))
                        (estate-insert-state)))
                   sentence-with-defaults))

          (copy region
                  ()
                  (estate-copy))
          (copy ,(lambda (x) (not (memq x '(region))))
                ()
                (,(lambda (sentence-with-defaults)
                    (let ((orig-point (point)))
                      (command-sentence-execute
                       ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                       (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                       command-sentence-current-configuration)
                      (estate-copy (cons (min (point) orig-point)
                                         (max (point) orig-point)))))
                 sentence-with-defaults))
          ))))

;; temporary convenience...
(setq command-sentence-current-configuration command-sentence--my-config)
(defun cs/move ()
  (command-sentence-add-to-current '((word-type . verb) (contents . move))))
(defun cs/word ()
  (command-sentence-add-to-current '((word-type . object) (contents . word))))

(provide 'command-sentence)
