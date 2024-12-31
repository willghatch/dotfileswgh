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
    (when (not verb)
      (error "No verb in command sentence: %s" sentence))
    (when (not object)
      (error "No object in command sentence: %s" sentence))
    ;; TODO - deeper validation of the structure of command sentence words, to be sure all parts are there.
    (let* ((verb-name (cdr (assq 'contents verb)))
           (object-name (cdr (assq 'contents object)))
           (verb-spec (cdr (assq verb-name
                                 (cdr (assq 'verbs config)))))
           (object-spec (cdr (assq object-name
                                   (cdr (assq 'objects config)))))
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
               (entry-mods (and (eq verb-name (car entry))
                                (eq object-name (cadr entry))
                                (caddr entry)))
               (full-match (and entry-mods
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
         ((move (direction . forward) (count . 1))
          (delete-move (direction . forward))
          ))
        (objects
         .
         ((word (location-within . beginning))
          (sentence (location-within . beginning))
          ))
        (match-table
         .
         ((move word
                ((direction forward) (location-within beginning eq))
                (forward-word (count)))
          (delete-move ANY
                       ()
                       (function-to-delete-after-moving sentence-with-defaults)))
         )))


(require 'ert)
(ert-deftest command-sentence--match-test ()
  "Test matching functionality of `command-sentence--match`."
  (let ((mv '((word-type . verb)
              (contents . move)))
        (wd '((word-type . object)
              (contents . word)))
        (fwd '((word-type . modifier)
               (parameter-name . direction)
               (contents . forward)))
        (beg '((word-type . modifier)
               (parameter-name . location-within)
               (contents . beginning))))

    (let ((result (command-sentence--match (list mv fwd beg wd) command-sentence--test-config)))
      (should result)
      (let ((params (car result))
            (executor (cdr result)))
(message "result: %s" result)
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
        ))

    ;; Same as above, but with default values
    (let ((result (command-sentence--match (list mv wd) command-sentence--test-config)))
      (should result)
      (let ((params (car result))
            (executor (cdr result)))
        (should (equal 'forward (cdr (assq 'direction params))))
        (should (equal 'beginning (cdr (assq 'location-within params))))
        (should (equal 'forward-word (car executor)))
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
          ;; TODO - add registers for delete-move and change-move to copy their old contents.  Also to copy-move.
          (delete-move (direction . forward) (num . 1))
          (change-move (direction . forward) (num . 1))
          (copy-move (direction . forward) (num . 1))
          (transpose (direction . forward) (num . 1))
          (join (direction . forward) (num . 1))
          (split)
          ))
        (objects
         .
         ((word (location-within . beginning))
          (sentence (location-within . beginning))
          (line (location-within . beginning))
          (sptw (location-within . beginning))
          ))
        (match-table
         .
         (
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


          (transpose word
                ((direction forward))
                (wgh/transpose-word-forward (num)))
          (transpose word
                ((direction backward))
                (wgh/transpose-word-backward (num)))
          (transpose sentence
                ((direction forward))
                (wgh/transpose-sentence-forward (num)))
          (transpose sentence
                ((direction backward))
                (wgh/transpose-sentence-backward (num)))
          (transpose line
                ((direction forward))
                (wgh/transpose-line-forward (num)))
          (transpose line
                ((direction backward))
                (wgh/transpose-line-backward (num)))
          (transpose sptw
                ((direction forward))
                (sptw-transpose-sibling-forward (num)))
          (transpose sptw
                ((direction backward))
                (sptw-transpose-sibling-backward (num)))

          ;; TODO - implement the ANY thing for object...
          (delete-move ANY
                       ()
                       (function-to-delete-after-moving sentence-with-defaults)))
         )))

;; temporary convenience...
(setq command-sentence-current-configuration command-sentence--my-config)
(defun cs/move ()
  (command-sentence-add-to-current '((word-type . verb) (contents . move))))
(defun cs/word ()
  (command-sentence-add-to-current '((word-type . object) (contents . word))))

(provide 'command-sentence)
