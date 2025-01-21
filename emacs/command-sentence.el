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
        (unless (seq-find (lambda (word) (and (eq (cdr (assq 'word-type word))
                                                  'modifier)
                                              (eq (cdr (assq 'parameter-name word))
                                                  param-name)))
                          old-sentence)
          (push `((word-type . modifier)
                  (parameter-name . ,param-name)
                  (contents . ,param-value))
                new-sentence))))
    new-sentence))

(defvar-local command-sentence-current-sentence nil)
(defvar command-sentence-current-configuration nil)

;; TODO - should I have just one previous sentence, or have a history that keeps up to N elements?  For now I'll do the simplest thing for my immediate wants.
(defvar-local command-sentence-previous-sentence nil)

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

(defun command-sentence-add-to-current-with-numeric-handling (exec-after-p &rest words)
  "Takes a list of command-sentence words, but returns an interactive function that takes a numeric argument, and adds the numeric argument to the modifier parameter 'num'."
  (lambda (&optional num)
    (interactive "p")
    (apply 'command-sentence-add-to-current
           (if (and num (not (equal num 1)))
               (cons `((word-type . modifier)
                       (parameter-name . num)
                       (contents . ,num)
                       (ui-hint . ,num))
                     words)
             words))
    (when exec-after-p
      (command-sentence-execute-current))))

(defun command-sentence-execute-current ()
  "Executes command-sentence-current-sentence and clears it."
  (interactive)
  (let ((sentence command-sentence-current-sentence))
    (setq command-sentence-previous-sentence sentence)
    (command-sentence-clear-current)
    (command-sentence-execute sentence command-sentence-current-configuration)))

(defun command-sentence-keyboard-macro-from-sentence (sentence)
  "Get a vector or string of keys used to create SENTENCE."
  ;; TODO -- I need better handling to always get keys used.  I'm currently always missing keys used for numeric arguments, and I'm missing some keys used for prefix maps.  I would like at least my config to consistently work for this, even if I can't consistently get all keys in a general way that anyone could use with arbitrary configurations.
  (apply (lambda (&rest args)
           (apply #'seq-concatenate 'vector args))
         (mapcar (lambda (x) (if (vectorp x) x (seq--into-vector x)))
                 (seq-filter #'identity
                             (mapcar (lambda (word) (cdr (assq 'keys word)))
                                     (reverse sentence))))))

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
         ((move (direction . forward)
                (tree-vertical . ,nil) ;; options, nil, 'up, 'down
                ;; TODO - tree-inner is very specific to smartparens object so I can go up to inner parent...
                (tree-inner . ,nil)     ;; boolean
                (tree-traversal . ,nil) ;; nil or 'inorder
                (expand-region . ,nil) ;; t for object selection, 'inner where that makes sense (eg. selecting tree children), may add more if more make sense, eg. 'space for adding surrounding white space.
                (num . 1))
          ;; TODO - add registers for delete, change, copy.  They should all copy, but delete and change can have a different default register if I don't want them to copy by default.  Also paste should have a register to paste from, but also have a register for replaced contents when replacing.
          ;; TODO - what arguments do most of these mean?  Eg. tree movement also needs arguments about up/down, when going down you can have a child index, etc.  I generally want an idempotence argument for movements, though maybe it's really only useful for things like “go to the end of the line” without going to the next line, but it's likely a useful option in principle especially for keyboard macros.  Also want movement to sibling vs strict full/half sibling (indent/org trees) vs unbound by tree (eg. move to next s-expression beginning whether or not it moves out of the current tree).
          ;; TODO - add modifier limit-to-single-line.
          ;; TODO - modifier for surrounding white space.  Eg. for expand-region to expand to the object, then expand again to include surrounding white space.  But there are several versions of this that I may want... sometimes I want all surrounding space, sometimes just the space before or after.  And for lines, by default I want to include the white space, but I want a modifier for eg. back-to-indentation and a similar one for the line end before extra white space.  So I probably want the key to invert for that case, or have some kind of special handling.
          (delete)
          (change)
          (copy)
          (upcase)
          (downcase)
          (transpose (direction . forward) (num . 1))
          (join (direction . forward) (num . 1))
          (split)
          (slurp (direction . forward) (num . 1))
          (barf (direction . forward) (num . 1))
          (open (direction . forward)) ;; IE new sibling
          ;; TODO - none of the below are really implemented yet.
          (splice) ;; TODO - splice makes sense for delimited trees (s-expressions, xml), meaning delete the parentheses or enclosing tags, but less so for eg. org or indent trees, but could maybe mean promote.
          (promote) ;; TODO - promote makes sense for org/outline trees, as well as indent-tree, and could maybe mean splice for lift-out-of-parent for symex...
          (demote) ;; For symex you should be prompted to choose a tag to wrap with, and for xml you should be prompted for a tag.
          (change-delimiter) ;; This really only makes sense for a few things... how many operators do I want to have that aren't really composable?  That said, it's a common operation, so I want it to be convenient in the layout even if it doesn't apply to most objects.
          ;; TODO - something like insert-move, eg. vi's I and A move to a useful place before inserting, which is useful for command repetition.  I can encapsulate a move then an edit with macros, which I want to use more frequently and have easily accessible.  But the command repetition of evil-mode is just so convenient, and if I implement something similar it would be nice to have a “move to relevant location then insert” as a single command to be captured for repetition.
          ;; TODO - what verbs?  Tree promote/demote, but eg. for paren trees we care about which kind of paren/bracket/brace/etc is used, or for xml we need a specific tag.  Tree splice - works for symex and xml, but less clearly useful for outline-mode or indent trees.  Tree change node type, eg. symex change paren type, xml change tag.  Tree raise - IE replace parent with child, except I'm used to the workflow of select-element, copy, expand to parent, paste.
          ))
        (objects
         .
         ((character (default-verb . move)
                     (location-within . beginning) ;; Most objects support location-within, which can be 'beginning, 'end, 'emacs-style (to match emacs-style going to end when forward, or beginning when backward), 'reverse-emacs-style, or in some cases a number (eg. the Nth tree child, a line or column number, or some other kind of offset), or nil (nil is mostly needed for matching region expansion instead of forward/backward movement).  Though since I'm partial to using beginning as a default and end sometimes as a modifier, most things only have those implemented so far...
                     (specific . ,nil) ;; This is specific to character, to implement something like vi's find character in line, but more consistent with this system.
                     )
          (word (default-verb . move) (location-within . beginning))
          (vi-like-word (default-verb . move) (location-within . beginning))
          (sentence (default-verb . move) (location-within . beginning))
          (paragraph (default-verb . move) (location-within . beginning))
          (line (default-verb . move) (location-within . beginning))
          (symbol (default-verb . move) (location-within . beginning))
          (sptw (default-verb . move) (location-within . beginning) (delimiter . ,nil) (respect-tree . ,t))
          (indent-tree (default-verb . move) (location-within . beginning))
          (outline (default-verb . move) (location-within . beginning))
          (tstw-qd (default-verb . move) (location-within . anchor))
          (region)
          (buffer (default-verb . move) (location-within . ,nil))

          (url (default-verb . move) (location-within . beginning))
          (email (default-verb . move) (location-within . beginning))


          ;; These are not really “text objects”, but I want composition with delete, change, yank, etc, to work with these.
          (repeatable-motion-repeat (default-verb . move))
          (isearch-new (default-verb . move))
          (isearch-repeat (default-verb . move))
          (goto-marker (default-verb . move))
          (goto-marker-line (default-verb . move))

          ;; TODO - some objects that I don't really have anything implemented for, but that I want.
          (treesitter) ;; TODO - I probably want some modifiers or alternate objects for various things within tree-sitter, to target specific nodes or families of nodes.  Eg. I want to be able to select operators but also use having the cursor at an operator as an anchor point to mean that it handles the tree for that operator, and I want to be able to select a function name and argument list both, but also the overall function call node, etc.  Maybe I should use a symbol or identifier object for operators and function names, while making the overall tree object at those points operate on the larger parse nodes those points represent?  Or maybe to select a function arg list, I should use the smartparens object, but making the treesitter object on the parens mean the function call.  Probably using the parens as the call anchor point is best, since higher-order calls and bracket indexing can lead to multiple levels of application and index nodes for a single function name.
          (xml-tag)
          (xml (default-verb . move) (location-within . beginning))
          (json) ;; Do I care about json outside of other things that already handle it?  Eg. smartparens does a good job with it, if not perfect, and treesitter probably does a good job.  But maybe I could improve handling of eg. commas with slurp/barf if I have a json-specific object.
          (function-arg)
          (definition) ;; this could be defun in elisp, and hopefully for treesitter I can typically find a good node to make this go to.  How much do I want to lean in to different grammar nodes?  Eg. I'm pretty sure that I would like argument/parameter, maybe I want function definition, class definition, to select specific types of nested definitions.  Helix has objects for function, class/type, argument, comment, test, and change.  I should think about all of the different kinds of AST nodes that would be nice to target specifically.
          ;; TODO - buffer-change object for changes in the buffer.  But to be useful it maybe requires monitoring changes and updating offsets of changes and syncing with undo.  So I'm not sure how useful that is vs the complexity of implementing it well.
          ;; TODO - VCS change object.  Select or move between changes in git.
          ;; TODO - I want modifiers for respecting or not respecting tree bounds.  Eg. I typically want go-to-sibling for tree things that don't go out to cousin nodes.  But sometimes it is convenient to just go to the start of the next thing not caring about tree siblings.  But maybe most of the places where I want to disrespect trees are for specific kinds of nodes.  Eg. I want a convenient “go to next/prev function definition”, but I rarely want “go to next expression disregarding tree shape”, or “go to next argument” that goes out to some other function call.
          ;; TODO - I want some modifier to go to a tree node with a given tag.  Eg. this could be a lisp form that starts with a particular symbol, or a specific xml tag, or a treesitter node of particular type.  For org-mode or indent-tree it could be a particular indentation depth or something that I can match about the header or line.
          ))
        (match-table
         .
         (
          ;; TODO - consider using more params and matching less.  I can replace several matches of forward/back begin/end by turning those things into parameters, if there is a function that implements them.  Though I need to consider repeatable motion.
          (move repeatable-motion-repeat
                ((direction forward))
                (repeatable-motion-forward (num)))
          (move repeatable-motion-repeat
                ((direction backward))
                (repeatable-motion-backward (num)))
          ;; TODO - isearch-new should still take a num arg
          (move isearch-new
                ((direction forward))
                (wgh/isearch-start-forward ()))
          (move isearch-new
                ((direction backward))
                (wgh/isearch-start-backward ()))
          (move isearch-repeat
                ((direction forward))
                (wgh/isearch-repeat-forward (num)))
          (move isearch-repeat
                ((direction backward))
                (wgh/isearch-repeat-backward (num)))
          (move goto-marker () (wgh/evil-goto-marker ()))
          (move goto-marker-line () (wgh/evil-goto-marker-line ()))


          (move buffer
                ((direction ,nil) (expand-region ,t))
                (,(lambda ()
                    (activate-mark)
                    (set-mark (point-min))
                    (goto-char (point-max)))
                 ()))
          (move buffer ((direction forward) (location-within ,nil)) (,(lambda () (goto-char (point-max))) ()))
          (move buffer ((direction backward) (location-within ,nil)) (,(lambda (x) (goto-char x)) (num)))
          (move buffer ((location-within beginning)) (,(lambda () (goto-char (point-min))) ()))
          (move buffer ((location-within end)) (,(lambda () (goto-char (point-max))) ()))
          (move buffer ((location-within x ,(lambda (actual expected) (numberp actual)))) (,(lambda (x) (goto-char x)) (location-within)))
          ;; TODO - how do I want to encode go-to-line-number-X and go-to-column-X?  I'm considering an “absolute” modifier, that ignores forward/backward to go to the Nth thing.  But that's probably only useful for going to numbered line, column, or point.  I have buffer with a numeric arg as going to point.  Do I want absolute-char to mean column, and absolute line to mean go-to-line?

          ;; TODO - expand region to specific char inner/outer -- good for ad-hoc regions delimited by the same character, can be used for '' strings and "" strings that don't have escapes, for $$ regions in latex, etc.
          (move character
                ((direction forward) (specific ,t) (location-within beginning))
                (rmo/wgh/find-char-beginning-in-line-forward) (num))
          (move character
                ((direction backward) (specific ,t) (location-within beginning))
                (rmo/wgh/find-char-beginning-in-line-backward) (num))
          (move character
                ((direction forward) (specific ,t) (location-within end))
                (rmo/wgh/find-char-end-in-line-forward) (num))
          (move character
                ((direction backward) (specific ,t) (location-within end))
                (rmo/wgh/find-char-end-in-line-backward) (num))
          (move character
                ((direction forward) (specific nil))
                (rmo/forward-char (num)))
          (move character
                ((direction backward) (specific nil))
                (rmo/backward-char (num)))

          ;; TODO - I really want more kinds of word objects, eg. to select camelCase word parts, to transpose them keeping the overall casing correct, etc.  Is there a useful way that I can specify ad-hoc word types and what delimiters should be allowed?  Eg. people use each of camelCase, snake_case, kebab-case, and more.  Sometimes they are mixed together or with extra separators like / or sigils, and can have ad-hoc meaning for the hierarchy of the different groupings.  Some people balk at mixing, but using more than one kind of case can be useful and meaningful, and sometimes mixing is out of your hands.  But I want tools to easily use all of these and select sub-parts at different levels of granularity.
          (move word
                ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-word ()))
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
          (move word
                ((direction forward) (location-within emacs-style))
                (rmo/forward-word (num)))
          (move word
                ((direction backward) (location-within emacs-style))
                (rmo/backward-word (num)))

          (move vi-like-word
                ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-vi-like-word ()))
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
                ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-symbol ()))
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
          (move symbol
                ((direction forward) (location-within emacs-style))
                (rmo/forward-symbol (num)))
          (move symbol
                ((direction backward) (location-within emacs-style))
                (rmo/backward-symbol (num)))

          (move sentence
                ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-sentence ()))
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
          (move sentence
                ((direction forward) (location-within emacs-style))
                (rmo/forward-sentence (num)))
          (move sentence
                ((direction backward) (location-within emacs-style))
                (rmo/backward-sentence (num)))

          (move paragraph
                ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-paragraph ()))
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
          (move paragraph
                ((direction forward) (location-within emacs-style))
                (rmo/forward-paragraph (num)))
          (move paragraph
                ((direction backward) (location-within emacs-style))
                (rmo/backward-paragraph (num)))

          (move line
                ((direction ,nil) (expand-region inner))
                (,(lambda () (wgh/expand-region-to-fill-lines nil)) ()))
          (move line
                ((direction ,nil) (expand-region ,t))
                (,(lambda () (wgh/expand-region-to-fill-lines t)) ()))
          (move line
                ((direction forward) (location-within beginning))
                (rmo/wgh/forward-line-no-newline-beginning (num)))
          (move line
                ((direction backward) (location-within beginning))
                (rmo/wgh/backward-line-no-newline-beginning (num)))
          (move line
                ((direction forward) (location-within end))
                (rmo/wgh/forward-line-no-newline-end (num)))
          (move line
                ((direction backward) (location-within end))
                (rmo/wgh/backward-line-no-newline-end (num)))
          ;; TODO - emacs-style, in keeping with the other things that are emacs-style but not in keeping with emacs next-line/previous-line,  should move forward to the end of the current line if not at the end of the line, then to the end of the next line, and going backwards should go to the start of the line first, then to the start of the previous line.
          (move line
                ((direction forward) (location-within keep-if-possible))
                (rmo-c/wgh/next-line (num)))
          (move line
                ((direction backward) (location-within keep-if-possible))
                (rmo-c/wgh/prev-line (num)))

          ;; TODO -- for all tree objects, I want a convenient modifier to go all the way to the root.  I guess I could give a big number and rely on it stopping when hitting the root, but it would be nice to have something symbolic to go to the root, or maybe to depth N.
          (move sptw
                ((direction ,nil) (expand-region ,t) (delimiter any))
                (sptw-expand-region-to-any-delimiter ()))
          (move sptw
                ((direction ,nil) (expand-region inner) (delimiter any))
                (sptw-expand-region/children-region ()))

          (move sptw
                ((direction ,nil) (expand-region ,t) (delimiter t ,(lambda (actual expected) (stringp actual))))
                (sptw-expand-region-to-delimiter (delimiter)))
          (move sptw
                ((direction ,nil) (expand-region inner) (delimiter t ,(lambda (actual expected) (stringp actual))))
                (sptw-expand-region-to-delimiter/children-region (delimiter)))

          (move sptw
                ((direction ,nil) (expand-region ,t) (delimiter ,nil))
                (sptw-expand-region ()))
          (move sptw
                ((direction ,nil) (expand-region inner) (delimiter ,nil))
                (sptw-expand-region/children-region ()))

          (move sptw
                ((direction forward) (tree-traversal inorder))
                (rmo/sptw-forward-inorder-traversal (num)))
          (move sptw
                ((direction backward) (tree-traversal inorder))
                (rmo/sptw-backward-inorder-traversal (num)))
          (move sptw
                ((direction forward) (location-within beginning) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,t))
                (rmo/sptw-forward-sibling-beginning (num)))
          (move sptw
                ((direction backward) (location-within beginning) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,t))
                (rmo/sptw-backward-sibling-beginning (num)))
          (move sptw
                ((direction forward) (location-within end) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,t))
                (rmo/sptw-forward-sibling-end (num)))
          (move sptw
                ((direction backward) (location-within end) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,t))
                (rmo/sptw-backward-sibling-end (num)))
          (move sptw
                ((direction forward) (location-within emacs-style) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/sp-forward-sexp (num)))
          (move sptw
                ((direction backward) (location-within emacs-style) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/sp-backward-sexp (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical up) (tree-inner ,nil))
                (rmo/sptw-up-parent-end (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical up) (tree-inner ,nil))
                (rmo/sptw-up-parent-beginning (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical down))
                (rmo/sptw-down-first-child-beginning (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical down))
                (rmo/sptw-down-last-child-end (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical up) (tree-inner ,t))
                (rmo/sp-end-of-sexp (num)))
          (move sptw
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical up) (tree-inner ,t))
                (rmo/sp-beginning-of-sexp (num)))
          (slurp sptw
                 ((direction forward))
                 (sptw-forward-slurp (num)))
          (slurp sptw
                 ((direction backward))
                 (sptw-backward-slurp (num)))
          (barf sptw
                ((direction forward))
                (sptw-forward-barf (num)))
          (barf sptw
                ((direction backward))
                (sptw-backward-barf (num)))
          (split sptw
                 ()
                 (sp-split-sexp (num)))



          (move tstw-qd
                ((direction ,nil) (expand-region ,t))
                (tstw-qd-expand-region ()))
          (move tstw-qd
                ((direction ,nil) (expand-region inner))
                (tstw-qd-expand-region/children-region ()))

          (move tstw-qd
                ((direction forward) (tree-traversal inorder))
                (rmo/tstw-qd-forward-inorder-traversal (num)))
          (move tstw-qd
                ((direction backward) (tree-traversal inorder))
                (rmo/tstw-qd-backward-inorder-traversal (num)))
          (move tstw-qd
                ((direction forward) (location-within anchor) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/tstw-qd-forward-sibling-anchor-point (num)))
          (move tstw-qd
                ((direction backward) (location-within anchor) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/tstw-qd-backward-sibling-anchor-point (num)))
          (move tstw-qd
                ((direction forward) (tree-vertical up) (tree-inner ,nil))
                (rmo/tstw-qd-up-to-parent-anchor-point (num)))
          (move tstw-qd
                ((direction backward) (tree-vertical up) (tree-inner ,nil))
                (rmo/tstw-qd-up-to-parent-anchor-point (num)))
          (move tstw-qd
                ((direction backward) (tree-vertical down))
                (rmo/tstw-qd-down-to-first-child-anchor-point (num)))
          (move tstw-qd
                ((direction forward) (tree-vertical down))
                (rmo/tstw-qd-down-to-last-child-anchor-point (num)))



          ;; TODO - for all xml stuff, I need to add repeatable motion, and consider wrapping nxml to explicitly go to begin/end, etc.  Also, a quick test shows this not working.  I'll figure it out later.
          (move xml
                ((direction forward) (location-within beginning) (tree-vertical ,nil))
                (nxml-forward-element (num)))
          (move xml
                ((direction backward) (location-within beginning) (tree-vertical ,nil))
                (nxml-backward-element (num)))
          (move xml
                ((direction backward) (tree-vertical ,t))
                (nxml-backward-up-element))
          (move xml
                ((direction forward) (tree-vertical ,t))
                (nxml-up-element))

          (move outline
                ((direction ,nil) (expand-region ,t))
                (wgh/outline-expand-region ()))
          (move outline
                ((direction ,nil) (expand-region inner))
                (wgh/outline-expand-region/children-region ()))
          (move outline
                ((direction forward) (tree-traversal inorder))
                (rmo/wgh/outline-inorder-traversal-forward (num)))
          (move outline
                ((direction backward) (tree-traversal inorder))
                (rmo/wgh/outline-inorder-traversal-backward (num)))
          (move outline
                ((direction forward) (location-within beginning) (tree-vertical ,nil))
                (rmo/outline-forward-same-level (num)))
          (move outline
                ((direction backward) (location-within beginning) (tree-vertical ,nil))
                (rmo/outline-backward-same-level (num)))
          (move outline
                ((tree-vertical up))
                (rmo/outline-up-heading (num)))
          (move outline
                ((tree-vertical down) (direction backward))
                (rmo/wgh/outline-down-to-first-child (num)))
          (move outline
                ((tree-vertical down) (direction forward))
                (rmo/wgh/outline-down-to-last-child (num)))
          ;; TODO - end of outline?
          (slurp outline
                 ((direction forward))
                 (wgh/outline-forward-slurp-heading ()))
          (barf outline
                ((direction forward))
                (wgh/outline-forward-barf-heading ()))


          (move indent-tree
                ((direction ,nil) (expand-region ,t))
                (indent-tree-expand-region ()))
          (move indent-tree
                ((direction ,nil) (expand-region inner))
                (indent-tree-expand-region/children-region ()))
          (move indent-tree
                ((direction forward) (tree-traversal inorder))
                (rmo/indent-tree-inorder-traversal-forward (num)))
          (move indent-tree
                ((direction backward) (tree-traversal inorder))
                (rmo/indent-tree-inorder-traversal-backward (num)))
          (move indent-tree
                ((direction forward) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/indent-tree-forward-full-or-half-sibling (num)))
          (move indent-tree
                ((direction backward) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/indent-tree-backward-full-or-half-sibling (num)))
          (move indent-tree
                ((tree-vertical up))
                (rmo/indent-tree-up-to-parent (num)))
          (move indent-tree
                ((tree-vertical down) (direction backward))
                (rmo/indent-tree-down-to-first-child (num)))
          (move indent-tree
                ((tree-vertical down) (direction forward))
                (rmo/indent-tree-down-to-last-child (num)))


          (move url ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-url))
          (move url ((direction forward) (location-within beginning))
                (rmo/wgh/forward-url-beginning (num)))
          (move url ((direction backward) (location-within beginning))
                (rmo/wgh/backward-url-beginning (num)))
          (move url ((direction forward) (location-within end))
                (rmo/wgh/forward-url-end (num)))
          (move url ((direction backward) (location-within end))
                (rmo/wgh/backward-url-end (num)))
          (move email ((direction ,nil) (expand-region ,t))
                (wgh/expand-region-to-email))
          (move email ((direction forward) (location-within beginning))
                (rmo/wgh/forward-email-beginning (num)))
          (move email ((direction backward) (location-within beginning))
                (rmo/wgh/backward-email-beginning (num)))
          (move email ((direction forward) (location-within end))
                (rmo/wgh/forward-email-end (num)))
          (move email ((direction backward) (location-within end))
                (rmo/wgh/backward-email-end (num)))

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
          (transpose tstw-qd ((direction forward)) (tstw-qd-transpose-sibling-forward (num)))
          (transpose tstw-qd ((direction backward)) (tstw-qd-transpose-sibling-backward (num)))
          (transpose outline ((direction forward)) (wgh/outline-transpose-sibling-forward (num)))
          (transpose outline ((direction backward)) (wgh/outline-transpose-sibling-backward (num)))
          (transpose indent-tree ((direction forward)) (indent-tree-transpose-sibling-forward (num)))
          (transpose indent-tree ((direction backward)) (indent-tree-transpose-sibling-backward (num)))

          ;; TODO - rename and put this vilish-open-line stuff... somewhere reasonable
          (open line ((direction forward)) (vilish-open-line-below))
          (open line ((direction backward)) (vilish-open-line-above))
          (open outline ((direction forward) (tree-vertical ,nil)) (,(lambda () (wgh/outline-add-heading-below) (estate-insert-state))))
          (open outline ((direction backward) (tree-vertical ,nil)) (,(lambda () (wgh/outline-add-heading-above) (estate-insert-state))))
          (open outline ((tree-vertical down)) (,(lambda () (message "TODO - implement open outline child"))))
          (open indent-tree ((direction forward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'indent-tree-open-sibling-forward))))
          (open indent-tree ((direction backward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'indent-tree-open-sibling-backward))))
          (open indent-tree ((tree-vertical down)) (,(lambda () (message "TODO - implement open indent-tree child"))))
          (open sptw ((direction forward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'sptw-open-sibling-forward)) ()))
          (open sptw ((direction backward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'sptw-open-sibling-backward)) ()))
          ;; TODO - symex open - ignore unwrapped forms and open a sibling form with the same paren type, hopefully matching indentation...

          (split line () (,(lambda () (open-line 1))))
          (split sptw () (sp-split-sexp))
          ;; TODO - is there something useful to do for split for outline or indent tree?  For symex or XML it has obvious meaning, but is used in the middle of a thing.  Maybe for outline it means to split the parent on the current header, inserting a new header above at the parent level.  And similar for indent tree.  Need to implement this...
          ;; TODO - split for non-tree objects has reasonably defined meaning, I suppose, but isn't very interesting.

          ;; TODO - I need to fix my join-line implementation to take a numerical argument
          (join line ((direction forward)) (,(lambda () (join-line/default-forward nil))))
          (join line ((direction backward)) (,(lambda () (join-line/default-forward -1))))
          (join sptw ((direction backward)) (sptw-join-sexp-backward (num)))
          (join sptw ((direction forward)) (sptw-join-sexp-forward (num)))
          ;; TODO - sptw - make a join-sexp function that takes a forward or backward argument


          ;; TODO - optional register for delete to be delete-copy
          ;; TODO - deduplicate these operations that delegate to the move command...
          (delete region
                  ()
                  (,(lambda () (delete-region (region-beginning) (region-end)))))
          (delete ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(lambda (sentence-with-defaults)
                      (let ((orig-point (point))
                            (new-point (save-mark-and-excursion
                                         (command-sentence-execute
                                          ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                                          (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                                          command-sentence-current-configuration)
                                         (if (region-active-p)
                                             (cons (region-beginning) (region-end))
                                           (point)))))
                        (if (consp new-point)
                            (delete-region (car new-point) (cdr new-point))
                          (delete-region (min orig-point new-point)
                                         (max orig-point new-point)))))
                   sentence-with-defaults))

          (change region
                  ()
                  (,(lambda ()
                      (estate-insert-state-with-thunk
                       (lambda () (delete-region (region-beginning) (region-end)))))))
          (change ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(lambda (sentence-with-defaults)
                      (let ((orig-point (point))
                            (new-point (save-mark-and-excursion
                                         (command-sentence-execute
                                          ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                                          (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                                          command-sentence-current-configuration)
                                         (if (region-active-p)
                                             (cons (region-beginning) (region-end))
                                           (point)))))
                        (estate-insert-state-with-thunk
                         (lambda () (if (consp new-point)
                                        (delete-region (car new-point) (cdr new-point))
                                      (delete-region (min orig-point new-point)
                                                     (max orig-point new-point)))))))
                   sentence-with-defaults))

          (copy region
                ()
                (estate-copy))
          (copy ,(lambda (x) (not (memq x '(region))))
                ()
                (,(lambda (sentence-with-defaults)
                    (save-mark-and-excursion
                      (let ((orig-point (point))
                            (new-point (save-mark-and-excursion
                                         (command-sentence-execute
                                          ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                                          (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                                          command-sentence-current-configuration)
                                         (if (region-active-p)
                                             (cons (region-beginning) (region-end))
                                           (point)))))
                        (estate-copy (if (consp new-point)
                                         new-point
                                       (cons (min orig-point new-point)
                                             (max orig-point new-point)))))))
                 sentence-with-defaults))

          (upcase region
                  ()
                  (,(lambda () (require 'evil) (evil-upcase (region-beginning) (region-end)))))
          (upcase ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(lambda (sentence-with-defaults)
                      (let ((orig-point (point))
                            (new-point (save-mark-and-excursion
                                         (command-sentence-execute
                                          ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                                          (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                                          command-sentence-current-configuration)
                                         (if (region-active-p)
                                             (cons (region-beginning) (region-end))
                                           (point)))))
                        (require 'evil)
                        (if (consp new-point)
                            (evil-upcase (car new-point) (cdr new-point))
                          (evil-upcase (min orig-point new-point)
                                       (max orig-point new-point)))))
                   sentence-with-defaults))
          (downcase region
                    ()
                    (,(lambda () (require 'evil) (evil-downcase (region-beginning) (region-end)))))
          (downcase ,(lambda (x) (not (memq x '(region))))
                    ()
                    (,(lambda (sentence-with-defaults)
                        (let ((orig-point (point))
                              (new-point (save-mark-and-excursion
                                           (command-sentence-execute
                                            ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                                            (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                                            command-sentence-current-configuration)
                                           (if (region-active-p)
                                               (cons (region-beginning) (region-end))
                                             (point)))))
                          (require 'evil)
                          (if (consp new-point)
                              (evil-downcase (car new-point) (cdr new-point))
                            (evil-downcase (min orig-point new-point)
                                           (max orig-point new-point)))))
                     sentence-with-defaults))


          ))))

;; temporary convenience...
(setq command-sentence-current-configuration command-sentence--my-config)

(provide 'command-sentence)
