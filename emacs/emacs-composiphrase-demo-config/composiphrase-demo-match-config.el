;;; -*- lexical-binding: t; -*-

;; TODO - are there things in this demo file that break if other things aren't required yet?

(require 'composiphrase)

(defun composiphrase--make-movement-delegated-command (f-on-region &optional pass-sentence)
  "F-ON-REGION must take two args, region start and region end."
  (lambda (sentence-with-defaults)
    (let ((orig-point (point))
          (new-point (save-mark-and-excursion
                       (composiphrase-execute
                        ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                        (cons '((word-type . verb) (contents . move)) sentence-with-defaults)
                        composiphrase-current-configuration)
                       (if (region-active-p)
                           (cons (region-beginning) (region-end))
                         (point)))))
      (if (consp new-point)
          (apply f-on-region (car new-point) (cdr new-point)
                 (and pass-sentence (list sentence-with-defaults)))
        (apply f-on-region
               (min orig-point new-point)
               (max orig-point new-point)
               (and pass-sentence (list sentence-with-defaults)))))))

(setq composiphrase-demo-match-config
      `((verbs
         .
         ((move (direction . forward) ;; options: forward, backward, expand-region
                (tree-vertical . ,nil) ;; options: nil, 'up, 'down
                ;; TODO - inner is very specific to smartparens object so I can go up to inner parent...
                (inner . ,nil) ;; boolean.  Somewhat specific to trees with delimiters (eg. smartparens) to go up to delimiter or just inside delimiter, but also for tree child area selection.
                (tree-traversal . ,nil) ;; nil or 'inorder
                ;;(num . 1) ;; Things that take a number typically allow nil for one.  But goto-column should default to 0.
                )
          ;; TODO - what arguments do most of these mean?  Eg. tree movement also needs arguments about up/down, when going down you can have a child index, etc.  I generally want an idempotence argument for movements, though maybe it's really only useful for things like “go to the end of the line” without going to the next line, but it's likely a useful option in principle especially for keyboard macros.  Also want movement to sibling vs strict full/half sibling (indent/org trees) vs unbound by tree (eg. move to next s-expression beginning whether or not it moves out of the current tree).
          ;; TODO - add modifier limit-to-single-line.
          ;; TODO - modifier for surrounding white space.  Eg. for expand-region to expand to the object, then expand again to include surrounding white space.  But there are several versions of this that I may want... sometimes I want all surrounding space, sometimes just the space before or after.  And for lines, by default I want to include the white space, but I want a modifier for eg. back-to-indentation and a similar one for the line end before extra white space.  So I probably want the key to invert for that case, or have some kind of special handling.
          (delete (register . ,(lambda () cpo-delete-default-register)))
          (change (register . ,(lambda () cpo-change-default-register)))
          (copy (register . ,(lambda () cpo-copy-default-register)))
          (paste-to-region-from-move (register . ,(lambda () cpo-paste-default-register))
                                     (register-for-old . ,(lambda () cpo-paste-copy-old-default-register))) ;; IE form a region by moving, paste into that region.
          (move-paste (register . ,(lambda () cpo-paste-default-register))
                      (register-for-old . ,(lambda () cpo-paste-copy-old-default-register))) ;; IE move, then paste at the new point, but restoring the original point.
          (move-insert)
          (upcase)
          (downcase)
          (toggle-case)
          (capitalize) ;; TODO - what is the difference between capitalize-region and upcase-initials-region?
          (indent)
          (dedent)
          (format)
          (comment)
          (uncomment)
          (toggle-comment)
          (initiate-isearch) ;; TODO - I want to write something similar to vim's * command, to search the thing at point, except maybe with an argument to choose word or symbol or whatnot.  But my first attempt at actually implementing it was unsuccessful.  I'll probably come back to it later.
          (transpose (direction . forward) (tree-vertical . nil) (num . 1))
          (join (direction . forward) (num . 1))
          (split)
          (slurp (direction . forward) (num . 1))
          (barf (direction . forward) (num . 1))
          (open (direction . forward)) ;; IE new sibling
          ;; TODO - none of the below are really implemented yet.
          (promote)
          (demote) ;; For symex you should be prompted to choose a tag to wrap with, and for xml you should be prompted for a tag.
          (change-delimiter) ;; This really only makes sense for a few things... how many operators do I want to have that aren't really composable?  That said, it's a common operation, so I want it to be convenient in the layout even if it doesn't apply to most objects.
          ;; TODO - something like insert-move, eg. vi's I and A move to a useful place before inserting, which is useful for command repetition.  I can encapsulate a move then an edit with macros, which I want to use more frequently and have easily accessible.  But the command repetition of evil-mode is just so convenient, and if I implement something similar it would be nice to have a “move to relevant location then insert” as a single command to be captured for repetition.
          ;; TODO - what verbs?  Tree promote/demote, but eg. for paren trees we care about which kind of paren/bracket/brace/etc is used, or for xml we need a specific tag.  Tree splice - works for symex and xml, but less clearly useful for outline-mode or indent trees.  Tree change node type, eg. symex change paren type, xml change tag.  Tree raise - IE replace parent with child, except I'm used to the workflow of select-element, copy, expand to parent, paste.
          ;; TODO - there are many commands that can operate on a region, and any of them could be put into the map similar to delete, change, yank, upcase, etc.  Examples: eval-region, fill-region, center-region, indent-region, ispell-region, flyspsell-region, comment-region, uncomment-region, printify-region (replace non-printing characters), ... many silly ones: print-region, kkc-region, rot13-region, morse-region, unmorse-region, nato-region, denato-region, encrypt/decrypt region, base64-encode/decode, ... most of these, even the ones that are less silly, are probably not very useful to have in the composable command map, because you wouldn't really care to use them on the various text objects.
          ;; TODO - reverse-region reverses the lines in the region, but it would be cool to have a reverse command that takes a region and an object and reverses the objects in the region, or works on the children of a tree type.
          ))
        (objects
         .
         ((character (default-verb . move)
                     (location-within . beginning) ;; Most objects support location-within, which can be 'beginning, 'end, 'emacs-style (to match emacs-style going to end when forward, or beginning when backward), 'reverse-emacs-style, or in some cases a number (eg. the Nth tree child, a line or column number, or some other kind of offset), or nil (nil is mostly needed for matching region expansion instead of forward/backward movement).  Though since I'm partial to using beginning as a default and end sometimes as a modifier, most things only have those implemented so far...
                     (specific . ,nil) ;; This is specific to character, to implement something like vi's find character in line, but more consistent with this system.
                     )
          (word (default-verb . move) (location-within . beginning))
          (cpo-vi-like-word (default-verb . move) (location-within . beginning))
          (sentence (default-verb . move) (location-within . beginning))
          (paragraph (default-verb . move) (location-within . beginning))
          (line (default-verb . move) (location-within . beginning))
          (symbol (default-verb . move) (location-within . beginning))
          (sexp (default-verb . move) (location-within . beginning))
          (list (default-verb . move) (location-within . beginning))
          (cpo-smartparens (default-verb . move) (location-within . beginning) (respect-tree . respect-tree) (delimiter . ,nil))
          (cpo-indent-tree (default-verb . move) (location-within . beginning) (respect-tree . respect-tree))
          (outline (default-verb . move) (location-within . beginning) (respect-tree . respect-tree))
          (cpo-treesitter-qd (default-verb . move) (location-within . anchor) (respect-tree . respect-tree)) ;; IE treesitter generic handler
          ;; TODO - other tree-sitter things that are more tuned to the language.  Well, maybe it would be good to have the generic cpo-treesitter-qd and a more specific one on the same map, where the specific one pulls in some language config.  I think it's likely worthwhile to still keep a generic one in the map, though.
          (region)
          (buffer (default-verb . move) (location-within . ,nil))

          ;; TODO - the implementations of these are pretty egregious.  Selection is ok, but I also wanted movement, so I just wrote something quick and dirty and extremely slow.  Ideally every kind of object would at least support movement and selection.
          (url (default-verb . move) (location-within . beginning))
          (email (default-verb . move) (location-within . beginning))


          ;; These are not really “text objects”, but I want composition with delete, change, yank, etc, to work with these.
          (repeatable-motion-repeat (default-verb . move))
          (isearch-new (default-verb . move))
          (isearch-repeat (default-verb . move))
          (jump-to-register (default-verb . move))

          ;;;;;
          ;; TODO - some objects that I don't really have anything implemented for, but that I want.
          ;;;;;
          (treesitter) ;; TODO - I probably want some modifiers or alternate objects for various things within tree-sitter, to target specific nodes or families of nodes.  Eg. I want to be able to select operators but also use having the cursor at an operator as an anchor point to mean that it handles the tree for that operator, and I want to be able to select a function name and argument list both, but also the overall function call node, etc.  Maybe I should use a symbol or identifier object for operators and function names, while making the overall tree object at those points operate on the larger parse nodes those points represent?  Or maybe to select a function arg list, I should use the smartparens object, but making the treesitter object on the parens mean the function call.  Probably using the parens as the call anchor point is best, since higher-order calls and bracket indexing can lead to multiple levels of application and index nodes for a single function name.
          (xml-tag)
          (xml (default-verb . move) (location-within . beginning))
          (json) ;; Do I care about json outside of other things that already handle it?  Eg. smartparens does a good job with it, if not perfect, and treesitter probably does a good job.  But maybe I could improve handling of eg. commas with slurp/barf if I have a json-specific object.
          (white-space)
          (function-arg)
          (definition) ;; this could be defun in elisp, and hopefully for treesitter I can typically find a good node to make this go to.  How much do I want to lean in to different grammar nodes?  Eg. I'm pretty sure that I would like argument/parameter, maybe I want function definition, class definition, to select specific types of nested definitions.  Helix has objects for function, class/type, argument, comment, test, and change.  I should think about all of the different kinds of AST nodes that would be nice to target specifically.
          (comment)
          (buffer-change) ;; TODO - for going forward/back or selecting a change to the buffer.  This is probably complicated to set up, eg. requiring monitoring changes to offsets and syncing with undo.  May not be worth it.
          (vcs-change) ;; TODO - eg. for going to next change or selecting change boundaries for changes since last git commit.
          (linter-warning) ;; TODO - eg. to move to next compiler/linter warning/error or select its region
          (ai-proposed-edit) ;; TODO - eg. I'm thinking move to or select region of next proposed edit by an AI tool, eg. stuff like cursor compose edit proposals, only an editor-agnostic tool that can have some protocol for communicating edits so emacs can show them nicely.
          ;; TODO - it would be nice to have options for basically any kind of thing that can have its bounds given and can be searched for to move forward/backward to a next one.
          (phone-number)
          (tracking-number)
          (file-name)
          ;; TODO - I want modifiers for respecting or not respecting tree bounds.  Eg. I typically want go-to-sibling for tree things that don't go out to cousin nodes.  But sometimes it is convenient to just go to the start of the next thing not caring about tree siblings.  But maybe most of the places where I want to disrespect trees are for specific kinds of nodes.  Eg. I want a convenient “go to next/prev function definition”, but I rarely want “go to next expression disregarding tree shape”, or “go to next argument” that goes out to some other function call.
          ;; TODO - I want some modifier to go to a tree node with a given tag.  Eg. this could be a lisp form that starts with a particular symbol, or a specific xml tag, or a treesitter node of particular type.  For org-mode or cpo-indent-tree it could be a particular indentation depth or something that I can match about the header or line.
          ))
        (match-table
         .
         (
          ;; TODO - consider using more params and matching less.  I can replace several matches of forward/back begin/end by turning those things into parameters, if there is a function that implements them.  Though I need to consider repeatable motion.

          ;; TODO - absolute modifier -- turns into goto, goto Nth line, Nth column, Nth position.  For trees, go to Nth sibling.  Allow negative number to go from end.  For trees, absolute plus up can mean absolute top-level tree.  I mostly see this as useful for line, column, position, and first/last sibling...
          ;; TODO - matching modifier -- go to the next thing that matches.  Eg. go to the next matching word, the next matching symbol, etc.
          ;; TODO - surrounding-space modifier -- for selection, select with surrounding white space
          ;; TODO - alternate modifier -- some things have some alternate mode.  This modifier is not well specified, and maybe takes a number or something so you can have multiple alternates... it can capture the fact that some modifiers are very noun or very verb specific, giving one or two keys for modifying many things.
          ;; TODO - idempotent modifier -- for movement or selection, don't move when already at a place that such a movement could have moved to.  Useful for keyboard macros, I think.


          (move repeatable-motion-repeat
                ((direction forward))
                (repeatable-motion-forward (num)))
          (move repeatable-motion-repeat
                ((direction backward))
                (repeatable-motion-backward (num)))
          ;; TODO - isearch-new should still take a num arg
          (move isearch-new
                ((direction forward))
                (cpo-isearch-start-forward ()))
          (move isearch-new
                ((direction backward))
                (cpo-isearch-start-backward ()))
          (move isearch-repeat
                ((direction forward))
                (cpo-isearch-repeat-forward (num)))
          (move isearch-repeat
                ((direction backward))
                (cpo-isearch-repeat-backward (num)))
          (move jump-to-register () ((lambda () (call-interactively 'jump-to-register)) ()))


          (move buffer
                ((direction expand-region))
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
          ;; TODO - absolute movement for character -- I've implemented it as column, with alternate absolute movement as absolute position in buffer, (and with absolute buffer position as numeric movement for buffer...) but maybe should swap... I think column is more useful and frequent to have be more convenient...
          (move character
                ((absolute absolute) (alternate alternate))
                (goto-char (num)))
          (move character
                ((absolute absolute))
                (goto-column (num)))
          (move character
                ((direction forward) (specific ,t) (location-within beginning))
                (rmo/cpo-find-char-beginning-in-line-forward (num)))
          (move character
                ((direction backward) (specific ,t) (location-within beginning))
                (rmo/cpo-find-char-beginning-in-line-backward (num)))
          (move character
                ((direction forward) (specific ,t) (location-within end))
                (rmo/cpo-find-char-end-in-line-forward (num)))
          (move character
                ((direction backward) (specific ,t) (location-within end))
                (rmo/cpo-find-char-end-in-line-backward (num)))
          (move character
                ((direction forward) (specific nil))
                (rmo/forward-char (num)))
          (move character
                ((direction backward) (specific nil))
                (rmo/backward-char (num)))

          ;; TODO - I really want more kinds of word objects, eg. to select camelCase word parts, to transpose them keeping the overall casing correct, etc.  Is there a useful way that I can specify ad-hoc word types and what delimiters should be allowed?  Eg. people use each of camelCase, snake_case, kebab-case, and more.  Sometimes they are mixed together or with extra separators like / or sigils, and can have ad-hoc meaning for the hierarchy of the different groupings.  Some people balk at mixing, but using more than one kind of case can be useful and meaningful, and sometimes mixing is out of your hands.  But I want tools to easily use all of these and select sub-parts at different levels of granularity.
          (move word
                ((direction expand-region))
                (cpo-expand-region-to-word ()))
          (move word
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-word-beginning (num)))
          (move word
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-word-beginning (num)))
          (move word
                ((direction forward) (location-within end))
                (rmo/cpo-forward-word-end (num)))
          (move word
                ((direction backward) (location-within end))
                (rmo/cpo-backward-word-end (num)))
          (move word
                ((direction forward) (location-within emacs-style))
                (rmo/forward-word (num)))
          (move word
                ((direction backward) (location-within emacs-style))
                (rmo/backward-word (num)))

          (move cpo-vi-like-word
                ((direction expand-region))
                (cpo-expand-region-to-cpo-vi-like-word ()))
          (move cpo-vi-like-word
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-cpo-vi-like-word-beginning (num)))
          (move cpo-vi-like-word
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-cpo-vi-like-word-beginning (num)))
          (move cpo-vi-like-word
                ((direction forward) (location-within end))
                (rmo/cpo-forward-cpo-vi-like-word-end (num)))
          (move cpo-vi-like-word
                ((direction backward) (location-within end))
                (rmo/cpo-backward-cpo-vi-like-word-end (num)))

          (move symbol
                ((direction expand-region))
                (cpo-expand-region-to-symbol ()))
          (move symbol
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-symbol-beginning (num)))
          (move symbol
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-symbol-beginning (num)))
          (move symbol
                ((direction forward) (location-within end))
                (rmo/cpo-forward-symbol-end (num)))
          (move symbol
                ((direction backward) (location-within end))
                (rmo/cpo-backward-symbol-end (num)))
          (move symbol
                ((direction forward) (location-within emacs-style))
                (rmo/forward-symbol (num)))
          (move symbol
                ((direction backward) (location-within emacs-style))
                (rmo/backward-symbol (num)))

          (move sexp
                ((direction expand-region))
                (cpo-expand-region-to-sexp ()))
          (move sexp
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-sexp-beginning (num)))
          (move sexp
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-sexp-beginning (num)))
          (move sexp
                ((direction forward) (location-within end))
                (rmo/cpo-forward-sexp-end (num)))
          (move sexp
                ((direction backward) (location-within end))
                (rmo/cpo-backward-sexp-end (num)))
          (move sexp
                ((direction forward) (location-within emacs-style))
                (rmo/forward-sexp (num)))
          (move sexp
                ((direction backward) (location-within emacs-style))
                (rmo/backward-sexp (num)))

          (move list
                ((direction expand-region))
                (cpo-expand-region-to-list ()))
          (move list
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-list-beginning (num)))
          (move list
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-list-beginning (num)))
          (move list
                ((direction forward) (location-within end))
                (rmo/cpo-forward-list-end (num)))
          (move list
                ((direction backward) (location-within end))
                (rmo/cpo-backward-list-end (num)))
          (move list
                ((direction forward) (location-within emacs-style))
                (rmo/forward-list (num)))
          (move list
                ((direction backward) (location-within emacs-style))
                (rmo/backward-list (num)))


          (move sentence
                ((direction expand-region))
                (cpo-expand-region-to-sentence ()))
          (move sentence
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-sentence-beginning (num)))
          (move sentence
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-sentence-beginning (num)))
          (move sentence
                ((direction forward) (location-within end))
                (rmo/cpo-forward-sentence-end (num)))
          (move sentence
                ((direction backward) (location-within end))
                (rmo/cpo-backward-sentence-end (num)))
          (move sentence
                ((direction forward) (location-within emacs-style))
                (rmo/forward-sentence (num)))
          (move sentence
                ((direction backward) (location-within emacs-style))
                (rmo/backward-sentence (num)))

          (move paragraph
                ((direction expand-region))
                (cpo-expand-region-to-paragraph ()))
          (move paragraph
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-paragraph-beginning (num)))
          (move paragraph
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-paragraph-beginning (num)))
          (move paragraph
                ((direction forward) (location-within end))
                (rmo/cpo-forward-paragraph-end (num)))
          (move paragraph
                ((direction backward) (location-within end))
                (rmo/cpo-backward-paragraph-end (num)))
          (move paragraph
                ((direction forward) (location-within emacs-style))
                (rmo/forward-paragraph (num)))
          (move paragraph
                ((direction backward) (location-within emacs-style))
                (rmo/backward-paragraph (num)))

          (move line
                ((absolute absolute)
                 ;; TODO - handle expand-region, too
                 ;; TODO - handle position modifiers -- beginning/end
                 )
                (cpo-goto-line-default-first (num)))
          (move line
                ((direction expand-region) (inner inner))
                (,(lambda () (cpo-expand-region-to-fill-lines nil)) ()))
          (move line
                ((direction expand-region) (inner ,nil))
                (,(lambda () (cpo-expand-region-to-fill-lines t)) ()))
          ;; TODO - alternate for back-to-indentation?  I don't love it, but I want this somewhere.
          (move line
                ((direction backward) (alternate alternate))
                (back-to-indentation ()))
          (move line
                ((direction forward) (alternate alternate))
                (TODO-forward-until-trailing-white-space ()))
          (move line
                ((direction forward) (location-within beginning))
                (rmo/cpo-forward-cpo-line-no-newline-beginning (num)))
          (move line
                ((direction backward) (location-within beginning))
                (rmo/cpo-backward-cpo-line-no-newline-beginning (num)))
          (move line
                ((direction forward) (location-within end))
                (rmo/cpo-forward-cpo-line-no-newline-end (num)))
          (move line
                ((direction backward) (location-within end))
                (rmo/cpo-backward-cpo-line-no-newline-end (num)))
          ;; TODO - emacs-style, in keeping with the other things that are emacs-style but not in keeping with emacs next-line/previous-line,  should move forward to the end of the current line if not at the end of the line, then to the end of the next line, and going backwards should go to the start of the line first, then to the start of the previous line.
          (move line
                ((direction forward) (location-within keep-if-possible))
                (rmo-c/cpo-next-line (num)))
          (move line
                ((direction backward) (location-within keep-if-possible))
                (rmo-c/cpo-prev-line (num)))

          ;; TODO - for all trees, make absolute movement go to the Nth sibling.
          (move cpo-smartparens
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-smartparens-select-root ()))
          (move cpo-smartparens
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-smartparens-up-to-root ()))
          (move cpo-smartparens
                ((direction expand-region) (inner ,nil) (delimiter any) (tree-vertical ,nil))
                (rmo/cpo-smartparens-expand-region-to-any-delimiter (num)))
          (move cpo-smartparens
                ((direction expand-region) (inner inner) (delimiter any) (tree-vertical ,nil))
                (rmo/cpo-smartparens-expand-region/children-region (num)))

          (move cpo-smartparens
                ((direction expand-region) (tree-vertical ,nil) (inner ,nil)
                 (delimiter t ,(lambda (actual expected) (stringp actual))))
                (cpo-smartparens-expand-region-to-delimiter (delimiter num)))
          (move cpo-smartparens
                ((direction expand-region) (tree-vertical ,nil) (inner inner) (delimiter t ,(lambda (actual expected) (stringp actual))))
                (cpo-smartparens-expand-region-to-delimiter/children-region (delimiter num)))

          (move cpo-smartparens
                ((direction expand-region) (tree-vertical ,nil) (inner ,nil) (delimiter ,nil))
                (rmo/cpo-smartparens-expand-region (num)))
          (move cpo-smartparens
                ((direction expand-region) (tree-vertical ,nil) (inner inner) (delimiter ,nil))
                (rmo/cpo-smartparens-expand-region/children-region (num)))

          (move cpo-smartparens
                ((direction forward) (tree-vertical ,nil) (tree-traversal inorder))
                (rmo/cpo-smartparens-forward-inorder-traversal (num)))
          (move cpo-smartparens
                ((direction backward) (tree-vertical ,nil) (tree-traversal inorder))
                (rmo/cpo-smartparens-backward-inorder-traversal (num)))
          (move cpo-smartparens
                ((direction forward) (location-within beginning) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-smartparens-forward-sibling-beginning (num)))
          (move cpo-smartparens
                ((direction backward) (location-within beginning) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-smartparens-backward-sibling-beginning (num)))
          (move cpo-smartparens
                ((direction forward) (location-within end) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-smartparens-forward-sibling-end (num)))
          (move cpo-smartparens
                ((direction backward) (location-within end) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-smartparens-backward-sibling-end (num)))
          (move cpo-smartparens
                ((direction forward) (location-within emacs-style) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/sp-forward-sexp (num)))
          (move cpo-smartparens
                ((direction backward) (location-within emacs-style) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/sp-backward-sexp (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical up) (inner ,nil))
                (rmo/cpo-smartparens-up-parent-end (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical up) (inner ,nil))
                (rmo/cpo-smartparens-up-parent-beginning (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical down))
                (rmo/cpo-smartparens-down-first-child-beginning (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical down))
                (rmo/cpo-smartparens-down-last-child-end (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction forward) (tree-vertical up) (inner inner))
                (rmo/sp-end-of-sexp (num)))
          (move cpo-smartparens
                ;; TODO - for my current key binding purposes, I want to use forward/backward to determine begin/end for parent...
                ((direction backward) (tree-vertical up) (inner inner))
                (rmo/sp-beginning-of-sexp (num)))
          (slurp cpo-smartparens
                 ((direction forward))
                 (cpo-smartparens-forward-slurp (num)))
          (slurp cpo-smartparens
                 ((direction backward))
                 (cpo-smartparens-backward-slurp (num)))
          (barf cpo-smartparens
                ((direction forward))
                (cpo-smartparens-forward-barf (num)))
          (barf cpo-smartparens
                ((direction backward))
                (cpo-smartparens-backward-barf (num)))
          (promote cpo-smartparens
                   ((delimiter ,nil))
                   (cpo-smartparens-splice ()))
          (promote cpo-smartparens
                   ((delimiter any))
                   (cpo-smartparens-splice ()))
          (promote cpo-smartparens
                   ((delimiter t ,(lambda (actual expected) (stringp actual))))
                   (TODO-cpo-smartparens-splice-specific-delimiter))
          (demote cpo-smartparens
                  ((delimiter ,nil))
                  (cpo-smartparens-TODO-wrap-with-delimiter-choice-prompt))
          (demote cpo-smartparens
                  ((delimiter t ,(lambda (actual expected) (stringp actual))))
                  (cpo-smartparens-wrap-with-delimiter (delimiter)))
          (demote cpo-smartparens
                  ((delimiter any))
                  ;; Convenience for my keyboard layout.
                  (,(lambda () (cpo-smartparens-wrap-with-delimiter "(")) ()))
          (rewrap cpo-smartparens
                  ((delimiter ,nil))
                  (cpo-smartparens-TODO-rewrap-nearest-delimiter-with-interactive-choice
                   (num)))
          (rewrap cpo-smartparens
                  ((delimiter t ,(lambda (actual expected) (stringp actual))))
                  ;; TODO - I should maybe make this take the command sentence and look for multiple instances of delimiter.  Use the first one given as the target to change, and the second one given as the one to change to.  But then I need to detect when I should expect a second delimiter key... doing an interactive prompt for what to change to will be just as well.
                  (cpo-smartparens-TODO-rewrap-nearest-specific-delimiter-with-interactive-choice
                   (num)))
          (split cpo-smartparens
                 ()
                 (sp-split-sexp (num)))


          (move cpo-treesitter-qd
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-treesitter-qd-select-root ()))
          (move cpo-treesitter-qd
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-treesitter-qd-up-to-root ()))
          (move cpo-treesitter-qd
                ((direction expand-region) (tree-vertical ,nil) (inner ,nil))
                (rmo/cpo-treesitter-qd-expand-region (num)))
          (move cpo-treesitter-qd
                ((direction expand-region) (tree-vertical ,nil) (inner inner))
                (rmo/cpo-treesitter-qd-expand-region/children-region (num)))

          (move cpo-treesitter-qd
                ((direction forward) (tree-traversal inorder))
                (rmo/cpo-treesitter-qd-forward-inorder-traversal (num)))
          (move cpo-treesitter-qd
                ((direction backward) (tree-traversal inorder))
                (rmo/cpo-treesitter-qd-backward-inorder-traversal (num)))
          (move cpo-treesitter-qd
                ((direction forward) (location-within anchor) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/cpo-treesitter-qd-forward-sibling-anchor-point (num)))
          (move cpo-treesitter-qd
                ((direction backward) (location-within anchor) (tree-vertical ,nil) (tree-traversal ,nil))
                (rmo/cpo-treesitter-qd-backward-sibling-anchor-point (num)))
          (move cpo-treesitter-qd
                ((direction forward) (tree-vertical up) (inner ,nil))
                (rmo/cpo-treesitter-qd-up-to-parent-anchor-point (num)))
          (move cpo-treesitter-qd
                ((direction backward) (tree-vertical up) (inner ,nil))
                (rmo/cpo-treesitter-qd-up-to-parent-anchor-point (num)))
          (move cpo-treesitter-qd
                ((direction backward) (tree-vertical down))
                (rmo/cpo-treesitter-qd-down-to-first-child-anchor-point (num)))
          (move cpo-treesitter-qd
                ((direction forward) (tree-vertical down))
                (rmo/cpo-treesitter-qd-down-to-last-child-anchor-point (num)))



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
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-outline-select-root ()))
          (move outline
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-outline-up-to-root ()))
          (move outline
                ((direction expand-region) (tree-vertical ,nil) (inner ,nil))
                (rmo/cpo-outline-expand-region (num)))
          (move outline
                ((direction expand-region) (tree-vertical ,nil) (inner inner))
                (rmo/cpo-outline-expand-region/children-region (num)))
          (move outline
                ((direction forward) (tree-traversal inorder))
                (rmo/cpo-outline-inorder-traversal-forward (num)))
          (move outline
                ((direction backward) (tree-traversal inorder))
                (rmo/cpo-outline-inorder-traversal-backward (num)))
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
                (rmo/cpo-outline-down-to-first-child (num)))
          (move outline
                ((tree-vertical down) (direction forward))
                (rmo/cpo-outline-down-to-last-child (num)))
          ;; (promote outline () (,(lambda () (outline-promote 'subtree)) ()))
          ;; (demote outline () (,(lambda () (outline-demote 'subtree)) ()))
          (promote outline ((alternate alternate)) (,(lambda () (require 'org) (org-promote)) ()))
          (demote outline ((alternate alternate)) (,(lambda () (require 'org) (org-demote)) ()))
          (promote outline () (,(lambda () (require 'org) (org-promote-subtree)) ()))
          (demote outline () (,(lambda () (require 'org) (org-demote-subtree)) ()))
          ;; TODO - commented out because they are broken
          (slurp outline
                 ((direction forward))
                 (cpo-outline-forward-slurp-heading ()))
          (barf outline
                ((direction forward))
                (cpo-outline-forward-barf-heading ()))

          (move cpo-indent-tree
                ;; TODO - the modifiers aren't correct, but I'm not sure where to shoehorn this in, so I'm going to roll with this for now.
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-indent-tree-select-root ()))
          (move cpo-indent-tree
                ;; TODO - the modifiers aren't correct, but I'm not sure where to shoehorn this in, so I'm going to roll with this for now.
                ((tree-vertical up) (direction expand-region) (inner ,nil))
                (cpo-indent-tree-up-to-root ()))
          (move cpo-indent-tree
                ((direction expand-region) (tree-vertical ,nil) (inner ,nil))
                (rmo/cpo-indent-tree-expand-region (num)))
          (move cpo-indent-tree
                ((direction expand-region) (tree-vertical ,nil) (inner inner))
                (rmo/cpo-indent-tree-expand-region/children-region (num)))
          (move cpo-indent-tree
                ((direction forward) (tree-traversal inorder))
                (rmo/cpo-indent-tree-inorder-traversal-forward (num)))
          (move cpo-indent-tree
                ((direction backward) (tree-traversal inorder))
                (rmo/cpo-indent-tree-inorder-traversal-backward (num)))
          (move cpo-indent-tree
                ((direction forward) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-indent-tree-forward-full-sibling (num)))
          (move cpo-indent-tree
                ((direction backward) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree respect-tree))
                (rmo/cpo-indent-tree-backward-full-sibling (num)))
          ;; It's not really fair to say that these half-sibling movements don't respect the tree, but I'm not sure what other modifier to use right now.  It is an alternate way of respecting the tree.
          (move cpo-indent-tree
                ((direction forward) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/cpo-indent-tree-forward-full-or-half-sibling (num)))
          (move cpo-indent-tree
                ((direction backward) (tree-vertical ,nil) (tree-traversal ,nil) (respect-tree ,nil))
                (rmo/cpo-indent-tree-backward-full-or-half-sibling (num)))
          (move cpo-indent-tree
                ((tree-vertical up))
                (rmo/cpo-indent-tree-up-to-parent (num)))
          (move cpo-indent-tree
                ((tree-vertical down) (direction backward))
                (rmo/cpo-indent-tree-down-to-first-child (num)))
          (move cpo-indent-tree
                ((tree-vertical down) (direction forward))
                (rmo/cpo-indent-tree-down-to-last-child (num)))
          (promote cpo-indent-tree () (cpo-indent-tree-promote ()))
          (demote cpo-indent-tree () (cpo-indent-tree-demote ()))


          (move url ((direction expand-region))
                (cpo-expand-region-to-url))
          (move url ((direction forward) (location-within beginning))
                (rmo/cpo-forward-url-beginning (num)))
          (move url ((direction backward) (location-within beginning))
                (rmo/cpo-backward-url-beginning (num)))
          (move url ((direction forward) (location-within end))
                (rmo/cpo-forward-url-end (num)))
          (move url ((direction backward) (location-within end))
                (rmo/cpo-backward-url-end (num)))
          (move email ((direction expand-region))
                (cpo-expand-region-to-email))
          (move email ((direction forward) (location-within beginning))
                (rmo/cpo-forward-email-beginning (num)))
          (move email ((direction backward) (location-within beginning))
                (rmo/cpo-backward-email-beginning (num)))
          (move email ((direction forward) (location-within end))
                (rmo/cpo-forward-email-end (num)))
          (move email ((direction backward) (location-within end))
                (rmo/cpo-backward-email-end (num)))

          ;; TODO - for transpose character, implement something that follows the character explicitly forward/backward.
          (transpose word ((direction forward)) (cpo-transpose-word-forward (num)))
          (transpose word ((direction backward)) (cpo-transpose-word-backward (num)))
          (transpose cpo-vi-like-word ((direction forward)) (cpo-transpose-cpo-vi-like-word-forward (num)))
          (transpose cpo-vi-like-word ((direction backward)) (cpo-transpose-cpo-vi-like-word-backward (num)))
          (transpose symbol ((direction forward)) (cpo-transpose-symbol-forward (num)))
          (transpose symbol ((direction backward)) (cpo-transpose-symbol-backward (num)))
          (transpose sentence ((direction forward)) (cpo-transpose-sentence-forward (num)))
          (transpose sentence ((direction backward)) (cpo-transpose-sentence-backward (num)))
          (transpose paragraph ((direction forward)) (cpo-transpose-paragraph-forward (num)))
          (transpose paragraph ((direction backward)) (cpo-transpose-paragraph-backward (num)))
          (transpose sexp ((direction forward)) (cpo-transpose-sexp-forward (num)))
          (transpose sexp ((direction backward)) (cpo-transpose-sexp-backward (num)))
          (transpose list ((direction forward)) (cpo-transpose-list-forward (num)))
          (transpose list ((direction backward)) (cpo-transpose-list-backward (num)))
          (transpose line ((direction forward)) (cpo-transpose-line-forward (num)))
          (transpose line ((direction backward)) (cpo-transpose-line-backward (num)))
          ;;(transpose url ((direction forward)) (cpo-transpose-url-forward (num)))
          ;;(transpose url ((direction backward)) (cpo-transpose-url-backward (num)))
          ;;(transpose email ((direction forward)) (cpo-transpose-email-forward (num)))
          ;;(transpose email ((direction backward)) (cpo-transpose-email-backward (num)))
          (transpose cpo-smartparens ((tree-vertical up)) (cpo-smartparens-ancestor-reorder (num)))
          (transpose cpo-smartparens ((tree-vertical ,nil) (direction forward)) (cpo-smartparens-transpose-sibling-forward (num)))
          (transpose cpo-smartparens ((tree-vertical ,nil) (direction backward)) (cpo-smartparens-transpose-sibling-backward (num)))
          (transpose cpo-treesitter-qd ((tree-vertical up)) (cpo-treesitter-qd-ancestor-reorder (num)))
          (transpose cpo-treesitter-qd ((tree-vertical ,nil) (direction forward)) (cpo-treesitter-qd-transpose-sibling-forward (num)))
          (transpose cpo-treesitter-qd ((tree-vertical ,nil) (direction backward)) (cpo-treesitter-qd-transpose-sibling-backward (num)))
          (transpose outline ((direction forward)) (cpo-outline-transpose-sibling-forward (num)))
          (transpose outline ((direction backward)) (cpo-outline-transpose-sibling-backward (num)))
          (transpose cpo-indent-tree ((direction forward)) (cpo-indent-tree-transpose-sibling-forward (num)))
          (transpose cpo-indent-tree ((direction backward)) (cpo-indent-tree-transpose-sibling-backward (num)))

          (open line ((direction forward)) (,(lambda () (estate-insert-state-with-thunk 'cpo-open-line-below))))
          (open line ((direction backward)) (,(lambda () (estate-insert-state-with-thunk 'cpo-open-line-above))))
          (open outline ((direction forward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-outline-add-heading-below))))
          (open outline ((direction backward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-outline-add-heading-above))))
          (open outline ((direction forward) (tree-vertical down)) (,(lambda (num) (estate-insert-state-with-thunk (lambda () (cpo-outline-add-child-heading num))))
                                                                    (num)))
          (open outline ((direction forward) (tree-vertical up)) (,(lambda (num) (estate-insert-state-with-thunk (lambda () (cpo-outline-add-ancestor-next-sibling-heading num))))
                                                                  (num)))
          (open cpo-indent-tree ((direction forward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-indent-tree-open-sibling-forward))))
          (open cpo-indent-tree ((direction backward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-indent-tree-open-sibling-backward))))
          (open cpo-indent-tree ((tree-vertical down)) (,(lambda () (message "TODO - implement open cpo-indent-tree child"))))
          (open cpo-smartparens ((direction forward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-smartparens-open-sibling-forward)) ()))
          (open cpo-smartparens ((direction backward) (tree-vertical ,nil)) (,(lambda () (estate-insert-state-with-thunk 'cpo-smartparens-open-sibling-backward)) ()))
          ;; TODO - symex open - ignore unwrapped forms and open a sibling form with the same paren type, hopefully matching indentation...

          (split line () (,(lambda () (open-line 1))))
          (split cpo-smartparens () (sp-split-sexp))
          ;; TODO - is there something useful to do for split for outline or indent tree?  For symex or XML it has obvious meaning, but is used in the middle of a thing.  Maybe for outline it means to split the parent on the current header, inserting a new header above at the parent level.  And similar for indent tree.  Need to implement this...
          ;; TODO - split for non-tree objects has reasonably defined meaning, I suppose, but isn't very interesting.

          ;; TODO - I need to fix my join-line implementation to take a numerical argument
          (join line ((direction forward)) (,(lambda () (join-line t))))
          (join line ((direction backward)) (,(lambda () (join-line))))
          (join cpo-smartparens ((direction backward)) (cpo-smartparens-join-sexp-backward (num)))
          (join cpo-smartparens ((direction forward)) (cpo-smartparens-join-sexp-forward (num)))
          ;; TODO - cpo-smartparens - make a join-sexp function that takes a forward or backward argument


          ;; TODO - optional register for delete to be delete-copy
          ;; TODO - deduplicate these operations that delegate to the move command...
          (delete region
                  ()
                  (cpo-delete (register)))
          (delete ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(composiphrase--make-movement-delegated-command
                     (lambda (beg end sentence)
                       (cpo-delete (cdr (assq 'register (composiphrase-sentence-modifiers sentence)))
                                   (cons beg end)))
                     'pass-sentence)
                   sentence-with-defaults))

          (change region
                  ()
                  (cpo-change (register)))
          (change ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(composiphrase--make-movement-delegated-command
                     (lambda (beg end sentence)
                       (cpo-change (cdr (assq 'register (composiphrase-sentence-modifiers sentence)))
                                   (cons beg end)))
                     'pass-sentence)
                   sentence-with-defaults))

          (copy region ()
                (cpo-copy (register)))
          (copy ,(lambda (x) (not (memq x '(region)))) ()
                (,(composiphrase--make-movement-delegated-command
                   (lambda (beg end sentence)
                     (let ((register (cdr (assq 'register
                                                (composiphrase-sentence-modifiers
                                                 sentence)))))
                       (cpo-copy register (cons beg end))))
                   'pass-sentence)
                 sentence-with-defaults))

          (upcase region ()
                  (,(lambda () (upcase-region (region-beginning) (region-end)))))
          (upcase ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'upcase-region)
                   sentence-with-defaults))
          (downcase region ()
                    (,(lambda () (downcase-region (region-beginning) (region-end)))))
          (downcase ,(lambda (x) (not (memq x '(region)))) ()
                    (,(composiphrase--make-movement-delegated-command 'downcase-region)
                     sentence-with-defaults))
          (toggle-case region ()
                    (,(lambda () (TODO-toggle-case-region (region-beginning) (region-end)))))
          (toggle-case ,(lambda (x) (not (memq x '(region)))) ()
                    (,(composiphrase--make-movement-delegated-command 'TODO-toggle-case-region)
                     sentence-with-defaults))
          (capitalize region ()
                      (,(lambda () (capitalize-region (region-beginning) (region-end)))))
          (capitalize ,(lambda (x) (not (memq x '(region)))) ()
                      (,(composiphrase--make-movement-delegated-command 'capitalize-region)
                       sentence-with-defaults))
          (paste-to-region-from-move region ()
                                     (cpo-paste (register register-for-old)))
          (paste-to-region-from-move ,(lambda (x) (not (memq x '(region))))
                                     ()
                                     ;; The intention of this one is that it deletes region from the move and replaces it with a paste.  But it doesn't seem as useful as move-paste, so I don't feel very motivated to implement it right now.
                                     (TODO-paste-to-region-from-move-handler))
          (indent region ()
                  (TODO-increase-indent ()))
          (indent ,(lambda (x) (not (memq x '(region))))
                  ()
                  (,(composiphrase--make-movement-delegated-command
                     'TODO-increase-indent)
                   sentence-with-defaults))
          (dedent region ()
                  (TODO-dedent-region ()))
          (dedent ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'TODO-dedent-region)
                   sentence-with-defaults))
          (auto-indent region ()
                       (indent-region ()))
          (auto-indent ,(lambda (x) (not (memq x '(region)))) ()
                       (,(composiphrase--make-movement-delegated-command 'indent-region)
                        sentence-with-defaults))
          (format region ()
                  (TODO-format-command ()))
          (format ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'TODO-format-command)
                   sentence-with-defaults))
          (comment region ()
                   (,(lambda () (comment-region (region-beginning) (region-end))) ()))
          (comment ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'comment-region)
                   sentence-with-defaults))
          (uncomment region ()
                  (,(lambda () (uncomment-region (region-beginning) (region-end))) ()))
          (uncomment ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'uncomment-region)
                   sentence-with-defaults))
          (toggle-comment region ()
                  (,(lambda () (comment-or-uncomment-region (region-beginning) (region-end))) ()))
          (toggle-comment ,(lambda (x) (not (memq x '(region)))) ()
                  (,(composiphrase--make-movement-delegated-command 'comment-or-uncomment-region)
                   sentence-with-defaults))

          (move-paste region
                      ()
                      (cpo-paste (register register-for-old)))
          (move-paste ,(lambda (x) (not (memq x '(region))))
                      ()
                      (cpo-move-paste-sentence-execute
                       sentence-with-defaults))
          (move-insert ,(lambda (x) (not (memq x '(region))))
                       ()
                       (,(lambda (s)
                           (composiphrase-execute
                            ;; TODO - I should maybe delete the old verb, but I'll just use alist shadowing...
                            (cons '((word-type . verb) (contents . move)) s)
                            composiphrase-current-configuration)
                           (estate-insert-state))
                        sentence-with-defaults))

          ;; (initiate-isearch region ((direction forward))
          ;;                   (,(lambda () (cpo-isearch-forward-for-text-in-region (region-beginning) (region-end))) ()))
          ;; (initiate-isearch ,(lambda (x) (not (memq x '(region)))) ((direction forward))
          ;;                   (,(composiphrase--make-movement-delegated-command 'cpo-isearch-forward-for-text-in-region)
          ;;                    sentence-with-defaults))


          ))))


(provide 'composiphrase-demo-match-config)
