;;; tree-walk.el --- TODO - description here -*- lexical-binding: t; -*-
;; Related work:
;; * https://github.com/volrath/treepy.el -- a library for navigating elisp data structures.  Cool!  But I want motions navigating a text buffer according to various kinds of trees encoded within the text.
;; * https://github.com/yangwen0228/jump-tree/ -- a library like undo-tree but for jump positions.  Awesome!
;; * https://github.com/mtekman/org-treeusage.el/ -- a library for showing size of org-mode sub-trees while exploring them.  Groovy!
;; * paredit, smartparens, symex, lispy
;; * combobulate, using treesitter to provide structured editing motions: https://github.com/mickeynp/combobulate
;;
;;
;; What operations do I want to support?
;; * next/prev sibling
;; * up to parent
;; * down to first child
;; * down to last child
;; * up to next/prev sibling of nearest ancestor that has one (IE for when I'm at a leaf node with no next sibling, to progress in an in-order-traversal I need to go up until I can go to next sibling)
;; * previous sibling's right-most bottom-most descendant
;; * in-order-traversal forwards/backwards
;; ** IE forwards is: down to first child if possible, else next sibling if possible, else up to first ancestor with a next sibling and then go to that next sibling.
;; ** Backwards is more complicated: if possible, go to the previous sibling's last child's last child etc recursively, if no previous sibling, go up.
;; * Maybe other traversal orders?  They might be interesting but seem less important.
;; * text objects on the trees, obviously
;; * slurp/barf operations if possible
;;
;; One of the biggest differences in trees I might like to support is whether or not they include an explicit end delimiter.
;; On trees with an explicit end delimiter, it can be really useful to go up to the end delimiter, but without an explicit end that operation doesn't really work -- eg. in an indentation tree where each line indents, the end of the (sub)tree is the end of each level of the subtree, so while it makes sense to go to the end point of any of those subtrees, it is ambigous if you want to go back.
;; This is also a problem for mixed trees, where if any of the trees mixed in lack explicit end delimiters, the mixed tree then doesn't universally have them.
;;
;; Another weird tree issue is the idea of “half-siblings” for indent trees and org/outline-mode trees.  These can both have a first child that is indented more than later children, and in fact can have an arbitrary number of early children that are indented deeper than later children.  Eg: * h1 **** h4 *** h3 ** h2.  Each of these may have valid sub-trees, but they are not full siblings despite sharing a parent.  So there should be a next/prev full-sibling function and a next/prev full-or-half-sibling function, and maybe a next/prev half-sibling region function, and the full inorder traversal function needs to use the full-or-half-sibling functions.  Also XML could have a definition where the attributes count as a set of half siblings -- not the main XML definition, but it could be an alternate mode.
;; But... does this tree-walk library need to care about full/half siblings?  The inorder traversal can just be given the next-full-or-half-sibling functions, and any specific trees with this quality can define their own helpers for dealing with full/half siblings.
;;
;; What are the common USEFUL operations that may be built on top of the basics, that I can bind to keys in a generic way so that once I learn the keys for operations and for trees I can compose them?
;; * Move to sibling is really useful for navigation and reading, eg. to get an overview of definitions at a certain level
;; * I delete to the end of the current s-exp.  That might be more complicated with generic trees to know exactly where to delete to for trees without an explicit end delimiter.
;; * highlight text objects -- with explicit delimiters there is a clear inner and outer object (inside and outside delimiter), without explicit delimiters the outer object is obvious, but they may have (possibly degenerate) inner objects as well
;; * slurp/barf -- sometimes I want to barf a single item out of multiple levels, but that's inconvenient.  I should add a barf operation where the cursor follows the barfee instead of the barfer.
;; * Down to last descendant is useful when navigating trees that don't have explicit delimiters to see where the end of a tree is, but it would be nice to be paired with an undo-move
;;
;; What kind of trees do I want to support (where effort allows)?
;; * Lisp (s-expressions)
;; * Org-mode/outline-mode hierarchies
;; * Lines with indentation
;; * Common data formats like JSON, XML
;; * Source code that can be parsed easily, eg. that has treesitter support
;; * Somewhere I had a list of a lot more, but I guess I didn't put them here.
;;
;; Starting key brainstorm:
;; Text object operations: <inner/outer-prefix><tree-object-key><tree-type>
;; Motion operations: <tree-prefix><motion-type><tree-type>
;; Doing the tree type last would allow for an alternate tree prefix for “last tree type” or “saved tree type” to have 2-key combos instead of 3-key.
;;
;; Non-motion operations:
;; * slurp -- forward turns neigbor sibling into last child, backward turns the previous sibling into the first child... but that makes more sense for s-expressions than eg. indent trees or org-mode.
;; * join -- splice current node with neighbor (forward or backward) -- for symex, basically delete two inner parens, for indent trees or org-mode it's close to joining lines.
;; * split -- add parens at point to split current symex into two, meaning it turns a single parent of a subtree into two siblings at the parent level.  So for org/indent it probably means inserting a header/indent at the parent level in the line above/below?
;; * splice -- IE delete parens for symex so each child becomes a sibling of where the parens were.  When at a paren it splices that paren, but when not on a paren it targets the parent parens.  For org-mode should this mean delete-this-level-and-promote-all-children?  Or promote this and all siblings?
;; * demote/wrap - opposite of splice, add parens, or add indent/header level.  Should this be demoting while adding a header above for org?  Org-mode has demotion without putting a header above, but that can make wonky trees, and indent trees have this problem even more.  Note that for symex or xml, surrounding requires a choice of which kind of paren or which tag.  Maybe there could be a default (eg. default to paren for lisp, to <TAG></TAG> or some TODO kind of form for xml), and then have another one that lets you select a paren/tag.
;; * maybe also demote-children, which leaves the current level in place but turns children into grandchildren of a new child?
;; * rewrap -- eg. change parent paren type, or xml tag type.  This doesn't exist for trees that don't have explicit delimiters.
;; * raise-sexp - IE replace parent tree with the current node
;; * transpose - IE swap sibling locations (with all siblings still with original parents).  Some have a difference between transpose and drag, basically where the cursor ends after.  IE for transpose forward does the cursor follow the thing that goes forward or stay in place?  I think moving (dragging) is generally more useful -- transpose twice without moving the cursor is a no-op, and dragging comes up more often than single transposition, I think.
;; * drag-backward-up - for symex, move current form to be first of siblings if not already, then hoist out of parent form (IE turn to sibling before parent paren), repeat.  For org-mode, it would turn the current heading to the first sibling of its parent, then become a sibling of the parent above it, similar for indent.
;; * drag-forward-down - not-quite-opposite of backward up, for symex become first child of the next paren.  For org-mode I guess it would be to become the first child of the next heading.  For indent tree, move below next line and dedent.  Many of these operations for indent trees would be useful if they could affect a range, IE a group of lines be dragged around together.
;; * drag-forward-up/drag-backward-down -- like the previous pair but with direction changed.
;; * convolute -- for symex, hoist parent and its children to point above grandparent.
;;   eg. (a b c (d e | f g)) -> (d e (a b c f g))
;;   So for org-mode it could mean to take the parent and any higher siblings and promote them, put them above grandparent and demote grandparent.  Similar for indent tree.
;; * delete forward/back -- eg. if point is at the start of a subtree kill it, putting the next subtree in its place.
;; * delete to parent -- IE in symex delete forward/backward to paren, in org-mode delete subtrees/siblings forward (IE until a non sibling) or backward until (but not including) parent
;; ** a modification of this is delete-to-parent-or-newline, which I find a little awkward.
;; * Ryan Culpepper wrote a cool sexp-rewrite package for matching and transforming trees, it's a cool idea that could be generalized, but would be a lot of work.
;; * new sibling forward/backward -- Eg. for indent tree or org-mode it opens a new line below/above the current element and indents it/bullets it appropriately.  My binding would probably also leave me in insert mode instead of normal/command mode.  For symex it can... well, maybe it can be context sensitive and open a new line when its in a list where each item starts on a new line, or it can move and add spaces to the correct place in a context where all is on one line.  But what to do when there is only one item in the list?  I mostly want this operation for org-mode and indent trees, less for symex.
;;
;; Movement and addressing:
;; * Movement operations should all move the cursor to some canonical anchor point.  This will probably typically be the beginning of the tree object, but could sometimes be elsewhere, eg. for infix trees the beginning of the infix operator is probably best.
;; * There should be a “thing at point” or similar function to get the bounds of the tree object at point.
;;
;;
;; TODO list
;; * Document things
;; * Make macro keywords consistent, and use names like def-X for symbols that the macro will define.
;; * Make it clear what APIs are necessary to define new tree-walk trees, and what APIs you get from it.
;; * Implement helper to get bounds for indent tree and similar trees -- convert it from being focused on evil-mode to focused on getting bounds more generally.
;; * Define a shared API for all trees to access bounds.  IE explicitly write some bounds helpers for smartparens, and decide if anything more is necessary.
;; * Define easy generic ops: transpose-sibling, transpose-sibling-region (IE transpose a group of siblings together, all of the siblings in the region (either strictly with the sibling regions exactly fitting the region or sloppily using all siblings that are partially in the region)), raise/replace-parent, etc.  Do I want things like delete forward/back, delete to parent, etc?  I think just having movements is enough for that -- IE move to start of first sibling, move to end of last sibling, etc paired with regions and deletion do it.
;; * Define additional motions -- IE in addition to the basic motions that go somewhere (hopefully to some canon anchor point) on the sibling/parent/child, define motions to go to specifically beginning/end of thing, also add idempotence option.
;; * Decide: what is necessary to have other generic ops, or how far can they be generic?  Eg. slurp/barf, join/split, splice, demote/wrap, rewrap, etc
;; * Define a full suite using smartparens as a base.
;; * Define a full suite using indent-tree as a base.
;; * Define a full suite using org-mode or outline-mode tree as a base.
;; * Define a full suite for treesitter
;; * Define a full suite for json
;; * Define a full suite for XML

(require 'cl-lib)

(defun tree-walk--motion-moved (motion)
  "Take MOTION, some movement function of no args.  Call it, and return true if the motion moved, leaving point where the motion moved it."
  (let ((start-pos (point)))
    (funcall motion)
    (not (= (point) start-pos))))

(defun tree-walk--motion-moved-region (motion)
  (let ((start-pos (point))
        (start-mark (mark))
        (start-region-active (region-active-p)))
    (funcall motion)
    (or (not (= (point) start-pos))
        (not (= (mark) start-mark))
        (not (equal start-region-active (region-active-p))))))

(defun tree-walk--last-leaf-forward-in-order (up next)
  (lambda ()
    ;; IE if you're at a leaf with no next sibling,
    ;; the next motion is to find the nearest
    ;; ancestor with a next sibling and go to that sibling.
    (interactive)
    (let ((backtrack-pos (point))
          (keep-going-p t)
          (success-p nil))
      (while (and keep-going-p (not success-p))
        (when (not (tree-walk--motion-moved up))
          (setq keep-going-p nil))
        (when (tree-walk--motion-moved next)
          (setq success-p t)))
      (when (not success-p)
        (goto-char backtrack-pos))
      success-p)))

(defun tree-walk--down-to-last-descendant (down-to-last-child)
  (interactive)
  (while (tree-walk--motion-moved down-to-last-child)))

(defun tree-walk--previous-sibling-last-descendant (prev last-descendant)
  (and (tree-walk--motion-moved prev) (funcall last-descendant)))

(defun tree-walk--inorder-traversal-forward-single (down next last-leaf-forward)
  (lambda ()
    (unless (tree-walk--motion-moved down)
      (unless (tree-walk--motion-moved next)
        (tree-walk--motion-moved last-leaf-forward)))))
(defun tree-walk--inorder-traversal-backward-single (up prev down-to-last-child)
  (lambda ()
    (unless (tree-walk--motion-moved
             (lambda ()
               (tree-walk--previous-sibling-last-descendant
                prev
                (lambda () (while (tree-walk--motion-moved down-to-last-child))))))
      (funcall up))))

(defmacro tree-walk--def-inorder-traversal (function-name object-name dir-name forward1 backward1)
  `(defun ,function-name (count)
       ,(format "Do COUNT steps %s of inorder tree traversal for %s." dir-name object-name)
     (interactive "p")
     (cond ((not (integerp count)) (funcall ,forward1))
           ((= count 0) t)
           ((< count 0) (funcall ,backward1 (- count)))
           (t
            ;; TODO - a custom while loop could exit early if the countber given is too high
            (dotimes (i count) (funcall ,forward1))))))



(defmacro tree-walk-define-inorder-traversal
    (object-name
     name-forward
     name-backward
     next prev
     up down down-to-last-child
     )
  "OBJECT-NAME is for the doc string, NAME-FORWARD is the name of the forward traversal func, NEXT PREV etc are movement functions."
  (let ((forward1 (gensym (format "%s_single_" name-forward)))
        (backward1 (gensym (format "%s_single_" name-backward)))
        (leaf-forward (gensym (format "%s_leaf-helper_" name-forward)))
        )
    `(progn
       (setq ,leaf-forward (tree-walk--last-leaf-forward-in-order ,up ,next))
       (setq ,forward1 (tree-walk--inorder-traversal-forward-single
                        ,down ,next ,leaf-forward))
       (setq ,backward1 (tree-walk--inorder-traversal-backward-single
                         ,up ,prev ,down-to-last-child))
       (tree-walk--def-inorder-traversal ,name-forward ,object-name "forward" ,forward1 ,backward1)
       (tree-walk--def-inorder-traversal ,name-backward ,object-name "backward" ,backward1 ,forward1))))

(defun tree-walk--down-to-last-child-default-impl (down-to-first-child next-sibling)
  "Default implementation of down-to-last-child."
  (and
   (tree-walk--motion-moved down-to-first-child)
   (while (tree-walk--motion-moved next-sibling))))

(defun tree-walk--region-less-or-equal (r1 r2)
  "Returns canon form of r2 if it includes all of r1, otherwise nil.
r1 and r2 should both be cons pairs of points eg. (beg . end), but also return nil if one of them is not.
r1 and r2 do not have to be in proper order.
If r2 is not in order, a successful return has in ordered."
  (and (consp r1)
       (consp r2)
       (and (integerp (car r1))
            (< 0 (car r1)))
       (and (integerp (cdr r1))
            (< 0 (cdr r1)))
       (and (integerp (car r2))
            (< 0 (car r2)))
       (and (integerp (cdr r2))
            (< 0 (cdr r2)))
       (<= (min (car r2) (cdr r2)) (min (car r1) (cdr r1)))
       (<= (max (car r1) (cdr r1)) (max (car r2) (cdr r2)))
       (cons (min (car r2) (cdr r2))
             (max (car r2) (cdr r2)))))

(defun tree-walk--region-strictly-less (r1 r2)
  "Returns canon form of r2 if it is strictly greater than r1, IE it covers but also goes beyond on at least one side."
  (let ((cover (tree-walk--region-less-or-equal r1 r2)))
    (and cover
         (or (< (car r2) (min (car r1) (cdr r1)))
             (<  (max (cdr r1) (car r1)) (cdr r2)))
         cover)))

(defun tree-walk--regions-not-overlapping (r1 r2)
  "Return t if regions do not overlap, nil otherwise."
  (if (< (car r1) (car r2))
      (<= (cdr r1) (car r2))
    (<= (cdr r2) (car r1))))

(defun tree-walk--transpose-sibling-once (bounds-func move-func goto-anchor-func)
  "Transpose a tree node forward by swapping it with its next neighbor forward, based on the given functions to operate on it.
If GOTO-ANCHOR-FUNC is nil, then the beginning of the bounds is assumed to be the anchor point.
Leave the cursor on the original thing, so you can keep dragging it forward or back."
  (let ((bounds-1 (funcall bounds-func (point))))
    (and bounds-1
         (let* ((bounds-2 (save-mark-and-excursion
                            (if goto-anchor-func
                                (funcall goto-anchor-func)
                              (goto-char (car bounds-1)))
                            (and (tree-walk--motion-moved move-func)
                                 (funcall bounds-func (point))))))
           (when (and bounds-2
                      (tree-walk--regions-not-overlapping bounds-1 bounds-2))
             (let* ((s1 (buffer-substring-no-properties (car bounds-1)
                                                        (cdr bounds-1)))
                    (s2 (buffer-substring-no-properties (car bounds-2)
                                                        (cdr bounds-2)))
                    (b1-earlier-p (< (car bounds-1) (car bounds-2)))
                    (bounds-earlier (if b1-earlier-p bounds-1 bounds-2))
                    (bounds-later (if b1-earlier-p bounds-2 bounds-1))
                    (s-earlier (if b1-earlier-p s1 s2))
                    (s-later (if b1-earlier-p s2 s1)))
               ;; swap regions
               (atomic-change-group
                 (delete-region (car bounds-later) (cdr bounds-later))
                 (goto-char (car bounds-later))
                 (insert s-earlier)
                 (delete-region (car bounds-earlier) (cdr bounds-earlier))
                 (goto-char (car bounds-earlier))
                 (insert s-later))
               ;; put cursor at beginning of later region
               (let ((len-diff (- (length s2) (length s1))))
                 (goto-char (if b1-earlier-p
                                (+ len-diff (car bounds-2))
                              (car bounds-2)))
                 (undo-boundary))))))))
(defun tree-walk--transpose-siblings (count bounds-func move-func goto-anchor-func move-backward-func)
  "Transpose siblings COUNT times.  Use the other args to move and find the things."
  (setq count (or count 1))
  (let ((fwd (< 0 count))
        (count (abs count)))
    (let ((cg (prepare-change-group)))
      (while (< 0 count)
        (if fwd
            (tree-walk--transpose-sibling-once bounds-func move-func goto-anchor-func)
          (tree-walk--transpose-sibling-once bounds-func (or move-backward-func (lambda () (funcall move-func -1))) goto-anchor-func))
        (setq count (- count 1)))
      (undo-amalgamate-change-group cg))))


;; TODO - all of this code about getting bounds is just a disaster.  I wanted to make the code I already had work, but I should have just started from scratch.  At any rate, it's mostly working.  I should come back and clean it up.  Eg. remove the stuff that's specific to evil-mode, and make things simpler and less super higher order due to whatever whims of the day I threw together the evil-mode version and then my desire to use that rather than starting over.
(defun tree-walk--bounds-of-thing-at-point-for-tree-with-no-end-delimiter
    (up down down-to-last-descendant finalize-left finalize-left-inner finalize-right)
  "Helper to define bounds for a tree with no end delimiter.
It takes motion functions up, down, down-to-last-descendant.
It takes finalize functions that take a point and give a final full region.
"
  (let ((bounds-of-thing
         (lambda (&optional anchor-point)
           "Takes ANCHOR-POINT (buffer location, default to `point') and returns region as (beg . end), or nil if it can't find the thing at the anchor point."
           (let ((anchor-point (or anchor-point (point))))
             (let ((left (funcall (tree-walk--outer-bounds-no-end-tree-for-point_left
                                   finalize-left)
                                  anchor-point))
                   (right (funcall (tree-walk--outer-bounds-no-end-tree-for-point_right
                                    up down-to-last-descendant finalize-right)
                                   anchor-point)))
               (and left right (cons left right))))))
        (bounds-of-children
         (lambda (&optional anchor-point)
           "Takes ANCHOR-POINT (buffer location, default to `point') and returns region as (beg . end), or nil if it can't find the thing at the anchor point.
Returns the region of the children, not the full tree."
           (let ((anchor-point (or anchor-point (point))))
             (let ((left (funcall (tree-walk--inner-bounds-no-end-tree-for-point_left
                                   up down (or finalize-left-inner finalize-left))
                                  anchor-point))
                   (right (funcall (tree-walk--inner-bounds-no-end-tree-for-point_right
                                    up down-to-last-descendant finalize-right)
                                   anchor-point)))
               (and left right (cons left right)))))))
    (list
     bounds-of-thing
     bounds-of-children
     )))

;; For these helpers, `final` is a function that gets the begin/end point after doing the tree motions to get to the node, first child, or last child.
(defun tree-walk--inner-bounds-no-end-tree-for-point_left (up down final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (and (tree-walk--motion-moved down)
           (funcall final)))))
(defun tree-walk--inner-bounds-no-end-tree-for-point_left_inner-bounds-for-parent-of-start-point (up down final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (and (tree-walk--motion-moved up)
           (funcall down))
      (funcall final))))
(defun tree-walk--inner-bounds-no-end-tree-for-point_right (up down-to-last-descendant final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (funcall up)
      (funcall down-to-last-descendant)
      (funcall final))))
(defun tree-walk--outer-bounds-no-end-tree-for-point_left (final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      ;; if the final function returns point, use it, otherwise assume it was a movement and get point.
      (or (funcall final) (point)))))
(defun tree-walk--outer-bounds-no-end-tree-for-point_right (up down-to-last-descendant final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (funcall down-to-last-descendant)
      (or (funcall final) (point)))))

(cl-defmacro tree-walk--define-bounds-functions-for-tree-with-no-end-delimiter
    (&key def-bounds-name
          def-children-bounds-name

          up-func
          down-func
          down-to-last-descendant-func

          left-finalize-func
          left-inner-finalize-func
          right-finalize-func
          )
  `(progn
     (let ((funcs (tree-walk--bounds-of-thing-at-point-for-tree-with-no-end-delimiter
                   ,up-func ,down-func ,down-to-last-descendant-func ,left-finalize-func
                   ,left-inner-finalize-func ,right-finalize-func)))
       (fset ',def-bounds-name
              (car funcs))
       (fset ',def-children-bounds-name
              (cadr funcs)))))

(defun tree-walk--expanded-region
    (get-bounds-func up-func)
  "
GET-BOUNDS-FUNC: takes an anchor point and returns a region or nil.
UP-FUNC: tree move to parent function.
"
  (letrec ((grow-func
            (lambda (strictly-grow anchor-point region)
              "
Walks up the tree from ANCHOR-POINT and returns the first region that covers REGION.
If STRICTLY-GROW, then the returned region is strictly larger.
Returns nil if it can't get an appropriate region.
The anchor-point doesn't necessarily need to be in the region.
It does not actually move the active region, it just takes and returns region data as cons pairs.
If REGION is null, then any region succeeds.
"
              (let ((compare-func (if strictly-grow
                                      'tree-walk--region-strictly-less
                                    'tree-walk--region-less-or-equal)))
                (save-mark-and-excursion
                 (let* ((bounds (funcall get-bounds-func anchor-point))
                        (success (and bounds (or (and (not region) bounds)
                                                 (funcall compare-func region bounds)))))

                   (if success
                       success
                     (and
                      (tree-walk--motion-moved up-func)
                      (funcall grow-func strictly-grow (point) region)))))))))
    grow-func))

(defun tree-walk--expand-region (expansion-func strictly-grow)
  "
Takes an expansion func as is returned by tree-walk--expanded-region.
Returns an interactive command to expand region.
"
  (lambda (&optional count)
    ;; TODO - add num
    (interactive "p")
    (dotimes (i (or count 1))
      (let* ((current-region (if (region-active-p)
                                 (cons (region-beginning)
                                       (region-end))
                               nil))
             (start-anchor (or (and current-region (car current-region))
                               (point)))
             (new-bounds (funcall expansion-func strictly-grow start-anchor current-region))
             )
        (when new-bounds
          (if (and (region-active-p)
                   (< (point) (mark)))
              (progn
                (set-mark (cdr new-bounds))
                (goto-char (car new-bounds)))
            (progn
              (set-mark (car new-bounds))
              (goto-char (cdr new-bounds)))))))))


(defun tree-walk--text-object-no-end-helper (lfunc rfunc up-func)
  "Helper for making evil-mode objects."
  (lambda (beg end)
    (let* ((get-l (lambda (beg end) (min (funcall lfunc beg)
                                         (funcall lfunc end)
                                         beg)))
           (get-r (lambda (beg end) (max (funcall rfunc beg)
                                         (funcall rfunc end)
                                         end)))
           (l (funcall get-l beg end))
           (r (funcall get-r beg end)))
      (if (and (= beg l) (= end r))
          (let ((new-beg (save-excursion
                           (goto-char beg)
                           (funcall up-func)
                           (point))))
            (list (funcall get-l new-beg end)
                  (funcall get-r new-beg end)))
        (list l
              r)))))

;; The outer object is the more sensible one, it highlights the tree rooted at the current point, or, if the whole current tree is in the region, it expands to the parent tree.
;; The inner object is... probably worse semantically but it's the way I could think of making it work and be semi-correct.  It highlights all the children of the parent node, or if that's already covered then it expands to all children of the grandparent.
(defmacro tree-walk-define-text-objects-no-end-tree
    (inner-name outer-name
     up down down-to-last-child
     left-finalize right-finalize)
  "Define text objects.
Define INNER-NAME and OUTER-NAME as evil-mode text objects (using with-eval-after-load).

TODO - everything below this is a lie until I implement it...
TODO - I need to define the below to have a bounds-of-thing-at-point implementation.

Define INNER-NAME-bounds, OUTER-NAME-bounds as functions to get bounds of tree at point.

The inner object is a little wonky, it is roughly the children of the tree node, but what exactly that means depends on the tree type.
The outer object is the full tree node, and makes a lot more sense.

Both bounds functions return a region as a cons cell (beg . end) or nil.

The outer-bounds function takes an optional arguments: strictly-grow (bool), and region in (beg . end).
If strictly-grow is true, it only returns a region if it both strictly contains the old region (given or current) AND is strictly greater than it (IE extending it to the left or the right).
If no region is given, it uses the current region (or ((point) . (point))).
"
  (let ((inner-left (gensym (format "-%s--inner-left" inner-name)))
        (outer-left (gensym (format "-%s--outer-left" outer-name)))
        (inner-right (gensym (format "-%s--inner-right" inner-name)))
        (outer-right (gensym (format "-%s--outer-right" outer-name)))
        (inner-helper (gensym (format "-%s--helper" inner-name)))
        (outer-helper (gensym (format "-%s--helper" outer-name)))
        )
    `(progn
       (setq ,inner-left
             (tree-walk--inner-bounds-no-end-tree-for-point_left ,up ,down ,left-finalize))
       (setq ,outer-left
             (tree-walk--outer-bounds-no-end-tree-for-point_left ,left-finalize))
       (setq ,inner-right
             (tree-walk--inner-bounds-no-end-tree-for-point_right
              ,up
              (lambda () (tree-walk--down-to-last-descendant ,down-to-last-child))
              ,right-finalize))
       (setq ,outer-right
             (tree-walk--outer-bounds-no-end-tree-for-point_right
              ,up
              (lambda () (tree-walk--down-to-last-descendant ,down-to-last-child))
              ,right-finalize))
       (setq ,inner-helper
             (tree-walk--text-object-no-end-helper ,inner-left ,inner-right ,up))
       (setq ,outer-helper
             (tree-walk--text-object-no-end-helper ,outer-left ,outer-right ,up))
       (with-eval-after-load 'evil-macros
         (evil-define-text-object ,inner-name (count &optional beg end type)
           ;; TODO - handle count, type
           (funcall ,inner-helper beg end)))
       (with-eval-after-load 'evil-macros
         (evil-define-text-object ,outer-name (count &optional beg end type)
           ;; TODO - handle count, type
           (funcall ,outer-helper beg end)))
       )))



(cl-defmacro tree-walk--define-expand-region-funcs
    (&key def-expand-region
          def-expand-region-idempotent
          def-select-children-once
          def-expand-region-to-children/ancestor-generation
          bounds-func
          children-bounds-func
          up-func)
  `(progn
     ,@(when def-expand-region
         `((fset ',def-expand-region
                  (tree-walk--expand-region
                   (tree-walk--expanded-region ,bounds-func ,up-func)
                   'strictly-grow))))
     ,@(when def-expand-region-idempotent
         `((fset ',def-expand-region-idempotent
                 (tree-walk--expand-region (tree-walk--expanded-region ,bounds-func ,up-func) nil))))

     ,@(when def-select-children-once
         `((fset ',def-select-children-once
                 (tree-walk--expand-region (tree-walk--expanded-region ,children-bounds-func ,up-func) nil))))

     ,@(when def-expand-region-to-children/ancestor-generation
         `((fset ',def-expand-region-to-children/ancestor-generation
                 (tree-walk--expand-region (tree-walk--expanded-region ,children-bounds-func ,up-func) 'strictly-grow))))))

(defun tree-walk--ancestor-reorder (parent-to-ancestor-distance expand-region-func)
  "Given operations for regions, and a number for the distance between parent and the ancestor to swap it with, swap ancestors.
This always swaps the parent of the tree object at point with its parent or some ancestor.
TODO - add an optional fix-up function, eg. to fix indentation for indent tree, fix org-mode depth, etc.
"
  (when (region-active-p)
    (error "starting with active region not yet supported"))
  ;; Select the current object.
  (let ((init-point (point)))
    (funcall expand-region-func)
    (let ((init-region (cons (region-beginning) (region-end)) ))
      ;; Select the parent object.
      (funcall expand-region-func)
      (let ((parent-region (cons (region-beginning) (region-end))))
        ;; Select grandparent/ancestor object.
        (dotimes (i parent-to-ancestor-distance) (funcall expand-region-func))
        (let ((ancestor-region (cons (region-beginning) (region-end))))
          (if (and (tree-walk--region-strictly-less init-region parent-region)
                   (tree-walk--region-strictly-less parent-region ancestor-region))
              (let ((child-text (buffer-substring-no-properties (car init-region) (cdr init-region)))
                    (parent-pre-text (buffer-substring-no-properties (car parent-region) (car init-region)))
                    (parent-post-text (buffer-substring-no-properties (cdr init-region) (cdr parent-region)))
                    (ancestor-pre-text (buffer-substring-no-properties (car ancestor-region) (car parent-region)))
                    (ancestor-post-text (buffer-substring-no-properties (cdr parent-region) (cdr ancestor-region)))
                    (change-group (prepare-change-group)))
                (delete-region (car ancestor-region) (cdr ancestor-region))
                ;; TODO - use fixup-function
                (insert parent-pre-text)
                (insert ancestor-pre-text)
                (insert child-text)
                (insert ancestor-post-text)
                (insert parent-post-text)
                (goto-char init-point)
                (undo-amalgamate-change-group change-group))
            (error "regions for thing, parent, and ancestor not strictly growing")))))))

(defun tree-walk--up-to-root (up-func)
  (while (tree-walk--motion-moved up-func)))
(defun tree-walk--select-root (up-func expand-region-func)
  ;; Alternatively I could repeat the expand-region-func, but this seems better as long as it works.
  (tree-walk--up-to-root up-func)
  (funcall expand-region-func))

(cl-defmacro tree-walk-define-operations
    (&key
     def-inorder-forward
     def-inorder-backward
     def-down-to-last-descendant

     def-evil-inner-object-for-tree-with-no-end-delimiter
     def-evil-outer-object-for-tree-with-no-end-delimiter

     def-bounds-for-tree-with-no-end-delimiter
     def-children-bounds-for-tree-with-no-end-delimiter
     def-down-to-last-child
     ;; TODO - these should be able to use the new definitions OR provided existing definitions.
     def-expand-region
     def-expand-region-idempotent
     def-select-children-once
     def-expand-region-to-children/ancestor-generation

     def-transpose-sibling-forward
     def-transpose-sibling-backward

     def-ancestor-reorder

     def-up-to-root
     def-select-root

     use-object-name

     use-up-to-parent
     use-down-to-first-child
     use-down-to-last-child
     use-next-sibling
     use-previous-sibling
     use-left-finalizer-for-tree-with-no-end-delimiter
     use-right-finalizer-for-tree-with-no-end-delimiter

     use-bounds
     use-children-bounds
     )
  ;; TODO - add error checking to be sure requirements are met for each non-null thing to be defined
  `(progn
     ,@(cl-remove-if-not
        (lambda (x) x)
        (list
         (when def-down-to-last-child
           `(defun ,def-down-to-last-child ()
              (interactive)
              (tree-walk--down-to-last-child-default-impl ,use-down-to-first-child ,use-next-sibling)))
         (when def-down-to-last-descendant
           `(defun ,def-down-to-last-descendant ()
              (interactive)
              (tree-walk--down-to-last-descendant ,(or use-down-to-last-child `',def-down-to-last-child))))
         (when (or def-evil-inner-object-for-tree-with-no-end-delimiter def-evil-outer-object-for-tree-with-no-end-delimiter)
           `(tree-walk-define-text-objects-no-end-tree
             ,def-evil-inner-object-for-tree-with-no-end-delimiter ,def-evil-outer-object-for-tree-with-no-end-delimiter
             ,use-up-to-parent ,use-down-to-first-child ,(or use-down-to-last-child `',def-down-to-last-child)
             ,use-left-finalizer-for-tree-with-no-end-delimiter ,use-right-finalizer-for-tree-with-no-end-delimiter))
         (when (or def-bounds-for-tree-with-no-end-delimiter def-children-bounds-for-tree-with-no-end-delimiter)
           `(tree-walk--define-bounds-functions-for-tree-with-no-end-delimiter
             :def-bounds-name ,def-bounds-for-tree-with-no-end-delimiter
             :def-children-bounds-name ,def-children-bounds-for-tree-with-no-end-delimiter
             :up-func ,use-up-to-parent
             :down-func ,use-down-to-first-child
             ;; TODO - make this not broken
             :down-to-last-descendant-func #',def-down-to-last-descendant
             :left-finalize-func ,use-left-finalizer-for-tree-with-no-end-delimiter
             :right-finalize-func ,use-right-finalizer-for-tree-with-no-end-delimiter
             ))
         (when (or def-expand-region def-expand-region-idempotent
                   def-select-children-once
                   def-expand-region-to-children/ancestor-generation)
           `(tree-walk--define-expand-region-funcs
             :def-expand-region ,def-expand-region
             :def-expand-region-idempotent ,def-expand-region-idempotent
             :def-select-children-once ,def-select-children-once
             :def-expand-region-to-children/ancestor-generation ,def-expand-region-to-children/ancestor-generation
             ;; TODO - generalize this
             :bounds-func ,(or use-bounds `',def-bounds-for-tree-with-no-end-delimiter)
             :children-bounds-func ,(or use-children-bounds `',def-children-bounds-for-tree-with-no-end-delimiter)
             :up-func ,use-up-to-parent))
         (when def-up-to-root
           `(defun ,def-up-to-root ()
              ,(format "Go up to the top tree ancestor for %s." use-object-name)
              (interactive)
              (tree-walk--up-to-root ,use-up-to-parent)))
         (when def-select-root
           `(defun ,def-select-root ()
              ,(format "Select region (activate region with the bounds) of top tree ancestor for %s." use-object-name)
              (interactive)
              (tree-walk--select-root ,use-up-to-parent ',def-expand-region)))
         (when def-ancestor-reorder
           `(defun ,def-ancestor-reorder (count)
              ,(format "Reorder ancestors for %s.  Take the region of the thing at point, the region of the parent, and the region of the ancestor COUNT generations above parent, and swap the parent and the ancestor.  Essentially, take the ancestor out, leaving a hole in the overall buffer, take the parent out of the ancestor, leaving a hole in the ancestor, and take the child out of the parent, leaving a hole in the parent.  Reassemble putting the parent in the ancestor's old hole, the ancestor in the parent's hole, and the child in the ancestor's hole." (or use-object-name "thing"))
              (interactive "p")
              (tree-walk--ancestor-reorder (or count 1) ',def-expand-region)))
         (when (or def-transpose-sibling-forward def-transpose-sibling-backward)
           `(progn
              (fset ',def-transpose-sibling-forward (lambda (&optional count)
                                                      ,(format "Transpose sibling %s forward COUNT times, moving point with the moved object so it can be dragged.  This movement respects tree boundaries." (or use-object-name "thing"))
                                                      (interactive "p")
                                                      (tree-walk--transpose-siblings (or count 1)
                                                                                     ,(or use-bounds `',def-bounds-for-tree-with-no-end-delimiter)
                                                                                     ,use-next-sibling
                                                                                     nil
                                                                                     ,use-previous-sibling)))
              (fset ',def-transpose-sibling-backward (lambda (&optional count)
                                                       ,(format "Transpose sibling %s backward COUNT times, moving point with the moved object so it can be dragged.  This movement respects tree boundaries." (or use-object-name "thing"))
                                                      (interactive "p")
                                                      (tree-walk--transpose-siblings (or count 1)
                                                                                     ,(or use-bounds `',def-bounds-for-tree-with-no-end-delimiter)
                                                                                     ,use-previous-sibling
                                                                                     nil
                                                                                     ,use-next-sibling)))))
         (when (or def-inorder-forward def-inorder-backward)
           `(tree-walk-define-inorder-traversal
             ,(or use-object-name "thing")
             ,def-inorder-forward
             ,def-inorder-backward
             ,use-next-sibling ,use-previous-sibling
             ,use-up-to-parent ,use-down-to-first-child ,(or use-down-to-last-child `',def-down-to-last-child)
             ))
         ))))

(provide 'tree-walk)
