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

(require 'cl-lib)

(defun tree-walk--motion-moved (motion)
  (let ((start-pos (point)))
    (funcall motion)
    (not (= (point) start-pos))))

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

(defun tree-walk--inorder-traversal-forward (forward1 backward1)
  (lambda (num)
    (interactive "p")
    (cond ((= num 0) t)
          ((< num 0) (funcall backward1 (- num)))
          (t
           ;; TODO - a custom while loop could exit early if the number given is too high
           (dotimes (i num) (funcall forward1))))))
(defun tree-walk--inorder-traversal-backward (forward1 backward1)
  (lambda (num)
    (interactive "p")
    (cond ((= num 0) t)
          ((< num 0) (funcall forward1 (- num)))
          (t
           ;; TODO - a custom while loop could exit early if the number given is too high
           (dotimes (i num) (funcall backward1))))))


(defmacro tree-walk-define-inorder-traversal
    (name-forward
     name-backward
     next prev
     up down down-to-last-child
     )
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
       (fset ',name-forward
             (tree-walk--inorder-traversal-forward ,forward1 ,backward1))
       (fset ',name-backward
             (tree-walk--inorder-traversal-backward ,forward1 ,backward1)))))



(defun tree-walk--inner-no-end-tree-for-point_left (up down final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (and (tree-walk--motion-moved up)
           (funcall down))
      (funcall final))))
(defun tree-walk--inner-no-end-tree-for-point_right (up down-to-last-descendant final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (funcall up)
      (funcall down-to-last-descendant)
      (funcall final))))
(defun tree-walk--outer-no-end-tree-for-point_left (final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (funcall final))))
(defun tree-walk--outer-no-end-tree-for-point_right (up down-to-last-descendant final)
  (lambda (start-point)
    (save-excursion
      (goto-char start-point)
      (funcall down-to-last-descendant)
      (funcall final))))

(defun tree-walk--text-object-no-end-helper (lfunc rfunc up-func)
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
  (let ((inner-left (gensym (format "-%s--inner-left" inner-name)))
        (outer-left (gensym (format "-%s--outer-left" outer-name)))
        (inner-right (gensym (format "-%s--inner-right" inner-name)))
        (outer-right (gensym (format "-%s--outer-right" outer-name)))
        (inner-helper (gensym (format "-%s--helper" inner-name)))
        (outer-helper (gensym (format "-%s--helper" outer-name)))
        )
    `(progn
       (setq ,inner-left
             (tree-walk--inner-no-end-tree-for-point_left ,up ,down ,left-finalize))
       (setq ,outer-left
             (tree-walk--outer-no-end-tree-for-point_left ,left-finalize))
       (setq ,inner-right
             (tree-walk--inner-no-end-tree-for-point_right
              ,up
              (lambda () (tree-walk--down-to-last-descendant ,down-to-last-child))
              ,right-finalize))
       (setq ,outer-right
             (tree-walk--outer-no-end-tree-for-point_right
              ,up
              (lambda () (tree-walk--down-to-last-descendant ,down-to-last-child))
              ,right-finalize))
       (setq ,inner-helper
             (tree-walk--text-object-no-end-helper ,inner-left ,inner-right ,up))
       (setq ,outer-helper
             (tree-walk--text-object-no-end-helper ,outer-left ,outer-right ,up))
       (evil-define-text-object ,inner-name (count &optional beg end type)
         ;; TODO - handle count, type
         (funcall ,inner-helper beg end))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         ;; TODO - handle count, type
         (funcall ,outer-helper beg end)))))

(cl-defmacro tree-walk-define-operations
    (&key
     inorder-forward
     inorder-backward
     down-to-last-descendant
     no-end-inner-object
     no-end-outer-object

     up-to-parent
     down-to-first-child
     down-to-last-child
     next-sibling
     previous-sibling
     no-end-object-left-finalize
     no-end-object-right-finalize
     )
  ;; TODO - add error checking to be sure requirements are met for each non-null thing to be defined
  `(progn
     ,@(remove-if-not
        (lambda (x) x)
        (list
         (when down-to-last-descendant
           `(defun ,down-to-last-descendant ()
              (interactive)
              (tree-walk--down-to-last-descendant ,down-to-last-child)))
         (when (or no-end-inner-object no-end-outer-object)
           `(tree-walk-define-text-objects-no-end-tree
             ,no-end-inner-object ,no-end-outer-object
             ,up-to-parent ,down-to-first-child ,down-to-last-child
             ,no-end-object-left-finalize ,no-end-object-right-finalize))
         (when (or inorder-forward inorder-backward)
           `(tree-walk-define-inorder-traversal
             ,inorder-forward
             ,inorder-backward
             ,next-sibling ,previous-sibling
             ,up-to-parent ,down-to-first-child ,down-to-last-child
             ))
         ))))

(provide 'tree-walk)
