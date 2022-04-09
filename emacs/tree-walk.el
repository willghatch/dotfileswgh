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
;;
;; What kind of trees do I want to support (where effort allows)?
;; * Lisp (s-expressions)
;; * Org-mode/outline-mode hierarchies
;; * Lines with indentation
;; * Common data formats like JSON, XML
;; * Source code that can be parsed easily, eg. that has treesitter support
;; * Somewhere I had a list of a lot more, but I guess I didn't put them here.  Too bad I haven't actually implemented anything yet, I was thinking I had at least implemented a first draft of some by-indentation motions.  But they aren't here.  Oh, they are in vfuncs, just next-line-same-indentation-in-block...  Well, hopefully some day I make time to implement this, because it would be super useful to have common keybindings for tree-based motions but with many different trees.  I want this all the time when editing.
;;
;; Starting key brainstorm:
;; Text object operations: <inner/outer-prefix><tree-object-key><tree-type>
;; Motion operations: <tree-prefix><motion-type><tree-type>
;; Doing the tree type last would allow for an alternate tree prefix for “last tree type” or “saved tree type” to have 2-key combos instead of 3-key.
