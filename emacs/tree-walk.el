;;; tree-walk.el --- TODO - description here -*- lexical-binding: t; -*-
;; Related work:
;; * https://github.com/volrath/treepy.el -- a library for navigating elisp data structures.  Cool!  But I want motions navigating a text buffer according to various kinds of trees encoded within the text.
;; * https://github.com/yangwen0228/jump-tree/ -- a library like undo-tree but for jump positions.  Awesome!
;; * https://github.com/mtekman/org-treeusage.el/ -- a library for showing size of org-mode sub-trees while exploring them.  Groovy!
;; * paredit, smartparens, symex, lispy
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
