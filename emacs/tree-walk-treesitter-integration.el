;; -*- lexical-binding: t; -*-

(require 'tree-walk)

;; TODO - how to use treesitter
;; The built-in treesitter support in emacs 29 is in the treesit group.
;; A buffer needs to be activated with (treesit-parser-create LANG-SYM), which attaches it to buffer, and includes it in the list (treesit-parser-list) for the buffer.
;; After buffer initialization, you can use (treesit-node-at POINT) to get leaf nodes (usually not what you actually want) and (treesit-node-on REGION-B REGION-E) to get the smallest node that covers the region.
;; Once you have a node, you can use treesit-node-parent, treesit-node-children, treesit-node-child (to select Nth or by name), treesit-node-start/end, treesit-node-type
;;
;; TODO - how to generically get interesting nodes?  Nodes with types like "(" and ")" are basically just lexer literals, but without a list of them, it's unclear how to differentiate that from eg. "symbol" type (in elisp) that is actually useful.  When printing a node, the printed representation seems to have symbol types sometimes and string types at other times, in a way that seems useful.  But treesit-node-type always returns a string!  But maybe it only looks like a symbol at times due to quirks of how it is presented.
;; TODO - so I think I need per-parser configuration for what nodes are interesting in what contexts.
;;
;; I'm going to write a trial generic handler as tstw-thumb-*


(defun tstw-core-bounds-of-thing-at-point (&optional pt)
  (let* ((pt (or pt (point)))
         (n (treesit-node-at pt)))
    (and n
         (cons (treesit-node-start n)
               (treesit-node-end n)))))

(defun tstw-thumb-node-interesting-p (node)
  (and node
       (let ((type (treesit-node-type node)))
         (not (or (member type '("(" ")"
                                 "{" "}"
                                 "[" "]"
                                 "\""
                                 "'"
                                 "+" "-" "*" "/"
                                 "~"
                                 "!"
                                 "=" "!=" "<" ">" "<=" ">="
                                 "+=" "-=" "/=" "*="
                                 "."
                                 ","
                                 ":" ";"
                                 "function"
                                 "const" "var" "let"
                                 "if" "else" "elif" "fi" "end" "while" "for"
                                 "do" "in" "of" "switch" "continue"
                                 "break"
                                 "return"
                                 ;; TODO - common list of symbols that are likely to be types for uninteresting lexer literals.
                                 ;; TODO - also make this configurable by language but with a reasonable default.
                                 ))
                  ;; TODO - checking if the type equals the string cat catch a bunch of things that the list can't, but it also catches things like symbols with text "symbol"...
                  ;;(equal type (treesit-node-string node))
                  )))))

(defun tstw-thumb-node-at-point (&optional pt)
  (let* ((pt (or pt (point)))
         (n (treesit-node-at (point))))
    (and n (treesit-parent-until n
                                 #'tstw-thumb-node-interesting-p
                                 'include-node))))

(defun tstw-thumb-bounds-of-thing-at-point (&optional pt)
  (let* ((pt (or pt (point)))
         (n (tstw-thumb-node-at-point pt)))
    (and n (cons (treesit-node-start n)
                 (treesit-node-end n)))))

(defun tstw--thumb-bounds-of-children-area (node)
  (let* ((first-interesting (tstw--thumb-first-interesting-child node))
         (left-out-of-bounds (treesit-node-prev-sibling first-interesting))
         (last-interesting (tstw--thumb-last-interesting-child node))
         (right-out-of-bounds (treesit-node-next-sibling last-interesting)))
    (cons (if left-out-of-bounds (treesit-node-end left-out-of-bounds) (treesit-node-start node))
          (if right-out-of-bounds (treesit-node-start right-out-of-bounds) (treesit-node-end node)))))

(defun tstw-thumb-bounds-of-thing-at-point/children-region (&optional pt)
  (let* ((pt (or pt (point)))
         (n (tstw-thumb-node-at-point pt)))
    (and n (tstw--thumb-bounds-of-children-area n))))

(defun tstw-thumb-node-anchor-point (node)
  "Return an anchor point for the node, where hopefully running `tstw-thumb-bounds-of-thing-at-point' at the anchor point will return the same node.
But this is a heuristic thing, so we'll see if it works well."
  ;; My guess is that the first uninteresting node is going to be an anchor point.  A node starts with something uninteresting for a parenthesized list.
  ;; TODO - but higher order application could start with a paren list as the function, then parens for application.  There is no anchor point I can use without also inspecting the parent.  I need something like a list of parent types for the current parser that override child types, but also it should only override some child types, probably...  This is tough to do generically.
  (if (zerop (treesit-node-child-count node))
      (treesit-node-start node)
    (let ((uninteresting (seq-find
                          (lambda (x) (not (tstw-thumb-node-interesting-p x)))
                          (treesit-node-children node))))
      (if uninteresting
          (treesit-node-start uninteresting)
        nil))))
;; TODO - This is working well for elisp, but I would like an end anchor point as well as a start anchor point.
;; TODO - autoloads

;;;###autoload (autoload 'tstw-thumb-up-to-parent-anchor-point "tree-walk-treesitter-integration.el" "" t)
(defun tstw-thumb-up-to-parent-anchor-point (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (n (tstw-thumb-node-at-point))
         (parent n)
         (_parent-set (dotimes (i count)
                        (and parent (setq parent (treesit-node-parent parent)))))
         (parent-anchor (and parent (tstw-thumb-node-anchor-point parent))))
    (and parent-anchor (goto-char parent-anchor))))

(defun tstw--thumb-next-interesting-sibling (node fwd)
  (let* ((sib node)
         (_sib-set (and sib (setq sib
                                  (if fwd
                                      (treesit-node-next-sibling sib)
                                    (treesit-node-prev-sibling sib))))))
    (while (and sib
                (not (tstw-thumb-node-interesting-p sib)))
      (setq sib (if fwd
                    (treesit-node-next-sibling sib)
                  (treesit-node-prev-sibling sib))))
    sib))

;;;###autoload (autoload 'tstw-thumb-forward-sibling-anchor-point "tree-walk-treesitter-integration.el" "" t)
(defun tstw-thumb-forward-sibling-anchor-point (&optional count)
  (interactive "p")
  (let* ((count (or count 1))
         (fwd (<= 0 count))
         (count (abs count))
         (n (tstw-thumb-node-at-point))
         (sib n)
         (_sib-set (dotimes (i count)
                     (setq sib (tstw--thumb-next-interesting-sibling sib fwd)))))
    (let ((anchor (and sib (tstw-thumb-node-anchor-point sib))))
      (and anchor (goto-char anchor)))))
;;;###autoload (autoload 'tstw-thumb-backward-sibling-anchor-point "tree-walk-treesitter-integration.el" "" t)
(defun tstw-thumb-backward-sibling-anchor-point (&optional count)
  (interactive "p")
  (tstw-thumb-forward-sibling-anchor-point (- (or count 1))))

(defun tstw--thumb-first-interesting-child (node)
  (let ((sib (treesit-node-child node 0)))
    (while (and sib
                (not (tstw-thumb-node-interesting-p sib)))
      (setq sib (treesit-node-next-sibling sib)))
    sib))
(defun tstw--thumb-last-interesting-child (node)
  (let ((sib (treesit-node-child node -1)))
    (while (and sib
                (not (tstw-thumb-node-interesting-p sib)))
      (setq sib (treesit-node-prev-sibling sib)))
    sib))

;;;###autoload (autoload 'tstw-thumb-down-to-first-child-anchor-point "tree-walk-treesitter-integration.el" "" t)
(defun tstw-thumb-down-to-first-child-anchor-point ()
  (interactive)
  (let* ((n (tstw-thumb-node-at-point))
         (sib (and n (tstw--thumb-first-interesting-child n)))
         (anchor (and sib (tstw-thumb-node-anchor-point sib))))
    (and anchor (goto-char anchor))))
;;;###autoload (autoload 'tstw-thumb-down-to-last-child-anchor-point "tree-walk-treesitter-integration.el" "" t)
(defun tstw-thumb-down-to-last-child-anchor-point ()
  (interactive)
  (let* ((n (tstw-thumb-node-at-point))
         (sib (and n (tstw--thumb-last-interesting-child n)))
         (anchor (and sib (tstw-thumb-node-anchor-point sib))))
    (and anchor (goto-char anchor))))



(tree-walk-define-operations
 :def-inorder-forward tstw-thumb-forward-inorder-traversal
 :def-inorder-backward tstw-thumb-backward-inorder-traversal

 :def-expand-region tstw-thumb-expand-region
 :def-expand-region-idempotent tstw-thumb-expand-region-idempotent
 :def-select-children-once tstw-thumb-select-children-region-idempotent
 :def-expand-region-to-children/ancestor-generation tstw-thumb-expand-region/children-region
 ;;:def-down-to-last-child tstw-thumb-down-to-last-child-beginning
 :def-transpose-sibling-forward tstw-thumb-transpose-sibling-forward
 :def-transpose-sibling-backward tstw-thumb-transpose-sibling-backward

 :use-down-to-last-child 'tstw-thumb-down-to-last-child-anchor-point

 :use-up-to-parent 'tstw-thumb-up-to-parent-anchor-point
 :use-down-to-first-child 'tstw-thumb-down-to-first-child-anchor-point
 :use-next-sibling 'tstw-thumb-forward-sibling-anchor-point
 :use-previous-sibling 'tstw-thumb-backward-sibling-anchor-point
 :use-bounds 'tstw-thumb-bounds-of-thing-at-point
 :use-children-bounds 'tstw-thumb-bounds-of-thing-at-point/children-region
 )

(require 'repeatable-motion)
;;;###autoload (autoload 'rmo/tstw-thumb-forward-inorder-traversal "tree-walk-treesitter-integration.el" "" t)
;;;###autoload (autoload 'rmo/tstw-thumb-backward-inorder-traversal "tree-walk-treesitter-integration.el" "" t)
(repeatable-motion-define-pair 'tstw-thumb-forward-inorder-traversal 'tstw-thumb-backward-inorder-traversal)
;;;###autoload (autoload 'rmo/tstw-thumb-forward-sibling-anchor-point "tree-walk-treesitter-integration.el" "" t)
;;;###autoload (autoload 'rmo/tstw-thumb-backward-sibling-anchor-point "tree-walk-treesitter-integration.el" "" t)
(repeatable-motion-define-pair 'tstw-thumb-forward-sibling-anchor-point 'tstw-thumb-backward-sibling-anchor-point)
(repeatable-motion-define-pair 'tstw-thumb-up-to-parent-anchor-point 'tstw-thumb-down-to-first-child-anchor-point)
;;;###autoload (autoload 'rmo/tstw-thumb-down-to-last-child-anchor-point "tree-walk-treesitter-integration.el" "" t)
(repeatable-motion-define 'tstw-thumb-down-to-last-child-anchor-point 'tstw-thumb-up-to-parent-anchor-point)

(provide 'tree-walk-treesitter-integration)