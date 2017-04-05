;;; hierarchy.el --- Library to create, query, navigate and display hierarchy structures  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Creation: After having created a hierarchy with `hierarchy-new', populate it by
;; calling `hierarchy-add-tree' or `hierarchy-add-trees'.  You can then optionally sort its
;; element with `hierarchy-sort'.

;; Querying: You can learn more about your hierarchy by using functions such as
;; `hierarchy-roots', `hierarchy-has-item', `hierarchy-length', `hierarchy-parent', `hierarchy-descendant-p'.

;; Navigation: When your hierarchy is ready, you can use `hierarchy-map-item', `hierarchy-map',
;; and `map-tree' to apply functions to elements of the hierarchy.

;; Display: You can render your hierarchy using an external library such as
;; hierarchy-out-text.el (to display as text), hierarchy-out-tabulated (to display as a tabulated
;; list) and hierarchy-out-tree (to display as a foldable tree widget).

;;; Limitation:

;; Current implementation uses #'equal to find and distinguish elements. Support
;; for user-provided equality definition is desired but not yet implemented.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (hierarchy
               (:constructor hierarchy--make)
               (:conc-name hierarchy--))
  (roots (list)) ; list of the hierarchy roots (no parent)
  (parents (make-hash-table :test 'equal)) ; map an item to its parent
  (children (make-hash-table :test 'equal)) ; map an item to its childre
  ;; cache containing the set of all items in the hierarchy
  (seen-items (make-hash-table :test 'equal)))  ; map an item to t

(defun hierarchy--seen-items-add (hierarchy item)
  "In HIERARCHY, add ITEM to seen items."
  (map-put (hierarchy--seen-items hierarchy) item t))

(defun hierarchy--compute-roots (hierarchy)
  "Search roots of HIERARCHY and return them."
  (cl-set-difference
   (map-keys (hierarchy--seen-items hierarchy))
   (map-keys (hierarchy--parents hierarchy))
   :test #'equal))

(defun hierarchy--sort-roots (hierarchy sortfn)
  "Compute, sort and store the roots of HIERARCHY.

SORTFN is a function taking two items of the hierarchy as parameter and
returning non-nil if the first parameter is lower than the second."
  (setf (hierarchy--roots hierarchy)
        (sort (hierarchy--roots hierarchy)
              sortfn)))

(defun hierarchy--add-relation (hierarchy item parent acceptfn)
  "In HIERARCHY, add ITEM as child of PARENT.

ACCEPTFN is a function returning non-nil if its parameter (any object)
should be an item of the hierarchy."
  (let* ((existing-parent (hierarchy-parent hierarchy item))
         (has-parent-p (funcall acceptfn existing-parent)))
    (cond
     ((and has-parent-p (not (equal existing-parent parent)))
      (error "An item (%s) can only have one parent: '%s' vs '%s'"
             item existing-parent parent))
     ((not has-parent-p)
      (push item (map-elt (hierarchy--children hierarchy) parent (list)))
      (map-put (hierarchy--parents hierarchy) item parent)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-new ()
  "Create a hierarchy and return it."
  (hierarchy--make))

(defun hierarchy-add-tree (hierarchy item parentfn &optional childrenfn acceptfn)
  "In HIERARCHY, add ITEM.

PARENTFN is either nil or a function defining the child-to-parent
relationship: this function takes an item as parameter and should return
the parent of this item in the hierarchy.  If the item has no parent in the
hierarchy (i.e., it should be a root), the function should return an object
not accepted by acceptfn (i.e., nil for the default value of acceptfn).

CHILDRENFN is either nil or a function defining the parent-to-children
relationship: this function takes an item as parameter and should return a
list of children of this item in the hierarchy.

If both PARENTFN and CHILDRENFN are non-nil, the results of PARENTFN and
CHILDRENFN are expected to be coherent with each other.

ACCEPTFN is a function returning non-nil if its parameter (any object)
should be an item of the hierarchy.  By default, ACCEPTFN returns non-nil
if its parameter is non-nil."
  (unless (hierarchy-has-item hierarchy item)
    (let ((acceptfn (or acceptfn #'identity)))
      (hierarchy--seen-items-add hierarchy item)
      (let ((parent (and parentfn (funcall parentfn item))))
        (when (funcall acceptfn parent)
          (hierarchy--add-relation hierarchy item parent acceptfn)
          (hierarchy-add-tree hierarchy parent parentfn childrenfn)))
      (let ((children (and childrenfn (funcall childrenfn item))))
        (mapc (lambda (child)
                (when (funcall acceptfn child)
                  (hierarchy--add-relation hierarchy child item acceptfn)
                  (hierarchy-add-tree hierarchy child parentfn childrenfn)))
              children)))))

(defun hierarchy-add-trees (hierarchy items parentfn &optional childrenfn acceptfn)
  "Call `hierarchy-add-tree' on HIERARCHY and each element of ITEMS.

PARENTFN, CHILDRENFN and ACCEPTFN have the same meaning as in `hierarchy-add'."
  (seq-map (lambda (item)
             (hierarchy-add-tree hierarchy item parentfn childrenfn acceptfn))
           items))

(defun hierarchy-sort (hierarchy &optional sortfn)
  "Modify HIERARCHY so that its roots and item's children are sorted.

SORTFN is a function taking two items of the hierarchy as parameter and
returning non-nil if the first parameter is lower than the second.  By
default, SORTFN is `string-lessp'."
  (let ((sortfn (or sortfn #'string-lessp)))
    (hierarchy--sort-roots hierarchy sortfn)
    (mapc (lambda (parent)
            (setf
             (map-elt (hierarchy--children hierarchy) parent)
             (sort (map-elt (hierarchy--children hierarchy) parent) sortfn)))
          (map-keys (hierarchy--children hierarchy)))))

(defun hierarchy-extract-tree (hierarchy item)
  "Return a copy of HIERARCHY with ITEM's descendants and parents."
  (let ((tree (hierarchy-new)))
    (hierarchy-add-tree tree item
                        (lambda (each) (hierarchy-parent hierarchy each))
                        (lambda (each) (when (or (equal each item)
                                            (hierarchy-descendant-p hierarchy each item))
                                    (hierarchy-children hierarchy each))))
    tree))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-has-item (hierarchy item)
  "Return t if HIERARCHY includes ITEM."
  (map-contains-key (hierarchy--seen-items hierarchy) item))

(defun hierarchy-length (hierarchy)
  "Return the number of items in HIERARCHY."
  (hash-table-count (hierarchy--seen-items hierarchy)))

(defun hierarchy-has-root (hierarchy item)
  "Return t if one of HIERARCHY's roots is ITEM.

A root is an item with no parent."
  (seq-contains (hierarchy-roots hierarchy) item))

(defun hierarchy-roots (hierarchy)
  "Return all roots of HIERARCHY.

A root is an item with no parent."
  (let ((roots (hierarchy--roots hierarchy)))
    (or roots
        (hierarchy--compute-roots hierarchy))))

(defun hierarchy-leafs (hierarchy &optional node)
  "Return all leafs of HIERARCHY.

A leaf is an item with no child.

If NODE is an item of HIERARCHY, only return leafs under NODE."
  (let ((leafs (cl-set-difference
                (map-keys (hierarchy--seen-items hierarchy))
                (map-keys (hierarchy--children hierarchy)))))
    (if (hierarchy-has-item hierarchy node)
        (seq-filter (lambda (item) (hierarchy-descendant-p hierarchy item node)) leafs)
      leafs)))

(defun hierarchy-parent (hierarchy item)
  "In HIERARCHY, return parent of ITEM."
  (map-elt (hierarchy--parents hierarchy) item))

(defun hierarchy-children (hierarchy parent)
  "In HIERARCHY, return children of PARENT."
  (map-elt (hierarchy--children hierarchy) parent (list)))

(defun hierarchy-child-p (hierarchy item1 item2)
  "In HIERARCHY, return non-nil if and only if ITEM1 is a child of ITEM2."
  (equal (hierarchy-parent hierarchy item1) item2))

(defun hierarchy-descendant-p (hierarchy item1 item2)
  "In HIERARCHY, return non-nil if and only if ITEM1 is a descendant of ITEM2.

ITEM1 is a descendant of ITEM2 if and only if both are items of HIERARCHY
and either:

- ITEM1 is child of ITEM2, or
- ITEM1's parent is a descendant of ITEM2."
  (and
   (hierarchy-has-item hierarchy item1)
   (hierarchy-has-item hierarchy item2)
   (or
    (hierarchy-child-p hierarchy item1 item2)
    (hierarchy-descendant-p hierarchy (hierarchy-parent hierarchy item1) item2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hierarchy-map-item (func item hierarchy &optional indent)
  "Return the result of applying FUNC to ITEM and its descendants in HIERARCHY.

This function navigates the tree top-down: FUNCTION is first called on item
and then on each of its children.  Results are concatenated in a list.

INDENT is a number (default 0) representing the indentation of ITEM in
HIERARCHY.  FUNC should take 2 argument: the item and its indentation
level."
  (let ((indent (or indent 0)))
    (cons
     (funcall func item indent)
     (seq-mapcat (lambda (child) (hierarchy-map-item func child hierarchy (1+ indent)))
                 (hierarchy-children hierarchy item)))))

(defun hierarchy-map (func hierarchy &optional indent)
  "Return the result of applying FUNC to each element of HIERARCHY.

This function navigates the tree top-down: FUNCTION is first called on each
root.  To do so, it calls `hierarchy-map-item' on each root
sequentially.  Results are concatenated in a list.

FUNC should take 2 arguments: the item and its indentation level.

INDENT is a number (default 0) representing the indentation of HIERARCHY's
roots."
  (let ((indent (or indent 0)))
    (seq-mapcat (lambda (root) (hierarchy-map-item func root hierarchy indent))
                (hierarchy-roots hierarchy))))

(defun hierarchy-map-tree (function hierarchy &optional item indent)
  "Apply FUNCTION on each item of HIERARCHY under ITEM.

This function navigates the tree bottom-up: FUNCTION is first called on
leafs and the result is passed as parameter when calling FUNCTION on
parents.

FUNCTION should take 3 parameters: the current item, its indentation
level (a number), and a list representing the result of applying
`hierarchy-map-tree' to each child of the item.

INDENT is 0 by default and is passed as second parameter to FUNCTION.
INDENT is incremented by 1 at each level of the tree.

This function returns the result of applying FUNCTION to ITEM (the first
root if nil)."
  (let ((item (or item (car (hierarchy-roots hierarchy))))
        (indent (or indent 0)))
    (funcall function item indent
             (mapcar (lambda (child)
                       (hierarchy-map-tree function hierarchy child (1+ indent)))
                     (hierarchy-children hierarchy item)))))

(provide 'hierarchy)

;;; hierarchy.el ends here
