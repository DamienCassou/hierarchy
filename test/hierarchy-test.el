;;; hierarchy-test.el --- Tests for hierarchy.el

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Maintainer: Damien Cassou <damien@cassou.me>
;; Version: 0.7.0
;; Package-Requires: ((emacs "25.1"))
;; GIT: https://github.com/DamienCassou/hierarchy
;; URL: https://github.com/DamienCassou/hierarchy

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

;; Tests for hierarchy.el

;;; Code:

(declare-function undercover "undercover")

(when (require 'undercover nil t)
  (undercover "hierarchy.el"))

(require 'hierarchy)

(require 'buttercup)

(defun hierarchy-test-animal ()
  "Create a sorted animal hierarchy."
  (let ((parentfn (lambda (item)
                    (cl-case item
                      (dove 'bird)
                      (pigeon 'bird)
                      (bird 'animal)
                      (dolphin 'animal)
                      (cow 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (hierarchy-add-tree hierarchy 'dolphin parentfn)
    (hierarchy-add-tree hierarchy 'cow parentfn)
    (hierarchy-sort hierarchy)
    hierarchy))

(describe "hierarchy"
  (it "add-one-root"
    (let ((parentfn (lambda (_) nil))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'animal parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))))

  (it "add-one-item-with-parent"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))))

  (it "add-one-item-with-parent-and-grand-parent"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (dove 'bird)
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'dove parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(dove))))

  (it "add-same-root-twice"
    (let ((parentfn (lambda (_) nil))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'animal parentfn)
      (hierarchy-add-tree hierarchy 'animal parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))))

  (it "add-same-child-twice"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))))

  (it "add-item-and-its-parent"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (hierarchy-add-tree hierarchy 'animal parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))))

  (it "add-item-and-its-child"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'animal parentfn)
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))))

  (it "add-two-items-sharing-parent"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (dove 'bird)
                        (pigeon 'bird))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'dove parentfn)
      (hierarchy-add-tree hierarchy 'pigeon parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(pigeon dove))))

  (it "add-two-hierarchies"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (dove 'bird)
                        (circle 'shape))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'dove parentfn)
      (hierarchy-add-tree hierarchy 'circle parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(bird shape))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(dove))
      (expect (hierarchy-children hierarchy 'shape) :to-equal '(circle))))

  (it "add-with-childrenfn"
    (let ((childrenfn (lambda (item)
                        (cl-case item
                          (animal '(bird))
                          (bird '(dove pigeon)))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'animal nil childrenfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(pigeon dove))))

  (it "add-with-parentfn-and-childrenfn"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal)
                        (animal 'life-form))))
          (childrenfn (lambda (item)
                        (cl-case item
                          (bird '(dove pigeon))
                          (pigeon '(ashy-wood-pigeon)))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn childrenfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(life-form))
      (expect (hierarchy-children hierarchy 'life-form) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(pigeon dove))
      (expect (hierarchy-children hierarchy 'pigeon) :to-equal '(ashy-wood-pigeon))))

  (it "add-twice-with-parentfn-and-childrenfn"
    (let* ((parentfn (lambda (item)
                       (cl-case item
                         (dove 'bird)
                         (bird 'animal))))
           (childrenfn (lambda (item)
                         (cl-case item
                           (animal '(bird))
                           (bird '(dove)))))
           (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn childrenfn)
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(dove))))

  (it "add-trees"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (dove 'bird)
                        (pigeon 'bird)
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-trees hierarchy '(dove pigeon) parentfn)
      (expect (hierarchy-roots hierarchy) :to-equal '(animal))
      (expect (hierarchy-children hierarchy 'animal) :to-equal '(bird))
      (expect (hierarchy-children hierarchy 'bird) :to-equal '(pigeon dove))))

  (it "from-list"
    (let ((hierarchy (hierarchy-from-list
                      '(animal (bird (dove)
                                     (pigeon))
                               (cow)
                               (dolphin)))))
      (hierarchy-sort hierarchy (lambda (item1 item2)
                                  (string< (car item1)
                                           (car item2))))
      (expect (hierarchy-to-string hierarchy (lambda (item) (symbol-name (car item)))) :to-equal
              "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n")))

  (it "from-list-with-duplicates"
    (let ((hierarchy (hierarchy-from-list
                      '(a (b) (b))
                      t)))
      (hierarchy-sort hierarchy (lambda (item1 item2)
                                  ;; sort by ID
                                  (< (car item1) (car item2))))
      (expect (hierarchy-length hierarchy) :to-equal 3)
      (expect (hierarchy-to-string
               hierarchy
               (lambda (item)
                 (format "%s(%s)"
                         (cadr item)
                         (car item)))) :to-equal
                         "a(1)\n  b(2)\n  b(3)\n")))

  (it "from-list-with-childrenfn"
    (let ((hierarchy (hierarchy-from-list
                      "abc"
                      nil
                      (lambda (item)
                        (when (string= item "abc")
                          (split-string item "" t))))))
      (hierarchy-sort hierarchy (lambda (item1 item2) (string< item1 item2)))
      (expect (hierarchy-length hierarchy) :to-equal 4)
      (expect (hierarchy-to-string hierarchy) :to-equal
              "abc\n  a\n  b\n  c\n")))

  (it "add-relation-check-error-when-different-parent"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'bird parentfn)
      (expect
       (lambda () (hierarchy--add-relation hierarchy 'bird 'cow #'identity))
       :to-throw)))

  (it "empty-p-return-non-nil-for-empty"
    (expect (hierarchy-empty-p (hierarchy-new)) :to-be-truthy))

  (it "empty-p-return-nil-for-non-empty"
    (expect (hierarchy-empty-p (hierarchy-test-animal)) :not :to-be-truthy))

  (it "length-of-empty-is-0"
    (expect (hierarchy-length (hierarchy-new)) :to-equal 0))

  (it "length-of-non-empty-counts-items"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal)
                        (dove 'bird)
                        (pigeon 'bird))))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'dove parentfn)
      (hierarchy-add-tree hierarchy 'pigeon parentfn)
      (expect (hierarchy-length hierarchy) :to-equal 4)))

  (it "has-root"
    (let ((parentfn (lambda (item)
                      (cl-case item
                        (bird 'animal)
                        (dove 'bird)
                        (pigeon 'bird))))
          (hierarchy (hierarchy-new)))
      (expect (hierarchy-has-root hierarchy 'animal) :not :to-be-truthy)
      (expect (hierarchy-has-root hierarchy 'bird) :not :to-be-truthy)
      (hierarchy-add-tree hierarchy 'dove parentfn)
      (hierarchy-add-tree hierarchy 'pigeon parentfn)
      (expect (hierarchy-has-root hierarchy 'animal) :to-be-truthy)
      (expect (hierarchy-has-root hierarchy 'bird) :not :to-be-truthy)))

  (it "leafs"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-leafs animals) :to-equal
              '(dove pigeon dolphin cow))))

  (it "leafs-includes-lonely-roots"
    (let ((parentfn (lambda (item) nil))
          (hierarchy (hierarchy-new)))
      (hierarchy-add-tree hierarchy 'foo parentfn)
      (expect (hierarchy-leafs hierarchy) :to-equal
              '(foo))))

  (it "leafs-of-node"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-leafs animals 'cow) :to-equal '())
      (expect (hierarchy-leafs animals 'animal) :to-equal '(dove pigeon dolphin cow))
      (expect (hierarchy-leafs animals 'bird) :to-equal '(dove pigeon))
      (expect (hierarchy-leafs animals 'dove) :to-equal '())))

  (it "child-p"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-child-p animals 'dove 'bird) :to-be-truthy)
      (expect (hierarchy-child-p animals 'bird 'animal) :to-be-truthy)
      (expect (hierarchy-child-p animals 'cow 'animal) :to-be-truthy)
      (expect (hierarchy-child-p animals 'cow 'bird) :not :to-be-truthy)
      (expect (hierarchy-child-p animals 'bird 'cow) :not :to-be-truthy)
      (expect (hierarchy-child-p animals 'animal 'dove) :not :to-be-truthy)
      (expect (hierarchy-child-p animals 'animal 'bird) :not :to-be-truthy)))

  (it "descendant"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-descendant-p animals 'dove 'animal) :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'dove 'bird) :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'bird 'animal) :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'cow 'animal) :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'cow 'bird) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'bird 'cow) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'animal 'dove) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'animal 'bird) :not :to-be-truthy)))

  (it "descendant-if-not-same"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-descendant-p animals 'cow 'cow) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'dove 'dove) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'bird 'bird) :not :to-be-truthy)
      (expect (hierarchy-descendant-p animals 'animal 'animal) :not :to-be-truthy)))

  ;; keywords supported: :test :key
  (it "set-equal"
    (expect     (hierarchy--set-equal '(1 2 3) '(1 2 3)) :to-be-truthy)
    (expect     (hierarchy--set-equal '(1 2 3) '(3 2 1)) :to-be-truthy)
    (expect     (hierarchy--set-equal '(3 2 1) '(1 2 3)) :to-be-truthy)
    (expect (hierarchy--set-equal '(2 3) '(3 2 1)) :not :to-be-truthy)
    (expect (hierarchy--set-equal '(1 2 3) '(2 3)) :not :to-be-truthy)
    (expect (hierarchy--set-equal '("1" "2") '("2" "1") :test #'eq) :not :to-be-truthy)
    (expect     (hierarchy--set-equal '("1" "2") '("2" "1") :test #'equal) :to-be-truthy)
    (expect (hierarchy--set-equal '(1 2) '(-1 -2)) :not :to-be-truthy)
    (expect     (hierarchy--set-equal '(1 2) '(-1 -2) :key #'abs) :to-be-truthy)
    (expect (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2))) :not :to-be-truthy)
    (expect (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :key #'car) :not :to-be-truthy)
    (expect (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :test #'equal) :not :to-be-truthy)
    (expect     (hierarchy--set-equal '(("1" 1) ("2" 1)) '(("1" 2) ("2" 2)) :key #'car :test #'equal) :to-be-truthy))

  (it "equal-returns-true-for-same-hierarchy"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-equal animals animals) :to-be-truthy)
      (expect (hierarchy-equal (hierarchy-test-animal) animals) :to-be-truthy)))

  (it "equal-returns-true-for-copies"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-equal animals (hierarchy-copy animals)) :to-be-truthy)))

  (it "sort"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-roots animals) :to-equal '(animal))
      (expect (hierarchy-children animals 'animal) :to-equal '(bird cow dolphin))
      (expect (hierarchy-children animals 'bird) :to-equal '(dove pigeon))))

  (it "map-item-on-leaf"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                       'cow
                                       animals)))
      (expect result :to-equal '((cow . 0)))))

  (it "map-item-on-leaf-with-indent"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                       'cow
                                       animals
                                       2)))
      (expect result :to-equal '((cow . 2)))))

  (it "map-item-on-parent"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                       'bird
                                       animals)))
      (expect result :to-equal '((bird . 0) (dove . 1) (pigeon . 1)))))

  (it "map-item-on-grand-parent"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                       'animal
                                       animals)))
      (expect result :to-equal '((animal . 0) (bird . 1) (dove . 2) (pigeon . 2)
                                 (cow . 1) (dolphin . 1)))))

  (it "map-conses"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map (lambda (item indent)
                                    (cons item indent))
                                  animals)))
      (expect result :to-equal '((animal . 0)
                                 (bird . 1)
                                 (dove . 2)
                                 (pigeon . 2)
                                 (cow . 1)
                                 (dolphin . 1)))))

  (it "map-tree"
    (let ((animals (hierarchy-test-animal)))
      (expect (hierarchy-map-tree (lambda (item indent children)
                                    (list item indent children))
                                  animals) :to-equal
                                  '(animal
                                    0
                                    ((bird 1 ((dove 2 nil) (pigeon 2 nil)))
                                     (cow 1 nil)
                                     (dolphin 1 nil))))))

  (it "map-keeps-hierarchy"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-map-hierarchy (lambda (item _) (identity item))
                                            animals)))
      (expect (hierarchy-equal animals result) :to-be-truthy)))

  (it "map-applies-function"
    (let* ((animals (hierarchy-test-animal))
           (parentfn (lambda (item)
                       (cond
                        ((equal item "bird") "animal")
                        ((equal item "dove") "bird")
                        ((equal item "pigeon") "bird")
                        ((equal item "cow") "animal")
                        ((equal item "dolphin") "animal"))))
           (expected (hierarchy-new)))
      (hierarchy-add-tree expected "dove" parentfn)
      (hierarchy-add-tree expected "pigeon" parentfn)
      (hierarchy-add-tree expected "cow" parentfn)
      (hierarchy-add-tree expected "dolphin" parentfn)
      (expect (hierarchy-equal
               (hierarchy-map-hierarchy (lambda (item _) (symbol-name item)) animals)
               expected) :to-be-truthy)))

  (it "extract-tree"
    (let* ((animals (hierarchy-test-animal))
           (birds (hierarchy-extract-tree animals 'bird)))
      (hierarchy-sort birds)
      (expect (hierarchy-roots birds) :to-equal '(animal))
      (expect (hierarchy-children birds 'animal) :to-equal '(bird))
      (expect (hierarchy-children birds 'bird) :to-equal '(dove pigeon))))

  (it "extract-tree-nil-if-not-in-hierarchy"
    (let* ((animals (hierarchy-test-animal)))
      (expect (hierarchy-extract-tree animals 'foobar) :not :to-be-truthy)))

  (it "items-of-empty-is-empty"
    (expect (seq-empty-p (hierarchy-items (hierarchy-new))) :to-be-truthy))

  (it "items-returns-sequence-of-same-length"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-items animals)))
      (expect (seq-length result) :to-be (hierarchy-length animals))))

  (it "items-return-all-elements-of-hierarchy"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-items animals)))
      (expect (seq-sort #'string< result) :to-equal '(animal bird cow dolphin dove pigeon))))

  (it "labelfn-indent-no-indent-if-0"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (labelfn (hierarchy-labelfn-indent labelfn-base)))
      (expect (with-temp-buffer
                (funcall labelfn "bar" 0)
                (buffer-substring (point-min) (point-max))) :to-equal
                "foo")))

  (it "labelfn-indent-three-times-if-3"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (labelfn (hierarchy-labelfn-indent labelfn-base)))
      (expect (with-temp-buffer
                (funcall labelfn "bar" 3)
                (buffer-substring (point-min) (point-max))) :to-equal
                "      foo")))

  (it "labelfn-indent-default-indent-string"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (labelfn (hierarchy-labelfn-indent labelfn-base)))
      (expect (with-temp-buffer
                (funcall labelfn "bar" 1)
                (buffer-substring (point-min) (point-max))) :to-equal
                "  foo")))

  (it "labelfn-indent-custom-indent-string"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (labelfn (hierarchy-labelfn-indent labelfn-base "###"))
           (content (with-temp-buffer
                      (funcall labelfn "bar" 1)
                      (buffer-substring (point-min) (point-max)))))
      (expect content :to-equal "###foo")))

  (it "labelfn-button-propertize"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (actionfn #'identity)
           (labelfn (hierarchy-labelfn-button labelfn-base actionfn))
           (properties (with-temp-buffer
                         (funcall labelfn "bar" 1)
                         (text-properties-at 1))))
      (expect (car properties) :to-equal 'action)))

  (it "labelfn-button-execute-labelfn"
    (let* ((labelfn-base (lambda (_item _indent) (insert "foo")))
           (actionfn #'identity)
           (labelfn (hierarchy-labelfn-button labelfn-base actionfn))
           (content (with-temp-buffer
                      (funcall labelfn "bar" 1)
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (expect content :to-equal "foo")))

  (it "labelfn-button-if-does-not-button-unless-condition"
    (let ((labelfn-base (lambda (_item _indent) (insert "foo")))
          (spy-count 0)
          (condition (lambda (_item _indent) nil)))
      (cl-letf (((symbol-function 'hierarchy-labelfn-button) (lambda (_labelfn _actionfn) (lambda (_item _indent) (cl-incf spy-count)))))
        (funcall (hierarchy-labelfn-button-if labelfn-base condition #'identity) nil nil)
        (expect spy-count :to-equal 0))))

  (it "labelfn-button-if-does-button-when-condition"
    (let ((labelfn-base (lambda (_item _indent) (insert "foo")))
          (spy-count 0)
          (condition (lambda (_item _indent) t)))
      (cl-letf (((symbol-function 'hierarchy-labelfn-button) (lambda (_labelfn _actionfn) (lambda (_item _indent) (cl-incf spy-count)))))
        (funcall (hierarchy-labelfn-button-if labelfn-base condition #'identity) nil nil)
        (expect spy-count :to-equal 1))))

  (it "labelfn-to-string"
    (let ((labelfn (lambda (item _indent) (insert item))))
      (expect (hierarchy-labelfn-to-string labelfn "foo" 1) :to-equal "foo")))

  (it "print" ()
      (let* ((animals (hierarchy-test-animal))
             (result (with-temp-buffer
                       (hierarchy-print animals)
                       (buffer-substring-no-properties (point-min) (point-max)))))
        (expect result :to-equal "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n")))

  (it "to-string"
    (let* ((animals (hierarchy-test-animal))
           (result (hierarchy-to-string animals)))
      (expect result :to-equal "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n")))

  (it "tabulated-display"
    (let* ((animals (hierarchy-test-animal))
           (labelfn (lambda (item _indent) (insert (symbol-name item))))
           (contents (with-temp-buffer
                       (hierarchy-tabulated-display animals labelfn (current-buffer))
                       (buffer-substring-no-properties (point-min) (point-max)))))
      (expect contents :to-equal "animal\nbird\ndove\npigeon\ncow\ndolphin\n"))))

(provide 'hierarchy-test)
;;; hierarchy-test.el ends here
