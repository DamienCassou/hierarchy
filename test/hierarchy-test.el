;;; hierarchy-tests.el --- Tests for hierarchy.el

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

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

(require 'ert)
(require 'hierarchy)

(ert-deftest hierarchy-add-one-root ()
  (let ((parentfn (lambda (_) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))))

(ert-deftest hierarchy-add-one-item-with-parent ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-one-item-with-parent-and-grand-parent ()
  (let ((parentfn (lambda (item) (cl-case item
                              (dove 'bird)
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))))

(ert-deftest hierarchy-add-same-root-twice ()
  (let ((parentfn (lambda (_) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))))

(ert-deftest hierarchy-add-same-child-twice ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-item-and-its-parent ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-item-and-its-child ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal parentfn)
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))))

(ert-deftest hierarchy-add-two-items-sharing-parent ()
  (let ((parentfn (lambda (item) (cl-case item
                              (dove 'bird)
                              (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (equal (hierarchy-roots hierarchy) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(pigeon dove)))))

(ert-deftest hierarchy-add-two-hierarchies ()
  (let ((parentfn (lambda (item) (cl-case item
                              (dove 'bird)
                              (circle 'shape))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'circle parentfn)
    (should (equal (hierarchy-roots hierarchy) '(bird shape)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))
    (should (equal (hierarchy-children hierarchy 'shape) '(circle)))))

(ert-deftest hierarchy-add-with-childrenfn ()
  (let ((childrenfn (lambda (item)
                      (cl-case item
                        (animal '(bird))
                        (bird '(dove pigeon)))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'animal nil childrenfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(pigeon dove)))))

(ert-deftest hierarchy-add-with-parentfn-and-childrenfn ()
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
    (should (equal (hierarchy-roots hierarchy) '(life-form)))
    (should (equal (hierarchy-children hierarchy 'life-form) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(pigeon dove)))
    (should (equal (hierarchy-children hierarchy 'pigeon) '(ashy-wood-pigeon)))))

(ert-deftest hierarchy-add-twice-with-parentfn-and-childrenfn ()
  (let* ((parentfn (lambda (item) (cl-case item
                               (dove 'bird)
                               (bird 'animal))))
         (childrenfn (lambda (item) (cl-case item
                                 (animal '(bird))
                                 (bird '(dove)))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn childrenfn)
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(dove)))))

(ert-deftest hierarchy-add-trees ()
  (let ((parentfn (lambda (item) (cl-case item
                              (dove 'bird)
                              (pigeon 'bird)
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy '(dove pigeon) parentfn)
    (should (equal (hierarchy-roots hierarchy) '(animal)))
    (should (equal (hierarchy-children hierarchy 'animal) '(bird)))
    (should (equal (hierarchy-children hierarchy 'bird) '(pigeon dove)))))

(ert-deftest hierarchy-add-relation-check-error-when-different-parent ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'bird parentfn)
    (should-error
     (hierarchy--add-relation hierarchy 'bird 'cow #'identity))))

(ert-deftest hierarchy-length-of-empty-is-0 ()
  (should (equal (hierarchy-length (hierarchy-new)) 0)))

(ert-deftest hierarchy-length-of-non-empty-counts-items ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal)
                              (dove 'bird)
                              (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (equal (hierarchy-length hierarchy) 4))))

(ert-deftest hierarchy-has-root ()
  (let ((parentfn (lambda (item) (cl-case item
                              (bird 'animal)
                              (dove 'bird)
                              (pigeon 'bird))))
        (hierarchy (hierarchy-new)))
    (should-not (hierarchy-has-root hierarchy 'animal))
    (should-not (hierarchy-has-root hierarchy 'bird))
    (hierarchy-add-tree hierarchy 'dove parentfn)
    (hierarchy-add-tree hierarchy 'pigeon parentfn)
    (should (hierarchy-has-root hierarchy 'animal))
    (should-not (hierarchy-has-root hierarchy 'bird))))

(ert-deftest hierarchy-leafs ()
  (let ((animals (test-helper-animals)))
    (should (equal (hierarchy-leafs animals)
                   '(dove pigeon dolphin cow)))))

(ert-deftest hierarchy-leafs-includes-lonely-roots ()
  (let ((parentfn (lambda (item) nil))
        (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy 'foo parentfn)
    (should (equal (hierarchy-leafs hierarchy)
                   '(foo)))))

(ert-deftest hierarchy-leafs-of-node ()
  (let ((animals (test-helper-animals)))
    (should (equal (hierarchy-leafs animals 'cow) '()))
    (should (equal (hierarchy-leafs animals 'animal) '(dove pigeon dolphin cow)))
    (should (equal (hierarchy-leafs animals 'bird) '(dove pigeon)))
    (should (equal (hierarchy-leafs animals 'dove) '()))))

(ert-deftest hierarchy-child-p ()
  (let ((animals (test-helper-animals)))
    (should (hierarchy-child-p animals 'dove 'bird))
    (should (hierarchy-child-p animals 'bird 'animal))
    (should (hierarchy-child-p animals 'cow 'animal))
    (should-not (hierarchy-child-p animals 'cow 'bird))
    (should-not (hierarchy-child-p animals 'bird 'cow))
    (should-not (hierarchy-child-p animals 'animal 'dove))
    (should-not (hierarchy-child-p animals 'animal 'bird))))

(ert-deftest hierarchy-descendant ()
  (let ((animals (test-helper-animals)))
    (should (hierarchy-descendant-p animals 'dove 'animal))
    (should (hierarchy-descendant-p animals 'dove 'bird))
    (should (hierarchy-descendant-p animals 'bird 'animal))
    (should (hierarchy-descendant-p animals 'cow 'animal))
    (should-not (hierarchy-descendant-p animals 'cow 'bird))
    (should-not (hierarchy-descendant-p animals 'bird 'cow))
    (should-not (hierarchy-descendant-p animals 'animal 'dove))
    (should-not (hierarchy-descendant-p animals 'animal 'bird))))

(ert-deftest hierarchy-descendant-if-not-same ()
  (let ((animals (test-helper-animals)))
    (should-not (hierarchy-descendant-p animals 'cow 'cow))
    (should-not (hierarchy-descendant-p animals 'dove 'dove))
    (should-not (hierarchy-descendant-p animals 'bird 'bird))
    (should-not (hierarchy-descendant-p animals 'animal 'animal))))

(ert-deftest hierarchy-sort ()
  (let ((animals (test-helper-animals)))
    (should (equal (hierarchy-roots animals) '(animal)))
    (should (equal (hierarchy-children animals 'animal) '(bird cow dolphin)))
    (should (equal (hierarchy-children animals 'bird) '(dove pigeon)))))

(ert-deftest hierarchy-map-item-on-leaf ()
  (let* ((animals (test-helper-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'cow
                                     animals)))
    (should (equal result '((cow . 0))))))

(ert-deftest hierarchy-map-item-on-leaf-with-indent ()
  (let* ((animals (test-helper-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'cow
                                     animals
                                     2)))
    (should (equal result '((cow . 2))))))

(ert-deftest hierarchy-map-item-on-parent ()
  (let* ((animals (test-helper-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'bird
                                     animals)))
    (should (equal result '((bird . 0) (dove . 1) (pigeon . 1))))))

(ert-deftest hierarchy-map-item-on-grand-parent ()
  (let* ((animals (test-helper-animals))
         (result (hierarchy-map-item (lambda (item indent) (cons item indent))
                                     'animal
                                     animals)))
    (should (equal result '((animal . 0) (bird . 1) (dove . 2) (pigeon . 2)
                            (cow . 1) (dolphin . 1))))))

(ert-deftest hierarchy-map-conses ()
  (let* ((animals (test-helper-animals))
         (result (hierarchy-map (lambda (item indent)
                                  (cons item indent))
                                animals)))
    (should (equal result '((animal . 0)
                            (bird . 1)
                            (dove . 2)
                            (pigeon . 2)
                            (cow . 1)
                            (dolphin . 1))))))

(ert-deftest hierarchy-map-tree ()
  (let ((animals (test-helper-animals)))
    (should (equal (hierarchy-map-tree (lambda (item indent children)
                                         (list item indent children))
                                       animals)
                   '(animal
                     0
                     ((bird 1 ((dove 2 nil) (pigeon 2 nil)))
                      (cow 1 nil)
                      (dolphin 1 nil)))))))

(ert-deftest hierarchy-extract-tree ()
  (let* ((animals (test-helper-animals))
         (birds (hierarchy-extract-tree animals 'bird)))
    (hierarchy-sort birds)
    (should (equal (hierarchy-roots birds) '(animal)))
    (should (equal (hierarchy-children birds 'animal) '(bird)))
    (should (equal (hierarchy-children birds 'bird) '(dove pigeon)))))

(provide 'hierarchy-test)

;;; hierarchy-test.el ends here
