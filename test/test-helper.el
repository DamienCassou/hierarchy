;;; test-helper.el --- Helper functions to test hierarchies  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(declare-function undercover "undercover")

(when (require 'undercover nil t)
  (undercover "hierarchy.el"))

(require 'hierarchy)

(defun test-helper-animals ()
  "Create a sorted animal hierarchy."
  (let ((parentfn (lambda (item) (cl-case item
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

;;; test-helper.el ends here
