;;; hierarchy-examples-faces.el --- Represent how faces inherit from each other  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

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

(require 'hierarchy)

(defun hierarchy-examples-faces--parent (face)
  "Return parent face of FACE.
If FACE doesn't inherit from any other face or inherit from an
invalid face, return 'root-face."
  (let ((parent-face (if (eq face 'root-face)
                         nil ;; the root has no parent
                       (or (face-attribute face :inherit nil 'default)
                           'root-face))))
    (cond ((facep parent-face) parent-face)
          ((null parent-face) nil) ;; the root has no parent
          (t 'root-face))))

(defun hierarchy-examples-faces--build-hierarchy ()
  "Return a hierarchy of all faces Emacs knows about."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy (face-list) #'hierarchy-examples-faces--parent)
    hierarchy))

(defun hierarchy-examples-faces-display-faces ()
  "Display hierarchy of all faces Emacs knows about in a tree widget."
  (interactive)
  (require 'tree-widget)
  (let* ((hierarchy (hierarchy-examples-faces--build-hierarchy)))
    (switch-to-buffer
     (hierarchy-tree-display
      hierarchy
      (lambda (face _) (insert (format "%s" face)))))))

;; (hierarchy-examples-faces-display-faces)

(provide 'hierarchy-examples-faces)
;;; hierarchy-examples-faces.el ends here
