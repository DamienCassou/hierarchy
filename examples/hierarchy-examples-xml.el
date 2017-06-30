;;; hierarchy-examples-xml.el --- Represent an XML document as a hierarchy  -*- lexical-binding: t; -*-

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

(require 'xml)

(defun hierarchy-examples-fs-children (node)
  "Return children of NODE."
  (if (stringp node)
      (list)
    (cddr node)))

(defun hierarchy-examples-fs-create-hierarchy (xml-string)
  "Return a hierarchy from XML-STRING."
  (let ((nodes (with-temp-buffer
                 (insert xml-string)
                 (xml-parse-region)))
        (hierarchy (hierarchy-new)))
    (mapc (lambda (node) (hierarchy-add-list hierarchy node t #'hierarchy-examples-fs-children))
          nodes)
    hierarchy))

(defun hierarchy-examples-fs--insert-parameters (pairs)
  "Insert PAIRS in current buffer."
  (mapc (lambda (pair)
          (insert (format "%s=%s" (car pair) (cdr pair))))
        pairs))

(defun hierarchy-examples-fs--insert (node)
  "Insert into current buffer a representation of NODE."
  (cond
   ((stringp node) (insert (format "%s" node)))
   ((cadr node) (insert (format "%s " (car node))) (hierarchy-examples-fs--insert-parameters (cadr node)))
   (t (insert (format "%s" (car node))))))

(defun hierarchy-examples-fs-display-tree (xml-string)
  "Display hierarchy of XML-STRING in a tree widget."
  (switch-to-buffer
   (hierarchy-tree-display
    (hierarchy-examples-fs-create-hierarchy xml-string)
    (lambda (item _) (hierarchy-examples-fs--insert (cdr item))))))

(defvar hierarchy-examples-fs-string "<persons><person name=\"foo\"><address>address of foo</address></person><person name=\"bar\"><address>address of bar</address></person></persons>")

(hierarchy-examples-fs-display-tree hierarchy-examples-fs-string)

(provide 'hierarchy-examples-xml)
;;; hierarchy-examples-xml.el ends here
