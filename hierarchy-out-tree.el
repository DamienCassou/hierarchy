;;; hierarchy-out-tree.el --- Display a hierarchy in a tree buffer  -*- lexical-binding: t; -*-

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
(require 'wid-edit)

(defun hierarchy-out-tree-convert-to-widget (hierarchy labelfn)
  "Return a `tree-widget` for HIERARCHY.

LABELFN is a function taking an item of HIERARCHY and an indentation
value (a number) as parameter and returning a string to be displayed as a
button label."
  (hierarchy-map-tree (lambda (item indent children)
                        (widget-convert
                         'tree-widget
                         :tag (funcall labelfn item indent)
                         :args children))
                      hierarchy))

(provide 'hierarchy-out-tree)
;;; hierarchy-out-tree.el ends here
