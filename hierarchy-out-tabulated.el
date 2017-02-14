;;; hierarchy-out-tabulated.el --- Display a hierarchy in a tabulated list  -*- lexical-binding: t; -*-

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

(define-derived-mode hierarchy-out-tabulated-mode tabulated-list-mode
  "Major mode to display a hierarchy as a tabulated list.")

(defun hierarchy-out-tabulated-display (hierarchy displayfn &optional buffer)
  "Display HIERARCHY as a tabulated list in `hierarchy-out-tabulated-mode'.

DISPLAYFN is a function taking an item of HIERARCHY and an indentation
level (a number) as input and returing a string to be displayed in the
table.  Beyond a string, DISPLAYFN can return anything that is accepted as
the contents part of `tabulated-list-entries'.

The tabulated list is displayed in BUFFER, or a newly created buffer if
nil.  The buffer is returned."
  (let ((buffer (or buffer (generate-new-buffer "hierarchy-out-tabulated"))))
    (with-current-buffer buffer
      (hierarchy-out-tabulated-mode)
      (setq tabulated-list-format
            (vector '("Item name" 0 nil)))
      (setq tabulated-list-entries
            (hierarchy-map (lambda (item indent)
                             (list item (vector (funcall displayfn item indent))))
                           hierarchy))
      (tabulated-list-init-header)
      (tabulated-list-print))
    buffer))

(defun hierarchy-out-tabulated-display-buttons (hierarchy labelfn actionfn &optional buffer)
  "Display HIERARCHY as a tabulated list of buttons.

This is similar to `hierarchy-out-tabulated-display' except that each item
is represented as a button triggering an action.

LABELFN is a function taking an item of HIERARCHY and an indentation
value (a number) as input and returning a string to be displayed as a
button label.

ACTIONFN is a function taking an item of HIERARCHY and an indentation
value (a number) as input.  This function is called when an item is
clicked.  The return value of ACTIONFN is ignored.

Each item is displayed indented by two spaces for each level of indentation
in the hierarchy.

The tabulated list is displayed in BUFFER, or a newly created buffer if
nil.  The buffer is returned."
  (hierarchy-out-tabulated-display
   hierarchy
   (lambda (item indent)
     (concat
      (make-string (* 2 indent) ?\s)
      (make-text-button
       (funcall labelfn item indent)
       nil
       'action (lambda (_)
                 (funcall actionfn item indent)))))
   buffer))

(provide 'hierarchy-out-tabulated)
;;; hierarchy-out-tabulated.el ends here
