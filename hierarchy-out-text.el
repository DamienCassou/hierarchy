;;; hierarchy-out-text.el --- Display a hierarchy as text  -*- lexical-binding: t; -*-

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

(defun hierarchy-out-text-insert (hierarchy displayfn &optional indent-string buffer)
  "Display HIERARCHY, on item per line passed through DISPLAYFN.

DISPLAYFN is a function taking an item of HIERARCHY and an indentation
level (a number) as input and returing a string to be displayed on a
dedicaded line.

Each item is indented with INDENT-STRING (default is a 2-space string)
below its parent.

The text is inserted in BUFFER, or the current buffer if nil."
  (let ((indent-string (or indent-string "  "))
        (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (hierarchy-map (lambda (item indent)
                       (dotimes (_ indent)
                         (insert indent-string))
                       (insert (funcall displayfn item indent)
                               "\n"))
                     hierarchy))))

(defun hierarchy-out-text-insert-statistics (hierarchy &optional buffer)
  "Show some statistics about HIERARCHY.

The text is inserted in BUFFER, or the current buffer if nil."
  (with-current-buffer (or buffer (current-buffer))
    (insert "Hierarchy:\n"
            "Number of roots: " (int-to-string (length (hierarchy-roots hierarchy))) "\n"
            "Number of elements: " (int-to-string (length (map-keys (hierarchy--seen-items hierarchy)))) "\n"
            "Roots:\n")
    (mapc (lambda (item) (insert "- " item "\n"))
          (hierarchy-roots hierarchy))
    (prin1 (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'hierarchy-out-text)
;;; hierarchy-out-text.el ends here
