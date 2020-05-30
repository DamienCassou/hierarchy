;;; hierarchy-examples-delayed.el --- Represent how faces inherit from each other  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Damien Cassou

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

(defun hierarchy-examples-delayed--find-number (num)
  "Find a number, NUM, by adding 1s together until you reach it.

This is entire contrived and mostly meant to be purposefully inefficient to
not be possible on a large scale.

Running the number 200 causes this function to crash; running this function in
`hierarchy-add-tree' with a root of 80 and no delayed children causes that to
 crash."

  (funcall (lambda (funct) (funcall funct 1 funct))
           (lambda (n funct)
             (if (< n num)
                 (+ 1 (funcall funct (+ 1 n) funct))
               1))))

(defun hierarchy-examples-delayed--childrenfn (hier-elem)
  "Return the children of HIER-ELEM.

Basially, feed the number, minus 1, to `hierarchy-examples-delayed--find-number'
and then create a list of the number plus 0.0–0.9."
  
  (when (> hier-elem 1)
    (let ((next (hierarchy-examples-delayed--find-number (1- hier-elem))))
      (mapcar (lambda (dec) (+ next dec)) '(.0 .1 .2 .3 .4 .5 .6 .7 .8 .9)))))

(defun hierarchy-examples-delayed--build-hierarchy ()
  "Return a hierarchy of all numbers from 190 to 1."

  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy                                190 nil
                        #'hierarchy-examples-delayed--childrenfn nil t)

    hierarchy))

(defun hierarchy-examples-delayed-display ()
  "Display hierarchy of all numbers from 190 to 1 in a tree widget."
  (interactive)

  (require 'tree-widget)
  (switch-to-buffer (hierarchy-tree-display
                     (hierarchy-examples-delayed--build-hierarchy)
                     (lambda (item _) (insert (number-to-string item))))))

;; (hierarchy-examples-delayed-display)

(provide 'hierarchy-examples-delayed)
;;; hierarchy-examples-delayed.el ends here
