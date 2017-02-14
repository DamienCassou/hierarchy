;;; hierarchy-examples.el --- Some example hierarchies  -*- lexical-binding: t; -*-

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
(require 'hierarchy-out-tabulated)
(require 'hierarchy-out-tree)
(require 'f)
(require 's)

(defun hierarchy-examples-build-fs-hierarchy (folder)
  "Return hierarchy of FOLDER."
  (let* ((folder (f-expand folder))
         (parentfn (lambda (file) (f-parent file)))
         (childrenfn (lambda (file) (when (and (f-directory? file)
                                          (s-starts-with? folder file))
                                 (f-directories file))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy folder parentfn childrenfn)
    ;; sort filenames alphabetically
    (hierarchy-sort hierarchy)
    hierarchy))

(defun hierarchy-examples-display-filesystem (&optional folder)
  "Display hierarchy of FOLDER in a tabulated list."
  (let* ((hierarchy (hierarchy-examples-build-fs-hierarchy folder))
         (buffer (hierarchy-out-tabulated-display-buttons
                  hierarchy
                  ;; labelfn: convert a path to a filename
                  (lambda (item _) (if (string= item "/") "/" (f-filename item)))
                  ;; actionfn: convert a path to an action
                  (lambda (item _) (message "%s" item)))))
    (switch-to-buffer buffer)))

;; (hierarchy-examples-display-filesystem "~/.emacs.d")

(defun hierarchy-examples-display-filesystem-tree (&optional folder)
  "Display hierarchy of FOLDER in a tree widget."
  (let* ((hierarchy (hierarchy-examples-build-fs-hierarchy folder))
         (tree-widget (hierarchy-out-tree-convert-to-widget
                       hierarchy
                       ;; labelfn: convert a path to a filename
                       (lambda (item _) (if (string= item "/") "/" (f-filename item))))))
    (with-current-buffer (get-buffer-create "*hierarchy-examples-tree*")
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create tree-widget))
      (switch-to-buffer (current-buffer)))))

;; (hierarchy-examples-display-filesystem-tree "~/.emacs.d")


(provide 'hierarchy-examples-fs)
;;; hierarchy-examples-fs.el ends here
