;;; hierarchy-examples-fs.el --- Some example hierarchies  -*- lexical-binding: t; -*-

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

(defun hierarchy-examples-fs-directory-p (file)
  "Return non-nil if FILE is a directory and not . or ..."
  (and (not (string-suffix-p "/." file))
       (not (string-suffix-p "/.." file))
       (file-directory-p file)))

(defun hierarchy-examples-fs-children (folder)
  "Return sub-directories of FOLDER as absolute paths."
  (when (file-directory-p folder)
    (seq-filter #'hierarchy-examples-fs-directory-p (directory-files folder t))))

(defun hierarchy-examples-fs-parent (folder)
  "Return parent of FOLDER."
  (when (not (string= folder "/"))
    (directory-file-name (file-name-directory folder))))

(defun hierarchy-examples-fs-build-fs-hierarchy (folder)
  "Return hierarchy of FOLDER."
  (let* ((folder (expand-file-name folder))
         (parentfn #'hierarchy-examples-fs-parent)
         (childrenfn (lambda (file) (when (string-prefix-p folder file)
                                 (hierarchy-examples-fs-children file))))
         (hierarchy (hierarchy-new)))
    (hierarchy-add-tree hierarchy folder parentfn childrenfn)
    (hierarchy-sort hierarchy)
    hierarchy))

(defun hierarchy-examples-fs-labelfn (folder _)
  "Insert name of FOLDER at current position.

_ is ignored."
  (insert (if (string= folder "/")
              "/"
            (file-name-nondirectory folder))))

(defun hierarchy-examples-fs-display-filesystem (&optional folder)
  "Display hierarchy of FOLDER in a tabulated list."
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    #'hierarchy-examples-fs-labelfn (lambda (item _) (dired item)))))))
    (switch-to-buffer buffer)))

;; (hierarchy-examples-fs-display-filesystem "~/.emacs.d")

(defun hierarchy-examples-fs-display-filesystem-tree (&optional folder)
  "Display hierarchy of FOLDER in a tree widget."
  (let* ((hierarchy (hierarchy-examples-fs-build-fs-hierarchy folder))
         (tree-widget (hierarchy-convert-to-tree-widget
                       hierarchy #'hierarchy-examples-fs-labelfn)))
    (with-current-buffer (get-buffer-create "*hierarchy-examples-fs-tree*")
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create tree-widget))
      (switch-to-buffer (current-buffer)))))

;; (hierarchy-examples-fs-display-filesystem-tree "~/.emacs.d")

(provide 'hierarchy-examples-fs)
;;; hierarchy-examples-fs.el ends here
