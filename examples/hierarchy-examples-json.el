;;; hierarchy-examples-json.el --- Represent JSON as a hierarchy  -*- lexical-binding: t; -*-

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
(require 'json)

(defvar hierarchy-examples-json-display-length 3
  "Number of JSON elements to print for an array or object.")

(defun hierarchy-examples-json-object-p (json)
  "Return non-nil if JSON is an object."
  (and
   (consp json)
   (hierarchy-examples-json-pair-p (car json))))

(defun hierarchy-examples-json-pair-p (json)
  "Return non-nil if JSON is an object's pair."
  (and
   (consp json)
   (symbolp (car json))))

(defun hierarchy-examples-json-array-p (json)
  "Return non-nil if JSON is an array."
  (vectorp json))

(defun hierarchy-examples-json--childrenfn (json)
  "Return children of JSON as a list."
  (cond
   ;; children of an object are its name/value pairs
   ((hierarchy-examples-json-object-p json) json)
   ;; children of a name/value pair is the value
   ((hierarchy-examples-json-pair-p json) (list (cdr json)))
   ;; children of an array are its values
   ((hierarchy-examples-json-array-p json) (seq-concatenate 'list json))
   ;; other cases have no children
   (t nil)))

(defun hierarchy-examples-json-create-hierarchy (json)
  "Return a hierarchy from JSON.

JSON should respect this non-default setting for `json-read':

- json-null ⇒ :json-nil

JSON should respect the default settings for `json-read', namely:

- json-object-type ⇒ 'alist

- json-array-type ⇒ 'vector

- json-key-type ⇒ nil

- json-false ⇒ :json-false"
  ;; wrap all JSON element in a cons with a UID so that hierarchy does
  ;; not confuse similar elements
  (let ((hierarchy (hierarchy-new))
        (id 0))
    (hierarchy-add-tree hierarchy (cons id json) nil
                        (lambda (item)
                          (mapcar (lambda (child) (cons (setq id (1+ id)) child))
                                  (hierarchy-examples-json--childrenfn (hierarchy-examples-json--unwrap item)))))
    hierarchy))

(defun hierarchy-examples-json--read ()
  "Read json from point."
  (let ((json-null :json-null))
    (json-read)))

(defun hierarchy-examples-json--read-example ()
  "Return json read from example.json."
  (with-temp-buffer
    (insert-file-contents "example.json")
    (goto-char (point-min))
    (hierarchy-examples-json--read)))

(defun hierarchy-examples-json--unwrap (item)
  "Return JSON element inside ITEM, ignoring UID."
  (cdr item))

(defun hierarchy-examples-json--insert-ellipsis ()
  "Insert an horizontal ellispis in current buffer."
  (insert "…"))

(defun hierarchy-examples-json--insert-sequence (json-seq insertfn)
  "Insert JSON-SEQ (array or object keys) into current buffer.

Call INSERTFN on each item of JSON-SEQ."
  (let ((first t))
    (seq-map (lambda (item)
               (if first
                   (setq first nil)
                 (insert ", "))
               (funcall insertfn item))
             (seq-take json-seq hierarchy-examples-json-display-length)))
  (when (> (seq-length json-seq) hierarchy-examples-json-display-length)
    (insert ", ")
    (hierarchy-examples-json--insert-ellipsis)))

(defun hierarchy-examples-json--insert-object (json-object &optional summarize)
  "Insert JSON-OBJECT into current buffer.

If SUMMARIZE is non-nil, insert a short representation of
JSON-OBJECT instead of a full one."
  (insert "{")
  (if summarize
      (unless (seq-empty-p json-object)
        (hierarchy-examples-json--insert-ellipsis))
    (hierarchy-examples-json--insert-sequence
     (map-keys json-object)
     (lambda (key)
       (insert (format "\"%s\": " key))
       (hierarchy-examples-json--insert (map-elt json-object key) t))))
  (insert "}"))

(defun hierarchy-examples-json--insert-array (json-array &optional summarize)
  "Insert JSON-ARRAY into current buffer.

If SUMMARIZE is non-nil, insert a short representation of JSON-ARRAY
instead of a full one."
  (if summarize
      (insert (format "Array[%s]" (seq-length json-array)))
    (insert "[")
    (hierarchy-examples-json--insert-sequence
     json-array
     (lambda (item) (hierarchy-examples-json--insert item t)))
    (when (> (map-length json-array) hierarchy-examples-json-display-length)
      (insert "..."))
    (insert "]")))

(defun hierarchy-examples-json--insert-pair (json-pair)
  "Insert key of JSON-PAIR into current buffer."
  (insert (format "%s" (car json-pair))))

(defun hierarchy-examples-json--insert (json &optional summarize)
  "Insert into current buffer a short representation of JSON.

If SUMMARIZE is non-nil, insert a short representation of JSON
instead of a full one."
  (cond
   ((hierarchy-examples-json-object-p json) (hierarchy-examples-json--insert-object json summarize))
   ((hierarchy-examples-json-array-p json) (hierarchy-examples-json--insert-array json summarize))
   ((hierarchy-examples-json-pair-p json) (hierarchy-examples-json--insert-pair json))
   (t (insert (format "%s" json)))))

(defun hierarchy-examples-json-display-tree (json)
  "Display hierarchy of JSON in a tree widget."
  (require 'tree-widget)
  (let* ((hierarchy (hierarchy-examples-json-create-hierarchy json))
         (tree-widget (hierarchy-convert-to-tree-widget
                       hierarchy
                       (lambda (item _) (hierarchy-examples-json--insert (hierarchy-examples-json--unwrap item))))))
    (with-current-buffer (get-buffer-create "*hierarchy-examples-json-tree*")
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (widget-create tree-widget)
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer)))))

(defun hierarchy-examples-json-navigate ()
  "Navigate JSON after point."
  (interactive)
  (hierarchy-examples-json-display-tree (hierarchy-examples-json--read-example)))

(provide 'hierarchy-examples-json)
;;; hierarchy-examples-json.el ends here
