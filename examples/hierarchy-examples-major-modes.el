;;; hierarchy-examples-major-modes.el --- Represent how major-modes inherit from each other  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

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

(defun hierarchy-examples-major-modes--major-mode-p (f)
  "Return non-nil if F is a major-mode function."
  ;; copy-edited from counsel.el (in the swiper/ivy repository)
  (and (commandp f) (string-match "-mode$" (symbol-name f))
       (null (help-function-arglist f))))

(defun hierarchy-examples-major-modes--all-major-modes ()
  "Return a list of all major modes."
  (let ((major-modes (list)))
    (mapatoms
     (lambda (symbol)
       (when (hierarchy-examples-major-modes--major-mode-p symbol)
         (setq major-modes (cons symbol major-modes)))))
    major-modes))

(defun hierarchy-examples-major-modes--major-mode-parent (f)
  "Return the major mode F derive from.
If F doesn't derive from any major-mode, return `root-mode'."
  (let ((parent-mode (or (get f 'derived-mode-parent))))
    (cond
     ((eq f 'root-mode) nil)
     ((null parent-mode) 'root-mode)
     (t parent-mode))))

(defun hierarchy-examples-major-modes--major-mode-build-hierarchy ()
  "Return a hierarchy of all major modes."
  (let ((hierarchy (hierarchy-new)))
    (hierarchy-add-trees hierarchy (hierarchy-examples-major-modes--all-major-modes) #'hierarchy-examples-major-modes--major-mode-parent)
    (hierarchy-sort hierarchy)
    hierarchy))

(defun hierarchy-examples-major-modes-display-major-modes ()
  "Display all major modes and their inheritance relationship."
  (interactive)
  (let* ((hierarchy (hierarchy-examples-major-modes--major-mode-build-hierarchy))
         (buffer (hierarchy-tabulated-display
                  hierarchy
                  (hierarchy-labelfn-indent
                   (hierarchy-labelfn-button
                    (lambda (item _) (insert (format "%s" item)))
                    (lambda (item _) (find-function item)))))))
    (switch-to-buffer buffer)))

;; (hierarchy-examples-major-modes-display-major-modes)

(provide 'hierarchy-examples-major-modes)
;;; hierarchy-examples-major-modes.el ends here
