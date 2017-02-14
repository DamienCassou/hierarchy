;;; hierarchy-out-text-test.el --- Tests for hierarchy-out-text.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

(require 'ert)
(require 'hierarchy-out-text)

(ert-deftest hierarchy-out-text-insert ()
  (let ((content (with-temp-buffer
                   (hierarchy-out-text-insert (test-helper-animals)
                            (lambda (item _)
                              (symbol-name item)))
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal content "animal\n  bird\n    dove\n    pigeon\n  cow\n  dolphin\n"))))

(ert-deftest hierarchy-out-text-insert-with-indent-string ()
  (let ((content (with-temp-buffer
                   (hierarchy-out-text-insert (test-helper-animals)
                            (lambda (item _)
                              (symbol-name item))
                            "--")
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal content "animal\n--bird\n----dove\n----pigeon\n--cow\n--dolphin\n"))))

(provide 'hierarchy-out-text-test)
;;; hierarchy-out-text-test.el ends here
