;;; python-components-delete.el --- Delete

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Keywords: languages, convenience

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

;;; Code:

(defun py-delete-statement ()
  "Delete STATEMENT at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-top-level ()
  "Delete TOP-LEVEL at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block ()
  "Delete BLOCK at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause ()
  "Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def ()
  "Delete DEF at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class ()
  "Delete CLASS at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class ()
  "Delete DEF-OR-CLASS at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-expression ()
  "Delete EXPRESSION at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-partial-expression ()
  "Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base "partial-expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block ()
  "Delete MINOR-BLOCK at point.

Don't store data in kill ring.
A minor block is started by a `for', `if', `try' or `with'."
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
    (delete-region (car erg) (cdr erg))))

;; python-components-delete ends here
(provide 'python-components-delete)
