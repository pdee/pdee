;;; python-components-copy.el --- Copy

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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

(defun py-copy-statement ()
  "Copy statement at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-top-level ()
  "Copy top-level at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block ()
  "Copy block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-clause ()
  "Copy clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block-or-clause ()
  "Copy block-or-clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def ()
  "Copy def at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-class ()
  "Copy class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def-or-class ()
  "Copy def-or-class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-expression ()
  "Copy expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-partial-expression ()
  "Copy partial-expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "partial-expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-minor-block ()
  "Copy minor-block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
    (copy-region-as-kill (car erg) (cdr erg))))

;; python-components-copy ends here
(provide 'python-components-copy)
