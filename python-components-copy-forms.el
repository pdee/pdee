;;; python-components-copy-forms.el --- copy forms

;; Copyright (C) 2011-2014  Andreas Roehler
;; Author: Andreas Roehler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-copy-statement ()
  "Copy statement at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-top-level ()
  "Copy top-level at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "top-level")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-top-level-bol ()
  "Delete top-level bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "top-level")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block ()
  "Copy block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-clause ()
  "Copy clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block-or-clause ()
  "Copy block-or-clause at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def ()
  "Copy def at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-class ()
  "Copy class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def-or-class ()
  "Copy def-or-class at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-expression ()
  "Copy expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-expression-bol ()
  "Delete expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-partial-expression ()
  "Copy partial-expression at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "partial-expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-partial-expression-bol ()
  "Delete partial-expression bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "partial-expression")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-minor-block ()
  "Copy minor-block at point.

Store data in kill ring, so it might yanked back. "
  (interactive "*")
  (let ((erg (py--mark-base "minor-block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-copy-minor-block-bol ()
  "Delete minor-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "minor-block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(provide 'python-components-copy-forms)
;; python-components-copy-forms.el ends here