;;; python-components-kill-forms.el --- kill forms

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-kill-block ()
  "Delete `block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete `block-or-clause' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "block-or-clause")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete `class' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete `clause' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "clause")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def ()
  "Delete `def' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "def")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def-or-class ()
  "Delete `def-or-class' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-elif-block ()
  "Delete `elif-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "elif-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-else-block ()
  "Delete `else-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "else-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-except-block ()
  "Delete `except-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "except-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-expression ()
  "Delete `expression' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-for-block ()
  "Delete `for-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "for-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-if-block ()
  "Delete `if-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "if-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-minor-block ()
  "Delete `minor-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "minor-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-partial-expression ()
  "Delete `partial-expression' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-section ()
  "Delete `section' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "section")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete `statement' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-top-level ()
  "Delete `top-level' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "top-level")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-try-block ()
  "Delete `try-block' at point.

Stores data in kill ring"
  (interactive "*")
  (let ((erg (py--mark-base "try-block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-if-block-bol ()
  "Delete if-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-try-block-bol ()
  "Delete try-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-minor-block-bol ()
  "Delete minor-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-for-block-bol ()
  "Delete for-block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-top-level-bol ()
  "Delete top-level bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py--mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(provide 'python-components-kill-forms)
;;; python-components-kill-forms.el ends here
