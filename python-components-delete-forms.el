;;; python-components-delete-forms.el --- delete forms

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


(defun py-delete-statement ()
  "Delete STATEMENT at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "statement")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-top-level ()
  "Delete TOP-LEVEL at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "top-level")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block ()
  "Delete BLOCK at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-clause ()
  "Delete CLAUSE at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause ()
  "Delete BLOCK-OR-CLAUSE at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "block-or-clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def (&optional arg)
  "Delete DEF at point.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class (&optional arg)
  "Delete CLASS at point.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class (&optional arg)
  "Delete DEF-OR-CLASS at point.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def-or-class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-expression ()
  "Delete EXPRESSION at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-partial-expression ()
  "Delete PARTIAL-EXPRESSION at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "partial-expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block ()
  "Delete MINOR-BLOCK at point.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "minor-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-paragraph-bol ()
  "Delete PARAGRAPH at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "paragraph") nil t))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-bol ()
  "Delete BLOCK at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "block") nil t))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block-bol ()
  "Delete MINOR-BLOCK at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "minor-block") nil t))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-clause-bol ()
  "Delete CLAUSE at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "clause") nil t))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause-bol ()
  "Delete BLOCK-OR-CLAUSE at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "block-or-clause") nil t))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-bol (&optional arg)
  "Delete DEF at point until beginning-of-line.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def" py-mark-decorators t)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class-bol (&optional arg)
  "Delete CLASS at point until beginning-of-line.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "class" py-mark-decorators t)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class-bol (&optional arg)
  "Delete DEF-OR-CLASS at point until beginning-of-line.

Don't store data in kill ring. 
With \\[universal-argument] or `py-mark-decorators' set to `t', `decorators' are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def-or-class" py-mark-decorators t)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-statement-bol ()
  "Delete STATEMENT at point until beginning-of-line.

Don't store data in kill ring. "
  (interactive)
  (let ((erg (py--mark-base "statement") nil t))
    (delete-region (car erg) (cdr erg))))

(provide 'python-components-delete-forms)
;; python-components-delete-forms.el ends here