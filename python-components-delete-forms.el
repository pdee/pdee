;;; python-components-delete-forms.el --- delete forms -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs
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


(defun py-delete-block ()
  "Delete BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause ()
  "Delete BLOCK-OR-CLAUSE at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "block-or-clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-class (&optional arg)
  "Delete CLASS at point until ‘beginning-of-line’.

Do not store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-clause ()
  "Delete CLAUSE at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "clause")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def (&optional arg)
  "Delete DEF at point until ‘beginning-of-line’.

Do not store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-def-or-class (&optional arg)
  "Delete DEF-OR-CLASS at point until ‘beginning-of-line’.

Do not store data in kill ring.
With ARG \\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included."
  (interactive "P")
 (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base "def-or-class" py-mark-decorators)))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-elif-block ()
  "Delete ELIF-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "elif-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-else-block ()
  "Delete ELSE-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "else-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-except-block ()
  "Delete EXCEPT-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "except-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-for-block ()
  "Delete FOR-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "for-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-if-block ()
  "Delete IF-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "if-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-indent ()
  "Delete INDENT at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "indent")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-minor-block ()
  "Delete MINOR-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "minor-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-statement ()
  "Delete STATEMENT at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "statement")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-try-block ()
  "Delete TRY-BLOCK at point until ‘beginning-of-line’.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base-bol "try-block")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-comment ()
  "Delete COMMENT at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "comment")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-line ()
  "Delete LINE at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "line")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-paragraph ()
  "Delete PARAGRAPH at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "paragraph")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-expression ()
  "Delete EXPRESSION at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-partial-expression ()
  "Delete PARTIAL-EXPRESSION at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "partial-expression")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-section ()
  "Delete SECTION at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "section")))
    (delete-region (car erg) (cdr erg))))

(defun py-delete-top-level ()
  "Delete TOP-LEVEL at point.

Do not store data in kill ring."
  (interactive)
  (let ((erg (py--mark-base "top-level")))
    (delete-region (car erg) (cdr erg))))

(provide (quote python-components-delete-forms))
;; python-components-delete-forms.el ends here
