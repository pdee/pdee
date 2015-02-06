;;; python-components-forms-code.el --- Return Python forms' code 

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

;;; Commentary: This fiel is generated `py-write-forms-code' from
;;  python-mode-utils.el in directory devel. I.e. edits here might not
;;  be persistent.

;;; Code:


(defun py-statement ()
  "Statement at point.

Return code of `py-statement' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "statement")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-top-level ()
  "Top-Level at point.

Return code of `py-top-level' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "top-level")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-block ()
  "Block at point.

Return code of `py-block' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "block")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-clause ()
  "Clause at point.

Return code of `py-clause' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "clause")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-block-or-clause ()
  "Block-Or-Clause at point.

Return code of `py-block-or-clause' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "block-or-clause")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-def ()
  "Def at point.

Return code of `py-def' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "def")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-class ()
  "Class at point.

Return code of `py-class' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "class")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-def-or-class ()
  "Def-Or-Class at point.

Return code of `py-def-or-class' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "def-or-class")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-expression ()
  "Expression at point.

Return code of `py-expression' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "expression")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-partial-expression ()
  "Partial-Expression at point.

Return code of `py-partial-expression' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "partial-expression")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(defun py-minor-block ()
  "Minor-Block at point.

Return code of `py-minor-block' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "minor-block")))
    (buffer-substring-no-properties (car erg) (cdr erg))))

(provide 'python-components-forms-code)
;;;  python-components-forms-code.el ends here
