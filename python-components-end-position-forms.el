;;; python-components-end-position-forms.el --- -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.


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


(defun py--end-of-block-position ()
  "Return end of block position."
  (save-excursion (py-forward-block)))

(defun py--end-of-block-or-clause-position ()
  "Return end of block-or-clause position."
  (save-excursion (py-forward-block-or-clause)))

(defun py--end-of-class-position ()
  "Return end of class position."
  (save-excursion (py-forward-class)))

(defun py--end-of-clause-position ()
  "Return end of clause position."
  (save-excursion (py-forward-clause)))

(defun py--end-of-comment-position ()
  "Return end of comment position."
  (save-excursion (py-forward-comment)))

(defun py--end-of-def-position ()
  "Return end of def position."
  (save-excursion (py-forward-def)))

(defun py--end-of-def-or-class-position ()
  "Return end of def-or-class position."
  (save-excursion (py-forward-def-or-class)))

(defun py--end-of-expression-position ()
  "Return end of expression position."
  (save-excursion (py-forward-expression)))

(defun py--end-of-except-block-position ()
  "Return end of except-block position."
  (save-excursion (py-forward-except-block)))

(defun py--end-of-if-block-position ()
  "Return end of if-block position."
  (save-excursion (py-forward-if-block)))

(defun py--end-of-indent-position ()
  "Return end of indent position."
  (save-excursion (py-forward-indent)))

(defun py--end-of-line-position ()
  "Return end of line position."
  (save-excursion (py-forward-line)))

(defun py--end-of-minor-block-position ()
  "Return end of minor-block position."
  (save-excursion (py-forward-minor-block)))

(defun py--end-of-partial-expression-position ()
  "Return end of partial-expression position."
  (save-excursion (py-forward-partial-expression)))

(defun py--end-of-paragraph-position ()
  "Return end of paragraph position."
  (save-excursion (py-forward-paragraph)))

(defun py--end-of-section-position ()
  "Return end of section position."
  (save-excursion (py-forward-section)))

(defun py--end-of-statement-position ()
  "Return end of statement position."
  (save-excursion (py-forward-statement)))

(defun py--end-of-top-level-position ()
  "Return end of top-level position."
  (save-excursion (py-forward-top-level)))

(defun py--end-of-try-block-position ()
  "Return end of try-block position."
  (save-excursion (py-forward-try-block)))

(defun py--end-of-block-position-bol ()
  "Return end of block position at ‘beginning-of-line’."
  (save-excursion (py-forward-block-bol)))

(defun py--end-of-block-or-clause-position-bol ()
  "Return end of block-or-clause position at ‘beginning-of-line’."
  (save-excursion (py-forward-block-or-clause-bol)))

(defun py--end-of-class-position-bol ()
  "Return end of class position at ‘beginning-of-line’."
  (save-excursion (py-forward-class-bol)))

(defun py--end-of-clause-position-bol ()
  "Return end of clause position at ‘beginning-of-line’."
  (save-excursion (py-forward-clause-bol)))

(defun py--end-of-def-position-bol ()
  "Return end of def position at ‘beginning-of-line’."
  (save-excursion (py-forward-def-bol)))

(defun py--end-of-def-or-class-position-bol ()
  "Return end of def-or-class position at ‘beginning-of-line’."
  (save-excursion (py-forward-def-or-class-bol)))

(defun py--end-of-elif-block-position-bol ()
  "Return end of elif-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-elif-block-bol)))

(defun py--end-of-else-block-position-bol ()
  "Return end of else-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-else-block-bol)))

(defun py--end-of-except-block-position-bol ()
  "Return end of except-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-except-block-bol)))

(defun py--end-of-for-block-position-bol ()
  "Return end of for-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-for-block-bol)))

(defun py--end-of-if-block-position-bol ()
  "Return end of if-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-if-block-bol)))

(defun py--end-of-indent-position-bol ()
  "Return end of indent position at ‘beginning-of-line’."
  (save-excursion (py-forward-indent-bol)))

(defun py--end-of-minor-block-position-bol ()
  "Return end of minor-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-minor-block-bol)))

(defun py--end-of-statement-position-bol ()
  "Return end of statement position at ‘beginning-of-line’."
  (save-excursion (py-forward-statement-bol)))

(defun py--end-of-try-block-position-bol ()
  "Return end of try-block position at ‘beginning-of-line’."
  (save-excursion (py-forward-try-block-bol)))

(provide (quote python-components-end-position-forms))
;;; python-components-end-position-forms.el ends here
