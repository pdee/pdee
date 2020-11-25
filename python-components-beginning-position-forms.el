;;; python-components-beginning-position-forms.el --- -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015-2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: languages

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


(defun py--beginning-of-block-position ()
  "Return beginning of block position."
  (save-excursion
    (or (py--beginning-of-block-p)
        (py-backward-block))))

(defun py--beginning-of-block-or-clause-position ()
  "Return beginning of block-or-clause position."
  (save-excursion
    (or (py--beginning-of-block-or-clause-p)
        (py-backward-block-or-clause))))

(defun py--beginning-of-class-position ()
  "Return beginning of class position."
  (save-excursion
    (or (py--beginning-of-class-p)
        (py-backward-class))))

(defun py--beginning-of-clause-position ()
  "Return beginning of clause position."
  (save-excursion
    (or (py--beginning-of-clause-p)
        (py-backward-clause))))

(defun py--beginning-of-comment-position ()
  "Return beginning of comment position."
  (save-excursion
    (or (py--beginning-of-comment-p)
        (py-backward-comment))))

(defun py--beginning-of-def-position ()
  "Return beginning of def position."
  (save-excursion
    (or (py--beginning-of-def-p)
        (py-backward-def))))

(defun py--beginning-of-def-or-class-position ()
  "Return beginning of def-or-class position."
  (save-excursion
    (or (py--beginning-of-def-or-class-p)
        (py-backward-def-or-class))))

(defun py--beginning-of-expression-position ()
  "Return beginning of expression position."
  (save-excursion
    (or (py--beginning-of-expression-p)
        (py-backward-expression))))

(defun py--beginning-of-except-block-position ()
  "Return beginning of except-block position."
  (save-excursion
    (or (py--beginning-of-except-block-p)
        (py-backward-except-block))))

(defun py--beginning-of-if-block-position ()
  "Return beginning of if-block position."
  (save-excursion
    (or (py--beginning-of-if-block-p)
        (py-backward-if-block))))

(defun py--beginning-of-indent-position ()
  "Return beginning of indent position."
  (save-excursion
    (or (py--beginning-of-indent-p)
        (py-backward-indent))))

(defun py--beginning-of-line-position ()
  "Return beginning of line position."
  (save-excursion
    (or (py--beginning-of-line-p)
        (py-backward-line))))

(defun py--beginning-of-minor-block-position ()
  "Return beginning of minor-block position."
  (save-excursion
    (or (py--beginning-of-minor-block-p)
        (py-backward-minor-block))))

(defun py--beginning-of-partial-expression-position ()
  "Return beginning of partial-expression position."
  (save-excursion
    (or (py--beginning-of-partial-expression-p)
        (py-backward-partial-expression))))

(defun py--beginning-of-paragraph-position ()
  "Return beginning of paragraph position."
  (save-excursion
    (or (py--beginning-of-paragraph-p)
        (py-backward-paragraph))))

(defun py--beginning-of-section-position ()
  "Return beginning of section position."
  (save-excursion
    (or (py--beginning-of-section-p)
        (py-backward-section))))

(defun py--beginning-of-statement-position ()
  "Return beginning of statement position."
  (save-excursion
    (or (py--beginning-of-statement-p)
        (py-backward-statement))))

(defun py--beginning-of-top-level-position ()
  "Return beginning of top-level position."
  (save-excursion
    (or (py--beginning-of-top-level-p)
        (py-backward-top-level))))

(defun py--beginning-of-try-block-position ()
  "Return beginning of try-block position."
  (save-excursion
    (or (py--beginning-of-try-block-p)
        (py-backward-try-block))))

(defun py--beginning-of-block-position-bol ()
  "Return beginning of block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-block-bol-p)
        (py-backward-block-bol))))

(defun py--beginning-of-block-or-clause-position-bol ()
  "Return beginning of block-or-clause position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-block-or-clause-bol-p)
        (py-backward-block-or-clause-bol))))

(defun py--beginning-of-class-position-bol ()
  "Return beginning of class position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-class-bol-p)
        (py-backward-class-bol))))

(defun py--beginning-of-clause-position-bol ()
  "Return beginning of clause position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-clause-bol-p)
        (py-backward-clause-bol))))

(defun py--beginning-of-def-position-bol ()
  "Return beginning of def position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-def-bol-p)
        (py-backward-def-bol))))

(defun py--beginning-of-def-or-class-position-bol ()
  "Return beginning of def-or-class position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-def-or-class-bol-p)
        (py-backward-def-or-class-bol))))

(defun py--beginning-of-elif-block-position-bol ()
  "Return beginning of elif-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-elif-block-bol-p)
        (py-backward-elif-block-bol))))

(defun py--beginning-of-else-block-position-bol ()
  "Return beginning of else-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-else-block-bol-p)
        (py-backward-else-block-bol))))

(defun py--beginning-of-except-block-position-bol ()
  "Return beginning of except-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-except-block-bol-p)
        (py-backward-except-block-bol))))

(defun py--beginning-of-for-block-position-bol ()
  "Return beginning of for-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-for-block-bol-p)
        (py-backward-for-block-bol))))

(defun py--beginning-of-if-block-position-bol ()
  "Return beginning of if-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-if-block-bol-p)
        (py-backward-if-block-bol))))

(defun py--beginning-of-indent-position-bol ()
  "Return beginning of indent position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-indent-bol-p)
        (py-backward-indent-bol))))

(defun py--beginning-of-minor-block-position-bol ()
  "Return beginning of minor-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-minor-block-bol-p)
        (py-backward-minor-block-bol))))

(defun py--beginning-of-statement-position-bol ()
  "Return beginning of statement position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-statement-bol-p)
        (py-backward-statement-bol))))

(defun py--beginning-of-try-block-position-bol ()
  "Return beginning of try-block position at ‘beginning-of-line’."
  (save-excursion
    (or (py--beginning-of-try-block-bol-p)
        (py-backward-try-block-bol))))

(provide 'python-components-beginning-position-forms)
;;; python-components-beginning-position-forms.el ends here
