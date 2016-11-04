;;; python-components-forms-code.el --- Return Python forms' code -*- lexical-binding: t; -*- 

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015-2016 Andreas Röhler

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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-block ()
  "Block at point.

Return code of `py-block' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "block")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-block-or-clause ()
  "Block-Or-Clause at point.

Return code of `py-block-or-clause' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "block-or-clause")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-buffer ()
  "Buffer at point.

Return code of `py-buffer' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "buffer")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-class ()
  "Class at point.

Return code of `py-class' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "class")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-clause ()
  "Clause at point.

Return code of `py-clause' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "clause")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-def ()
  "Def at point.

Return code of `py-def' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "def")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-def-or-class ()
  "Def-Or-Class at point.

Return code of `py-def-or-class' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "def-or-class")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-expression ()
  "Expression at point.

Return code of `py-expression' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "expression")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-indent ()
  "Indent at point.

Return code of `py-indent' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "indent")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-line ()
  "Line at point.

Return code of `py-line' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "line")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-minor-block ()
  "Minor-Block at point.

Return code of `py-minor-block' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "minor-block")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-paragraph ()
  "Paragraph at point.

Return code of `py-paragraph' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "paragraph")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-partial-expression ()
  "Partial-Expression at point.

Return code of `py-partial-expression' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "partial-expression")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-region ()
  "Region at point.

Return code of `py-region' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "region")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-statement ()
  "Statement at point.

Return code of `py-statement' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "statement")))
    (py--forms-report-result erg (called-interactively-p 'any))))

(defun py-top-level ()
  "Top-Level at point.

Return code of `py-top-level' at point, a string. "
  (interactive)
  (let ((erg (py--mark-base "top-level")))
    (py--forms-report-result erg (called-interactively-p 'any))))

;; python-components-forms-code.el ends here
(provide 'python-components-forms-code)
