;;; python-components-booleans-end-forms.el --- booleans-end forms

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


(defun py--end-of-block-p ()
  "Returns position, if cursor is at the end of a block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-block)
      (py-end-of-block)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-clause-p ()
  "Returns position, if cursor is at the end of a clause, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-clause)
      (py-end-of-clause)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-block-or-clause-p ()
  "Returns position, if cursor is at the end of a block-or-clause, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-block-or-clause)
      (py-end-of-block-or-clause)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-def-p ()
  "Returns position, if cursor is at the end of a def, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-def)
      (py-end-of-def)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-class-p ()
  "Returns position, if cursor is at the end of a class, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-class)
      (py-end-of-class)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-def-or-class-p ()
  "Returns position, if cursor is at the end of a def-or-class, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-def-or-class)
      (py-end-of-def-or-class)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-if-block-p ()
  "Returns position, if cursor is at the end of a if-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-if-block)
      (py-end-of-if-block)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-try-block-p ()
  "Returns position, if cursor is at the end of a try-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-try-block)
      (py-end-of-try-block)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-minor-block-p ()
  "Returns position, if cursor is at the end of a minor-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-minor-block)
      (py-end-of-minor-block)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-for-block-p ()
  "Returns position, if cursor is at the end of a for-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-for-block)
      (py-end-of-for-block)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-top-level-p ()
  "Returns position, if cursor is at the end of a top-level, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-top-level)
      (py-end-of-top-level)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-statement-p ()
  "Returns position, if cursor is at the end of a statement, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-statement)
      (py-end-of-statement)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-expression-p ()
  "Returns position, if cursor is at the end of a expression, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-expression)
      (py-end-of-expression)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-partial-expression-p ()
  "Returns position, if cursor is at the end of a partial-expression, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-partial-expression)
      (py-end-of-partial-expression)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-block)
      (py-end-of-block-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a clause, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-clause)
      (py-end-of-clause-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-block-or-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a block-or-clause, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-block-or-clause)
      (py-end-of-block-or-clause-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-def-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a def, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-def)
      (py-end-of-def-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a class, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-class)
      (py-end-of-class-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-def-or-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a def-or-class, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-def-or-class)
      (py-end-of-def-or-class-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-if-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a if-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-if-block)
      (py-end-of-if-block-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-try-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a try-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-try-block)
      (py-end-of-try-block-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-minor-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a minor-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-minor-block)
      (py-end-of-minor-block-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-for-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a for-block, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-for-block)
      (py-end-of-for-block-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-top-level-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a top-level, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-top-level)
      (py-end-of-top-level-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(defun py--end-of-statement-bol-p ()
  "Returns position, if cursor is at beginning-of-line at the end of a statement, nil otherwise. "
  (let ((orig (point))
	erg)
    (save-excursion
      (py-beginning-of-statement)
      (py-end-of-statement-bol)
      (when (eq orig (point))
	(setq erg orig))
      erg)))

(provide 'python-components-booleans-end-forms)
;; python-components-booleans-end-forms.el ends here