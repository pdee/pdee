;;; python-components-booleans-beginning-forms.el --- booleans-beginning forms

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


(defun py--beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a `block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-block))
      (py-beginning-of-block)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a `clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-clause))
      (py-beginning-of-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a `block-or-clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-block-or-clause))
      (py-beginning-of-block-or-clause)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a `def', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-def))
      (py-beginning-of-def)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a `class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-class))
      (py-beginning-of-class)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a `def-or-class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-def-or-class))
      (py-beginning-of-def-or-class)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-if-block-p ()
  "Returns position, if cursor is at the beginning of a `if-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-if-block))
      (py-beginning-of-if-block)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-try-block-p ()
  "Returns position, if cursor is at the beginning of a `try-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-try-block))
      (py-beginning-of-try-block)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-minor-block-p ()
  "Returns position, if cursor is at the beginning of a `minor-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-minor-block))
      (py-beginning-of-minor-block)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-for-block-p ()
  "Returns position, if cursor is at the beginning of a `for-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-for-block))
      (py-beginning-of-for-block)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-top-level-p ()
  "Returns position, if cursor is at the beginning of a `top-level', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-top-level))
      (py-beginning-of-top-level)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a `statement', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-statement))
      (py-beginning-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a `expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-expression))
      (py-beginning-of-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a `partial-expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-partial-expression))
      (py-beginning-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-block-bol))
      (py-beginning-of-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-clause-bol))
      (py-beginning-of-clause-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-block-or-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `block-or-clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-block-or-clause-bol))
      (py-beginning-of-block-or-clause-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `def', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-def-bol))
      (py-beginning-of-def-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-class-bol))
      (py-beginning-of-class-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-or-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `def-or-class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-def-or-class-bol))
      (py-beginning-of-def-or-class-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-if-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `if-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-if-block-bol))
      (py-beginning-of-if-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-try-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `try-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-try-block-bol))
      (py-beginning-of-try-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-minor-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `minor-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-minor-block-bol))
      (py-beginning-of-minor-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-for-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `for-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-for-block-bol))
      (py-beginning-of-for-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-statement-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `statement', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-end-of-statement-bol))
      (py-beginning-of-statement-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(provide 'python-components-booleans-beginning-forms)
;; python-components-booleans-beginning-forms.el ends here