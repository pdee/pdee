;;; python-components-booleans-beginning-forms.el --- booleans-beginning forms

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


(defun py--beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a `block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-block)
        (py-backward-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a `block-or-clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-block-or-clause)
        (py-backward-block-or-clause)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a `class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-class)
        (py-backward-class)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a `clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-clause)
        (py-backward-clause)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a `def', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-def)
        (py-backward-def)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a `def-or-class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-def-or-class)
        (py-backward-def-or-class)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-elif-block-p ()
  "Returns position, if cursor is at the beginning of a `elif-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-elif-block)
        (py-backward-elif-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-else-block-p ()
  "Returns position, if cursor is at the beginning of a `else-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-else-block)
        (py-backward-else-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-except-block-p ()
  "Returns position, if cursor is at the beginning of a `except-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-except-block)
        (py-backward-except-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a `expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))

        (py-forward-expression)
        (py-backward-expression)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-for-block-p ()
  "Returns position, if cursor is at the beginning of a `for-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-for-block)
        (py-backward-for-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-if-block-p ()
  "Returns position, if cursor is at the beginning of a `if-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-if-block)
        (py-backward-if-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-minor-block-p ()
  "Returns position, if cursor is at the beginning of a `minor-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-minor-block)
        (py-backward-minor-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a `partial-expression', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))

        (py-forward-partial-expression)
        (py-backward-partial-expression)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-section-p ()
  "Returns position, if cursor is at the beginning of a `section', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-section)
        (py-backward-section)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a `statement', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-statement)
        (py-backward-statement)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-top-level-p ()
  "Returns position, if cursor is at the beginning of a `top-level', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-top-level)
        (py-backward-top-level)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-try-block-p ()
  "Returns position, if cursor is at the beginning of a `try-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (or (py-in-string-or-comment-p) (and (eolp) (not (empty-line-p))))
        (py-forward-try-block)
        (py-backward-try-block)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

(defun py--beginning-of-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-block-bol))
      (py-backward-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-clause-bol))
      (py-backward-clause-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-block-or-clause-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `block-or-clause', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-block-or-clause-bol))
      (py-backward-block-or-clause-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `def', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-def-bol))
      (py-backward-def-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-class-bol))
      (py-backward-class-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-def-or-class-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `def-or-class', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-def-or-class-bol))
      (py-backward-def-or-class-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-if-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `if-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-if-block-bol))
      (py-backward-if-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-try-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `try-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-try-block-bol))
      (py-backward-try-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-minor-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `minor-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-minor-block-bol))
      (py-backward-minor-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-for-block-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `for-block', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-for-block-bol))
      (py-backward-for-block-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-top-level-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `top-level', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-top-level-bol))
      (py-backward-top-level-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defun py--beginning-of-statement-bol-p ()
  "Returns position, if cursor is at beginning-of-line and the beginning of a `statement', nil otherwise. "
  (let ((orig (point))
        erg)
    (save-excursion
      (unless (and (eolp) (not (empty-line-p)))
        (py-forward-statement-bol))
      (py-backward-statement-bol)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(provide 'python-components-booleans-beginning-forms)
;; python-components-booleans-beginning-forms.el ends here
