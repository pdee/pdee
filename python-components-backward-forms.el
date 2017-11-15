;;; python-components-backward-forms.el --- Go to beginning of form or further backward -*- lexical-binding: t; -*-

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

(defun py-backward-region ()
  "Go to the beginning of current region."
  (interactive)
  (let ((beg (region-beginning)))
    (when beg (goto-char beg))))

(defun py-backward-block (&optional indent)
  "Go to beginning of ‘block’ according to INDENT.

If already at beginning, go one ‘block’ backward.
Return beginning of ‘block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-block-re 'py-block-re (called-interactively-p 'any)))

(defun py-backward-block-or-clause (&optional indent)
  "Go to beginning of ‘block-or-clause’ according to INDENT.

If already at beginning, go one ‘block-or-clause’ backward.
Return beginning of ‘block-or-clause’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (called-interactively-p 'any)))

;;;###autoload
(defun py-backward-class (&optional indent decorator bol)
  "Go to beginning of ‘class’ according to INDENT.

If already at beginning, go one ‘class’ backward.
Optional DECORATOR BOL

Return beginning of ‘class’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-class-re 'py-class-re (called-interactively-p 'any) decorator bol))

(defun py-backward-clause (&optional indent)
  "Go to beginning of ‘clause’ according to INDENT.

If already at beginning, go one ‘clause’ backward.
Return beginning of ‘clause’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (called-interactively-p 'any)))

;;;###autoload
(defun py-backward-def (&optional indent decorator bol)
  "Go to beginning of ‘def’ according to INDENT.

If already at beginning, go one ‘def’ backward.
Optional DECORATOR BOL

Return beginning of ‘def’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-def-re 'py-def-re (called-interactively-p 'any) decorator bol))

;;;###autoload
(defun py-backward-def-or-class (&optional indent decorator bol)
  "Go to beginning of ‘def-or-class’ according to INDENT.

If already at beginning, go one ‘def-or-class’ backward.
Optional DECORATOR BOL

Return beginning of ‘def-or-class’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-def-or-class-re 'py-def-or-class-re (called-interactively-p 'any) decorator bol))

(defun py-backward-elif-block (&optional indent)
  "Go to beginning of ‘elif-block’ according to INDENT.

If already at beginning, go one ‘elif-block’ backward.
Return beginning of ‘elif-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-elif-block-re 'py-elif-block-re (called-interactively-p 'any)))

(defun py-backward-else-block (&optional indent)
  "Go to beginning of ‘else-block’ according to INDENT.

If already at beginning, go one ‘else-block’ backward.
Return beginning of ‘else-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-else-block-re 'py-else-block-re (called-interactively-p 'any)))

(defun py-backward-except-block (&optional indent)
  "Go to beginning of ‘except-block’ according to INDENT.

If already at beginning, go one ‘except-block’ backward.
Return beginning of ‘except-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-except-block-re 'py-except-block-re (called-interactively-p 'any)))

(defun py-backward-for-block (&optional indent)
  "Go to beginning of ‘for-block’ according to INDENT.

If already at beginning, go one ‘for-block’ backward.
Return beginning of ‘for-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-for-block-re 'py-for-block-re (called-interactively-p 'any)))

(defun py-backward-if-block (&optional indent)
  "Go to beginning of ‘if-block’ according to INDENT.

If already at beginning, go one ‘if-block’ backward.
Return beginning of ‘if-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-if-block-re 'py-if-block-re (called-interactively-p 'any)))

(defun py-backward-minor-block (&optional indent)
  "Go to beginning of ‘minor-block’ according to INDENT.

If already at beginning, go one ‘minor-block’ backward.
Return beginning of ‘minor-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-minor-block-re 'py-minor-block-re (called-interactively-p 'any)))

(defun py-backward-try-block (&optional indent)
  "Go to beginning of ‘try-block’ according to INDENT.

If already at beginning, go one ‘try-block’ backward.
Return beginning of ‘try-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-try-block-re 'py-try-block-re (called-interactively-p 'any)))

(defun py-backward-block-bol (&optional indent)
  "Go to beginning of ‘block’ according to INDENT, go to BOL.
If already at beginning, go one ‘block’ backward.
Return beginning of ‘block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-block-or-clause-bol (&optional indent)
  "Go to beginning of ‘block-or-clause’ according to INDENT, go to BOL.
If already at beginning, go one ‘block-or-clause’ backward.
Return beginning of ‘block-or-clause’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (called-interactively-p 'any) nil t))

;;;###autoload
(defun py-backward-class-bol (&optional indent decorator)
  "Go to beginning of ‘class’ according to INDENT, go to BOL.
Optional DECORATOR BOL

If already at beginning, go one ‘class’ backward.
Return beginning of ‘class’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-class-re 'py-extended-block-or-clause-re (called-interactively-p 'any) decorator t))

(defun py-backward-clause-bol (&optional indent)
  "Go to beginning of ‘clause’ according to INDENT, go to BOL.
If already at beginning, go one ‘clause’ backward.
Return beginning of ‘clause’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (called-interactively-p 'any) nil t))

;;;###autoload
(defun py-backward-def-bol (&optional indent decorator)
  "Go to beginning of ‘def’ according to INDENT, go to BOL.
Optional DECORATOR BOL

If already at beginning, go one ‘def’ backward.
Return beginning of ‘def’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-def-re 'py-extended-block-or-clause-re (called-interactively-p 'any) decorator t))

;;;###autoload
(defun py-backward-def-or-class-bol (&optional indent decorator)
  "Go to beginning of ‘def-or-class’ according to INDENT, go to BOL.
Optional DECORATOR BOL

If already at beginning, go one ‘def-or-class’ backward.
Return beginning of ‘def-or-class’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (called-interactively-p 'any) decorator t))

(defun py-backward-elif-block-bol (&optional indent)
  "Go to beginning of ‘elif-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘elif-block’ backward.
Return beginning of ‘elif-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-elif-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-else-block-bol (&optional indent)
  "Go to beginning of ‘else-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘else-block’ backward.
Return beginning of ‘else-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-else-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-except-block-bol (&optional indent)
  "Go to beginning of ‘except-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘except-block’ backward.
Return beginning of ‘except-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-except-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-for-block-bol (&optional indent)
  "Go to beginning of ‘for-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘for-block’ backward.
Return beginning of ‘for-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-for-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-if-block-bol (&optional indent)
  "Go to beginning of ‘if-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘if-block’ backward.
Return beginning of ‘if-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-if-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-minor-block-bol (&optional indent)
  "Go to beginning of ‘minor-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘minor-block’ backward.
Return beginning of ‘minor-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-minor-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(defun py-backward-try-block-bol (&optional indent)
  "Go to beginning of ‘try-block’ according to INDENT, go to BOL.
If already at beginning, go one ‘try-block’ backward.
Return beginning of ‘try-block’ if successful, nil otherwise"
  (interactive)
  (py--backward-prepare indent 'py-try-block-re 'py-clause-re (called-interactively-p 'any) nil t))

(provide 'python-components-backward-forms)
;;; python-components-backward-forms.el ends here
