;;; python-components-forward-forms.el -- Go to the end of forms -*- lexical-binding: t; -*-

;; This file is generated. Edits here might not be persistent.


;; URL: https://gitlab.com/groups/python-mode-devs
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

;; 

;;; Code:


(defun py-forward-assignment (&optional orig bol)
  "Go to end of assignment.

Return end of ‘assignment’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (py--end-base (quote py-assignment-re) orig bol))

(defun py-forward-assignment-bol ()
  "Goto beginning of line following end of ‘assignment’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-assignment’."
  (interactive)
  (py-forward-assignment nil t))

(defun py-forward-region ()
  "Go to the end of current region."
  (interactive)
  (let ((end (region-end)))
    (when end (goto-char end))))

(defun py-forward-block (&optional orig bol)
  "Go to end of block.

Return end of ‘block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-block-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-block-re) orig bol)))
    erg))

(defun py-forward-block-bol ()
  "Goto beginning of line following end of ‘block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-block’."
  (interactive)
  (py-forward-block nil t))

(defun py-forward-block-or-clause (&optional orig bol)
  "Go to end of block-or-clause.

Return end of ‘block-or-clause’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-block-or-clause-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-block-or-clause-re) orig bol)))
    erg))

(defun py-forward-block-or-clause-bol ()
  "Goto beginning of line following end of ‘block-or-clause’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-block-or-clause’."
  (interactive)
  (py-forward-block-or-clause nil t))

(defun py-forward-class (&optional orig bol)
  "Go to end of class.

Return end of ‘class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-class-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-class-re) orig bol)))
    erg))

(defun py-forward-class-bol ()
  "Goto beginning of line following end of ‘class’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-class’."
  (interactive)
  (py-forward-class nil t))

(defun py-forward-clause (&optional orig bol)
  "Go to end of clause.

Return end of ‘clause’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-clause-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-clause-re) orig bol)))
    erg))

(defun py-forward-clause-bol ()
  "Goto beginning of line following end of ‘clause’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-clause’."
  (interactive)
  (py-forward-clause nil t))

(defun py-forward-def (&optional orig bol)
  "Go to end of def.

Return end of ‘def’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-def-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-def-re) orig bol)))
    erg))

(defun py-forward-def-bol ()
  "Goto beginning of line following end of ‘def’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-def’."
  (interactive)
  (py-forward-def nil t))

(defun py-forward-def-or-class (&optional orig bol)
  "Go to end of def-or-class.

Return end of ‘def-or-class’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-def-or-class-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-def-or-class-re) orig bol)))
    erg))

(defun py-forward-def-or-class-bol ()
  "Goto beginning of line following end of ‘def-or-class’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-def-or-class’."
  (interactive)
  (py-forward-def-or-class nil t))

(defun py-forward-elif-block (&optional orig bol)
  "Go to end of elif-block.

Return end of ‘elif-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-elif-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-elif-re) orig bol)))
    erg))

(defun py-forward-elif-block-bol ()
  "Goto beginning of line following end of ‘elif-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-elif-block’."
  (interactive)
  (py-forward-elif-block nil t))

(defun py-forward-else-block (&optional orig bol)
  "Go to end of else-block.

Return end of ‘else-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-else-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-else-re) orig bol)))
    erg))

(defun py-forward-else-block-bol ()
  "Goto beginning of line following end of ‘else-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-else-block’."
  (interactive)
  (py-forward-else-block nil t))

(defun py-forward-except-block (&optional orig bol)
  "Go to end of except-block.

Return end of ‘except-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-except-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-except-re) orig bol)))
    erg))

(defun py-forward-except-block-bol ()
  "Goto beginning of line following end of ‘except-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-except-block’."
  (interactive)
  (py-forward-except-block nil t))

(defun py-forward-for-block (&optional orig bol)
  "Go to end of for-block.

Return end of ‘for-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-for-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-for-re) orig bol)))
    erg))

(defun py-forward-for-block-bol ()
  "Goto beginning of line following end of ‘for-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-for-block’."
  (interactive)
  (py-forward-for-block nil t))

(defun py-forward-if-block (&optional orig bol)
  "Go to end of if-block.

Return end of ‘if-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-if-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-if-re) orig bol)))
    erg))

(defun py-forward-if-block-bol ()
  "Goto beginning of line following end of ‘if-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-if-block’."
  (interactive)
  (py-forward-if-block nil t))

(defun py-forward-minor-block (&optional orig bol)
  "Go to end of minor-block.

Return end of ‘minor-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-minor-block-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-minor-block-re) orig bol)))
    erg))

(defun py-forward-minor-block-bol ()
  "Goto beginning of line following end of ‘minor-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-minor-block’."
  (interactive)
  (py-forward-minor-block nil t))

(defun py-forward-try-block (&optional orig bol)
  "Go to end of try-block.

Return end of ‘try-block’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (let (erg)
    (unless (setq erg (py--end-base (quote py-try-re) orig bol))
      (skip-chars-forward " \t\r\n\f")
      (setq erg (py--end-base (quote py-try-re) orig bol)))
    erg))

(defun py-forward-try-block-bol ()
  "Goto beginning of line following end of ‘try-block’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-try-block’."
  (interactive)
  (py-forward-try-block nil t))

(provide 'python-components-forward-forms)
;;; python-components-forward-forms.el ends here
