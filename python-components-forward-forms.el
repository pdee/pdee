;;; python-components-forward-forms.el -- Go to the end of forms

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015  Andreas Röhler

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


(defun py-forward-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-block-bol (&optional indent)
  "Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-forward-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-block-or-clause (&optional indent)
  "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-or-clause-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-block-or-clause-bol (&optional indent)
  "Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. "
  (interactive)
  (let ((erg (py-forward-block-or-clause indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-class (&optional indent)
  "Go to end of class.

Returns end of class if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-class-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-class-bol (&optional indent)
  "Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-forward-class indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-clause (&optional indent)
  "Go to end of clause.

Returns end of clause if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-clause-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-clause-bol (&optional indent)
  "Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-forward-clause indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-def (&optional indent)
  "Go to end of def.

Returns end of def if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-def-bol (&optional indent)
  "Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-forward-def indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-def-or-class (&optional indent)
  "Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-or-class-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-def-or-class-bol (&optional indent)
  "Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. "
  (interactive)
  (let ((erg (py-forward-def-or-class indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-if-block (&optional indent)
  "Go to end of if-block.

Returns end of if-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-if-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-if-block-bol (&optional indent)
  "Goto beginning of line following end of if-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-if-block': down from current definition to next beginning of if-block below. "
  (interactive)
  (let ((erg (py-forward-if-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-elif-block (&optional indent)
  "Go to end of elif-block.

Returns end of elif-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-elif-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-elif-block-bol (&optional indent)
  "Goto beginning of line following end of elif-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-elif-block': down from current definition to next beginning of elif-block below. "
  (interactive)
  (let ((erg (py-forward-elif-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-else-block (&optional indent)
  "Go to end of else-block.

Returns end of else-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-else-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-else-block-bol (&optional indent)
  "Goto beginning of line following end of else-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-else-block': down from current definition to next beginning of else-block below. "
  (interactive)
  (let ((erg (py-forward-else-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-for-block (&optional indent)
  "Go to end of for-block.

Returns end of for-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-for-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-for-block-bol (&optional indent)
  "Goto beginning of line following end of for-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-for-block': down from current definition to next beginning of for-block below. "
  (interactive)
  (let ((erg (py-forward-for-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-except-block (&optional indent)
  "Go to end of except-block.

Returns end of except-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-except-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-except-block-bol (&optional indent)
  "Goto beginning of line following end of except-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-except-block': down from current definition to next beginning of except-block below. "
  (interactive)
  (let ((erg (py-forward-except-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-try-block (&optional indent)
  "Go to end of try-block.

Returns end of try-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-try-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-try-block-bol (&optional indent)
  "Goto beginning of line following end of try-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-try-block': down from current definition to next beginning of try-block below. "
  (interactive)
  (let ((erg (py-forward-try-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-minor-block (&optional indent)
  "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-minor-block-re orig)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-minor-block-bol (&optional indent)
  "Goto beginning of line following end of minor-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-minor-block': down from current definition to next beginning of minor-block below. "
  (interactive)
  (let ((erg (py-forward-minor-block indent)))
    (setq erg (py--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;; python-components-forward-forms.el ends here
(provide 'python-components-forward-forms)
