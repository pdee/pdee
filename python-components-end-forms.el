;;; python-components-end-forms.el -- Go to the end of forms

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


(defalias 'py-down-block 'py-end-of-block)
(defun py-end-of-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-block-lc 'py-end-of-block-bol)
(defun py-end-of-block-bol ()
  "Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-end-of-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-clause 'py-end-of-clause)
(defun py-end-of-clause (&optional indent)
  "Go to end of clause.

Returns end of clause if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-clause-lc 'py-end-of-clause-bol)
(defun py-end-of-clause-bol ()
  "Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-end-of-clause)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-block-or-clause 'py-end-of-block-or-clause)
(defun py-end-of-block-or-clause (&optional indent)
  "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-block-or-clause-lc 'py-end-of-block-or-clause-bol)
(defun py-end-of-block-or-clause-bol ()
  "Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. "
  (interactive)
  (let ((erg (py-end-of-block-or-clause)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-def 'py-end-of-def)
(defun py-end-of-def (&optional indent)
  "Go to end of def.

Returns end of def if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-def-lc 'py-end-of-def-bol)
(defun py-end-of-def-bol ()
  "Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-end-of-def)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-class 'py-end-of-class)
(defun py-end-of-class (&optional indent)
  "Go to end of class.

Returns end of class if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-class-lc 'py-end-of-class-bol)
(defun py-end-of-class-bol ()
  "Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-end-of-class)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-def-or-class 'py-end-of-def-or-class)
(defun py-end-of-def-or-class (&optional indent)
  "Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-or-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-def-or-class-lc 'py-end-of-def-or-class-bol)
(defun py-end-of-def-or-class-bol ()
  "Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. "
  (interactive)
  (let ((erg (py-end-of-def-or-class)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-if-block 'py-end-of-if-block)
(defun py-end-of-if-block (&optional indent)
  "Go to end of if-block.

Returns end of if-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-if-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-if-block-lc 'py-end-of-if-block-bol)
(defun py-end-of-if-block-bol ()
  "Goto beginning of line following end of if-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-if-block': down from current definition to next beginning of if-block below. "
  (interactive)
  (let ((erg (py-end-of-if-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-elif-block 'py-end-of-elif-block)
(defun py-end-of-elif-block (&optional indent)
  "Go to end of elif-block.

Returns end of elif-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-elif-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-elif-block-lc 'py-end-of-elif-block-bol)
(defun py-end-of-elif-block-bol ()
  "Goto beginning of line following end of elif-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-elif-block': down from current definition to next beginning of elif-block below. "
  (interactive)
  (let ((erg (py-end-of-elif-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-else-block 'py-end-of-else-block)
(defun py-end-of-else-block (&optional indent)
  "Go to end of else-block.

Returns end of else-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-else-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-else-block-lc 'py-end-of-else-block-bol)
(defun py-end-of-else-block-bol ()
  "Goto beginning of line following end of else-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-else-block': down from current definition to next beginning of else-block below. "
  (interactive)
  (let ((erg (py-end-of-else-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-try-block 'py-end-of-try-block)
(defun py-end-of-try-block (&optional indent)
  "Go to end of try-block.

Returns end of try-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-try-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-try-block-lc 'py-end-of-try-block-bol)
(defun py-end-of-try-block-bol ()
  "Goto beginning of line following end of try-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-try-block': down from current definition to next beginning of try-block below. "
  (interactive)
  (let ((erg (py-end-of-try-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-minor-block 'py-end-of-minor-block)
(defun py-end-of-minor-block (&optional indent)
  "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-minor-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-minor-block-lc 'py-end-of-minor-block-bol)
(defun py-end-of-minor-block-bol ()
  "Goto beginning of line following end of minor-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-minor-block': down from current definition to next beginning of minor-block below. "
  (interactive)
  (let ((erg (py-end-of-minor-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-for-block 'py-end-of-for-block)
(defun py-end-of-for-block (&optional indent)
  "Go to end of for-block.

Returns end of for-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-for-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-for-block-lc 'py-end-of-for-block-bol)
(defun py-end-of-for-block-bol ()
  "Goto beginning of line following end of for-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-for-block': down from current definition to next beginning of for-block below. "
  (interactive)
  (let ((erg (py-end-of-for-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-except-block 'py-end-of-except-block)
(defun py-end-of-except-block (&optional indent)
  "Go to end of except-block.

Returns end of except-block if successful, nil otherwise"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-except-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-except-block-lc 'py-end-of-except-block-bol)
(defun py-end-of-except-block-bol ()
  "Goto beginning of line following end of except-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-except-block': down from current definition to next beginning of except-block below. "
  (interactive)
  (let ((erg (py-end-of-except-block)))
    (setq erg (py--beginning-of-line-form))
    (when (interactive-p) (message "%s" erg))
    erg))

;; python-components-end-forms.el ends here
(provide 'python-components-end-forms)
