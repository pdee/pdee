;;; python-components-forward-forms.el -- Go to the end of forms -*- lexical-binding: t; -*-

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

(defun py-forward-region ()
  "Go to the end of current region."
  (interactive)
  (let ((end (region-end)))
    (when end (goto-char end))))

(defun py-forward-block (&optional decorator bol)
  "Go to end of block.

Return end of block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-block-bol ()
  "Goto beginning of line following end of block.

Return position reached, if successful, nil otherwise.
See also `py-down-block': down from current definition to next beginning of block below."
  (interactive)
  (let ((erg (py-forward-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-block-or-clause (&optional decorator bol)
  "Go to end of block-or-clause.

Return end of block-or-clause if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-block-or-clause-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-block-or-clause-bol ()
  "Goto beginning of line following end of block-or-clause.

Return position reached, if successful, nil otherwise.
See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below."
  (interactive)
  (let ((erg (py-forward-block-or-clause)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;;;###autoload
(defun py-forward-class (&optional decorator bol)
  "Go to end of class.

Return end of class if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-class-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-class-bol ()
  "Goto beginning of line following end of class.

Return position reached, if successful, nil otherwise.
See also `py-down-class': down from current definition to next beginning of class below."
  (interactive)
  (let ((erg (py-forward-class)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-clause (&optional decorator bol)
  "Go to end of clause.

Return end of clause if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-clause-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-clause-bol ()
  "Goto beginning of line following end of clause.

Return position reached, if successful, nil otherwise.
See also `py-down-clause': down from current definition to next beginning of clause below."
  (interactive)
  (let ((erg (py-forward-clause)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;;;###autoload
(defun py-forward-def-or-class (&optional decorator bol)
  "Go to end of def-or-class.

Return end of def-or-class if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-def-or-class-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-def-or-class-bol ()
  "Goto beginning of line following end of def-or-class.

Return position reached, if successful, nil otherwise.
See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below."
  (interactive)
  (let ((erg (py-forward-def-or-class)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;;;###autoload
(defun py-forward-def (&optional decorator bol)
  "Go to end of def.

Return end of def if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-def-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-def-bol ()
  "Goto beginning of line following end of def.

Return position reached, if successful, nil otherwise.
See also `py-down-def': down from current definition to next beginning of def below."
  (interactive)
  (let ((erg (py-forward-def)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-if-block (&optional decorator bol)
  "Go to end of if-block.

Return end of if-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-if-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-if-block-bol ()
  "Goto beginning of line following end of if-block.

Return position reached, if successful, nil otherwise.
See also `py-down-if-block': down from current definition to next beginning of if-block below."
  (interactive)
  (let ((erg (py-forward-if-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-elif-block (&optional decorator bol)
  "Go to end of elif-block.

Return end of elif-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-elif-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-elif-block-bol ()
  "Goto beginning of line following end of elif-block.

Return position reached, if successful, nil otherwise.
See also `py-down-elif-block': down from current definition to next beginning of elif-block below."
  (interactive)
  (let ((erg (py-forward-elif-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-else-block (&optional decorator bol)
  "Go to end of else-block.

Return end of else-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-else-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-else-block-bol ()
  "Goto beginning of line following end of else-block.

Return position reached, if successful, nil otherwise.
See also `py-down-else-block': down from current definition to next beginning of else-block below."
  (interactive)
  (let ((erg (py-forward-else-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-for-block (&optional decorator bol)
  "Go to end of for-block.

Return end of for-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-for-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-for-block-bol ()
  "Goto beginning of line following end of for-block.

Return position reached, if successful, nil otherwise.
See also `py-down-for-block': down from current definition to next beginning of for-block below."
  (interactive)
  (let ((erg (py-forward-for-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-except-block (&optional decorator bol)
  "Go to end of except-block.

Return end of except-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-except-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-except-block-bol ()
  "Goto beginning of line following end of except-block.

Return position reached, if successful, nil otherwise.
See also `py-down-except-block': down from current definition to next beginning of except-block below."
  (interactive)
  (let ((erg (py-forward-except-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-try-block (&optional decorator bol)
  "Go to end of try-block.

Return end of try-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-try-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-try-block-bol ()
  "Goto beginning of line following end of try-block.

Return position reached, if successful, nil otherwise.
See also `py-down-try-block': down from current definition to next beginning of try-block below."
  (interactive)
  (let ((erg (py-forward-try-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-minor-block (&optional decorator bol)
  "Go to end of minor-block.

Return end of minor-block if successful, nil otherwise
Optional arg DECORATOR is used if form supports one
With optional BOL, go to beginning of line following match."
  (interactive)
  (let* ((orig (point))
         (erg (py--end-base 'py-minor-block-re orig decorator bol)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-minor-block-bol ()
  "Goto beginning of line following end of minor-block.

Return position reached, if successful, nil otherwise.
See also `py-down-minor-block': down from current definition to next beginning of minor-block below."
  (interactive)
  (let ((erg (py-forward-minor-block)))
    (setq erg (py--beginning-of-line-form erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;; python-components-forward-forms.el ends here
(provide 'python-components-forward-forms)
