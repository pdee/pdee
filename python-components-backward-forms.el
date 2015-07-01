;;; python-components-backward-forms.el --- Go to beginning of form or further backward 

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


(defun py-backward-block (&optional indent)
 "Go to beginning block.

If already at beginning, go one block backward.
Returns beginning of block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-block-re 'py-clause-re (interactive-p)))

(defun py-backward-block-or-clause (&optional indent)
 "Go to beginning block-or-clause.

If already at beginning, go one block-or-clause backward.
Returns beginning of block-or-clause if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-backward-class (&optional indent)
 "Go to beginning class.

If already at beginning, go one class backward.
Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-backward-clause (&optional indent)
 "Go to beginning clause.

If already at beginning, go one clause backward.
Returns beginning of clause if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-backward-def (&optional indent)
 "Go to beginning def.

If already at beginning, go one def backward.
Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-backward-def-or-class (&optional indent)
 "Go to beginning def-or-class.

If already at beginning, go one def-or-class backward.
Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-backward-if-block (&optional indent)
 "Go to beginning if-block.

If already at beginning, go one if-block backward.
Returns beginning of if-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-if-block-re 'py-clause-re (interactive-p)))

(defun py-backward-elif-block (&optional indent)
 "Go to beginning elif-block.

If already at beginning, go one elif-block backward.
Returns beginning of elif-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-elif-block-re 'py-clause-re (interactive-p)))

(defun py-backward-else-block (&optional indent)
 "Go to beginning else-block.

If already at beginning, go one else-block backward.
Returns beginning of else-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-else-block-re 'py-clause-re (interactive-p)))

(defun py-backward-for-block (&optional indent)
 "Go to beginning for-block.

If already at beginning, go one for-block backward.
Returns beginning of for-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-for-block-re 'py-clause-re (interactive-p)))

(defun py-backward-except-block (&optional indent)
 "Go to beginning except-block.

If already at beginning, go one except-block backward.
Returns beginning of except-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-except-block-re 'py-clause-re (interactive-p)))

(defun py-backward-try-block (&optional indent)
 "Go to beginning try-block.

If already at beginning, go one try-block backward.
Returns beginning of try-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-try-block-re 'py-clause-re (interactive-p)))

(defun py-backward-line (&optional indent)
 "Go to beginning line.

If already at beginning, go one line backward.
Returns beginning of line if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-line-re 'py-clause-re (interactive-p)))

(defun py-backward-minor-block (&optional indent)
 "Go to beginning minor-block.

If already at beginning, go one minor-block backward.
Returns beginning of minor-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p)))

(defalias 'py-backward-block-lc 'py-backward-block-bol)
(defun py-backward-block-bol (&optional indent)
 "Go to beginning block, go to BOL.

If already at beginning, go one block backward.
Returns beginning of block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-block-or-clause-lc 'py-backward-block-or-clause-bol)
(defun py-backward-block-or-clause-bol (&optional indent)
 "Go to beginning block-or-clause, go to BOL.

If already at beginning, go one block-or-clause backward.
Returns beginning of block-or-clause if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-backward-class-lc 'py-backward-class-bol)
(defun py-backward-class-bol (&optional indent)
 "Go to beginning class, go to BOL.

If already at beginning, go one class backward.
Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-backward-clause-lc 'py-backward-clause-bol)
(defun py-backward-clause-bol (&optional indent)
 "Go to beginning clause, go to BOL.

If already at beginning, go one clause backward.
Returns beginning of clause if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-backward-def-lc 'py-backward-def-bol)
(defun py-backward-def-bol (&optional indent)
 "Go to beginning def, go to BOL.

If already at beginning, go one def backward.
Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-backward-def-or-class-lc 'py-backward-def-or-class-bol)
(defun py-backward-def-or-class-bol (&optional indent)
 "Go to beginning def-or-class, go to BOL.

If already at beginning, go one def-or-class backward.
Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "
  (interactive)
  (py--backward-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-backward-elif-block-lc 'py-backward-elif-block-bol)
(defun py-backward-elif-block-bol (&optional indent)
 "Go to beginning elif-block, go to BOL.

If already at beginning, go one elif-block backward.
Returns beginning of elif-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-elif-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-else-block-lc 'py-backward-else-block-bol)
(defun py-backward-else-block-bol (&optional indent)
 "Go to beginning else-block, go to BOL.

If already at beginning, go one else-block backward.
Returns beginning of else-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-else-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-except-block-lc 'py-backward-except-block-bol)
(defun py-backward-except-block-bol (&optional indent)
 "Go to beginning except-block, go to BOL.

If already at beginning, go one except-block backward.
Returns beginning of except-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-except-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-except-block-lc 'py-backward-except-block-bol)
(defun py-backward-except-block-bol (&optional indent)
 "Go to beginning except-block, go to BOL.

If already at beginning, go one except-block backward.
Returns beginning of except-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-except-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-for-block-lc 'py-backward-for-block-bol)
(defun py-backward-for-block-bol (&optional indent)
 "Go to beginning for-block, go to BOL.

If already at beginning, go one for-block backward.
Returns beginning of for-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-for-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-if-block-lc 'py-backward-if-block-bol)
(defun py-backward-if-block-bol (&optional indent)
 "Go to beginning if-block, go to BOL.

If already at beginning, go one if-block backward.
Returns beginning of if-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-if-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-minor-block-lc 'py-backward-minor-block-bol)
(defun py-backward-minor-block-bol (&optional indent)
 "Go to beginning minor-block, go to BOL.

If already at beginning, go one minor-block backward.
Returns beginning of minor-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-statement-lc 'py-backward-statement-bol)
(defun py-backward-statement-bol (&optional indent)
 "Go to beginning statement, go to BOL.

If already at beginning, go one statement backward.
Returns beginning of statement if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-statement-re 'py-clause-re (interactive-p) t))

(defalias 'py-backward-try-block-lc 'py-backward-try-block-bol)
(defun py-backward-try-block-bol (&optional indent)
 "Go to beginning try-block, go to BOL.

If already at beginning, go one try-block backward.
Returns beginning of try-block if successful, nil otherwise

"
  (interactive)
  (py--backward-prepare indent 'py-try-block-re 'py-clause-re (interactive-p) t))

(provide 'python-components-backward-forms)
;;; python-components-backward-forms.el ends here
