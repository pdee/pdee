;;; python-components-beginning-forms.el --- Forms start described by a regular-expression 

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

;;; Code:


(defun py-beginning-of-block (&optional indent)
 "Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-clause (&optional indent)
 "Go to beginning clause, skip whitespace at BOL.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-block-or-clause (&optional indent)
 "Go to beginning block-or-clause, skip whitespace at BOL.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def (&optional indent)
 "Go to beginning def, skip whitespace at BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-class (&optional indent)
 "Go to beginning class, skip whitespace at BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-def-or-class (&optional indent)
 "Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p)))

(defun py-beginning-of-if-block (&optional indent)
 "Go to beginning if-block, skip whitespace at BOL.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-try-block (&optional indent)
 "Go to beginning try-block, skip whitespace at BOL.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p)))

(defun py-beginning-of-minor-block (&optional indent)
 "Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p)))

(defalias 'py-beginning-of-block-lc 'py-beginning-of-block-bol)
(defun py-beginning-of-block-bol (&optional indent)
 "Go to beginning block, go to BOL.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-clause-lc 'py-beginning-of-clause-bol)
(defun py-beginning-of-clause-bol (&optional indent)
 "Go to beginning clause, go to BOL.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-block-or-clause-lc 'py-beginning-of-block-or-clause-bol)
(defun py-beginning-of-block-or-clause-bol (&optional indent)
 "Go to beginning block-or-clause, go to BOL.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-extended-block-or-clause-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-lc 'py-beginning-of-def-bol)
(defun py-beginning-of-def-bol (&optional indent)
 "Go to beginning def, go to BOL.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-class-lc 'py-beginning-of-class-bol)
(defun py-beginning-of-class-bol (&optional indent)
 "Go to beginning class, go to BOL.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-def-or-class-lc 'py-beginning-of-def-or-class-bol)
(defun py-beginning-of-def-or-class-bol (&optional indent)
 "Go to beginning def-or-class, go to BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-def-or-class-re 'py-extended-block-or-clause-re (interactive-p) t))

(defalias 'py-beginning-of-if-block-lc 'py-beginning-of-if-block-bol)
(defun py-beginning-of-if-block-bol (&optional indent)
 "Go to beginning if-block, go to BOL.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-if-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-try-block-lc 'py-beginning-of-try-block-bol)
(defun py-beginning-of-try-block-bol (&optional indent)
 "Go to beginning try-block, go to BOL.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-try-block-re 'py-clause-re (interactive-p) t))

(defalias 'py-beginning-of-minor-block-lc 'py-beginning-of-minor-block-bol)
(defun py-beginning-of-minor-block-bol (&optional indent)
 "Go to beginning minor-block, go to BOL.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-prepare indent 'py-minor-block-re 'py-clause-re (interactive-p) t))

(provide 'python-components-beginning-forms)
;; python-components-beginning-forms.el ends here
