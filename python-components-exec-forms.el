;;; python-components-exec-forms.el --- Execute forms at point

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

;;; Execute forms at point
(defun py-execute-statement ()
  "Send statement at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "statement"))

(defun py-execute-block ()
  "Send block at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "block"))

(defun py-execute-block-or-clause ()
  "Send block-or-clause at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "block-or-clause"))

(defun py-execute-def ()
  "Send def at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "def"))

(defun py-execute-class ()
  "Send class at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "class"))

(defun py-execute-def-or-class ()
  "Send def-or-class at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "def-or-class"))

(defun py-execute-expression ()
  "Send expression at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "expression"))

(defun py-execute-partial-expression ()
  "Send partial-expression at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "partial-expression"))

(defun py-execute-top-level ()
  "Send top-level at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "top-level"))

(defun py-execute-clause ()
  "Send clause at point to a Python interpreter. "
  (interactive)
  (py--execute-prepare "clause"))

(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here
