;;; python-components-close-forms.el --- close forms -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.


;; URL: https://gitlab.com/python-mode-devs
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-close-block ()
  "Close block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-block-re))

(defun py-close-class ()
  "Close class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-class-re))

(defun py-close-clause ()
  "Close clause at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-clause-re))

(defun py-close-block-or-clause ()
  "Close block-or-clause at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-block-or-clause-re))

(defun py-close-def ()
  "Close def at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-def-re))

(defun py-close-def-or-class ()
  "Close def-or-class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-def-or-class-re))

(defun py-close-minor-block ()
  "Close minor-block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-minor-block-re))

(defun py-close-statement ()
  "Close statement at point.

Set indent level to that of beginning of function definition.

If final line isn't empty
and `py-close-block-provides-newline' non-nil,
insert a newline."
  (interactive "*")
  (py--close-intern 'py-statement-re))

(provide 'python-components-close-forms)
;;; python-components-close-forms.el ends here
