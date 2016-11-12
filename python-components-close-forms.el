;;; python-components-close-forms.el --- close forms -*- lexical-binding: t; -*-

;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;; Copyright (C) 2015-2016  Andreas Röhler

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


(defun py-close-block ()
  "Close block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-block-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Close class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-class-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-close-def ()
  "Close def at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-def-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-close-def-or-class ()
  "Close def-or-class at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-def-or-class-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-close-minor-block ()
  "Close minor-block at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-minor-block-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-close-statement ()
  "Close statement at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline.
"
  (interactive "*")
  (let ((erg (py--close-intern 'py-statement-re)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(provide 'python-components-close-forms)
;;; python-components-close-forms.el ends here
