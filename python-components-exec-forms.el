;;; python-components-exec-forms.el --- Execute forms at point

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
  (let ((beg (prog1
		 (or (py-beginning-of-statement-p)
		     (save-excursion
		       (py-beginning-of-statement)))))
	(end (save-excursion
	       (py-end-of-statement))))
    (py-execute-region beg end)))

(defun py-execute-block ()
  "Send block at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-block-p)
		     (save-excursion
		       (py-beginning-of-block)))))
	(end (save-excursion
	       (py-end-of-block))))
    (py-execute-region beg end)))

(defun py-execute-block-or-clause ()
  "Send block-or-clause at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-block-or-clause-p)
		     (save-excursion
		       (py-beginning-of-block-or-clause)))))
	(end (save-excursion
	       (py-end-of-block-or-clause))))
    (py-execute-region beg end)))

(defun py-execute-def ()
  "Send def at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-def-p)
		     (save-excursion
		       (py-beginning-of-def)))))
	(end (save-excursion
	       (py-end-of-def))))
    (py-execute-region beg end)))

(defun py-execute-class ()
  "Send class at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-class-p)
		     (save-excursion
		       (py-beginning-of-class)))))
	(end (save-excursion
	       (py-end-of-class))))
    (py-execute-region beg end)))

(defun py-execute-def-or-class ()
  "Send def-or-class at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-def-or-class-p)
		     (save-excursion
		       (py-beginning-of-def-or-class)))))
	(end (save-excursion
	       (py-end-of-def-or-class))))
    (py-execute-region beg end)))

(defun py-execute-expression ()
  "Send expression at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-expression-p)
		     (save-excursion
		       (py-beginning-of-expression)))))
	(end (save-excursion
	       (py-end-of-expression))))
    (py-execute-region beg end)))

(defun py-execute-partial-expression ()
  "Send partial-expression at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-partial-expression-p)
		     (save-excursion
		       (py-beginning-of-partial-expression)))))
	(end (save-excursion
	       (py-end-of-partial-expression))))
    (py-execute-region beg end)))

(defun py-execute-top-level ()
  "Send top-level at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-top-level-p)
		     (save-excursion
		       (py-beginning-of-top-level)))))
	(end (save-excursion
	       (py-end-of-top-level))))
    (py-execute-region beg end)))

(defun py-execute-clause ()
  "Send clause at point to a Python interpreter. "
  (interactive)
  (let ((beg (prog1
		 (or (py-beginning-of-clause-p)
		     (save-excursion
		       (py-beginning-of-clause)))))
	(end (save-excursion
	       (py-end-of-clause))))
    (py-execute-region beg end)))

(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here
