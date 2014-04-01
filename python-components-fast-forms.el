;;; python-components-fast-forms.el --- Execute forms at point

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

;;; Process forms fast

(defun py-execute-statement-fast ()
  "Process statement at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-statement-p)
		     (save-excursion
		       (py-beginning-of-statement)))))
	(end (save-excursion
	       (py-end-of-statement))))
    (py-execute-region beg end)))

(defun py-execute-block-fast ()
  "Process block at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-block-p)
		     (save-excursion
		       (py-beginning-of-block)))))
	(end (save-excursion
	       (py-end-of-block))))
    (py-execute-region beg end)))

(defun py-execute-block-or-clause-fast ()
  "Process block-or-clause at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-block-or-clause-p)
		     (save-excursion
		       (py-beginning-of-block-or-clause)))))
	(end (save-excursion
	       (py-end-of-block-or-clause))))
    (py-execute-region beg end)))

(defun py-execute-def-fast ()
  "Process def at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-def-p)
		     (save-excursion
		       (py-beginning-of-def)))))
	(end (save-excursion
	       (py-end-of-def))))
    (py-execute-region beg end)))

(defun py-execute-class-fast ()
  "Process class at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-class-p)
		     (save-excursion
		       (py-beginning-of-class)))))
	(end (save-excursion
	       (py-end-of-class))))
    (py-execute-region beg end)))

(defun py-execute-def-or-class-fast ()
  "Process def-or-class at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-def-or-class-p)
		     (save-excursion
		       (py-beginning-of-def-or-class)))))
	(end (save-excursion
	       (py-end-of-def-or-class))))
    (py-execute-region beg end)))

(defun py-execute-expression-fast ()
  "Process expression at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-expression-p)
		     (save-excursion
		       (py-beginning-of-expression)))))
	(end (save-excursion
	       (py-end-of-expression))))
    (py-execute-region beg end)))

(defun py-execute-partial-expression-fast ()
  "Process partial-expression at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-partial-expression-p)
		     (save-excursion
		       (py-beginning-of-partial-expression)))))
	(end (save-excursion
	       (py-end-of-partial-expression))))
    (py-execute-region beg end)))

(defun py-execute-top-level-fast ()
  "Process top-level at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-top-level-p)
		     (save-excursion
		       (py-beginning-of-top-level)))))
	(end (save-excursion
	       (py-end-of-top-level))))
    (py-execute-region beg end)))

(defun py-execute-clause-fast ()
  "Process clause at point by a Python interpreter. 

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t)
        (beg (prog1
		 (or (py-beginning-of-clause-p)
		     (save-excursion
		       (py-beginning-of-clause)))))
	(end (save-excursion
	       (py-end-of-clause))))
    (py-execute-region beg end)))

(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here
 
