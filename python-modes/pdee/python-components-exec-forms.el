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

;;; Code
(require 'python-components-macros) 

(defun py-execute-statement (&optional shell dedicated switch)
  "Send statement at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-statement-p)
                       (py-beginning-of-statement))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-block (&optional shell dedicated switch)
  "Send block at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-p)
                       (py-beginning-of-block))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-clause (&optional shell dedicated switch)
  "Send clause at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-clause-p)
                       (py-beginning-of-clause))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-block-or-clause (&optional shell dedicated switch)
  "Send block-or-clause at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-or-clause-p)
                       (py-beginning-of-block-or-clause))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-def (&optional shell dedicated switch)
  "Send def at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-p)
                       (py-beginning-of-def))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-class (&optional shell dedicated switch)
  "Send class at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-class-p)
                       (py-beginning-of-class))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-def-or-class (&optional shell dedicated switch)
  "Send def-or-class at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-or-class-p)
                       (py-beginning-of-def-or-class))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-expression (&optional shell dedicated switch)
  "Send expression at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-expression-p)
                       (py-beginning-of-expression))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-partial-expression (&optional shell dedicated switch)
  "Send partial-expression at point to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.

When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)"
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-partial-expression-p)
                       (py-beginning-of-partial-expression))))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end shell dedicated switch))))

(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here
 
