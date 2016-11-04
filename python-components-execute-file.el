;;; python-components-execute-file.el --- Execute files from python-mode -*- lexical-binding: t; -*- 

;; Copyright (C) 2015-2016 Andreas Röhler

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

;;; Code:

;;  Execute file commands

(defun py-execute-file-python (&optional filename)
  "Send file to Python default interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python" nil nil nil nil t))

(defun py-execute-file-python-switch (&optional filename)
  "Send file to Python default interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" nil 'switch nil nil t))

(defun py-execute-file-python-no-switch (&optional filename)
  "Send file to Python default interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" nil 'no-switch nil nil t))

(defun py-execute-file-python-dedicated (&optional filename)
  "Send file to Python default interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python" 'dedicated nil nil nil t))

(defun py-execute-file-python-dedicated-switch (&optional filename)
  "Send file to Python default interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python" 'dedicated 'switch nil nil t))

(defun py-execute-file-ipython (&optional filename)
  "Send file to a Ipython interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil nil nil nil t))

(defun py-execute-file-ipython-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil 'switch nil nil t))

(defun py-execute-file-ipython-no-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" nil 'no-switch nil nil t))

(defun py-execute-file-ipython-dedicated (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" 'dedicated nil nil nil t))

(defun py-execute-file-ipython-dedicated-switch (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "ipython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3 (&optional filename)
  "Send file to a Python3 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil nil nil nil t))

(defun py-execute-file-python3-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil 'switch nil nil t))

(defun py-execute-file-python3-no-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" nil 'no-switch nil nil t))

(defun py-execute-file-python3-dedicated (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" 'dedicated nil nil nil t))

(defun py-execute-file-python3-dedicated-switch (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2 (&optional filename)
  "Send file to a Python2 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil nil nil nil t))

(defun py-execute-file-python2-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil 'switch nil nil t))

(defun py-execute-file-python2-no-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" nil 'no-switch nil nil t))

(defun py-execute-file-python2-dedicated (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" 'dedicated nil nil nil t))

(defun py-execute-file-python2-dedicated-switch (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python2.7 (&optional filename)
  "Send file to a Python2.7 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil nil nil nil t))

(defun py-execute-file-python2.7-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil 'switch nil nil t))

(defun py-execute-file-python2.7-no-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" nil 'no-switch nil nil t))

(defun py-execute-file-python2.7-dedicated (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" 'dedicated nil nil nil t))

(defun py-execute-file-python2.7-dedicated-switch (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python2.7" 'dedicated 'switch nil nil t))

(defun py-execute-file-jython (&optional filename)
  "Send file to a Jython interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil nil nil nil t))

(defun py-execute-file-jython-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil 'switch nil nil t))

(defun py-execute-file-jython-no-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" nil 'no-switch nil nil t))

(defun py-execute-file-jython-dedicated (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" 'dedicated nil nil nil t))

(defun py-execute-file-jython-dedicated-switch (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "jython" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.2 (&optional filename)
  "Send file to a Python3.2 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil nil nil nil t))

(defun py-execute-file-python3.2-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil 'switch nil nil t))

(defun py-execute-file-python3.2-no-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" nil 'no-switch nil nil t))

(defun py-execute-file-python3.2-dedicated (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" 'dedicated nil nil nil t))

(defun py-execute-file-python3.2-dedicated-switch (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.2" 'dedicated 'switch nil nil t))

(defun py-execute-file-python3.3 (&optional filename)
  "Send file to a Python3.3 interpreter."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil nil nil nil t))

(defun py-execute-file-python3.3-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil 'switch nil nil t))

(defun py-execute-file-python3.3-no-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" nil 'no-switch nil nil t))

(defun py-execute-file-python3.3-dedicated (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" 'dedicated nil nil nil t))

(defun py-execute-file-python3.3-dedicated-switch (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py--execute-prepare filename "python3.3" 'dedicated 'switch nil nil t))

(provide 'python-components-execute-file)
;;;  'python-components-execute-file.el ends here
