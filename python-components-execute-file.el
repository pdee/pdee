;;; python-components-execute-file.el --- Execute files from python-mode

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

;;; Execute file commands

(defun py-execute-file-python (&optional filename)
  "Send file to a Python interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python" nil nil))

(defun py-execute-file-python-switch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python" nil 'switch))

(defun py-execute-file-python-noswitch (&optional filename)
  "Send file to a Python interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python" nil 'noswitch))

(defun py-execute-file-python-dedicated (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python" 'dedicated nil))

(defun py-execute-file-python-dedicated-switch (&optional filename)
  "Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python" 'dedicated 'switch))

(defun py-execute-file-ipython (&optional filename)
  "Send file to a Ipython interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "ipython" nil nil))

(defun py-execute-file-ipython-switch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "ipython" nil 'switch))

(defun py-execute-file-ipython-noswitch (&optional filename)
  "Send file to a Ipython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "ipython" nil 'noswitch))

(defun py-execute-file-ipython-dedicated (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "ipython" 'dedicated nil))

(defun py-execute-file-ipython-dedicated-switch (&optional filename)
  "Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "ipython" 'dedicated 'switch))

(defun py-execute-file-python3 (&optional filename)
  "Send file to a Python3 interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python3" nil nil))

(defun py-execute-file-python3-switch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3" nil 'switch))

(defun py-execute-file-python3-noswitch (&optional filename)
  "Send file to a Python3 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3" nil 'noswitch))

(defun py-execute-file-python3-dedicated (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python3" 'dedicated nil))

(defun py-execute-file-python3-dedicated-switch (&optional filename)
  "Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3" 'dedicated 'switch))

(defun py-execute-file-python2 (&optional filename)
  "Send file to a Python2 interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python2" nil nil))

(defun py-execute-file-python2-switch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2" nil 'switch))

(defun py-execute-file-python2-noswitch (&optional filename)
  "Send file to a Python2 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2" nil 'noswitch))

(defun py-execute-file-python2-dedicated (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python2" 'dedicated nil))

(defun py-execute-file-python2-dedicated-switch (&optional filename)
  "Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2" 'dedicated 'switch))

(defun py-execute-file-python2.7 (&optional filename)
  "Send file to a Python2.7 interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python2.7" nil nil))

(defun py-execute-file-python2.7-switch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2.7" nil 'switch))

(defun py-execute-file-python2.7-noswitch (&optional filename)
  "Send file to a Python2.7 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2.7" nil 'noswitch))

(defun py-execute-file-python2.7-dedicated (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python2.7" 'dedicated nil))

(defun py-execute-file-python2.7-dedicated-switch (&optional filename)
  "Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python2.7" 'dedicated 'switch))

(defun py-execute-file-jython (&optional filename)
  "Send file to a Jython interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "jython" nil nil))

(defun py-execute-file-jython-switch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "jython" nil 'switch))

(defun py-execute-file-jython-noswitch (&optional filename)
  "Send file to a Jython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "jython" nil 'noswitch))

(defun py-execute-file-jython-dedicated (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "jython" 'dedicated nil))

(defun py-execute-file-jython-dedicated-switch (&optional filename)
  "Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "jython" 'dedicated 'switch))

(defun py-execute-file-python3.2 (&optional filename)
  "Send file to a Python3.2 interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python3.2" nil nil))

(defun py-execute-file-python3.2-switch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.2" nil 'switch))

(defun py-execute-file-python3.2-noswitch (&optional filename)
  "Send file to a Python3.2 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.2" nil 'noswitch))

(defun py-execute-file-python3.2-dedicated (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python3.2" 'dedicated nil))

(defun py-execute-file-python3.2-dedicated-switch (&optional filename)
  "Send file to a Python3.2 interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.2" 'dedicated 'switch))

(defun py-execute-file-python3.3 (&optional filename)
  "Send file to a Python3.3 interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "python3.3" nil nil))

(defun py-execute-file-python3.3-switch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.3" nil 'switch))

(defun py-execute-file-python3.3-noswitch (&optional filename)
  "Send file to a Python3.3 interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.3" nil 'noswitch))

(defun py-execute-file-python3.3-dedicated (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "python3.3" 'dedicated nil))

(defun py-execute-file-python3.3-dedicated-switch (&optional filename)
  "Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "python3.3" 'dedicated 'switch))

(defun py-execute-file-bpython (&optional filename)
  "Send file to a Bpython interpreter."
  (interactive "fFile: ")
  (py-execute-file filename "bpython" nil nil))

(defun py-execute-file-bpython-switch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "bpython" nil 'switch))

(defun py-execute-file-bpython-noswitch (&optional filename)
  "Send file to a Bpython interpreter.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"nil\""
  (interactive "fFile: ")
  (py-execute-file filename "bpython" nil 'noswitch))

(defun py-execute-file-bpython-dedicated (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell."
  (interactive "fFile: ")
  (py-execute-file filename "bpython" 'dedicated nil))

(defun py-execute-file-bpython-dedicated-switch (&optional filename)
  "Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-shell-switch-buffers-on-execute-p', uses it with value \"non-nil\""
  (interactive "fFile: ")
  (py-execute-file filename "bpython" 'dedicated 'switch))

(provide 'python-components-execute-file)
;;; 'python-components-execute-file.el ends here
 
