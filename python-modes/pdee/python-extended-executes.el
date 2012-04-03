;;; python-extended-executes.el --- more execute forms
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

;;; Code:


;;; Executes
(defun py-execute-prepare (form &optional shell dedicated switch)
  "Used by python-extended-executes ."
  (save-excursion
    (let ((beg (prog1
                   (or (funcall (intern-soft (concat "py-beginning-of-" form "-p")))

                       (funcall (intern-soft (concat "py-beginning-of-" form)))
                       (push-mark))))
          (end (funcall (intern-soft (concat "py-end-of-" form)))))
      (py-execute-base beg end shell dedicated switch))))

(defun py-execute-statement-python ()
  "Send statement at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python" nil nil))

(defun py-execute-statement-python-switch ()
  "Send statement at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python" nil 'switch))

(defun py-execute-statement-python-noswitch ()
  "Send statement at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python" nil 'noswitch))

(defun py-execute-statement-python-dedicated ()
  "Send statement at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python" t nil))

(defun py-execute-statement-python-dedicated-switch ()
  "Send statement at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python" t 'switch))

(defun py-execute-statement-ipython ()
  "Send statement at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil nil))

(defun py-execute-statement-ipython-switch ()
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil 'switch))

(defun py-execute-statement-ipython-noswitch ()
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "ipython" nil 'noswitch))

(defun py-execute-statement-ipython-dedicated ()
  "Send statement at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "ipython" t nil))

(defun py-execute-statement-ipython-dedicated-switch ()
  "Send statement at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "ipython" t 'switch))

(defun py-execute-statement-python3 ()
  "Send statement at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3" nil nil))

(defun py-execute-statement-python3-switch ()
  "Send statement at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python3" nil 'switch))

(defun py-execute-statement-python3-noswitch ()
  "Send statement at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python3" nil 'noswitch))

(defun py-execute-statement-python3-dedicated ()
  "Send statement at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3" t nil))

(defun py-execute-statement-python3-dedicated-switch ()
  "Send statement at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python3" t 'switch))

(defun py-execute-statement-python2 ()
  "Send statement at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2" nil nil))

(defun py-execute-statement-python2-switch ()
  "Send statement at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python2" nil 'switch))

(defun py-execute-statement-python2-noswitch ()
  "Send statement at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python2" nil 'noswitch))

(defun py-execute-statement-python2-dedicated ()
  "Send statement at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2" t nil))

(defun py-execute-statement-python2-dedicated-switch ()
  "Send statement at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python2" t 'switch))

(defun py-execute-statement-python2.7 ()
  "Send statement at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil nil))

(defun py-execute-statement-python2.7-switch ()
  "Send statement at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil 'switch))

(defun py-execute-statement-python2.7-noswitch ()
  "Send statement at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python2.7" nil 'noswitch))

(defun py-execute-statement-python2.7-dedicated ()
  "Send statement at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" t nil))

(defun py-execute-statement-python2.7-dedicated-switch ()
  "Send statement at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python2.7" t 'switch))

(defun py-execute-statement-jython ()
  "Send statement at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "statement" "jython" nil nil))

(defun py-execute-statement-jython-switch ()
  "Send statement at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "jython" nil 'switch))

(defun py-execute-statement-jython-noswitch ()
  "Send statement at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "jython" nil 'noswitch))

(defun py-execute-statement-jython-dedicated ()
  "Send statement at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "jython" t nil))

(defun py-execute-statement-jython-dedicated-switch ()
  "Send statement at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "jython" t 'switch))

(defun py-execute-statement-python3.2 ()
  "Send statement at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil nil))

(defun py-execute-statement-python3.2-switch ()
  "Send statement at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil 'switch))

(defun py-execute-statement-python3.2-noswitch ()
  "Send statement at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" "python3.2" nil 'noswitch))

(defun py-execute-statement-python3.2-dedicated ()
  "Send statement at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" t nil))

(defun py-execute-statement-python3.2-dedicated-switch ()
  "Send statement at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" "python3.2" t 'switch))

(defun py-execute-block-python ()
  "Send block at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "block" "python" nil nil))

(defun py-execute-block-python-switch ()
  "Send block at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python" nil 'switch))

(defun py-execute-block-python-noswitch ()
  "Send block at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python" nil 'noswitch))

(defun py-execute-block-python-dedicated ()
  "Send block at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python" t nil))

(defun py-execute-block-python-dedicated-switch ()
  "Send block at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python" t 'switch))

(defun py-execute-block-ipython ()
  "Send block at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block" "ipython" nil nil))

(defun py-execute-block-ipython-switch ()
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "ipython" nil 'switch))

(defun py-execute-block-ipython-noswitch ()
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "ipython" nil 'noswitch))

(defun py-execute-block-ipython-dedicated ()
  "Send block at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block" "ipython" t nil))

(defun py-execute-block-ipython-dedicated-switch ()
  "Send block at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "ipython" t 'switch))

(defun py-execute-block-python3 ()
  "Send block at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3" nil nil))

(defun py-execute-block-python3-switch ()
  "Send block at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python3" nil 'switch))

(defun py-execute-block-python3-noswitch ()
  "Send block at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python3" nil 'noswitch))

(defun py-execute-block-python3-dedicated ()
  "Send block at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3" t nil))

(defun py-execute-block-python3-dedicated-switch ()
  "Send block at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python3" t 'switch))

(defun py-execute-block-python2 ()
  "Send block at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2" nil nil))

(defun py-execute-block-python2-switch ()
  "Send block at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python2" nil 'switch))

(defun py-execute-block-python2-noswitch ()
  "Send block at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python2" nil 'noswitch))

(defun py-execute-block-python2-dedicated ()
  "Send block at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2" t nil))

(defun py-execute-block-python2-dedicated-switch ()
  "Send block at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python2" t 'switch))

(defun py-execute-block-python2.7 ()
  "Send block at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil nil))

(defun py-execute-block-python2.7-switch ()
  "Send block at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil 'switch))

(defun py-execute-block-python2.7-noswitch ()
  "Send block at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python2.7" nil 'noswitch))

(defun py-execute-block-python2.7-dedicated ()
  "Send block at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python2.7" t nil))

(defun py-execute-block-python2.7-dedicated-switch ()
  "Send block at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python2.7" t 'switch))

(defun py-execute-block-jython ()
  "Send block at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block" "jython" nil nil))

(defun py-execute-block-jython-switch ()
  "Send block at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "jython" nil 'switch))

(defun py-execute-block-jython-noswitch ()
  "Send block at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "jython" nil 'noswitch))

(defun py-execute-block-jython-dedicated ()
  "Send block at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block" "jython" t nil))

(defun py-execute-block-jython-dedicated-switch ()
  "Send block at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "jython" t 'switch))

(defun py-execute-block-python3.2 ()
  "Send block at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil nil))

(defun py-execute-block-python3.2-switch ()
  "Send block at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil 'switch))

(defun py-execute-block-python3.2-noswitch ()
  "Send block at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" "python3.2" nil 'noswitch))

(defun py-execute-block-python3.2-dedicated ()
  "Send block at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "block" "python3.2" t nil))

(defun py-execute-block-python3.2-dedicated-switch ()
  "Send block at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" "python3.2" t 'switch))

(defun py-execute-clause-python ()
  "Send clause at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python" nil nil))

(defun py-execute-clause-python-switch ()
  "Send clause at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python" nil 'switch))

(defun py-execute-clause-python-noswitch ()
  "Send clause at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python" nil 'noswitch))

(defun py-execute-clause-python-dedicated ()
  "Send clause at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python" t nil))

(defun py-execute-clause-python-dedicated-switch ()
  "Send clause at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python" t 'switch))

(defun py-execute-clause-ipython ()
  "Send clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil nil))

(defun py-execute-clause-ipython-switch ()
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil 'switch))

(defun py-execute-clause-ipython-noswitch ()
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "ipython" nil 'noswitch))

(defun py-execute-clause-ipython-dedicated ()
  "Send clause at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "ipython" t nil))

(defun py-execute-clause-ipython-dedicated-switch ()
  "Send clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "ipython" t 'switch))

(defun py-execute-clause-python3 ()
  "Send clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3" nil nil))

(defun py-execute-clause-python3-switch ()
  "Send clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python3" nil 'switch))

(defun py-execute-clause-python3-noswitch ()
  "Send clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python3" nil 'noswitch))

(defun py-execute-clause-python3-dedicated ()
  "Send clause at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3" t nil))

(defun py-execute-clause-python3-dedicated-switch ()
  "Send clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python3" t 'switch))

(defun py-execute-clause-python2 ()
  "Send clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2" nil nil))

(defun py-execute-clause-python2-switch ()
  "Send clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python2" nil 'switch))

(defun py-execute-clause-python2-noswitch ()
  "Send clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python2" nil 'noswitch))

(defun py-execute-clause-python2-dedicated ()
  "Send clause at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2" t nil))

(defun py-execute-clause-python2-dedicated-switch ()
  "Send clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python2" t 'switch))

(defun py-execute-clause-python2.7 ()
  "Send clause at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil nil))

(defun py-execute-clause-python2.7-switch ()
  "Send clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil 'switch))

(defun py-execute-clause-python2.7-noswitch ()
  "Send clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python2.7" nil 'noswitch))

(defun py-execute-clause-python2.7-dedicated ()
  "Send clause at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" t nil))

(defun py-execute-clause-python2.7-dedicated-switch ()
  "Send clause at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python2.7" t 'switch))

(defun py-execute-clause-jython ()
  "Send clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "clause" "jython" nil nil))

(defun py-execute-clause-jython-switch ()
  "Send clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "jython" nil 'switch))

(defun py-execute-clause-jython-noswitch ()
  "Send clause at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "jython" nil 'noswitch))

(defun py-execute-clause-jython-dedicated ()
  "Send clause at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "jython" t nil))

(defun py-execute-clause-jython-dedicated-switch ()
  "Send clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "jython" t 'switch))

(defun py-execute-clause-python3.2 ()
  "Send clause at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil nil))

(defun py-execute-clause-python3.2-switch ()
  "Send clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil 'switch))

(defun py-execute-clause-python3.2-noswitch ()
  "Send clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" "python3.2" nil 'noswitch))

(defun py-execute-clause-python3.2-dedicated ()
  "Send clause at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" t nil))

(defun py-execute-clause-python3.2-dedicated-switch ()
  "Send clause at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" "python3.2" t 'switch))

(defun py-execute-block-or-clause-python ()
  "Send block-or-clause at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil nil))

(defun py-execute-block-or-clause-python-switch ()
  "Send block-or-clause at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil 'switch))

(defun py-execute-block-or-clause-python-noswitch ()
  "Send block-or-clause at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" nil 'noswitch))

(defun py-execute-block-or-clause-python-dedicated ()
  "Send block-or-clause at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" t nil))

(defun py-execute-block-or-clause-python-dedicated-switch ()
  "Send block-or-clause at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python" t 'switch))

(defun py-execute-block-or-clause-ipython ()
  "Send block-or-clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil nil))

(defun py-execute-block-or-clause-ipython-switch ()
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil 'switch))

(defun py-execute-block-or-clause-ipython-noswitch ()
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" nil 'noswitch))

(defun py-execute-block-or-clause-ipython-dedicated ()
  "Send block-or-clause at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" t nil))

(defun py-execute-block-or-clause-ipython-dedicated-switch ()
  "Send block-or-clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "ipython" t 'switch))

(defun py-execute-block-or-clause-python3 ()
  "Send block-or-clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil nil))

(defun py-execute-block-or-clause-python3-switch ()
  "Send block-or-clause at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil 'switch))

(defun py-execute-block-or-clause-python3-noswitch ()
  "Send block-or-clause at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" nil 'noswitch))

(defun py-execute-block-or-clause-python3-dedicated ()
  "Send block-or-clause at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" t nil))

(defun py-execute-block-or-clause-python3-dedicated-switch ()
  "Send block-or-clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3" t 'switch))

(defun py-execute-block-or-clause-python2 ()
  "Send block-or-clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil nil))

(defun py-execute-block-or-clause-python2-switch ()
  "Send block-or-clause at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil 'switch))

(defun py-execute-block-or-clause-python2-noswitch ()
  "Send block-or-clause at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" nil 'noswitch))

(defun py-execute-block-or-clause-python2-dedicated ()
  "Send block-or-clause at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" t nil))

(defun py-execute-block-or-clause-python2-dedicated-switch ()
  "Send block-or-clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2" t 'switch))

(defun py-execute-block-or-clause-python2.7 ()
  "Send block-or-clause at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil nil))

(defun py-execute-block-or-clause-python2.7-switch ()
  "Send block-or-clause at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil 'switch))

(defun py-execute-block-or-clause-python2.7-noswitch ()
  "Send block-or-clause at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" nil 'noswitch))

(defun py-execute-block-or-clause-python2.7-dedicated ()
  "Send block-or-clause at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" t nil))

(defun py-execute-block-or-clause-python2.7-dedicated-switch ()
  "Send block-or-clause at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python2.7" t 'switch))

(defun py-execute-block-or-clause-jython ()
  "Send block-or-clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil nil))

(defun py-execute-block-or-clause-jython-switch ()
  "Send block-or-clause at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil 'switch))

(defun py-execute-block-or-clause-jython-noswitch ()
  "Send block-or-clause at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" nil 'noswitch))

(defun py-execute-block-or-clause-jython-dedicated ()
  "Send block-or-clause at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" t nil))

(defun py-execute-block-or-clause-jython-dedicated-switch ()
  "Send block-or-clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "jython" t 'switch))

(defun py-execute-block-or-clause-python3.2 ()
  "Send block-or-clause at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil nil))

(defun py-execute-block-or-clause-python3.2-switch ()
  "Send block-or-clause at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil 'switch))

(defun py-execute-block-or-clause-python3.2-noswitch ()
  "Send block-or-clause at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" nil 'noswitch))

(defun py-execute-block-or-clause-python3.2-dedicated ()
  "Send block-or-clause at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" t nil))

(defun py-execute-block-or-clause-python3.2-dedicated-switch ()
  "Send block-or-clause at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" "python3.2" t 'switch))

(defun py-execute-def-python ()
  "Send def at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "def" "python" nil nil))

(defun py-execute-def-python-switch ()
  "Send def at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python" nil 'switch))

(defun py-execute-def-python-noswitch ()
  "Send def at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python" nil 'noswitch))

(defun py-execute-def-python-dedicated ()
  "Send def at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python" t nil))

(defun py-execute-def-python-dedicated-switch ()
  "Send def at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python" t 'switch))

(defun py-execute-def-ipython ()
  "Send def at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "def" "ipython" nil nil))

(defun py-execute-def-ipython-switch ()
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "ipython" nil 'switch))

(defun py-execute-def-ipython-noswitch ()
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "ipython" nil 'noswitch))

(defun py-execute-def-ipython-dedicated ()
  "Send def at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "def" "ipython" t nil))

(defun py-execute-def-ipython-dedicated-switch ()
  "Send def at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "ipython" t 'switch))

(defun py-execute-def-python3 ()
  "Send def at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3" nil nil))

(defun py-execute-def-python3-switch ()
  "Send def at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python3" nil 'switch))

(defun py-execute-def-python3-noswitch ()
  "Send def at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python3" nil 'noswitch))

(defun py-execute-def-python3-dedicated ()
  "Send def at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3" t nil))

(defun py-execute-def-python3-dedicated-switch ()
  "Send def at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python3" t 'switch))

(defun py-execute-def-python2 ()
  "Send def at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2" nil nil))

(defun py-execute-def-python2-switch ()
  "Send def at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python2" nil 'switch))

(defun py-execute-def-python2-noswitch ()
  "Send def at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python2" nil 'noswitch))

(defun py-execute-def-python2-dedicated ()
  "Send def at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2" t nil))

(defun py-execute-def-python2-dedicated-switch ()
  "Send def at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python2" t 'switch))

(defun py-execute-def-python2.7 ()
  "Send def at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil nil))

(defun py-execute-def-python2.7-switch ()
  "Send def at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil 'switch))

(defun py-execute-def-python2.7-noswitch ()
  "Send def at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python2.7" nil 'noswitch))

(defun py-execute-def-python2.7-dedicated ()
  "Send def at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python2.7" t nil))

(defun py-execute-def-python2.7-dedicated-switch ()
  "Send def at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python2.7" t 'switch))

(defun py-execute-def-jython ()
  "Send def at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "def" "jython" nil nil))

(defun py-execute-def-jython-switch ()
  "Send def at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "jython" nil 'switch))

(defun py-execute-def-jython-noswitch ()
  "Send def at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "jython" nil 'noswitch))

(defun py-execute-def-jython-dedicated ()
  "Send def at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "def" "jython" t nil))

(defun py-execute-def-jython-dedicated-switch ()
  "Send def at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "jython" t 'switch))

(defun py-execute-def-python3.2 ()
  "Send def at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil nil))

(defun py-execute-def-python3.2-switch ()
  "Send def at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil 'switch))

(defun py-execute-def-python3.2-noswitch ()
  "Send def at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" "python3.2" nil 'noswitch))

(defun py-execute-def-python3.2-dedicated ()
  "Send def at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "def" "python3.2" t nil))

(defun py-execute-def-python3.2-dedicated-switch ()
  "Send def at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" "python3.2" t 'switch))

(defun py-execute-class-python ()
  "Send class at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "class" "python" nil nil))

(defun py-execute-class-python-switch ()
  "Send class at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python" nil 'switch))

(defun py-execute-class-python-noswitch ()
  "Send class at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python" nil 'noswitch))

(defun py-execute-class-python-dedicated ()
  "Send class at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python" t nil))

(defun py-execute-class-python-dedicated-switch ()
  "Send class at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python" t 'switch))

(defun py-execute-class-ipython ()
  "Send class at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "class" "ipython" nil nil))

(defun py-execute-class-ipython-switch ()
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "ipython" nil 'switch))

(defun py-execute-class-ipython-noswitch ()
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "ipython" nil 'noswitch))

(defun py-execute-class-ipython-dedicated ()
  "Send class at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "class" "ipython" t nil))

(defun py-execute-class-ipython-dedicated-switch ()
  "Send class at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "ipython" t 'switch))

(defun py-execute-class-python3 ()
  "Send class at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3" nil nil))

(defun py-execute-class-python3-switch ()
  "Send class at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python3" nil 'switch))

(defun py-execute-class-python3-noswitch ()
  "Send class at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python3" nil 'noswitch))

(defun py-execute-class-python3-dedicated ()
  "Send class at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3" t nil))

(defun py-execute-class-python3-dedicated-switch ()
  "Send class at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python3" t 'switch))

(defun py-execute-class-python2 ()
  "Send class at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2" nil nil))

(defun py-execute-class-python2-switch ()
  "Send class at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python2" nil 'switch))

(defun py-execute-class-python2-noswitch ()
  "Send class at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python2" nil 'noswitch))

(defun py-execute-class-python2-dedicated ()
  "Send class at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2" t nil))

(defun py-execute-class-python2-dedicated-switch ()
  "Send class at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python2" t 'switch))

(defun py-execute-class-python2.7 ()
  "Send class at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil nil))

(defun py-execute-class-python2.7-switch ()
  "Send class at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil 'switch))

(defun py-execute-class-python2.7-noswitch ()
  "Send class at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python2.7" nil 'noswitch))

(defun py-execute-class-python2.7-dedicated ()
  "Send class at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python2.7" t nil))

(defun py-execute-class-python2.7-dedicated-switch ()
  "Send class at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python2.7" t 'switch))

(defun py-execute-class-jython ()
  "Send class at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "class" "jython" nil nil))

(defun py-execute-class-jython-switch ()
  "Send class at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "jython" nil 'switch))

(defun py-execute-class-jython-noswitch ()
  "Send class at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "jython" nil 'noswitch))

(defun py-execute-class-jython-dedicated ()
  "Send class at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "class" "jython" t nil))

(defun py-execute-class-jython-dedicated-switch ()
  "Send class at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "jython" t 'switch))

(defun py-execute-class-python3.2 ()
  "Send class at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil nil))

(defun py-execute-class-python3.2-switch ()
  "Send class at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil 'switch))

(defun py-execute-class-python3.2-noswitch ()
  "Send class at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" "python3.2" nil 'noswitch))

(defun py-execute-class-python3.2-dedicated ()
  "Send class at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "class" "python3.2" t nil))

(defun py-execute-class-python3.2-dedicated-switch ()
  "Send class at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" "python3.2" t 'switch))

(defun py-execute-region-python (beg end)
  "Send region at point to Python interpreter. "
  (interactive "r")
  (py-execute-base beg end "python" nil nil))

(defun py-execute-region-python-switch (beg end)
  "Send region at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "python" nil 'switch))

(defun py-execute-region-python-noswitch (beg end)
  "Send region at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "python" nil 'noswitch))

(defun py-execute-region-python-dedicated (beg end)
  "Send region at point to Pythonunique interpreter. "
  (interactive "r")
  (py-execute-base beg end "python" t nil))

(defun py-execute-region-python-dedicated-switch (beg end)
  "Send region at point to Python unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "python" t 'switch))

(defun py-execute-region-ipython (beg end)
  "Send region at point to IPython interpreter. "
  (interactive "r")
  (py-execute-base beg end "ipython" nil nil))

(defun py-execute-region-ipython-switch (beg end)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "ipython" nil 'switch))

(defun py-execute-region-ipython-noswitch (beg end)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "ipython" nil 'noswitch))

(defun py-execute-region-ipython-dedicated (beg end)
  "Send region at point to IPythonunique interpreter. "
  (interactive "r")
  (py-execute-base beg end "ipython" t nil))

(defun py-execute-region-ipython-dedicated-switch (beg end)
  "Send region at point to IPython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "ipython" t 'switch))

(defun py-execute-region-python3 (beg end)
  "Send region at point to Python3 interpreter. "
  (interactive "r")
  (py-execute-base beg end "python3" nil nil))

(defun py-execute-region-python3-switch (beg end)
  "Send region at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "python3" nil 'switch))

(defun py-execute-region-python3-noswitch (beg end)
  "Send region at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "python3" nil 'noswitch))

(defun py-execute-region-python3-dedicated (beg end)
  "Send region at point to Python3unique interpreter. "
  (interactive "r")
  (py-execute-base beg end "python3" t nil))

(defun py-execute-region-python3-dedicated-switch (beg end)
  "Send region at point to Python3 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "python3" t 'switch))

(defun py-execute-region-python2 (beg end)
  "Send region at point to Python2 interpreter. "
  (interactive "r")
  (py-execute-base beg end "python2" nil nil))

(defun py-execute-region-python2-switch (beg end)
  "Send region at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "python2" nil 'switch))

(defun py-execute-region-python2-noswitch (beg end)
  "Send region at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "python2" nil 'noswitch))

(defun py-execute-region-python2-dedicated (beg end)
  "Send region at point to Python2unique interpreter. "
  (interactive "r")
  (py-execute-base beg end "python2" t nil))

(defun py-execute-region-python2-dedicated-switch (beg end)
  "Send region at point to Python2 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "python2" t 'switch))

(defun py-execute-region-python2.7 (beg end)
  "Send region at point to Python2.7 interpreter. "
  (interactive "r")
  (py-execute-base beg end "python2.7" nil nil))

(defun py-execute-region-python2.7-switch (beg end)
  "Send region at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "python2.7" nil 'switch))

(defun py-execute-region-python2.7-noswitch (beg end)
  "Send region at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "python2.7" nil 'noswitch))

(defun py-execute-region-python2.7-dedicated (beg end)
  "Send region at point to Python2.7unique interpreter. "
  (interactive "r")
  (py-execute-base beg end "python2.7" t nil))

(defun py-execute-region-python2.7-dedicated-switch (beg end)
  "Send region at point to Python2.7 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "python2.7" t 'switch))

(defun py-execute-region-jython (beg end)
  "Send region at point to Jython interpreter. "
  (interactive "r")
  (py-execute-base beg end "jython" nil nil))

(defun py-execute-region-jython-switch (beg end)
  "Send region at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "jython" nil 'switch))

(defun py-execute-region-jython-noswitch (beg end)
  "Send region at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "jython" nil 'noswitch))

(defun py-execute-region-jython-dedicated (beg end)
  "Send region at point to Jythonunique interpreter. "
  (interactive "r")
  (py-execute-base beg end "jython" t nil))

(defun py-execute-region-jython-dedicated-switch (beg end)
  "Send region at point to Jython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "jython" t 'switch))

(defun py-execute-region-python3.2 (beg end)
  "Send region at point to Python3.2 interpreter. "
  (interactive "r")
  (py-execute-base beg end "python3.2" nil nil))

(defun py-execute-region-python3.2-switch (beg end)
  "Send region at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-base beg end "python3.2" nil 'switch))

(defun py-execute-region-python3.2-noswitch (beg end)
  "Send region at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-base beg end "python3.2" nil 'noswitch))

(defun py-execute-region-python3.2-dedicated (beg end)
  "Send region at point to Python3.2unique interpreter. "
  (interactive "r")
  (py-execute-base beg end "python3.2" t nil))

(defun py-execute-region-python3.2-dedicated-switch (beg end)
  "Send region at point to Python3.2 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-base beg end "python3.2" t 'switch))

(defun py-execute-buffer-python ()
  "Send buffer at point to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python" nil nil))))

(defun py-execute-buffer-python-switch ()
  "Send buffer at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python" nil 'switch))))

(defun py-execute-buffer-python-noswitch ()
  "Send buffer at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python" nil 'noswitch))))

(defun py-execute-buffer-python-dedicated ()
  "Send buffer at point to Pythonunique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python" t nil))))

(defun py-execute-buffer-python-dedicated-switch ()
  "Send buffer at point to Python unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python" t 'switch))))

(defun py-execute-buffer-ipython ()
  "Send buffer at point to IPython interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "ipython" nil nil))))

(defun py-execute-buffer-ipython-switch ()
  "Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "ipython" nil 'switch))))

(defun py-execute-buffer-ipython-noswitch ()
  "Send buffer at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "ipython" nil 'noswitch))))

(defun py-execute-buffer-ipython-dedicated ()
  "Send buffer at point to IPythonunique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "ipython" t nil))))

(defun py-execute-buffer-ipython-dedicated-switch ()
  "Send buffer at point to IPython unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "ipython" t 'switch))))

(defun py-execute-buffer-python3 ()
  "Send buffer at point to Python3 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3" nil nil))))

(defun py-execute-buffer-python3-switch ()
  "Send buffer at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3" nil 'switch))))

(defun py-execute-buffer-python3-noswitch ()
  "Send buffer at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3" nil 'noswitch))))

(defun py-execute-buffer-python3-dedicated ()
  "Send buffer at point to Python3unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3" t nil))))

(defun py-execute-buffer-python3-dedicated-switch ()
  "Send buffer at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3" t 'switch))))

(defun py-execute-buffer-python2 ()
  "Send buffer at point to Python2 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2" nil nil))))

(defun py-execute-buffer-python2-switch ()
  "Send buffer at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2" nil 'switch))))

(defun py-execute-buffer-python2-noswitch ()
  "Send buffer at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2" nil 'noswitch))))

(defun py-execute-buffer-python2-dedicated ()
  "Send buffer at point to Python2unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2" t nil))))

(defun py-execute-buffer-python2-dedicated-switch ()
  "Send buffer at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2" t 'switch))))

(defun py-execute-buffer-python2.7 ()
  "Send buffer at point to Python2.7 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2.7" nil nil))))

(defun py-execute-buffer-python2.7-switch ()
  "Send buffer at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2.7" nil 'switch))))

(defun py-execute-buffer-python2.7-noswitch ()
  "Send buffer at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2.7" nil 'noswitch))))

(defun py-execute-buffer-python2.7-dedicated ()
  "Send buffer at point to Python2.7unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2.7" t nil))))

(defun py-execute-buffer-python2.7-dedicated-switch ()
  "Send buffer at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python2.7" t 'switch))))

(defun py-execute-buffer-jython ()
  "Send buffer at point to Jython interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "jython" nil nil))))

(defun py-execute-buffer-jython-switch ()
  "Send buffer at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "jython" nil 'switch))))

(defun py-execute-buffer-jython-noswitch ()
  "Send buffer at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "jython" nil 'noswitch))))

(defun py-execute-buffer-jython-dedicated ()
  "Send buffer at point to Jythonunique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "jython" t nil))))

(defun py-execute-buffer-jython-dedicated-switch ()
  "Send buffer at point to Jython unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "jython" t 'switch))))

(defun py-execute-buffer-python3.2 ()
  "Send buffer at point to Python3.2 interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3.2" nil nil))))

(defun py-execute-buffer-python3.2-switch ()
  "Send buffer at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3.2" nil 'switch))))

(defun py-execute-buffer-python3.2-noswitch ()
  "Send buffer at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3.2" nil 'noswitch))))

(defun py-execute-buffer-python3.2-dedicated ()
  "Send buffer at point to Python3.2unique interpreter. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3.2" t nil))))

(defun py-execute-buffer-python3.2-dedicated-switch ()
  "Send buffer at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end "python3.2" t 'switch))))

(defun py-execute-expression-python ()
  "Send expression at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python" nil nil))

(defun py-execute-expression-python-switch ()
  "Send expression at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python" nil 'switch))

(defun py-execute-expression-python-noswitch ()
  "Send expression at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python" nil 'noswitch))

(defun py-execute-expression-python-dedicated ()
  "Send expression at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python" t nil))

(defun py-execute-expression-python-dedicated-switch ()
  "Send expression at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python" t 'switch))

(defun py-execute-expression-ipython ()
  "Send expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil nil))

(defun py-execute-expression-ipython-switch ()
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil 'switch))

(defun py-execute-expression-ipython-noswitch ()
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "ipython" nil 'noswitch))

(defun py-execute-expression-ipython-dedicated ()
  "Send expression at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "ipython" t nil))

(defun py-execute-expression-ipython-dedicated-switch ()
  "Send expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "ipython" t 'switch))

(defun py-execute-expression-python3 ()
  "Send expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3" nil nil))

(defun py-execute-expression-python3-switch ()
  "Send expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python3" nil 'switch))

(defun py-execute-expression-python3-noswitch ()
  "Send expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python3" nil 'noswitch))

(defun py-execute-expression-python3-dedicated ()
  "Send expression at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3" t nil))

(defun py-execute-expression-python3-dedicated-switch ()
  "Send expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python3" t 'switch))

(defun py-execute-expression-python2 ()
  "Send expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2" nil nil))

(defun py-execute-expression-python2-switch ()
  "Send expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python2" nil 'switch))

(defun py-execute-expression-python2-noswitch ()
  "Send expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python2" nil 'noswitch))

(defun py-execute-expression-python2-dedicated ()
  "Send expression at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2" t nil))

(defun py-execute-expression-python2-dedicated-switch ()
  "Send expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python2" t 'switch))

(defun py-execute-expression-python2.7 ()
  "Send expression at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil nil))

(defun py-execute-expression-python2.7-switch ()
  "Send expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil 'switch))

(defun py-execute-expression-python2.7-noswitch ()
  "Send expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python2.7" nil 'noswitch))

(defun py-execute-expression-python2.7-dedicated ()
  "Send expression at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" t nil))

(defun py-execute-expression-python2.7-dedicated-switch ()
  "Send expression at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python2.7" t 'switch))

(defun py-execute-expression-jython ()
  "Send expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "expression" "jython" nil nil))

(defun py-execute-expression-jython-switch ()
  "Send expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "jython" nil 'switch))

(defun py-execute-expression-jython-noswitch ()
  "Send expression at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "jython" nil 'noswitch))

(defun py-execute-expression-jython-dedicated ()
  "Send expression at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "jython" t nil))

(defun py-execute-expression-jython-dedicated-switch ()
  "Send expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "jython" t 'switch))

(defun py-execute-expression-python3.2 ()
  "Send expression at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil nil))

(defun py-execute-expression-python3.2-switch ()
  "Send expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil 'switch))

(defun py-execute-expression-python3.2-noswitch ()
  "Send expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" "python3.2" nil 'noswitch))

(defun py-execute-expression-python3.2-dedicated ()
  "Send expression at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" t nil))

(defun py-execute-expression-python3.2-dedicated-switch ()
  "Send expression at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" "python3.2" t 'switch))

(defun py-execute-partial-expression-python ()
  "Send partial-expression at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil nil))

(defun py-execute-partial-expression-python-switch ()
  "Send partial-expression at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil 'switch))

(defun py-execute-partial-expression-python-noswitch ()
  "Send partial-expression at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python" nil 'noswitch))

(defun py-execute-partial-expression-python-dedicated ()
  "Send partial-expression at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" t nil))

(defun py-execute-partial-expression-python-dedicated-switch ()
  "Send partial-expression at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python" t 'switch))

(defun py-execute-partial-expression-ipython ()
  "Send partial-expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil nil))

(defun py-execute-partial-expression-ipython-switch ()
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil 'switch))

(defun py-execute-partial-expression-ipython-noswitch ()
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" nil 'noswitch))

(defun py-execute-partial-expression-ipython-dedicated ()
  "Send partial-expression at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" t nil))

(defun py-execute-partial-expression-ipython-dedicated-switch ()
  "Send partial-expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "ipython" t 'switch))

(defun py-execute-partial-expression-python3 ()
  "Send partial-expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil nil))

(defun py-execute-partial-expression-python3-switch ()
  "Send partial-expression at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil 'switch))

(defun py-execute-partial-expression-python3-noswitch ()
  "Send partial-expression at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" nil 'noswitch))

(defun py-execute-partial-expression-python3-dedicated ()
  "Send partial-expression at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" t nil))

(defun py-execute-partial-expression-python3-dedicated-switch ()
  "Send partial-expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3" t 'switch))

(defun py-execute-partial-expression-python2 ()
  "Send partial-expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil nil))

(defun py-execute-partial-expression-python2-switch ()
  "Send partial-expression at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil 'switch))

(defun py-execute-partial-expression-python2-noswitch ()
  "Send partial-expression at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" nil 'noswitch))

(defun py-execute-partial-expression-python2-dedicated ()
  "Send partial-expression at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" t nil))

(defun py-execute-partial-expression-python2-dedicated-switch ()
  "Send partial-expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2" t 'switch))

(defun py-execute-partial-expression-python2.7 ()
  "Send partial-expression at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil nil))

(defun py-execute-partial-expression-python2.7-switch ()
  "Send partial-expression at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil 'switch))

(defun py-execute-partial-expression-python2.7-noswitch ()
  "Send partial-expression at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" nil 'noswitch))

(defun py-execute-partial-expression-python2.7-dedicated ()
  "Send partial-expression at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" t nil))

(defun py-execute-partial-expression-python2.7-dedicated-switch ()
  "Send partial-expression at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python2.7" t 'switch))

(defun py-execute-partial-expression-jython ()
  "Send partial-expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil nil))

(defun py-execute-partial-expression-jython-switch ()
  "Send partial-expression at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil 'switch))

(defun py-execute-partial-expression-jython-noswitch ()
  "Send partial-expression at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" nil 'noswitch))

(defun py-execute-partial-expression-jython-dedicated ()
  "Send partial-expression at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" t nil))

(defun py-execute-partial-expression-jython-dedicated-switch ()
  "Send partial-expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "jython" t 'switch))

(defun py-execute-partial-expression-python3.2 ()
  "Send partial-expression at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil nil))

(defun py-execute-partial-expression-python3.2-switch ()
  "Send partial-expression at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil 'switch))

(defun py-execute-partial-expression-python3.2-noswitch ()
  "Send partial-expression at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" nil 'noswitch))

(defun py-execute-partial-expression-python3.2-dedicated ()
  "Send partial-expression at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" t nil))

(defun py-execute-partial-expression-python3.2-dedicated-switch ()
  "Send partial-expression at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" "python3.2" t 'switch))

(defun py-execute-line-python ()
  "Send line at point to Python interpreter. "
  (interactive)
  (py-execute-prepare "line" "python" nil nil))

(defun py-execute-line-python-switch ()
  "Send line at point to Python interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python" nil 'switch))

(defun py-execute-line-python-noswitch ()
  "Send line at point to Python interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python" nil 'noswitch))

(defun py-execute-line-python-dedicated ()
  "Send line at point to Pythonunique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python" t nil))

(defun py-execute-line-python-dedicated-switch ()
  "Send line at point to Python unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python" t 'switch))

(defun py-execute-line-ipython ()
  "Send line at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "line" "ipython" nil nil))

(defun py-execute-line-ipython-switch ()
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "ipython" nil 'switch))

(defun py-execute-line-ipython-noswitch ()
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "ipython" nil 'noswitch))

(defun py-execute-line-ipython-dedicated ()
  "Send line at point to IPythonunique interpreter. "
  (interactive)
  (py-execute-prepare "line" "ipython" t nil))

(defun py-execute-line-ipython-dedicated-switch ()
  "Send line at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "ipython" t 'switch))

(defun py-execute-line-python3 ()
  "Send line at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3" nil nil))

(defun py-execute-line-python3-switch ()
  "Send line at point to Python3 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python3" nil 'switch))

(defun py-execute-line-python3-noswitch ()
  "Send line at point to Python3 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python3" nil 'noswitch))

(defun py-execute-line-python3-dedicated ()
  "Send line at point to Python3unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3" t nil))

(defun py-execute-line-python3-dedicated-switch ()
  "Send line at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python3" t 'switch))

(defun py-execute-line-python2 ()
  "Send line at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2" nil nil))

(defun py-execute-line-python2-switch ()
  "Send line at point to Python2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python2" nil 'switch))

(defun py-execute-line-python2-noswitch ()
  "Send line at point to Python2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python2" nil 'noswitch))

(defun py-execute-line-python2-dedicated ()
  "Send line at point to Python2unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2" t nil))

(defun py-execute-line-python2-dedicated-switch ()
  "Send line at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python2" t 'switch))

(defun py-execute-line-python2.7 ()
  "Send line at point to Python2.7 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil nil))

(defun py-execute-line-python2.7-switch ()
  "Send line at point to Python2.7 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil 'switch))

(defun py-execute-line-python2.7-noswitch ()
  "Send line at point to Python2.7 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python2.7" nil 'noswitch))

(defun py-execute-line-python2.7-dedicated ()
  "Send line at point to Python2.7unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python2.7" t nil))

(defun py-execute-line-python2.7-dedicated-switch ()
  "Send line at point to Python2.7 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python2.7" t 'switch))

(defun py-execute-line-jython ()
  "Send line at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "line" "jython" nil nil))

(defun py-execute-line-jython-switch ()
  "Send line at point to Jython interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "jython" nil 'switch))

(defun py-execute-line-jython-noswitch ()
  "Send line at point to Jython interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "jython" nil 'noswitch))

(defun py-execute-line-jython-dedicated ()
  "Send line at point to Jythonunique interpreter. "
  (interactive)
  (py-execute-prepare "line" "jython" t nil))

(defun py-execute-line-jython-dedicated-switch ()
  "Send line at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "jython" t 'switch))

(defun py-execute-line-python3.2 ()
  "Send line at point to Python3.2 interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil nil))

(defun py-execute-line-python3.2-switch ()
  "Send line at point to Python3.2 interpreter.

Switch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil 'switch))

(defun py-execute-line-python3.2-noswitch ()
  "Send line at point to Python3.2 interpreter.

Keep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" "python3.2" nil 'noswitch))

(defun py-execute-line-python3.2-dedicated ()
  "Send line at point to Python3.2unique interpreter. "
  (interactive)
  (py-execute-prepare "line" "python3.2" t nil))

(defun py-execute-line-python3.2-dedicated-switch ()
  "Send line at point to Python3.2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" "python3.2" t 'switch))

(provide 'python-extended-executes)
;;; python-extended-executes.el ends here

