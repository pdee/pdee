;; Extended executes --- more execute forms -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs
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

(defun py--execute-prepare (form shell &optional dedicated switch beg end filename fast proc wholebuf split)
  "Update some vars."
  (save-excursion
    (let* ((form (prin1-to-string form))
           (origline (py-count-lines))
	   (fast
	    (or fast py-fast-process-p))
	   (py-exception-buffer (current-buffer))
           (beg (unless filename
                  (prog1
                      (or beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

                          (funcall (intern-soft (concat "py-backward-" form)))
                          (push-mark)))))
           (end (unless filename
                  (or end (save-excursion (funcall (intern-soft (concat "py-forward-" form))))))))
      ;; (setq py-buffer-name nil)
      (if filename
            (if (file-readable-p filename)
                (py--execute-file-base (expand-file-name filename) nil nil nil origline)
              (message "%s not readable. %s" filename "Do you have write permissions?"))
        (py--execute-base beg end shell filename proc wholebuf fast dedicated split switch)))))

(defun py-execute-block (&optional shell dedicated fast split switch proc)
  "Send block at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-switch (&optional shell dedicated fast split proc)
  "Send block at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-no-switch (&optional shell dedicated fast split  proc)
  "Send block at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-dedicated (&optional shell fast split switch proc)
  "Send block at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-dedicated-switch (&optional shell  fast split  proc)
  "Send block at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython (&optional dedicated fast split switch proc)
  "Send block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython-switch (&optional dedicated fast split proc)
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython-no-switch (&optional dedicated fast split  proc)
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython-dedicated (&optional fast split switch proc)
  "Send block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython-dedicated-switch (&optional  fast split  proc)
  "Send block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython2.7 (&optional dedicated fast split switch proc)
  "Send block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython2.7-switch (&optional dedicated fast split proc)
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython2.7-dedicated (&optional fast split switch proc)
  "Send block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3 (&optional dedicated fast split switch proc)
  "Send block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3-switch (&optional dedicated fast split proc)
  "Send block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3-dedicated (&optional fast split switch proc)
  "Send block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython (&optional dedicated fast split switch proc)
  "Send block at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython-switch (&optional dedicated fast split proc)
  "Send block at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython-no-switch (&optional dedicated fast split  proc)
  "Send block at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython-dedicated (&optional fast split switch proc)
  "Send block at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-jython-dedicated-switch (&optional  fast split  proc)
  "Send block at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python (&optional dedicated fast split switch proc)
  "Send block at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python-switch (&optional dedicated fast split proc)
  "Send block at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python-no-switch (&optional dedicated fast split  proc)
  "Send block at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python-dedicated (&optional fast split switch proc)
  "Send block at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python-dedicated-switch (&optional  fast split  proc)
  "Send block at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2 (&optional dedicated fast split switch proc)
  "Send block at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2-switch (&optional dedicated fast split proc)
  "Send block at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2-no-switch (&optional dedicated fast split  proc)
  "Send block at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2-dedicated (&optional fast split switch proc)
  "Send block at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python2-dedicated-switch (&optional  fast split  proc)
  "Send block at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3 (&optional dedicated fast split switch proc)
  "Send block at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3-switch (&optional dedicated fast split proc)
  "Send block at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3-no-switch (&optional dedicated fast split  proc)
  "Send block at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3-dedicated (&optional fast split switch proc)
  "Send block at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-python3-dedicated-switch (&optional  fast split  proc)
  "Send block at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause (&optional shell dedicated fast split switch proc)
  "Send block-or-clause at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-switch (&optional shell dedicated fast split proc)
  "Send block-or-clause at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-no-switch (&optional shell dedicated fast split  proc)
  "Send block-or-clause at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-dedicated (&optional shell fast split switch proc)
  "Send block-or-clause at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-dedicated-switch (&optional shell  fast split  proc)
  "Send block-or-clause at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython2.7 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython2.7-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython2.7-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-jython-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python2-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3 (&optional dedicated fast split switch proc)
  "Send block-or-clause at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3-switch (&optional dedicated fast split proc)
  "Send block-or-clause at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3-no-switch (&optional dedicated fast split  proc)
  "Send block-or-clause at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3-dedicated (&optional fast split switch proc)
  "Send block-or-clause at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-block-or-clause-python3-dedicated-switch (&optional  fast split  proc)
  "Send block-or-clause at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'block-or-clause 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-buffer (&optional shell dedicated fast split switch proc)
  "Send buffer at point to  interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
	(shell (or shell (py-choose-shell)))
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-switch (&optional shell dedicated fast split proc)
  "Send buffer at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-no-switch (&optional shell dedicated fast split  proc)
  "Send buffer at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-dedicated (&optional shell fast split switch proc)
  "Send buffer at point to  unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-dedicated-switch (&optional shell  fast split  proc)
  "Send buffer at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer shell t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython (&optional dedicated fast split switch proc)
  "Send buffer at point to IPython interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython-switch (&optional dedicated fast split proc)
  "Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython-dedicated (&optional fast split switch proc)
  "Send buffer at point to IPython unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython2.7 (&optional dedicated fast split switch proc)
  "Send buffer at point to IPython interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython2.7 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython2.7-switch (&optional dedicated fast split proc)
  "Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython2.7 dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython2.7 dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython2.7-dedicated (&optional fast split switch proc)
  "Send buffer at point to IPython unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython2.7 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython2.7 t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3 (&optional dedicated fast split switch proc)
  "Send buffer at point to IPython interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3-switch (&optional dedicated fast split proc)
  "Send buffer at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3-dedicated (&optional fast split switch proc)
  "Send buffer at point to IPython unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'ipython3 t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython (&optional dedicated fast split switch proc)
  "Send buffer at point to Jython interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython-switch (&optional dedicated fast split proc)
  "Send buffer at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython-dedicated (&optional fast split switch proc)
  "Send buffer at point to Jython unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-jython-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'jython t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python (&optional dedicated fast split switch proc)
  "Send buffer at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python-switch (&optional dedicated fast split proc)
  "Send buffer at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python-dedicated (&optional fast split switch proc)
  "Send buffer at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2 (&optional dedicated fast split switch proc)
  "Send buffer at point to Python2 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2-switch (&optional dedicated fast split proc)
  "Send buffer at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2-dedicated (&optional fast split switch proc)
  "Send buffer at point to Python2 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python2-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python2 t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3 (&optional dedicated fast split switch proc)
  "Send buffer at point to Python3 interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 dedicated switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3-switch (&optional dedicated fast split proc)
  "Send buffer at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 dedicated 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3-no-switch (&optional dedicated fast split  proc)
  "Send buffer at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 dedicated 'no-switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3-dedicated (&optional fast split switch proc)
  "Send buffer at point to Python3 unique interpreter."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 t switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-buffer-python3-dedicated-switch (&optional  fast split  proc)
  "Send buffer at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))
    (py--execute-prepare 'buffer 'python3 t 'switch (point-min) (point-max) nil fast proc wholebuf split)))

(defun py-execute-class (&optional shell dedicated fast split switch proc)
  "Send class at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-switch (&optional shell dedicated fast split proc)
  "Send class at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-no-switch (&optional shell dedicated fast split  proc)
  "Send class at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-dedicated (&optional shell fast split switch proc)
  "Send class at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-dedicated-switch (&optional shell  fast split  proc)
  "Send class at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython (&optional dedicated fast split switch proc)
  "Send class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython-switch (&optional dedicated fast split proc)
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython-no-switch (&optional dedicated fast split  proc)
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython-dedicated (&optional fast split switch proc)
  "Send class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython-dedicated-switch (&optional  fast split  proc)
  "Send class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython2.7 (&optional dedicated fast split switch proc)
  "Send class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython2.7-switch (&optional dedicated fast split proc)
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython2.7-dedicated (&optional fast split switch proc)
  "Send class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3 (&optional dedicated fast split switch proc)
  "Send class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3-switch (&optional dedicated fast split proc)
  "Send class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3-dedicated (&optional fast split switch proc)
  "Send class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython (&optional dedicated fast split switch proc)
  "Send class at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython-switch (&optional dedicated fast split proc)
  "Send class at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython-no-switch (&optional dedicated fast split  proc)
  "Send class at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython-dedicated (&optional fast split switch proc)
  "Send class at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-jython-dedicated-switch (&optional  fast split  proc)
  "Send class at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python (&optional dedicated fast split switch proc)
  "Send class at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python-switch (&optional dedicated fast split proc)
  "Send class at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python-no-switch (&optional dedicated fast split  proc)
  "Send class at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python-dedicated (&optional fast split switch proc)
  "Send class at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python-dedicated-switch (&optional  fast split  proc)
  "Send class at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2 (&optional dedicated fast split switch proc)
  "Send class at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2-switch (&optional dedicated fast split proc)
  "Send class at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2-no-switch (&optional dedicated fast split  proc)
  "Send class at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2-dedicated (&optional fast split switch proc)
  "Send class at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python2-dedicated-switch (&optional  fast split  proc)
  "Send class at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3 (&optional dedicated fast split switch proc)
  "Send class at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3-switch (&optional dedicated fast split proc)
  "Send class at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3-no-switch (&optional dedicated fast split  proc)
  "Send class at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3-dedicated (&optional fast split switch proc)
  "Send class at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-class-python3-dedicated-switch (&optional  fast split  proc)
  "Send class at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'class 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause (&optional shell dedicated fast split switch proc)
  "Send clause at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-switch (&optional shell dedicated fast split proc)
  "Send clause at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-no-switch (&optional shell dedicated fast split  proc)
  "Send clause at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-dedicated (&optional shell fast split switch proc)
  "Send clause at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-dedicated-switch (&optional shell  fast split  proc)
  "Send clause at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython (&optional dedicated fast split switch proc)
  "Send clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython-switch (&optional dedicated fast split proc)
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython-dedicated (&optional fast split switch proc)
  "Send clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython2.7 (&optional dedicated fast split switch proc)
  "Send clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython2.7-switch (&optional dedicated fast split proc)
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython2.7-dedicated (&optional fast split switch proc)
  "Send clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3 (&optional dedicated fast split switch proc)
  "Send clause at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3-switch (&optional dedicated fast split proc)
  "Send clause at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3-dedicated (&optional fast split switch proc)
  "Send clause at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython (&optional dedicated fast split switch proc)
  "Send clause at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython-switch (&optional dedicated fast split proc)
  "Send clause at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython-dedicated (&optional fast split switch proc)
  "Send clause at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-jython-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python (&optional dedicated fast split switch proc)
  "Send clause at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python-switch (&optional dedicated fast split proc)
  "Send clause at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python-dedicated (&optional fast split switch proc)
  "Send clause at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2 (&optional dedicated fast split switch proc)
  "Send clause at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2-switch (&optional dedicated fast split proc)
  "Send clause at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2-dedicated (&optional fast split switch proc)
  "Send clause at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python2-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3 (&optional dedicated fast split switch proc)
  "Send clause at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3-switch (&optional dedicated fast split proc)
  "Send clause at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3-no-switch (&optional dedicated fast split  proc)
  "Send clause at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3-dedicated (&optional fast split switch proc)
  "Send clause at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-clause-python3-dedicated-switch (&optional  fast split  proc)
  "Send clause at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'clause 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def (&optional shell dedicated fast split switch proc)
  "Send def at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-switch (&optional shell dedicated fast split proc)
  "Send def at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-no-switch (&optional shell dedicated fast split  proc)
  "Send def at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-dedicated (&optional shell fast split switch proc)
  "Send def at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-dedicated-switch (&optional shell  fast split  proc)
  "Send def at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython (&optional dedicated fast split switch proc)
  "Send def at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython-switch (&optional dedicated fast split proc)
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython-no-switch (&optional dedicated fast split  proc)
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython-dedicated (&optional fast split switch proc)
  "Send def at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython-dedicated-switch (&optional  fast split  proc)
  "Send def at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython2.7 (&optional dedicated fast split switch proc)
  "Send def at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython2.7-switch (&optional dedicated fast split proc)
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython2.7-dedicated (&optional fast split switch proc)
  "Send def at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send def at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3 (&optional dedicated fast split switch proc)
  "Send def at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3-switch (&optional dedicated fast split proc)
  "Send def at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send def at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3-dedicated (&optional fast split switch proc)
  "Send def at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send def at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython (&optional dedicated fast split switch proc)
  "Send def at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython-switch (&optional dedicated fast split proc)
  "Send def at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython-no-switch (&optional dedicated fast split  proc)
  "Send def at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython-dedicated (&optional fast split switch proc)
  "Send def at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-jython-dedicated-switch (&optional  fast split  proc)
  "Send def at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python (&optional dedicated fast split switch proc)
  "Send def at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python-switch (&optional dedicated fast split proc)
  "Send def at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python-no-switch (&optional dedicated fast split  proc)
  "Send def at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python-dedicated (&optional fast split switch proc)
  "Send def at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python-dedicated-switch (&optional  fast split  proc)
  "Send def at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2 (&optional dedicated fast split switch proc)
  "Send def at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2-switch (&optional dedicated fast split proc)
  "Send def at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2-no-switch (&optional dedicated fast split  proc)
  "Send def at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2-dedicated (&optional fast split switch proc)
  "Send def at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python2-dedicated-switch (&optional  fast split  proc)
  "Send def at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3 (&optional dedicated fast split switch proc)
  "Send def at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3-switch (&optional dedicated fast split proc)
  "Send def at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3-no-switch (&optional dedicated fast split  proc)
  "Send def at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3-dedicated (&optional fast split switch proc)
  "Send def at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-python3-dedicated-switch (&optional  fast split  proc)
  "Send def at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class (&optional shell dedicated fast split switch proc)
  "Send def-or-class at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-switch (&optional shell dedicated fast split proc)
  "Send def-or-class at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-no-switch (&optional shell dedicated fast split  proc)
  "Send def-or-class at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-dedicated (&optional shell fast split switch proc)
  "Send def-or-class at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-dedicated-switch (&optional shell  fast split  proc)
  "Send def-or-class at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython (&optional dedicated fast split switch proc)
  "Send def-or-class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython2.7 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython2.7-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython2.7-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython (&optional dedicated fast split switch proc)
  "Send def-or-class at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-jython-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python (&optional dedicated fast split switch proc)
  "Send def-or-class at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python2-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3 (&optional dedicated fast split switch proc)
  "Send def-or-class at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3-switch (&optional dedicated fast split proc)
  "Send def-or-class at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3-no-switch (&optional dedicated fast split  proc)
  "Send def-or-class at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3-dedicated (&optional fast split switch proc)
  "Send def-or-class at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-def-or-class-python3-dedicated-switch (&optional  fast split  proc)
  "Send def-or-class at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'def-or-class 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression (&optional shell dedicated fast split switch proc)
  "Send expression at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-switch (&optional shell dedicated fast split proc)
  "Send expression at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-no-switch (&optional shell dedicated fast split  proc)
  "Send expression at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-dedicated (&optional shell fast split switch proc)
  "Send expression at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-dedicated-switch (&optional shell  fast split  proc)
  "Send expression at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython (&optional dedicated fast split switch proc)
  "Send expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython-switch (&optional dedicated fast split proc)
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython-dedicated (&optional fast split switch proc)
  "Send expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython2.7 (&optional dedicated fast split switch proc)
  "Send expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython2.7-switch (&optional dedicated fast split proc)
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython2.7-dedicated (&optional fast split switch proc)
  "Send expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3 (&optional dedicated fast split switch proc)
  "Send expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3-switch (&optional dedicated fast split proc)
  "Send expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3-dedicated (&optional fast split switch proc)
  "Send expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython (&optional dedicated fast split switch proc)
  "Send expression at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython-switch (&optional dedicated fast split proc)
  "Send expression at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython-dedicated (&optional fast split switch proc)
  "Send expression at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-jython-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python (&optional dedicated fast split switch proc)
  "Send expression at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python-switch (&optional dedicated fast split proc)
  "Send expression at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python-dedicated (&optional fast split switch proc)
  "Send expression at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2 (&optional dedicated fast split switch proc)
  "Send expression at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2-switch (&optional dedicated fast split proc)
  "Send expression at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2-dedicated (&optional fast split switch proc)
  "Send expression at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python2-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3 (&optional dedicated fast split switch proc)
  "Send expression at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3-switch (&optional dedicated fast split proc)
  "Send expression at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3-no-switch (&optional dedicated fast split  proc)
  "Send expression at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3-dedicated (&optional fast split switch proc)
  "Send expression at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-expression-python3-dedicated-switch (&optional  fast split  proc)
  "Send expression at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'expression 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent (&optional shell dedicated fast split switch proc)
  "Send indent at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-switch (&optional shell dedicated fast split proc)
  "Send indent at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-no-switch (&optional shell dedicated fast split  proc)
  "Send indent at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-dedicated (&optional shell fast split switch proc)
  "Send indent at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-dedicated-switch (&optional shell  fast split  proc)
  "Send indent at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython (&optional dedicated fast split switch proc)
  "Send indent at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython-switch (&optional dedicated fast split proc)
  "Send indent at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython-dedicated (&optional fast split switch proc)
  "Send indent at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython2.7 (&optional dedicated fast split switch proc)
  "Send indent at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython2.7-switch (&optional dedicated fast split proc)
  "Send indent at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython2.7-dedicated (&optional fast split switch proc)
  "Send indent at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3 (&optional dedicated fast split switch proc)
  "Send indent at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3-switch (&optional dedicated fast split proc)
  "Send indent at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3-dedicated (&optional fast split switch proc)
  "Send indent at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython (&optional dedicated fast split switch proc)
  "Send indent at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython-switch (&optional dedicated fast split proc)
  "Send indent at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython-dedicated (&optional fast split switch proc)
  "Send indent at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-jython-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python (&optional dedicated fast split switch proc)
  "Send indent at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python-switch (&optional dedicated fast split proc)
  "Send indent at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python-dedicated (&optional fast split switch proc)
  "Send indent at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2 (&optional dedicated fast split switch proc)
  "Send indent at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2-switch (&optional dedicated fast split proc)
  "Send indent at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2-dedicated (&optional fast split switch proc)
  "Send indent at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python2-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3 (&optional dedicated fast split switch proc)
  "Send indent at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3-switch (&optional dedicated fast split proc)
  "Send indent at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3-no-switch (&optional dedicated fast split  proc)
  "Send indent at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3-dedicated (&optional fast split switch proc)
  "Send indent at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-indent-python3-dedicated-switch (&optional  fast split  proc)
  "Send indent at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'indent 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line (&optional shell dedicated fast split switch proc)
  "Send line at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-switch (&optional shell dedicated fast split proc)
  "Send line at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-no-switch (&optional shell dedicated fast split  proc)
  "Send line at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-dedicated (&optional shell fast split switch proc)
  "Send line at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-dedicated-switch (&optional shell  fast split  proc)
  "Send line at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython (&optional dedicated fast split switch proc)
  "Send line at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython-switch (&optional dedicated fast split proc)
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython-no-switch (&optional dedicated fast split  proc)
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython-dedicated (&optional fast split switch proc)
  "Send line at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython-dedicated-switch (&optional  fast split  proc)
  "Send line at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython2.7 (&optional dedicated fast split switch proc)
  "Send line at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython2.7-switch (&optional dedicated fast split proc)
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython2.7-dedicated (&optional fast split switch proc)
  "Send line at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send line at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3 (&optional dedicated fast split switch proc)
  "Send line at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3-switch (&optional dedicated fast split proc)
  "Send line at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send line at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3-dedicated (&optional fast split switch proc)
  "Send line at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send line at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython (&optional dedicated fast split switch proc)
  "Send line at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython-switch (&optional dedicated fast split proc)
  "Send line at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython-no-switch (&optional dedicated fast split  proc)
  "Send line at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython-dedicated (&optional fast split switch proc)
  "Send line at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-jython-dedicated-switch (&optional  fast split  proc)
  "Send line at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python (&optional dedicated fast split switch proc)
  "Send line at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python-switch (&optional dedicated fast split proc)
  "Send line at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python-no-switch (&optional dedicated fast split  proc)
  "Send line at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python-dedicated (&optional fast split switch proc)
  "Send line at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python-dedicated-switch (&optional  fast split  proc)
  "Send line at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2 (&optional dedicated fast split switch proc)
  "Send line at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2-switch (&optional dedicated fast split proc)
  "Send line at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2-no-switch (&optional dedicated fast split  proc)
  "Send line at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2-dedicated (&optional fast split switch proc)
  "Send line at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python2-dedicated-switch (&optional  fast split  proc)
  "Send line at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3 (&optional dedicated fast split switch proc)
  "Send line at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3-switch (&optional dedicated fast split proc)
  "Send line at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3-no-switch (&optional dedicated fast split  proc)
  "Send line at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3-dedicated (&optional fast split switch proc)
  "Send line at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-line-python3-dedicated-switch (&optional  fast split  proc)
  "Send line at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'line 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block (&optional shell dedicated fast split switch proc)
  "Send minor-block at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-switch (&optional shell dedicated fast split proc)
  "Send minor-block at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-no-switch (&optional shell dedicated fast split  proc)
  "Send minor-block at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-dedicated (&optional shell fast split switch proc)
  "Send minor-block at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-dedicated-switch (&optional shell  fast split  proc)
  "Send minor-block at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython (&optional dedicated fast split switch proc)
  "Send minor-block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython-switch (&optional dedicated fast split proc)
  "Send minor-block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython-dedicated (&optional fast split switch proc)
  "Send minor-block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython2.7 (&optional dedicated fast split switch proc)
  "Send minor-block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython2.7-switch (&optional dedicated fast split proc)
  "Send minor-block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython2.7-dedicated (&optional fast split switch proc)
  "Send minor-block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3 (&optional dedicated fast split switch proc)
  "Send minor-block at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3-switch (&optional dedicated fast split proc)
  "Send minor-block at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3-dedicated (&optional fast split switch proc)
  "Send minor-block at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython (&optional dedicated fast split switch proc)
  "Send minor-block at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython-switch (&optional dedicated fast split proc)
  "Send minor-block at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython-dedicated (&optional fast split switch proc)
  "Send minor-block at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-jython-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python (&optional dedicated fast split switch proc)
  "Send minor-block at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python-switch (&optional dedicated fast split proc)
  "Send minor-block at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python-dedicated (&optional fast split switch proc)
  "Send minor-block at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2 (&optional dedicated fast split switch proc)
  "Send minor-block at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2-switch (&optional dedicated fast split proc)
  "Send minor-block at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2-dedicated (&optional fast split switch proc)
  "Send minor-block at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python2-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3 (&optional dedicated fast split switch proc)
  "Send minor-block at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3-switch (&optional dedicated fast split proc)
  "Send minor-block at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3-no-switch (&optional dedicated fast split  proc)
  "Send minor-block at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3-dedicated (&optional fast split switch proc)
  "Send minor-block at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-minor-block-python3-dedicated-switch (&optional  fast split  proc)
  "Send minor-block at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'minor-block 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph (&optional shell dedicated fast split switch proc)
  "Send paragraph at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-switch (&optional shell dedicated fast split proc)
  "Send paragraph at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-no-switch (&optional shell dedicated fast split  proc)
  "Send paragraph at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-dedicated (&optional shell fast split switch proc)
  "Send paragraph at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-dedicated-switch (&optional shell  fast split  proc)
  "Send paragraph at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython (&optional dedicated fast split switch proc)
  "Send paragraph at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython-switch (&optional dedicated fast split proc)
  "Send paragraph at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython-dedicated (&optional fast split switch proc)
  "Send paragraph at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython2.7 (&optional dedicated fast split switch proc)
  "Send paragraph at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython2.7-switch (&optional dedicated fast split proc)
  "Send paragraph at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython2.7-dedicated (&optional fast split switch proc)
  "Send paragraph at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3 (&optional dedicated fast split switch proc)
  "Send paragraph at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3-switch (&optional dedicated fast split proc)
  "Send paragraph at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3-dedicated (&optional fast split switch proc)
  "Send paragraph at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython (&optional dedicated fast split switch proc)
  "Send paragraph at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython-switch (&optional dedicated fast split proc)
  "Send paragraph at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython-dedicated (&optional fast split switch proc)
  "Send paragraph at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-jython-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python (&optional dedicated fast split switch proc)
  "Send paragraph at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python-switch (&optional dedicated fast split proc)
  "Send paragraph at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python-dedicated (&optional fast split switch proc)
  "Send paragraph at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2 (&optional dedicated fast split switch proc)
  "Send paragraph at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2-switch (&optional dedicated fast split proc)
  "Send paragraph at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2-dedicated (&optional fast split switch proc)
  "Send paragraph at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python2-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3 (&optional dedicated fast split switch proc)
  "Send paragraph at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3-switch (&optional dedicated fast split proc)
  "Send paragraph at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3-no-switch (&optional dedicated fast split  proc)
  "Send paragraph at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3-dedicated (&optional fast split switch proc)
  "Send paragraph at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-paragraph-python3-dedicated-switch (&optional  fast split  proc)
  "Send paragraph at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'paragraph 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression (&optional shell dedicated fast split switch proc)
  "Send partial-expression at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-switch (&optional shell dedicated fast split proc)
  "Send partial-expression at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-no-switch (&optional shell dedicated fast split  proc)
  "Send partial-expression at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-dedicated (&optional shell fast split switch proc)
  "Send partial-expression at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-dedicated-switch (&optional shell  fast split  proc)
  "Send partial-expression at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython (&optional dedicated fast split switch proc)
  "Send partial-expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython2.7 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython2.7-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython2.7-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython (&optional dedicated fast split switch proc)
  "Send partial-expression at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-jython-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python (&optional dedicated fast split switch proc)
  "Send partial-expression at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python2-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3 (&optional dedicated fast split switch proc)
  "Send partial-expression at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3-switch (&optional dedicated fast split proc)
  "Send partial-expression at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3-no-switch (&optional dedicated fast split  proc)
  "Send partial-expression at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3-dedicated (&optional fast split switch proc)
  "Send partial-expression at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-partial-expression-python3-dedicated-switch (&optional  fast split  proc)
  "Send partial-expression at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'partial-expression 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-region (beg end &optional shell dedicated fast split switch proc)
  "Send region at point to  interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-switch (beg end &optional shell dedicated fast split proc)
  "Send region at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-no-switch (beg end &optional shell dedicated fast split  proc)
  "Send region at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-dedicated (beg end &optional shell fast split switch proc)
  "Send region at point to  unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-dedicated-switch (beg end &optional shell  fast split  proc)
  "Send region at point to  unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region shell t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython (beg end &optional dedicated fast split switch proc)
  "Send region at point to IPython interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython-switch (beg end &optional dedicated fast split proc)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython-dedicated (beg end &optional fast split switch proc)
  "Send region at point to IPython unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to IPython unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython2.7 (beg end &optional dedicated fast split switch proc)
  "Send region at point to IPython interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython2.7 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython2.7-switch (beg end &optional dedicated fast split proc)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython2.7 dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython2.7-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython2.7 dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython2.7-dedicated (beg end &optional fast split switch proc)
  "Send region at point to IPython unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython2.7 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython2.7-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to IPython unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython2.7 t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3 (beg end &optional dedicated fast split switch proc)
  "Send region at point to IPython interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3-switch (beg end &optional dedicated fast split proc)
  "Send region at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3-dedicated (beg end &optional fast split switch proc)
  "Send region at point to IPython unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-ipython3-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to IPython unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'ipython3 t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython (beg end &optional dedicated fast split switch proc)
  "Send region at point to Jython interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython-switch (beg end &optional dedicated fast split proc)
  "Send region at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython-dedicated (beg end &optional fast split switch proc)
  "Send region at point to Jython unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-jython-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to Jython unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'jython t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python (beg end &optional dedicated fast split switch proc)
  "Send region at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python-switch (beg end &optional dedicated fast split proc)
  "Send region at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python-dedicated (beg end &optional fast split switch proc)
  "Send region at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2 (beg end &optional dedicated fast split switch proc)
  "Send region at point to Python2 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2-switch (beg end &optional dedicated fast split proc)
  "Send region at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2-dedicated (beg end &optional fast split switch proc)
  "Send region at point to Python2 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python2-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to Python2 unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python2 t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3 (beg end &optional dedicated fast split switch proc)
  "Send region at point to Python3 interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 dedicated switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3-switch (beg end &optional dedicated fast split proc)
  "Send region at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 dedicated 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3-no-switch (beg end &optional dedicated fast split  proc)
  "Send region at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 dedicated 'no-switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3-dedicated (beg end &optional fast split switch proc)
  "Send region at point to Python3 unique interpreter."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 t switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-region-python3-dedicated-switch (beg end &optional  fast split  proc)
  "Send region at point to Python3 unique interpreter.
Switch to result."
  (interactive "r")
  (let ((wholebuf nil))
    (py--execute-prepare 'region 'python3 t 'switch (or beg (region-beginning)) (or end (region-end)) nil fast proc wholebuf split)))

(defun py-execute-statement (&optional shell dedicated fast split switch proc)
  "Send statement at point to interpreter."
  (interactive)
  (let ((wholebuf nil))
    ;; (macroexpand
    (py--execute-prepare 'statement shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-switch (&optional shell dedicated fast split proc)
  "Send statement at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-no-switch (&optional shell dedicated fast split  proc)
  "Send statement at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-dedicated (&optional shell fast split switch proc)
  "Send statement at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-dedicated-switch (&optional shell  fast split  proc)
  "Send statement at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython (&optional dedicated fast split switch proc)
  "Send statement at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython-switch (&optional dedicated fast split proc)
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython-dedicated (&optional fast split switch proc)
  "Send statement at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython2.7 (&optional dedicated fast split switch proc)
  "Send statement at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython2.7-switch (&optional dedicated fast split proc)
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython2.7-dedicated (&optional fast split switch proc)
  "Send statement at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3 (&optional dedicated fast split switch proc)
  "Send statement at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3-switch (&optional dedicated fast split proc)
  "Send statement at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3-dedicated (&optional fast split switch proc)
  "Send statement at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython (&optional dedicated fast split switch proc)
  "Send statement at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython-switch (&optional dedicated fast split proc)
  "Send statement at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython-dedicated (&optional fast split switch proc)
  "Send statement at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-jython-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python (&optional dedicated fast split switch proc)
  "Send statement at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python-switch (&optional dedicated fast split proc)
  "Send statement at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python-dedicated (&optional fast split switch proc)
  "Send statement at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2 (&optional dedicated fast split switch proc)
  "Send statement at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2-switch (&optional dedicated fast split proc)
  "Send statement at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2-dedicated (&optional fast split switch proc)
  "Send statement at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python2-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3 (&optional dedicated fast split switch proc)
  "Send statement at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3-switch (&optional dedicated fast split proc)
  "Send statement at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3-no-switch (&optional dedicated fast split  proc)
  "Send statement at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3-dedicated (&optional fast split switch proc)
  "Send statement at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-statement-python3-dedicated-switch (&optional  fast split  proc)
  "Send statement at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'statement 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level (&optional shell dedicated fast split switch proc)
  "Send top-level at point to  interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-switch (&optional shell dedicated fast split proc)
  "Send top-level at point to  interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-no-switch (&optional shell dedicated fast split  proc)
  "Send top-level at point to  interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-dedicated (&optional shell fast split switch proc)
  "Send top-level at point to  unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-dedicated-switch (&optional shell  fast split  proc)
  "Send top-level at point to  unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level shell t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython (&optional dedicated fast split switch proc)
  "Send top-level at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython-switch (&optional dedicated fast split proc)
  "Send top-level at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython-dedicated (&optional fast split switch proc)
  "Send top-level at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython2.7 (&optional dedicated fast split switch proc)
  "Send top-level at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython2.7 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython2.7-switch (&optional dedicated fast split proc)
  "Send top-level at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython2.7 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython2.7-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython2.7 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython2.7-dedicated (&optional fast split switch proc)
  "Send top-level at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython2.7 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython2.7-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython2.7 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3 (&optional dedicated fast split switch proc)
  "Send top-level at point to IPython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3-switch (&optional dedicated fast split proc)
  "Send top-level at point to IPython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to IPython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3-dedicated (&optional fast split switch proc)
  "Send top-level at point to IPython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-ipython3-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to IPython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'ipython3 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython (&optional dedicated fast split switch proc)
  "Send top-level at point to Jython interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython-switch (&optional dedicated fast split proc)
  "Send top-level at point to Jython interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to Jython interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython-dedicated (&optional fast split switch proc)
  "Send top-level at point to Jython unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-jython-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to Jython unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'jython t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python (&optional dedicated fast split switch proc)
  "Send top-level at point to default interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python-switch (&optional dedicated fast split proc)
  "Send top-level at point to default interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to default interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python-dedicated (&optional fast split switch proc)
  "Send top-level at point to default unique interpreter.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to default unique interpreter.
Switch to result.

For ‘default’ see value of ‘py-shell-name’"
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2 (&optional dedicated fast split switch proc)
  "Send top-level at point to Python2 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2-switch (&optional dedicated fast split proc)
  "Send top-level at point to Python2 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to Python2 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2-dedicated (&optional fast split switch proc)
  "Send top-level at point to Python2 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python2-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to Python2 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python2 t 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3 (&optional dedicated fast split switch proc)
  "Send top-level at point to Python3 interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 dedicated switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3-switch (&optional dedicated fast split proc)
  "Send top-level at point to Python3 interpreter.

Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 dedicated 'switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3-no-switch (&optional dedicated fast split  proc)
  "Send top-level at point to Python3 interpreter.

Keep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 dedicated 'no-switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3-dedicated (&optional fast split switch proc)
  "Send top-level at point to Python3 unique interpreter."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 t switch nil nil nil fast proc wholebuf split)))

(defun py-execute-top-level-python3-dedicated-switch (&optional  fast split  proc)
  "Send top-level at point to Python3 unique interpreter.
Switch to result."
  (interactive)
  (let ((wholebuf nil))
    (py--execute-prepare 'top-level 'python3 t 'switch nil nil nil fast proc wholebuf split)))

(provide 'python-components-extended-executes)
;;; python-components-extended-executes.el ends here
