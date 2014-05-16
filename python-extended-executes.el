;;; Extended executes --- more execute forms
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


;; created by `write-unified-extended-execute-forms'
(defun py-masterfile ()
  "Internal use. Set master-file, if given. "
  (and (or py-master-file (py-fetch-py-master-file))
       (let* ((filename (expand-file-name py-master-file))
              (buffer (or (get-file-buffer filename)
                          (find-file-noselect filename))))
         (set-buffer buffer))))

(defun py-execute-prepare (form &optional shell dedicated switch beg end file)
  "Used by python-extended-executes ."
  (save-excursion
    (let* ((beg (unless file
		  (prog1
		      (or beg (funcall (intern-soft (concat "py-beginning-of-" form "-p")))

			  (funcall (intern-soft (concat "py-beginning-of-" form)))
			  (push-mark)))))
	   (end (unless file
		  (or end (funcall (intern-soft (concat "py-end-of-" form))))))
	   (shell (or shell py-shell-name))
	   (py-dedicated-process-p dedicated)
	   (py-switch-buffers-on-execute-p (cond ((eq 'switch switch)
						  t)
						 ((eq 'no-switch switch)
						  nil)
						 (t py-switch-buffers-on-execute-p)))
	   filename)
      (if file
          (progn
            (setq filename (expand-file-name form))
            (if (file-readable-p filename)
                (setq erg (py-execute-file-base nil filename nil nil (or (and (boundp 'py-orig-buffer-or-file) py-orig-buffer-or-file) filename)))
              (message "%s not readable. %s" file "Do you have write permissions?")))
        (py-execute-base beg end shell)))))

(defun py-execute-statement-python ()
  "Send statement at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "statement" 'py-python-command nil nil))

(defun py-execute-statement-python-switch ()
  "Send statement at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "statement" 'py-python-command nil 'switch))

(defun py-execute-statement-python-no-switch ()
  "Send statement at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "statement" 'py-python-command nil 'no-switch))

(defun py-execute-statement-python-dedicated ()
  "Send statement at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "statement" 'py-python-command t nil))

(defun py-execute-statement-python-dedicated-switch ()
  "Send statement at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "statement" 'py-python-command t 'switch))

(defun py-execute-statement-ipython ()
  "Send statement at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-ipython-command nil nil))

(defun py-execute-statement-ipython-switch ()
  "Send statement at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" 'py-ipython-command nil 'switch))

(defun py-execute-statement-ipython-no-switch ()
  "Send statement at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" 'py-ipython-command nil 'no-switch))

(defun py-execute-statement-ipython-dedicated ()
  "Send statement at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-ipython-command t nil))

(defun py-execute-statement-ipython-dedicated-switch ()
  "Send statement at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" 'py-ipython-command t 'switch))

(defun py-execute-statement-python3 ()
  "Send statement at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-python3-command nil nil))

(defun py-execute-statement-python3-switch ()
  "Send statement at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" 'py-python3-command nil 'switch))

(defun py-execute-statement-python3-no-switch ()
  "Send statement at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" 'py-python3-command nil 'no-switch))

(defun py-execute-statement-python3-dedicated ()
  "Send statement at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-python3-command t nil))

(defun py-execute-statement-python3-dedicated-switch ()
  "Send statement at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" 'py-python3-command t 'switch))

(defun py-execute-statement-python2 ()
  "Send statement at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-python2-command nil nil))

(defun py-execute-statement-python2-switch ()
  "Send statement at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" 'py-python2-command nil 'switch))

(defun py-execute-statement-python2-no-switch ()
  "Send statement at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" 'py-python2-command nil 'no-switch))

(defun py-execute-statement-python2-dedicated ()
  "Send statement at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-python2-command t nil))

(defun py-execute-statement-python2-dedicated-switch ()
  "Send statement at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" 'py-python2-command t 'switch))

(defun py-execute-statement-jython ()
  "Send statement at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-jython-command nil nil))

(defun py-execute-statement-jython-switch ()
  "Send statement at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" 'py-jython-command nil 'switch))

(defun py-execute-statement-jython-no-switch ()
  "Send statement at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" 'py-jython-command nil 'no-switch))

(defun py-execute-statement-jython-dedicated ()
  "Send statement at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-jython-command t nil))

(defun py-execute-statement-jython-dedicated-switch ()
  "Send statement at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" 'py-jython-command t 'switch))

(defun py-execute-statement-bpython ()
  "Send statement at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-bpython-command nil nil))

(defun py-execute-statement-bpython-switch ()
  "Send statement at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "statement" 'py-bpython-command nil 'switch))

(defun py-execute-statement-bpython-no-switch ()
  "Send statement at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "statement" 'py-bpython-command nil 'no-switch))

(defun py-execute-statement-bpython-dedicated ()
  "Send statement at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "statement" 'py-bpython-command t nil))

(defun py-execute-statement-bpython-dedicated-switch ()
  "Send statement at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "statement" 'py-bpython-command t 'switch))

(defun py-execute-block-python ()
  "Send block at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block" 'py-python-command nil nil))

(defun py-execute-block-python-switch ()
  "Send block at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block" 'py-python-command nil 'switch))

(defun py-execute-block-python-no-switch ()
  "Send block at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block" 'py-python-command nil 'no-switch))

(defun py-execute-block-python-dedicated ()
  "Send block at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block" 'py-python-command t nil))

(defun py-execute-block-python-dedicated-switch ()
  "Send block at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block" 'py-python-command t 'switch))

(defun py-execute-block-ipython ()
  "Send block at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-ipython-command nil nil))

(defun py-execute-block-ipython-switch ()
  "Send block at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" 'py-ipython-command nil 'switch))

(defun py-execute-block-ipython-no-switch ()
  "Send block at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" 'py-ipython-command nil 'no-switch))

(defun py-execute-block-ipython-dedicated ()
  "Send block at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-ipython-command t nil))

(defun py-execute-block-ipython-dedicated-switch ()
  "Send block at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" 'py-ipython-command t 'switch))

(defun py-execute-block-python3 ()
  "Send block at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-python3-command nil nil))

(defun py-execute-block-python3-switch ()
  "Send block at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" 'py-python3-command nil 'switch))

(defun py-execute-block-python3-no-switch ()
  "Send block at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" 'py-python3-command nil 'no-switch))

(defun py-execute-block-python3-dedicated ()
  "Send block at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-python3-command t nil))

(defun py-execute-block-python3-dedicated-switch ()
  "Send block at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" 'py-python3-command t 'switch))

(defun py-execute-block-python2 ()
  "Send block at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-python2-command nil nil))

(defun py-execute-block-python2-switch ()
  "Send block at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" 'py-python2-command nil 'switch))

(defun py-execute-block-python2-no-switch ()
  "Send block at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" 'py-python2-command nil 'no-switch))

(defun py-execute-block-python2-dedicated ()
  "Send block at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-python2-command t nil))

(defun py-execute-block-python2-dedicated-switch ()
  "Send block at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" 'py-python2-command t 'switch))

(defun py-execute-block-jython ()
  "Send block at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-jython-command nil nil))

(defun py-execute-block-jython-switch ()
  "Send block at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" 'py-jython-command nil 'switch))

(defun py-execute-block-jython-no-switch ()
  "Send block at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" 'py-jython-command nil 'no-switch))

(defun py-execute-block-jython-dedicated ()
  "Send block at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-jython-command t nil))

(defun py-execute-block-jython-dedicated-switch ()
  "Send block at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" 'py-jython-command t 'switch))

(defun py-execute-block-bpython ()
  "Send block at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-bpython-command nil nil))

(defun py-execute-block-bpython-switch ()
  "Send block at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block" 'py-bpython-command nil 'switch))

(defun py-execute-block-bpython-no-switch ()
  "Send block at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block" 'py-bpython-command nil 'no-switch))

(defun py-execute-block-bpython-dedicated ()
  "Send block at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "block" 'py-bpython-command t nil))

(defun py-execute-block-bpython-dedicated-switch ()
  "Send block at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block" 'py-bpython-command t 'switch))

(defun py-execute-clause-python ()
  "Send clause at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "clause" 'py-python-command nil nil))

(defun py-execute-clause-python-switch ()
  "Send clause at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "clause" 'py-python-command nil 'switch))

(defun py-execute-clause-python-no-switch ()
  "Send clause at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "clause" 'py-python-command nil 'no-switch))

(defun py-execute-clause-python-dedicated ()
  "Send clause at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "clause" 'py-python-command t nil))

(defun py-execute-clause-python-dedicated-switch ()
  "Send clause at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "clause" 'py-python-command t 'switch))

(defun py-execute-clause-ipython ()
  "Send clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-ipython-command nil nil))

(defun py-execute-clause-ipython-switch ()
  "Send clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" 'py-ipython-command nil 'switch))

(defun py-execute-clause-ipython-no-switch ()
  "Send clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" 'py-ipython-command nil 'no-switch))

(defun py-execute-clause-ipython-dedicated ()
  "Send clause at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-ipython-command t nil))

(defun py-execute-clause-ipython-dedicated-switch ()
  "Send clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" 'py-ipython-command t 'switch))

(defun py-execute-clause-python3 ()
  "Send clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-python3-command nil nil))

(defun py-execute-clause-python3-switch ()
  "Send clause at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" 'py-python3-command nil 'switch))

(defun py-execute-clause-python3-no-switch ()
  "Send clause at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" 'py-python3-command nil 'no-switch))

(defun py-execute-clause-python3-dedicated ()
  "Send clause at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-python3-command t nil))

(defun py-execute-clause-python3-dedicated-switch ()
  "Send clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" 'py-python3-command t 'switch))

(defun py-execute-clause-python2 ()
  "Send clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-python2-command nil nil))

(defun py-execute-clause-python2-switch ()
  "Send clause at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" 'py-python2-command nil 'switch))

(defun py-execute-clause-python2-no-switch ()
  "Send clause at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" 'py-python2-command nil 'no-switch))

(defun py-execute-clause-python2-dedicated ()
  "Send clause at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-python2-command t nil))

(defun py-execute-clause-python2-dedicated-switch ()
  "Send clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" 'py-python2-command t 'switch))

(defun py-execute-clause-jython ()
  "Send clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-jython-command nil nil))

(defun py-execute-clause-jython-switch ()
  "Send clause at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" 'py-jython-command nil 'switch))

(defun py-execute-clause-jython-no-switch ()
  "Send clause at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" 'py-jython-command nil 'no-switch))

(defun py-execute-clause-jython-dedicated ()
  "Send clause at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-jython-command t nil))

(defun py-execute-clause-jython-dedicated-switch ()
  "Send clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" 'py-jython-command t 'switch))

(defun py-execute-clause-bpython ()
  "Send clause at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-bpython-command nil nil))

(defun py-execute-clause-bpython-switch ()
  "Send clause at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "clause" 'py-bpython-command nil 'switch))

(defun py-execute-clause-bpython-no-switch ()
  "Send clause at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "clause" 'py-bpython-command nil 'no-switch))

(defun py-execute-clause-bpython-dedicated ()
  "Send clause at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "clause" 'py-bpython-command t nil))

(defun py-execute-clause-bpython-dedicated-switch ()
  "Send clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "clause" 'py-bpython-command t 'switch))

(defun py-execute-block-or-clause-python ()
  "Send block-or-clause at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python-command nil nil))

(defun py-execute-block-or-clause-python-switch ()
  "Send block-or-clause at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python-command nil 'switch))

(defun py-execute-block-or-clause-python-no-switch ()
  "Send block-or-clause at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python-command nil 'no-switch))

(defun py-execute-block-or-clause-python-dedicated ()
  "Send block-or-clause at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python-command t nil))

(defun py-execute-block-or-clause-python-dedicated-switch ()
  "Send block-or-clause at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python-command t 'switch))

(defun py-execute-block-or-clause-ipython ()
  "Send block-or-clause at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-ipython-command nil nil))

(defun py-execute-block-or-clause-ipython-switch ()
  "Send block-or-clause at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-ipython-command nil 'switch))

(defun py-execute-block-or-clause-ipython-no-switch ()
  "Send block-or-clause at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-ipython-command nil 'no-switch))

(defun py-execute-block-or-clause-ipython-dedicated ()
  "Send block-or-clause at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-ipython-command t nil))

(defun py-execute-block-or-clause-ipython-dedicated-switch ()
  "Send block-or-clause at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-ipython-command t 'switch))

(defun py-execute-block-or-clause-python3 ()
  "Send block-or-clause at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python3-command nil nil))

(defun py-execute-block-or-clause-python3-switch ()
  "Send block-or-clause at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python3-command nil 'switch))

(defun py-execute-block-or-clause-python3-no-switch ()
  "Send block-or-clause at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python3-command nil 'no-switch))

(defun py-execute-block-or-clause-python3-dedicated ()
  "Send block-or-clause at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python3-command t nil))

(defun py-execute-block-or-clause-python3-dedicated-switch ()
  "Send block-or-clause at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python3-command t 'switch))

(defun py-execute-block-or-clause-python2 ()
  "Send block-or-clause at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python2-command nil nil))

(defun py-execute-block-or-clause-python2-switch ()
  "Send block-or-clause at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python2-command nil 'switch))

(defun py-execute-block-or-clause-python2-no-switch ()
  "Send block-or-clause at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python2-command nil 'no-switch))

(defun py-execute-block-or-clause-python2-dedicated ()
  "Send block-or-clause at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python2-command t nil))

(defun py-execute-block-or-clause-python2-dedicated-switch ()
  "Send block-or-clause at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-python2-command t 'switch))

(defun py-execute-block-or-clause-jython ()
  "Send block-or-clause at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-jython-command nil nil))

(defun py-execute-block-or-clause-jython-switch ()
  "Send block-or-clause at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-jython-command nil 'switch))

(defun py-execute-block-or-clause-jython-no-switch ()
  "Send block-or-clause at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-jython-command nil 'no-switch))

(defun py-execute-block-or-clause-jython-dedicated ()
  "Send block-or-clause at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-jython-command t nil))

(defun py-execute-block-or-clause-jython-dedicated-switch ()
  "Send block-or-clause at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-jython-command t 'switch))

(defun py-execute-block-or-clause-bpython ()
  "Send block-or-clause at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-bpython-command nil nil))

(defun py-execute-block-or-clause-bpython-switch ()
  "Send block-or-clause at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-bpython-command nil 'switch))

(defun py-execute-block-or-clause-bpython-no-switch ()
  "Send block-or-clause at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-bpython-command nil 'no-switch))

(defun py-execute-block-or-clause-bpython-dedicated ()
  "Send block-or-clause at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-bpython-command t nil))

(defun py-execute-block-or-clause-bpython-dedicated-switch ()
  "Send block-or-clause at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "block-or-clause" 'py-bpython-command t 'switch))

(defun py-execute-def-python ()
  "Send def at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "def" 'py-python-command nil nil))

(defun py-execute-def-python-switch ()
  "Send def at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "def" 'py-python-command nil 'switch))

(defun py-execute-def-python-no-switch ()
  "Send def at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "def" 'py-python-command nil 'no-switch))

(defun py-execute-def-python-dedicated ()
  "Send def at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "def" 'py-python-command t nil))

(defun py-execute-def-python-dedicated-switch ()
  "Send def at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "def" 'py-python-command t 'switch))

(defun py-execute-def-ipython ()
  "Send def at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-ipython-command nil nil))

(defun py-execute-def-ipython-switch ()
  "Send def at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" 'py-ipython-command nil 'switch))

(defun py-execute-def-ipython-no-switch ()
  "Send def at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" 'py-ipython-command nil 'no-switch))

(defun py-execute-def-ipython-dedicated ()
  "Send def at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-ipython-command t nil))

(defun py-execute-def-ipython-dedicated-switch ()
  "Send def at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" 'py-ipython-command t 'switch))

(defun py-execute-def-python3 ()
  "Send def at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-python3-command nil nil))

(defun py-execute-def-python3-switch ()
  "Send def at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" 'py-python3-command nil 'switch))

(defun py-execute-def-python3-no-switch ()
  "Send def at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" 'py-python3-command nil 'no-switch))

(defun py-execute-def-python3-dedicated ()
  "Send def at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-python3-command t nil))

(defun py-execute-def-python3-dedicated-switch ()
  "Send def at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" 'py-python3-command t 'switch))

(defun py-execute-def-python2 ()
  "Send def at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-python2-command nil nil))

(defun py-execute-def-python2-switch ()
  "Send def at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" 'py-python2-command nil 'switch))

(defun py-execute-def-python2-no-switch ()
  "Send def at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" 'py-python2-command nil 'no-switch))

(defun py-execute-def-python2-dedicated ()
  "Send def at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-python2-command t nil))

(defun py-execute-def-python2-dedicated-switch ()
  "Send def at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" 'py-python2-command t 'switch))

(defun py-execute-def-jython ()
  "Send def at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-jython-command nil nil))

(defun py-execute-def-jython-switch ()
  "Send def at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" 'py-jython-command nil 'switch))

(defun py-execute-def-jython-no-switch ()
  "Send def at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" 'py-jython-command nil 'no-switch))

(defun py-execute-def-jython-dedicated ()
  "Send def at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-jython-command t nil))

(defun py-execute-def-jython-dedicated-switch ()
  "Send def at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" 'py-jython-command t 'switch))

(defun py-execute-def-bpython ()
  "Send def at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-bpython-command nil nil))

(defun py-execute-def-bpython-switch ()
  "Send def at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "def" 'py-bpython-command nil 'switch))

(defun py-execute-def-bpython-no-switch ()
  "Send def at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "def" 'py-bpython-command nil 'no-switch))

(defun py-execute-def-bpython-dedicated ()
  "Send def at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "def" 'py-bpython-command t nil))

(defun py-execute-def-bpython-dedicated-switch ()
  "Send def at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "def" 'py-bpython-command t 'switch))

(defun py-execute-class-python ()
  "Send class at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "class" 'py-python-command nil nil))

(defun py-execute-class-python-switch ()
  "Send class at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "class" 'py-python-command nil 'switch))

(defun py-execute-class-python-no-switch ()
  "Send class at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "class" 'py-python-command nil 'no-switch))

(defun py-execute-class-python-dedicated ()
  "Send class at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "class" 'py-python-command t nil))

(defun py-execute-class-python-dedicated-switch ()
  "Send class at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "class" 'py-python-command t 'switch))

(defun py-execute-class-ipython ()
  "Send class at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-ipython-command nil nil))

(defun py-execute-class-ipython-switch ()
  "Send class at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" 'py-ipython-command nil 'switch))

(defun py-execute-class-ipython-no-switch ()
  "Send class at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" 'py-ipython-command nil 'no-switch))

(defun py-execute-class-ipython-dedicated ()
  "Send class at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-ipython-command t nil))

(defun py-execute-class-ipython-dedicated-switch ()
  "Send class at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" 'py-ipython-command t 'switch))

(defun py-execute-class-python3 ()
  "Send class at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-python3-command nil nil))

(defun py-execute-class-python3-switch ()
  "Send class at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" 'py-python3-command nil 'switch))

(defun py-execute-class-python3-no-switch ()
  "Send class at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" 'py-python3-command nil 'no-switch))

(defun py-execute-class-python3-dedicated ()
  "Send class at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-python3-command t nil))

(defun py-execute-class-python3-dedicated-switch ()
  "Send class at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" 'py-python3-command t 'switch))

(defun py-execute-class-python2 ()
  "Send class at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-python2-command nil nil))

(defun py-execute-class-python2-switch ()
  "Send class at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" 'py-python2-command nil 'switch))

(defun py-execute-class-python2-no-switch ()
  "Send class at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" 'py-python2-command nil 'no-switch))

(defun py-execute-class-python2-dedicated ()
  "Send class at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-python2-command t nil))

(defun py-execute-class-python2-dedicated-switch ()
  "Send class at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" 'py-python2-command t 'switch))

(defun py-execute-class-jython ()
  "Send class at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-jython-command nil nil))

(defun py-execute-class-jython-switch ()
  "Send class at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" 'py-jython-command nil 'switch))

(defun py-execute-class-jython-no-switch ()
  "Send class at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" 'py-jython-command nil 'no-switch))

(defun py-execute-class-jython-dedicated ()
  "Send class at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-jython-command t nil))

(defun py-execute-class-jython-dedicated-switch ()
  "Send class at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" 'py-jython-command t 'switch))

(defun py-execute-class-bpython ()
  "Send class at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-bpython-command nil nil))

(defun py-execute-class-bpython-switch ()
  "Send class at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "class" 'py-bpython-command nil 'switch))

(defun py-execute-class-bpython-no-switch ()
  "Send class at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "class" 'py-bpython-command nil 'no-switch))

(defun py-execute-class-bpython-dedicated ()
  "Send class at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "class" 'py-bpython-command t nil))

(defun py-execute-class-bpython-dedicated-switch ()
  "Send class at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "class" 'py-bpython-command t 'switch))

(defun py-execute-region-python (beg end)
  "Send region at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py-execute-prepare "region" 'py-python-command nil nil beg end))

(defun py-execute-region-python-switch (beg end)
  "Send region at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py-execute-prepare "region" 'py-python-command nil 'switch beg end))

(defun py-execute-region-python-no-switch (beg end)
  "Send region at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py-execute-prepare "region" 'py-python-command nil 'no-switch beg end))

(defun py-execute-region-python-dedicated (beg end)
  "Send region at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py-execute-prepare "region" 'py-python-command t nil beg end))

(defun py-execute-region-python-dedicated-switch (beg end)
  "Send region at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive "r")
  (py-execute-prepare "region" 'py-python-command t 'switch beg end))

(defun py-execute-region-ipython (beg end)
  "Send region at point to IPython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-ipython-command nil nil beg end))

(defun py-execute-region-ipython-switch (beg end)
  "Send region at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" 'py-ipython-command nil 'switch beg end))

(defun py-execute-region-ipython-no-switch (beg end)
  "Send region at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" 'py-ipython-command nil 'no-switch beg end))

(defun py-execute-region-ipython-dedicated (beg end)
  "Send region at point to IPython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-ipython-command t nil beg end))

(defun py-execute-region-ipython-dedicated-switch (beg end)
  "Send region at point to IPython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" 'py-ipython-command t 'switch beg end))

(defun py-execute-region-python3 (beg end)
  "Send region at point to Python3 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python3-command nil nil beg end))

(defun py-execute-region-python3-switch (beg end)
  "Send region at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python3-command nil 'switch beg end))

(defun py-execute-region-python3-no-switch (beg end)
  "Send region at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" 'py-python3-command nil 'no-switch beg end))

(defun py-execute-region-python3-dedicated (beg end)
  "Send region at point to Python3 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python3-command t nil beg end))

(defun py-execute-region-python3-dedicated-switch (beg end)
  "Send region at point to Python3 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python3-command t 'switch beg end))

(defun py-execute-region-python2 (beg end)
  "Send region at point to Python2 interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python2-command nil nil beg end))

(defun py-execute-region-python2-switch (beg end)
  "Send region at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python2-command nil 'switch beg end))

(defun py-execute-region-python2-no-switch (beg end)
  "Send region at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" 'py-python2-command nil 'no-switch beg end))

(defun py-execute-region-python2-dedicated (beg end)
  "Send region at point to Python2 unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python2-command t nil beg end))

(defun py-execute-region-python2-dedicated-switch (beg end)
  "Send region at point to Python2 unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" 'py-python2-command t 'switch beg end))

(defun py-execute-region-jython (beg end)
  "Send region at point to Jython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-jython-command nil nil beg end))

(defun py-execute-region-jython-switch (beg end)
  "Send region at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" 'py-jython-command nil 'switch beg end))

(defun py-execute-region-jython-no-switch (beg end)
  "Send region at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" 'py-jython-command nil 'no-switch beg end))

(defun py-execute-region-jython-dedicated (beg end)
  "Send region at point to Jython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-jython-command t nil beg end))

(defun py-execute-region-jython-dedicated-switch (beg end)
  "Send region at point to Jython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" 'py-jython-command t 'switch beg end))

(defun py-execute-region-bpython (beg end)
  "Send region at point to Bpython interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-bpython-command nil nil beg end))

(defun py-execute-region-bpython-switch (beg end)
  "Send region at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive "r")
  (py-execute-prepare "region" 'py-bpython-command nil 'switch beg end))

(defun py-execute-region-bpython-no-switch (beg end)
  "Send region at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (py-execute-prepare "region" 'py-bpython-command nil 'no-switch beg end))

(defun py-execute-region-bpython-dedicated (beg end)
  "Send region at point to Bpython unique interpreter. "
  (interactive "r")
  (py-execute-prepare "region" 'py-bpython-command t nil beg end))

(defun py-execute-region-bpython-dedicated-switch (beg end)
  "Send region at point to Bpython unique interpreter and switch to result. "
  (interactive "r")
  (py-execute-prepare "region" 'py-bpython-command t 'switch beg end))

(defun py-execute-buffer-python ()
  "Send buffer at point to default interpreter. 

For `default' see value of `py-shell-name'"
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
      (py-execute-prepare "buffer" 'py-python-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python-switch ()
  "Send buffer at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
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
      (py-execute-prepare "buffer" 'py-python-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python-no-switch ()
  "Send buffer at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
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
      (py-execute-prepare "buffer" 'py-python-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated ()
  "Send buffer at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
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
      (py-execute-prepare "buffer" 'py-python-command t nil (point-min) (point-max)))))

(defun py-execute-buffer-python-dedicated-switch ()
  "Send buffer at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
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
      (py-execute-prepare "buffer" 'py-python-command t 'switch (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-ipython-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-ipython-switch ()
  "Send buffer at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
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
      (py-execute-prepare "buffer" 'py-ipython-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-ipython-no-switch ()
  "Send buffer at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
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
      (py-execute-prepare "buffer" 'py-ipython-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-ipython-dedicated ()
  "Send buffer at point to IPython unique interpreter. "
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
      (py-execute-prepare "buffer" 'py-ipython-command t nil (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-ipython-command t 'switch (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-python3-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python3-switch ()
  "Send buffer at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
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
      (py-execute-prepare "buffer" 'py-python3-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python3-no-switch ()
  "Send buffer at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
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
      (py-execute-prepare "buffer" 'py-python3-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python3-dedicated ()
  "Send buffer at point to Python3 unique interpreter. "
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
      (py-execute-prepare "buffer" 'py-python3-command t nil (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-python3-command t 'switch (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-python2-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-python2-switch ()
  "Send buffer at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
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
      (py-execute-prepare "buffer" 'py-python2-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-python2-no-switch ()
  "Send buffer at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
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
      (py-execute-prepare "buffer" 'py-python2-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-python2-dedicated ()
  "Send buffer at point to Python2 unique interpreter. "
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
      (py-execute-prepare "buffer" 'py-python2-command t nil (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-python2-command t 'switch (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-jython-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-jython-switch ()
  "Send buffer at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
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
      (py-execute-prepare "buffer" 'py-jython-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-jython-no-switch ()
  "Send buffer at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
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
      (py-execute-prepare "buffer" 'py-jython-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-jython-dedicated ()
  "Send buffer at point to Jython unique interpreter. "
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
      (py-execute-prepare "buffer" 'py-jython-command t nil (point-min) (point-max)))))

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
      (py-execute-prepare "buffer" 'py-jython-command t 'switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython ()
  "Send buffer at point to Bpython interpreter. "
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
      (py-execute-prepare "buffer" 'py-bpython-command nil nil (point-min) (point-max)))))

(defun py-execute-buffer-bpython-switch ()
  "Send buffer at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
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
      (py-execute-prepare "buffer" 'py-bpython-command nil 'switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython-no-switch ()
  "Send buffer at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
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
      (py-execute-prepare "buffer" 'py-bpython-command nil 'no-switch (point-min) (point-max)))))

(defun py-execute-buffer-bpython-dedicated ()
  "Send buffer at point to Bpython unique interpreter. "
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
      (py-execute-prepare "buffer" 'py-bpython-command t nil (point-min) (point-max)))))

(defun py-execute-buffer-bpython-dedicated-switch ()
  "Send buffer at point to Bpython unique interpreter and switch to result. "
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
      (py-execute-prepare "buffer" 'py-bpython-command t 'switch (point-min) (point-max)))))

(defun py-execute-expression-python ()
  "Send expression at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "expression" 'py-python-command nil nil))

(defun py-execute-expression-python-switch ()
  "Send expression at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "expression" 'py-python-command nil 'switch))

(defun py-execute-expression-python-no-switch ()
  "Send expression at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "expression" 'py-python-command nil 'no-switch))

(defun py-execute-expression-python-dedicated ()
  "Send expression at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "expression" 'py-python-command t nil))

(defun py-execute-expression-python-dedicated-switch ()
  "Send expression at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "expression" 'py-python-command t 'switch))

(defun py-execute-expression-ipython ()
  "Send expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-ipython-command nil nil))

(defun py-execute-expression-ipython-switch ()
  "Send expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" 'py-ipython-command nil 'switch))

(defun py-execute-expression-ipython-no-switch ()
  "Send expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" 'py-ipython-command nil 'no-switch))

(defun py-execute-expression-ipython-dedicated ()
  "Send expression at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-ipython-command t nil))

(defun py-execute-expression-ipython-dedicated-switch ()
  "Send expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" 'py-ipython-command t 'switch))

(defun py-execute-expression-python3 ()
  "Send expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-python3-command nil nil))

(defun py-execute-expression-python3-switch ()
  "Send expression at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" 'py-python3-command nil 'switch))

(defun py-execute-expression-python3-no-switch ()
  "Send expression at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" 'py-python3-command nil 'no-switch))

(defun py-execute-expression-python3-dedicated ()
  "Send expression at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-python3-command t nil))

(defun py-execute-expression-python3-dedicated-switch ()
  "Send expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" 'py-python3-command t 'switch))

(defun py-execute-expression-python2 ()
  "Send expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-python2-command nil nil))

(defun py-execute-expression-python2-switch ()
  "Send expression at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" 'py-python2-command nil 'switch))

(defun py-execute-expression-python2-no-switch ()
  "Send expression at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" 'py-python2-command nil 'no-switch))

(defun py-execute-expression-python2-dedicated ()
  "Send expression at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-python2-command t nil))

(defun py-execute-expression-python2-dedicated-switch ()
  "Send expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" 'py-python2-command t 'switch))

(defun py-execute-expression-jython ()
  "Send expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-jython-command nil nil))

(defun py-execute-expression-jython-switch ()
  "Send expression at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" 'py-jython-command nil 'switch))

(defun py-execute-expression-jython-no-switch ()
  "Send expression at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" 'py-jython-command nil 'no-switch))

(defun py-execute-expression-jython-dedicated ()
  "Send expression at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-jython-command t nil))

(defun py-execute-expression-jython-dedicated-switch ()
  "Send expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" 'py-jython-command t 'switch))

(defun py-execute-expression-bpython ()
  "Send expression at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-bpython-command nil nil))

(defun py-execute-expression-bpython-switch ()
  "Send expression at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "expression" 'py-bpython-command nil 'switch))

(defun py-execute-expression-bpython-no-switch ()
  "Send expression at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "expression" 'py-bpython-command nil 'no-switch))

(defun py-execute-expression-bpython-dedicated ()
  "Send expression at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "expression" 'py-bpython-command t nil))

(defun py-execute-expression-bpython-dedicated-switch ()
  "Send expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "expression" 'py-bpython-command t 'switch))

(defun py-execute-partial-expression-python ()
  "Send partial-expression at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python-command nil nil))

(defun py-execute-partial-expression-python-switch ()
  "Send partial-expression at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python-command nil 'switch))

(defun py-execute-partial-expression-python-no-switch ()
  "Send partial-expression at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python-command nil 'no-switch))

(defun py-execute-partial-expression-python-dedicated ()
  "Send partial-expression at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python-command t nil))

(defun py-execute-partial-expression-python-dedicated-switch ()
  "Send partial-expression at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python-command t 'switch))

(defun py-execute-partial-expression-ipython ()
  "Send partial-expression at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-ipython-command nil nil))

(defun py-execute-partial-expression-ipython-switch ()
  "Send partial-expression at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-ipython-command nil 'switch))

(defun py-execute-partial-expression-ipython-no-switch ()
  "Send partial-expression at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-ipython-command nil 'no-switch))

(defun py-execute-partial-expression-ipython-dedicated ()
  "Send partial-expression at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-ipython-command t nil))

(defun py-execute-partial-expression-ipython-dedicated-switch ()
  "Send partial-expression at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-ipython-command t 'switch))

(defun py-execute-partial-expression-python3 ()
  "Send partial-expression at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python3-command nil nil))

(defun py-execute-partial-expression-python3-switch ()
  "Send partial-expression at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python3-command nil 'switch))

(defun py-execute-partial-expression-python3-no-switch ()
  "Send partial-expression at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python3-command nil 'no-switch))

(defun py-execute-partial-expression-python3-dedicated ()
  "Send partial-expression at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python3-command t nil))

(defun py-execute-partial-expression-python3-dedicated-switch ()
  "Send partial-expression at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python3-command t 'switch))

(defun py-execute-partial-expression-python2 ()
  "Send partial-expression at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python2-command nil nil))

(defun py-execute-partial-expression-python2-switch ()
  "Send partial-expression at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python2-command nil 'switch))

(defun py-execute-partial-expression-python2-no-switch ()
  "Send partial-expression at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python2-command nil 'no-switch))

(defun py-execute-partial-expression-python2-dedicated ()
  "Send partial-expression at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python2-command t nil))

(defun py-execute-partial-expression-python2-dedicated-switch ()
  "Send partial-expression at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-python2-command t 'switch))

(defun py-execute-partial-expression-jython ()
  "Send partial-expression at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-jython-command nil nil))

(defun py-execute-partial-expression-jython-switch ()
  "Send partial-expression at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-jython-command nil 'switch))

(defun py-execute-partial-expression-jython-no-switch ()
  "Send partial-expression at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-jython-command nil 'no-switch))

(defun py-execute-partial-expression-jython-dedicated ()
  "Send partial-expression at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-jython-command t nil))

(defun py-execute-partial-expression-jython-dedicated-switch ()
  "Send partial-expression at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-jython-command t 'switch))

(defun py-execute-partial-expression-bpython ()
  "Send partial-expression at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-bpython-command nil nil))

(defun py-execute-partial-expression-bpython-switch ()
  "Send partial-expression at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-bpython-command nil 'switch))

(defun py-execute-partial-expression-bpython-no-switch ()
  "Send partial-expression at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-bpython-command nil 'no-switch))

(defun py-execute-partial-expression-bpython-dedicated ()
  "Send partial-expression at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-bpython-command t nil))

(defun py-execute-partial-expression-bpython-dedicated-switch ()
  "Send partial-expression at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "partial-expression" 'py-bpython-command t 'switch))

(defun py-execute-line-python ()
  "Send line at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "line" 'py-python-command nil nil))

(defun py-execute-line-python-switch ()
  "Send line at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "line" 'py-python-command nil 'switch))

(defun py-execute-line-python-no-switch ()
  "Send line at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "line" 'py-python-command nil 'no-switch))

(defun py-execute-line-python-dedicated ()
  "Send line at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "line" 'py-python-command t nil))

(defun py-execute-line-python-dedicated-switch ()
  "Send line at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "line" 'py-python-command t 'switch))

(defun py-execute-line-ipython ()
  "Send line at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-ipython-command nil nil))

(defun py-execute-line-ipython-switch ()
  "Send line at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" 'py-ipython-command nil 'switch))

(defun py-execute-line-ipython-no-switch ()
  "Send line at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" 'py-ipython-command nil 'no-switch))

(defun py-execute-line-ipython-dedicated ()
  "Send line at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-ipython-command t nil))

(defun py-execute-line-ipython-dedicated-switch ()
  "Send line at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" 'py-ipython-command t 'switch))

(defun py-execute-line-python3 ()
  "Send line at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-python3-command nil nil))

(defun py-execute-line-python3-switch ()
  "Send line at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" 'py-python3-command nil 'switch))

(defun py-execute-line-python3-no-switch ()
  "Send line at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" 'py-python3-command nil 'no-switch))

(defun py-execute-line-python3-dedicated ()
  "Send line at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-python3-command t nil))

(defun py-execute-line-python3-dedicated-switch ()
  "Send line at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" 'py-python3-command t 'switch))

(defun py-execute-line-python2 ()
  "Send line at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-python2-command nil nil))

(defun py-execute-line-python2-switch ()
  "Send line at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" 'py-python2-command nil 'switch))

(defun py-execute-line-python2-no-switch ()
  "Send line at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" 'py-python2-command nil 'no-switch))

(defun py-execute-line-python2-dedicated ()
  "Send line at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-python2-command t nil))

(defun py-execute-line-python2-dedicated-switch ()
  "Send line at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" 'py-python2-command t 'switch))

(defun py-execute-line-jython ()
  "Send line at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-jython-command nil nil))

(defun py-execute-line-jython-switch ()
  "Send line at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" 'py-jython-command nil 'switch))

(defun py-execute-line-jython-no-switch ()
  "Send line at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" 'py-jython-command nil 'no-switch))

(defun py-execute-line-jython-dedicated ()
  "Send line at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-jython-command t nil))

(defun py-execute-line-jython-dedicated-switch ()
  "Send line at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" 'py-jython-command t 'switch))

(defun py-execute-line-bpython ()
  "Send line at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-bpython-command nil nil))

(defun py-execute-line-bpython-switch ()
  "Send line at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "line" 'py-bpython-command nil 'switch))

(defun py-execute-line-bpython-no-switch ()
  "Send line at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "line" 'py-bpython-command nil 'no-switch))

(defun py-execute-line-bpython-dedicated ()
  "Send line at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "line" 'py-bpython-command t nil))

(defun py-execute-line-bpython-dedicated-switch ()
  "Send line at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "line" 'py-bpython-command t 'switch))

(defun py-execute-top-level-python ()
  "Send top-level at point to default interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "top-level" 'py-python-command nil nil))

(defun py-execute-top-level-python-switch ()
  "Send top-level at point to default interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "top-level" 'py-python-command nil 'switch))

(defun py-execute-top-level-python-no-switch ()
  "Send top-level at point to default interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "top-level" 'py-python-command nil 'no-switch))

(defun py-execute-top-level-python-dedicated ()
  "Send top-level at point to default unique interpreter. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "top-level" 'py-python-command t nil))

(defun py-execute-top-level-python-dedicated-switch ()
  "Send top-level at point to default unique interpreter and switch to result. 

For `default' see value of `py-shell-name'"
  (interactive)
  (py-execute-prepare "top-level" 'py-python-command t 'switch))

(defun py-execute-top-level-ipython ()
  "Send top-level at point to IPython interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-ipython-command nil nil))

(defun py-execute-top-level-ipython-switch ()
  "Send top-level at point to IPython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "top-level" 'py-ipython-command nil 'switch))

(defun py-execute-top-level-ipython-no-switch ()
  "Send top-level at point to IPython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "top-level" 'py-ipython-command nil 'no-switch))

(defun py-execute-top-level-ipython-dedicated ()
  "Send top-level at point to IPython unique interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-ipython-command t nil))

(defun py-execute-top-level-ipython-dedicated-switch ()
  "Send top-level at point to IPython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "top-level" 'py-ipython-command t 'switch))

(defun py-execute-top-level-python3 ()
  "Send top-level at point to Python3 interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python3-command nil nil))

(defun py-execute-top-level-python3-switch ()
  "Send top-level at point to Python3 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python3-command nil 'switch))

(defun py-execute-top-level-python3-no-switch ()
  "Send top-level at point to Python3 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "top-level" 'py-python3-command nil 'no-switch))

(defun py-execute-top-level-python3-dedicated ()
  "Send top-level at point to Python3 unique interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python3-command t nil))

(defun py-execute-top-level-python3-dedicated-switch ()
  "Send top-level at point to Python3 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python3-command t 'switch))

(defun py-execute-top-level-python2 ()
  "Send top-level at point to Python2 interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python2-command nil nil))

(defun py-execute-top-level-python2-switch ()
  "Send top-level at point to Python2 interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python2-command nil 'switch))

(defun py-execute-top-level-python2-no-switch ()
  "Send top-level at point to Python2 interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "top-level" 'py-python2-command nil 'no-switch))

(defun py-execute-top-level-python2-dedicated ()
  "Send top-level at point to Python2 unique interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python2-command t nil))

(defun py-execute-top-level-python2-dedicated-switch ()
  "Send top-level at point to Python2 unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "top-level" 'py-python2-command t 'switch))

(defun py-execute-top-level-jython ()
  "Send top-level at point to Jython interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-jython-command nil nil))

(defun py-execute-top-level-jython-switch ()
  "Send top-level at point to Jython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "top-level" 'py-jython-command nil 'switch))

(defun py-execute-top-level-jython-no-switch ()
  "Send top-level at point to Jython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "top-level" 'py-jython-command nil 'no-switch))

(defun py-execute-top-level-jython-dedicated ()
  "Send top-level at point to Jython unique interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-jython-command t nil))

(defun py-execute-top-level-jython-dedicated-switch ()
  "Send top-level at point to Jython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "top-level" 'py-jython-command t 'switch))

(defun py-execute-top-level-bpython ()
  "Send top-level at point to Bpython interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-bpython-command nil nil))

(defun py-execute-top-level-bpython-switch ()
  "Send top-level at point to Bpython interpreter. 

Switch to output buffer. Ignores `py-switch-buffers-on-execute-p'. "
  (interactive)
  (py-execute-prepare "top-level" 'py-bpython-command nil 'switch))

(defun py-execute-top-level-bpython-no-switch ()
  "Send top-level at point to Bpython interpreter. 

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive)
  (py-execute-prepare "top-level" 'py-bpython-command nil 'no-switch))

(defun py-execute-top-level-bpython-dedicated ()
  "Send top-level at point to Bpython unique interpreter. "
  (interactive)
  (py-execute-prepare "top-level" 'py-bpython-command t nil))

(defun py-execute-top-level-bpython-dedicated-switch ()
  "Send top-level at point to Bpython unique interpreter and switch to result. "
  (interactive)
  (py-execute-prepare "top-level" 'py-bpython-command t 'switch))

(provide 'python-extended-executes)
;;; python-extended-executes.el ends here
 
