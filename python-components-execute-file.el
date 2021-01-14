;;; python-components-execute-file --- Runs files -*- lexical-binding: t; -*-


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

;; Execute file given

(defun py-execute-file-ipython (filename)
  "Send file to IPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "ipython" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython2.7 (filename)
  "Send file to IPython2.7 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "ipython2.7" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython3 (filename)
  "Send file to IPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "ipython3" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-jython (filename)
  "Send file to Jython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "jython" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python (filename)
  "Send file to Python interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "python" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python2 (filename)
  "Send file to Python2 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "python2" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python3 (filename)
  "Send file to Python3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "python3" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-pypy (filename)
  "Send file to PyPy interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
	(buffer (py-shell nil nil nil "pypy" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython-dedicated (filename)
  "Send file to a dedicatedIPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "ipython" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython2.7-dedicated (filename)
  "Send file to a dedicatedIPython2.7 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "ipython2.7" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-ipython3-dedicated (filename)
  "Send file to a dedicatedIPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "ipython3" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-jython-dedicated (filename)
  "Send file to a dedicatedJython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "jython" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python-dedicated (filename)
  "Send file to a dedicatedPython interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python2-dedicated (filename)
  "Send file to a dedicatedPython2 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python2" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-python3-dedicated (filename)
  "Send file to a dedicatedPython3 interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "python3" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(defun py-execute-file-pypy-dedicated (filename)
  "Send file to a dedicatedPyPy interpreter"
  (interactive "fFile: ")
  (let ((interactivep (called-interactively-p 'interactive))
        (buffer (py-shell nil nil t "pypy" nil t nil py-split-window-on-execute py-switch-buffers-on-execute-p)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t interactivep)))

(provide 'python-components-execute-file)
;;; python-components-execute-file.el ends here
