;;; python-components-execute-file --- Runs files -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2026 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Keywords: convenience

;;; Commentary:

;;; Code:

(defun py-execute-file-ipython (filename)
  "Send file to IPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython3 (filename)
  "Send file to IPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-jython (filename)
  "Send file to Jython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python (filename)
  "Send file to Python interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python2 (filename)
  "Send file to Python2 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python3 (filename)
  "Send file to Python3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-pypy (filename)
  "Send file to PyPy interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file- (filename)
  "Send file to  interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil nil "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython-dedicated (filename)
  "Send file to a dedicatedIPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "ipython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-ipython3-dedicated (filename)
  "Send file to a dedicatedIPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "ipython3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-jython-dedicated (filename)
  "Send file to a dedicatedJython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "jython" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python-dedicated (filename)
  "Send file to a dedicatedPython interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python2-dedicated (filename)
  "Send file to a dedicatedPython2 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python2" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-python3-dedicated (filename)
  "Send file to a dedicatedPython3 interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "python3" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file-pypy-dedicated (filename)
  "Send file to a dedicatedPyPy interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "pypy" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(defun py-execute-file--dedicated (filename)
  "Send file to a dedicated interpreter"
  (interactive "fFile: ")
  (let ((buffer (py-shell nil nil t "" nil t)))
    (py--execute-file-base filename (get-buffer-process buffer) nil buffer nil t)))

(provide (quote python-components-execute-file))
;;; python-components-execute-file.el ends here
