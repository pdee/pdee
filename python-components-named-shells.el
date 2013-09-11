;;; python-components-named-shells.el --- Versioned Python shells

;; Copyright (C) 2011  Andreas Roehler

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
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

;;; Code:
(require 'python-components-macros)
;;; Python named shells
(defun python (&optional argprompt)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python"))

(defun ipython (&optional argprompt)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "ipython"))

(defun python3 (&optional argprompt)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3"))

(defun python2 (&optional argprompt)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python2"))

(defun python2.7 (&optional argprompt)
  "Start an Python2.7 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python2.7"))

(defun jython (&optional argprompt)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "jython"))

(defun python3.2 (&optional argprompt)
  "Start an Python3.2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3.2"))

(defun python3.3 (&optional argprompt)
  "Start an Python3.3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3.3"))

(defun python3.4 (&optional argprompt)
  "Start an Python3.3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.4 interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "python3.4"))

(defun bpython (&optional argprompt)
  "Start an Bpython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'. "
  (interactive "P")
  (py-shell argprompt nil "bpython"))

;; dedicated
(defun python-dedicated (&optional argprompt switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python")))

(defun ipython-dedicated (&optional argprompt switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "ipython")))

(defun python3-dedicated (&optional argprompt switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3")))

(defun python2-dedicated (&optional argprompt switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python2")))

(defun python2.7-dedicated (&optional argprompt switch)
  "Start an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python2.7")))

(defun jython-dedicated (&optional argprompt switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "jython")))

(defun python3.2-dedicated (&optional argprompt switch)
  "Start an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3.2")))

(defun python3.3-dedicated (&optional argprompt switch)
  "Start an unique Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "python3.3")))

(defun bpython-dedicated (&optional argprompt switch)
  "Start an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t))
    (py-shell argprompt t "bpython")))

;; switch
(defun python-switch (&optional argprompt)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python")))

(defun ipython-switch (&optional argprompt)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "ipython")))

(defun python3-switch (&optional argprompt)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3")))

(defun python2-switch (&optional argprompt)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python2")))

(defun python2.7-switch (&optional argprompt)
  "Switch to Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python2.7")))

(defun jython-switch (&optional argprompt)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "jython")))

(defun python3.2-switch (&optional argprompt)
  "Switch to Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3.2")))

(defun python3.3-switch (&optional argprompt)
  "Switch to Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "python3.3")))

(defun bpython-switch (&optional argprompt)
  "Switch to Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-switch-buffers-on-execute-p t))
    (py-shell argprompt nil "bpython")))

;; no-switch
(defun python-no-switch (&optional argprompt)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python")))

(defun ipython-no-switch (&optional argprompt)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "ipython")))

(defun python3-no-switch (&optional argprompt)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3")))

(defun python2-no-switch (&optional argprompt)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python2")))

(defun python2.7-no-switch (&optional argprompt)
  "Open an Python2.7 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python2.7")))

(defun jython-no-switch (&optional argprompt)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "jython")))

(defun python3.2-no-switch (&optional argprompt)
  "Open an Python3.2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3.2")))

(defun python3.3-no-switch (&optional argprompt)
  "Open an Python3.3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "python3.3")))

(defun bpython-no-switch (&optional argprompt)
  "Open an Bpython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let (py-switch-buffers-on-execute-p)
    (py-shell argprompt nil "bpython")))

;; dedicated switch
(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python")))

(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "ipython")))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3")))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python2")))

(defalias 'python2.7-dedicated-switch 'python2.7-switch-dedicated)
(defun python2.7-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python2.7")))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "jython")))

(defalias 'python3.2-dedicated-switch 'python3.2-switch-dedicated)
(defun python3.2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3.2")))

(defalias 'python3.3-dedicated-switch 'python3.3-switch-dedicated)
(defun python3.3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "python3.3")))

(defalias 'bpython-dedicated-switch 'bpython-switch-dedicated)
(defun bpython-switch-dedicated (&optional argprompt)
  "Switch to an unique Bpython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Bpython interpreter. See `py-python-command-args'."
  (interactive "P")
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py-shell argprompt t "bpython")))

(defalias 'Python 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)

(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
