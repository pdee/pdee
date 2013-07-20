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
(defun py-shell-prepare (argprompt dedicated shell switch)
  (if py-fontify-shell-buffer-p
      (save-excursion
        (let ((oldbuf (current-buffer))
              (orig (point))
              (erg (py-shell argprompt dedicated shell switch)))
          (set-buffer erg)
          (font-lock-unfontify-region (point-min) (line-beginning-position))
          (unless switch
            (set-buffer oldbuf)
            (goto-char orig))
          erg))
    (py-shell argprompt dedicated shell switch)))

(defun python (&optional argprompt dedicated switch)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "python" switch))

(defalias 'ipyhton 'ipython)
(defalias 'iypthon 'ipython)
(defun ipython (&optional argprompt dedicated switch)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "ipython" switch))

(defun python3 (&optional argprompt dedicated switch)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "python3" switch))

(defun python2 (&optional argprompt dedicated switch)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "python2" switch))

(defun python2.7 (&optional argprompt dedicated switch)
  "Start an Python2.7 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "python2.7" switch))

(defun jython (&optional argprompt dedicated switch)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "jython" switch))

(defun bpython (&optional argprompt dedicated switch)
  "Start an BPython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "bpython" switch))

(defun python3.2 (&optional argprompt dedicated switch)
  "Start an Python3.2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-shell argprompt dedicated "python3.2" switch))

(defun python3.3 (&optional argprompt dedicated switch)
  "Start an Python3.3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. 

Command expects Python3.3 installed at your system. "
  (interactive "P")
  (py-shell argprompt dedicated "python3.3" switch))

;; dedicated
(defun python-dedicated (&optional argprompt switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python" switch))

(defun ipython-dedicated (&optional argprompt switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "ipython" switch))

(defun python3-dedicated (&optional argprompt switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python3" switch))

(defun python2-dedicated (&optional argprompt switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python2" switch))

(defun python2.7-dedicated (&optional argprompt switch)
  "Start an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python2.7" switch))

(defun jython-dedicated (&optional argprompt switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "jython" switch))

(defun python3.2-dedicated (&optional argprompt switch)
  "Start an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python3.2" switch))

;; switch
(defun python-switch (&optional argprompt dedicated)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python" 'switch))

(defun ipython-switch (&optional argprompt dedicated)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "ipython" 'switch))

(defun python3-switch (&optional argprompt dedicated)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python3" 'switch))

(defun python2-switch (&optional argprompt dedicated)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python2" 'switch))

(defun python2.7-switch (&optional argprompt dedicated)
  "Switch to Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python2.7" 'switch))

(defun jython-switch (&optional argprompt dedicated)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "jython" 'switch))

(defun python3.2-switch (&optional argprompt dedicated)
  "Switch to Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python3.2" 'switch))

(defun python-no-switch (&optional argprompt dedicated)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python" 'no-switch))

(defun ipython-no-switch (&optional argprompt dedicated)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "ipython" 'no-switch))

(defun python3-no-switch (&optional argprompt dedicated)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python3" 'no-switch))

(defun python2-no-switch (&optional argprompt dedicated)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python2" 'no-switch))

(defun python2.7-no-switch (&optional argprompt dedicated)
  "Open an Python2.7 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python2.7" 'no-switch))

(defun jython-no-switch (&optional argprompt dedicated)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "jython" 'no-switch))

(defun python3.2-no-switch (&optional argprompt dedicated)
  "Open an Python3.2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt dedicated "python3.2" 'no-switch))

(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python" 'switch))

(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "ipython" 'switch))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python3" 'switch))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python2" 'switch))

(defalias 'python2.7-dedicated-switch 'python2.7-switch-dedicated)
(defun python2.7-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python2.7" 'switch))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "jython" 'switch))

(defalias 'python3.2-dedicated-switch 'python3.2-switch-dedicated)
(defun python3.2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-shell argprompt t "python3.2" 'switch))

(defalias 'Python 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)

(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
