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
;;; Python named shells
(defun python (&optional argprompt dedicated switch)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python" switch))

(defun ipython (&optional argprompt dedicated switch)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (let ((py-shell-name "ipython"))
    ;; (py-set-shell-completion-environment)
    (py-shell argprompt dedicated "ipython" switch)))

(defun python3 (&optional argprompt dedicated switch)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python3" switch))

(defun python2 (&optional argprompt dedicated switch)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python2" switch))

(defun python2.7 (&optional argprompt dedicated switch)
  "Start an Python2.7 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python2.7" switch))

(defun jython (&optional argprompt dedicated switch)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "jython" switch))

(defun python3.2 (&optional argprompt dedicated switch)
  "Start an Python3.2 interpreter.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. "
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python3.2" switch))

;; dedicated
(defun python-dedicated (&optional argprompt switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python" switch))

(defun ipython-dedicated (&optional argprompt switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "ipython" switch))

(defun python3-dedicated (&optional argprompt switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3" switch))

(defun python2-dedicated (&optional argprompt switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2" switch))

(defun python2.7-dedicated (&optional argprompt switch)
  "Start an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2.7" switch))

(defun jython-dedicated (&optional argprompt switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "jython" switch))

(defun python3.2-dedicated (&optional argprompt switch)
  "Start an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3.2" switch))

;; switch
(defun python-switch (&optional argprompt dedicated)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python" switch))

(defun ipython-switch (&optional argprompt dedicated)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "ipython" switch))

(defun python3-switch (&optional argprompt dedicated)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3" switch))

(defun python2-switch (&optional argprompt dedicated)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2" switch))

(defun python2.7-switch (&optional argprompt dedicated)
  "Switch to Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2.7" switch))

(defun jython-switch (&optional argprompt dedicated)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "jython" switch))

(defun python3.2-switch (&optional argprompt dedicated)
  "Switch to Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3.2" switch))

(defun python-no-switch (&optional argprompt dedicated)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python" 'noswitch))

(defun ipython-no-switch (&optional argprompt dedicated)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "ipython" 'noswitch))

(defun python3-no-switch (&optional argprompt dedicated)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python3" 'noswitch))

(defun python2-no-switch (&optional argprompt dedicated)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python2" 'noswitch))

(defun python2.7-no-switch (&optional argprompt dedicated)
  "Open an Python2.7 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python2.7" 'noswitch))

(defun jython-no-switch (&optional argprompt dedicated)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "jython" 'noswitch))

(defun python3.2-no-switch (&optional argprompt dedicated)
  "Open an Python3.2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated "python3.2" 'noswitch))

(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python" 'switch))

(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the IPython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "ipython" 'switch))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3" 'switch))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2" 'switch))

(defalias 'python2.7-dedicated-switch 'python2.7-switch-dedicated)
(defun python2.7-switch-dedicated (&optional argprompt)
  "Switch to an unique Python2.7 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2.7" 'switch))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Jython interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "jython" 'switch))

(defalias 'python3.2-dedicated-switch 'python3.2-switch-dedicated)
(defun python3.2-switch-dedicated (&optional argprompt)
  "Switch to an unique Python3.2 interpreter in another window.

Optional \\[universal-argument] prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."
  (interactive "P")
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3.2" 'switch))

(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
