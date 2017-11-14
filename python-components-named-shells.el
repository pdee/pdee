;;; Python named shells -*- lexical-binding: t; -*- 

;; Copyright (C) 2015-2016 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:

;;;###autoload
(defun ipython (&optional argprompt buffer fast exception-buffer split switch)
  "Start an IPython interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython" buffer fast exception-buffer split switch))

;;;###autoload
(defun ipython2.7 (&optional argprompt buffer fast exception-buffer split switch)
  "Start an IPython2.7 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython2.7" buffer fast exception-buffer split switch))

;;;###autoload
(defun ipython3 (&optional argprompt buffer fast exception-buffer split switch)
  "Start an IPython3 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython3" buffer fast exception-buffer split switch))

;;;###autoload
(defun jython (&optional argprompt buffer fast exception-buffer split switch)
  "Start an Jython interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "jython" buffer fast exception-buffer split switch))

;;;###autoload
(defun python (&optional argprompt buffer fast exception-buffer split switch)
  "Start an Python interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python" buffer fast exception-buffer split switch))

;;;###autoload
(defun python2 (&optional argprompt buffer fast exception-buffer split switch)
  "Start an Python2 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python2" buffer fast exception-buffer split switch))

;;;###autoload
(defun python3 (&optional argprompt buffer fast exception-buffer split switch)
  "Start an Python3 interpreter.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python3" buffer fast exception-buffer split switch))

;; dedicated
(defun ipython-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython" buffer fast exception-buffer split switch))

(defun ipython2.7-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique IPython2.7 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython2.7" buffer fast exception-buffer split switch))

(defun ipython3-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique IPython3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython3" buffer fast exception-buffer split switch))

(defun jython-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "jython" buffer fast exception-buffer split switch))

(defun python-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python" buffer fast exception-buffer split switch))

(defun python2-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python2" buffer fast exception-buffer split switch))

(defun python3-dedicated (&optional argprompt buffer fast exception-buffer split switch)
  "Start an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python3" buffer fast exception-buffer split switch))

;; switch
(defun ipython-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython" buffer fast exception-buffer split t))

(defun ipython2.7-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to IPython2.7 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython2.7" buffer fast exception-buffer split t))

(defun ipython3-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to IPython3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython3" buffer fast exception-buffer split t))

(defun jython-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "jython" buffer fast exception-buffer split t))

(defun python-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python" buffer fast exception-buffer split t))

(defun python2-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python2" buffer fast exception-buffer split t))

(defun python3-switch (&optional argprompt buffer fast exception-buffer split)
  "Switch to Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python3" buffer fast exception-buffer split t))

;; no-switch
(defun ipython-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an IPython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython" buffer fast exception-buffer split))

(defun ipython2.7-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an IPython2.7 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython2.7" buffer fast exception-buffer split))

(defun ipython3-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an IPython3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "ipython3" buffer fast exception-buffer split))

(defun jython-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an Jython interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "jython" buffer fast exception-buffer split))

(defun python-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an Python interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python" buffer fast exception-buffer split))

(defun python2-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an Python2 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python2" buffer fast exception-buffer split))

(defun python3-no-switch (&optional argprompt buffer fast exception-buffer split)
  "Open an Python3 interpreter in another window, but do not switch to it.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt nil "python3" buffer fast exception-buffer split))

;; dedicated switch
(defalias 'ipython-dedicated-switch 'ipython-switch-dedicated)
(defun ipython-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique IPython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython" buffer fast exception-buffer split t))

(defalias 'ipython2.7-dedicated-switch 'ipython2.7-switch-dedicated)
(defun ipython2.7-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique IPython2.7 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython2.7" buffer fast exception-buffer split t))

(defalias 'ipython3-dedicated-switch 'ipython3-switch-dedicated)
(defun ipython3-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique IPython3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "ipython3" buffer fast exception-buffer split t))

(defalias 'jython-dedicated-switch 'jython-switch-dedicated)
(defun jython-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique Jython interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "jython" buffer fast exception-buffer split t))

(defalias 'python-dedicated-switch 'python-switch-dedicated)
(defun python-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique Python interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python" buffer fast exception-buffer split t))

(defalias 'python2-dedicated-switch 'python2-switch-dedicated)
(defun python2-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique Python2 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python2" buffer fast exception-buffer split t))

(defalias 'python3-dedicated-switch 'python3-switch-dedicated)
(defun python3-switch-dedicated (&optional argprompt buffer fast exception-buffer split)
  "Switch to an unique Python3 interpreter in another window.

Optional \\[universal-argument] prompts for path to the interpreter. "
  (interactive "p")
  (py-shell argprompt t "python3" buffer fast exception-buffer split t))

(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
