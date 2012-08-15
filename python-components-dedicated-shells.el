;;; python-components-dedicated-shells.el --- Versioned dedicated Python shells

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
;;

;;; Code:

(defun python-dedicated (&optional argprompt)
  "Start an Python dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python"))

(defun python2-dedicated (&optional argprompt)
  "Start an Python2 dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python2 interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2"))

(defun python2.7-dedicated (&optional argprompt)
  "Start an Python2.7 dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python2.7 interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python2.7"))

(defun python3-dedicated (&optional argprompt)
  "Start an Python3 dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python3 interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3"))

(defun python3.2-dedicated (&optional argprompt)
  "Start an Python3.2 dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python3.2 interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "python3.2"))

(defun ipython-dedicated (&optional argprompt)
  "Start an IPython dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the IPython interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "ipython"))

(defun jython-dedicated (&optional argprompt)
  "Start an Jython dedicated interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Jython interpreter. "
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t "jython"))

(provide 'python-components-dedicated-shells)
;;; python-components-dedicated-shells.el ends here
