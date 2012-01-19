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
  "Start an unique Python interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python interpreter. "
  (interactive)
  (let ((py-shell-name "python"))
    (py-shell argprompt t)))

(defun python2-dedicated (&optional argprompt)
  "Start an unique Python2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2 interpreter. "
  (interactive)
  (let ((py-shell-name "python2"))
    (py-shell argprompt t)))

(defun python2.7-dedicated (&optional argprompt)
  "Start an unique Python2.7 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2.7 interpreter. "
  (interactive)
  (let ((py-shell-name "python2.7"))
    (py-shell argprompt t)))

(defun python3-dedicated (&optional argprompt)
  "Start an unique Python3 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3 interpreter. "
  (interactive)
  (let ((py-shell-name "python3"))
    (py-shell argprompt t)))

(defun python3.2-dedicated (&optional argprompt)
  "Start an unique Python3.2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3.2 interpreter. "
  (interactive)
  (let ((py-shell-name "python3.2"))
    (py-shell argprompt t)))

(defun ipython-dedicated (&optional argprompt)
  "Start an unique IPython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the IPython interpreter. "
  (interactive)
  (let ((py-shell-name "ipython"))
    (py-shell argprompt t)))

(defun jython-dedicated (&optional argprompt)
  "Start an unique Jython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Jython interpreter. "
  (interactive)
  (let ((py-shell-name "jython"))
    (py-shell argprompt t)))

(provide 'python-components-dedicated-shells)
;;; python-components-dedicated-shells.el ends here
