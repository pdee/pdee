;;; python-named-shells.el --- Versioned Python shells

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
;; Python-components-mode started from python-mode.el
;; and python.el. Tim Peters, Barry A. Warsaw, Skip
;; Montanaro, Ken Manheimer, Dave Love and many others
;; wrote major parts.
;;

;;; Code:
(defun python (&optional argprompt)
  "Start an Python interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python interpreter. "
  (interactive)
  (let ((py-shell-name "python"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python2 (&optional argprompt)
  "Start an Python2 interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python2 interpreter. "
  (interactive)
  (let ((py-shell-name "python2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python2.7 (&optional argprompt)
  "Start an Python2.7 interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python2.7 interpreter. "
  (interactive)
  (let ((py-shell-name "python2.7"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python3 (&optional argprompt)
  "Start an Python3 interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python3 interpreter. "
  (interactive)
  (let ((py-shell-name "python3"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python3.2 (&optional argprompt)
  "Start an Python3.2 interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Python3.2 interpreter. "
  (interactive)
  (let ((py-shell-name "python3.2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))


(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)
(defun ipython (&optional argprompt)
  "Start an IPython interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the IPython interpreter. "
  (interactive)
  (let ((py-shell-name "ipython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'ipython-complete)
    (py-shell argprompt)))

(defun jython (&optional argprompt)
  "Start an Jython interpreter in another window.
   With optional \\[universal-argument] user is prompted
    for options to pass to the Jython interpreter. "
  (interactive)
  (let ((py-shell-name "jython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))


(provide 'python-named-shells)
;;; python-named-shells.el ends here
