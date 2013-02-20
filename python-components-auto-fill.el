;;; python-components-auto-fill.el --- toggle comment-auto-fill

;; Copyright (C) 2012  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
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

;; http://stackoverflow.com/questions/11969442/comment-mode-in-emacs

;; I commonly program in languages that don't have any
;; sort of block/multiline comment (e.g. python [#] and
;; fortran [c or !].). Is there any way to define a
;; minor mode in emacs that would allow me to enter
;; multiline comments? By that I mean, it would cause
;; emacs to wrap text automatically after X lines (say
;; 72) and automatically prepend a comment character
;; (taken from the current major mode) to the beginning
;; of each line?

;;; Code:

(defvar auto-fill-mode-orig (auto-fill-mode)
  "Store the original state of auto-fill-mode. ")

(defvar fill-column-orig fill-column
  "Store the original value of fill-column. ")

(defun py-comment-auto-fill ()
  "Toggles comment-auto-fill mode"
  (interactive)
  (if (or (and arg (< 0 arg)) (not py-comment-auto-fill))
      (progn
        (set (make-local-variable 'py-comment-auto-fill) t)
        (setq fill-column comment-fill-column)
        (auto-fill-mode 1))
    (set (make-local-variable 'py-comment-auto-fill) nil)
;;    (set (make-local-variable 'py-comment-auto-fill-only-comments) nil)
    (setq fill-column fill-column-orig)
    (auto-fill-mode -1)))

(defun py-comment-auto-fill-on ()
  (interactive)
  (py-comment-auto-fill 1))

(defun py-comment-auto-fill-off ()
  (interactive)
  (py-comment-auto-fill -1))

(provide 'python-components-auto-fill)
;;; python-components-auto-fill.el ends here
