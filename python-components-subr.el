;;; python-components-subr.el --- filling -*- lexical-binding: t; -*-

;; Maintainer https://gitlab.com/groups/python-mode-devs

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

;;; Commentary: Delivering filling styles of was first done at python.el
;; Proceeding here adapted for python-mode.el

;;

;;; Code:

(defun py-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (save-match-data (looking-at py-empty-line-p-chars))))

(defun py-trim-string-left (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string
Argument STRG start."
  (setq arg (or arg 1))
  (substring strg arg))

(defun py-trim-string-right (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string
Argument STRG end."
  (setq arg (or arg 1))
  (let ((laenge (length strg)))
    (substring strg 0 (- laenge arg))))

(defun py-trim-string (strg &optional left right)
  "Remove ARG characters from beginning and end of STRING.

With no arguments remove just one character
Return the shortened string
Argument STRG strg.
Optional argument LEFT border.
Optional argument RIGHT border."
  (let ((left (or left 1))
	(right (or right 1))
	(laenge (length strg)))
    (setq right (- laenge right))
    (substring strg left right)))

(provide 'python-components-subr)
;;; python-components-subr.el ends here
