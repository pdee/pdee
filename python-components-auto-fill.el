;;; python-components-auto-fill.el --- toggle comment-auto-fill -*- lexical-binding: t; -*- 


;; URL: https://gitlab.com/python-mode-devs

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

(provide 'python-components-auto-fill)
;;; python-components-auto-fill.el ends here
