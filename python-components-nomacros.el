;;; python-components-macros.el --- Macro definitions here as functins for developing speed

;; Copyright (C) 2012  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords:

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

(defun py-separator-char ()
  "Return the file-path separator char from current machine.

When `py-separator-char' is customized, its taken.
Returns char found. "
  (let ((erg (cond ((characterp py-separator-char)
                    (char-to-string py-separator-char))
                   ;; epd hack
                   ((and
                     (string-match "[Ii][Pp]ython" py-shell-name)
                     (string-match "epd\\|EPD" py-shell-name))
                    (replace-regexp-in-string "\n" ""
                                              (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))))))
    (if (and erg (string-match "^$" erg))
        (setq erg (substring erg (string-match "^$" erg)))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -W ignore" " -c \"import os; print(os.sep)\"")))))
    erg))

(defun pps-emacs-version ()
  "Include the appropriate `parse-partial-sexp' "
  (if (featurep 'xemacs)
      '(parse-partial-sexp (point-min) (point))
    '(syntax-ppss)))

(defun py-count-lines ()
  "Count lines in buffer, optional without given boundaries.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (save-restriction
    (widen)
    (if (featurep 'xemacs)
        (count-lines (point-min) (point-max))
      (count-matches "[\n\C-m]" (point-min) (point-max)))))

(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (syntax-ppss))
      (when (or (looking-at "\"")(looking-at "[ \t]*#[ \t]*"))
        (match-beginning 0))))

(provide 'python-components-nomacros)
;;; python-components-nomacros.el ends here
