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

(defun py-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

Returns position if succesful. "
  (save-restriction
    (widen)
    (let ((pps (syntax-ppss)))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

;; (defun py-beginning-of-comment ()
;;   "Go to the beginning of current line's comment, if any.
;;
;; Returns position if succesful. "
;;   (interactive)
;;   (save-restriction
;;     (widen)
;;     (if (looking-at comment-start)
;;         (point)
;;       (let ((pps (parse-partial-sexp (line-beginning-position) (point))))
;;         (and (nth 4 pps)
;;              (goto-char (nth 8 pps)))))))

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
                    (setq erg (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))
                    (setq erg (replace-regexp-in-string "\n" "" erg))
                    (when (string-match "^$" erg)
                      (setq erg (substring erg (string-match "^$" erg)))))
                   (t (setq erg (shell-command-to-string (concat py-shell-name " -W ignore" " -c \"import os; print(os.sep)\"")))))))
    (replace-regexp-in-string "\n" "" erg)))

(defun pps-emacs-version ()
  "Include the appropriate `parse-partial-sexp' "
  (if (featurep 'xemacs)
      '(parse-partial-sexp (point-min) (point))
    '(syntax-ppss)))

(defun empty-line-p ()
  "Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise."
  (interactive "p")
  (save-excursion
    (progn
      (beginning-of-line)
      (looking-at "\\s-*$"))))

(defun py-escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  (save-excursion
    (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defun py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (and (eq (char-before (point)) ?\\ )
         (py-escaped))))

(defun py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\r\n\f")
    (and (eq (char-before (point)) ?\\ )
         (py-escaped))))

(defun py-continuation-line-p ()
  "Return t iff current line ends with a backslash. "
  (save-excursion
    (end-of-line)
    (and (eq (char-before (point)) ?\\ )
         (py-escaped))))

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
