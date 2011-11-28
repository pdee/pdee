;;; python-components-completion.el --- complete symbol at point

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

;; This completion code was been written by Dave Love
;;; Code:


;; Fixme: This fails the first time if the sub-process isn't already
;; running.  Presumably a timing issue with i/o to the process.
(defun python-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Python process.
The list is sorted.
Uses `python-imports' to load modules against which to complete."
  (when (stringp symbol)
    (let ((python-imports (or
                           python-imports
                           (python-find-imports)
                           "\"import string, re, sys, os, pdb, random, time\\n\""))
          (completions
	   (condition-case ()
	       (car (read-from-string
		     (python-send-receive
		      (format "emacs.complete(%S,%s)"
			      (substring-no-properties symbol)
			      python-imports))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun py-completion-at-point ()
  (interactive "*")
  (let* ((end (point))
         (start (save-excursion
                  (and (re-search-backward
                        (rx (or buffer-start (regexp "[^[:alnum:]._]"))
                            (group (1+ (regexp "[[:alnum:]._]"))) point)
                        nil t)
                       (match-beginning 1))))
         (completion (when start
                        (python-symbol-completions (buffer-substring-no-properties start end)))))
    (when completion (delete-region start end)
          (insert (car completion)))))
    ;; (list start end
    ;; (completion-table-dynamic 'python-symbol-completions)))))

;; https://github.com/fgallina/python.el
(defun python-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point-marker)))
      (indent-for-tab-command)
    (py-completion-at-point)
    ;; (comint-dynamic-complete)
))

(provide 'python-components-completion)
;;; python-components-completion.el ends here
