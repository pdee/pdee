;;; python-components-macros.el --- Macro definitions

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

(defmacro empty-line-p ()
  "Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise."
  (interactive "p")
  `(save-excursion
     (progn
       (beginning-of-line)
       (looking-at "\\s-*$"))))

(defmacro py-escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

;; (defmacro py-continuation-line-p ()
;;   "Return t iff current line is a continuation line."
;;   `(save-excursion
;;      (beginning-of-line)
;;      (or (py-preceding-line-backslashed-p)
;;          (< 0 (nth 0 (syntax-ppss))))))

(defmacro py-count-lines ()
  "Count lines in buffer, optional without given boundaries.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (save-restriction
    (widen)
    `(if (featurep 'xemacs)
         (count-lines (point-min) (point-max))
       (count-matches "[\n\C-m]" (point-min) (point-max)))))

;; (defun py-in-string-or-comment-p ()
;;   "Returns beginning position if inside a string or comment, nil otherwise. "
;;   (interactive)
;;   (let* ((erg (nth 8 (if (featurep 'xemacs)
;;                          (parse-partial-sexp (point-min) (point))
;;                        (syntax-ppss))))
;;          (la (unless erg (when (or (looking-at "\"")(looking-at comment-start)(looking-at comment-start-skip))
;;                            (match-beginning 0)))))
;;     (setq erg (or erg la))
;;     (when (interactive-p) (message "%s" erg))
;;     erg))  a4643 (#o11043, #x1223, ?ሣ)

;; (defmacro py-in-string-or-comment-p ()
;;   "Returns beginning position if inside a string or comment, nil otherwise. "
;;   `(or (nth 8 (syntax-ppss))
;;        (when (or (looking-at "\"")(looking-at "[ \t]*#[ \t]*"))
;;          (match-beginning 0))))

(provide 'python-components-macros)
;;; python-components-macros.el ends here
