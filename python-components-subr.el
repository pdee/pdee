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

(defcustom py-empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :tag "py-empty-line-p-chars"
  :group 'python-mode)

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


(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer.
Return nil otherwise. "
  (when (bobp)(point)))

(defun py-toggle-py-debug-p ()
  "Toggle value of ‘py-debug-p’."
  (interactive)
  (setq py-debug-p (not py-debug-p))
  (when (called-interactively-p 'interactive) (message "py-debug-p: %s" py-debug-p)))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped-p))))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (and (nth 4 pps) (nth 8 pps))))

(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (when (or (looking-at "\"") (looking-at "[ \t]*#[ \t]*"))
        (point))))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon at the same line.

Returns ‘t’ if point was moved"
  (let ((orig (point)))
    (skip-chars-backward "^;" (or limit (line-beginning-position)))
    (skip-chars-forward " \t" (line-end-position))
    (< (point) orig)))

(defun py-forward-comment ()
  "Go to the end of commented section at point."
  (interactive)
  (let (last)
    (while
        (and (not (eobp))
             (or
              (and comment-start (looking-at comment-start))
              (and comment-start-skip (looking-at comment-start-skip))
              (nth 4 (parse-partial-sexp (point-min) (point)))))
      (setq last (line-end-position))
      (forward-line 1)
      (skip-chars-forward " \t\r\n\f")
      (unless (or (eobp) (eq (point) last))
        (back-to-indentation)))
    (when last (goto-char last))))

(defun py--forward-string-maybe (&optional start)
  "Go to the end of string.

Expects START position of string
Return position of moved, nil otherwise."
  (let ((orig (point)))
    (when start (goto-char start)
          (when (looking-at "\"\"\"\\|'''")
            (goto-char (1- (match-end 0)))
            (forward-sexp))
          ;; maybe at the inner fence
          (when (looking-at "\"\"\\|''")
            (goto-char (match-end 0)))
          (and (< orig (point)) (point)))))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside or at comment-start. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (ignore-errors (looking-at (concat "[ \t]*" comment-start)))
          (setq erg (point))))
      erg)))

(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (nth 1 (parse-partial-sexp (or start (point-min)) (point))))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (parse-partial-sexp ppstart (point)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" end))
    end))


(defun py-backward-comment ()
  "Got to beginning of a commented section.

Start from POS if specified"
  (interactive)
  (let ((last (point))
        (orig (point)))
    (while (and (not (bobp))
                (ignore-errors (< (ignore-errors (goto-char (py-in-comment-p))) last)))
      (setq last (point))
      (skip-chars-backward " \t\r\n\f"))
    (and (< (point) orig) (< (point)  last) (goto-char last))))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro ‘py-backward-comment’ instead"
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
          (py-backward-comment))))

(provide 'python-components-subr)
;;; python-components-subr.el ends here
