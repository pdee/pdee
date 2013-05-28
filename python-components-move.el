;;; python-components-move.el --- functions moving point

;; Copyright (C) 2011 Andreas Roehler

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

;;

;;; Code:
(require 'python-components-macros)
;; Expression
(defalias 'py-backward-expression 'py-beginning-of-expression)
(defun py-beginning-of-expression (&optional arg)
  "Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.
"
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (setq erg (py-beginning-of-expression-intern)))
      (setq arg (abs arg))
      (setq erg (py-end-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ((empty-line-p)
        (while
            (and (empty-line-p)(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (py-beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward py-expression-skip-chars)
        (while (or (looking-back (concat py-string-delim-re py-expression-looking-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
                   (looking-back (concat "[[:alnum:]_]*" py-operator-regexp "[ \t]*") (line-beginning-position) t))
          (goto-char (match-beginning 0))))
       ;; listed elements
       ((looking-back (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
        (goto-char (match-beginning 0))
        (while (looking-back (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
          (goto-char (match-beginning 0)))
        (unless (or (looking-back py-assignment-regexp) (looking-back "^[ \t]*"))
          (py-beginning-of-expression-intern orig)))
       ;; strings
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back py-assignment-regexp))
              ((looking-back py-operator-regexp)
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (py-beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (py-beginning-of-expression-intern orig)))
       ;; concatenated strings
       ((looking-back (concat py-string-delim-re py-expression-looking-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-looking-re py-string-delim-re))
        (goto-char (match-beginning 0))
        (while (looking-back (concat py-string-delim-re py-expression-looking-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
          (goto-char (match-beginning 0)))
        (skip-chars-backward py-expression-skip-chars))
       ;; before comment
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (py-beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back py-assignment-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (py-beginning-of-expression-intern orig))
       ((looking-back py-operator-regexp)
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'")
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ((looking-back "[\])}]")
        (forward-char -1)
        (unless (looking-back py-assignment-regexp)
          (py-beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back py-expression-looking-re)
        (skip-chars-backward py-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*") (looking-back py-assignment-regexp))
          (py-beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" py-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*")
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]"))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-looking-re))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((and (looking-at py-expression-looking-re) (not (looking-back "[ \t\r\n\f]")))
        (unless (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*="))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-expression-intern orig)))
      (unless (or (eq (point) orig)(looking-at "[ \t]*#"))
        (setq erg (point)))
      erg)))

(defalias 'py-forward-expression 'py-end-of-expression)
(defun py-end-of-expression (&optional arg)
  "Go to the end of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (while (< 0 arg)
            (setq erg (py-end-of-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (py-beginning-of-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-expression-intern (&optional orig)
  (unless (eobp)
    (let* ((orig (or orig (point)))
           (pps (syntax-ppss))
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
      (cond
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (forward-list))
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ;; in comment
       ((nth 4 pps)
        (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
        (py-end-of-expression-intern orig))
       ((empty-line-p)
        (while
            (and (empty-line-p)(not (eobp)))
          (forward-line 1))
        (py-end-of-expression-intern orig))
       ((looking-at (concat py-string-delim-re py-expression-looking-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-looking-re py-string-delim-re))
        (goto-char (match-end 0))
        (while (looking-at (concat py-operator-regexp py-string-delim-re py-expression-looking-re py-string-delim-re))
          (goto-char (match-end 0))))
       ;; inside string
       ((py-in-string-p)
        (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
          (goto-char (match-end 0)))
        (while
            (nth 3 (syntax-ppss))
          (forward-char 1))
        ;; (if (looking-at ":")
        ;; (forward-char 1)
        (unless (looking-at "[ \t]*$")
          (py-end-of-expression-intern orig)))
       ((looking-at "[(\[]")
        (forward-list)
        ;; (if (looking-at ":")
        ;; (forward-char 1)
        (unless (looking-at "[ \t]*$")
          (py-end-of-expression-intern orig)))
       ;; ((looking-at ":")
       ;; (forward-char 1)
       ;; (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
       ;; (py-end-of-expression-intern orig)))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
        (while (and (looking-at "[ \t]*#") (not (eobp)))
          (forward-line 1))
        (py-end-of-expression-intern orig))
       ((looking-at py-assignment-regexp)
        (goto-char (match-end 0))
        (if (looking-at "[(\[]")
            (forward-list 1)
          (py-end-of-expression-intern orig)))
       ((looking-at (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
        (goto-char (match-end 0))
        (while (looking-at (concat "[^ \t\n\r\f]+" py-delimiter-regexp))
          (goto-char (match-end 0)))
        (forward-char -1)
        (unless (looking-at (concat py-assignment-regexp "\\|[ \t]*$\\|" py-delimiter-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at (concat "[ \t]*" "[^ (\t\n\r\f]+" py-operator-regexp "[^ \t\n\r\f]+"))
        (goto-char (match-end 0))
        (while (looking-at (concat "[ \t]*" "[^ (\t]+" py-operator-regexp "[^ \t]+"))
          (goto-char (match-end 0)))
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at py-not-expression-regexp)
        (skip-chars-forward py-not-expression-chars)
        (unless (or (looking-at "[ \t]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ((looking-at py-expression-skip-regexp)
        (skip-chars-forward py-expression-skip-chars)
        (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at py-assignment-regexp))
          (py-end-of-expression-intern orig)))
       ;; ((looking-at ":")
       ;; (forward-char 1))
       )
      (unless (or (eq (point) orig)(and (eobp)(bolp)))
        (setq erg (point)))
      erg)))

;; Partial- or Minor Expression
(defalias 'py-backward-partial-expression 'py-beginning-of-partial-expression)
(defun py-beginning-of-partial-expression (&optional arg)
  "Go to the beginning of a minor python expression.

With numeric ARG do it that many times.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.

If already at the beginning or before a partial-expression, go to next partial-expression in buffer upwards "
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)

          (while (< 0 arg)
            (setq erg (py-beginning-of-partial-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (py-end-of-partial-expression (abs arg))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))



(defun py-beginning-of-partial-expression-intern (&optional orig)
  (unless (bobp)
    (let* ((orig (or orig (point)))
           (pps (syntax-ppss))
           erg)
      (cond
       ((and (empty-line-p)(not (bobp)))
        (while
            (and (empty-line-p)(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (py-beginning-of-partial-expression-intern orig))
       ;; this would up-list prematurely
       ;; ((nth 1 pps)
       ;; (goto-char (nth 1 pps)))
       ((nth 4 pps)
        (skip-chars-backward "^#")
        (while (nth 4 (syntax-ppss))
          (forward-char -1))
        (unless (bobp) (forward-char -1))
        (py-beginning-of-partial-expression-intern orig))
       ((looking-back ":")
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-chars)))
          (py-beginning-of-partial-expression-intern orig)))
       ((looking-back py-partial-expression-looking-regexp)
        (cond ((looking-back "\"\\|'")
               (forward-char -1)
               (skip-chars-backward "\"'")
               (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-chars)))
                 (py-beginning-of-partial-expression-intern orig)))
              ((looking-back "]\\|)")
               (forward-char -1)
               (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-backward-chars)))
                 (py-beginning-of-partial-expression-intern orig)))
              ((and (looking-back "(\\|\\[") (not (nth 8 pps)))
               (forward-char -1))
              (t (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-chars)))
                   (py-beginning-of-partial-expression-intern orig)))))
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (forward-line -1)
        (unless (bobp)
          (end-of-line)
          (py-beginning-of-partial-expression-intern orig)))
       ((looking-back py-delimiter-regexp)
        (skip-chars-backward " \t")
        (skip-chars-backward py-delimiter-chars)
        (forward-char -1)
        (skip-chars-backward py-partial-expression-skip-backward-chars))
       ((looking-back py-assignment-regexp)
        (goto-char (1- (match-beginning 0)))
        (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-backward-chars)))
          (py-beginning-of-partial-expression-intern orig)))
       ((looking-back py-operator-regexp)
        (goto-char (1- (match-beginning 0)))
        (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-backward-chars)))
          (py-beginning-of-partial-expression-intern orig)))
       ((looking-back ")\\|]")
        (goto-char (1- (match-beginning 0)))
        (unless (< 0 (abs (skip-chars-backward py-partial-expression-skip-backward-chars)))
          (py-beginning-of-partial-expression-intern orig)))
       ((looking-back "[ \.\t\r\n\f]")
        (skip-chars-backward py-not-partial-expression-skip-chars)
        ;; (skip-chars-backward py-partial-expression-skip-backward-chars))
        (py-beginning-of-partial-expression-intern orig))
       ((nth 9 pps)
        (goto-char (car (last (nth 9 pps)))))
       ((nth 8 pps)
        (when (nth 2 pps)
          (goto-char (nth 2 pps)))
        (goto-char (1- (nth 8 pps)))
        (py-beginning-of-partial-expression-intern orig))))
    (unless (eq (point) orig)
      (setq erg (point)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-partial-expression 'py-end-of-partial-expression)
(defun py-end-of-partial-expression (&optional arg)
  "Go to the end of a minor python expression.

With numeric ARG do it that many times.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (while (< 0 arg)
            (setq erg (py-end-of-partial-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (py-beginning-of-partial-expression arg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-partial-expression-intern (&optional orig)
  (unless (eobp)
    (let*
        ((orig (or orig (point)))
         (pps (syntax-ppss))
         erg
         ;; use by scan-lists
         parse-sexp-ignore-comments)
      (cond
       ((and (empty-line-p)(not (eobp)))
        (while
            (and (empty-line-p)(not (eobp)))
          (forward-line 1))
        (py-end-of-partial-expression-intern orig))
       ((and (eq (point) orig) (looking-at "[ \t\n\f\r]"))
        (skip-chars-forward " \t\r\n\f")
        (py-end-of-partial-expression-intern orig))
       ((looking-at "[ \t]*#")
        (while (and (looking-at "[ \t]*#") (forward-line 1)))
        (py-end-of-partial-expression-intern orig))
       ;; inside string
       ((nth 3 pps)
        (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
          (goto-char (match-end 0)))
        (or (< 0 (abs (skip-chars-forward "^ \t\r\n\f")))
            (while
                (and (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
                     (nth 3 (syntax-ppss))))))
       ;; in comment
       ((nth 4 pps)
        (while (or (nth 4 (syntax-ppss)) (looking-at comment-start))
          (forward-line 1)))
       ((member (char-after) (list ?\" ?\'))
        (let (forward-sexp-function)
          (forward-sexp 1)))
       ((looking-at py-operator-regexp)
        (goto-char (match-end 0))
        (when (< 0 (skip-chars-forward " \t\r\n\f"))
          (forward-char 1))
        (if (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (progn
              (goto-char (match-end 0))
              (py-end-of-partial-expression-intern orig))
          (unless (< 0 (abs (skip-chars-forward py-partial-expression-forward-regexp)))
            (py-end-of-partial-expression-intern orig))))
       ((looking-at py-partial-expression-looking-regexp)
        (unless (< 0 (abs (skip-chars-forward py-partial-expression-forward-regexp)))
          (when (looking-at "(")
            (forward-char 1))
          (py-end-of-partial-expression-intern orig)))
       ((looking-at py-not-partial-expression-regexp)
        (if (and (< orig (point)) (looking-at ")"))
            (forward-char 1)
          (skip-chars-forward py-not-partial-expression-skip-chars)
          (cond ((nth 4 (syntax-ppss))
                 (while (or (nth 4 (syntax-ppss)) (looking-at comment-start))
                   (forward-line 1))
                 (py-end-of-expression-intern orig))
                ((looking-at "\"\"\"\\|'''\\|\"\\|'")
                 (goto-char (match-end 0))
                 (py-end-of-partial-expression-intern orig))
                ((looking-at py-operator-regexp)
                 (goto-char (match-end 0))
                 (py-end-of-partial-expression-intern orig))
                (t (unless (< 0 (abs (skip-chars-forward py-partial-expression-forward-regexp)))
                     (py-end-of-partial-expression-intern orig))))))
       ((and (eq (point) orig) (not (eobp)))
        (forward-char 1)
        (py-end-of-partial-expression-intern orig))
       ((nth 9 pps)
        (goto-char (car (last (nth 9 pps))))
        (forward-list)
        (py-end-of-partial-expression-intern orig))
       ((and (nth 1 pps) (<= orig (nth 1 pps)))
        (goto-char (nth 1 pps))(forward-list)
        (py-end-of-partial-expression-intern orig))
       ((and (ignore-errors (<= orig (nth 2 pps))))
        (goto-char (nth 2 pps))
        (skip-chars-forward py-partial-expression-forward-regexp)
        (py-end-of-partial-expression-intern orig)))
      (unless (or (eq (point) orig)(and (eobp)(bolp)))
        (setq erg (point)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;;; Line
(defun py-beginning-of-line ()
  "Go to beginning-of-line, return position.

If already at beginning-of-line and not at BOB, go to beginning of previous line. "
  (interactive)
  (unless (bobp)
    (let ((erg
           (if (bolp)
               (progn
                 (forward-line -1)
                 (progn (beginning-of-line)(point)))
             (progn (beginning-of-line)(point)))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-end-of-line ()
  "Go to end-of-line, return position.

If already at end-of-line and not at EOB, go to end of next line. "
  (interactive)
  (unless (eobp)
    (let ((erg
           (if (eolp)
               (progn
                 (forward-line 1)
                 (progn (end-of-line)(point)))
             (progn (end-of-line)(point)))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;;; Statement
(defalias 'py-backward-statement 'py-beginning-of-statement)
(defalias 'py-previous-statement 'py-beginning-of-statement)
(defalias 'py-statement-backward 'py-beginning-of-statement)
(defun py-beginning-of-statement (&optional orig done)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html
"
  (interactive)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (cui (current-indentation))
          (pps (syntax-ppss))
          (done done)
          erg)
      (cond
       ((or (empty-line-p)(nth 8 pps))
        ;; when travelling large sections of empty or comment lines
        ;; recursive calls might run into `max-specpdl-size' error
        (while (and (not (bobp)) (or (empty-line-p) (setq this (nth 8 (syntax-ppss)))))
          (if (empty-line-p)
              (skip-chars-backward " \t\r\n\f")
            (goto-char this)
            (skip-chars-backward " \t\r\n\f")))
        (py-beginning-of-statement orig done))
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (py-beginning-of-statement orig done))
       ((nth 1 pps)
        (goto-char (1- (nth 1 pps)))
        (setq done t)
        (py-beginning-of-statement orig done))
       ((py-preceding-line-backslashed-p)
        (forward-line -1)
        (back-to-indentation)
        (setq done t)
        (py-beginning-of-statement orig done))
       ((looking-at py-string-delim-re)
        (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
          (setq done t))
        (back-to-indentation)
        (py-beginning-of-statement orig done))
       ((and (eq (point) orig)(looking-back ";[ \t]*"))
        (goto-char (match-beginning 0))
        (skip-chars-backward ";")
        (py-beginning-of-statement orig done))
       ((and (not (eq (point) orig))(looking-back ";[ \t]*"))
        (setq erg (point)))
       ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
        ;; (setq done t)
        (py-beginning-of-statement orig done))
       ((not (eq (current-column) (current-indentation)))
        (if (< 0 (abs (skip-chars-backward "^;\t\r\n\f")))
            (progn
              (setq done t)
              (back-to-indentation)
              (py-beginning-of-statement orig done))
          (back-to-indentation)
          (setq done t)
          (py-beginning-of-statement orig done)))
       ((looking-at "[ \t]*#")
        (skip-chars-backward " \t\r\n\f")
        (setq done t)
        (py-beginning-of-statement orig done)))
      (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
        (when (< (point) orig)(setq erg (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-eos-handle-comment-start ()
  (end-of-line)
  (forward-comment 99999)
  ;; (skip-chars-forward (concat "^" comment-start) (line-end-position))
  ;; (skip-chars-backward " \t\r\n\f" (line-beginning-position))
  )

(defun py-eos-handle-string-start ()
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (or (if (save-match-data (string-match "'" (match-string-no-properties 0))) (ignore-errors (forward-sexp))) (while (and (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss))))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-eos-handle-doublequoted-string-start ()
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (while (and (not (eobp)) (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss)))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-eos-handle-singlequoted-string-start ()
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (ignore-errors (forward-sexp)) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-handle-eol ()
  (skip-chars-backward " \t\r\n\f" (line-beginning-position))
  (when (py-beginning-of-comment)
    (skip-chars-backward " \t\r\n\f" (line-beginning-position))))

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py-end-of-statement (&optional orig done origline)
  "Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "
  (interactive)
  (unless (eobp)
    (let ((pps (syntax-ppss))
          (origline (or origline (py-count-lines)))
          (orig (or orig (point)))
          erg this
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm)

      (cond
       ((nth 1 pps)
        (when (< orig (point))
          (setq orig (point)))
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (if (ignore-errors (forward-list))
              (progn
                (when (looking-at ":[ \t]*$")
                  (forward-char 1))
                (setq done t)
                (skip-chars-forward (concat "^" comment-start) (line-end-position))
                (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                (py-end-of-statement orig done origline))
            (goto-char orig))))
       ((and (nth 8 pps)(nth 3 pps))
        (goto-char (nth 8 pps))
        (unless (looking-back "^[ \t]*")
          (setq stm t))
        (when (looking-at "'''\\|'")
          (py-eos-handle-singlequoted-string-start))
        (when (looking-at "\"\"\"\\|\"")
          (py-eos-handle-doublequoted-string-start))
        (when stm (setq done t))
        (setq stm nil)
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ;; in comment
       ((nth 4 pps)
        (unless (eobp)
          (skip-chars-forward (concat "^" comment-start) (line-end-position))
          (forward-comment 99999)
          (py-handle-eol)
          (py-end-of-statement orig done origline)))
       ((py-current-line-backslashed-p)
        (end-of-line)
        (py-handle-eol)
        (when (and (eq (char-before (point)) ?\\ )
                   (py-escaped))
          (forward-line 1))
        (py-end-of-statement orig done origline))
       ((and (not done)(looking-at "[ \t]*#"))
        (py-eos-handle-comment-start)
        (py-end-of-statement orig done origline))
       ((looking-at "'''\\|'")
        (py-eos-handle-singlequoted-string-start)
        ;; string not terminated
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ((looking-at "\"\"\"\\|\"")
        (py-eos-handle-doublequoted-string-start)
        ;; string not terminated
        (unless (nth 3 (syntax-ppss))
          (py-end-of-statement orig done origline)))
       ((looking-at py-string-delim-re)
        (py-eos-handle-string-start)
        (py-end-of-statement orig done origline))
       ((and (looking-at py-no-outdent-re)(not (nth 8 pps)))
        (end-of-line)
        (py-handle-eol))
       ((and (eq (point) orig) (< (current-column) (current-indentation)))
        (back-to-indentation)
        (py-end-of-statement orig done origline))
       ((and (not done)
             (< 0 (abs (skip-chars-forward (concat "^" comment-start) (line-end-position)))))
        (py-handle-eol)
        ;; with trailing whitespaces at orig
        (if (and (< orig (point)) (not (progn (setq pps (syntax-ppss))(or (nth 8 pps)(nth 1 pps)))))
            (setq done t)
          (if (or (nth 8 pps)(nth 1 pps))
              (py-end-of-statement orig done origline)
            (forward-line 1)
            (py-handle-eol)))
        (setq done t)
        (py-end-of-statement orig done origline))
       ((and (not done) (< 0 (skip-chars-forward " \t\r\n\f")))
        (when (looking-at "[ \t]*#")
          (py-eos-handle-comment-start))
        (py-end-of-statement orig done origline))
       ((py-current-line-backslashed-p)
        (skip-chars-forward " \t\r\n\f")
        (skip-chars-forward (concat "^" comment-start) (line-end-position))
        (py-beginning-of-comment)
        (skip-chars-backward " " (line-beginning-position))
        (setq done t)
        (py-end-of-statement orig done origline))
       ((and (not done) (eq (point) orig)(looking-at ";"))
        (skip-chars-forward ";" (line-end-position))
        (when (< 0 (skip-chars-forward (concat "^" comment-start) (line-end-position)))
          (py-beginning-of-comment)
          (skip-chars-backward " \t\r\n\f")
          (setq done t))
        (py-end-of-statement orig done origline))
       ((bolp)
        (end-of-line)
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f")
        (setq done t)
        (py-end-of-statement orig done origline))
       ((and (not (ignore-errors (eq (point) done)))(looking-back py-string-delim-re) (progn (goto-char (match-beginning 0))(and (nth 8 (syntax-ppss))(nth 3 (syntax-ppss)))))
        (end-of-line)
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f")
        (setq done (point))
        (py-end-of-statement orig done origline))
       ((and done (eq (current-column) (current-indentation)))
        (skip-chars-forward (concat "^" comment-start) (line-end-position))
        (skip-chars-backward " \t\r\n\f")
        (py-beginning-of-comment)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (setq done t)
        (py-end-of-statement orig done origline)))
      (unless
          (or
           (eq (point) orig)
           (empty-line-p))
        (setq erg (point)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-goto-statement-below ()
  "Goto beginning of next statement. "
  (interactive)
  (let ((orig (point))
        (erg (py-end-of-statement)))
    (py-beginning-of-statement)
    (when (< (point) orig)
      (goto-char erg)
      (py-end-of-statement)
      (py-beginning-of-statement))))

;;; Mark forms
(defun py-mark-base (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-mark-paragraph ()
  "Mark paragraph at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "paragraph"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-clause ()
  "Mark clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-class (&optional arg)
  "Mark class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-line ()
  "Mark line at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "line"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "partial-expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;;; Decorator
(defun py-beginning-of-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful "
  (interactive)
  (back-to-indentation)
  (while (and (not (looking-at "@\\w+"))(not (empty-line-p))(not (bobp))(forward-line -1))
    (back-to-indentation))
  (let ((erg (when (looking-at "@\\w+")(match-beginning 0))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-decorator ()
  "Go to the end of a decorator.

Returns position if succesful "
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-beginning-of-decorator)))
    (when erg
      (if
          (re-search-forward py-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            (py-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (py-in-list-p)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;;; Copying

(defalias 'py-expression 'py-copy-expression)
(defun py-copy-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons.
See also py-partial-expression. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-minor-expression 'py-copy-partial-expression)
(defalias 'py-partial-expression 'py-copy-partial-expression)
(defun py-copy-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

Given the function below, `py-partial-expression'
called at pipe symbol would copy and return:

def usage():
    print \"\"\"Usage: %s
    ....\"\"\" % (
        os.path.basename(sys.argv[0]))
------------|-------------------------
==> path

        os.path.basename(sys.argv[0]))
------------------|-------------------
==> basename(sys.argv[0]))

        os.path.basename(sys.argv[0]))
--------------------------|-----------
==> sys

        os.path.basename(sys.argv[0]))
------------------------------|-------
==> argv[0]

while `py-expression' would copy and return

\(
 os.path.basename(sys.argv[0]))

;;;;;

Also for existing commands a shorthand is defined:

\(defalias 'py-statement 'py-copy-statement)"

  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-statement 'py-copy-statement)
(defun py-copy-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "statement")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block 'py-copy-block)
(defun py-copy-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block-or-clause 'py-copy-block-or-clause)
(defun py-copy-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-def 'py-copy-def)
(defun py-copy-def (&optional arg)
  "Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-def-or-class (&optional arg)
  "Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def-or-class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-class 'py-copy-class)
(defun py-copy-class (&optional arg)
  "Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-clause 'py-copy-clause)
(defun py-copy-clause ()
  "Mark clause at point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

;; Deleting
(defun py-kill-expression ()
  "Delete expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-partial-expression ()
  "Delete partial-expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions."
  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-region (region-beginning) (region-end))))

(defun py-kill-def-or-class ()
  "Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def ()
  "Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "clause")))
    (kill-region (car erg) (cdr erg))))

;;; Helper functions

(defun py-forward-line (&optional arg)
  "Goes to end of line after forward move.

Travels right-margin comments. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (forward-line arg)
    (end-of-line)
    (skip-chars-backward " \t")
    (py-beginning-of-comment)
    (skip-chars-backward " \t")))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro `py-beginning-of-comment' instead "
  (interactive)
  (let ((erg (py-beginning-of-comment)))
    (when (and py-verbose-p (interactive-p))
      (message "%s" erg))))

(defun py-go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (py-beginning-of-statement)
      (if (and (looking-at regexp)(if maxindent
                                      (<= (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py-go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (py-beginning-of-statement)
      (if (and (looking-at regexp)(if maxindent
                                      (<= (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py-go-to-keyword-above (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent (or maxindent (and (< 0 (current-indentation))(current-indentation))
                       ;; make maxindent large enough if not set
                       (* 99 py-indent-offset)))
        (first t)
        done erg cui)
    (while (and (not done) (not (bobp)))
      (py-beginning-of-statement)
      (if (and (looking-at regexp)(if maxindent
                                      (< (current-indentation) maxindent) t))
          (progn
            (setq erg (point))
            (setq done t))
        (when (and first (not maxindent))
          (setq maxindent (current-indentation))
          (setq first nil))))
    (when erg
      (if (looking-at regexp)
          (setq erg (cons (current-indentation) erg))
        (setq erg nil
              )))
    erg))

(defun py-clause-lookup-keyword (regexp arg &optional indent orig origline)
  "Returns a list, whose car is indentation, cdr position. "
  (let* ((orig (or orig (point)))
         (origline (or origline (py-count-lines)))
         (stop (if (< 0 arg)'(eobp)'(bobp)))
         (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
         (count 1)
         (maxindent (cond (indent indent)
                          ((< (py-count-lines) origline)
                           (current-indentation))
                          (t 0)))
         (complement-re
          (cond ((or (string-match "finally" regexp)
                     (string-match "except" regexp))
                 py-try-re)
                ((string-match "elif" regexp)
                 py-if-re)
                ((string-match "else" regexp)
                 py-minor-block-re)))
         (first t)
         erg done strict)
    (while (and (not (eval stop))
                (< 0 count)
                (or done (setq erg (funcall function))))
      (setq done nil)
      (when (and first (< maxindent (current-indentation)))
        (setq maxindent (current-indentation))
        (setq first nil))
      (when (if strict
                (< (current-indentation) maxindent)
              (<= (current-indentation) maxindent))
        (unless (looking-at py-block-or-clause-re)
          (setq maxindent (current-indentation)))
        ;; (message "%s %s" count indent)
        ;; nesting
        (cond
         ((and (looking-at "\\_<finally\\>[: \n\t]")(save-match-data (string-match regexp "finally")))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try"))))))
         ;; ((and (looking-at "\\<except\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
         ;;  (setq indent (current-indentation))
         ;;  (setq count (1+ count))
         ;;  (while
         ;;      (and
         ;;       (not (eval stop))
         ;;       (funcall function)
         ;;       (setq done t)
         ;;       (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\<else\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<else\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<elif\\>[ \n\t]")(save-match-data (string-match "elif" regexp)))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               ;; doesn't mean nesting yet
               (setq count (1- count))
               (not (and (eq indent (current-indentation)) (looking-at "if"))))))
         ((and (looking-at complement-re)(<= (current-indentation) maxindent))
          (setq count (1- count)))
         (t (cond ((and (string-match "except" regexp)(looking-at py-block-re))
                   (setq count (1- count)))
                  ((and (string-match "else" regexp)(looking-at "except"))
                   (current-indentation))
                  (t
                   (setq strict t)
                   ))))))
    (when erg
      (if (looking-at py-def-or-class-re)
          (setq erg (cons (+ (current-indentation) py-indent-offset) erg))
        (setq erg (cons (current-indentation) erg))))
    erg))

(defun py-leave-comment-or-string-backward (&optional pos)
  "If inside a comment or string, leave it backward. "
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (syntax-ppss))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside."
  (interactive "p")
  (let* ((orig (or orig (point)))
         (ppstart (or ppstart (re-search-backward "^[a-zA-Z]" nil t 1) (point-min)))
         erg)
    (unless done (goto-char orig))
    (setq done t)
    (if
        (setq erg (nth 1 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (syntax-ppss))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      (when iact (message "%s" last))
      last)))

(defun py-beginning-of-list (&optional iact orig limit done last)
  "Go to beginning of any parentized, braced or bracketed expression in statement. "
  (interactive "p")
  (save-restriction
    (let ((orig (or orig (point)))
          (done done)
          (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
          (last last))
      (unless (or done (not limit)) (narrow-to-region limit (point-max)))
      (setq done t)
      (goto-char orig)
      (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
             (br (car-safe (ar-in-braced-p-atpt)))
             (bk (car-safe (ar-in-bracketed-p-atpt)))
             (erg (car (sort (delq nil (list pt br bk)) '<))))
        (if erg
            (progn
              (goto-char (1- erg))
              (setq last erg)
              (py-beginning-of-list iact (1- erg) limit done last))
          (when last
            (goto-char last))
          (when iact (message "%s" last))
          last)))))

(defun py-end-of-list (&optional iact orig limit done last)
  "Go to end of any parentized, braced or bracketed expression in statement. "
  (interactive "p")
  (save-restriction
    (let ((orig (or orig (point)))
          (done done)
          (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
          (last last))
      (unless (or done (not limit)) (narrow-to-region limit (point-max)))
      (setq done t)
      (goto-char orig)
      (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
             (br (car-safe (ar-in-braced-p-atpt)))
             (bk (car-safe (ar-in-bracketed-p-atpt)))
             (erg (car (sort (delq nil (list pt br bk)) '<))))
        (if erg
            (progn
              (goto-char (1- erg))
              (setq last erg)
              (py-end-of-list iact (1- erg) limit done last))
          (when last
            (goto-char last)
            (match-paren)
            (setq last (1+ (point)))
            (when iact (message "%s" last))
            last))))))

(defun py-forward-into-nomenclature (&optional arg iact)
  "Move forward to end of a nomenclature section or word.

With \\[universal-argument] (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((case-fold-search nil)
        (orig (point))
        erg)
    (if (> arg 0)
        (while (and (not (eobp)) (> arg 0))
          ;; (setq erg (re-search-forward "\\(\\W+[_[:lower:][:digit:]ß]+\\)" nil t 1))
          (cond
           ((or (not (eq 0 (skip-chars-forward "[[:blank:][:punct:]\n\r]")))
                (not (eq 0 (skip-chars-forward "_"))))
            (when (or
                   (< 1 (skip-chars-forward "[:upper:]"))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
                   (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
              (setq arg (1- arg))))
           ((or
             (< 1 (skip-chars-forward "[:upper:]"))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]ß]")))
             (not (eq 0 (skip-chars-forward "[[:lower:][:digit:]]"))))
            (setq arg (1- arg)))))
      (while (and (not (bobp)) (< arg 0))
        (when (not (eq 0 (skip-chars-backward "[[:blank:][:punct:]\n\r\f_]")))

          (forward-char -1))
        (or
         (not (eq 0 (skip-chars-backward "[:upper:]")))
         (not (eq 0 (skip-chars-backward "[[:lower:][:digit:]ß]")))
         (skip-chars-backward "[[:lower:][:digit:]ß]"))
        (setq arg (1+ arg))))
    (if (< (point) orig)
        (progn
          (when (looking-back "[[:upper:]]")
            ;; (looking-back "[[:blank:]]"
            (forward-char -1))
          (if (looking-at "[[:alnum:]ß]")
              (setq erg (point))
            (setq erg nil)))
      (if (and (< orig (point)) (not (eobp)))
          (setq erg (point))
        (setq erg nil)))
    (when (and py-verbose-p (or iact (interactive-p))) (message "%s" erg))
    erg))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (setq arg (or arg 1))
  (py-forward-into-nomenclature (- arg) arg))

(defalias 'py-match-paren 'match-paren)

(defun match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg \C-u insert a `%'. "
  (interactive "P")
  (let ((parse-sexp-ignore-comments t))
    (if arg
        (self-insert-command (if (numberp arg) arg 1))
      (cond
       ((and (not match-paren-no-use-syntax-pps) (looking-at "\\s("))
        (forward-list 1)
        (backward-char 1))
       ((and (not match-paren-no-use-syntax-pps)(looking-at "\\s)"))
        (forward-char 1) (backward-list 1))
       ;; if match-paren-no-syntax-pps
       ((looking-at "(")
        (ar-parentized-end-atpt))
       ((looking-at ")")
        (ar-parentized-beginning-atpt))
       ((looking-at "\\\[")
        (ar-bracketed-end-atpt))
       ((looking-at "]")
        (ar-bracketed-beginning-atpt))
       ((looking-at "{")
        (ar-braced-end-atpt))
       ((looking-at "}")
        (ar-braced-beginning-atpt))
       (t (self-insert-command 1))))))

(defun py-travel-current-indent (indent &optional orig)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (unless (eobp)
    (let ((orig (or orig (point)))
          last else)
      (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                  (save-excursion (or (<= indent (progn  (py-beginning-of-statement)(current-indentation)))(eq last (line-beginning-position))))
                  (py-end-of-statement-p)))
      (goto-char last)
      (when (< orig last)
        last))))

(provide 'python-components-move)
;;; python-components-move.el ends here
