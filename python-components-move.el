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

;; backward compatibility
;; some third party relying on v5 serie might use this
(defalias 'py-goto-beyond-block 'py-end-of-block-bol)
(defalias 'py-goto-beyond-final-line 'py-end-of-statement-bol)

;; Expression
(defalias 'py-backward-expression 'py-beginning-of-expression)
(defun py-beginning-of-expression (&optional arg)
  "Go to the beginning of a compound python expression.

With numeric ARG do it that many times.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

If already at the beginning or before a expression, go to next expression in buffer upwards

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."
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
       ( ;; (empty-line-p)
        (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
             (eq 9 (char-after))(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (py-beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward py-expression-skip-chars)
        (while (or (looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
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
       ((looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
        (goto-char (match-beginning 0))
        (while (looking-back (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp) (line-beginning-position) t)
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
       ((looking-back py-expression-re)
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
       ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-re))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward py-expression-skip-chars)))
          (py-beginning-of-expression-intern orig)))
       ((and (looking-at py-expression-re) (not (looking-back "[ \t\r\n\f]")))
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
       ( ;; (empty-line-p)
 (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
  (eq 9 (char-after))(not (eobp)))
          (forward-line 1))
        (py-end-of-expression-intern orig))
       ((looking-at (concat py-string-delim-re py-expression-re py-string-delim-re py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
        (goto-char (match-end 0))
        (while (looking-at (concat py-operator-regexp py-string-delim-re py-expression-re py-string-delim-re))
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

(defun py-beginning-of-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-backward py-partial-expression-forward-chars)
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-forward py-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (interactive-p) (message "%s" erg))
    erg))

;; Partial- or Minor Expression
(defalias 'py-backward-partial-expression 'py-beginning-of-partial-expression)

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
(defun py-beginning-of-statement (&optional orig done limit)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (this (point))
             (cui (current-indentation))

             (pps (progn (goto-char this)
                         (parse-partial-sexp (or limit (point-min))(point))))
             (done done)
             erg)
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-beginning-of-statement orig done limit))
         ((nth 8 pps)
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (py-beginning-of-statement orig done limit))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
          (setq done t)
          (py-beginning-of-statement orig done limit))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-beginning-of-statement orig done limit))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (py-beginning-of-statement orig done limit)))
         ((looking-at "[ \t]*#")
          (skip-chars-backward (concat "^" comment-start) (line-beginning-position))
          (back-to-indentation)
          (unless (bobp)
            (py-beginning-of-statement orig done limit)))
         ((and (not done) (looking-at py-string-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (py-beginning-of-statement orig done limit))
         ((and (not (eq (point) orig))(looking-back "^[ \t]*"))
          (setq erg (point)))
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          ;; (setq done t)
          (py-beginning-of-statement orig done limit))
         ((not (eq (current-column) (current-indentation)))
          (if (< 0 (abs (skip-chars-backward "^\t\r\n\f")))
              (progn
                (setq done t)
                (back-to-indentation)
                (py-beginning-of-statement orig done limit))
            (back-to-indentation)
            (setq done t)
            (py-beginning-of-statement orig done limit))))
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defun py-eos-handle-comment-start ()
  (end-of-line)
  (forward-comment 99999))

(defun py-eos-handle-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (or (if (save-match-data (string-match "'" (match-string-no-properties 0))) (ignore-errors (goto-char (scan-sexps (point) 1)))) (while (and (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss))))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

;; (defun py-eos-handle-doublequoted-string-start (this)
;;   "Internal use, find possible end of statement from string start. "
;;   (when
;;       (and (setq this (point)) (progn (while (and (not (eobp)) (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss)))) (< this (point))))
;;     (skip-chars-forward (concat "^" comment-start "\\|" (match-string-no-properties 0)) (line-end-position))
;;     (skip-chars-backward " \t\r\n\f")))

(defun py-eos-handle-doublequoted-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (while (and (not (eobp)) (search-forward (match-string-no-properties 0) nil t 1) (nth 8 (syntax-ppss)))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start "\\|" (match-string-no-properties 0)) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-eos-handle-singlequoted-string-start (this)
  "Internal use, find possible end of statement from string start. "
  (when
      (and (setq this (point)) (progn (ignore-errors (goto-char (scan-sexps (point) 1))) (< this (point))))
    (skip-chars-forward (concat "^" comment-start) (line-end-position))
    (skip-chars-backward " \t\r\n\f")))

(defun py-handle-eol ()
  (skip-chars-backward " \t\r\n\f" (line-beginning-position))
  (when (py-beginning-of-comment)
    (skip-chars-backward " \t\r\n\f" (line-beginning-position))))

(defun py--skip-to-comment-or-semicolon ()
  (and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
       (if (eq ?\; (char-after))
           (skip-chars-forward ";" (line-end-position))
         (skip-chars-backward " \t" (line-beginning-position)))
       (setq done t)))

(defun py--eos-in-string ()
  "Return stm, i.e. if string is part of a (print)-statement. "
  (let ((orig (point))
        pos stm)
    (goto-char (nth 8 pps))
    (unless (looking-back "^[ \t]*")
      (setq stm t))
    ;; go to end of string
    (and (member (char-after) (list ?' ?\"))
         (ignore-errors (setq pos (scan-sexps (point) 1)))
         (goto-char pos))
    ;; if no closing string delimiter, pos doesn't exist
    (unless (or stm (not pos))
      (setq done t)
      (unless (eq 10 (char-after))
        (and (< 0 (abs (skip-chars-forward "^;#" (line-end-position))))
             (eq ?\; (char-after))
             (skip-chars-forward ";"))))
    stm))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list ?# 10))(forward-line 1)(back-to-indentation))))

(defun py--end-of-statement-intern ()
  (py--skip-to-comment-or-semicolon)
  (let ((pos (point))
        (pps (syntax-ppss))
        stm)
    (cond ((nth 3 pps)
           (and (py--eos-in-string) (py--end-of-statement-intern)))
          ((nth 4 pps)
           (py--end-of-comment-intern pos))
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
                   (skip-chars-forward "^#" (line-end-position))
                   (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                   (py-end-of-statement orig done repeat))
               (goto-char orig)))))))

(defun py-end-of-statement (&optional orig done repeat)
  "Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps)
      (unless done
        (py--skip-to-comment-or-semicolon))
      (setq pps (syntax-ppss))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; wich-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
        (error "py-end-of-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
       ;; list
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
                (skip-chars-forward "^#" (line-end-position))
                (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                (py-end-of-statement orig done repeat))
            (goto-char orig))))
       ;; string
       ((nth 3 pps)
        (and (py--eos-in-string) (py--end-of-statement-intern))
        (setq pps (syntax-ppss))
        (unless (and done (not (or (nth 1 pps) (nth 8 pps)))) (py-end-of-statement orig t repeat)))
       ;; in comment
       ((nth 4 pps)
        (py--end-of-comment-intern (point))
        (py--skip-to-comment-or-semicolon)
        ;; (and (not done)
        ;;      (< 0 (abs (skip-chars-forward "^;#" (line-end-position))))
        ;;      (or (and (eq ?\; (char-after))
        ;;               (skip-chars-forward ";"))
        ;;          (skip-chars-backward " \t")))
        ;; travle backslashed lines
        (while (and (eq (char-before (point)) ?\\ )
                    (py-escaped)(setq last (point)))
          (forward-line 1)(end-of-line))
        (and last (goto-char last)
             (forward-line 1)
             (back-to-indentation))
        (py-end-of-statement orig t repeat))
       ((py-current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\ )
                    (py-escaped))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (py-end-of-statement orig done repeat)))
       ((eq (current-indentation) (current-column))
        (or (py--skip-to-comment-or-semicolon)
            (forward-char 1))
        (setq pps (syntax-ppss))
        (unless done (py--end-of-statement-intern)
                (py-end-of-statement orig done repeat)))

       ((eq orig (point))
        (skip-chars-forward " \t\r\n\f#'\"")
        (py--skip-to-comment-or-semicolon)
        (py--end-of-statement-intern)
        (py-end-of-statement orig done repeat)))
      (unless
          (or
           (eq (point) orig)
           (member (char-before) (list 10 32 9)))
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

;; (defun py-mark-paragraph ()
;;   "Mark paragraph at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "paragraph"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-block ()
;;   "Mark block at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "block"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-clause ()
;;   "Mark clause at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "clause"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-block-or-clause ()
;;   "Mark block-or-clause at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "block-or-clause"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-class (&optional arg)
;;   "Mark class at point.
;;
;; With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive "P")
;;   (let ((py-mark-decorators (or arg py-mark-decorators))
;;         erg)
;;     (py-mark-base "class" py-mark-decorators)
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-def-or-class (&optional arg)
;;   "Mark def-or-class at point.
;;
;; With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive "P")
;;   (let ((py-mark-decorators (or arg py-mark-decorators))
;;         erg)
;;     (py-mark-base "def-or-class" py-mark-decorators)
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-line ()
;;   "Mark line at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "line"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-statement ()
;;   "Mark statement at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "statement"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-expression ()
;;   "Mark expression at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "expression"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))
;;
;; (defun py-mark-partial-expression ()
;;   "Mark partial-expression at point.
;;
;; Returns beginning and end positions of marked area, a cons. "
;;   (interactive)
;;   (let (erg)
;;     (setq erg (py-mark-base "partial-expression"))
;;     (exchange-point-and-mark)
;;     (when (and py-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))

;;; Decorator
(defun py-beginning-of-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful "
  (interactive)
  (back-to-indentation)
  (while (and (not (looking-at "@\\w+"))
              (not
               ;; (empty-line-p)
               (eq 9 (char-after)))
              (not (bobp))(forward-line -1))
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

;;; Kill
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

(defun py-kill-top-level ()
  "Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "top-level")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-minor-block ()
  "Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "minor-block")))
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

(defalias 'py-kill-minor-expression 'py-kill-partial-expression)
;; Helper functions

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
      (while (and (re-search-backward regexp nil 'move 1)(nth 8 (syntax-ppss))))  
      ;; (or (< (point) orig) (py-beginning-of-statement))
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
          last)
      (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                  (save-excursion (or (<= indent (progn  (py-beginning-of-statement)(current-indentation)))(eq last (line-beginning-position))))x
                  ;; (py-end-of-statement-p)
))
      (goto-char last)
      (when (< orig last)
        last))))

(defun py-beginning-of-block-current-column ()
  "Reach next beginning of block upwards which starts at current column.

Return position"
  (interactive)
  (let* ((orig (point))
         (cuco (current-column))
         (str (make-string cuco ?\s))
         pps erg)
    (while (and (not (bobp))(re-search-backward (concat "^" str py-block-keywords) nil t)(or (nth 8 (setq pps (syntax-ppss))) (nth 1 pps))))
    (back-to-indentation)
    (and (< (point) orig)(setq erg (point)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-beginning-of-decorator-bol 'py-beginning-of-decorator)

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)

(provide 'python-components-move)
;;; python-components-move.el ends here
