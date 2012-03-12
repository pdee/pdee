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

;; Block
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defun py-beginning-of-block (&optional indent)
  "Looks up for nearest opening block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-re -1 indent)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-if-block ()
  "Looks up for nearest opening if-block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-if-re -1)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-beginning-of-try-block ()
  "Looks up for nearest opening try-block, i.e. compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-try-block-re -1)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-goto-beyond-block 'py-end-of-block)
(defun py-end-of-block ()
  "Go to the end of a compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point)))
    (py-end-base py-block-re orig (interactive-p))))

;; Block or clause
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defun py-beginning-of-block-or-clause (&optional arg indent)
  "Looks up for nearest opening clause or block.

With universal argument looks for next compound statements
i.e. blocks only.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")

  (let* ((regexp (if arg
                     py-block-re
                   py-block-or-clause-re))
         (erg (ignore-errors (cdr (py-go-to-keyword regexp -1 indent)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-goto-beyond-block-or-clause 'py-end-of-block-or-clause)
(defun py-end-of-block-or-clause (&optional arg)
  "Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.
Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((regexp (if arg
                    py-block-re
                  py-block-or-clause-re))
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Class
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defun py-beginning-of-class ()
  "Move point to start of next `class'.

See also `py-beginning-of-def-or-class'.
Returns position reached, if any, nil otherwise."
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-class-re -1)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-next-class 'py-end-of-class)
(defun py-end-of-class (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let ((orig (point))
        (regexp py-class-re))
    (py-end-base regexp orig iact)))

;; Clause
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defun py-beginning-of-clause ()
  "Looks up for nearest opening clause, i.e. a compound statements
subform.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-or-clause-re -1)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'py-goto-beyond-clause 'py-end-of-clause)
(defun py-end-of-clause ()
  "Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((regexp py-block-or-clause-re)
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Method Definition or Class
(defun py-beginning-of-def ()
  "Move point to start of `def'.

Returns position reached, if any, nil otherwise "
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-def-re -1)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-def (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let* ((orig (point))
         (regexp py-def-re))
    (py-end-base regexp orig iact)))

(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
(defun py-beginning-of-def-or-class (&optional arg)
  "Move point to start of `def' or `class', whatever is next.

With optional universal arg CLASS, move to the beginn of class definition.
Returns position reached, if any, nil otherwise "
  (interactive "P")
  (let* ((regexp (if (eq 4 (prefix-numeric-value arg))
                     py-class-re
                   py-def-or-class-re))
         (res (ignore-errors (cdr (py-go-to-keyword regexp -1))))
         (erg
          (when (looking-at regexp)
            res)))
    (when (and py-verbose-p (interactive-p)) (message "%s" (prin1-to-string erg)))
    erg))

(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-next-def-or-class 'py-end-of-def-or-class)
(defun py-end-of-def-or-class (&optional arg)
  "Move point beyond next `def' or `class' definition.

With optional universal arg, move to the end of class exclusively.
Returns position reached, if any, nil otherwise."
  (interactive "P")
  (let* ((orig (point))
         (regexp
          (cond ((eq 4 (prefix-numeric-value arg))
                 py-class-re)
                (t py-def-or-class-re))))
    (py-end-base regexp orig (interactive-p))))

;; Expression
(defalias 'py-backward-expression 'py-beginning-of-expression)
(defun py-beginning-of-expression (&optional orig origline done)
  "Go to the beginning of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.
"
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (when (looking-at "\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)")
        (goto-char (1- (match-beginning 0)))
        (skip-chars-backward " \t\r\n\f")
        (forward-char -1))
      (when (looking-back "[\])}]")
        (forward-char -1))
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps (if (featurep 'xemacs)
                     (parse-partial-sexp (point-min) (point))
                   (syntax-ppss)))
            (done done)
            erg)
        (setq erg
              (cond
               ;; if in string
               ((and (nth 3 pps)(nth 8 pps)
                     (goto-char (nth 8 pps)))
                (unless (looking-back "\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)[ \t]*")
                  (goto-char (nth 2 pps)))
                (py-beginning-of-expression orig origline))
               ;; comments left, as strings are done
               ((nth 8 pps)
                (goto-char (1- (nth 8 pps)))
                (py-beginning-of-expression orig origline))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
                (forward-line -1)
                (unless (bobp)
                  (end-of-line)
                  (py-beginning-of-expression orig origline)))
               ;; character address of start of innermost containing list; nil if none.
               ((nth 1 pps)
                (goto-char (nth 1 pps))
                (when
                    (not (looking-back "[ \t]+"))
                  (skip-chars-backward py-expression-skip-regexp))
                (py-beginning-of-expression orig origline))
               ;; inside expression
               ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-looking-regexp))
                (skip-chars-backward py-expression-skip-regexp)
                (py-beginning-of-expression orig origline))
               ((looking-at py-expression-looking-regexp)
                (point))
               (t (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))(point)))))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defalias 'py-forward-expression 'py-end-of-expression)
(defun py-end-of-expression (&optional orig origline done)
  "Go to the end of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (eobp)
      (let*
          ((orig (or orig (point)))
           (origline (or origline (py-count-lines)))
           (pps (if (featurep 'xemacs)
                    (parse-partial-sexp (point-min) (point))
                  (syntax-ppss)))
           (done done)
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
        (cond
         ((and (empty-line-p)(not done)(not (eobp)))
          (while
              (and (empty-line-p)(not done)(not (eobp)))
            (forward-line 1))
          (py-end-of-expression orig origline done))
         ;; inside string
         ((py-in-string-p)
          (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (goto-char (match-end 0))
            (setq done t))
          ;; (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
          (while
              (nth 3
                   (if (featurep 'xemacs)
                       (parse-partial-sexp (point-min) (point))
                     (syntax-ppss)))
            (setq done t)
            (forward-char 1))
          (py-end-of-expression orig origline done))
         ;; in comment
         ((nth 4 pps)
          (forward-line 1)
          (py-end-of-expression orig origline done))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*")(not done))
          (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                      (beginning-of-line)))
          (end-of-line)
          ;;          (setq done t)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-expression orig origline done))
         ;; start of innermost containing list; nil if none.
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (let ((parse-sexp-ignore-comments t))
            (forward-list)
            (py-end-of-expression orig origline done)))
         ((and (not done)(looking-at py-not-expression-regexp)(not (eobp)))
          (skip-chars-forward py-not-expression-regexp)
          (py-end-of-expression orig origline done))
         ((and (not done)(looking-at py-expression-skip-regexp)(not (eobp)))
          (skip-chars-forward py-not-expression-regexp)
          (forward-char -1)
          (py-end-of-expression orig origline done))
         ((and (looking-at py-expression-looking-regexp)(not (eobp)))
          (forward-char 1)
          (setq done (< 0 (skip-chars-forward py-expression-skip-regexp)))
          (when done (forward-char -1))
          (setq done t)
          (py-end-of-expression orig origline done)))
        (unless (eq (point) orig)
          (setq erg (point)))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

;; Partial- or Minor Expression
(defalias 'py-backward-partial-expression 'py-beginning-of-partial-expression)
(defalias 'py-beginning-of-minor-expression 'py-beginning-of-partial-expression)
(defun py-beginning-of-partial-expression (&optional orig origline done)
  "Go to the beginning of a minor python expression.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (when (looking-at "\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)")
        (goto-char (1- (match-beginning 0)))
        (skip-chars-backward " \t\r\n\f")
        (forward-char -1))
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps (if (featurep 'xemacs)
                     (parse-partial-sexp (point-min) (point))
                   (syntax-ppss)))
            (done done)
            erg)
        (setq erg
              (cond
               ;; if in string
               ((and (nth 3 pps)(nth 8 pps)
                     (save-excursion
                       (ignore-errors
                         (goto-char (nth 2 pps)))))
                (goto-char (nth 2 pps))
                (py-beginning-of-partial-expression orig origline))
               ;; comments left, as strings are done
               ((nth 8 pps)
                (goto-char (1- (nth 8 pps)))
                (py-beginning-of-partial-expression orig origline))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
                (forward-line -1)
                (unless (bobp)
                  (end-of-line)
                  (py-beginning-of-partial-expression orig origline)))
               ((nth 1 pps)
                (skip-chars-backward py-minor-expression-backward-regexp)
                (point))
               ((and (eq (point) orig) (not (bobp)) (looking-back py-minor-expression-looking-regexp))
                (skip-chars-backward py-minor-expression-skip-regexp)
                (py-beginning-of-partial-expression orig origline))
               ((looking-at py-minor-expression-looking-regexp)
                (point))
               (t (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))(point)))))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defalias 'py-forward-partial-expression 'py-end-of-partial-expression)
(defalias 'py-end-of-minor-expression 'py-end-of-partial-expression)
(defun py-end-of-partial-expression (&optional orig origline done)
  "Go to the end of a minor python expression.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (eobp)
      (let*
          ((orig (or orig (point)))
           (origline (or origline (py-count-lines)))
           (pps (if (featurep 'xemacs)
                    (parse-partial-sexp (point-min) (point))
                  (syntax-ppss)))
           (done done)
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
        (cond
         ((and (empty-line-p)(not done)(not (eobp)))
          (while
              (and (empty-line-p)(not done)(not (eobp)))
            (forward-line 1))
          (py-end-of-partial-expression orig origline done))
         ;; inside string
         ((nth 3 pps)
          (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (goto-char (match-end 0)))
          (while
              (and (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
                   (nth 3
                        (if (featurep 'xemacs)
                            (parse-partial-sexp (point-min) (point))
                          (syntax-ppss)))))
          (py-end-of-partial-expression orig origline done))
         ;; in comment
         ((nth 4 pps)
          (forward-line 1)
          (py-end-of-partial-expression orig origline done))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*")(not done))
          (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                      (beginning-of-line)))
          (end-of-line)
          ;;          (setq done t)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-partial-expression orig origline done))
         ((and (nth 1 pps) (<= orig (nth 1 pps)))
          (goto-char (nth 1 pps))
          (let ((parse-sexp-ignore-comments t))
            (forward-list)
            (setq done t)
            (py-end-of-partial-expression orig origline done)))
         ((and (looking-at "\\.")(< orig (point)))
          (point))
         ((and (not done)(looking-at "\\.\\|=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!="))
          (goto-char (match-end 0))
          (when (< 0 (skip-chars-forward " \t\r\n\f"))
            (forward-char 1))
          (py-end-of-partial-expression orig origline done))
         ((and (not done)(looking-at py-minor-expression-looking-regexp)(not (eobp)))
          (skip-chars-forward py-minor-expression-forward-regexp)
          (setq done t)
          (py-end-of-partial-expression orig origline done))
         ((and (not done)(looking-at py-not-minor-expression-regexp)(not (eobp)))
          (skip-chars-forward py-not-minor-expression-skip-regexp)
          (py-end-of-partial-expression orig origline done))
         ((and (eq (point) orig) (not (eobp)))
          (forward-char 1)
          (py-end-of-partial-expression orig origline done)))
        (unless (eq (point) orig)
          (setq erg (point)))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

;; Statement
(defalias 'py-backward-statement 'py-beginning-of-statement)
(defalias 'py-previous-statement 'py-beginning-of-statement)
(defalias 'py-statement-backward 'py-beginning-of-statement)
(defun py-beginning-of-statement (&optional orig origline done)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html
"
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps
             (if (featurep 'xemacs)
                 (parse-partial-sexp (point-min) (point))
               (syntax-ppss)))
            erg done)
        (cond
         ((and (bobp) (eq (point) orig)))
         ((and (not (< (point) orig))(not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (setq done t)
          (py-beginning-of-statement orig origline done))
         ((empty-line-p)
          (forward-line -1)
          (while (and (not (bobp))(empty-line-p))
            (forward-line -1))
          (end-of-line)
          (py-beginning-of-statement orig origline done))
         ((nth 8 pps)
          (setq done t)
          (goto-char (nth 8 pps))
          (py-beginning-of-statement orig origline done))
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (forward-line -1)
          (unless (bobp)
            (end-of-line)
            (py-beginning-of-statement orig origline done)))
         ((py-continuation-line-p)
          (forward-line -1)
          (py-beginning-of-statement orig origline done))
         ;; character address of start of innermost containing list; nil if none.
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (forward-char -1)
          (setq done t)
          (when (< (point) orig) (setq erg (point)))
          (py-beginning-of-statement orig origline done))
         ((and (eq (current-indentation) (current-column))
               (eq (point) orig) (not (bolp)))
          (beginning-of-line)
          (py-beginning-of-statement orig origline done))
         ((not (eq (current-column) (current-indentation)))
          (back-to-indentation)
          (setq erg (point))
          (setq done t)
          (py-beginning-of-statement orig origline done))
         ((and (eq (point) orig)(or (bolp) (<= (current-column)(current-indentation))))
          (forward-line -1)
          (end-of-line)
          (skip-chars-backward " \t")
          (py-beginning-of-statement orig origline done)))
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (and py-verbose-p (interactive-p)) (message "%s" erg))
        erg))))

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py-end-of-statement (&optional orig origline done)
  "Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-lc'. "
  (interactive)
  (save-restriction
    (widen)
    (let (erg)
      (unless (eobp)
        (let*
            ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             (pps
              (if (featurep 'xemacs)
                  (parse-partial-sexp (point-min) (point))
                (syntax-ppss)))
             ;; use by scan-lists
             parse-sexp-ignore-comments)
          (cond
           ((and (empty-line-p) (not (eobp)))
            (while
                (and (empty-line-p) (not (eobp)))
              (forward-line 1))
            (py-end-of-statement orig origline done))
           ((and (not done)(looking-at "\"\"\"\\|'''\\|\"\\|'"))
            (goto-char (match-end 0))
            (while (and (re-search-forward (match-string-no-properties 0) nil (quote move) 1)(setq done t)
                        (nth 3
                             (if (featurep 'xemacs)
                                 (parse-partial-sexp (point-min) (point))
                               (syntax-ppss)))))
            (py-end-of-statement orig origline done))
           ;; inside string
           ((nth 8 pps)
            (cond
             ((nth 3 pps)
              (goto-char (nth 8 pps))
              (when (looking-at "\"\"\"\\|'''")
                (goto-char (match-end 0))
                (while (and (re-search-forward (match-string-no-properties 0) nil (quote move) 1)
                            (setq done nil)
                            (nth 3
                                 (if (featurep 'xemacs)
                                     (parse-partial-sexp (point-min) (point))
                                   (syntax-ppss)))))
                (setq done t)
                (end-of-line)
                (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                (setq erg (point))
                (py-end-of-statement orig origline done)))
             ;; in comment
             ((nth 4 pps)
              (if (eobp)
                  nil
                (forward-line 1)
                (end-of-line)
                (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                (setq erg (point))
                (setq done t)
                (py-end-of-statement orig origline done)))))
           ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                        (beginning-of-line))
              (setq done t))
            (end-of-line)
            (when (and done (looking-at "[ \t]*$") (not (looking-back "^[ \t]*")))
              (py-beginning-of-comment)
              (skip-chars-backward " \t\r\n\f"))
            (py-end-of-statement orig origline done))
           ((py-current-line-backslashed-p)
            (py-forward-line)
            (setq done t)
            (py-end-of-statement orig origline done))
           ;; start of innermost containing list; nil if none.
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
                    (py-end-of-statement orig origline done))
                (goto-char orig))))
           ((eq (point) orig)
            (cond ((not (looking-at "[ \t]*$"))
                   (end-of-line)
                   (py-beginning-of-comment)
                   (skip-chars-backward " \t")
                   (if (< orig (point))
                       (py-end-of-statement orig origline t)
                     (py-forward-line)
                     (py-end-of-statement orig origline done)))
                  ((and (looking-at "[ \t]*$")(not (eobp)))
                   (py-forward-line)
                   (setq done t)
                   (py-end-of-statement orig origline done))
                  ((not (eobp))
                   (py-forward-line)
                   (py-end-of-statement orig origline done))))
           ((and (bolp) (not (empty-line-p)))
            (end-of-line)
            (skip-chars-backward " \t\r\n\f" (line-beginning-position))
            (py-beginning-of-comment)
            (setq done t)
            (py-end-of-statement orig origline done))
           ((looking-at "\\.\\([A-Za-z_][A-Za-z_0-9]*\\)")
            (forward-char 1)
            (skip-chars-forward "A-Za-z_0-9")
            (forward-char 1)
            (py-end-of-statement orig origline done)))
          (unless (or (eq (point) orig)(empty-line-p)
                      (if (featurep 'xemacs)
                          (nth 4 (parse-partial-sexp (point-min) (point)))
                        (nth 4 (syntax-ppss)))
                      (eq 0 (current-column)))
            (setq erg (point)))
          (when (and py-verbose-p (interactive-p)) (message "%s" erg))
          ;; (message "%s" erg)
          erg)))))

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
    (when (and py-verbose-p (interactive-p)) (message "%s %s" beg end))
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

(defun py-mark-def (&optional arg)
  "Mark def at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-class (&optional arg)
  "Mark class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def" py-mark-decorators)
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

(defun py-mark-minor-expression ()
  "Mark minor-expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "minor-expression"))
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

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-partial-expression 'py-copy-partial-expression)
(defalias 'py-copy-minor-expression 'py-copy-partial-expression)
(defalias 'py-minor-expression 'py-copy-partial-expression)
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

(
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

(defalias 'py-kill-minor-expression 'py-kill-partial-expression)
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

(defun py-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any. "
  (interactive)
  (save-restriction
    (widen)
    (let ((pps
           (if (featurep 'xemacs)
               (parse-partial-sexp (line-beginning-position) (point))
             (syntax-ppss))))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

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

(defun py-go-to-keyword (regexp arg &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (origline (py-count-lines))
        (stop (if (< 0 arg)'(eobp)'(bobp)))
        (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
        (maxindent maxindent)
        done erg cui)
    (while (and (not done) (not (eval stop)))
      (funcall function)
      (when (and (looking-at regexp)(if maxindent
                                        (< (current-indentation) maxindent)t))
        (setq erg (point))
        (setq done t)))
    (when erg (setq erg (cons (current-indentation) erg)))
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

;; Complementary left corner commands start
(defun py-down-block-lc ()
  "Goto beginning of line following end of block.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-block' stops at right corner.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-end-of-block)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-clause-lc ()
  "Goto beginning of line following end of clause.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-clause' stops at right corner.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-end-of-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-def-lc ()
  "Goto beginning of line following end of def.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-def' stops at right corner.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-end-of-def)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-class-lc ()
  "Goto beginning of line following end of class.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-class' stops at right corner.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-end-of-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-statement-lc ()
  "Goto beginning of line following end of statement.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-statement' stops at right corner.

See also `py-down-statement': down from current definition to next beginning of statement below. "
  (interactive)
  (let ((erg (py-end-of-statement)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; Complementary left corner commands end

;; Py-down commands start
(defun py-down-statement ()
  "Go to the beginning of next statement below in buffer.

Returns indentation if statement found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (progn
        (when (setq erg (py-end-of-statement))
          (if (< orig (setq erg (py-beginning-of-statement-position)))
              (goto-char erg)
            (setq erg (py-end-of-statement))
            (when erg
              (py-beginning-of-statement))))
        (when erg
          (setq erg (current-column)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-block ()
  "Go to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (re-search-forward py-block-re nil (quote move))
                  (nth 8 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (syntax-ppss)))))
      (back-to-indentation)
      (when (looking-at py-block-re) (setq erg (current-indentation)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-down-clause ()
  "Go to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-clause-re)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-block-or-clause ()
  "Go to the beginning of next block-or-clause below in buffer.

Returns indentation if block-or-clause found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-block-or-clause-re)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-def ()
  "Go to the beginning of next function definition below in buffer.

Returns indentation if found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-def-re)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-class ()
  "Go to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-class-re)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-down-def-or-class ()
  "Go to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-def-or-class-re)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

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
    (when (and py-report-position-p (or iact (interactive-p))) (message "%s" erg))
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

(defun py-travel-current-indent (listing)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (let ((start (ignore-errors (cdr listing)))
        (indent (ignore-errors (car listing)))
        last)
    (if start
        (progn
          (goto-char start)
          (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                      (<= indent (progn (save-excursion (py-beginning-of-statement)(current-indentation))))
                      (not (and (empty-line-p)(or (nth 0 (syntax-ppss)))(nth 8 (syntax-ppss))))))
          (when last (goto-char last))
          last))))

(provide 'python-components-move)
;;; python-components-move.el ends here
