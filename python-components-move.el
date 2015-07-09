;;; python-components-move.el --- Functions moving point which need special treatment

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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
;; Expression
(defun py-backward-expression (&optional orig done repeat)
  "Go to the beginning of a python expression.

If already at the beginning or before a expression, go to next expression in buffer upwards"
  (interactive)
  (unless (bobp)
    (unless done (skip-chars-backward " \t\r\n\f"))
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "`py-backward-expression' reached loops max.")
	(cond
	 ;; comments
	 ((nth 8 pps)
	  (goto-char (nth 8 pps))
	  (py-backward-expression orig done repeat))
	 ;; lists
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (skip-chars-backward py-expression-skip-chars))
	 ;; in string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps)))
	 ;; after operator
	 ((and (not done) (looking-back py-operator-re))
	  (skip-chars-backward "^ \t\r\n\f")
	  (skip-chars-backward " \t\r\n\f")
	  (py-backward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (abs (skip-chars-backward py-expression-skip-chars))))
	  (setq done t)
	  (py-backward-expression orig done repeat))))
      (unless (or (eq (point) orig)(and (bobp)(eolp)))
	(setq erg (point)))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py-forward-expression (&optional orig done repeat)
  "Go to the end of a compound python expression.

Operators are ignored. "
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "`py-forward-expression' reached loops max.")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
	  (py-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
	  (while (and (looking-at "[ \t]*#") (not (eobp)))
	    (forward-line 1))
	  (py-forward-expression orig done repeat))
	 ;; inside string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ((looking-at "\"\"\"\\|'''\\|\"\\|'")
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ((and (eq orig (point)) (looking-at py-operator-re))
	  (goto-char (match-end 0))
	  (py-forward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (skip-chars-forward py-expression-skip-chars)))
	  (setq done t)
	  (py-forward-expression orig done repeat))
	 ;; at colon following arglist
	 ((looking-at ":[ \t]*$")
	  (forward-char 1)))
	(unless (or (eq (point) orig)(and (eobp)(bolp)))
	  (setq erg (point)))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun py-backward-partial-expression (&optional orig)
  (interactive)
  (let ((orig (point))
	erg)
    (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))(not (bobp))(forward-char -1))
    (when (py--in-comment-p)
      (py-backward-comment)
      (skip-chars-backward " \t\r\n\f"))
    ;; part of py-partial-expression-forward-chars
    (when (member (char-after) (list ?\ ?\" ?' ?\) ?} ?\] ?: ?#))
      (forward-char -1))
    (skip-chars-backward py-partial-expression-forward-chars)
    (when (< 0 (abs (skip-chars-backward py-partial-expression-backward-chars)))
      (while (and (not (bobp)) (py--in-comment-p)(< 0 (abs (skip-chars-backward py-partial-expression-backward-chars))))))
    (when (< (point) orig)
      (unless
	  (and (bobp) (member (char-after) (list ?\ ?\t ?\r ?\n ?\f)))
	(setq erg (point))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-partial-expression (&optional orig)
  (interactive)
  (let (erg)
    (skip-chars-forward py-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;; Partial- or Minor Expression
;;  Line
(defun py-backward-line ()
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
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py-forward-line ()
  "Go to end-of-line, return position.

If already at end-of-line and not at EOB, go to end of next line. "
  (interactive)
  (unless (eobp)
    (let ((orig (point))
	  erg)
      (when (eolp) (forward-line 1))
      (end-of-line)
      (when (< orig (point))(setq erg (point)))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

;;  Statement
(defun py-backward-statement (&optional orig done limit)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-backward-block.
For beginning of clause py-backward-clause."
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
	;; lp:1382788
	(unless done
	  (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
 	       (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit))
         ((nth 8 pps)
	  ;; inside string
          (and (nth 3 pps) (setq done t))
          (goto-char (nth 8 pps))
          (py-backward-statement orig done limit))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
          (setq done t)
          (py-backward-statement orig done limit))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-backward-statement orig done limit))
	 ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (forward-comment -1))
          (unless (bobp)
            (py-backward-statement orig done limit)))
	 ;; at inline comment
         ((looking-at "[ \t]*#")
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
	    (skip-chars-forward " \t")
	    (unless (bobp)
	      (py-backward-statement orig done limit))))
	 ;; at beginning of string
         ((and (not done) (looking-at py-string-delim-re))
          (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
            (setq done t))
          (back-to-indentation)
          (py-backward-statement orig done limit))
	 ;; after end of statement
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (py-backward-statement orig done limit))
	 ;; travel until indentation or semicolon
	 ((and (not done) (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point))))
	  (py-backward-statement orig done limit))
	 ;; at current indent
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (py-backward-statement orig done limit)))
	;; return nil when before comment
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
	  (when (< (point) orig)(setq erg (point))))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun py-backward-statement-bol (&optional indent)
  "Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. "
  (interactive)
  (let* ((indent (or indent (when (eq 'py-forward-statement-bol (car
  py-bol-forms-last-indent))(cdr py-bol-forms-last-indent))))
	 (orig (point))
         erg)
    (if indent
        (while (and (setq erg (py-backward-statement)) (< indent (current-indentation))(not (bobp))))
      (setq erg (py-backward-statement)))
    ;; reset
    (setq py-bol-forms-last-indent nil)
    (beginning-of-line)
    (and (< (point) orig) (setq erg (point)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc. "
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          erg pos last
          ;; use by scan-lists
          ;; parse-sexp-ignore-comments
          forward-sexp-function
          stringchar stm pps err)
      (unless done (py--skip-to-comment-or-semicolon))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; wich-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
        (error "py-forward-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
       ;; list
       ((nth 1 pps)
        (if (<= orig (point))
	    (progn
	      (setq orig (point))
	      ;; do not go back at a possible unclosed list
	      (goto-char (nth 1 pps))
	      (let ((parse-sexp-ignore-comments t))
		(if
		    (ignore-errors (forward-list))
		    (progn
		      (when (looking-at ":[ \t]*$")
			(forward-char 1))
		      (setq done t)
		      (skip-chars-forward "^#" (line-end-position))
		      (skip-chars-backward " \t\r\n\f" (line-beginning-position))
		      (py-forward-statement orig done repeat))
		  (setq err (py--record-list-error pps))
		  (goto-char orig))))))
       ;; string
       ((nth 3 pps)
	(when (py-end-of-string)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f")
	  (setq pps (parse-partial-sexp (point-min) (point)))
	  (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (py-forward-statement orig done repeat))))
       ;; in non-terminated string

       ;; in comment
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon)
	(while (and (eq (char-before (point)) ?\\ )
		    (py-escaped)(setq last (point)))
	  (forward-line 1)(end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	(py-forward-statement orig done repeat))
       ((py-current-line-backslashed-p)
	(end-of-line)
	(skip-chars-backward " \t\r\n\f" (line-beginning-position))
	(while (and (eq (char-before (point)) ?\\ )
		    (py-escaped))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
	(unless (eobp)
	  (py-forward-statement orig done repeat)))
       ((eq orig (point))
	(skip-chars-forward " \t\r\n\f#'\"")
	(py--skip-to-comment-or-semicolon)
	(py-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(py--skip-to-comment-or-semicolon)
	;; (setq pps (parse-partial-sexp (point-min) (point)))
	(unless done
	  (py-forward-statement orig done repeat)))

       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon))
	(py-forward-statement orig done repeat)))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9 ?#)))
	(setq erg (point)))
      (if (and py-verbose-p err)
	  (py--message-error err)
        (and py-verbose-p (called-interactively-p 'any) (message "%s" erg)))
      erg)))

(defun py-forward-statement-bol ()
  "Go to the beginning-of-line following current statement."
  (interactive)
  (let ((erg (py-forward-statement)))
    (setq erg (py--beginning-of-line-form))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;;  Decorator
(defun py-backward-decorator ()
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
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-decorator ()
  "Go to the end of a decorator.

Returns position if succesful "
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-backward-decorator)))
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
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

;;  Helper functions
(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro `py-backward-comment' instead "
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (maxindent
	 (or maxindent
	     (if (empty-line-p)
		 (progn
		   (py-backward-statement)
		   (current-indentation))
	       (or maxindent (and (< 0 (current-indentation))(current-indentation))
		   ;; make maxindent large enough if not set
		   (* 99 py-indent-offset)))))
        done erg cui)
    (while (and (not done) (not (bobp)))
      ;; (while (and (re-search-backward regexp nil 'move 1)(nth 8 (parse-partial-sexp (point-min) (point)))))
      ;; (or (< (point) orig) (py-backward-statement))
      (py-backward-statement)

      (when
	  (and (<= (current-indentation) maxindent)
	       (setq maxindent (current-indentation))
	       (looking-at regexp))
	(setq erg (point))
	(setq done t)
        ;; (when (and first (not maxindent))
	;; (setq maxindent (current-indentation))
	;; (setq first nil))
	))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py--clause-lookup-keyword (regexp arg &optional indent orig origline)
  "Returns a list, whose car is indentation, cdr position. "
  (let* ((orig (or orig (point)))
         (origline (or origline (py-count-lines)))
         (stop (if (< 0 arg)'(eobp)'(bobp)))
         (function (if (< 0 arg) 'py-forward-statement 'py-backward-statement))
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
           (parse-partial-sexp (point-min) (point)))))
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
                           (parse-partial-sexp (point-min) (point)))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      (when iact (message "%s" last))
      last)))

(defun py-forward-into-nomenclature (&optional arg iact)
  "Move forward to end of a nomenclature symbol.

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
    (when (and py-verbose-p (or iact (called-interactively-p 'any))) (message "%s" erg))
    erg))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature symbol.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (setq arg (or arg 1))
  (py-forward-into-nomenclature (- arg) arg))

(defun py--travel-current-indent (indent &optional orig)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (unless (eobp)
    (let ((orig (or orig (point)))
          last)
      (while (and (setq last (point))(not (eobp))(py-forward-statement)
                  (save-excursion (or (<= indent (progn  (py-backward-statement)(current-indentation)))(eq last (line-beginning-position))))
                  ;; (py--end-of-statement-p)
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
    (while (and (not (bobp))(re-search-backward (concat "^" str py-block-keywords) nil t)(or (nth 8 (setq pps (parse-partial-sexp (point-min) (point)))) (nth 1 pps))))
    (back-to-indentation)
    (and (< (point) orig)(setq erg (point)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-backward-section ()
  "Go to next section start upward in buffer.

Return position if successful"
  (interactive)
  (let ((orig (point)))
    (while (and (re-search-backward py-section-start nil t 1)
		(nth 8 (parse-partial-sexp (point-min) (point)))))
    (when (and (looking-at py-section-start)(< (point) orig))
      (point))))

(defun py-forward-section ()
  "Go to next section end downward in buffer.

Return position if successful"
  (interactive)
  (let ((orig (point))
	last)
    (while (and (re-search-forward py-section-end nil t 1)
		(setq last (point))
		(goto-char (match-beginning 0))
		(nth 8 (parse-partial-sexp (point-min) (point)))
		(goto-char (match-end 0))))
    (and last (goto-char last))
    (when (and (looking-back py-section-end)(< orig (point)))
      (point))))

(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-backward-decorator-bol 'py-backward-decorator)
(defalias 'py-beginning-of-block 'py-backward-block)
(defalias 'py-beginning-of-block-bol 'py-backward-block-bol)
(defalias 'py-beginning-of-block-or-clause 'py-backward-block-or-clause)
(defalias 'py-beginning-of-class 'py-backward-class)
(defalias 'py-beginning-of-class-bol 'py-backward-class-bol)
(defalias 'py-beginning-of-clause 'py-backward-clause)
(defalias 'py-beginning-of-clause-bol 'py-backward-clause-bol)
(defalias 'py-beginning-of-comment 'py-backward-comment)
(defalias 'py-beginning-of-declarations 'py-backward-declarations)
(defalias 'py-beginning-of-decorator 'py-backward-decorator)
(defalias 'py-beginning-of-decorator-bol 'py-backward-decorator)
(defalias 'py-beginning-of-def-or-class 'py-backward-def-or-class)
(defalias 'py-beginning-of-expression 'py-backward-expression)
(defalias 'py-beginning-of-line 'py-backward-line)
(defalias 'py-beginning-of-minor-block 'py-backward-minor-block)
(defalias 'py-beginning-of-partial-expression 'py-backward-partial-expression)
(defalias 'py-beginning-of-section 'py-backward-section)
(defalias 'py-beginning-of-statement 'py-backward-statement)
(defalias 'py-beginning-of-statement-bol 'py-backward-statement-bol)
(defalias 'py-beginning-of-top-level 'py-backward-top-level)
(defalias 'py-end-of-comment 'py-forward-comment)
(defalias 'py-end-of-decorator 'py-forward-decorator)
(defalias 'py-end-of-def-or-class 'py-forward-def-or-class)
(defalias 'py-end-of-expression 'py-forward-expression)
(defalias 'py-end-of-line 'py-forward-line)
(defalias 'py-end-of-partial-expression 'py-forward-partial-expression)
(defalias 'py-end-of-section 'py-forward-section)
(defalias 'py-end-of-statement 'py-forward-statement)
(defalias 'py-end-of-statement-bol 'py-forward-statement-bol)
(defalias 'py-end-of-top-level 'py-forward-top-level)
(defalias 'py-end-of-top-level 'py-forward-top-level)
(defalias 'py-goto-block-or-clause-up 'py-backward-block-or-clause)
(defalias 'py-goto-block-up 'py-backward-block)
(defalias 'py-goto-clause-up 'py-backward-clause)
(defalias 'py-next-statement 'py-forward-statement)
(defalias 'py-previous-block 'py-backward-block)
(defalias 'py-previous-block-or-clause 'py-backward-block-or-clause)
(defalias 'py-previous-class 'py-backward-class)
(defalias 'py-previous-clause 'py-backward-clause)
(defalias 'py-previous-def-or-class 'py-backward-def-or-class)
(defalias 'py-previous-statement 'py-backward-statement)

(provide 'python-components-move)
;;;  python-components-move.el ends here
