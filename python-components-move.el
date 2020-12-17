;;; python-components-move.el --- Functions moving point which need special treatment -*- lexical-binding: t; -*-


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

;;

;;; Code:

(defun py-backward-paragraph ()
  "Go to beginning of current paragraph.

If already at beginning, go to start of next paragraph upwards"
  (interactive)
  (let ((erg (and (backward-paragraph)(point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-paragraph ()
    "Go to end of current paragraph.

If already at end, go to end of next paragraph downwards"
  (interactive)
  (let ((erg (and (forward-paragraph)(point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;; Indentation
;; Travel current level of indentation
(defun py--travel-this-indent-backward (&optional indent)
  "Travel current INDENT backward.

With optional INDENT travel bigger or equal indentation"
  (let ((indent (or indent (current-indentation)))
	last)
    (while (and (not (bobp))
		(py-backward-statement)
		(<= indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    last))

(defun py-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (py--travel-this-indent-backward))
      (when erg (goto-char erg))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py--travel-this-indent-backward-bol (indent)
  "Internal use.

Travel this INDENT backward until bol"
  (let (erg)
    (while (and (py-backward-statement-bol)
		(or indent (setq indent (current-indentation)))
		(eq indent (current-indentation))(setq erg (point)) (not (bobp))))
    (when erg (goto-char erg))))

(defun py-backward-indent-bol ()
  "Go to the beginning of line of a section of equal indent.

If already at the beginning or before an indent,
go to next indent in buffer upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let ((indent (when (eq (current-indentation) (current-column)) (current-column)))
	  erg)
      (setq erg (py--travel-this-indent-backward-bol indent))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py--travel-this-indent-forward (indent)
  "Internal use.

Travel this INDENT forward"
  (let (last erg)
    (while (and (py-down-statement)
		(eq indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    (setq erg (py-forward-statement))
    erg))

(defun py-forward-indent ()
  "Go to the end of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (let (done
	(last (point))
	(orig (point))
	(indent (current-indentation)))
    (while (and (not (eobp)) (not done) (progn (forward-line 1) (back-to-indentation) (or (py-empty-line-p) (and (<= indent (current-indentation))(< last (point))(setq last (point)))(setq done t))))
      (and (< indent (current-indentation))(setq done t)))
    (if (and last (< orig last))
	(progn (goto-char last)
	       (end-of-line)
	       (skip-chars-backward " \t\r\n\f"))
      (skip-chars-forward " \t\r\n\f")
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (and (< orig (point))(point))))

(defun py-forward-indent-bol ()
  "Go to beginning of line following of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (eobp)
    (let (erg indent)
      (when (py-forward-statement)
      	(save-excursion
      	  (setq indent (and (py-backward-statement)(current-indentation))))
	(setq erg (py--travel-this-indent-forward indent))
	(unless (eobp) (forward-line 1) (beginning-of-line) (setq erg (point)))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg)))
      erg)))

(defun py-backward-expression (&optional orig done repeat)
  "Go to the beginning of a python expression.

If already at the beginning or before a expression,
go to next expression in buffer upwards

ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless (bobp)
    (unless done (skip-chars-backward " \t\r\n\f"))
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "`py-backward-expression' reached loops max")
	(cond
	 ;; comments
	 ((nth 8 pps)
	  (goto-char (nth 8 pps))
	  (py-backward-expression orig done repeat))
	 ;; lists
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (skip-chars-backward py-expression-skip-chars)
	  )
	 ;; in string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps)))
	 ;; after operator
	 ((and (not done) (looking-back py-operator-re (line-beginning-position)))
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

Operators are ignored.
ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (pps (parse-partial-sexp (point-min) (point)))
          (orig (or orig (point)))
          erg)
      (if (< py-max-specpdl-size repeat)
	  (error "`py-forward-expression' reached loops max")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1) (point)))(forward-line 1))
	  (py-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
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
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (skip-chars-forward py-expression-skip-chars)
	  (setq done t))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (skip-chars-forward py-expression-skip-chars)
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
	(unless (or (eq (point) orig)(and (eobp) (bolp)))
	  (setq erg (point)))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun py-backward-partial-expression ()
  "Backward partial-expression."
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

(defun py-forward-partial-expression ()
  "Forward partial-expression."
  (interactive)
  (let (erg)
    (skip-chars-forward py-partial-expression-backward-chars)
    ;; group arg
    (while
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;; Partial- or Minor Expression
;;  Line
(defun py-backward-line ()
  "Go to ‘beginning-of-line’, return position.

If already at ‘beginning-of-line’ and not at BOB, go to beginning of previous line."
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
  "Go to ‘end-of-line’, return position.

If already at ‘end-of-line’ and not at EOB, go to end of next line."
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
(defun py-backward-statement (&optional orig done limit ignore-in-string-p repeat maxindent)
  "Go to the initial line of a simple statement.

For beginning of compound statement use ‘py-backward-block’.
For beginning of clause ‘py-backward-clause’.

`ignore-in-string-p' allows moves inside a docstring, used when
computing indents
ORIG - consider orignial position or point.
DONE - transaktional argument
LIMIT - honor limit
IGNORE-IN-STRING-P - also much inside a string
REPEAT - count and consider repeats
Optional MAXINDENT: don't stop if indentation is larger"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((repeat (or (and repeat (1+ repeat)) 0))
	     (orig (or orig (point)))
             (pps (parse-partial-sexp (or limit (point-min))(point)))
             (done done)
             erg)
	;; lp:1382788
	(unless done
	  (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
 	       (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
	 ((< py-max-specpdl-size repeat)
	  (error "Py-forward-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
         ((and (bolp) (eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; inside string
         ((and (nth 3 pps) (not ignore-in-string-p))
	  (setq done t)
	  (goto-char (nth 8 pps))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ((nth 4 pps)
	  (while (ignore-errors (goto-char (nth 8 pps)))
	    (skip-chars-backward " \t\r\n\f")
	    (setq pps (parse-partial-sexp (line-beginning-position) (point))))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at raw-string
	 ;; (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	 ((and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	  (forward-char -1)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	 ;; at inline comment
         ((looking-at "[ \t]*#")
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at beginning of string
	 ((looking-at py-string-delim-re)
	  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	    (setq done t))
	  (back-to-indentation)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; after end of statement
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; travel until indentation or semicolon
	 ((and (not done) (py--skip-to-semicolon-backward))
	  (unless (and maxindent (< maxindent (current-indentation)))
	    (setq done t))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at current indent
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ((and maxindent (< maxindent (current-indentation)))
	  (forward-line -1)
	  (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	;; return nil when before comment
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
	  (when (< (point) orig)(setq erg (point))))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun py-backward-statement-bol ()
  "Goto beginning of line where statement start.
Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above."
  (interactive)
  (let* ((orig (point))
         erg)
    (unless (bobp)
      (cond ((bolp)
	     (and (py-backward-statement orig)
		  (progn (beginning-of-line)
			 (setq erg (point)))))
	    (t (setq erg
		     (and
		      (py-backward-statement)
		      (progn (beginning-of-line) (point)))))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

ORIG - consider orignial position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (switch-to-buffer (current-buffer))
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (orig (or orig (point)))
	  erg last
	  ;; use by scan-lists
	  forward-sexp-function pps err)
      ;; (unless done (py--skip-to-comment-or-semicolon done))
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; which-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
	(error "py-forward-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
       ;; list
       ((nth 1 pps)
	(if (<= orig (point))
	    (progn
	      (setq orig (point))
	      ;; do not go back at a possible unclosed list
	      (goto-char (nth 1 pps))
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
		(goto-char orig)))))
       ;; in comment
       ((and comment-start (looking-at (concat " *" comment-start)))
	(goto-char (match-end 0))
	(py-forward-statement orig done repeat))
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon done)
	(while (and (eq (char-before (point)) ?\\)
		    (py-escaped) (setq last (point)))
	  (forward-line 1) (end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	(py-forward-statement orig done repeat))
       ;; string
       ((looking-at py-string-delim-re)
	(goto-char (match-end 0))
	(py-forward-statement orig done repeat))
       ((nth 3 pps)
	(when (py-end-of-string)
	  (end-of-line)
	  (skip-chars-forward " \t\r\n\f")
	  (setq pps (parse-partial-sexp (point-min) (point)))
	  (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (py-forward-statement orig done repeat))))
       ((py-current-line-backslashed-p)
	(end-of-line)
	(skip-chars-backward " \t\r\n\f" (line-beginning-position))
	(while (and (eq (char-before (point)) ?\\)
		    (py-escaped))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
	(unless (eobp)
	  (py-forward-statement orig done repeat)))
       ((eq orig (point))
	(if (eolp)
	    (skip-chars-forward " \t\r\n\f#'\"")
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" orig))
	;; point at orig due to a trailing whitespace
	(and (eq (point) orig) (skip-chars-forward " \t\r\n\f"))
	(setq done t)
	(py-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(py--skip-to-comment-or-semicolon done)
	(setq pps (parse-partial-sexp orig (point)))
	(if (nth 1 pps)
	    (py-forward-statement orig done repeat)
	  (unless done
	    (py-forward-statement orig done repeat))))
       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon done))
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
  "Go to the ‘beginning-of-line’ following current statement."
  (interactive)
  (let ((erg (py-forward-statement)))
    (setq erg (py--beginning-of-line-form))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;;  Decorator
(defun py-backward-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)))
    (unless (bobp) (forward-line -1)
	    (back-to-indentation)
	    (while (and (progn (looking-at "@\\w+")(not (looking-at "\\w+")))
			(not
			 ;; (py-empty-line-p)
			 (member (char-after) (list 9 10)))
			(not (bobp))(forward-line -1))
	      (back-to-indentation))
	    (or (and (looking-at "@\\w+") (match-beginning 0))
		(goto-char orig)))))

(defun py-forward-decorator ()
  "Go to the end of a decorator.

Returns position if succesful"
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

(defun py-backward-comment (&optional pos)
  "Got to beginning of a commented section.

Start from POS if specified"
  (interactive)
  (let ((erg pos)
	last)
    (when erg (goto-char erg))
    (while (and (not (bobp)) (setq erg (py-in-comment-p)))
      (when (< erg (point))
	(goto-char erg)
	(setq last (point)))
      (skip-chars-backward " \t\r\n\f"))
    (when last (goto-char last))
    last))

(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro `py-backward-comment' instead"
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--up-decorators-maybe (indent)
  (let ((last (point)))
    (while (and (not (bobp))
		(py-backward-statement)
		(eq (current-indentation) indent)
		(if (looking-at py-decorator-re)
		    (progn (setq last (point)) nil)
		  t)))
    (goto-char last)))

(defun py--go-to-keyword (regexp &optional maxindent condition ignoreindent)
  "Expects being called from beginning of a statement.

Argument REGEXP: a symbol.

Return a list, whose car is indentation, cdr position.

Keyword detected from REGEXP
Honor MAXINDENT if provided
Optional IGNOREINDENT: find next keyword at any indentation"
  (unless (bobp)
    ;;    (when (py-empty-line-p) (skip-chars-backward " \t\r\n\f"))
    (let* ((orig (point))
	   (condition
	    (or condition '<))
	   ;; py-clause-re would not match block
	   (regexp (if (eq regexp 'py-clause-re) 'py-extended-block-or-clause-re regexp))
	   (regexpvalue (symbol-value regexp))
	   (maxindent
	    (if ignoreindent
		;; just a big value
		9999
	      (or maxindent
		  (if
		      (or (looking-at regexpvalue) (eq 0 (current-indentation)))
		      (current-indentation)
		    (abs
		     (- (current-indentation) py-indent-offset))))))
	   erg)
      (unless (py-beginning-of-statement-p)
	(py-backward-statement))
      (cond
       ((looking-at (concat (symbol-value regexp)))
	(if (eq (point) orig)
	    (setq erg (py--backward-regexp regexp maxindent condition orig regexpvalue))
	  (setq erg (point))))
       (t (setq erg (py--backward-regexp regexp maxindent condition orig regexpvalue))))
      (when erg (setq erg (cons (current-indentation) erg)))
      (list (car erg) (cdr erg) (py--end-base-determine-secondvalue regexp)))))

(defun py-leave-comment-or-string-backward ()
  "If inside a comment or string, leave it backward."
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (parse-partial-sexp (point-min) (point)))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.

IACT - if called interactively
LAST - was last match.
Optional PPSTART indicates a start-position for `parse-partial-sexp'.
ORIG - consider orignial position or point.
DONE - transaktional argument
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
IACT - if called interactively
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
          (when (looking-back "[[:upper:]]" (line-beginning-position))
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
  "Move down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and ORIG position."
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
"Reach next beginning of block upwards which start at current column.

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
    (when (and (looking-back py-section-end (line-beginning-position))(< orig (point)))
      (point))))

;; (defun py-forward-paragraph ()
;;   (interactive)
;;   (py--end-of-paragraph 'py-paragraph-re))

(defun py-backward-assignment()
  "Go to backward in buffer to beginning of an assigment.

Return position if successful."
  (interactive)
  (let* (last
	 (erg
	  (progn
	    (while (and (setq last (py-backward-statement))
			(not (looking-at py-assignment-re))))
	    (and (looking-at py-assignment-re) last))))
    (when (and py-verbose-p (called-interactively-p 'interactive))
      (message "%s" erg))
    erg))

(defun py-beginning-of-assignment()
  "Go to beginning of assigment if inside.

Return position of successful, nil of not started from inside."
  (interactive)
  (let* (last
	 (erg
	  (or (py--beginning-of-assignment-p)
	      (progn
		(while (and (setq last (py-backward-statement))
			    (not (looking-at py-assignment-re))
			    ;; (not (bolp))
			    ))
		(and (looking-at py-assignment-re) last)))))
    (when (and py-verbose-p (called-interactively-p 'interactive))
      (message "%s" erg))
    erg))

(defun py--forward-assignment-intern ()
  (and (looking-at py-assignment-re)
       (goto-char (match-end 2))
       (skip-chars-forward " \t\r\n\f")
       ;; (eq (car (syntax-after (point))) 4)
       (progn (forward-sexp) (point))))

(defun py-end-of-assignment()
  "Go to end of assigment at point if inside.

Return position of successful, nil of not started from inside"
  (interactive)
  (unless (eobp)
    (if (eq last-command 'py-backward-assignment)
	;; assume at start of an assignment
	(py--forward-assignment-intern)
      ;; ‘py-backward-assignment’ here, avoid ‘py--beginning-of-assignment-p’ a second time
      (let* (last
	     (beg
	      (or (py--beginning-of-assignment-p)
		  (progn
		    (while (and (setq last (py-backward-statement))
				(not (looking-at py-assignment-re))
				;; (not (bolp))
				))
		    (and (looking-at py-assignment-re) last))))
	     erg)
	(and beg (setq erg (py--forward-assignment-intern)))
	(when (and py-verbose-p (called-interactively-p 'interactive))
          (message "%s" erg))
	erg))))

(defun py-forward-assignment()
  "Go to end of assigment at point if inside.

Return position of successful, nil of not started from inside
When called at the end of an assignment, check next form downwards."
  (interactive)
  (unless (eobp)
    (if (eq last-command 'py-backward-assignment)
	;; assume at start of an assignment
	(py--forward-assignment-intern)
      ;; ‘py-backward-assignment’ here, avoid ‘py--beginning-of-assignment-p’ a second time
      (let* (last
	     (orig (point))
	     (beg
	      (or (py--beginning-of-assignment-p)
		  (progn
		    (while (and (setq last (py-backward-statement))
				(not (looking-at py-assignment-re))
				;; (not (bolp))
				))
		    (and (looking-at py-assignment-re) last))))
	     erg)
	(and beg (setq erg (py--forward-assignment-intern)))
	(when (eq (point) orig)
	  (while (and (not (eobp)) (re-search-forward py-assignment-re) (setq last (match-beginning 1)) (py-in-string-or-comment-p)))
	  (when last
	    (goto-char last)
	    (setq erg (point))))
	(when (and py-verbose-p (called-interactively-p 'interactive))
          (message "%s" erg))
	erg))))

(provide 'python-components-move)
;;;  python-components-move.el ends here
