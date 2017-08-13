;;; python-components-move.el --- Functions moving point which need special treatment -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Andreas Röhler

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

;; Indentation
;; Travel current level of indentation
(defun py--travel-this-indent-backward (&optional indent)
  (let (erg)
    (while (and (py-backward-statement)
		(or indent (setq indent (current-indentation)))
		(eq indent (current-indentation))(setq erg (point)) (not (bobp))))
    erg))

(defun py-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent in buffer upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (py--travel-this-indent-backward))
      (when erg (goto-char erg))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py--travel-this-indent-backward-bol (indent)
  (let (erg)
    (while (and (py-backward-statement-bol)
		(or indent (setq indent (current-indentation)))
		(eq indent (current-indentation))(setq erg (point)) (not (bobp))))
    (when erg (goto-char erg))))

(defun py-backward-indent-bol ()
  "Go to the beginning of line of a section of equal indent.

If already at the beginning or before an indent, go to next indent in buffer upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let ((indent (when (eq (current-indentation) (current-column)) (current-column)))
	  erg)
      (setq erg (py--travel-this-indent-backward-bol indent))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py--travel-this-indent-forward (indent)
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
  (unless (eobp)
    (let (done indent)
      (when (py-forward-statement)
	(save-excursion
	  (setq done (point))
	  (setq indent (and (py-backward-statement)(current-indentation))))
      (setq done (py--travel-this-indent-forward indent))
      (when done (goto-char done)))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" done))
      done)))

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
	 ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
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

(defun py-backward-partial-expression ()
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
(defun py-backward-statement (&optional orig done limit ignore-in-string-p repeat)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-backward-block.
For beginning of clause py-backward-clause.

`ignore-in-string-p' allows moves inside a docstring, used when
computing indents"
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
	  (error "py-forward-statement reached loops max. If no error, customize `py-max-specpdl-size'"))
         ((and (bolp)(eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; inside string
         ((and (nth 3 pps)(not ignore-in-string-p))
	  (setq done t)
	  (goto-char (nth 8 pps))
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ((nth 4 pps)
	  (while (ignore-errors (goto-char (nth 8 pps)))
	    (skip-chars-backward " \t\r\n\f")
	    (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	    )
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
	    (setq done t))
          (py-backward-statement orig done limit ignore-in-string-p repeat))
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; at raw-string
	 ;; (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	 ((and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	  (forward-char -1)
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (py-backward-statement orig done limit ignore-in-string-p repeat)))
	 ;; at inline comment
         ((looking-at "[ \t]*#")
	  (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation)(point)))
	    (setq done t))
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; at beginning of string
	 ((looking-at py-string-delim-re)
	  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	    (setq done t))
	  (back-to-indentation)
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; after end of statement
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; travel until indentation or semicolon
	 ((and (not done) (py--skip-to-semicolon-backward))
	  (setq done t)
	  (py-backward-statement orig done limit ignore-in-string-p repeat))
	 ;; at current indent
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
	  (py-backward-statement orig done limit ignore-in-string-p repeat)))
	;; return nil when before comment
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
	  (when (< (point) orig)(setq erg (point))))
	(when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun py-backward-statement-bol ()
  "Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. "
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

Optional argument REPEAT, the number of loops done already, is checked for py-max-specpdl-size error. Avoid eternal loops due to missing string delimters etc. "
  (interactive)
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
       ((looking-at (concat " *" comment-start))
	(goto-char (match-end 0))
	(py-forward-statement orig done repeat))
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon done)
	(while (and (eq (char-before (point)) ?\\ )
		    (py-escaped)(setq last (point)))
	  (forward-line 1)(end-of-line))
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
	(while (and (eq (char-before (point)) ?\\ )
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
  "Go to the beginning-of-line following current statement."
  (interactive)
  (let ((erg (py-forward-statement)))
    (setq erg (py--beginning-of-line-form erg))
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

(defun py-backward-comment (&optional pos)
  "Got to beginning of a commented section. "
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

(defun py-forward-comment (&optional pos char)
  "Go to end of commented section.

Optional args position and comment-start character
Travel empty lines "
  (interactive)
  (let ((orig (or pos (point)))
	(char (or char (string-to-char comment-start)))
	py-forward-comment-last erg)
    (while (and (not (eobp))
		(or
		 (forward-comment 99999)
		 (when (py--in-comment-p)
		   (progn
		     (end-of-line)
		     (skip-chars-backward " \t\r\n\f")
		     (setq py-forward-comment-last (point))))
		 (prog1 (forward-line 1)
		   (end-of-line)))))
    (when py-forward-comment-last (goto-char py-forward-comment-last))
    ;; forward-comment fails sometimes
    (and (eq orig (point)) (prog1 (forward-line 1) (back-to-indentation))
	 (while (member (char-after) (list char 10))(forward-line 1)(back-to-indentation)))
    (when (< orig (point)) (setq erg (point)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

;;  Helper functions
(defun py-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro `py-backward-comment' instead "
  (interactive)
  (let ((erg (py-backward-comment)))
    (when (and py-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun py--up-decorators-maybe (indent)
  (let (done erg)
    (while (and (not done) (not (bobp)))
      (py-backward-statement)
      (if
	  (and (current-indentation) indent
	       (looking-at py-decorator-re))
	  (setq erg (point))
	(setq done t)))
    erg))

(defun py--go-to-keyword-bol (regexp)
  (let ((orig (point))
	done pps)
    (while (and (not done) (not (bobp)) (re-search-backward (concat "^" regexp) nil t 1))
      (setq pps (parse-partial-sexp (point-min) (point)))
      (or
       (nth 8 pps)(nth 1 pps)
       ;; (member (char-after)(list 32 9 ?# ?' ?\"))
       (setq done (point))))
    (when (< (point) orig) (point))))

(defun py--go-to-keyword (regexp &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((maxindent
	 (or maxindent
	     (if (empty-line-p)
		 (progn
		   (py-backward-statement)
		   (current-indentation))
	       (or maxindent (and (< 0 (current-indentation))(current-indentation))
		   ;; make maxindent large enough if not set
		   (* 99 py-indent-offset)))))
        done erg)
    (if (eq 0 maxindent)
	;; faster jump to top-level forms
	(setq erg (py--go-to-keyword-bol regexp))
      (while (and (not done) (not (bobp)))
	(py-backward-statement)
	(cond ((eq 0 (current-indentation))
	       (when (looking-at regexp) (setq erg (point)))
	       (setq done t))
	      ;; ((and (< (current-indentation) maxindent)
	      ;; 	  (setq maxindent (current-indentation))
	      ;; 	  (looking-at regexp))
	      ;;  (setq erg (point))
	      ;;  (setq done t))
	      ((and (<= (current-indentation) maxindent)
		    (setq maxindent (current-indentation))
		    (looking-at regexp))
	       (setq erg (point))
	       (setq done t)))))
    (when (and py-mark-decorators (looking-at py-def-or-class-re))
      (setq done (py--up-decorators-maybe (current-indentation)))
      (when done (setq erg done)))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py--clause-lookup-keyword (regexp arg &optional indent origline)
  "Returns a list, whose car is indentation, cdr position. "
  (let* ((origline (or origline (py-count-lines)))
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
         ((and complement-re (looking-at complement-re)(<= (current-indentation) maxindent))
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

(defun py-leave-comment-or-string-backward ()
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
    (when (and (looking-back py-section-end (line-beginning-position))(< orig (point)))
      (point))))

(provide 'python-components-move)
;;;  python-components-move.el ends here
