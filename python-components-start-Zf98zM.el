;;; python-components-start-Zf98zM.el -- Searching downwards in buffer -*- lexical-binding: t; -*-

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

;;; Commentary: ‘ar--go-to-keyword’ and related fundamental stuff

;;; Code:

(defcustom py-mark-decorators nil
  "If decorators should be marked too.

Default is nil.

Also used by navigation"
  :type 'boolean
  :tag "py-mark-decorators")

(defun py-end-of-string ()
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point)))
        (sapo (car (syntax-after (point)))))
    (if (and (nth 3 pps)(nth 8 pps))
        (progn (goto-char (nth 8 pps))
               (forward-sexp))
      (if (or (eq sapo 7) (eq sapo 15))
          (and (skip-syntax-forward "|\"")
               (skip-syntax-forward "^|\""))))
    (skip-syntax-forward "|\"")))

(defun py-escaped-p (&optional pos)
    "Return t if char at POS is preceded by an odd number of backslashes. "
    (save-excursion
      (when pos (goto-char pos))
      (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped-p))))

(defun py--skip-to-comment-or-semicolon ()
  "Returns position if point was moved."
  (let ((orig (point)))
    (cond ((while (and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
                       ;; (sit-for 1)
                       (and (nth 8 (parse-partial-sexp (point-min) (point))) (skip-chars-forward "#;" (line-end-position)))))))
    (and (< orig (point))(point))))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list  (string-to-char comment-start) 10))(forward-line 1)(back-to-indentation))))

(defun py-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

ORIG - consider original position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (orig (or orig (point)))
	  erg last
	  ;; use by scan-lists
	  forward-sexp-function pps err)
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; which-function-mode, lp:1235375
       ;; (re-search-forward "'ar-\\([[:alpha:]-]+" nil t 1)
       ((< py-max-specpdl-size repeat)
	(error "forward-statement reached loops max. If no error, customize ‘max-specpdl-size’"))
       ((looking-at (symbol-value (quote py-def-or-class-re)))
        (end-of-line)
        (skip-chars-backward " \t\r\n\f"))
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
        (py--end-of-comment-intern (point)))
       ;; (goto-char (match-end 0))
       ;; (py-forward-statement orig done repeat))
       ((nth 4 pps)
	(py--end-of-comment-intern (point))
	(py--skip-to-comment-or-semicolon)
	(while (and (eq (char-before (point)) ?\\)
		    (py-escaped-p) (setq last (point)))
	  (forward-line 1) (end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	;; py-forward-statement-test-3JzvVW
	(unless (or (looking-at (concat " *" comment-start))(eolp))
	  (py-forward-statement orig done repeat)))
       ;; string
       ((looking-at py-string-delim-re)
	(goto-char (match-end 0))
	(py-forward-statement (match-beginning 0) done repeat))
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
		    (py-escaped-p))
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
	;; (setq done t)
	(py-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(py--skip-to-comment-or-semicolon)
	(setq pps (parse-partial-sexp orig (point)))
	(if (nth 1 pps)
	    (py-forward-statement orig done repeat)
	  (unless done
	    (py-forward-statement orig done repeat))))
       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon))
	(py-forward-statement orig done repeat)))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9 ?#)))
	(setq erg (point)))
      (if (and py-verbose-p err)
	  (py--message-error err))
      erg)))

(defun py-backward-statement (&optional orig done limit ignore-in-string-p repeat maxindent)
  "Go to the initial line of a simple statement.

For beginning of compound statement use ‘ar-backward-block’.
For beginning of clause ‘ar-backward-clause’.

‘ignore-in-string-p’ allows moves inside a docstring, used when
computing indents
ORIG - consider original position or point.
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
	  (error "py-forward-statement reached loops max. If no error, customize ‘ar-max-specpdl-size’"))
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
         ((looking-at comment-start-skip)
	  (setq done t)
	  (forward-char -1)
          (skip-chars-backward " \t\r\n\f")
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
	erg))))

(defun py-backward-statement-bol ()
  "Goto beginning of line where statement start.
Returns position reached, if successful, nil otherwise.

See also ‘ar-up-statement’"
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
    erg))

(defun py-forward-statement-bol ()
  "Go to the ‘beginning-of-line’ following current statement."
  (interactive)
  (py-forward-statement)
  (py--beginning-of-line-form))

(defun py-beginning-of-statement-p ()
  (interactive)
  (save-restriction
    (eq (point)
    (save-excursion
      (py-forward-statement)
      (py-backward-statement)))))

(defun py-up-statement ()
  "go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise."
  (interactive)
  (if (py--beginning-of-statement-p)
      (py-backward-statement)
    (progn (and (py-backward-statement) (py-backward-statement)))))

(defun py--end-of-statement-p ()
  "Return position, if cursor is at the end of a statement, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (py-backward-statement)
      (py-forward-statement)
      (when (eq orig (point))
        orig))))

(defun py-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Corresponds to backward-up-list in Elisp
Return position if statement found, nil otherwise."
  (interactive)
  (let* ((orig (point)))
    (cond ((py--end-of-statement-p)
	   (progn
	     (and
	      (py-forward-statement)
	      (py-backward-statement)
	      (< orig (point))
	      (point))))
	  ((ignore-errors (< orig (and (py-forward-statement) (py-backward-statement))))
	   (point))
	  ((ignore-errors (< orig (and (py-forward-statement) (py-forward-statement)(py-backward-statement))))
	     (point)))))

(defun py--fetch-indent-statement-above (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (back-to-indentation)
    (if (or (looking-at comment-start)(py-beginning-of-statement-p))
        (current-indentation)
      (py-backward-statement)
      (current-indentation))))

(defun py--end-base-determine-secondvalue (regexp)
  "Expects being at block-opener.

REGEXP: a symbol"
  (cond
   ((eq regexp (quote py-minor-block-re))
    (cond ((looking-at py-else-re)
	   nil)
	  ((or (looking-at (concat py-try-re)))
	   (concat py-elif-re "\\|" py-else-re "\\|" py-except-re))
	  ((or (looking-at (concat py-except-re "\\|" py-elif-re "\\|" py-if-re)))
	   (concat py-elif-re "\\|" py-else-re))))
   ((member regexp
	    (list
	     (quote py-block-re)
	     (quote py-block-or-clause-re)
	     (quote py-clause-re)
	     (quote py-if-re)
	     ))
    (cond ((looking-at py-if-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-elif-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-else-re))
	  ((looking-at py-try-re)
	   (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
	  ((looking-at py-except-re)
	   (concat py-else-re "\\|" py-finally-re))
	  ((looking-at py-finally-re)
	   nil)))
   ((eq regexp (quote py-for-re)) nil)
   ((eq regexp (quote py-try-re))
    (cond
     ;; ((looking-at py-try-re)
     ;;  (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
     ((looking-at py-except-re)
      (concat py-else-re "\\|" py-finally-re))
     ((looking-at py-finally-re))
     (t
      (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))))))

(defun py--backward-regexp (regexp &optional indent condition orig regexpvalue)
  "Search backward next regexp not in string or comment.

Return position if successful"
  (unless (bobp)
    (save-match-data
      (unless (py-beginning-of-statement-p) (skip-chars-backward " \t\r\n\f")
	      (py-backward-comment (point)))
      (let* (pps
	     (regexpvalue (or regexpvalue (symbol-value regexp)))
	     (indent (or indent (current-indentation)))
             (condition (or condition '<=))
	     (orig (or orig (point)))
             (allvalue (if (member regexp (list (quote py-block-re) (quote py-clause-re) (quote py-def-or-class-re) (quote py-def-re) (quote py-class-re))) regexpvalue (symbol-value (quote py-block-or-clause-re)))))
        (if (eq (current-indentation) (current-column))
	    (while (and (not (bobp))
		        (re-search-backward allvalue nil 'move 1)
		        (not (and (looking-back "async *" (line-beginning-position))
			          (goto-char (match-beginning 0))))
		        (or (and
                             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
                             (goto-char pps))
                            (not (looking-at regexpvalue))
                            (and (not (eq (current-column) 0))
                                 (looking-at regexpvalue)
                                 indent
                                 ;; def foo(): pass
                                 ;; def bar(): pass_|_
                                 ;; need '<
		      	         (funcall condition indent (current-indentation))))
                        (prog1 t
                          (when (< (current-indentation) indent)

                            ;; update required indent if a form should not match
                            ;; as ‘def bar()’, when ‘ar-backward-def’ from end below

                            ;; def foo():
                            ;;     if True:
                            ;;         def bar():
                            ;;             pass
                            ;;     elif False:
                            ;;         def baz():
                            ;;             pass
                            ;;     else:
                            ;;         try:
                            ;;             1 == 1
                            ;;         except:
                            ;;             pass

                            (setq indent (current-indentation))))))
	  (unless (bobp)
            (back-to-indentation)
	    (and
             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
             (goto-char pps))
	    (unless (and (< (point) orig) (looking-at regexpvalue))
	      (py--backward-regexp regexp (current-indentation) condition orig))
            (unless (or (eq (point) orig)(bobp)) (back-to-indentation))))
        (and (looking-at regexpvalue) (not (nth 8 (parse-partial-sexp (point-min) (point))))(point))))))

(defun py--go-to-keyword (regexp &optional condition maxindent ignoreindent)
  "Expects being called from beginning of a statement.

Argument REGEXP: a symbol.

Return a list if found, whose car holds indentation, cdr position in buffer.

Keyword detected from REGEXP
Honor MAXINDENT if provided
Optional IGNOREINDENT: find next keyword at any indentation"
  (unless (bobp)
    ;;    (when (py-empty-line-p) (skip-chars-backward " \t\r\n\f"))
    (let* ((orig (point))
	   (regexp (if (eq regexp (quote py-clause-re)) (quote py-extended-block-or-clause-re) regexp))
	   (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
	   (maxindent
	    (if ignoreindent
		;; just a big value
		9999
	      (or maxindent
                  (if (py-empty-line-p) (current-column) (current-indentation)))))
           (allvalue (symbol-value (quote py-block-or-clause-re)))
           erg)
      (unless (py-beginning-of-statement-p)
	(py-backward-statement))
      (when (and (not (string= "" py-block-closing-keywords-re))(looking-at py-block-closing-keywords-re))
        (setq maxindent (min maxindent (- (current-indentation) py-indent-offset))))
      (cond
       ((and (looking-at regexpvalue)(< (point) orig))
        (setq erg (point)))
        (t (while
              (not (or (bobp) (and (looking-at regexpvalue)(< (point) orig) (not (nth 8 (parse-partial-sexp (point-min) (point)))))))
             ;; search backward and reduce maxindent, if non-matching forms suggest it
              (setq erg (py--backward-regexp allvalue maxindent
                                         (or condition '<=)
                                         orig regexpvalue)))))
      erg)))

(defun py-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like `(quote py-clause-re)'

Return position if successful"
  (unless (py-beginning-of-statement-p) (py-backward-statement))
  (unless (looking-at (symbol-value regexp))
    (py--go-to-keyword regexp '< (or indent (current-indentation))))
  ;; now from beginning-of-block go one indent level upwards
  (when
      (looking-at (symbol-value regexp))
    (py--go-to-keyword regexp '< (- (or indent (current-indentation)) py-indent-offset))))

(defun py-up-base-bol (regexp)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise.
Argument REGEXP determined by form"
  (let* (;;(orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (parse-partial-sexp (point-min) (point)))))
      (beginning-of-line)
      (when (looking-at regexp) (setq erg (point)))
      ;; (when py-verbose-p (message "%s" erg))
      erg)))

(defun py--down-according-to-indent (regexp secondvalue &optional indent use-regexp)
  "Return position if moved, nil otherwise.

Optional ENFORCE-REGEXP: search for regexp only."
  (unless (eobp)
    (let* ((orig (point))
	   (indent (or indent 0))
	   done
	   (regexpvalue (if (member regexp (list (quote py-def-re) (quote py-def-or-class-re) (quote py-class-re)))
			    (concat (symbol-value regexp) "\\|" (symbol-value (quote py-decorator-re)))
			  (symbol-value regexp)))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`ar-try-re (concat py-finally-re "\\|" py-except-re "\\|" py-else-re))
			     (`ar-if-re py-else-re))))
           last)
      (if (eq regexp (quote py-clause-re))
          (py-forward-clause-intern indent)
        (while
	    (and
	     (not done)
	     (progn (end-of-line)
		    (cond (use-regexp
			   ;; using regexpvalue might stop behind global settings, missing the end of form
			   (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
			  (t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_@]+") nil 'move 1))))
	     (or (nth 8 (parse-partial-sexp (point-min) (point)))
	         (progn (back-to-indentation) (py--forward-string-maybe (nth 8 (parse-partial-sexp orig (point)))))
	         (and secondvalue (looking-at secondvalue) (setq last (point)))
	         (and lastvalue (looking-at lastvalue)(setq last (point)))
	         (and (looking-at regexpvalue) (setq done t) (setq last (point)))
	         ;; py-forward-def-or-class-test-3JzvVW
	         ;; (setq done t)
                 ))))
      (when last (goto-char last))
      (and (< orig (point)) (point)))))

(defun py--end-base (regexp &optional orig bol repeat)
  "Used internal by functions going to the end FORM.

Returns the indentation of FORM-start
Arg REGEXP, a symbol"
  (unless (eobp)
    (let (;; not looking for an assignment
	  (use-regexp (member regexp (list (quote py-def-re) (quote py-class-re) (quote py-def-or-class-re))))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-backward-statement))
	(let* (;; when at block-start, be specific
	       ;; (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
               ;; (regexp (or regexp (symbol-value (quote py-extended-block-or-clause-re))))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
                           (current-indentation)
			   ;; (if (bolp) 0
			   ;;   (abs
			   ;;    (- (current-indentation) py-indent-offset)))
			 (current-indentation)))
               (secondvalue (py--end-base-determine-secondvalue regexp))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (py-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list (quote py-def-re) (quote py-def-or-class-re) (quote py-class-re)))
				(looking-at py-decorator-re)
				(py-down-def-or-class (current-indentation)))
			   (and (member regexp (list (quote py-minor-block-re) (quote py-if-re) (quote py-for-re) (quote py-try-re)))
				(looking-at py-minor-clause-re))))
		  (list (current-indentation) (point) secondvalue))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) secondvalue))
		 ((eq 0 (current-indentation))
		  (py--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (py--go-to-keyword regexp (if (member regexp (list (quote py-def-re) (quote py-class-re) (quote py-def-or-class-re))) '< '<=)
                                       ;; (if (and (member regexp (list (quote py-block-re) (quote py-clause-re) (quote py-def-or-class-re) (quote py-def-re) (quote py-class-re))) (looking-at (symbol-value regexp))) '< '<=)
                                       )))))
	  ;; (py-for-block-p (looking-at py-for-re))
	  ;; (setq indent (current-indentation))
	  (cond
	   (res
	    (and
	     (py--down-according-to-indent regexp secondvalue (current-indentation))
             (progn
               (when (and secondvalue (looking-at secondvalue))
                 ;; (when (looking-at py-else-re)
                 (py--down-according-to-indent regexp secondvalue (current-indentation)))
	       ;; (if (>= indent (current-indentation))
	       (py--down-end-form))
	     ;; (py--end-base regexp orig bol repeat)
	     ;; )
	     ))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (py--forward-regexp (symbol-value regexp))
	      (beginning-of-line)
	      (and
	       (py--down-according-to-indent regexp secondvalue (current-indentation) t)
	       (py--down-end-form))))
	  (cond ((< orig (point))
		 (if bol
                     (py--beginning-of-line-form)
		   (point)))
		((eq (point) orig)
		 (unless (eobp)
		   (cond
		    ((and (< repeat 1)
			  (or
			   ;; looking next indent as part of body
			   (py--down-according-to-indent regexp secondvalue
							 indent
							 ;; if expected indent is 0,
							 ;; search for new start,
							 ;; search for regexp only
							 (eq 0 indent))
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent py-indent-offset)) indent))
			    (py--down-according-to-indent regexp secondvalue
							  indent t))))
		     (py--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (py--down-according-to-indent regexp secondvalue nil t)
		   (py--end-base regexp (point) bol (1+ repeat))))))))))

;; python-components-start-Zf98zM.el ends here
(provide 'python-components-start-Zf98zM)
