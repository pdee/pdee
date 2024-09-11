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

;;; Commentary: ‘py--go-to-keyword’ and related fundamental stuff

;;; Code:

(defun py--end-base-determine-secondvalue (regexp)
  "Expects being at block-opener.

REGEXP: a symbol"
  (cond
   ((eq regexp 'py-minor-block-re)
    (cond ((looking-at py-else-re)
	   nil)
	  ((or (looking-at (concat py-try-re)))
	   (concat py-elif-re "\\|" py-else-re "\\|" py-except-re))
	  ((or (looking-at (concat py-except-re "\\|" py-elif-re "\\|" py-if-re)))
	   (concat py-elif-re "\\|" py-else-re))))
   ((member regexp
	    (list
	     'py-block-re
	     'py-block-or-clause-re
	     'py-clause-re
	     'py-if-re
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
   ((eq regexp 'py-for-re) nil)
   ((eq regexp 'py-try-re)
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
             (allvalue (if (member regexp (list 'py-block-re 'py-clause-re 'py-def-or-class-re 'py-def-re 'py-class-re)) regexpvalue (symbol-value 'py-block-or-clause-re))))
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
                            ;; as ‘def bar()’, when ‘py-backward-def’ from end below

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
	   (regexp (if (eq regexp 'py-clause-re) 'py-extended-block-or-clause-re regexp))
	   (regexpvalue (symbol-value regexp))
	   (maxindent
	    (if ignoreindent
		;; just a big value
		9999
	      (or maxindent
                  (if (py-empty-line-p) (current-column) (current-indentation)))))
           (allvalue (symbol-value 'py-block-or-clause-re))
           erg)
      (unless (py-beginning-of-statement-p)
	(py-backward-statement))
      (when (looking-at py-block-closing-keywords-re)
        (setq maxindent (min maxindent (- (current-indentation) py-indent-offset))))
      (cond
       ((and (looking-at regexpvalue)(< (point) orig))
        (setq erg (point)))
        (t (while
              (not (or (bobp) (and (looking-at regexpvalue)(< (point) orig) (not (nth 8 (parse-partial-sexp (point-min) (point)))))))
              (setq erg (py--backward-regexp allvalue maxindent
                                         (or condition '<=)
                                         orig regexpvalue)))))
      erg)))

(defun py-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like `'py-clause-re'

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
	   (regexpvalue (if (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
			    (concat (symbol-value regexp) "\\|" (symbol-value 'py-decorator-re))
			  (symbol-value regexp)))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`py-try-re (concat py-finally-re "\\|" py-except-re "\\|" py-else-re))
			     (`py-if-re py-else-re))))
           last)
      (if (eq regexp 'py-clause-re)
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
	  (use-regexp (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-backward-statement))
	(let* (;; when at block-start, be specific
	       ;; (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (symbol-value regexp))
               ;; (regexp (or regexp (symbol-value 'py-extended-block-or-clause-re)))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
			   (if (bolp) 0
			     (abs
			      (- (current-indentation) py-indent-offset)))
			 (current-indentation)))
               (secondvalue (py--end-base-determine-secondvalue regexp))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (py-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
				(looking-at py-decorator-re)
				(py-down-def-or-class (current-indentation)))
			   (and (member regexp (list 'py-minor-block-re 'py-if-re 'py-for-re 'py-try-re))
				(looking-at py-minor-clause-re))))
		  (list (current-indentation) (point) secondvalue))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) secondvalue))
		 ((eq 0 (current-indentation))
		  (py--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (py--go-to-keyword regexp (if (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)) '< '<=)
                                       ;; (if (and (member regexp (list 'py-block-re 'py-clause-re 'py-def-or-class-re 'py-def-re 'py-class-re)) (looking-at (symbol-value regexp))) '< '<=)
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
