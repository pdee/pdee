;;; python-components-paragraph.el --- filling -*- lexical-binding: t; -*-

;; Original Author: Fabián E. Gallina <fabian@anue.biz>
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

(defun py-fill-paren (&optional justify)
  "Paren fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (interactive "*P")
  (save-restriction
    (save-excursion
      (let ((pps (parse-partial-sexp (point-min) (point))))
	(if (nth 1 pps)
	    (let* ((beg (copy-marker (nth 1 pps)))
		   (end (and beg (save-excursion (goto-char (nth 1 pps))
						 (forward-list))))
		   (paragraph-start "\f\\|[ \t]*$")
		   (paragraph-separate ","))
	      (when (and beg end (narrow-to-region beg end))
		(fill-region beg end justify)
		(while (not (eobp))
		  (forward-line 1)
		  (py-indent-line)
		  (goto-char (line-end-position))))))))))

(defun py-fill-string-django (&optional justify)
  "Fill docstring according to Django's coding standards style.

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'django t))

(defun py-fill-string-onetwo (&optional justify)
  "One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'onetwo t))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257 t))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257-nn t))

(defun py-fill-string-symmetric (&optional justify)
  "Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'symmetric t))

(defun py-set-nil-docstring-style ()
  "Set py-docstring-style to \\='nil"
  (interactive)
  (setq py-docstring-style 'nil)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-nn-docstring-style ()
  "Set py-docstring-style to \\='pep-257-nn"
  (interactive)
  (setq py-docstring-style 'pep-257-nn)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-docstring-style ()
  "Set py-docstring-style to \\='pep-257"
  (interactive)
  (setq py-docstring-style 'pep-257)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-django-docstring-style ()
  "Set py-docstring-style to \\='django"
  (interactive)
  (setq py-docstring-style 'django)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-symmetric-docstring-style ()
  "Set py-docstring-style to \\='symmetric"
  (interactive)
  (setq py-docstring-style 'symmetric)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-onetwo-docstring-style ()
  "Set py-docstring-style to \\='onetwo"
  (interactive)
  (setq py-docstring-style 'onetwo)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
  (interactive "*P")
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix (format "%s" r the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          (fill-paragraph justify))))
    t))

(defun py-fill-labelled-string (beg end)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (interactive "r*")
  (let ((end (copy-marker end))
        (last (copy-marker (point)))
        this-beg)
    (save-excursion
      (save-restriction
        ;; (narrow-to-region beg end)
        (goto-char beg)
        (skip-chars-forward " \t\r\n\f")
        (if (looking-at py-labelled-re)
            (progn
              (setq this-beg (line-beginning-position))
              (goto-char (match-end 0))
              (while (and (not (eobp)) (re-search-forward py-labelled-re end t 1)(< last (match-beginning 0))(setq last (match-beginning 0)))
                (save-match-data (fill-region this-beg (1- (line-beginning-position))))
                (setq this-beg (line-beginning-position))
                (goto-char (match-end 0)))))))))

(defun py--in-or-behind-or-before-a-docstring (pps)
  (interactive "*")
  (save-excursion
    (let* ((strg-start-pos (when (nth 3 pps) (nth 8 pps)))
	   (n8pps (or strg-start-pos
		      (when
			  (equal (string-to-syntax "|")
				 (syntax-after (point)))
			(and
			 (< 0 (skip-chars-forward "\"'"))
			 (nth 3 (parse-partial-sexp (point-min) (point))))))))
      (and n8pps (py--docstring-p n8pps)))))

(defun py--string-fence-delete-spaces (&optional start)
  "Delete spaces following or preceding delimiters of string at point. "
  (interactive "*")
  (let ((beg (or start (nth 8 (parse-partial-sexp (point-min) (point))))))
    (save-excursion
      (goto-char beg)
      (skip-chars-forward "\"'rRuU")
      (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point)))
      (goto-char beg)
      (forward-char 1)
      (skip-syntax-forward "^|")
      (skip-chars-backward "\"'rRuU")
      ;; (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point)))
)))

(defun py--skip-raw-string-front-fence ()
  "Skip forward chars u, U, r, R followed by string-delimiters. "
  (when (member (char-after) (list ?u ?U ?r ?R))
    (forward-char 1))
  (skip-chars-forward "\'\""))

(defun py--fill-fix-end (thisend orig delimiters-style)
  ;; Add the number of newlines indicated by the selected style
  ;; at the end.
  ;; (widen)
  (goto-char thisend)
  (skip-chars-backward "\"'\n ")
  (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
  (unless (eq (char-after) 10)
    (and
     (cdr delimiters-style)
     (or (newline (cdr delimiters-style)) t)))
  (py-indent-line nil t)
  (goto-char orig))

(defun py--fill-docstring-last-line (thisend beg end multi-line-p)
  (widen)
  ;; (narrow-to-region thisbeg thisend)
  (goto-char thisend)
  (skip-chars-backward "\"'")
  (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point)))
  ;; (narrow-to-region beg end)
  (fill-region beg end)
  (setq multi-line-p (string-match "\n" (buffer-substring-no-properties beg end)))
  (when multi-line-p
    ;; adjust the region to fill according to style
    (goto-char end)))

(defun py--fill-docstring-base (thisbeg thisend style multi-line-p beg end py-current-indent orig)
  ;; (widen)
  ;; fill-paragraph causes wrong indent, lp:1397936
  ;; (narrow-to-region thisbeg thisend)
  (let ((delimiters-style
	 (pcase style
	   ;; delimiters-style is a cons cell with the form
	   ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
	   ;; is NIL means to not add any newlines for start or end
	   ;; of docstring.  See `py-docstring-style' for a
	   ;; graphic idea of each style.
	   (`django (cons 1 1))
	   (`onetwo (and multi-line-p (cons 1 2)))
	   (`pep-257 (and multi-line-p (cons nil 2)))
	   (`pep-257-nn (and multi-line-p (cons nil 1)))
	   (`symmetric (and multi-line-p (cons 1 1))))))
    ;;  (save-excursion
    (when style
      ;; Add the number of newlines indicated by the selected style
      ;; at the start.
      (goto-char thisbeg)
      (py--skip-raw-string-front-fence)
      (skip-chars-forward "'\"")
      (when
	  (car delimiters-style)
	(unless (or (py-empty-line-p)(eolp))
	  (newline (car delimiters-style))))
      (indent-region beg end py-current-indent))
    (when multi-line-p
      (goto-char thisbeg)
      (py--skip-raw-string-front-fence)
      (skip-chars-forward " \t\r\n\f")
      (forward-line 1)
      (beginning-of-line)
      (unless (py-empty-line-p) (newline 1)))
    (py--fill-fix-end thisend orig delimiters-style)))

(defun py--fill-docstring-first-line (beg end)
  "Refill first line after newline maybe. "
  (fill-region-as-paragraph beg (line-end-position) nil t t)
  (save-excursion
    (end-of-line)
    (unless (eobp)
      (forward-line 1)
      (back-to-indentation)
      (unless (or (< end (point)) (py-empty-line-p))
	(newline 1)
	))))

(defun py--fill-paragraph-in-docstring (beg &optional done)
  ;; (goto-char innerbeg)
  (let* ((fill-column (- fill-column (current-indentation)))
	 (parabeg (max beg (py--beginning-of-paragraph-position)))
	 (paraend (copy-marker (py--end-of-paragraph-position)))
         (done done))
    ;; if paragraph is a substring, take it
    (goto-char parabeg)
    (unless done (py--fill-docstring-first-line parabeg paraend))
    (end-of-line) 
    (skip-chars-forward " \t\r\n\f")
    (unless (or 
             ;; (< paraend (point))
             (eobp))
      (py--fill-paragraph-in-docstring (point) t))))

(defun py--fill-docstring (justify style docstring orig py-current-indent &optional beg end)
  ;; Delete spaces after/before string fencge
  (py--string-fence-delete-spaces beg)
  (let* ((beg (or beg docstring))
	 (innerbeg (copy-marker (progn (goto-char beg) (py--skip-raw-string-front-fence) (point))))
         (end (copy-marker
	       (or end
                   (progn
		     (goto-char innerbeg)
		     ;; (py--skip-raw-string-front-fence)
		     (skip-syntax-forward "^|")
		     (1+ (point))))))
	 (innerend (copy-marker (progn (goto-char end)(skip-chars-backward "\\'\"") (point))))
	 (multi-line-p (string-match "\n" (buffer-substring-no-properties innerbeg innerend))))
    (save-restriction
      (narrow-to-region (point-min) end)

      (when (string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end))
	(py-fill-labelled-string beg end))
      ;; (first-line-p (<= (line-beginning-position) beg)
      (goto-char innerbeg)
      (py--fill-paragraph-in-docstring beg))
    (py--fill-docstring-base innerbeg innerend style multi-line-p beg end py-current-indent orig)))

(defun py-fill-string (&optional justify style docstring pps)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' "
  (interactive "*")
  (let* ((justify (or justify (if current-prefix-arg 'full t)))
	 (style (or style py-docstring-style))
	 (pps (or pps (parse-partial-sexp (point-min) (point))))
	 (indent
	  ;; set inside tqs
	  ;; (save-excursion (and (nth 3 pps) (goto-char (nth 8 pps)) (current-indentation)))
	  nil)
	 (orig (copy-marker (point)))
	 ;; (docstring (or docstring (py--in-or-behind-or-before-a-docstring pps)))
	 (docstring (cond (docstring
			   (if (not (number-or-marker-p docstring))
			       (py--in-or-behind-or-before-a-docstring pps))
			   docstring)
			  (t (py--in-or-behind-or-before-a-docstring pps))))
	 (beg (and (nth 3 pps) (nth 8 pps)))
	 (tqs (progn (and beg (goto-char beg) (looking-at "\"\"\"\\|'''") (setq indent (current-column)))))
	 (end (copy-marker (if tqs
			       (or
				(progn (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
				(goto-char orig)
				(line-end-position))
			     (or (progn (goto-char beg) (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
				 (goto-char orig)
				 (line-end-position))))))
    (goto-char orig)
    (when beg
      (if docstring
	  (py--fill-docstring justify style docstring orig indent beg end)
	(save-restriction
	  (if (not tqs)
	      (if (py-preceding-line-backslashed-p)
		  (progn
		    (setq end (copy-marker (line-end-position)))
		    (narrow-to-region (line-beginning-position) end)
		    (fill-region (line-beginning-position) end justify t)
		    (when (< 1 (py-count-lines))
		      (py--continue-lines-region (point-min) end)))
		(narrow-to-region beg end)
		(fill-region beg end justify t)
		(when
		    ;; counting in narrowed buffer
		    (< 1 (py-count-lines))
		  (py--continue-lines-region beg end)))
	    (fill-region beg end justify)))))))

(defun py--continue-lines-region (beg end)
  (save-excursion
    (goto-char beg)
    (while (< (line-end-position) end)
      (end-of-line)
      (unless (py-escaped-p) (insert-and-inherit 32) (insert-and-inherit 92))
      (ignore-errors (forward-line 1)))))

(defun py-fill-paragraph (&optional justify pps beg end tqs)
  (interactive "*")
  (save-excursion
    (save-restriction
      (window-configuration-to-register py--windows-config-register)
      (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
	     (docstring (unless (not py-docstring-style) (py--in-or-behind-or-before-a-docstring pps)))
	     (fill-column py-comment-fill-column)
	     (in-string (nth 3 pps)))
	(cond ((or (nth 4 pps)
		   (and (bolp) (looking-at "[ \t]*#[# \t]*")))
	       (py-fill-comment))
	      (docstring
	       (setq fill-column py-docstring-fill-column)
	       (py--fill-docstring justify py-docstring-style docstring (point)
				   ;; current indentation
				   (save-excursion (and (nth 3 pps) (goto-char (nth 8 pps)) (current-indentation)))))
	      (t
	       (let* ((beg (or beg (save-excursion
				     (if (looking-at paragraph-start)
					 (point)
				       (backward-paragraph)
				       (when (looking-at paragraph-start)
					 (point))))
			       (and (nth 3 pps) (nth 8 pps))))
		      (end (or end
			       (when beg
				 (save-excursion
				   (or
				    (and in-string
					 (progn
					   (goto-char (nth 8 pps))
					   (setq tqs (looking-at "\"\"\"\\|'''"))
					   (forward-sexp) (point)))
				    (progn
				      (forward-paragraph)
				      (when (looking-at paragraph-separate)
					(point)))))))))
		 (and beg end (fill-region beg end))
		 (when (and in-string (not tqs))
		   (py--continue-lines-region beg end))))))
      (jump-to-register py--windows-config-register))))

(defun py-fill-string-or-comment ()
  "Serve auto-fill-mode"
  (unless (< (current-column) fill-column)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (if (nth 3 pps)
	(py-fill-string nil nil nil pps)
      ;; (py-fill-comment pps)
      (do-auto-fill)
      ))))

(provide 'python-components-paragraph)
;;; python-components-paragraph.el ends here
