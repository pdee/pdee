;;; python-components-paragraph.el --- filling -*- lexical-binding: t; -*- 

;; Original Author: Fabián E. Gallina <fabian@anue.biz>
;; Maintainer: Andreas Röhler <andreas.roehler@online.de>

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
	      (when end (narrow-to-region beg end)
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
  "Set py-docstring-style to 'nil"
  (interactive)
  (setq py-docstring-style 'nil)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-nn-docstring-style ()
  "Set py-docstring-style to 'pep-257-nn"
  (interactive)
  (setq py-docstring-style 'pep-257-nn)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-docstring-style ()
  "Set py-docstring-style to 'pep-257"
  (interactive)
  (setq py-docstring-style 'pep-257)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-django-docstring-style ()
  "Set py-docstring-style to 'django"
  (interactive)
  (setq py-docstring-style 'django)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-symmetric-docstring-style ()
  "Set py-docstring-style to 'symmetric"
  (interactive)
  (setq py-docstring-style 'symmetric)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-onetwo-docstring-style ()
  "Set py-docstring-style to 'onetwo"
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

(defun py--in-or-behind-or-before-a-docstring ()
  (interactive "*")
  (save-excursion
    (let* ((raw-pps (nth 8 (parse-partial-sexp (point-min) (point))))
	   ;; ;; maybe just behind a string
	   (n8 (or raw-pps
		   ;; maybe in front of a string
		   (back-to-indentation)
		   (nth 8 (parse-partial-sexp (point-min) (point)))))
	   (n8pps (or n8
		      (when
			  (equal (string-to-syntax "|")
				 (syntax-after (point)))
			(and
			  (< 0 (skip-chars-forward "\"'"))
			  (nth 8 (parse-partial-sexp (point-min) (point))))))))
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
      (skip-syntax-forward "^\|")
      (skip-chars-backward "\"'rRuU")
      ;; (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point)))
)))

(defun py--skip-raw-string-front-fence ()
  "Skip forward chars u, U, r, R followed by string-delimiters. "
  (when (member (char-after) (list ?u ?U ?r ?R))
    (forward-char 1))
  (skip-chars-forward "\'\""))

(defun py--fill-fix-end (thisend orig docstring delimiters-style)
  ;; Add the number of newlines indicated by the selected style
  ;; at the end.
  ;; (widen)
  (goto-char thisend)
  (skip-chars-backward "\"'\n ")
  (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
  (unless (eq (char-after) ?\n)
    (and
     (cdr delimiters-style)
     (or (newline (cdr delimiters-style)) t)))
  ;; (py-indent-region docstring thisend)
  (goto-char orig))

(defun py--fill-docstring-base (thisbeg thisend style multi-line-p beg end py-current-indent orig docstring)
  ;; (widen)
  ;; fill-paragraph causes wrong indent, lp:1397936
  ;; (narrow-to-region thisbeg thisend)
  (let ((delimiters-style
	 (case style
	   ;; delimiters-style is a cons cell with the form
	   ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
	   ;; is NIL means to not add any newlines for start or end
	   ;; of docstring.  See `py-docstring-style' for a
	   ;; graphic idea of each style.
	   (django (cons 1 1))
	   (onetwo (and multi-line-p (cons 1 2)))
	   (pep-257 (and multi-line-p (cons nil 2)))
	   (pep-257-nn (and multi-line-p (cons nil 1)))
	   (symmetric (and multi-line-p (cons 1 1))))))
    ;;  (save-excursion
    (when style
      ;; Add the number of newlines indicated by the selected style
      ;; at the start.
      (goto-char thisbeg)
      (py--skip-raw-string-front-fence)
      (skip-chars-forward "\'\"")
      (when
	  (car delimiters-style)
	(unless (or (empty-line-p)(eolp))
	  (newline (car delimiters-style))))
      (indent-region beg end py-current-indent))
    (when multi-line-p
      (goto-char thisbeg)
      (py--skip-raw-string-front-fence)
      (skip-chars-forward " \t\r\n\f")
      (forward-line 1)
      (beginning-of-line)
      (unless (empty-line-p) (newline)))
    (py--fill-fix-end thisend orig docstring delimiters-style)))

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

(defun py--fill-docstring-first-line (beg end thisbeg thisend style)
  "Refill first line after newline maybe. "
  (fill-region beg (line-end-position))
  (forward-line 1)
  (fill-region (line-beginning-position) end)
  (save-restriction
    (widen)
    (setq multi-line-p (string-match "\n" (buffer-substring-no-properties thisbeg thisend))))
  (when multi-line-p
    ;; adjust the region to fill according to style
    (goto-char beg)
    (skip-chars-forward "\"'")
    ;; style might be nil
    (when style
      (unless (or (eq style 'pep-257-nn)(eq style 'pep-257)(eq (char-after) ?\n))
	(newline-and-indent)
	;; if TQS is at a single line, re-fill remaining line
	(fill-region (point) end)))))

(defun py--fill-docstring (justify style docstring orig py-current-indent)
  ;; Delete spaces after/before string fence
  (py--string-fence-delete-spaces docstring)
  (let* ((thisbeg (copy-marker docstring))
         (thisend (copy-marker
                   (progn
                     (goto-char thisbeg)
		     (py--skip-raw-string-front-fence)
		     (skip-syntax-forward "^\|")
                     (point))))
         (parabeg (progn (goto-char orig) (py--beginning-of-paragraph-position)))
         (paraend (progn (goto-char orig) (py--end-of-paragraph-position)))
         ;; if paragraph is a substring, take it
         (beg (copy-marker (if (< thisbeg parabeg) parabeg thisbeg)))
         (end (copy-marker (if (< thisend paraend) thisend paraend)))
	 (multi-line-p (string-match "\n" (buffer-substring-no-properties thisbeg thisend)))
         first-line-p)
    ;;    (narrow-to-region beg end)
    (goto-char beg)
    (setq first-line-p (member (char-after) (list ?\" ?\' ?u ?U ?r ?R)))
    (cond ((string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end))
           (py-fill-labelled-string beg end))
          (first-line-p
           (py--fill-docstring-first-line beg end thisbeg thisend style))
          ((save-excursion (goto-char end)
			   (or (member (char-after) (list ?\" ?\'))
			       (member (char-before) (list ?\" ?\'))))
           (py--fill-docstring-last-line thisend beg end multi-line-p))
          (t ;; (narrow-to-region beg end)
	     (fill-region beg end justify)))
    (py--fill-docstring-base thisbeg thisend style multi-line-p beg end py-current-indent orig docstring)))

(defun py-fill-string (&optional justify style docstring)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' "
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (list (if current-prefix-arg 'full) t))
    py-docstring-style
    (or docstring (py--in-or-behind-or-before-a-docstring))))
  (let* ((pps (parse-partial-sexp (point-min) (point)))
	 (indent (save-excursion (and (nth 3 pps) (goto-char (nth 8 pps)) (current-indentation))))
	 ;; fill-paragraph sets orig
	 (orig (if (boundp 'orig) (copy-marker orig) (copy-marker (point))))
	 (docstring (if (and docstring (not (number-or-marker-p docstring)))
			(py--in-or-behind-or-before-a-docstring)
		      docstring)))
    (if docstring
	(py--fill-docstring justify style docstring orig indent)
      (py-fill-paragraph justify))))

(defun py-fill-paragraph (&optional justify)
  (interactive "*")
  (save-excursion
    (save-restriction
      (window-configuration-to-register py-windows-config-register)
      (let* ((pps (parse-partial-sexp (point-min) (point)))
	     (docstring (unless (not py-docstring-style)(py--in-or-behind-or-before-a-docstring)))
	     (fill-column py-comment-fill-column))
	(cond ((or (nth 4 pps)
		   (and (bolp) (looking-at "[ \t]*#[# \t]*")))
	       (py-fill-comment))
	      (docstring
	       (setq fill-column py-docstring-fill-column)
	       (py-fill-string justify py-docstring-style docstring))
	      (t
	       (let* ((beg (save-excursion
			       (if (looking-at paragraph-start)
				   (point)
				 (backward-paragraph)
				 (when (looking-at paragraph-start)
				   (point)))))
		      (end
		       (when beg
			 (save-excursion
			   (forward-paragraph)
			   (when (looking-at paragraph-separate)
			     (point))))))
		 (and beg end (fill-region beg end))))))
      (jump-to-register py-windows-config-register))))

(provide 'python-components-paragraph)
;;; python-components-paragraph.el ends here
