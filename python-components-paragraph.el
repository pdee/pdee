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

(defun py--fill-docstring-first-line (beg end multi-line-p)
  "Refill first line after newline maybe. "
  (let (;;(beg (copy-marker
        (lae (length (buffer-substring-no-properties beg (line-end-position)))))
    (cond ((and
            ;; newline if multiline
            (member py-docstring-style (list 'django 'onetwo 'symmetric))
            (or multi-line-p (< lae (+ 3 (- fill-column (current-indentation))))))
           (goto-char beg)
           (newline 1)
           (indent-according-to-mode)))
    (fill-region-as-paragraph beg (line-end-position) nil t t)
    (goto-char beg)
    (forward-line 1)
    (back-to-indentation)
    (unless (or (< end (point)) (py-empty-line-p))
	  (split-line))))

(defun py-travel-single-words-and-symbols (beg end)
  (skip-chars-forward " \t\r\n\f" end)
  (while (and (looking-at " *\\w+ *$\\| *\\s.+ *$")(< end (line-end-position)))
    (forward-line 1)
    (back-to-indentation))
  (max beg (point)))

(defun py-fill-labelled-string (beg end)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (interactive "r*")
  (let ((end (copy-marker end))
        (old-fill-prefix fill-prefix))
    (goto-char beg)
    (when (save-excursion (end-of-line) (re-search-forward py-labelled-re end t 1))
      (setq end (match-beginning 0)))
    (skip-chars-forward " \t\r\n\f")
    (py-travel-single-words-and-symbols beg end)
    (if (looking-at py-star-labelled-re)
        (setq fill-prefix (make-string (+ (current-indentation) 2) 32))
      (setq fill-prefix (make-string (+ (current-indentation) py-indent-offset) 32)))
    ;; (while (or (looking-at py-colon-labelled-re)
    ;;            (looking-at py-star-labelled-re))
    ;;   (forward-line 1))
    (fill-region-as-paragraph (line-beginning-position) end)
    (setq fill-prefix old-fill-prefix)))

(defun py--fill-docstring (docstring &optional beg end)
  (let* ((orig (point))
         (beg (or beg (progn (goto-char docstring) (line-beginning-position))))
         (end (copy-marker (or end (progn (goto-char beg)
                                          (skip-chars-forward " \t\r\n\f")
                                          (py--skip-raw-string-front-fence)
                                          (skip-syntax-forward "^|")
		                          (1+ (point)))))))
    (save-restriction
      ;; don't go backward beyond beginning of string
      (narrow-to-region beg end)
      (let* (;; If paragraph starts with beginning of string, skip the fence-chars
	     (innerbeg (copy-marker
                        (goto-char docstring)
                        (max
                         (py--skip-raw-string-front-fence)
		         (progn (unless (looking-at paragraph-start)
		                  (backward-paragraph))
                                (skip-chars-forward " \t\r\n\f")
		                ;; (when (looking-at paragraph-start)
		                (point)))))
	     (innerend (copy-marker (progn (goto-char end) (skip-chars-backward "\\'\"") (skip-chars-backward " \t\r\n\f") (point))))
	     (multi-line-p (string-match "\n" (buffer-substring-no-properties innerbeg innerend)))
             ;; (paragraph-separate (concat py-symbol-re "\\|" py-star-labelled-re "\\|" py-colon-labelled-re "\\|" paragraph-separate))
             ;; (paragraph-start (concat py-symbol-re "\\|" py-star-labelled-re "\\|" py-colon-labelled-re "\\|"  paragraph-start))
             parabeg paraend on-first-line)
        (setq paraend
              (save-excursion
                (goto-char orig)
                (py-travel-single-words-and-symbols innerbeg innerend)
                (end-of-line)
                (if (re-search-forward py-labelled-re end t)
                    (progn
                      (min (progn (beginning-of-line) (skip-chars-backward " \t\r\n\f") (point))
                           (save-excursion (goto-char orig) (forward-paragraph) (point)) innerend))
                  (progn (forward-paragraph) (skip-chars-backward " \t\r\n\f" orig) (min (point) innerend)))))
        (setq parabeg (max (progn (goto-char paraend) (backward-paragraph) (skip-chars-forward " \t\r\n\f") (point)) innerbeg))
        (setq on-first-line (< (line-beginning-position) docstring))
        (if (or (string-match (concat "^" py-colon-labelled-re) (buffer-substring-no-properties parabeg paraend))
                (string-match (concat "^" py-star-labelled-re) (buffer-substring-no-properties parabeg paraend)))
            (py-fill-labelled-string parabeg paraend)
          (when on-first-line (py--fill-docstring-first-line parabeg (line-end-position) multi-line-p))
          (setq parabeg (py-travel-single-words-and-symbols parabeg paraend))
          (goto-char parabeg)
          (setq fill-prefix (make-string (current-column) 32))
          (fill-region-as-paragraph parabeg paraend t))
        (when (member py-docstring-style (list 'pep-257 'onetwo))
          (goto-char paraend)
          (forward-line -1)
          (unless (py-empty-line-p)
            (forward-line 1)
            (split-line)))))))

(defun py-fill-string (&optional justify docstring pps)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' "
  (interactive "*")
  (let* ((justify (or justify (if current-prefix-arg 'full t)))
	 ;; (style (or style py-docstring-style))
	 (pps (or pps (parse-partial-sexp (point-min) (point))))
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
    (save-restriction
      ;; don't go backward beyond beginning of string
      (narrow-to-region beg (point-max))
      (goto-char orig)
      (when beg
        (if docstring
	    (py--fill-docstring docstring beg end)
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

(defun py-fill-paragraph (&optional pps beg end tqs)
  (interactive "*")
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
	   (py--fill-docstring docstring
			       ;; current indentation
			       ;; (save-excursion (and (nth 3 pps) (goto-char (nth 8 pps)) (current-indentation)))
                               ))
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
  (jump-to-register py--windows-config-register))

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
