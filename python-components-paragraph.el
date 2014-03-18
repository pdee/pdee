;;; python-components-paragraph.el --- filling

;; Original Author: Fabián E. Gallina <fabian@anue.biz>
;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
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
(defun py-fill-paragraph (&optional justify style start end docstring)
  "`fill-paragraph-function'

See also `py-fill-string' "
  (interactive "P")
  (save-excursion
    (if (looking-at "[ \t]*$")
        (progn
          (skip-chars-backward " \t\r\n\f")
          (when (member (char-before) (list ?\' ?\"))
            (skip-chars-backward "'\"")))
      (end-of-line) (skip-chars-backward " \t\r\n\f'\""))
    (or (fill-comment-paragraph justify)
        (let* ((windows-config (window-configuration-to-register 313465889))
               (orig (copy-marker (point)))
	       (pps (syntax-ppss))
	       (docstring (or docstring (py-docstring-p (nth 8 pps))))
               (beg (cond (start)
                          ((use-region-p)
                           (region-beginning))
                          (docstring)
                          (t (py-beginning-of-paragraph-position))))
               (end (copy-marker
                     (cond (end)
                           ((use-region-p) (region-end))
                           (docstring (py-end-of-string (nth 8 pps)))
                           (t (if (or (looking-at paragraph-start)(re-search-forward paragraph-start nil t 1))
                                  (progn (skip-chars-backward " \t\r\n\f")(point))
                                (point))))))
               (style (or style py-docstring-style))
               (this-end (point-min))
               last)
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (cond
               ;; Comments
               ((nth 4 pps)
                (py-fill-comment justify))
               ;; Strings/Docstrings
               ((or (nth 3 pps)
                    (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (looking-at py-string-delim-re))
                (py-fill-string justify style beg end docstring))
               ;; Decorators
               ((save-excursion
                  (and (py-beginning-of-statement)
                       (equal (char-after) ?\@)))
                (py-fill-decorator justify))
               (t (goto-char orig)
                  (py-fill-string justify style (if (py-beginning-of-paragraph-p) (point) (py-beginning-of-paragraph)) (py-end-of-paragraph)))))))
        (py-restore-window-configuration)
        t)))

(defun py-fill-labelled-string (beg end)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (interactive "r*")
  (let ((end (copy-marker end))
        (last (copy-marker (point)))
        this-beg this-end)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
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

(defun py-fill-string (&optional justify style beg end docstring)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

Fill according to `py-docstring-style' "
  (interactive "P")
  (save-excursion
    (save-restriction
      (let* ((style (or style py-docstring-style))
             (fill-column (- (if (integerp py-docstring-fill-column)
				 py-docstring-fill-column
			       fill-column) py-indent-offset))
             ;; unset python-mode value this time
             forward-sexp-function
             (pps (or
                   ;; not needed if beg and end are given
                   (and beg end)
                   (progn
                     (and (eobp)(skip-chars-backward "\"'"))
                     (syntax-ppss))))
             (beg (or (and beg (copy-marker beg))
                      ;; take the beginning of a TQS
                      (cond ((and (nth 3 pps) (nth 8 pps))
                             (goto-char (nth 8 pps))
                             (skip-chars-forward "\"'")
                             (copy-marker (point)))
                            ((equal (string-to-syntax "|")
                                    (syntax-after (point)))
                             (copy-marker (point))))))
             ;; Assume docstrings at BOL resp. indentation
             (docstring (or docstring (unless (eq 'no docstring)
                                        (py-docstring-p pps))))
	     ;; 	     (fill-column
	     ;; 	      (if (or (eq style 'pep-257-nn)(eq style 'pep-257))
	     ;; 		  (- py-docstring-fill-column py-indent-offset)
	     ;; 		py-docstring-fill-column))
	     ;; 	     (fill-column py-docstring-fill-column)
             (end (or (ignore-errors (and end (goto-char end) (skip-chars-backward "\"' \t\f\n")(copy-marker (point))))
                      (progn (or (eq (marker-position beg) (point)) (goto-char (nth 8 pps)))
                             (forward-sexp)
                             (skip-chars-backward "\"'") (point-marker))))
             multi-line-p
             delimiters-style
             erg)
        (goto-char beg)
	;; Line 1 of a multiline comment has to be formatted
	;; differently
	(if (or (eq style 'pep-257-nn)(eq style 'pep-257))
	    (skip-chars-forward "\"'")
	  (setq beg (progn (skip-chars-forward "\"'") (copy-marker (point)))))
	(and docstring
             (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point))))
        (goto-char end)
        (and docstring
             (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point))))
        (cond
         ((and docstring
               (string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end)))
          (py-fill-labelled-string beg end))
         (t (narrow-to-region beg end)
            (goto-char beg)
            (while (re-search-forward "[\n\r \t]+" nil t 1)
              (replace-match " "))
            (fill-region (point-min) (point-max))))
        (and docstring (setq multi-line-p
                             (> (count-matches "\n" beg end) 0)))
        (and docstring
             (setq delimiters-style
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
        (and docstring py-verbose-p (message "%s" delimiters-style))
        (widen)
        (save-excursion
          (when (and docstring style)
            ;; Add the number of newlines indicated by the selected style
            ;; at the start.
            (goto-char beg)
            (and
             (car delimiters-style)
             (unless (or (empty-line-p) (save-excursion (forward-line -1)(empty-line-p)))
               (or (newline (car delimiters-style)) t))
             (indent-region beg end))
            (and multi-line-p
                 (forward-line 1)
                 (unless (empty-line-p) (insert "\n")))
            ;; Add the number of newlines indicated by the selected style
            ;; at the end.
            (goto-char end)
            (unless (eq (char-after) ?\n)
              (and
               (cdr delimiters-style)
               (or (newline (cdr delimiters-style)) t)))
            (setq end (progn (skip-chars-forward " \t\r\n\f")(skip-chars-forward "\"'")(copy-marker (point))))
            (setq beg (progn (goto-char beg) (skip-chars-backward " \t\r\n\f")(skip-chars-backward "\"'") (copy-marker (point)))))
          (indent-region beg end)
          (goto-char end)
          (beginning-of-line)
          (unless (eq (current-indentation) (setq erg (py-compute-indentation)))
            (fixup-whitespace)
            (indent-to erg)))))))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
  (interactive "*P")
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
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
          ;;(message "paragraph-start %S paragraph-separate %S"
          ;;paragraph-start paragraph-separate)
          (fill-paragraph justify))))
    t))

(defun py-fill-decorator (&optional justify)
  "Decorator fill function for `py-fill-paragraph'.
"
  ;; (interactive "*P")
  t)

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
  (py-fill-string justify 'django))

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
  (py-fill-string justify 'onetwo))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257-nn))

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
  (py-fill-string justify 'symmetric))

(defun py-set-onetwo-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'onetwo))

(defun py-set-pep-257-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'pep-257))

(defun py-set-pep-257-nn-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'pep-257-nn))

(defun py-set-symmetric-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'symmetric))

(defun py-set-django-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'django))

(defun py-set-nil-docstring-style ()
  "See customizable `py-docstring-style' "
  (interactive)
  (setq py-docstring-style 'nil))

(provide 'python-components-paragraph)
;;; python-components-paragraph.el ends here
