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
(defun py-fill-this-paragraph ()
  "Fill just the paragraph at point. "
  (interactive "*")
  (py-fill-string justify style (if (py-beginning-of-paragraph-p) (point) (progn (py-beginning-of-paragraph)(point))) (progn (py-end-of-paragraph)(point))))

(defun py-fill-paragraph (&optional justify style start end docstring)
  "`fill-paragraph-function'

If `py-paragraph-fill-docstring-p' and inside a docstring, the whole docstring is formatted.
See also `py-fill-string' "
  (interactive "P")
  (or (fill-comment-paragraph justify)
      (let* ((orig (copy-marker (point)))
             (pps (syntax-ppss))
             (docstring (and
                         ;; py-paragraph-fill-docstring-p

                         (or docstring (py-docstring-p (nth 8 pps)))))
             (beg (or start (and (use-region-p) (region-beginning)) (and
                                                                     ;; py-paragraph-fill-docstring-p
                                                                     docstring (nth 8 pps)) (py-beginning-of-paragraph-position)))
             (end (copy-marker (or end (and (use-region-p) (region-end)) (and
                                                                          ;; py-paragraph-fill-docstring-p

                                                                          docstring (py-end-of-string (nth 8 pps))) (py-end-of-paragraph-position))))
             (style (or style py-docstring-style))
             (this-end (point-min)))
        (when (and (nth 3 pps) (< beg (nth 8 pps))
                   (py-docstring-p (nth 8 pps))
                   (setq beg (nth 8 pps)))
          (setq end (py-end-of-string (nth 8 pps))))
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
              (goto-char beg)
              (if (and py-paragraph-fill-docstring-p docstring
                       ;; (re-search-forward (concat "^" py-labelled-re) nil t)
                       )
                  (progn
                    (goto-char beg)
                    ;; must process one by one
                    (while (and (not (eobp)) (setq last (point)) (forward-paragraph) (< last (point))(< (point) end)(setq this-end (point)))
                      (save-restriction
                        (narrow-to-region last this-end)
                        (goto-char last)
                        ;; (if (re-search-forward (concat "^" py-labelled-re) nil t this-end)
                        ;; (py-fill-labelled-string last this-end)

                        (py-fill-string justify style last this-end pps 'no)
                        ;;)
                        (goto-char this-end)
                        (widen))))
                (goto-char orig)
                (py-fill-this-paragraph)))
             ;; Decorators
             ((save-excursion
                (and (py-beginning-of-statement)
                     (equal (char-after)
                            ;; (back-to-indentation)
                            ;; (point))
                            ?\@)))
              (py-fill-decorator justify))
             (t (goto-char orig)
                (py-fill-string justify style (if (py-beginning-of-paragraph-p) (point) (py-beginning-of-paragraph)) (py-end-of-paragraph))))))
        (goto-char orig)
        (back-to-indentation))
      (recenter-top-bottom)
      ;; fill-paragraph expexts t
      t))

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

(defun py-fill-string (&optional justify style beg end pps docstring)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'.

DOCSTRING is either a boolean or 'no
If `py-paragraph-fill-docstring-p' is `t', `M-q` fills the
complete docstring according to setting of `py-docstring-style' "
  (interactive "P")
  (save-excursion
    (save-restriction
      (let* ((style (or style py-docstring-style))
             (fill-column (if (integerp py-docstring-fill-column)
                              py-docstring-fill-column
                            fill-column))
             ;; unset python-mode value this time
             forward-sexp-function
             (orig (point-marker))
             (pps (or pps (syntax-ppss)))
             ;; if beginning of string is closer than arg beg, use this
             (beg (or (and (numberp beg)
                           (ignore-errors (copy-marker beg)))
                      (cond ((and (nth 3 pps) (nth 8 pps))
                             (goto-char (nth 8 pps))
                             (skip-chars-forward "\"'")
                             (copy-marker (point)))
                            ((equal (string-to-syntax "|")
                                    (syntax-after (point)))
                             (point-marker)))))
             ;; Assume docstrings at BOL resp. indentation
             (docstring (and (not (eq 'no docstring))(py-docstring-p (nth 8 pps))))
             (end (or (ignore-errors (and end (goto-char end) (skip-chars-backward "\"'")(copy-marker (point))))
                      (progn (goto-char (nth 8 pps)) (scan-sexps (point) 1) (skip-chars-backward "\"'") (point-marker))))
             multi-line-p
             delimiters-style
             erg)
        ;; whitespace and newline will be added according to mode again
        (goto-char beg)
        (setq beg (progn (skip-chars-forward "\"'") (copy-marker (point))))
        (and docstring
             (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (skip-chars-forward " \t\r\n\f")(point))))
        (goto-char end)
        (and docstring
             (delete-region (point) (progn (skip-chars-backward " \t\r\n\f")(point))))
        (cond
         ((and docstring
               (string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end)))
          (py-fill-labelled-string beg end))
         ((and docstring)
          (narrow-to-region beg end)
          (fill-region (point-min) (point-max)))
         (t (narrow-to-region beg end)
            (sit-for 0.1)
            (fill-region beg end)))
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
            ;; at the start of the docstring.
            (goto-char beg)
            (and
             (car delimiters-style)
             (unless (or (empty-line-p) (save-excursion (forward-line -1)(empty-line-p)))
               (or (newline (car delimiters-style)) t))
             (indent-region beg end))
            ;; Add the number of newlines indicated by the selected style
            ;; at the end of the docstring.
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
