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
(defun py-fill-paragraph (&optional justify style start end)
  "`fill-paragraph-function'

commands py-fill-paragraph-SUFFIX
choose one of the following implemented styles:

DJANGO, ONETWO, PEP-257, PEP-257-NN, SYMMETRIC

Otherwise `py-fill-docstring-style' is used. Explanation:

DJANGO:

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

ONETWO:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257-NN:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

SYMMETRIC:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"
"
  (interactive "P")
  (or (fill-comment-paragraph justify)
      (let ((orig (copy-marker (point)))
            (beg (or start (if (use-region-p) (region-beginning) (py-beginning-of-paragraph-position))))
            (end (copy-marker (or end (if (use-region-p) (region-end) (py-end-of-paragraph-position)))))
            pps
            (style (or style py-fill-docstring-style))
            (this-end (point-min)))
        (save-excursion
          (save-restriction
            (goto-char beg) (end-of-line)
            (setq pps (syntax-ppss))
            (narrow-to-region beg end)
            (cond
             ;; Comments
             ((nth 4 pps)
              (py-fill-comment justify))
             ;; Strings/Docstrings
             ((save-excursion
                (or (nth 3 pps)
                    (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (looking-at py-string-delim-re))
                (goto-char (point-min))
                (while (and (progn (forward-paragraph) (< this-end (point)))(setq this-end (copy-marker (point))))
                  (py-fill-string justify style beg this-end)
                  (goto-char this-end)
                  ;; (end-of-line) (while (nth 8 (syntax-ppss))(forward-char 1))
                  (set (make-local-variable 'py-fill-docstring-style) nil))))
             ;; Decorators
             ((save-excursion
                (equal (char-after
                        (py-beginning-of-statement))
                       ;; (back-to-indentation)
                       ;; (point))
                       ?\@))
              (py-fill-decorator justify))
             ;; Parens
             ((or (nth 1 pps)
                  (looking-at (python-rx open-paren))
                  (save-excursion
                    (skip-syntax-forward "^(" (line-end-position))
                    (looking-at (python-rx open-paren))))
              (py-fill-paren justify))
             (t t)))))))

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

;; (defun py-fill-comment (&optional justify)
;;   "Comment fill function for `py-fill-paragraph'.
;; JUSTIFY should be used (if applicable) as in `fill-paragraph'."
;;   (fill-comment-paragraph justify))

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

(defun py-fill-string (&optional justify style beg end)
  "String fill function for `py-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (interactive "P")
  (let* ((py-fill-docstring-style (or style py-fill-docstring-style))
         (fill-column (if (integerp py-docstring-fill-column)
                          py-docstring-fill-column
                        fill-column))
         ;; unset python-mode value this time
         forward-sexp-function
         (orig (point-marker))
         (pps (syntax-ppss))
         (beg (or beg (if (nth 3 pps)
                          (copy-marker (nth 8 pps))
                        (when (and (equal (string-to-syntax "|")
                                          (syntax-after (point))))
                          (point-marker)))))
         (delim-length (progn (goto-char beg)(when (looking-at py-string-delim-re) (- (match-end 0) (match-beginning 0)))))
         ;; Assume docstrings at BOL resp. indentation
         (docstring-p
          (and delim-length
               (eq (current-column) (current-indentation))
               (not (looking-at py-labelled-re))))
         (end (or end (progn
                        (forward-sexp)
                        (point-marker))))
         (multi-line-p
          ;; Docstring styles may vary for oneliners and multi-liners.
          (> (count-matches "\n" beg end) 0))
         (delimiters-style
          (case py-fill-docstring-style
            ;; delimiters-style is a cons cell with the form
            ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
            ;; is NIL means to not add any newlines for start or end
            ;; of docstring.  See `py-fill-docstring-style' for a
            ;; graphic idea of each style.
            (django (cons 1 1))
            (onetwo (and multi-line-p (cons 1 2)))
            (pep-257 (and multi-line-p (cons nil 2)))
            (pep-257-nn (and multi-line-p (cons nil 1)))
            (symmetric (and multi-line-p (cons 1 1)))))
         (fill-paragraph-function))
    (save-restriction
      (cond (docstring-p
             (narrow-to-region (+ beg delim-length) (- end delim-length))
             (fill-region (+ beg delim-length) (- end delim-length)))
            ((string-match (concat "^" py-labelled-re) (buffer-substring-no-properties beg end))
             (py-fill-labelled-string beg end))
            (t (narrow-to-region beg end)
               (fill-region beg end))))
    (save-excursion
      (when (and docstring-p py-fill-docstring-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ beg delim-length))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             ;; (indent-according-to-mode)
             (indent-region (+ beg delim-length) (- end delim-length)))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= end (point-max)))
                       (- end delim-length)
                     end))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= end (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))
        (when (or (eq style 'pep-257)(eq style 'pep-257-nn))
          (indent-region beg end))) t)))

(defun py-fill-decorator (&optional justify)
  "Decorator fill function for `py-fill-paragraph'.
"
  ;; (interactive "*P")
  t)

(defun py-fill-paren (&optional justify)
  "Paren fill function for `py-fill-paragraph'.
"
  (interactive "*P")
  (save-restriction
    (narrow-to-region
     (progn
       (while (python-syntax-context 'paren)
         (goto-char (1- (point-marker))))
       (point-marker)
       (line-beginning-position))
     (progn
       (when (not (python-syntax-context 'paren))
         (end-of-line)
         (when (not (python-syntax-context 'paren))
           (skip-syntax-backward "^)")))
       (while (python-syntax-context 'paren)
         (goto-char (1+ (point-marker))))
       (point-marker)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (py-indent-line)
      (goto-char (line-end-position)))) t)

(defun py-fill-string-django (&optional justify)
  "Fill docstring according to Django's coding standards style.

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
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

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'onetwo))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'pep-257))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
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

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
"
  (interactive "*P")
  (py-fill-string justify 'symmetric))

(provide 'python-components-paragraph)
;;; python-components-paragraph.el ends here
