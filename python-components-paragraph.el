;;; python-components-paragraph.el --- filling -*- lexical-binding: t; -*-

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

;;; Commentary:
;;

;;; Code:

(defun py-set-nil-docstring-style ()
  "Set ‘py-docstring-style’ to \\='nil."
  (interactive)
  (setq py-docstring-style 'nil)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-nn-docstring-style ()
  "Set ‘py-docstring-style’ to \\='pep-257-nn."
  (interactive)
  (setq py-docstring-style 'pep-257-nn)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-pep-257-docstring-style ()
  "Set ‘py-docstring-style’ to \\='pep-257."
  (interactive)
  (setq py-docstring-style 'pep-257)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-django-docstring-style ()
  "Set ‘py-docstring-style’ to \\='django."
  (interactive)
  (setq py-docstring-style 'django)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-symmetric-docstring-style ()
  "Set ‘py-docstring-style’ to \\='symmetric."
  (interactive)
  (setq py-docstring-style 'symmetric)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py-set-onetwo-docstring-style ()
  "Set ‘py-docstring-style’ to \\='onetwo."
  (interactive)
  (setq py-docstring-style 'onetwo)
  (when (and (called-interactively-p 'any) py-verbose-p)
    (message "docstring-style set to:  %s" py-docstring-style)))

(defun py--continue-lines-region (beg end)
  ".
Argument BEG beginning of the region.
Argument END end of the region."
  (save-excursion
    (goto-char beg)
    (while (< (line-end-position) end)
      (end-of-line)
      (unless (py-escaped-p) (insert-and-inherit 32) (insert-and-inherit 92))
      (ignore-errors (forward-line 1)))))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point.
Optional argument JUSTIFY: see ‘fill-paragraph’."
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
       ;; which starts the comment should not be part of a string or character.
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
  "Return start/end position of a docstring, if inside.

Nil otherwise
Argument PPS result of ‘parse-partial-sexp’."
  (interactive "*")
  (save-excursion
    (let ((pps (or pps
                   (cond ((looking-back (concat py-delimiter-re "[ \t]*") (line-beginning-position))
                          (goto-char (match-beginning 0))
                          (skip-chars-backward "^ \t\r\n\f")
                          (parse-partial-sexp (point-min) (point)))
                         ((looking-at (concat "[ \t]*" py-delimiter-re))
                          (goto-char (match-end 0))
                          (parse-partial-sexp (point-min) (point)))))))
      (when (nth 8 pps)
        (goto-char (nth 8 pps))
        (skip-chars-backward "^ \t\r\n\f")
        (save-excursion
          ;; (setq erg (point))
          (while (or
                  (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                  (py-backward-comment)))
          (back-to-indentation)
          (when (looking-at py-def-or-class-re)
            (nth 8 pps)))))))

(defun py--skip-raw-string-front-fence ()
  "Skip forward chars u, U, r, R followed by string-delimiters."
  (when (member (char-after) (list ?u ?U ?r ?R))
    (forward-char 1))
  (skip-chars-forward "\'\""))

(defun py-fill-labelled-string (beg end this-fill-prefix)
  "Fill string or paragraph containing lines starting with label

See lp:1066489 "
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (let ((end (copy-marker end))
          (old-fill-prefix this-fill-prefix)
          (new-fill-prefix
           (if (looking-at py-star-labelled-re)
               (make-string (progn (goto-char (match-beginning 2))(current-column)) 32)
             (make-string (+ (current-indentation) py-indent-offset) 32))))
      ;; fill the first line with different fill-prefix
      (py--fill-region (line-beginning-position) (line-end-position) this-fill-prefix)
      ;; (forward-line 1)
      ;; words from first line here?
      (end-of-line) (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
      (fixup-whitespace)
      ;; (setq fill-prefix new-fill-prefix)
      (py--fill-region (line-beginning-position) end new-fill-prefix)
      ;; restore fill-prefix
      (setq fill-prefix old-fill-prefix))))

(defun py--docstring-first-line (innerbeg innerend multi-line-p this-fill-prefix)
  "Fill the first line according to ‘py-docstring-style’.

Return position of first line incluse empty line according to style"
  (goto-char innerbeg)
  (py-fixup-whitespace)
  ;; (narrow-to-region innerbeg end)
  (save-excursion
    (if multi-line-p
        (progn 
        (save-restriction
          ;; (narrow-to-region innerbeg (line-end-position))
          (narrow-to-region (line-beginning-position) (line-end-position))
          ;; w/ pep-257-nn and multiline: no newline after quotes:
          ;; """Process foo, return bar.
          (unless (member py-docstring-style (list 'pep-257 'pep-257-nn))
            (newline 1)
            (indent-according-to-mode))
          (save-excursion
            (save-restriction
              ;; (narrow-to-region (point) (line-end-position))
              (narrow-to-region (line-beginning-position) (line-end-position))
              (py--fill-region (line-beginning-position) (line-end-position) this-fill-prefix))))
      (forward-line 1)
      (unless (py-empty-line-p)
        (newline 1)
        (indent-according-to-mode)))
    ;; a single line
    ;; (goto-char innerbeg)
    ;; (delete-horizontal-space)
    (py--fill-region (point) innerend this-fill-prefix)
    (goto-char innerbeg)
    (when (eq py-docstring-style 'django)
      (newline 1)
      (indent-according-to-mode)
      (goto-char innerend)
      (newline 1)
      (indent-according-to-mode))))
  (forward-line 1)
  (end-of-line)
  (skip-chars-forward " \t\r\n\f")
  (line-beginning-position))

(defun py--fill-next-end ()
  "Check if next paragraph starts from an empty line or ‘labelled’. "
  (let ((next-labelled-start (save-excursion (end-of-line) (or (setq next-labelled-start (and (re-search-forward (concat py-star-labelled-re "\\|" py-colon-labelled-re) nil t 1 ) (line-beginning-position))) (point-max))))
         (plain-paragraph-start (save-excursion (progn (py-forward-paragraph) (point)))))
    ;; a paragraph may end before the next match
    (if (< plain-paragraph-start next-labelled-start) plain-paragraph-start next-labelled-start)))
        ;; fill the lesser region
        ;; (py-fill-labelled-string (point) this this-fill-prefix)
      ;; (py-fill-labelled-string (point) (- last 1) this-fill-prefix))))

(defun py--fill-docstring-intern (this-fill-prefix &optional justify)
  "Call it form string-start position. "
  (skip-chars-forward " \t\r\n\f")
  (unless (eobp)
    (beginning-of-line)
    (unless (eobp)
      (let ((this-end (copy-marker (py--fill-next-end))))
        (cond
         ((looking-at (concat py-star-labelled-re "\\|" py-colon-labelled-re))
          (py-fill-labelled-string (line-beginning-position)
                                   (- this-end 1)
                                   ;; this-end
                                   this-fill-prefix)
          (end-of-line)
          ;; (py-fill-labelled-string (point) end-pos this-fill-prefix)
          ;; end is next label or paragraph-separator
          (py--fill-docstring-intern this-fill-prefix))
         (t (py--fill-region (point)
                             (- this-end 1)
                             ;; this-end
                             this-fill-prefix)
            (end-of-line)
            (py--fill-docstring-intern this-fill-prefix)))))))

(defun py--fill-docstring (&optional beg end pps docstring this-fill-prefix)
  "Fills paragraph in docstring below or at cursor position."
  (interactive "*")
  (let* ((pps (or pps (parse-partial-sexp (point-min) (point))))
         (beg (or beg
                  (when (and (nth 3 pps) (nth 8 pps))
                    (goto-char (nth 8 pps))
                    (skip-chars-backward "\"\\|'")
                    (point))))
         (docstring (or docstring beg))
         (cuid (current-indentation))
         (end (or end
                  (copy-marker
                   (progn (forward-sexp))(point))))
         (innerbeg (copy-marker
                    (progn
                      (goto-char docstring)
                      (py--skip-raw-string-front-fence)
                      (point))))
         (innerend (if end
                       ;; (sit-for 0.1)
                       (copy-marker (progn (goto-char end) (skip-chars-backward "\\'\"") (skip-chars-backward " \t\r\n\f") (point)))
                     (error "py--fill-docstring: cant see ‘end’")))
         (multi-line-p
          ;; yes, if first line is longer than fill-column
          (<= (if docstring py-docstring-fill-column fill-column)
              (+ cuid (if (eq py-docstring-style 'django)
                          (+ 3 (- innerend innerbeg))
                        ;; the string to be subtracted
                        (- innerend innerbeg)))))
         (new-beg (py--docstring-first-line innerbeg innerend multi-line-p this-fill-prefix)))
    (save-restriction
      (narrow-to-region new-beg innerend)
      (goto-char (point-min))
      (py--fill-docstring-intern this-fill-prefix)
      (when
          ;; (and multi-line-p
          (or (eq py-docstring-style 'onetwo) (eq py-docstring-style 'pep-257))
        (goto-char end)
        (unless (py-empty-line-p)
          (split-line))))))

(defun py--forward-paragraph ()
  "Returns position reached. "
  (interactive)
  (re-search-forward (concat py-paragraph-start "\\|" py-paragraph-separate) nil 'move 1)
  (when (looking-at "[ \t]*-+")
    (forward-line -1)
    ;; (beginning-of-line)
    )
  (skip-chars-backward " \t\r\n\f")
  (point))

(defun py--fill-region (beg end this-fill-prefix)
  "Start is expected at BOL. "
  (save-restriction
    (narrow-to-region beg end)
    (let ((end (copy-marker end)))
      ;; (goto-char beg)
      (while (not (eobp))
        (unless (eq (current-indentation) (length this-fill-prefix))
          (beginning-of-line)
          (delete-horizontal-space)
          (insert this-fill-prefix))
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (save-excursion (delete-region (point) (progn (skip-chars-forward " \t\r\n\f" (max end (line-end-position))) (point))))
        (fixup-whitespace)
        (end-of-line)
        (while (< (current-fill-column) (current-column))
          (backward-word))
        (unless (or (eobp)(eolp))
          (newline 1)
          (insert this-fill-prefix))
        ;; (end-of-line)
        ;; (setq last (point))
        )
      ;; fix the last line
      (unless (eq (current-indentation) (length this-fill-prefix))
        (save-excursion
          (beginning-of-line)
          (delete-horizontal-space)
          (insert this-fill-prefix))))))
;; (<= fill-column (current-column))
;; (py-fixup-whitespace '(4) end)

(defun py--fill-paragraph-intern (beg-raw pps docstring in-string end-first end-raw justify)
  ""
  (let* ((beg (or beg-raw
                  ;; (save-excursion
                  (cond ((looking-at paragraph-start)
                         (line-beginning-position))
                        (in-string
                         (max (nth 8 pps)
                              (and docstring (re-search-backward (concat py-paragraph-start "\\|" py-paragraph-separate) nil 'move 1)
                                   (goto-char (max (line-beginning-position) docstring))
                                   ;; docstring is not at BOL, so this jump is needed
                                   ;; (line-beginning-position)
                                   )
                              docstring)))))
         (tqs (or docstring (and in-string (goto-char (nth 8 pps)) (looking-at "\"\"\"\\|'''"))))
         ;;
         (end
          (save-excursion
            ;; unless definded by region: end-in-docstring
            ;; check later against result of forward-paragraph, take the min
            (copy-marker
             (or end-first
                 (if end-raw
                     (min (progn (end-of-line)
                                 (py--forward-paragraph)
                                 (line-beginning-position))
                          end-raw)
                   (progn (end-of-line)
                          (py--forward-paragraph)
                          (line-beginning-position)))))))
         ;; (labelled (progn (goto-char beg) (skip-chars-forward "\"'") (skip-chars-forward " \t\r\n\f") (looking-at (concat py-star-labelled-re "\\|" py-colon-labelled-re))))
         ;; ‘labelled’ reached beg
         (this-fill-prefix (make-string (current-indentation) 32)))
    (save-restriction
      ;; (narrow-to-region (or beg docstring) end)
      (cond ((nth 4 pps) ;; inside comment
             (goto-char (nth 8 pps))
             
             (py-fill-comment))
             ((looking-at "[ \t]*#[# \t]*")
              (py-fill-comment))
            ;; (labelled (py-fill-labelled-string beg end this-fill-prefix))
            ((and docstring
                  (or (eq docstring beg)
                      (progn (goto-char docstring)
                             (skip-chars-backward " \t\r\n\f")
                             (skip-chars-backward "\"'")
                             (eq beg (point)))))
             ;; (setq fill-column py-docstring-fill-colum;; n)
             (py--fill-docstring beg end pps docstring this-fill-prefix))
            (t
             (and beg end (py--fill-region beg end this-fill-prefix))
             (when (and in-string (not tqs))
               (py--continue-lines-region beg end)))))
    (jump-to-register py--windows-config-register)))

(defun py-fill-paragraph (&optional justify pps beg end)
  "Fill the paragraph at point honoring ‘py-docstring-style’."
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (let* (end-raw
         (beg-first (or beg (and (use-region-p) (region-beginning))))
         (end-first (or end (and (use-region-p) (region-end))))
         (pps (or pps (parse-partial-sexp (point-min) (point))))
         (docstring (unless (not py-docstring-style) (py--in-or-behind-or-before-a-docstring pps)))
         ;; determining prefix need ‘line-beginning-position’
         (beg-raw (if beg-first (progn (goto-char beg-first) (line-beginning-position)) docstring))
         ;; (fill-column py-comment-fill-column)
         (in-string (and (nth 3 pps) (nth 8 pps))))
    (if in-string
        (save-restriction
          (save-excursion
            (narrow-to-region (progn (goto-char (nth 8 pps))(line-beginning-position))
                              (setq end-raw (or end-first (progn (goto-char (nth 8 pps)) (forward-sexp) (point))))))
          (py--fill-paragraph-intern (or beg-raw (point-min)) pps docstring in-string end-first end-raw justify))
      (py--fill-paragraph-intern beg-raw pps docstring in-string end-first end-raw justify))))

(defun py-fill-string (&optional justify docstring pps)
  "String fill function.
JUSTIFY should be used (if applicable) as in ‘fill-paragraph’.

Fill according to ‘py-docstring-style’ "
  (interactive "*")
  (let* ((this-fill-prefix fill-prefix)
         (justify (or justify (if current-prefix-arg 'full t)))
         ;; (style (or style py-docstring-style))
         (pps (or pps (parse-partial-sexp (point-min) (point))))
         (orig (copy-marker (point)))
         ;; (docstring (or docstring (py--in-or-behind-or-before-a-docstring pps)))
         (docstring (cond (docstring
                           (if (not (number-or-marker-p docstring))
                               ;; (nth 8 (parse-partial-sexp (point-min) (point))
                               (py--in-or-behind-or-before-a-docstring pps)
                             docstring))
                          (t (and (nth 3 pps) (nth 8 pps) (py--in-or-behind-or-before-a-docstring pps)))))
         (beg (when
                  (and (nth 3 pps) (nth 8 pps))
                (skip-chars-backward "\"\\|'" (line-beginning-position))
                (point)))
         (tqs (and beg
                   ;; (goto-char beg)
                   ;; (looking-at "\"\" \"\\|'''")
                   (looking-at py-string-delim-re)
                   (point)))
         (end (copy-marker (if tqs
                               (or
                                (progn
                                  ;; (skip-chars-backward "\"\\|'")
                                  (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
                                (goto-char orig)
                                (line-end-position))
                             (or (progn (goto-char beg) (ignore-errors (forward-sexp))(and (< orig (point)) (point)))
                                 (goto-char orig)
                                 (line-end-position))))))
    (save-restriction
      ;; do not go backward beyond beginning of string
      (narrow-to-region beg (point-max))
      (goto-char orig)
      (when beg
        (if docstring
            (py--fill-docstring beg end pps docstring this-fill-prefix)
          (if (not tqs)
              (if (py-preceding-line-backslashed-p)
                  (progn
                    (setq end (copy-marker (line-end-position)))
                    (narrow-to-region (line-beginning-position) end)
                    (py--fill-region (line-beginning-position) end this-fill-prefix)
                    (when (< 1 (py-count-lines))
                      (py--continue-lines-region (point-min) end)))
                (narrow-to-region beg end)
                (py--fill-region beg end this-fill-prefix)
                (when
                    ;; counting in narrowed buffer
                    (< 1 (py-count-lines))
                  (py--continue-lines-region beg end)))
            (py--fill-region beg end this-fill-prefix)))))))

(defun py-fill-string-or-comment ()
  "Serve auto-fill-mode"
  (unless (< (current-column) fill-column)
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (if (nth 3 pps)
          (py-fill-string nil nil pps)
        ;; (py-fill-comment pps)
        (do-auto-fill)))))

(defun py-fill-string-django (&optional justify)
  "Fill docstring according to Django's coding standards style.

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
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

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'onetwo t))

(defun py-fill-string-pep-257 (&optional justify)
  "PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'pep-257 t))

(defun py-fill-string-pep-257-nn (&optional justify)
  "PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
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

See available styles at ‘py-fill-paragraph’ or var ‘py-docstring-style’
"
  (interactive "*P")
  (py-fill-string justify 'symmetric t))

(provide 'python-components-paragraph)
;;; python-components-paragraph.el ends here
