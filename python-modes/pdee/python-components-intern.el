;; python-components-intern.el --- Part of python-components-mode

;; Helper functions

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes

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

(defun py-nesting-level (&optional pps)
  "Accepts the output of `parse-partial-sexp'. "
  (interactive)
  (let* ((pps (or (ignore-errors (nth 0 pps))
                  (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (syntax-ppss))))
         (erg (nth 0 pps)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line inside repeat)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             (closing closing)
             ;; line: moved already a line backward
             (line line)
             (pps (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (syntax-ppss)))
             ;; in a recursive call already
             (repeat repeat)
             ;; inside: started inside a list
             (inside inside)
             erg indent this-line)
        (unless repeat (setq inside (nth 1 pps))
                (setq repeat t))
        (setq indent
              (cond
               ((and (bobp)
                     (eq origline (py-count-lines)))
                (current-indentation))
               ((and (bobp)(py-statement-opens-block-p))
                (+ (if py-smart-indentation (py-guess-indent-offset nil orig origline) py-indent-offset) (current-indentation)))
               ((and (bobp)(not (py-statement-opens-block-p)))
                (current-indentation))
               ;; (py-in-triplequoted-string-p)
               ((and (nth 3 pps)(nth 8 pps))
                (if (eq origline (py-count-lines))
                    (progn
                      (forward-line -1)
                      (end-of-line)
                      (skip-chars-backward " \t\r\n\f")
                      (if (ignore-errors (< (nth 2 (if (featurep 'xemacs)
                                                       (parse-partial-sexp (point-min) (point))
                                                     (syntax-ppss))) (line-beginning-position)))
                          (current-indentation)
                        (ignore-errors (goto-char (nth 2 pps)))
                        (py-line-backward-maybe)
                        (back-to-indentation)
                        (py-compute-indentation orig origline closing line inside repeat)))
                  (current-indentation)))
               ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               ;; comments
               ((nth 8 pps)
                (if (eq origline (py-count-lines))
                    (progn
                      (goto-char (nth 8 pps))
                      (py-line-backward-maybe)
                      (skip-chars-backward " \t")
                      (py-compute-indentation orig origline closing line inside repeat))
                  (goto-char (nth 8 pps))
                  (if (and line (or py-indent-honors-inline-comment (looking-back "^[ \t]*")))
                      (current-column)
                    (forward-char -1)
                    (py-compute-indentation orig origline closing line inside repeat))))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not (eq (line-beginning-position) (point-min))))
                (forward-line -1)
                (end-of-line)
                (setq line t)
                (py-compute-indentation orig origline closing line inside repeat))
               ;; lists
               ((nth 1 pps)
                (cond ((and inside (not line))
                       (when (and (eq (point) orig) (looking-at "[ \t]*\\()\\)[ \t]*$"))
                         (setq closing (match-beginning 0)))
                       (save-excursion
                         (goto-char (nth 1 pps))
                         (setq this-line (py-count-lines))
                         (cond
                          ((< 0 (- origline this-line))
                           (if (< 1 (- origline this-line))
                               (if closing
                                   (if py-closing-list-dedents-bos
                                       (current-indentation)
                                     (+ (current-indentation) py-indent-offset))
                                 (py-fetch-previous-indent orig))
                             (cond ((looking-at "\\s([ \t]*$")
                                    (if
                                        (progn
                                          (save-excursion
                                            (back-to-indentation)
                                            (looking-at py-block-or-clause-re)))
                                        (progn
                                          (back-to-indentation)
                                          (+ (current-column) (* 2 py-indent-offset)))
                                      (back-to-indentation)
                                      (+ (current-column) py-indent-offset)))
                                   ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
                                    (goto-char (match-beginning 1))
                                    (current-column))
                                   (t (+ (current-column) (* (nth 0 pps)))))))
                          (t (back-to-indentation)
                             (py-beginning-of-statement)
                             (py-compute-indentation orig origline closing line inside repeat)))))
                      ((and (not inside) line)
                       (py-beginning-of-statement)
                       (py-compute-indentation orig origline closing line inside repeat))
                      ((not inside)
                       (progn (goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
                              (when (looking-at "[ \t]+")
                                (goto-char (match-end 0)))
                              (current-column)))
                      (t
                       (goto-char (nth 1 pps))
                       (py-compute-indentation orig origline closing line inside repeat))))
               ((py-preceding-line-backslashed-p)
                (progn
                  (py-beginning-of-statement)
                  (setq this-line (py-count-lines))
                  (if (< 1 (- origline this-line))
                      (py-fetch-previous-indent orig)
                    (if (looking-at "from +\\([^ \t\n]+\\) +import")
                        5
                      (+ (current-indentation) py-continuation-offset)))))
               ((looking-at py-no-outdent-re)
                (if (eq (py-count-lines) origline)
                    (progn
                      (back-to-indentation)
                      (py-line-backward-maybe)
                      (py-compute-indentation orig origline closing line inside repeat))
                  (current-indentation)))
               ((and (looking-at py-block-closing-keywords-re)(eq (py-count-lines) origline))
                (py-beginning-of-block-or-clause)
                (+
                 (if py-smart-indentation (py-guess-indent-offset nil orig origline) py-indent-offset)
                 ;; py-indent-offset
                 (current-indentation)))
               ((looking-at py-block-closing-keywords-re)
                (py-beginning-of-block-or-clause (current-indentation))
                (current-indentation))
               ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
                (py-line-backward-maybe)
                (car (py-clause-lookup-keyword py-elif-re -1 nil orig origline)))
               ((and (looking-at py-clause-re)(eq origline (py-count-lines)))
                (cond ((looking-at py-finally-re)
                       (car (py-clause-lookup-keyword py-finally-re -1 nil orig origline)))
                      ((looking-at py-except-re)
                       (car (py-clause-lookup-keyword py-except-re -1 nil orig origline)))
                      ((looking-at py-else-re)
                       ;; (car (py-clause-lookup-keyword py-else-re -1 (current-indentation))))
                       (car (py-clause-lookup-keyword py-else-re -1 nil orig origline)))
                      ((looking-at py-elif-re)
                       (car (py-clause-lookup-keyword py-elif-re -1 nil orig origline)))
                      ;; maybe at if, try, with
                      (t (car (py-clause-lookup-keyword py-block-or-clause-re -1 nil orig origline)))))
               ((looking-at py-block-or-clause-re)
                (cond ((eq origline (py-count-lines))
                       (py-line-backward-maybe)
                       (py-compute-indentation orig origline closing line inside t))
                      (t (+ (if py-smart-indentation (py-guess-indent-offset nil orig origline) py-indent-offset)(current-indentation)))))
               ((looking-at py-block-closing-keywords-re)
                (py-beginning-of-block)
                (current-indentation))
               ((and (< (current-indentation) (current-column)))
                (back-to-indentation)
                (unless line
                  (setq inside
                        (if (featurep 'xemacs)
                            (nth 1 (parse-partial-sexp (point-min) (point)))
                          (nth 1 (syntax-ppss)))))
                (py-compute-indentation orig origline closing line inside repeat))
               ((not (py-beginning-of-statement-p))
                (if (bobp)
                    (current-column)
                  (if (eq (point) orig)
                      (progn
                        (py-line-backward-maybe)
                        (py-compute-indentation orig origline closing line inside repeat))
                    (py-beginning-of-statement)
                    (py-compute-indentation orig origline closing line inside repeat))))
               ((py-statement-opens-block-p)
                (if (< (py-count-lines) origline)
                    (+ (if py-smart-indentation (py-guess-indent-offset nil orig origline) py-indent-offset) (current-indentation))
                  (py-compute-indentation orig origline closing line inside t)))
               ((and (< (py-count-lines) origline)(looking-at py-assignment-re))
                (current-indentation))
               ((looking-at py-assignment-re)
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               ((and (eq origline (py-count-lines))
                     (save-excursion (and (setq erg (py-go-to-keyword py-block-or-clause-re -1))
                                          (ignore-errors (< orig (py-end-of-block-or-clause))))))
                (+ (car erg) (if py-smart-indentation (py-guess-indent-offset nil orig origline) py-indent-offset)))
               ((and (eq origline (py-count-lines))
                     (py-beginning-of-statement-p))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               (t (current-indentation))))
        (when (and py-verbose-p (interactive-p)) (message "%s" indent))
        indent))))

(defun py-line-backward-maybe ()
  (skip-chars-backward " \t\f" (line-beginning-position))
  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
    (setq line t)))

(defun py-fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (let ((erg (if (eq 1 arg)
                 py-continuation-offset
               (when (numberp arg)
                 (prog1
                     arg
                   (setq py-continuation-offset arg))))))
    (when (interactive-p) (message "%s" py-continuation-offset))
    py-continuation-offset))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py-beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg
          (if (featurep 'xemacs)
              (nth 1 (parse-partial-sexp ppstart (point)))
            (nth 1 (syntax-ppss)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (if (featurep 'xemacs)
                  (parse-partial-sexp ppstart (point))
                (syntax-ppss)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (interactive-p) (message "%s" end))
    end))

(defun py-continuation-line-p ()
  "Return t iff current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (py-preceding-line-backslashed-p)
        (< 0 (py-nesting-level)))))

(defun py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\r\n\f")
    (let ((erg (and (eq (char-before (point)) ?\\ )
                    (py-escaped))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  (interactive)
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (let ((erg (and (eq (char-before (point)) ?\\ )
                    (py-escaped))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-escaped (&optional iact)
  "Return t if char is preceded by an odd number of backslashes. "
  (interactive "p")
  (lexical-let ((orig (point))
                erg)
    (setq erg (< 0 (abs (% (skip-chars-backward "\\\\")2))))
    (goto-char orig)
    (when iact (message "%s" erg))
    erg))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps
            (if (featurep 'xemacs)
                (parse-partial-sexp (line-beginning-position) (point))
              (syntax-ppss)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps
          (if (featurep 'xemacs)
              (parse-partial-sexp (point-min) (point))
            (syntax-ppss)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (if (featurep 'xemacs)
                                          (parse-partial-sexp (point-min) (point))
                                        (syntax-ppss)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-in-string-p ()
  "Returns character address of start of string, nil if not inside. "
  (interactive)
  (let* ((pps
          (if (featurep 'xemacs)
              (parse-partial-sexp (point-min) (point))
            (syntax-ppss)))
         (erg (when (nth 3 pps) (nth 8 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\\|'")
                            (forward-char 1)
                            (setq pps (if (featurep 'xemacs)
                                          (parse-partial-sexp (point-min) (point))
                                        (syntax-ppss)))
                            (when (nth 3 pps) (nth 8 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (interactive)
  (let* ((erg (nth 8 (if (featurep 'xemacs)
                         (parse-partial-sexp (point-min) (point))
                       (syntax-ppss))))
         (la (unless erg (when (or (looking-at "\"")(looking-at comment-start)(looking-at comment-start-skip))
                           (match-beginning 0)))))
    (setq erg (or erg la))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (interactive-p) (message "%s" erg))
        erg))))

;;; Beginning-of- p
(defun py-beginning-of-paragraph-p ()
  "Returns position, if cursor is at the beginning of a paragraph, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (if (and (bolp) (looking-at paragraph-separate))
        (setq erg (point))
      (save-excursion
        (py-end-of-paragraph)
        (py-beginning-of-paragraph)
        (when (eq orig (point))
          (setq erg orig)))
      (when (interactive-p)
        (message "%s" erg))
      erg)))

(defun py-beginning-of-line-p ()
  "Returns position, if cursor is at the beginning of a line, nil otherwise. "
  (interactive)
  (let ((erg (when (bolp)(point))))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a statement, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (eq orig (point))
        (setq erg orig))
      (when (interactive-p)
        (message "%s" erg))
      erg)))

(defun py-beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a expression, nil otherwise. "
  (interactive)
  (let ((orig (point))
        (pps (if (featurep 'xemacs)
                 (parse-partial-sexp (point-min) (point))
               (syntax-ppss)))
        erg)
    (unless (and (setq erg (py-in-string-or-comment-p))
                 (eq erg orig)
                 (not (nth 1 pps)))
      (setq erg nil)
      (unless (or (nth 1 pps)(nth 3 pps)(nth 8 pps)
                  (looking-at ".?[ \t]*\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)"))
        (save-excursion
          (py-end-of-expression)
          (py-beginning-of-expression)
          (when (eq orig (point))
            (setq erg orig)))))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a partial-expression, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (save-excursion
      (py-end-of-partial-expression)
      (py-beginning-of-partial-expression)
      (when (eq orig (point))
        (setq erg orig))
      (when (interactive-p)
        (message "%s" erg))
      erg)))

(defun py-beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a block, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-block-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a clause, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-clause-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a block-or-clause, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-block-or-clause-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a def, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-def-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a class, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-class-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

(defun py-beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a def-or-class, nil otherwise. "
  (interactive)
  (let (erg)
    (when (and (looking-at py-def-or-class-re)
               (not (py-in-string-or-comment-p)))
      (setq erg (point)))
    (when (interactive-p)
      (message "%s" erg))
    erg))

;;; Opens
(defun py-statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (interactive)
  (let* ((regexp (or regexp py-block-re))
         (erg (py-statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (looking-back "^[ \t]*") (<= (line-beginning-position)(point))(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-clause-re))

(defun py-statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-block-or-clause-re))

(defun py-statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-class-re))

(defun py-statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-re))

(defun py-statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-or-class-re))

(defun py-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (unless (py-beginning-of-statement-p) (py-beginning-of-statement))
    (prog1
        (looking-at py-block-closing-keywords-re)
      (goto-char here))))

(defun py-end-base (regexp &optional orig)
  "Used internal by functions going to the end forms. "
  (let ((orig (or orig (point)))
        (erg (if (py-statement-opens-block-p regexp)
                 (point)
               (py-go-to-keyword regexp -1)
               (when (py-statement-opens-block-p regexp)
                 (point))))
        ind)
    (if erg
        (progn
          (setq ind (+ py-indent-offset (current-indentation)))
          (py-end-of-statement)
          (forward-line 1)
          (setq erg (py-travel-current-indent (cons ind (point)))))
      (py-look-downward-for-beginning regexp)
      (unless (eobp)(py-end-base regexp orig)))
    (if (< orig (point))
        (setq erg (point))
      (setq erg (py-look-downward-for-beginning regexp))
      (when erg (py-end-base regexp orig)))
    erg))

(defun py-look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let ((erg (re-search-forward regexp nil (quote move) 1)))
    (if (and erg (not (py-in-string-or-comment-p))
             (not (py-in-list-p)))
        erg
      (unless (eobp)
        (py-look-downward-for-beginning regexp)))))

(defun py-current-defun (&optional iact)
  "Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-beginning-of-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
                   (exchange-point-and-mark)
                   (sit-for py-current-defun-delay)))
        (when iact (message (prin1-to-string erg)))
        erg))))

(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
                (looking-at py-clause-re))
         ;; short circuit infloop on illegal construct
         (not (bobp))
         (progn (forward-line -1)
                (py-beginning-of-statement)
                (back-to-indentation)
                (when (looking-at py-blank-or-comment-re)
                  (backward-to-indentation 1))
                (not (looking-at py-no-outdent-re))))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py-join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py-in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (if (featurep 'xemacs)
                    (parse-partial-sexp lim (point))
                  (syntax-ppss))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defun empty-line-p (&optional iact)
  "Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise."
  (interactive "p")
  (save-excursion
    (let ((erg (progn
                 (beginning-of-line)
                 (looking-at "\\s-*$"))))
      (when iact
        (message "%s" erg))
      erg)))

(defun py-count-lines (&optional start end)
  "Count lines in buffer, optional without given boundaries.
Ignores common region.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (save-restriction
    (widen)
    (let ((beg (cond (start)
                     (t (point-min))))
          (end (cond (end)
                     (t (point))))
          erg)
      (if (featurep 'xemacs)
          (setq erg (count-lines beg end))
        (setq erg (1+ (count-matches "[\n\C-m]" beg end))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-which-function ()
  "Return the name of the function or class, if curser is in, return nil otherwise. "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((orig (point))
            (erg (if (and (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+")) (not (py-in-string-or-comment-p)))
                     (match-string-no-properties 2)
                   (progn
                     (py-beginning-of-def-or-class)
                     (when (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+"))
                       (match-string-no-properties 2))))))
        (if (and erg (< orig (py-end-of-def-or-class)))
            (when (interactive-p) (message "%s" erg))
          (setq erg nil)
          (when (interactive-p) (message "%s" "Not inside a function or class"))
          erg)))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
http://launchpad.net/python-mode
is preferable for that. ")

(provide 'python-components-intern)
;;; python-components-intern.el ends here
