;;; python-components-start-Zf98zM.el -- Searching downwards in buffer -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/groups/python-mode-devs
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
    (and (eq (char-after) ?\;)(forward-char 1))
    (and (< orig (point))(point))))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list  (string-to-char comment-start) 10))(forward-line 1)(back-to-indentation))))

(defun py-backward-statement-bol ()
  "Goto beginning of line where statement start.
Returns position reached, if successful, nil otherwise.

See also ‘py-up-statement’"
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
    (if (or (looking-at comment-start)(py--beginning-of-statement-p))
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

(defun py--backward-regexp (regexp &optional indent condition orig)
  "Search backward next regexp not in string or comment.

Return position if successful
REGEXP: the expression to search for
SECONDVALUE: travel these expressions
"
  (unless (bobp)
    (save-match-data
      (unless (py--beginning-of-statement-p) (skip-chars-backward " \t\r\n\f")
              (py-backward-comment))
      (let* (pps
             (regexpvalue (symbol-value regexp))
             (secondvalue (pcase regexp
                            (py-def-re py-block-re)
                            ;; (unless (member regexp (list 'py-def-re 'py-class-re))
                            ;; (or secondvalue (symbol-value regexp))))
                            ))
             (indent (or indent (current-indentation)))
             (condition (or condition '<=))
             (orig (or orig (point))))
        (if (eq (current-indentation) (current-column))
            (while (and (not (bobp))
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

                        ;; When looking for beginning-of-def from EOB,
                        ;; make sure, the further indented ‘def
                        ;; baz():’ in the middle isn't matched, but
                        ;; BOB. Therefor the ‘secondvalue’, which may
                        ;; correct the required indent
                        (re-search-backward (concat "^ \\{0,"(format "%s" indent) "\\}\\(" regexpvalue "\\|" secondvalue "\\)") nil 'move 1)
                        (goto-char (match-beginning 1))
                        (not (and (looking-back "async *" (line-beginning-position))
                                  (goto-char (match-beginning 0))))
                        (or (and
                             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
                             (goto-char pps))
                            (and (not (eq (current-column) 0))
                                 (not (looking-at regexpvalue))
                                 (looking-at secondvalue)
                                 indent
                                 ))
                        (prog1 t
                          (cond ((< (current-indentation) indent)
                                 (setq indent (current-indentation)))
                                ((and (not (looking-at regexpvalue))
                                      (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)) )
                                 (setq indent (- (current-indentation) py-indent-offset)))))
                        ))
          (unless (bobp)
            (back-to-indentation)
            (and
             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
             (goto-char pps))
            ;; (unless (and (< (point) orig) (not (looking-at regexpvalue)) (looking-at secondvalue))
            (unless (and (< (point) orig) (or (looking-at regexpvalue) (and secondvalue (looking-at secondvalue))))
              (py--backward-regexp regexp (current-indentation) condition orig))
            (unless (or (eq (point) orig)(bobp)) (back-to-indentation))))
        (and (looking-at regexpvalue) (not (nth 8 (parse-partial-sexp (point-min) (point))))(point))))))

(defun py--go-to-keyword (regexp &optional condition maxindent ignoreindent)
  "Expects being called from beginning of a statement.

Argument REGEXP: a symbol.

Return position if successful, nil otherwise

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

           ;; (allvalue (symbol-value (quote py-block-or-clause-re)))
           )
      (unless (py--beginning-of-statement-p)
        (py-backward-statement))
      (when (and (not (string= "" py-block-closing-keywords-re))(looking-at py-block-closing-keywords-re))
        (setq maxindent (min maxindent (- (current-indentation) py-indent-offset))))
      (cond
       ((and (looking-at regexpvalue)(< (point) orig))
        (point))
       (t (while
              (not (or (bobp) (and (looking-at regexpvalue)(< (point) orig) (not (nth 8 (parse-partial-sexp (point-min) (point)))))))
            ;; search backward and reduce maxindent, if non-matching forms suggest it
            (py--backward-regexp regexp maxindent
                                 (or condition '<=)
                                 orig))))
      (and (< (point) orig)(looking-at regexpvalue)(point)))))

(defun py-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like `(quote py-clause-re)'

Return position if successful"
  (unless (py--beginning-of-statement-p) (py-backward-statement))
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
                             (`py-try-re (concat py-finally-re "\\|" py-except-re "\\|" py-else-re))
                             (`py-if-re py-else-re))))
           last)
      (if (eq regexp (quote py-clause-re))
          (py-forward-clause-intern indent)
        (while
            (and
             (not done)
             (progn (end-of-line)
                    (pcase indent
                      (0
                       (cond (use-regexp
                              (re-search-forward (concat "^" regexpvalue) nil 'move 1))
                             (t (re-search-forward "^[[:alnum:]_@]+" nil 'move 1))))
                      (_
                       (cond (use-regexp
                              (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
                             (t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_@]+") nil 'move 1))))))
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
        (unless (py--beginning-of-statement-p nil bol)
          (py-backward-statement))
        (let* (;; when at block-start, be specific
               ;; (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
               ;; (regexp (or regexp (symbol-value (quote py-extended-block-or-clause-re))))
               (repeat (if repeat (1+ repeat) 0))
               (indent (current-indentation))
               (secondvalue (py--end-base-determine-secondvalue regexp))
               ;; when at block-start, be specific
               ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
               (res
                (cond
                 ((and ;; (py--beginning-of-statement-p)
                       ;; (eq 0 (current-column))
                       (or (looking-at (concat "[ \\ŧ]*" regexpvalue))
                           (and (member regexp (list (quote py-def-re) (quote py-def-or-class-re) (quote py-class-re)))
                                (looking-at py-decorator-re)
                                (py-down-def-or-class indent))
                           (and (member regexp (list (quote py-minor-block-re) (quote py-if-re) (quote py-for-re) (quote py-try-re)))
                                (looking-at py-minor-clause-re))))
                  (list (current-indentation) (point) secondvalue))
                 ((looking-at regexpvalue)
                  (list (current-indentation) (point) secondvalue))
                 ((eq 0 (current-indentation))
                  (py--down-according-to-indent regexp nil 0 use-regexp))
                 ;; look upward
                 (t (py--go-to-keyword regexp (if (member regexp (list (quote py-def-re) (quote py-class-re) (quote py-def-or-class-re))) '< '<=))))))
          (cond
           (res
            (and
             (py--down-according-to-indent regexp secondvalue (current-indentation))
             (progn
               (when (and secondvalue (looking-at secondvalue))
                 ;; (when (looking-at py-else-re)
                 (py--down-according-to-indent regexp secondvalue (current-indentation)))
               (if (>= indent (current-indentation))
                   (py--down-end-form)
                 (end-of-line)
                 (skip-chars-backward " \t\r\n\f")))
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
