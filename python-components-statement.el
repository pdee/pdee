;;; python-components-statement.el -- Searching downwards in buffer -*- lexical-binding: t; -*-

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

;;; Code:

(defun py-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

ORIG - consider original position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
          (orig (or orig (point)))
          last
          ;; use by scan-lists
          (pps (parse-partial-sexp (point-min) (point)))
          forward-sexp-function pps err)
      ;; (origline (or origline (py-count-lines)))
      (cond
       ;; which-function-mode, lp:1235375
       ((< py-max-specpdl-size repeat)
        (error "forward-statement reached loops max. If no error, customize ‘max-specpdl-size’"))
       ((looking-at (symbol-value (quote py-def-or-class-re)))
        (end-of-line)
        (skip-chars-backward " \t\r\n\f"))
       ;; list
       ((nth 1 pps)
        (if (<= orig (point))
            (progn
              (setq orig (point))
              ;; do not go back at a possible unclosed list
              (goto-char (nth 1 pps))
              (if
                  (ignore-errors (forward-list))
                  (progn
                    (when (looking-at ":[ \t]*$")
                      (forward-char 1))
                    (setq done t)
                    (skip-chars-forward "^#" (line-end-position))
                    (skip-chars-backward " \t\r\n\f" (line-beginning-position))
                    (py-forward-statement orig done repeat))
                (setq err (py--record-list-error pps))
                (goto-char orig)))))
       ;; in comment
       ((and comment-start (looking-at (concat " *" comment-start)))
        (py--end-of-comment-intern (point))
        (py-forward-statement orig))
       ;; (goto-char (match-end 0))
       ;; (py-forward-statement orig done repeat))
       ((nth 4 pps)
        (py--end-of-comment-intern (point))
        (py--skip-to-comment-or-semicolon)
        (while (and (eq (char-before (point)) ?\\)
                    (py-escaped-p) (setq last (point)))
          (forward-line 1) (end-of-line))
        (and last (goto-char last)
             (forward-line 1)
             (back-to-indentation))
        ;; py-forward-statement-test-3JzvVW
        (unless (or (looking-at (concat " *" comment-start))(eolp))
          (py-forward-statement orig done repeat)))
       ;; string
       ((looking-at py-string-delim-re)
        (goto-char (match-end 0))
        (py-forward-statement (match-beginning 0) done repeat))
       ((nth 3 pps)
        (when (py-end-of-string)
          (end-of-line)
          (skip-chars-forward " \t\r\n\f")
          (setq pps (parse-partial-sexp (point-min) (point)))
          (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (py-forward-statement orig done repeat))))
       ((py-current-line-backslashed-p)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f" (line-beginning-position))
        (while (and (eq (char-before (point)) ?\\)
                    (py-escaped-p))
          (forward-line 1)
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
        (unless (eobp)
          (py-forward-statement orig done repeat)))
       ((eq orig (point))
        (if (eolp)
            ;; (skip-chars-forward " \t\r\n\f#'\"")
            (skip-chars-forward " \t\r\n\f")
          (end-of-line)
          (skip-chars-backward " \t\r\n\f" orig))
        ;; point at orig due to a trailing whitespace
        (and (eq (point) orig) (skip-chars-forward " \t\r\n\f"))
        ;; (setq done t)
        (py-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
        ;; ‘py--skip-to-comment-or-semicolon’ may return nil
        (setq done (ignore-errors (< 0 (ignore-errors (abs (py--skip-to-comment-or-semicolon))))))
        (and done (skip-chars-backward " \t\r\n\f"))
        (setq pps (parse-partial-sexp orig (point)))
        (if (nth 1 pps)
            (py-forward-statement orig done repeat)
          (unless done
            (py-forward-statement orig done repeat))))
       ((and (looking-at "[[:print:]]+$") (not done) (py--skip-to-comment-or-semicolon))
        (py-forward-statement orig done repeat)))
      (or err
          (and (< orig (point))
               (not (member (char-before) (list 10 32 9 ?#)))
               (point))))))


(defun py-backward-statement (&optional orig done limit ignore-in-string-p repeat maxindent)
  "Go to the initial line of a simple statement.

Statement is understood in an editorial sense, not syntactically.
Nonetheless, while travelling code, it should match syntactic bounderies too.

For beginning of compound statement use ‘py-backward-block’.
For beginning of clause ‘py-backward-clause’.

‘ignore-in-string-p’ allows moves inside a docstring, used when
computing indents
ORIG - consider original position or point.
DONE - transaktional argument
LIMIT - honor limit
IGNORE-IN-STRING-P - also much inside a string
REPEAT - count and consider repeats
Optional MAXINDENT: do not stop if indentation is larger"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let ((repeat (or (and repeat (1+ repeat)) 0))
            (orig (or orig (point)))
            (pps (parse-partial-sexp (or limit (point-min))(point)))
            (done done))
        ;; lp:1382788
        ;; (unless done
        ;;   (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
        ;;        (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
         ((< py-max-specpdl-size repeat)
          (error "py-forward-statement reached loops max. If no error, customize ‘py-max-specpdl-size’"))
         ((and (bolp) (eolp))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; inside string
         ((and (nth 3 pps) (not ignore-in-string-p))
          (setq done t)
          (goto-char (nth 8 pps))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 4 pps)
          (goto-char (nth 8 pps))
          (skip-chars-backward " \t\r\n\f")
          ;; (setq pps (parse-partial-sexp (line-beginning-position) (point)))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 1 pps)
          (if (and
               (< (nth 1 pps) (line-beginning-position))
               (save-excursion
                 (beginning-of-line)
                 (skip-chars-backward ",] \t\r\n\f")
                 (< (nth 1 pps) (nth 1 (parse-partial-sexp (point-min) (point))))))
              (progn
                (beginning-of-line)
                (skip-chars-backward ",] \t\r\n\f")
                (goto-char (nth 1 (parse-partial-sexp (point-min) (point)))))
            (goto-char (1- (nth 1 pps))))
          (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
            (setq done t))
          ;; (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)
          )
         ((py-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((looking-at comment-start-skip)
          (setq done t)
          (forward-char -1)
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; at raw-string
         ;; (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
         ((and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
          (forward-char -1)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
         ;; at inline comment
         ((looking-at "[ \t]*#")
          (when (py--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
            (setq done t))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; at beginning of string
         ;; ((looking-at py-string-delim-re)
         ;;  (py-end-of-string))
         ((and (not done) (eq (char-before) ?\;))
          (skip-chars-backward ";")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; travel until indentation or semicolon
         ((and (not done) (py--skip-to-semicolon-backward))
          (unless (and maxindent (< maxindent (current-indentation)))
            (setq done t))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ;; at current indent
         ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((and maxindent (< maxindent (current-indentation)))
          (forward-line -1)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((eq orig (point))
          (skip-chars-backward " \t\r\n\f")
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((not (eq (current-indentation) (current-column)))
          (back-to-indentation)
          (py-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
        ;; return nil when before comment
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (when (< (point) orig) (point)))))))

;; python-components-statement.el ends here
(provide 'python-components-statement)
