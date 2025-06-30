;; python-components-compute-indentation.el --- Part of python-components-mode -*- lexical-binding: t; -*-

;; Helper functions

;; URL: https://gitlab.com/python-mode-devs

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

;;  Keymap

;;  Utility stuff

(defun py--computer-closing-inner-list ()
  "Compute indentation according to py-closing-list-dedents-bos."
  (if py-closing-list-dedents-bos
      (+ (current-indentation) py-indent-offset)
    (1+ (current-column))))

(defun py-compute-indentation-according-to-list-style-intern()
  (pcase py-indent-list-style
    (`line-up-with-first-element
     (if (looking-at "\\s([ \\t]*$")
         (cond ((save-excursion
                  (back-to-indentation)
                  (looking-at py-if-re))
                ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42513
                0)
               ((looking-back "^[ \\t]+ " (line-beginning-position))
                ;; line with 'sword', from a single opener
                ;; var5: Sequence[Mapping[str, Sequence[str]]] = [
                ;;     {
                ;;      'red': ['scarlet', 'vermilion', 'ruby'],
                ;;      'green': ['emerald', 'aqua']
                ;;     },
                ;;     {
                ;;                 'sword': ['cutlass', 'rapier']
                ;;     }
                ;; ]
                (+ (current-indentation) 1))
               (t
                (+ (current-indentation) py-indent-offset)))
       (+ (current-column) 1)))
    (`one-level-to-beginning-of-statement
     (+ (current-indentation) py-indent-offset))
    (`one-level-from-first-element
     (+ 1 (current-column) py-indent-offset))))

(defun py-compute-indentation-according-to-list-style (pps line-beginning-position)
  "See ‘py-indent-list-style’

Choices are:

\\='line-up-with-first-element (default)
\\='one-level-to-beginning-of-statement
\\='one-level-from-opener

See also py-closing-list-dedents-bos"
  (let ((orig (point))
        (lines (py-count-lines))
        (just-at-closer (save-excursion
                          (or (looking-back "^[ \t]*\\s)" (line-beginning-position))
                              (and (looking-at "[ \t]*\\s)")
                                   (looking-back "^[ \t]*" (line-beginning-position))))))
        (lines-from
         (progn (goto-char (nth 1 pps))
                (py-count-lines))))
    ;; now at start of inner list
    (cond
     ((save-excursion
        ;; from last 'pk': chained lists as special case
        ;; data = {'key': {
        ;;     'objlist': [
        ;;         {'pk': 1,
        ;;          'name': 'first'},
        ;;         {'pk': 2,
        ;;          'name': 'second'}
        ;;     ]
        ;; }}
        (and
         ;; list starts at current line
         (< line-beginning-position (nth 1 pps))
         ;; if previous line contains in another list, indent according to its start
         (progn
           (beginning-of-line)
           (skip-chars-backward " \t\r\n\f")
           (skip-chars-backward "^[[:alnum:]]")
           (eq (nth 0 pps) (nth 0 (parse-partial-sexp (point-min) (point)))))))
      (beginning-of-line)
      (skip-chars-backward " \t\r\n\f")
      (skip-chars-backward "^[[:alnum:]]")
      (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
      (current-indentation))
     (just-at-closer
      ;; dedents at opener or at openers indentation
      (cond ((or (eq line-beginning-position (line-beginning-position)) py-closing-list-dedents-bos)
             (current-indentation))
            (t (py-compute-indentation-according-to-list-style-intern))))
     ((save-excursion
        (and
         (not just-at-closer)
         (< 1 (- lines lines-from))
         (progn
           (goto-char orig)
           (forward-line -1)
           ;; ignore if in higher nesting
           (eq (nth 0 pps) (nth 0 (parse-partial-sexp (point-min) (point)))))))
      (progn
        (goto-char orig)
        (forward-line -1)
        (current-indentation)))
     ((eq line-beginning-position (line-beginning-position))
      ;; (and (eq line-beginning-position (line-beginning-position))  (looking-back [^ \\t]*))
      (current-indentation))
     ((eq (current-column) 0)
      ;; List starts at BOL or indent,
      ;; https://bugs.launchpad.net/python-mode/+bug/328842
      (+ (current-indentation) py-indent-offset))
     (t (py-compute-indentation-according-to-list-style-intern)))))

(defun py-compute-comment-indentation (pps iact orig origline closing line nesting repeat indent-offset liep)
  (cond ((nth 8 pps)
         (goto-char (nth 8 pps))
         (cond ((and line (eq (current-column) (current-indentation)))
                (current-indentation))
               ((and (eq liep (line-end-position)) py-indent-honors-inline-comment)
                (current-column))
               ((py--line-backward-maybe)
                (setq line t)
                (skip-chars-backward " \t")
                (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
               (t (if py-indent-comments
                      (progn
                        (py-backward-comment)
                        (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
                    0))))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))(not line)
          (eq liep (line-end-position)))
         (if py-indent-comments
             (progn
               (setq line t)
               (skip-chars-backward " \t\r\n\f")
               ;; as previous comment-line might
               ;; be wrongly unindented, travel
               ;; whole commented section
               (py-backward-comment)
               (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
           0))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))
          (not (eq liep (line-end-position))))
         (current-indentation))
        ((and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment)
         (current-column))))

(defun py-compute-indentation--at-closer-maybe (erg)
  (goto-char erg)
  (backward-sexp)
  (if py-closing-list-dedents-bos
      (current-indentation)
    (+ (current-indentation) py-indent-offset)))

(defun py-compute-indentation--at-closer-p ()
  "If on a line on with just on or more chars closing a list."
  ;; (interactive)
  (or
   (and (looking-back "^[ \\t]*[\])}]+[ \\t]*" (line-beginning-position))(match-end 0))
   (and (looking-back "^ *" (line-beginning-position))
        (looking-at "[ \\t]*[\]})]+[ \\t]*$")
        (match-end 0)
        )))

(defun py--compute-indentation-in-docstring ()
  ""
  (save-excursion
    ;; (goto-char (match-beginning 0))
    (back-to-indentation)
    (if (looking-at "[uUrR]?\"\"\"\\|[uUrR]?'''")
        (progn
          (skip-chars-backward " \t\r\n\f")
          (back-to-indentation)
          (if (looking-at py-def-or-class-re)
              (+ (current-column) py-indent-offset)
            (current-indentation)))
      (beginning-of-line) 
      (skip-chars-backward " \t\r\n\f")
      (back-to-indentation)
      (current-indentation))))

(defun py-compute-indentation (&optional iact orig origline closing line nesting repeat indent-offset liep beg)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as ‘return’,
‘raise’, ‘break’, ‘continue’, and ‘pass’ force one level of dedenting.

ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING is currently ignored, if executing from inside a list
REPEAT counter enables checks against ‘py-max-specpdl-size’
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive "p")
  ;; (and (not line) (< (current-column) (current-indentation)) (back-to-indentation))
  (let ((beg
         (or beg
             (and (comint-check-proc (current-buffer))
                  (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1))
             (point-min))))
    (save-excursion
      (save-restriction
        ;; (narrow-to-region beg (line-end-position))
        ;; in shell, narrow from previous prompt
        ;; needed by closing
        (let* ((orig (or orig (copy-marker (point))))
               (origline (or origline (py-count-lines (point-min) (point))))
               ;; closing indicates: when started, looked
               ;; at a single closing parenthesis
               ;; line: moved already a line backward
               (liep (or liep (line-end-position)))
               (line (or line (not (eq origline (py-count-lines (point-min) (point))))))
               ;; (line line)
               (pps (progn
                      (unless (eq (current-indentation) (current-column))(skip-chars-backward " " (line-beginning-position)))
                      ;; (when (eq 5 (car (syntax-after (1- (point)))))
                      ;;   (forward-char -1))
                      (parse-partial-sexp (point-min) (point))))

               ;; in a recursive call already
               (repeat (or repeat 0))
               ;; nesting: started nesting a list
               (nesting nesting)
               indent this-line)
          (if (< py-max-specpdl-size repeat)
              (error "‘py-compute-indentation’ reached loops max.")
            (setq nesting (nth 0 pps))
            (setq indent
                  (cond
                   ((bobp)
                    (cond ((eq liep (line-end-position))
                           0)
                          ;; - ((looking-at py-outdent-re)
                          ;; - (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                          ((and line (looking-at py-block-or-clause-re))
                           py-indent-offset)
                          ((looking-at py-outdent-re)
                           (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                          (t
                           (current-indentation))))
                   ;; in string
                   ((and (nth 3 pps) (nth 8 pps))
                    (cond
                     ((py--docstring-p (nth 8 pps))
                      (py--compute-indentation-in-docstring))
                     ;; string in list
                     ;; ;; data = {'key': {
                     ;;     'objlist': [
                     ;;         {'pk': 1,
                     ;;          'name': 'first'},
                     ;;         {'pk': 2,
                     ;;          'name': 'second'}
                     ;;     ]
                     ;; }}
                     (t (goto-char (nth 8 pps))
                      (if
                          (or line (< (py-count-lines (point-min) (point)) origline))
                          (current-column)
                        (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg)))))
                   ((and (looking-at "\"\"\"\\|'''") (not (bobp)))
                    (py-backward-statement)
                    (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))
                   ;; comments
                   ((or
                     (nth 8 pps)
                     (and
                      (looking-at (concat "[ \t]*" comment-start))
                      (looking-back "^[ \t]*" (line-beginning-position))(not line))
                     (and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment))
                    (py-compute-comment-indentation pps iact orig origline closing line nesting (+ repeat 1) indent-offset liep))
                   ;; lists
                   ((nth 1 pps)
                    (py-compute-indentation-according-to-list-style pps (line-beginning-position)))
                   ;; Compute according to ‘py-indent-list-style’

                   ;; Choices are:

                   ;; \\='line-up-with-first-element (default)
                   ;; \\='one-level-to-beginning-of-statement
                   ;; \\='one-level-from-opener"

                   ;; See also py-closing-list-dedents-bos
                   ;;   (py-compute-indentation-in-list pps line closing orig)
                   ;; (back-to-indentation)
                   ;; (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg)))
                   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
                    (1+ (current-column)))
                   ((py-preceding-line-backslashed-p)
                    (progn
                      (py-backward-statement)
                      (setq this-line (py-count-lines))
                      (if (< 1 (- origline this-line))
                          (py--fetch-indent-line-above orig)
                        (if (looking-at "from +\\([^ \t\n]+\\) +import")
                            py-backslashed-lines-indent-offset
                          (if (< 20 (line-end-position))
                              8
                            (+ (current-indentation) py-continuation-offset))))))
                   ((and (looking-at py-block-closing-keywords-re)
                         (eq liep (line-end-position)))
                    (skip-chars-backward "[ \t\r\n\f]")
                    (py-backward-statement)
                    (cond ((looking-at py-extended-block-or-clause-re)
                           (+
                            ;; (if py-smart-indentation (py-guess-indent-offset) indent-offset)
                            (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset)
                            (current-indentation)))
                          ((looking-at py-block-closing-keywords-re)
                           (- (current-indentation) (or indent-offset py-indent-offset)))
                          (t (current-column))))
                   ((looking-at py-block-closing-keywords-re)
                    (if (< (line-end-position) orig)
                        ;; #80, Lines after return cannot be correctly indented
                        (if (looking-at "return[ \\t]*$")
                            (current-indentation)
                          (- (current-indentation) (or indent-offset py-indent-offset)))
                      (py-backward-block-or-clause)
                      (current-indentation)))
                   ((and (looking-at py-minor-clause-re) (not line)
                         (eq liep (line-end-position)))
                    (cond
                     ((looking-at py-case-re)
                      (and (py--backward-regexp (quote py-match-case-re) nil (quote >))
                           ;; (+ (current-indentation) py-indent-offset)
                           (current-indentation)))
                     ((and (py--backward-regexp (quote py-block-or-clause-re)
                                                ;; an arbitray large
                                                ;; number, larger than
                                                ;; any real expected
                                                ;; indent
                                                (* 99 py-indent-offset)
                                                (quote <))
                           (current-indentation)))
                     ((looking-at py-outdent-re)
                      (and (py--backward-regexp (quote py-block-or-clause-re)
                                                ;; an arbitray number, larger than an real expected indent
                                                (* 99 py-indent-offset)
                                                (quote <))))
                     ((bobp) 0)
                     (t (save-excursion
                          ;; (skip-chars-backward " \t\r\n\f")
                          (if (py-backward-block)
                              ;; (py--backward-regexp (quote py-block-or-clause-re))
                              (+ py-indent-offset (current-indentation))
                            0)))))
                   ((looking-at py-extended-block-or-clause-re)
                    (cond ((and (not line)
                                (eq liep (line-end-position)))
                           (when (py--line-backward-maybe)
                             (py-compute-indentation iact orig origline closing t nesting (+ repeat 1) indent-offset liep beg)))
                          (t (+
                              (cond (indent-offset)
                                    (py-smart-indentation
                                     (py-guess-indent-offset))
                                    (t py-indent-offset))
                              (current-indentation)))))
                   ((and
                     (< (line-end-position) liep)
                     (eq (current-column) (current-indentation)))
                    ;; from beginning of previous line
                    (cond
                     ((looking-at py-assignment-re)
                      (goto-char (match-end 0))
                      ;; multiline-assignment
                      (if (and nesting (looking-at " *[[{(]") (not (looking-at ".+[]})][ \t]*$")))
                          (+ (current-indentation) (or indent-offset py-indent-offset))
                        (current-indentation)))
                     ((looking-at py-assignment-re)
                      (py-backward-statement)
                      (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))
                     ((looking-at py-block-or-clause-re)
                      (+ (current-indentation) py-indent-offset))
                     (t (current-indentation))))
                   ((and (< (current-indentation) (current-column))(not line))
                    (back-to-indentation)
                    (unless line
                      (setq nesting (nth 0 (parse-partial-sexp (point-min) (point)))))
                    (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))
                   ((and (not (py--beginning-of-statement-p)) (not (and line (eq 11 (syntax-after (point))))))
                    (if (bobp)
                        (current-column)
                      (if (eq (point) orig)
                          (progn
                            (when (py--line-backward-maybe) (setq line t))
                            (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))
                        (py-backward-statement)
                        (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))))
                   ((or (py--statement-opens-block-p py-extended-block-or-clause-re) (looking-at "@"))
                    (if (< (py-count-lines) origline)
                        (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation))
                      (skip-chars-backward " \t\r\n\f")
                      (setq line t)
                      (back-to-indentation)
                      (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg)))
                   ((and py-empty-line-closes-p (py--after-empty-line))
                    (progn (py-backward-statement)
                           (- (current-indentation) (or indent-offset py-indent-offset))))
                   ;; still at original line
                   ((and (eq liep (line-end-position))
                         (save-excursion
                           (and
                            (py--go-to-keyword (quote py-extended-block-or-clause-re) nil (* py-indent-offset 99))
                            (if (looking-at (concat py-block-re "\\|" py-outdent-re))
                                (+ (current-indentation)
                                   (if py-smart-indentation
                                       (or indent-offset (py-guess-indent-offset))
                                     (or indent-offset py-indent-offset)))
                              (current-indentation))))))
                   ((and (not line)
                         (eq liep (line-end-position))
                         (py--beginning-of-statement-p))
                    (py-backward-statement)
                    (py-compute-indentation iact orig origline closing line nesting (+ repeat 1) indent-offset liep beg))
                   (t (current-indentation))))
            ;; (when (or (eq 1 (prefix-numeric-value iact)) py-verbose-p) (message "%s" indent))
            (when (or iact py-verbose-p) (message "%s" indent))
            indent))))))

(provide (quote python-components-compute-indentation))
;;; python-components-compute-indentation.el ends here
