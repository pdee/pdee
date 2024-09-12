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

(defun py-compute-indentation-according-to-list-style (pps)
  "See ‘py-indent-list-style’

Choices are:

\\='line-up-with-first-element (default)
\\='one-level-to-beginning-of-statement
\\='one-level-from-opener

See also py-closing-list-dedents-bos"
  (goto-char (nth 1 pps))
  (cond
   ((and (looking-back py-assignment-re (line-beginning-position))
         ;; flexible-indentation-lp-328842
         (not (eq (match-beginning 0) (line-beginning-position))))
    (+ (current-indentation) py-indent-offset))
   (py-closing-list-dedents-bos
    (current-indentation))
   (t (pcase py-indent-list-style
        (`line-up-with-first-element
         (if (and (eq (car (syntax-after (point))) 4) (save-excursion (forward-char 1) (eolp)))
             ;; asdf = {
             ;;     'a':{
             ;;          'b':3,
             ;;          'c':4"
             ;;
             ;; b is at col 9
             ;; (+ (current-indentation) py-indent-offset) would yield 8
             ;; EOL-case dedent starts if larger at least 2
             (cond ((< 1 (- (1+ (current-column))(+ (current-indentation) py-indent-offset)))
                   (min (+ (current-indentation) py-indent-offset)(1+ (current-column))))
                   (t (1+ (current-column))))
           (1+ (current-column))))
        (`one-level-to-beginning-of-statement
         (+ (current-indentation) py-indent-offset))
        (`one-level-from-first-element
         (+ 1 (current-column) py-indent-offset))))))

(defun py-compute-indentation-closing-list (pps)
  (cond
   ((< 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    ;; reach the outer list
    (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
    (py--computer-closing-inner-list))
   ;; just close an maybe outer list
   ((eq 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    (py-compute-indentation-according-to-list-style pps))))

(defun py-compute-indentation-in-list (pps line closing orig)
  (if closing
      (py-compute-indentation-closing-list pps)
    (cond ((and (not line) (looking-back py-assignment-re (line-beginning-position)))
	   (py--fetch-indent-statement-above orig))
	  ;; (py-compute-indentation-according-to-list-style pps iact orig origline line nesting repeat indent-offset liep)
	  (t (when (looking-back "[ \t]*\\(\\s(\\)" (line-beginning-position))
	       (goto-char (match-beginning 1))
	       (setq pps (parse-partial-sexp (point-min) (point))))
	     (py-compute-indentation-according-to-list-style pps)))))

(defun py-compute-comment-indentation (pps iact orig origline closing line nesting repeat indent-offset liep)
  (cond ((nth 8 pps)
         (goto-char (nth 8 pps))
         (cond ((and line (eq (current-column) (current-indentation)))
                (current-indentation))
               ((and (eq liep (line-end-position))py-indent-honors-inline-comment)
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

(defun py-compute-indentation--at-closer-maybe (pps)
  (save-excursion
    (when (looking-back "^[ \t]*\\(\\s)\\)" (line-beginning-position))
      (forward-char -1)
      (setq pps (parse-partial-sexp (point-min) (point))))
    (when (and (nth 1 pps)
               (looking-at "[ \t]*\\(\\s)\\)") (nth 0 pps))
      (cond
       ;; no indent at empty argument (list
       ((progn (skip-chars-backward " \t\r\n\f") (ignore-errors (eq 4 (car (syntax-after (1- (point)))))))
        (current-indentation))
       ;; beyond list start?
       ((ignore-errors (< (progn (unless (bobp) (forward-line -1) (line-beginning-position))) (nth 1 (setq pps (parse-partial-sexp (point-min) (point))))))
        (py-compute-indentation-according-to-list-style pps))
       (py-closing-list-dedents-bos
        (- (current-indentation) py-indent-offset))
       (t (current-indentation))))))

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
  (let ((beg
         (or beg
             (and (comint-check-proc (current-buffer))
                  (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1))
             (point-min))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg (line-end-position))
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
               (closing
                (or closing
                    ;; returns update pps
                    ;; (and line (py-compute-indentation--at-closer-maybe pps))
                    (py-compute-indentation--at-closer-maybe pps)))
               ;; in a recursive call already
               (repeat (if repeat
                           (setq repeat (1+ repeat))
                         0))
               ;; nesting: started nesting a list
               (nesting nesting)
               indent this-line)
          (if (< py-max-specpdl-size repeat)
              (error "‘py-compute-indentation’ reached loops max.")
            (setq nesting (nth 0 pps))
            (setq indent
                  (cond ;; closing)
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
			  (skip-chars-backward " \t\r\n\f")
			  (back-to-indentation)
			  (current-indentation))))
                     ;; string in list
                     ((save-excursion (goto-char (nth 8 pps))(nth 0 (parse-partial-sexp (point-min) (point))))
                      (if
                          (or line (save-excursion (goto-char (nth 8 pps))(< (py-count-lines (point-min) (point)) origline)))
                          (progn
                            (goto-char (nth 8 pps)) (current-column))
                        (goto-char (nth 8 pps))
                        (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
                     ((or line (< (py-count-lines (point-min) (point)) origline))
                      (goto-char (nth 8 pps))(current-indentation))
		     (t 0)))
		   ((and (looking-at "\"\"\"\\|'''") (not (bobp)))
		    (py-backward-statement)
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ;; comments
		   ((or
		     (nth 8 pps)
		     (and
		      (looking-at (concat "[ \t]*" comment-start))
		      (looking-back "^[ \t]*" (line-beginning-position))(not line))
		     (and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment))
		    (py-compute-comment-indentation pps iact orig origline closing line nesting repeat indent-offset liep))
		   ;; lists
		   ((nth 1 pps)
		    (if (< (nth 1 pps) (line-beginning-position))
                        ;; Compute according to ‘py-indent-list-style’

                        ;; Choices are:

                        ;; \\='line-up-with-first-element (default)
                        ;; \\='one-level-to-beginning-of-statement
                        ;; \\='one-level-from-opener"

                        ;; See also py-closing-list-dedents-bos
			(py-compute-indentation-in-list pps line closing orig)
		      (back-to-indentation)
		      (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
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
		   ;; ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
		   ;; (when (py--line-backward-maybe) (setq line t))
		   ;; (car (py--clause-lookup-keyword py-elif-re -1 nil origline)))
		   ((and (looking-at py-minor-clause-re) (not line)
                         (eq liep (line-end-position)))
		    (cond
                     ((looking-at py-case-re)
                      (and (py--backward-regexp (quote py-match-case-re) nil '>)
                                           (+ (current-indentation) py-indent-offset)
                                           ))
                     ((looking-at py-minor-clause-re)
		      (and (py--backward-regexp (quote py-block-or-clause-re)
                      ;; an arbitray number, larger than an real expected indent
                      (* 99 py-indent-offset)
                      '<)
                           (current-indentation)))

                     ((looking-at py-outdent-re)
		      (and (py--backward-regexp (quote py-block-or-clause-re)
                      ;; an arbitray number, larger than an real expected indent
                      (* 99 py-indent-offset)
                      '<)))
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
			   (when (py--line-backward-maybe) (setq line t))
			   (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
                          (t (+
			      (cond (indent-offset)
				    (py-smart-indentation
				     (py-guess-indent-offset))
				    (t py-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
		     (eq (current-column) (current-indentation)))
		    (and
		     (looking-at py-assignment-re)
		     (goto-char (match-end 0)))
		    ;; multiline-assignment
		    (if (and nesting (looking-at " *[[{(]") (not (looking-at ".+[]})][ \t]*$")))
			(+ (current-indentation) (or indent-offset py-indent-offset))
		      (current-indentation)))
		   ((looking-at py-assignment-re)
		    (py-backward-statement)
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (parse-partial-sexp (point-min) (point)))))
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   ((and (not (py--beginning-of-statement-p)) (not (and line (eq 11 (syntax-after (point))))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
                          (progn
			    (when (py--line-backward-maybe) (setq line t))
			    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
			(py-backward-statement)
			(py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))))
		   ((or (py--statement-opens-block-p py-extended-block-or-clause-re) (looking-at "@"))
		    (if (< (py-count-lines) origline)
			(+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg)))
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
		    (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep beg))
		   (t (current-indentation))))
            ;; (when (or (eq 1 (prefix-numeric-value iact)) py-verbose-p) (message "%s" indent))
            (when py-verbose-p (message "%s" indent))
            indent))))))

(provide 'python-components-compute-indentation)
 ;;; python-components-compute-indentation.el ends here
