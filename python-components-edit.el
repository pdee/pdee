;;; python-components-edit.el --- Some more Python edit utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016, 2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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

(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python."
  (interactive "*")
  (let* ((erg (if py-edit-only-p
                  py-shell-name
                (executable-find py-shell-name)))
         (sheb (concat "#! " erg)))
    (insert sheb)))

(defun py--beginning-of-expression-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘expression’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at (concat "\\b" py-expression-re))
         (point))))

;; TODO: The following function can lead to false positive (inside a string).
;; See ticket #95: https://gitlab.com/python-mode-devs/python-mode/-/issues/95
(defun py--top-level-form-p ()
  "Return non-nil, if line start with a top level definition.

Used by `py-electric-colon', which will not indent then."
  (save-excursion
    (beginning-of-line)
    (or (looking-at py-class-re)
        (looking-at py-def-re))))

(defun py-indent-line-outmost (&optional arg)
  "Indent the current line to the outmost reasonable indent.

With optional \\[universal-argument] ARG, unconditionally insert an indent of
`py-indent-offset' length."
  (interactive "*P")
  (cond
   ((eq 4 (prefix-numeric-value arg))
    (if indent-tabs-mode
        (insert (make-string 1 9))
      (insert (make-string py-indent-offset 32))))
   ;;
   (t
    (let* ((need (py-compute-indentation (point)))
           (cui (current-indentation))
           (cuc (current-column)))
      (if (and (eq need cui)
               (not (eq cuc cui)))
          (back-to-indentation)
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to need))))))


(defun py--re-indent-line ()
  "Re-indent the current line."
  (beginning-of-line)
  (delete-region (point)
                 (progn (skip-chars-forward " \t\r\n\f")
                        (point)))
  (indent-to (py-compute-indentation)))

;; TODO: the following function can fall into an infinite loop.
;; See https://gitlab.com/python-mode-devs/python-mode/-/issues/99
(defun py--indent-fix-region-intern (beg end)
  "Used when `py-tab-indents-region-p' is non-nil.

Requires BEG, END as the boundery of region"
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((end (copy-marker end)))
	(forward-line 1)
	(narrow-to-region (line-beginning-position) end)
	(py--re-indent-line)
	(while (< (line-end-position) end)
          (forward-line 1)
          (py--re-indent-line))))))

(defun py-indent-current-line (need)
  "Indent current line to NEED."
  (beginning-of-line)
  (delete-horizontal-space)
  (indent-to need))

;; TODO: Add docstring.
;; What is the intent of the this utility function?
;; What is the purpose of each argument?
(defun py--indent-line-intern (need cui indent col &optional beg end region)
  (let (erg)
    (if py-tab-indent
	(progn
	  (and py-tab-indents-region-p region
	       (py--indent-fix-region-intern beg end))
	  (cond
	   ((bolp)
	    (if (and py-tab-shifts-region-p region)
                (while (< (current-indentation) need)
                  (py-shift-region-right 1))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)))
           ;;
	   ((< need cui)
	    (if (and py-tab-shifts-region-p region)
		(progn
		  (when (eq (point) (region-end))
		    (exchange-point-and-mark))
		  (while (< 0 (current-indentation))
		    (py-shift-region-left 1)))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)))
           ;;
	   ((eq need cui)
	    (if (or (eq this-command last-command)
		    (eq this-command 'py-indent-line))
		(if (and py-tab-shifts-region-p region)
		    (while (and (goto-char beg) (< 0 (current-indentation)))
		      (py-shift-region-left 1))
		  (beginning-of-line)
		  (delete-horizontal-space)
		  (if (<= (line-beginning-position) (+ (point) (- col cui)))
		      (forward-char (- col cui))
		    (beginning-of-line)))))
           ;;
	   ((< cui need)
	    (if (and py-tab-shifts-region-p region)
                (py-shift-region-right 1)
              (beginning-of-line)
              (delete-horizontal-space)
              ;; indent one indent only if goal < need
              (setq erg (+ (* (/ cui indent) indent) indent))
              (if (< need erg)
                  (indent-to need)
                (indent-to erg))
              (forward-char (- col cui))))
           ;;
	   (t
	    (if (and py-tab-shifts-region-p region)
                (while (< (current-indentation) need)
                  (py-shift-region-right 1))
	      (beginning-of-line)
	      (delete-horizontal-space)
	      (indent-to need)
	      (back-to-indentation)
	      (if (<= (line-beginning-position) (+ (point) (- col cui)))
		  (forward-char (- col cui))
		(beginning-of-line))))))
      (insert-tab))))

(defun py--indent-line-or-region-base (beg end region cui need arg this-indent-offset col)
  (cond ((eq 4 (prefix-numeric-value arg))
	 (if (and (eq cui (current-indentation))
		  (<= need cui))
	     (if indent-tabs-mode (insert "\t")(insert (make-string py-indent-offset 32)))
	   (beginning-of-line)
	   (delete-horizontal-space)
	   (indent-to (+ need py-indent-offset))))
	((not (eq 1 (prefix-numeric-value arg)))
	 (py-smart-indentation-off)
	 (py--indent-line-intern need cui this-indent-offset col beg end region))
	(t (py--indent-line-intern need cui this-indent-offset col beg end region))))

(defun py--calculate-indent-backwards (cui indent-offset)
  "Return the next reasonable indent lower than current indentation.

Requires current indent as CUI
Requires current indent-offset as INDENT-OFFSET"
  (if (< 0 (% cui py-indent-offset))
      ;; not correctly indented at all
      (/ cui indent-offset)
    (- cui indent-offset)))

(defun py-indent-line (&optional arg outmost-only)
  "Indent the current line according ARG.

When called interactivly with \\[universal-argument],
ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

An optional \\[universal-argument] followed by a numeric argument
neither 1 nor 4 will switch off `py-smart-indentation' for this execution.
This permits to correct allowed but unwanted indents. Similar to
`toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].

When bound to TAB, \\[quoted-insert] TAB inserts a TAB.

OUTMOST-ONLY stops circling possible indent.

When `py-tab-shifts-region-p' is t, not just the current line,
but the region is shiftet that way.

If `py-tab-indents-region-p' is t and first TAB doesn't shift
--as indent is at outmost reasonable--, ‘indent-region’ is called.

\\[quoted-insert] TAB inserts a literal TAB-character."
  (interactive "P")
  (unless (eq this-command last-command)
    (setq py-already-guessed-indent-offset nil))
  (let ((orig (copy-marker (point)))
	;; TAB-leaves-point-in-the-wrong-lp-1178453-test
	(region (use-region-p))
        cui
	outmost
	col
	beg
	end
	need
	this-indent-offset)
    (and region
	 (setq beg (region-beginning))
	 (setq end (region-end))
	 (goto-char beg))
    (setq cui (current-indentation))
    (setq col (current-column))
    (setq this-indent-offset
	  (cond ((and py-smart-indentation (not (eq this-command last-command)))
		 (py-guess-indent-offset))
		((and py-smart-indentation (eq this-command last-command) py-already-guessed-indent-offset)
		 py-already-guessed-indent-offset)
		(t (default-value 'py-indent-offset))))
    (setq outmost (py-compute-indentation nil nil nil nil nil nil nil this-indent-offset))
    ;; now choose the indent
    (setq need
	  (cond ((eq this-command last-command)
		 (if (eq cui outmost)
		     (when (not outmost-only)
		       (py--calculate-indent-backwards cui this-indent-offset)))
		 (if (bolp)
		     (py-compute-indentation orig)
		   (py--calculate-indent-backwards cui this-indent-offset)))
		(t
		 outmost
		 ;; (py-compute-indentation orig)
		 )))
    (when (and (called-interactively-p 'any) py-verbose-p) (message "py-indent-line, need: %s" need))
    ;; if at outmost
    ;; and not (eq this-command last-command), need remains nil
    (when need
      (py--indent-line-or-region-base beg end region cui need arg this-indent-offset col)
      (and region (or py-tab-shifts-region-p
		      py-tab-indents-region-p)
	   (not (eq (point) orig))
	   (exchange-point-and-mark))
      (when (and (called-interactively-p 'any) py-verbose-p)(message "%s" (current-indentation)))
      (current-indentation))))

(defun py--delete-trailing-whitespace (orig)
  "Delete trailing whitespace.

Either `py-newline-delete-trailing-whitespace-p'
or `
py-trailing-whitespace-smart-delete-p' must be t.

Start from position ORIG"
  (when (or py-newline-delete-trailing-whitespace-p py-trailing-whitespace-smart-delete-p)
    (let ((pos (copy-marker (point))))
      (save-excursion
	(goto-char orig)
	(if (py-empty-line-p)
	    (if (py---emacs-version-greater-23)
		(delete-trailing-whitespace (line-beginning-position) pos)
	      (save-restriction
		(narrow-to-region (line-beginning-position) pos)
		(delete-trailing-whitespace)))
	  (skip-chars-backward " \t")
	  (if (py---emacs-version-greater-23)
	      (delete-trailing-whitespace (line-beginning-position) pos)
	    (save-restriction
	      (narrow-to-region (point) pos)
	      (delete-trailing-whitespace))))))))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines."
  (interactive "*")
  (let* ((orig (point))
	 ;; lp:1280982, deliberatly dedented by user
	 (this-dedent
	  (when (and (or (eq 10 (char-after))(eobp))(looking-back "^[ \t]*" (line-beginning-position)))
	    (current-column)))
	 erg)
    (newline 1)
    (py--delete-trailing-whitespace orig)
    (setq erg
	  (cond (this-dedent
		 (indent-to-column this-dedent))
		((and py-empty-line-closes-p (or (eq this-command last-command)(py--after-empty-line)))
		 (indent-to-column (save-excursion (py-backward-statement)(- (current-indentation) py-indent-offset))))
		(t
		 (fixup-whitespace)
		 (indent-to-column (py-compute-indentation)))))
    (when (and (called-interactively-p 'any) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-newline-and-close-block 'py-newline-and-dedent)
(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current.
Returns column."
  (interactive "*")
  (let ((cui (current-indentation))
        erg)
    (newline 1)
    (when (< 0 cui)
      (setq erg (- (py-compute-indentation) py-indent-offset))
      (indent-to-column erg))
    (when (and (called-interactively-p 'any) py-verbose-p) (message "%s" erg))
    erg))

(defun py-toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to."
  (interactive)
  (when
      (setq indent-tabs-mode (not indent-tabs-mode))
    (setq tab-width py-indent-offset))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "indent-tabs-mode %s  py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode (arg &optional iact)
  "With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to.

If IACT is provided, message result"
  (interactive "p")
  (if (< 0 arg)
      (progn
        (setq indent-tabs-mode t)
        (setq tab-width py-indent-offset))
    (setq indent-tabs-mode nil))
  (when (and py-verbose-p (or iact (called-interactively-p 'any))) (message "indent-tabs-mode %s   py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode-on (arg)
  "Switch `indent-tabs-mode' according to ARG."
  (interactive "p")
  (py-indent-tabs-mode (abs arg)(called-interactively-p 'any)))

(defun py-indent-tabs-mode-off (arg)
  "Switch `indent-tabs-mode' according to ARG."
  (interactive "p")
  (py-indent-tabs-mode (- (abs arg))(called-interactively-p 'any)))

;;  Guess indent offset
(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py--guess-indent-final (indents)
  "Calculate and do sanity-check.

Expects INDENTS, a cons"
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      (- first second)
                    (- second first))
                (default-value 'py-indent-offset))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))

(defun py--guess-indent-forward ()
  "Called when moving to end of a form and `py-smart-indentation' is on."
  (let* ((first (if
                    (py--beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (py-forward-statement)
                    (py-backward-statement)
                    (current-indentation))))
         (second (if (or (looking-at py-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (py-forward-statement)
                       (py-forward-statement)
                       (py-backward-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (parse-partial-sexp (point-min) (point))))))
                   (current-indentation))))
    (list first second)))

(defun py--guess-indent-backward ()
  "Called when moving to beginning of a form and `py-smart-indentation' is on."
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward py-extended-block-or-clause-re nil 'move 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (parse-partial-sexp (point-min) (point))))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (py-forward-statement) (py-forward-statement) (py-backward-statement)(current-indentation))))
    (list first second)))

(defun py-guess-indent-offset (&optional direction)
  "Guess `py-indent-offset'.

Set local value of `py-indent-offset', return it

Might change local value of `py-indent-offset' only when called
downwards from beginning of block followed by a statement.
Otherwise ‘default-value’ is returned.
Unless DIRECTION is symbol 'forward, go backward first"
  (interactive)
  (save-excursion
    (let* ((indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py--guess-indent-forward)
                     (py--guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py--guess-indent-forward))
                  (t (py--guess-indent-backward))))
           (erg (py--guess-indent-final indents)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value 'py-indent-offset)))
      (when (called-interactively-p 'any) (message "%s" py-indent-offset))
      py-indent-offset)))

(defun py--comment-indent-function ()
  "Python version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1))))))

;;  make general form below work also in these cases
;;  (defalias 'py-backward-paragraph 'backward-paragraph)
(defun py-backward-paragraph ()
  "Go to beginning of current paragraph.

If already at beginning, go to start of next paragraph upwards"
  (interactive)
  (let ((erg (and (backward-paragraph)(point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;;  (defalias 'py-end-of-paragraph 'forward-paragraph)
(defun py-forward-paragraph ()
    "Go to end of current paragraph.

If already at end, go to end of next paragraph downwards"
  (interactive)
  (let ((erg (and (forward-paragraph)(point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;; ;
(defun py-indent-and-forward (&optional indent)
  "Indent current line according to mode, move one line forward.

If optional INDENT is given, use it"
  (interactive "*")
  (beginning-of-line)
  (when (member (char-after) (list 32 9 10 12 13)) (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point))))
  (indent-to (or indent (py-compute-indentation)))
  (if (eobp)
      (newline-and-indent)
    (forward-line 1))
  (back-to-indentation))

(defun py--indent-line-by-line (beg end)
  "Indent every line until end to max reasonable extend.

Starts from second line of region specified
BEG END deliver the boundaries of region to work within"
  (goto-char beg)
  (py-indent-and-forward)
  ;; (forward-line 1)
  (while (< (line-end-position) end)
    (if (py-empty-line-p)
	(forward-line 1)
      (py-indent-and-forward)))
  (unless (py-empty-line-p) (py-indent-and-forward)))

(defun py-indent-region (&optional beg end no-check)
  "Reindent a region delimited by BEG END.

In case first line accepts an indent, keep the remaining
lines relative.
Otherwise lines in region get outmost indent,
same with optional argument

In order to shift a chunk of code, where the first line is okay, start with second line.

Optional BEG: used by tests
Optional END: used by tests
Optional NO-CHECK: used by tests
"
  (interactive "*")
  (or no-check (use-region-p) (error "Don't see an active region"))
  (let ((end (copy-marker (or end (region-end)))))
    (goto-char (or beg (region-beginning)))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t\r\n\f")
    (py--indent-line-by-line beg end)))

(defun py--beginning-of-buffer-position ()
  "Provided for abstract reasons."
  (point-min))

(defun py--end-of-buffer-position ()
  "Provided for abstract reasons."
  (point-max))

;;  Declarations start
(defun py--bounds-of-declarations ()
  "Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also ‘py--bounds-of-statements’"
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py--beginning-of-statement-p)
                          (py-backward-statement))
                        (unless (py--beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (progn
                (unless (py--beginning-of-statement-p)
                  (py-backward-statement))
                (line-beginning-position))
              (py-backward-statement)
              (not (py--beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char beg)
      (if (and beg end)
          (progn
            (when (called-interactively-p 'any) (message "%s %s" beg end))
            (cons beg end))
        (when (called-interactively-p 'any) (message "%s" nil))
        nil))))

(defun py-backward-declarations ()
  "Got to the beginning of assigments resp. statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-declarations ()
  "Got to the end of assigments resp. statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defalias 'py-copy-declarations 'py-declarations)
(defun py-declarations ()
  "Forms in current level,which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword."
  (interactive)
  (let* ((bounds (py--bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-declarations ()
  "Delete variables declared in current level.

Store deleted variables in ‘kill-ring’"
  (interactive "*")
  (let* ((bounds (py--bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;;  Declarations end

;;  Statements start
(defun py--bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks."
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py--beginning-of-statement-p)
                          (py-backward-statement))
                        (unless (py--beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (point))
      (while (and (setq last beg)
                  (setq beg
                        (when (py-backward-statement)
                          (line-beginning-position)))
		  ;; backward-statement shouldn't stop in string
                  ;; (not (py-in-string-p))
                  (not (py--beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (py--end-of-statement-position))
                  (setq end (py-down-statement))
                  (not (py--beginning-of-block-p))
                  ;; (not (looking-at py-keywords))
                  ;; (not (looking-at "pdb\."))
                  ;; (not (py-in-string-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char orig)
      (if (and beg end)
          (progn
            (when (called-interactively-p 'any) (message "%s %s" beg end))
            (cons beg end))
        (when (called-interactively-p 'any) (message "%s" nil))
        nil))))

(defun py-backward-statements ()
  "Got to the beginning of statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-forward-statements ()
  "Got to the end of statements in current level which don't open blocks."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defalias 'py-copy-statements 'py-statements)
(defun py-statements ()
  "Copy and mark simple statements in current level which don't open blocks.

More general than ‘py-declarations’, which would stop at keywords like a print-statement."
  (interactive)
  (let* ((bounds (py--bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-statements ()
  "Delete statements declared in current level.

Store deleted statements in ‘kill-ring’"
  (interactive "*")
  (let* ((bounds (py--bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))

(defun py--join-words-wrapping (words separator prefix line-length)
  (let ((lines ())
        (current-line prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted."
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                     (py-backward-def)
                     (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                       (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (ver (py-which-python))
         classname erg)
    (if (< ver 3)
        (progn
          (py-backward-class)
          (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
            (setq classname (match-string-no-properties 2)))
          (goto-char orig)
          (setq erg (concat "super(" classname ", self)." funcname "(" args ")"))
          ;; super(C, self).method(arg)"
          (insert erg))
      (goto-char orig)
      (setq erg (concat "super()." funcname "(" args ")"))
      (insert erg))
    erg))

;; Comments
(defun py-delete-comments-in-def-or-class ()
  "Delete all commented lines in def-or-class at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-def-or-class-position))
          (end (py--end-of-def-or-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-class ()
  "Delete all commented lines in class at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-class-position))
          (end (py--end-of-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-block ()
  "Delete all commented lines in block at point."
  (interactive "*")
  (save-excursion
    (let ((beg (py--beginning-of-block-position))
          (end (py--end-of-block-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-region (beg end)
  "Delete all commented lines in region delimited by BEG END."
  (interactive "r*")
  (save-excursion
    (py--delete-comments-intern beg end)))

(defun py--delete-comments-intern (beg end)
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (and (< (line-end-position) end) (not (eobp)))
      (beginning-of-line)
      (if (looking-at (concat "[ \t]*" comment-start))
          (delete-region (point) (1+ (line-end-position)))
        (forward-line 1)))))

;; Edit docstring
(defun py--edit-set-vars ()
  (save-excursion
    (let ((py--editbeg (when (use-region-p) (region-beginning)))
	  (py--editend (when (use-region-p) (region-end)))
	  (pps (parse-partial-sexp (point-min) (point))))
      (when (nth 3 pps)
	(setq py--editbeg (or py--editbeg (progn (goto-char (nth 8 pps))
						 (skip-chars-forward (char-to-string (char-after)))(push-mark) (point))))
	(setq py--editend (or py--editend
			      (progn (goto-char (nth 8 pps))
				     (forward-sexp)
				     (skip-chars-backward (char-to-string (char-before)))
				     (point)))))
      (cons (copy-marker py--editbeg) (copy-marker py--editend)))))

(defun py--write-edit ()
  "When edit is finished, write docstring back to orginal buffer."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[\"']" nil t 1)
    (or (py-escaped)
	(replace-match (concat "\\\\" (match-string-no-properties 0)))))
  (jump-to-register py--edit-register)
  ;; (py-restore-window-configuration)
  (delete-region py--docbeg py--docend)
  (insert-buffer-substring py-edit-buffer))

(defun py-edit--intern (buffer-name mode &optional beg end prefix suffix action)
  "Edit string or active region in ‘python-mode’.

arg BUFFER-NAME: a string.
arg MODE: which buffer-mode used in edit-buffer"
  (interactive "*")
  (save-excursion
    (save-restriction
      (window-configuration-to-register py--edit-register)
      (setq py--oldbuf (current-buffer))
      (let* ((orig (point))
	     (bounds (or (and beg end)(py--edit-set-vars)))
	     relpos editstrg
	     erg)
	(setq py--docbeg (or beg (car bounds)))
	(setq py--docend (or end (cdr bounds)))
	;; store relative position in editstrg
	(setq relpos (1+ (- orig py--docbeg)))
	(setq editstrg (buffer-substring py--docbeg py--docend))
	(set-buffer (get-buffer-create buffer-name))
	(erase-buffer)
	(switch-to-buffer (current-buffer))
	(when prefix (insert prefix))
	(insert editstrg)
	(when suffix (insert suffix))
	(funcall mode)
	(when action
	  (setq erg (funcall action))
	  (erase-buffer)
	  (insert erg))
	(local-set-key [(control c) (control c)] 'py--write-edit)
	(goto-char relpos)
	(message "%s" "Type C-c C-c writes contents back")))))

(defun py-edit-docstring ()
  "Edit docstring or active region in ‘python-mode’."
  (interactive "*")
  (py-edit--intern "Edit docstring" 'python-mode))

(defun py-unpretty-assignment ()
  "Revoke prettyprint, write assignment in a shortest way."
  (interactive "*")
  (save-excursion
    (let* ((beg (py-beginning-of-assignment))
	   (end (copy-marker (py-forward-assignment)))
	   last)
      (goto-char beg)
      (while (and (not (eobp))(re-search-forward "^\\([ \t]*\\)\[\]\"'{}]" end t 1) (setq last (copy-marker (point))))
	(save-excursion (goto-char (match-end 1))
			(when (eq (current-column) (current-indentation)) (delete-region (point) (progn (skip-chars-backward " \t\r\n\f") (point)))))
	(when last (goto-char last))))))

(defun py--prettyprint-assignment-intern (beg end name buffer)
  (let ((proc (get-buffer-process buffer))
	erg)
    ;; (py-send-string "import pprint" proc nil t)
    (py-fast-send-string "import json" proc buffer)
    ;; send the dict/assigment
    (py-fast-send-string (buffer-substring-no-properties beg end) proc buffer)
    ;; do pretty-print
    ;; print(json.dumps(neudict4, indent=4))
    (setq erg (py-fast-send-string (concat "print(json.dumps("name", indent=5))") proc buffer t))
    ;; (message "%s" erg)
    ;; (py-edit--intern "PPrint" 'python-mode beg end)
    ;; (message "%s" (current-buffer))
    ;; (switch-to-buffer (current-buffer))
    (goto-char beg)
    (skip-chars-forward "^{")
    (delete-region (point) (progn (forward-sexp) (point)))
    (insert erg)))

(defun py-prettyprint-assignment ()
  "Prettyprint assignment in ‘python-mode’."
  (interactive "*")
  (window-configuration-to-register py-windows-config-register)
  (save-excursion
    (let* ((beg (py-beginning-of-assignment))
	   (name (py-expression))
	   (end (py-end-of-assignment))
	   (proc-buf (python nil nil "Fast Intern Utility Re-Use")))
      (py--prettyprint-assignment-intern beg end name proc-buf)))
  (py-restore-window-configuration))

(provide 'python-components-edit)
;;; python-components-edit.el ends here
