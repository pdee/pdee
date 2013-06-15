;;; python-components-edit.el --- Python edit utilities

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
(require 'python-components-macros)

(defvar py-keywords "\\<\\(ArithmeticError\\|AssertionError\\|AttributeError\\|BaseException\\|BufferError\\|BytesWarning\\|DeprecationWarning\\|EOFError\\|Ellipsis\\|EnvironmentError\\|Exception\\|False\\|FloatingPointError\\|FutureWarning\\|GeneratorExit\\|IOError\\|ImportError\\|ImportWarning\\|IndentationError\\|IndexError\\|KeyError\\|KeyboardInterrupt\\|LookupError\\|MemoryError\\|NameError\\|NoneNotImplementedError\\|NotImplemented\\|OSError\\|OverflowError\\|PendingDeprecationWarning\\|ReferenceError\\|RuntimeError\\|RuntimeWarning\\|StandardError\\|StopIteration\\|SyntaxError\\|SyntaxWarning\\|SystemError\\|SystemExit\\|TabError\\|True\\|TypeError\\|UnboundLocalError\\|UnicodeDecodeError\\|UnicodeEncodeError\\|UnicodeError\\|UnicodeTranslateError\\|UnicodeWarning\\|UserWarning\\|ValueError\\|Warning\\|ZeroDivisionError\\|__debug__\\|__import__\\|__name__\\|abs\\|all\\|and\\|any\\|apply\\|as\\|assert\\|basestring\\|bin\\|bool\\|break\\|buffer\\|bytearray\\|callable\\|chr\\|class\\|classmethod\\|cmp\\|coerce\\|compile\\|complex\\|continue\\|copyright\\|credits\\|def\\|del\\|delattr\\|dict\\|dir\\|divmod\\|elif\\|else\\|enumerate\\|eval\\|except\\|exec\\|execfile\\|exit\\|file\\|filter\\|float\\|for\\|format\\|from\\|getattr\\|global\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|if\\|import\\|in\\|input\\|int\\|intern\\|is\\|isinstance\\|issubclass\\|iter\\|lambda\\|len\\|license\\|list\\|locals\\|long\\|map\\|max\\|memoryview\\|min\\|next\\|not\\|object\\|oct\\|open\\|or\\|ord\\|pass\\|pow\\|print\\|property\\|quit\\|raise\\|range\\|raw_input\\|reduce\\|reload\\|repr\\|return\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|unichr\\|unicode\\|vars\\|while\\|with\\|xrange\\|yield\\|zip\\|\\)\\>"
  "Contents like py-fond-lock-keyword")

;;;
(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python. "
  (interactive "*")
  (let* ((erg (if py-edit-only-p
                  py-shell-name
                (executable-find py-shell-name)))
         (sheb (concat "#! " erg)))
    (insert sheb)))

(defun py-top-level-form-p ()
  "Return non-nil, if line starts with a top level definition.

Used by `py-electric-colon', which will not indent than. "
  (let (erg)
    (save-excursion
      (beginning-of-line)
      (setq erg (or (looking-at py-class-re)
                    (looking-at py-def-re))))
    erg))


(defun py-indent-line-outmost (&optional arg)
  "Indent the current line to the outmost reasonable indent.

With optional \\[universal-argument] an indent with length `py-indent-offset' is inserted unconditionally "
  (interactive "*P")
  (let* ((need (py-compute-indentation (point)))
         (cui (current-indentation))
         (cuc (current-column)))
    (cond ((eq 4 (prefix-numeric-value arg))
	   (if indent-tabs-mode
	       (insert (make-string 1 9))
	     (insert (make-string py-indent-offset 32))))
          (t
           (if (and (eq need cui)(not (eq cuc cui)))
               (back-to-indentation)
             (beginning-of-line)
             (delete-horizontal-space)
             (indent-to need))))))

(defun py-indent-fix-region-intern (beg end)
  "Used when `py-tab-indents-region-p' is non-nil. "
  (let (indent)
    (save-excursion
      (save-restriction
        (beginning-of-line)
        (narrow-to-region beg end)
        (forward-line 1)
        (narrow-to-region (line-beginning-position) end)
        (beginning-of-line)
        (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
        (indent-to (py-compute-indentation))
        (while
            (< (line-end-position) end)
          (forward-line 1)
          (beginning-of-line)
          (delete-region (point) (progn (skip-chars-forward " \t\r\n\f") (point)))
          (indent-to (py-compute-indentation))))))
  ;; (exchange-point-and-mark)
  )

(defun py-indent-line-intern (need cui py-indent-offset &optional beg end region)
  (if py-tab-indent
      (progn
        (and py-tab-indents-region-p region
             (py-indent-fix-region-intern beg end))
        (cond ((eq need cui)
               (if (or (eq this-command last-command)
                       ;; (eq this-command 'exchange-point-and-mark)
                       (eq this-command 'py-indent-line))
                   (if (and py-tab-shifts-region-p region)
                       (while (save-excursion (goto-char beg) (< 0 (current-indentation)))
                         (py-shift-region-left 1 beg end))
                     (beginning-of-line)
                     (delete-horizontal-space)
                     (if (<= (line-beginning-position) (+ (point) (- col cui)))
                         (forward-char (- col cui))
                       (beginning-of-line)))))
              ((< cui need)
               ;; (if (eq this-command last-command)
               (if (and py-tab-shifts-region-p region)
                   (progn
                     (py-shift-region-right 1))
                 (progn
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to (+ (* (/ cui py-indent-offset) py-indent-offset) py-indent-offset))
                   (forward-char (- col cui)))))
              ;; (if (and py-tab-shifts-region-p region)
              ;;     (while (< (current-indentation) need)
              ;;       (py-shift-region-right 1))
              ;;   (beginning-of-line)
              ;;   (delete-horizontal-space)
              ;;   (indent-to need)
              ;;   (forward-char (- col cui)))))
              ((< need cui)
               (if (and py-tab-shifts-region-p region)
                   (progn
                     (when (eq (point) (region-end))
                       (exchange-point-and-mark))
                     (while (< 0 (current-indentation))
                       (py-shift-region-left 1)))
                 (beginning-of-line)
                 (delete-horizontal-space)))
              (t
               (if (and py-tab-shifts-region-p region)
                   (progn
                     ;; (when (eq (point) (region-end))
                     ;; (exchange-point-and-mark))
                     (while (< (current-indentation) need)
                       (py-shift-region-right 1)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to need)
                 (back-to-indentation)
                 (if (<= (line-beginning-position) (+ (point) (- col cui)))
                     (forward-char (- col cui))
                   (beginning-of-line))))))
    (insert-tab)))

(defun py-indent-line (&optional arg)
  "Indent the current line according to Python rules.

When called interactivly with \\[universal-argument], ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

An optional \\[universal-argument] followed by a numeric argument neither 1 nor 4 will switch off `py-smart-indentation' for this execution. This permits to correct allowed but unwanted indents.
Similar to `toggle-py-smart-indentation' resp. `py-smart-indentation-off' followed by TAB.

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].
Returns current indentation

When `py-tab-shifts-region-p' is `t', not just the current line,
but the region is shiftet that way.

If `py-tab-indents-region-p' is `t' and first TAB doesn't shift
--as indent is at outmost reasonable--, indent-region is called. "
  (interactive "P")
  (if (interactive-p)
      ;; TAB-leaves-point-in-the-wrong-lp-1178453-test
      ;; (save-excursion
      (let ((orig (copy-marker (point)))
	    (region (use-region-p))
	    beg end)
	(and region (setq beg (region-beginning))
	     (setq end (region-end)))
	(let ((cui (current-indentation))
	      (col (current-column))
	      (this-indent-offset (cond ((and py-smart-indentation (not (eq this-command last-command)))
					 (py-guess-indent-offset))
					((and py-smart-indentation (eq this-command last-command) py-already-guessed-indent-offset)
					 py-already-guessed-indent-offset)
					(t (default-value 'py-indent-offset))))
	      (need (if (and (eq this-command last-command) py-already-guessed-indent-offset)
			(if region
			    (save-excursion
			      ;; if previous command was an indent
			      ;; already, position reached might
			      ;; produce false guesses
			      (goto-char beg) (py-compute-indentation beg nil nil nil nil nil py-already-guessed-indent-offset))
			  (py-compute-indentation beg nil nil nil nil nil py-already-guessed-indent-offset))
		      (if region
			  (save-excursion
			    (goto-char beg)
			    (save-excursion (goto-char beg) (py-compute-indentation)))
			(py-compute-indentation)))))
	  (unless (eq this-command last-command)
	    (setq py-already-guessed-indent-offset this-indent-offset))
	  (cond ((eq 4 (prefix-numeric-value arg))
		 (beginning-of-line)
		 (delete-horizontal-space)
		 (indent-to (+ need py-indent-offset)))
		((not (eq 1 (prefix-numeric-value arg)))
		 (py-smart-indentation-off)
		 (py-indent-line-intern need cui this-indent-offset beg end region))
		(t (py-indent-line-intern need cui this-indent-offset beg end region)))
	  (when (and (interactive-p) py-verbose-p)(message "%s" (current-indentation)))
	  (current-indentation))
	(goto-char orig)
	(if region
	    (progn
	      (or py-tab-shifts-region-p
		  py-tab-indents-region-p)
	      (eq (point) end)
	      (not (eq (point) orig))
	      (exchange-point-and-mark))
	  (and (< (current-column) (current-indentation))(back-to-indentation))))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (py-compute-indentation))))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. "
  (interactive "*")
  (let ((orig (point))
        erg pos)
    (newline)
    (when (or py-newline-delete-trailing-whitespace-p py-trailing-whitespace-smart-delete-p)
      (setq pos (copy-marker (point)))
      (save-excursion
        (goto-char orig)
        (if (empty-line-p)
            (delete-trailing-whitespace (line-beginning-position) pos)
          (skip-chars-backward " \t") 
          (delete-trailing-whitespace (point)  pos))))
    (setq erg (indent-to-column (py-compute-indentation)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-newline-and-close-block 'py-newline-and-dedent)
(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current.
Returns column. "
  (interactive "*")
  (let ((cui (current-indentation))
        erg)
    (newline)
    (when (< 0 cui)
      (setq erg (- (py-compute-indentation) py-indent-offset))
      (indent-to-column erg))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py-toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to. "
  (interactive)
  (when
      (setq indent-tabs-mode (not indent-tabs-mode))
    (setq tab-width py-indent-offset))
  (when (and py-verbose-p (interactive-p)) (message "indent-tabs-mode %s  py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode (arg &optional iact)
  "With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to. "
  (interactive "p")
  (if (< 0 arg)
      (progn
        (setq indent-tabs-mode t)
        (setq tab-width py-indent-offset))
    (setq indent-tabs-mode nil))
  (when (and py-verbose-p (or iact (interactive-p))) (message "indent-tabs-mode %s   py-indent-offset %s" indent-tabs-mode py-indent-offset))
  indent-tabs-mode)

(defun py-indent-tabs-mode-on (arg)
  "Switch `indent-tabs-mode' on. "
  (interactive "p")
  (py-indent-tabs-mode (abs arg)(interactive-p)))

(defun py-indent-tabs-mode-off (arg)
  "Switch `indent-tabs-mode' on. "
  (interactive "p")
  (py-indent-tabs-mode (- (abs arg))(interactive-p)))

;;; Guess indent offset
(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py-guess-indent-final (indents orig)
  "Calculate and do sanity-check. "
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      ;; (< (point) orig)
                      (- first second)
                    (- second first))
                (default-value 'py-indent-offset))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))


(defun py-guess-indent-forward ()
  "Called when moving to end of a form and `py-smart-indentation' is on. "
  (interactive)
  (let* ((first (if
                    (py-beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (py-end-of-statement)
                    (py-beginning-of-statement)
                    (current-indentation))))
         (second (if (or (looking-at py-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (py-end-of-statement)
                       (py-end-of-statement)
                       (py-beginning-of-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (syntax-ppss)))))
                   (current-indentation))))
    (list first second)))

(defun py-guess-indent-backward ()
  "Called when moving to beginning of a form and `py-smart-indentation' is on. "
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (syntax-ppss)))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement)(current-indentation))))
    (list first second)))

(defun py-guess-indent-offset (&optional direction)
  "Guess `py-indent-offset'.

Set local value of `py-indent-offset', return it

Might change local value of `py-indent-offset' only when called
downwards from beginning of block followed by a statement. Otherwise default-value is returned.
"
  (interactive)
  (save-excursion
    (let* ((orig (point))
           (indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py-guess-indent-forward)
                     (py-guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py-guess-indent-forward))
                  (t (py-guess-indent-backward))))
           (erg (py-guess-indent-final indents orig)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value 'py-indent-offset)))
      (when (interactive-p) (message "%s" py-indent-offset))
      py-indent-offset)))

;; (defun py-guess-indent-offset (&optional global orig origline)
;;   "Guess a value for, and change, `py-indent-offset'.
;;
;; By default, make a buffer-local copy of `py-indent-offset' with the
;; new value.
;; With optional argument GLOBAL change the global value of `py-indent-offset'.
;;
;; Returns `py-indent-offset'"
;;   (interactive "P")
;;   (save-excursion
;;     (let* ((orig (or orig (point)))
;;            (origline (or origline (py-count-lines)))
;;            last down done firstindent secondindent
;;            (count 0)
;;            guessed)
;;       (back-to-indentation)
;;       (when (looking-at py-block-closing-keywords-re)
;;         (setq count (1+ count)))
;;       (skip-chars-backward " \t\r\n\f")
;;       (back-to-indentation)
;;       (when (looking-at py-block-closing-keywords-re)
;;         (setq count (1+ count)))
;;       (when (< 0 count)
;;         (while (and (< 0 count)(re-search-backward py-block-re nil t 1)
;;                     (or
;;                      (nth 8 (parse-partial-sexp (point-min) (point)))
;;                      (progn (setq count (1- count)) t))))
;;         (setq firstindent (current-indentation)))
;;       (unless firstindent
;;         (setq firstindent
;;               (cond ((and (py-beginning-of-statement-p) (looking-at py-extended-block-or-clause-re))
;;                      (current-indentation))
;;                     ((and (py-beginning-of-statement-p)(looking-at py-block-closing-keywords-re))
;;                      (while (and (re-search-backward py-extended-block-or-clause-re nil t 1)(nth 8 (syntax-ppss))))
;;                      ;; (py-beginning-of-statement)
;;                      (when (py-beginning-of-statement-p) (current-indentation)))
;;                     ((and (py-beginning-of-statement)(looking-at py-extended-block-or-clause-re))
;;                      (current-indentation))
;;                     (t (while (and (setq last (py-beginning-of-statement))(not (looking-at py-extended-block-or-clause-re))))
;;                        (if last
;;                            (progn
;;                              (setq last (point))
;;                              (setq down t)
;;                              (current-indentation))
;;                          (if (and (goto-char orig)
;;                                   (py-end-of-statement)
;;                                   (py-end-of-statement)
;;                                   (py-beginning-of-statement)
;;                                   (looking-at py-extended-block-or-clause-re))
;;
;;                              (prog1
;;                                  (current-indentation)
;;                                (setq last (point))
;;                                (goto-char orig))
;;                            (while (and (setq last (py-down-statement))(not (looking-at py-extended-block-or-clause-re)))
;;                              (if last
;;                                  (progn (setq last (point))
;;                                         (setq down t)
;;                                         (current-column))
;;                                ;; if nothing suitable around, us default
;;                                (setq done t)
;;                                (default-value 'py-indent-offset)))))))))
;;       (setq secondindent
;;             (unless done
;;               (if (and firstindent (numberp firstindent))
;;                   ;; let's look if inside a clause
;;                   (cond ((and
;;                           ;; (goto-char orig)
;;                           (not (eobp))(py-end-of-statement)(py-end-of-statement)(setq last (point))
;;                           (save-excursion (< firstindent (progn (py-beginning-of-statement)(current-indentation))))
;;                           (py-end-of-statement-p))
;;                          (py-beginning-of-statement) (current-indentation))
;;                         (t (if (progn (setq orig (point)) (while (and (py-beginning-of-statement)(>= firstindent (current-indentation)) (setq last (point)) (not (looking-at py-extended-block-or-clause-re)))) last)
;;                                (current-indentation)
;;                              (goto-char orig)
;;                              (while (and (not (eobp))(py-end-of-statement)(setq last (point))
;;                                          (save-excursion (or (>= firstindent (progn (py-beginning-of-statement)(current-indentation)))(eq last (line-beginning-position))))
;;                                          (py-end-of-statement-p)))
;;                              (when last (py-beginning-of-statement) (current-indentation))))))))
;;       (unless (or done secondindent)
;;         (setq secondindent
;;               (when (and (py-end-of-statement)
;;                          (py-end-of-statement)
;;                          (py-beginning-of-statement))
;;                 (current-indentation))))
;;       (when (and secondindent (numberp secondindent) (numberp firstindent))
;;         (when (eq 0 (abs (- secondindent firstindent)))
;;           (when (if (py-beginning-of-statement) (< (current-indentation) secondindent))
;;             (setq secondindent (current-indentation))))
;;         (setq guessed
;;               (abs (- secondindent firstindent)))
;;         (when (and (eq 0 guessed)(not (eq 0 secondindent)))
;;           (setq guessed secondindent)))
;;       (if (and guessed (py-guessed-sanity-check guessed))
;;           (setq py-indent-offset guessed)
;;         (setq py-indent-offset (default-value 'py-indent-offset)))
;;       (funcall (if global 'kill-local-variable 'make-local-variable)
;;                'py-indent-offset)
;;       (when (and py-verbose-p (interactive-p))
;;         (message "%s value of py-indent-offset:  %d"
;;                  (if global "Global" "Local")
;;                  py-indent-offset))
;;       py-indent-offset))
  ;; )

(defun py-comment-indent-function ()
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

(defun py-narrow-to-defun ()
  "Make text outside current def or class invisible.

The defun visible is the one that contains point or follows point. "
  (interactive)
  (save-excursion
    (let ((start (if (py-statement-opens-def-or-class-p)
                     (point)
                   (py-beginning-of-def-or-class))))
      (py-end-of-def-or-class)
      (narrow-to-region (point) start))))

;; make general form below work also in these cases
(defalias 'py-beginning-of-paragraph 'backward-paragraph)
;; (defun py-beginning-of-paragraph ()
;;   "Go to beginning of paragraph.
;;
;; Consider a string starting at current indentation as beginning too "
;;   (interactive)
;;   (let* ((pps (syntax-ppss))
;;          (erg (and (nth 3 pps)(nth 8 pps))))
;;     (if (and erg (progn (back-to-indentation) (looking-at "\"\"\"\\|'''\\|\"\\|'")))
;;         erg
;;       (backward-paragraph))))

(defalias 'py-end-of-paragraph 'forward-paragraph)

;;;
(defun py-indent-and-forward ()
  "Indent current line according to mode, move one line forward. "
  (interactive "*")
  (beginning-of-line)
  (fixup-whitespace)
  (indent-to (py-compute-indentation))
  (if (eobp)
      (newline-and-indent)
    (forward-line 1))
  (back-to-indentation))

(defun py-indent-region (start end &optional indent-offset recursive)
  "Reindent a region of Python code.

With optional INDENT-OFFSET specify a different value than `py-indent-offset' at place.

Guesses the outmost reasonable indent
Returns and keeps relative position "
  (interactive "*r\nP")
  (let ((orig (copy-marker (point)))
        (beg start)
        (end (copy-marker end))
        (py-indent-offset (prefix-numeric-value
                           (or indent-offset py-indent-offset))))
    (goto-char beg)
    (while (< (line-end-position) end)
      (if (empty-line-p)
          (forward-line 1)
        (py-indent-and-forward)))
    (unless (empty-line-p) (py-indent-line))
    (goto-char orig)))

;;; Positions
(defun py-beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn (py-beginning-of-paragraph) (skip-chars-forward " \t\r\n\f") (point))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn (py-end-of-paragraph) (skip-chars-backward " \t\r\n\f") (point))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-position ()
  "Returns beginning of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-block-position ()
  "Returns end of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-clause-position ()
  "Returns beginning of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-clause-position ()
  "Returns end of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-or-clause-position ()
  "Returns beginning of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block-or-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-block-or-clause-position ()
  "Returns end of block-or-clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block-or-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-position ()
  "Returns beginning of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-def-position ()
  "Returns end of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-class-position ()
  "Returns beginning of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-class-position ()
  "Returns end of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-or-class-position ()
  "Returns beginning of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def-or-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-def-or-class-position ()
  "Returns end of def-or-class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def-or-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-line-position ()
  "Returns beginning of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-line-position ()
  "Returns end of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-statement-position ()
  "Returns beginning of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-statement-position ()
  "Returns end of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-expression-position ()
  "Returns beginning of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-expression)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-expression-position ()
  "Returns end of expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-expression)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-partial-expression-position ()
  "Returns beginning of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-partial-expression)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-partial-expression-position ()
  "Returns end of partial-expression position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-partial-expression)))
      (when (interactive-p) (message "%s" erg))
      erg)))

;;; Bounds
(defun py-bounds-of-statement (&optional position)
  "Returns bounds of statement at point.

With optional POSITION, a number, report bounds of statement at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-statement-position))
            (end (py-end-of-statement-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-block (&optional position)
  "Returns bounds of block at point.

With optional POSITION, a number, report bounds of block at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-block-position))
            (end (py-end-of-block-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-clause (&optional position)
  "Returns bounds of clause at point.

With optional POSITION, a number, report bounds of clause at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-clause-position))
            (end (py-end-of-clause-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-block-or-clause (&optional position)
  "Returns bounds of block-or-clause at point.

With optional POSITION, a number, report bounds of block-or-clause at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-block-or-clause-position))
            (end (py-end-of-block-or-clause-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-def (&optional position)
  "Returns bounds of def at point.

With optional POSITION, a number, report bounds of def at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-def-position))
            (end (py-end-of-def-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-class (&optional position)
  "Returns bounds of class at point.

With optional POSITION, a number, report bounds of class at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-class-position))
            (end (py-end-of-class-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-region ()
  "Returns bounds of region at point.

Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-buffer (&optional position)
  "Returns bounds of buffer at point.

With optional POSITION, a number, report bounds of buffer at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-buffer-position))
            (end (py-end-of-buffer-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-expression (&optional position)
  "Returns bounds of expression at point.

With optional POSITION, a number, report bounds of expression at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-expression-position))
            (end (py-end-of-expression-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-bounds-of-partial-expression (&optional position)
  "Returns bounds of partial-expression at point.

With optional POSITION, a number, report bounds of partial-expression at POSITION.
Returns a list, whose car is beg, cdr - end."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-partial-expression-position))
            (end (py-end-of-partial-expression-position)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py-beginning-of-buffer-position ()
  (point-min))

(defun py-end-of-buffer-position ()
  (point-max))

;;; Declarations start
(defun py-bounds-of-declarations ()
  "Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also py-bounds-of-statements "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (progn
                (unless (py-beginning-of-statement-p)
                  (py-beginning-of-statement))
                (line-beginning-position))
              (py-beginning-of-statement)
              (not (py-beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char beg)
      (if (and beg end)
          (progn
            (when (interactive-p) (message "%s %s" beg end))
            (cons beg end))
        (when (interactive-p) (message "%s" nil))
        nil))))

(defalias 'py-backward-declarations 'py-beginning-of-declarations)
(defun py-beginning-of-declarations ()
  "Got to the beginning of assigments resp. statements in current level which don't open blocks.
"
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-of-declarations 'py-end-of-declarations)
(defun py-end-of-declarations ()
  "Got to the end of assigments resp. statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-copy-declarations 'py-declarations)
(defun py-declarations ()
  "Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
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

Store deleted variables in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;; Declarations end

;; Statements start
(defun py-bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (point))
      (while (and (setq last beg)
                  (setq beg
                        (when (py-beginning-of-statement)
                          (line-beginning-position)))
                  (not (py-in-string-p))
                  (not (py-beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  ;; (not (looking-at py-keywords))
                  ;; (not (looking-at "pdb\."))
                  (not (py-in-string-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char orig)
      (if (and beg end)
          (progn
            (when (interactive-p) (message "%s %s" beg end))
            (cons beg end))
        (when (interactive-p) (message "%s" nil))
        nil))))

(defalias 'py-backward-statements 'py-beginning-of-statements)
(defun py-beginning-of-statements ()
  "Got to the beginning of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-of-statements 'py-end-of-statements)
(defun py-end-of-statements ()
  "Got to the end of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-copy-statements 'py-statements)
(defun py-statements ()
  "Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
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

Store deleted statements in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;;;

(defun py-join-words-wrapping (words separator line-prefix line-length)
  (let ((lines ())
        (current-line line-prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat line-prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-fix-this-indent (indent)
  (unless (and (eq (current-indentation) (current-column))
               (eq (current-column) indent))
    (beginning-of-line)
    (indent-to-column indent)
    (delete-region
     (point)
     (progn (skip-chars-forward " \t") (point)))))

(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)

Returns the string inserted. "
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                     (py-beginning-of-def)
                     (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                       (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (ver (py-which-python))
         classname erg)
    (if (< ver 3)
        (progn
          (py-beginning-of-class)
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

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start py-block-comment-prefix))
    (comment-region beg end arg)))

(defun py-delete-comments-in-def-or-class ()
  "Delete all commented lines in def-or-class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-def-or-class-position))
          (end (py-end-of-def-or-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-class ()
  "Delete all commented lines in class at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-class-position))
          (end (py-end-of-class-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-block ()
  "Delete all commented lines in block at point"
  (interactive "*")
  (save-excursion
    (let ((beg (py-beginning-of-block-position))
          (end (py-end-of-block-position)))
      (and beg end (py--delete-comments-intern beg end)))))

(defun py-delete-comments-in-region (beg end)
  "Delete all commented lines in region. "
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

(provide 'python-components-edit)
