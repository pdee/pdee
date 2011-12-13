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

;; (defcustom py-just-one-whitespace nil
;;   "If filling functions should normalize whitespaces in region. "

;;   :type 'boolean
;;   :group 'python)

(defvar py-keywords "\\<\\(ArithmeticError\\|AssertionError\\|AttributeError\\|BaseException\\|BufferError\\|BytesWarning\\|DeprecationWarning\\|EOFError\\|Ellipsis\\|EnvironmentError\\|Exception\\|False\\|FloatingPointError\\|FutureWarning\\|GeneratorExit\\|IOError\\|ImportError\\|ImportWarning\\|IndentationError\\|IndexError\\|KeyError\\|KeyboardInterrupt\\|LookupError\\|MemoryError\\|NameError\\|NoneNotImplementedError\\|NotImplemented\\|OSError\\|OverflowError\\|PendingDeprecationWarning\\|ReferenceError\\|RuntimeError\\|RuntimeWarning\\|StandardError\\|StopIteration\\|SyntaxError\\|SyntaxWarning\\|SystemError\\|SystemExit\\|TabError\\|True\\|TypeError\\|UnboundLocalError\\|UnicodeDecodeError\\|UnicodeEncodeError\\|UnicodeError\\|UnicodeTranslateError\\|UnicodeWarning\\|UserWarning\\|ValueError\\|Warning\\|ZeroDivisionError\\|__debug__\\|__import__\\|__name__\\|abs\\|all\\|and\\|any\\|apply\\|as\\|assert\\|basestring\\|bin\\|bool\\|break\\|buffer\\|bytearray\\|callable\\|chr\\|class\\|classmethod\\|cmp\\|coerce\\|compile\\|complex\\|continue\\|copyright\\|credits\\|def\\|del\\|delattr\\|dict\\|dir\\|divmod\\|elif\\|else\\|enumerate\\|eval\\|except\\|exec\\|execfile\\|exit\\|file\\|filter\\|float\\|for\\|format\\|from\\|getattr\\|global\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|if\\|import\\|in\\|input\\|int\\|intern\\|is\\|isinstance\\|issubclass\\|iter\\|lambda\\|len\\|license\\|list\\|locals\\|long\\|map\\|max\\|memoryview\\|min\\|next\\|not\\|object\\|oct\\|open\\|or\\|ord\\|pass\\|pow\\|print\\|property\\|quit\\|raise\\|range\\|raw_input\\|reduce\\|reload\\|repr\\|return\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|unichr\\|unicode\\|vars\\|while\\|with\\|xrange\\|yield\\|zip\\|\\)\\>"
  "Contents like py-fond-lock-keyword")

(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python. "
  (interactive "*")
  (let* ((erg (py-python-default-environment))
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
           (insert (make-string py-indent-offset ?\ )))
          (t
           (if (and (eq need cui)(not (eq cuc cui)))
               (back-to-indentation)
             (beginning-of-line)
             (delete-horizontal-space)
             (indent-to need))))))

(defun py-indent-line (&optional arg)
  "Indent the current line according to Python rules.

When called interactivly with \\[universal-argument], ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].
Returns current indentation "
  (interactive "P")
  (let ((cui (current-indentation))
        (indent (py-compute-indentation))
        (col (current-column)))
    (if (interactive-p)
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (if (eq 4 (prefix-numeric-value arg))
              (indent-to (+ indent py-indent-offset)))
          (indent-to indent))
      (if py-tab-indent
          (cond ((eq indent cui)
                 (when (eq this-command last-command)
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (if (<= (line-beginning-position) (+ (point) (- col cui)))
                       (forward-char (- col cui))
                     (beginning-of-line))))
                ((< cui indent)
                 (if (eq this-command last-command)
                     (progn
                       (beginning-of-line)
                       (delete-horizontal-space)
                       (indent-to (+ (* (/ cui py-indent-offset) py-indent-offset) py-indent-offset))
                       (forward-char (- col cui)))
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to indent)
                   (forward-char (- col cui))))
                (t (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to indent)
                   (if (<= (line-beginning-position) (+ (point) (- col cui)))
                       (forward-char (- col cui))
                     (beginning-of-line))))
        (insert-tab))))
  (message "%s" (current-indentation))
  (current-indentation))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. "
  (interactive "*")
  (let ((ci (current-indentation))
        erg)
    (if (< ci (current-column))         ; if point beyond indentation
        (progn
          (newline)
          (setq erg (indent-to-column (py-compute-indentation))))
      (beginning-of-line)
      (insert-char ?\n 1)
      (setq erg (move-to-column (py-compute-indentation))))
    (when (and (looking-at "[ \t]+")
               (nth 1 (if (featurep 'xemacs)
                          (parse-partial-sexp (point-min) (point))
                        (syntax-ppss))))
      (delete-region (match-beginning 0) (match-end 0)))
    (when (interactive-p) (message "%s" erg))
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
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py-guess-indent-offset (&optional global orig)
  "Guess a value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value.
With optional argument GLOBAL change the global value of `py-indent-offset'. "
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (when orig (goto-char orig))
      (when (< (current-column) (current-indentation))
        (back-to-indentation))
      (let ((lastindent (if
                            (py-beginning-of-statement-p)
                            (current-indentation)
                          (progn
                            (py-beginning-of-statement)
                            (current-indentation)))))
        (if (eq 0 lastindent)
            (setq py-indent-offset (default-value 'py-indent-offset))
          (let* ((firstindent (progn
                                (while (and (<= lastindent (current-indentation))
                                            (not (bobp))
                                            (py-beginning-of-statement)))
                                (current-indentation)))
                 (guessed (- lastindent firstindent)))
            (unless (py-guessed-sanity-check guessed)
              ;; no indent between statements at point
              (setq firstindent (progn
                                  (py-beginning-of-block)
                                  (current-indentation)))
              (setq guessed (- lastindent firstindent)))
            (if (and (py-guessed-sanity-check guessed) (/= guessed py-indent-offset))
                (progn
                  (funcall (if global 'kill-local-variable 'make-local-variable)
                           'py-indent-offset)
                  (setq py-indent-offset guessed))
              (setq py-indent-offset (default-value 'py-indent-offset))))))))
  (when (interactive-p)
    (message "%s value of py-indent-offset:  %d"
             (if global "Global" "Local")
             py-indent-offset))
  py-indent-offset)

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

(defun py-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.

The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (py-end-of-def-or-class class)
    (let ((end (point)))
      (py-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))



(defalias 'py-shift-region-left 'py-shift-left)
(defun py-shift-left (&optional count start end)
  "Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "
  (interactive "p")
  (let ((erg (py-shift-intern (- count) start end)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-shift-region-right 'py-shift-right)
(defun py-shift-right (&optional count beg end)
  "Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "
  (interactive "p")
  (let ((erg (py-shift-intern count beg end)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-intern (count &optional start end)
  (save-excursion
    (let* ((inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position))))
           (orig end))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (dotimes (i (abs count))
        (if (< 0 count)
            (indent-rigidly beg end py-indent-offset)
          (indent-rigidly beg end (- py-indent-offset))))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward " \t\r\n\f"))
    (py-indentation-of-statement)))

;; make general form below work also in these cases
(defalias 'py-beginning-of-paragraph 'backward-paragraph)
(defalias 'py-end-of-paragraph 'forward-paragraph)
(defalias 'py-beginning-of-line 'beginning-of-line)
(defalias 'py-end-of-line 'end-of-line)

;; Shifting forms start
(defun py-shift-forms-base (form arg &optional beg end)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((region-active-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py-shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-right (&optional arg)
  "Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-left (&optional arg)
  "Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-indent-region (start end &optional indent-offset)
  "Reindent a region of Python code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `py-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Python), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")                 ; region; raw prefix arg
  (save-excursion
    (goto-char end) (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((py-indent-offset (prefix-numeric-value
                             (or indent-offset py-indent-offset)))
          (indents '(-1))               ; stack of active indent levels
          (target-column 0)             ; column to which to indent
          (base-shifted-by 0)           ; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (py-compute-indentation)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (eq (following-char) ?#)  ; comment in column 1
              (looking-at "[ \t]*$"))   ; entirely blank
          (setq target-column 0))
         ((py-preceding-line-backslashed-p)      ; shift relative to base line
          (setq target-column (+ ci base-shifted-by)))
         (t                             ; new base line
          (if (> ci (car indents))      ; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* py-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))
;; Shifting forms end

(defun py-beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-paragraph)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-position ()
  "Returns beginning of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-position ()
  "Returns beginning of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-class-position ()
  "Returns beginning of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-clause-position ()
  "Returns beginning of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-line-position ()
  "Returns beginning of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-statement-position ()
  "Returns beginning of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-paragraph)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-block-position ()
  "Returns end of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-clause-position ()
  "Returns end of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-def-position ()
  "Returns end of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-class-position ()
  "Returns end of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-line-position ()
  "Returns end of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-statement-position ()
  "Returns end of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

;; Declarations start
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
                  (not (looking-at py-keywords))
                  (not (looking-at "pdb\."))
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
;; Statements end

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start py-block-comment-prefix))
    (comment-region beg end arg)))

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

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
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

(defun py-fix-this-indent (indent)
  (unless (and (eq (current-indentation) (current-column))
               (eq (current-column) indent))
    (beginning-of-line)
    (indent-to-column indent)
    (delete-region
     (point)
     (progn (skip-chars-forward " \t") (point)))))

(defun py-fill-string (start &optional justify)
  "Fill the paragraph around (point) in the string starting at start"
  ;; basic strategy: narrow to the string and call the default
  ;; implementation
  (let (;; the start of the string's contents
        string-start
        ;; the end of the string's contents
        string-end
        ;; length of the string's delimiter
        delim-length
        ;; The string delimiter
        delim)

    (save-excursion
      (goto-char start)
      (if (looking-at "\\([urbURB]*\\(?:'''\\|\"\"\"\\|'\\|\"\\)\\)\\\\?\n?")
          (setq string-start (match-end 0)
                delim-length (- (match-end 1) (match-beginning 1))
                delim (buffer-substring-no-properties (match-beginning 1)
                                                      (match-end 1)))
        (error "The parameter start is not the beginning of a python string"))

      ;; if the string is the first token on a line and doesn't start with
      ;; a newline, fill as if the string starts at the beginning of the
      ;; line. this helps with one line docstrings
      (save-excursion
        (beginning-of-line)
        (and (/= (char-before string-start) ?\n)
             (looking-at (concat "[ \t]*" delim))
             (setq string-start (point))))

      ;; move until after end of string, then the end of the string's contents
      ;; is delim-length characters before that
      (forward-sexp)
      (setq string-end (- (point) delim-length)))

    ;; Narrow to the string's contents and fill the current paragraph
    (save-restriction
      (narrow-to-region string-start string-end)
      (let ((ends-with-newline (= (char-before (point-max)) ?\n)))
        (fill-paragraph justify)
        (if (and (not ends-with-newline)
                 (= (char-before (point-max)) ?\n))
            ;; the default fill-paragraph implementation has inserted a
            ;; newline at the end. Remove it again.
            (save-excursion
              (goto-char (point-max))
              (delete-char -1)))))

    ;; return t to indicate that we've done our work
    t))

;; designed to word independently from syntax, not needed for now
;; (defun py-fill-string (beg end &optional justify kind this-fill-column)
;;   "Fill the paragraph around (point) in the string starting at start"
;;   (save-excursion
;;     (let* ((start (cond ((or (eq kind 'triplequoted-dq)
;;                              (eq kind 'triplequoted-sq))
;;                          (- beg 3))
;;                         ((or (eq kind 'doublequoted)
;;                              (eq kind 'singlequoted))
;;                          (- beg 1))
;;                         (t beg)))
;;            (indent (progn (goto-char start) (current-column)))
;;            (orig beg)
;;            (fill-column-old fill-column)
;;            empty)
;;       (when this-fill-column (setq fill-column this-fill-column))
;;       (goto-char beg)
;;       (setq empty (looking-at "[ \t]*$"))
;;       (when empty
;;         (forward-line 1))
;;       (beginning-of-line)
;;       (narrow-to-region beg end)
;;       (while (and (<= beg (point))
;;                   (< (setq beg (point))
;;                      (progn (forward-paragraph)(point))))
;;         (save-restriction
;;           (narrow-to-region beg (point))
;;           (goto-char beg)
;;           (when (looking-at "[ \t]*$")
;;             (forward-line 1) (setq beg (point)))
;;           (py-fix-this-indent indent)
;;           ;;           (when py-just-one-whitespace
;;           ;;             (while (re-search-forward "[ \n\r\t]+" nil t 1)
;;           ;;               (replace-match " ")))
;;           (fill-region beg end justify)
;;           (unless (empty-line-p)
;;             (newline))
;;           (widen)))
;;       (when this-fill-column (setq fill-column fill-column-old)))))

(defun py-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Python comments and strings.

If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.
"
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((pps
             (if (featurep 'xemacs)
                 (parse-partial-sexp (point-min) (point))
               (syntax-ppss))))
        (cond
         ;; inside a comment
         ((nth 4 pps)
          (py-fill-comment justify))
         ;; only whitespace before the comment start
         ((save-excursion (beginning-of-line) (looking-at "[ \t]*#"))
          (py-fill-comment justify))
         ;; inside a string
         ((nth 3 pps)
          (py-fill-string (nth 8 pps)))
         ;; opening quote of a string
         ((progn (save-excursion (forward-char 1)(nth 3 pps)))
          (save-excursion
            (forward-char 1)
            (py-fill-string (nth 8 pps)))))))))

(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)"
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                 (py-beginning-of-def)
                 (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                   (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (erg (py-which-python))
         classname)
    (if (< erg 3)
        (progn
          (py-beginning-of-class)
          (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
            (setq classname (match-string-no-properties 2)))
          (goto-char orig)
          ;; super(C, self).method(arg)"
          (insert (concat "super(" classname ", self)." funcname "(" args ")")))
      (goto-char orig)
      (insert (concat "super()." funcname "(" args ")")))))

(provide 'python-components-edit)
;;; python-components-edit.el ends here
