;;; thingatpt-python-expressions.el --- more th-at-point edit functions

;; Copyright (C) 2010 Andreas Roehler, unless
;; indicated otherwise

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Keywords: convenience

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

(require 'thingatpt-utils-base)

;; Introduces a framework to return, mover over or
;; manipulate a given Python programming expressing, a
;; THING. 

;; Implemented for the moment are py-block,
;; py-block-or-clause, py-class, py-clause,
;; py-def-or-class, py-def, py-expression,
;; py-partial-expression, py-statement, py-string. See
;; the contents of variable `ar-atpt-python-list'

;; For all of these the following functions are provided:
;; ar-THING-atpt
;; ar-THING-bounds-atpt
;; ar-THING-beginning-position-atpt
;; ar-THING-end-position-atpt
;; ar-THING-beginning-atpt
;; ar-THING-end-atpt
;; ar-THING-length-atpt
;; ar-THING-copy-atpt
;; ar-THING-kill-atpt
;; ar-THING-forward-atpt
;; ar-THING-backward-atpt
;; ar-THING-transpose-atpt
;; ar-THING-sort-atpt
;; ar-THING-check-atpt

;; Beside of the mentioned Python specific stuff,
;; thing-at-point-utils.el delivers a set of lower level
;; commands for text manipulation:

;; For example `ar-alnum-atpt' will return all
;; alpha-numerical chars below and around cursor as a
;; string. `ar-bounds-of-alnum-atpt' returns the
;; borders of that string as a list and so on. 

;; The following example given for `bracketed' exists
;; for braced, parentized and many more

;; ar-bracketed-atpt
;; ar-bracketed-bounds-atpt
;; ar-bracketed-beginning-position-atpt
;; ar-bracketed-end-position-atpt
;; ar-bracketed-beginning-atpt
;; ar-bracketed-end-atpt
;; ar-bracketed-length-atpt
;; ar-bracketed-copy-atpt
;; ar-bracketed-kill-atpt
;; ar-bracketed-forward-atpt
;; ar-bracketed-backward-atpt
;; ar-bracketed-transpose-atpt
;; ar-bracketed-sort-atpt
;; ar-bracketed-check-atpt

;; Kind of commands below will rather seldom come in use
;; As related tasks usually will be done with a
;; regexp-search-and-replace - if the command needed
;; exists already, all the time spent with typos etc.
;; might be saved, just try:

;; ar-THING-slash-atpt
;; ar-THING-double-backslash-atpt
;; ar-THING-doubleslash-atpt
;; ar-THING-delete-in-region
;; ar-blok-THING-atpt
;; ar-THING-escape-atpt
;; ar-THING-doublequote-atpt
;; ar-THING-doubleslash-paren-atpt
;; ar-THING-slashparen-atpt
;; ar-THING-dollar-atpt
;; ar-THING-equalize-atpt
;; ar-THING-greater-angle-atpt
;; ar-THING-lesser-angle-atpt
;; ar-THING-backslash-atpt
;; ar-THING-brace-atpt
;; ar-THING-bracket-atpt
;; ar-comment-THING-atpt
;; ar-commatize-THING-atpt
;; ar-quote-THING-atpt
;; ar-THING-hyphen-atpt
;; ar-THING-mark-atpt
;; ar-THING-hide-atpt
;; ar-THING-show-atpt
;; ar-THING-hide-show-atpt
;; ar-THING-left-right-singlequote-atpt
;; ar-THING-parentize-atpt
;; ar-THING-separate-atpt
;; ar-THING-singlequote-atpt
;; ar-THING-trim-atpt
;; ar-THING-left-trim-atpt
;; ar-THING-right-trim-atpt
;; ar-underscore-THING-atpt
;; ar-whitespace-THING-atpt

;; A littel bit harder to aquire, however very
;; effective in daily use here are commans named
;; ar-THING-or-copy-atpt.

;; For example the docstring of `ar-brace-or-copy-atpt'
;; says:

;; "If region is highlighted, provide THING at point
;; with brace(s), otherwise copy brace(ed) at point.
;; With NO-DELIMITERS, copy brace(ed) without
;; delimiters. With negative argument kill brace(ed) at
;; point. "

;; see also
;; thing-at-point-utils.el from
;; https://code.launchpad.net/s-x-emacs-werkstatt/

;; If `thing-at-point-utils.el' is loaded,
;; see: M-x apropos bracketed
;; See also: M-x apropos parentized

;;; Code

;; Python-expression
(put 'py-expression 'beginning-op-at
     (lambda ()
       (py-beginning-of-expression)))

(put 'py-expression 'end-op-at
     (lambda ()
       (py-end-of-expression)))

;; Block
(put 'py-block 'beginning-op-at
     'py-beginning-of-block)

(put 'py-block 'end-op-at
     (lambda ()
       (py-end-of-block)))

;; Block-Or-Clause
(put 'py-block-or-clause 'beginning-op-at
     (lambda ()
       (py-beginning-of-block-or-clause)))

(put 'py-block-or-clause 'end-op-at
     (lambda ()
       (py-end-of-block-or-clause)))

;; Def-Or-Class
(put 'py-def-or-class 'beginning-op-at
     (lambda ()
       (py-beginning-of-def-or-class)))

(put 'py-def-or-class 'end-op-at
     (lambda ()
       (py-end-of-def-or-class)))

;; Class
(put 'py-class 'beginning-op-at
     (lambda ()
       (py-beginning-of-class)))

(put 'py-class 'end-op-at
     (lambda ()
       (py-end-of-class)))

;; Clause
(put 'py-clause 'beginning-op-at
     (lambda ()
       (py-beginning-of-clause)))

(put 'py-clause 'end-op-at
     (lambda ()
       (py-end-of-clause)))

;; Def
(put 'py-def 'beginning-op-at
     (lambda ()
       (py-beginning-of-def)))

(put 'py-def 'end-op-at
     (lambda ()
       (py-end-of-def)))

;; Statement
(put 'py-statement 'beginning-op-at
     (lambda ()
       (py-beginning-of-statement)))

(put 'py-statement 'end-op-at
     (lambda ()
       (py-end-of-statement)))

;; Py-String
(put 'py-string 'beginning-op-at
     (lambda ()
       (let ((erg (ar-in-string-p)))
         (when erg
           (goto-char (car-safe erg)))
         (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
           (list (match-beginning 0) (match-end 0))))))

(put 'py-string 'end-op-at
     (lambda ()
       (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
         (goto-char (match-end 0))
         (end-of-form-base (match-string-no-properties 0) (match-string-no-properties 0) nil 'move 1 nil t))))


;; python-unpaired start

(defalias 'ar-in-triplequoted-p-atpt 'ar-triplequoted-in-p-atpt)

(defun ar-triplequoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` triplequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"\"\"\\|'''" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-triplequoted-dq-p-atpt 'ar-triplequoted-dq-in-p-atpt)

(defun ar-triplequoted-dq-in-p-atpt (&optional condition)
  "Returns beginning position of ` triplequoted-dq' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"\"\"" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-triplequoted-sq-p-atpt 'ar-triplequoted-sq-in-p-atpt)

(defun ar-triplequoted-sq-in-p-atpt (&optional condition)
  "Returns beginning position of ` triplequoted-sq' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "'''" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

;; python-unpaired end

;; python-list start


(defun py-block-atpt (&optional arg no-delimiters)
  "Returns py-block at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-block arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-block-atpt 'py-block-bounds-atpt)

(defun py-block-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-block if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-block no-delimiters (interactive-p)))


(defun py-block-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-block at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-block no-delimiters (interactive-p)))


(defun py-block-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-block at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-block no-delimiters (interactive-p)))

(defalias 'beginning-of-py-block-atpt 'py-block-beginning-atpt)

(defun py-block-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-block at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-block no-delimiters (interactive-p)))

(defalias 'end-of-py-block-atpt 'py-block-end-atpt)

(defun py-block-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-block at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-block no-delimiters (interactive-p)))

(defalias 'length-of-py-block-atpt 'py-block-length-atpt)

(defun py-block-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-block at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-block no-delimiters (interactive-p)))

(defalias 'copy-py-block-atpt 'py-block-copy-atpt)

(defun py-block-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-block at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-block no-delimiters (interactive-p)))

(defalias 'delete-py-block-in-region 'py-block-delete-in-region)

(defun py-block-delete-in-region (beg end)
  "Deletes py-block at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-block beg end (interactive-p)))


(defun comment-py-block-atpt (&optional no-delimiters)
  "Comments py-block at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-block no-delimiters (interactive-p)))

(defalias 'mark-py-block-atpt 'py-block-mark-atpt)

(defun py-block-mark-atpt (&optional no-delimiters)
  "Marks py-block at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-block))

(defalias 'hide-py-block-atpt 'py-block-hide-atpt)

(defun py-block-hide-atpt ()
  "Hides py-block at point. "
  (interactive)
  (ar-th-hide 'py-block))

(defalias 'show-py-block-atpt 'py-block-show-atpt)

(defun py-block-show-atpt ()
  "Shows hidden py-block at point. "
  (interactive)
  (ar-th-show 'py-block))

(defalias 'hide-show-py-block-atpt 'py-block-hide-show-atpt)

(defun py-block-hide-show-atpt ()
  "Alternatively hides or shows py-block at point. "
  (interactive)
  (ar-th-hide-show 'py-block))

(defalias 'highlight-py-block-atpt-mode 'py-block-highlight-atpt-mode)

(defun py-block-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-block-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-block no-delimiters (interactive-p)))

(defalias 'kill-py-block-atpt 'py-block-kill-atpt)

(defun py-block-kill-atpt (&optional no-delimiters)
  "Kills py-block at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-block no-delimiters (interactive-p)))

(defalias 'separate-py-block-atpt 'py-block-separate-atpt)

(defun py-block-separate-atpt (&optional no-delimiters)
  "Separates py-block at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-block no-delimiters (interactive-p)))

(defalias 'forward-py-block-atpt 'py-block-forward-atpt)

(defun py-block-forward-atpt (&optional arg)
  "Moves forward over py-block at point if any, does nothing otherwise.
Returns end position of py-block "
  (interactive "p")
  (ar-th-forward 'py-block arg (interactive-p)))

(defalias 'backward-py-block-atpt 'py-block-backward-atpt)

(defun py-block-backward-atpt (&optional arg)
  "Moves backward over py-block before point if any, does nothing otherwise.
Returns beginning position of py-block "
  (interactive "p")
  (ar-th-backward 'py-block arg (interactive-p)))

(defalias 'check-py-block-atpt 'py-block-check-atpt)

(defun py-block-check-atpt ()
  "Return t if a py-block at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-block-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-block-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-block-or-clause-atpt (&optional arg no-delimiters)
  "Returns py-block-or-clause at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-block-or-clause arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-block-or-clause-atpt 'py-block-or-clause-bounds-atpt)

(defun py-block-or-clause-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-block-or-clause if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-block-or-clause no-delimiters (interactive-p)))


(defun py-block-or-clause-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-block-or-clause at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-block-or-clause no-delimiters (interactive-p)))


(defun py-block-or-clause-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-block-or-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'beginning-of-py-block-or-clause-atpt 'py-block-or-clause-beginning-atpt)

(defun py-block-or-clause-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-block-or-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'end-of-py-block-or-clause-atpt 'py-block-or-clause-end-atpt)

(defun py-block-or-clause-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-block-or-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'length-of-py-block-or-clause-atpt 'py-block-or-clause-length-atpt)

(defun py-block-or-clause-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-block-or-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'copy-py-block-or-clause-atpt 'py-block-or-clause-copy-atpt)

(defun py-block-or-clause-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-block-or-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'delete-py-block-or-clause-in-region 'py-block-or-clause-delete-in-region)

(defun py-block-or-clause-delete-in-region (beg end)
  "Deletes py-block-or-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-block-or-clause beg end (interactive-p)))


(defun comment-py-block-or-clause-atpt (&optional no-delimiters)
  "Comments py-block-or-clause at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'mark-py-block-or-clause-atpt 'py-block-or-clause-mark-atpt)

(defun py-block-or-clause-mark-atpt (&optional no-delimiters)
  "Marks py-block-or-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-block-or-clause))

(defalias 'hide-py-block-or-clause-atpt 'py-block-or-clause-hide-atpt)

(defun py-block-or-clause-hide-atpt ()
  "Hides py-block-or-clause at point. "
  (interactive)
  (ar-th-hide 'py-block-or-clause))

(defalias 'show-py-block-or-clause-atpt 'py-block-or-clause-show-atpt)

(defun py-block-or-clause-show-atpt ()
  "Shows hidden py-block-or-clause at point. "
  (interactive)
  (ar-th-show 'py-block-or-clause))

(defalias 'hide-show-py-block-or-clause-atpt 'py-block-or-clause-hide-show-atpt)

(defun py-block-or-clause-hide-show-atpt ()
  "Alternatively hides or shows py-block-or-clause at point. "
  (interactive)
  (ar-th-hide-show 'py-block-or-clause))

(defalias 'highlight-py-block-or-clause-atpt-mode 'py-block-or-clause-highlight-atpt-mode)

(defun py-block-or-clause-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-block-or-clause-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'kill-py-block-or-clause-atpt 'py-block-or-clause-kill-atpt)

(defun py-block-or-clause-kill-atpt (&optional no-delimiters)
  "Kills py-block-or-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'separate-py-block-or-clause-atpt 'py-block-or-clause-separate-atpt)

(defun py-block-or-clause-separate-atpt (&optional no-delimiters)
  "Separates py-block-or-clause at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-block-or-clause no-delimiters (interactive-p)))

(defalias 'forward-py-block-or-clause-atpt 'py-block-or-clause-forward-atpt)

(defun py-block-or-clause-forward-atpt (&optional arg)
  "Moves forward over py-block-or-clause at point if any, does nothing otherwise.
Returns end position of py-block-or-clause "
  (interactive "p")
  (ar-th-forward 'py-block-or-clause arg (interactive-p)))

(defalias 'backward-py-block-or-clause-atpt 'py-block-or-clause-backward-atpt)

(defun py-block-or-clause-backward-atpt (&optional arg)
  "Moves backward over py-block-or-clause before point if any, does nothing otherwise.
Returns beginning position of py-block-or-clause "
  (interactive "p")
  (ar-th-backward 'py-block-or-clause arg (interactive-p)))

(defalias 'check-py-block-or-clause-atpt 'py-block-or-clause-check-atpt)

(defun py-block-or-clause-check-atpt ()
  "Return t if a py-block-or-clause at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-block-or-clause-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-block-or-clause-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-class-atpt (&optional arg no-delimiters)
  "Returns py-class at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-class arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-class-atpt 'py-class-bounds-atpt)

(defun py-class-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-class if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-class no-delimiters (interactive-p)))


(defun py-class-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-class at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-class no-delimiters (interactive-p)))


(defun py-class-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-class no-delimiters (interactive-p)))

(defalias 'beginning-of-py-class-atpt 'py-class-beginning-atpt)

(defun py-class-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-class no-delimiters (interactive-p)))

(defalias 'end-of-py-class-atpt 'py-class-end-atpt)

(defun py-class-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-class no-delimiters (interactive-p)))

(defalias 'length-of-py-class-atpt 'py-class-length-atpt)

(defun py-class-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-class no-delimiters (interactive-p)))

(defalias 'copy-py-class-atpt 'py-class-copy-atpt)

(defun py-class-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-class no-delimiters (interactive-p)))

(defalias 'delete-py-class-in-region 'py-class-delete-in-region)

(defun py-class-delete-in-region (beg end)
  "Deletes py-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-class beg end (interactive-p)))


(defun comment-py-class-atpt (&optional no-delimiters)
  "Comments py-class at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-class no-delimiters (interactive-p)))

(defalias 'mark-py-class-atpt 'py-class-mark-atpt)

(defun py-class-mark-atpt (&optional no-delimiters)
  "Marks py-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-class))

(defalias 'hide-py-class-atpt 'py-class-hide-atpt)

(defun py-class-hide-atpt ()
  "Hides py-class at point. "
  (interactive)
  (ar-th-hide 'py-class))

(defalias 'show-py-class-atpt 'py-class-show-atpt)

(defun py-class-show-atpt ()
  "Shows hidden py-class at point. "
  (interactive)
  (ar-th-show 'py-class))

(defalias 'hide-show-py-class-atpt 'py-class-hide-show-atpt)

(defun py-class-hide-show-atpt ()
  "Alternatively hides or shows py-class at point. "
  (interactive)
  (ar-th-hide-show 'py-class))

(defalias 'highlight-py-class-atpt-mode 'py-class-highlight-atpt-mode)

(defun py-class-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-class-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-class no-delimiters (interactive-p)))

(defalias 'kill-py-class-atpt 'py-class-kill-atpt)

(defun py-class-kill-atpt (&optional no-delimiters)
  "Kills py-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-class no-delimiters (interactive-p)))

(defalias 'separate-py-class-atpt 'py-class-separate-atpt)

(defun py-class-separate-atpt (&optional no-delimiters)
  "Separates py-class at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-class no-delimiters (interactive-p)))

(defalias 'forward-py-class-atpt 'py-class-forward-atpt)

(defun py-class-forward-atpt (&optional arg)
  "Moves forward over py-class at point if any, does nothing otherwise.
Returns end position of py-class "
  (interactive "p")
  (ar-th-forward 'py-class arg (interactive-p)))

(defalias 'backward-py-class-atpt 'py-class-backward-atpt)

(defun py-class-backward-atpt (&optional arg)
  "Moves backward over py-class before point if any, does nothing otherwise.
Returns beginning position of py-class "
  (interactive "p")
  (ar-th-backward 'py-class arg (interactive-p)))

(defalias 'check-py-class-atpt 'py-class-check-atpt)

(defun py-class-check-atpt ()
  "Return t if a py-class at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-class-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-class-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-clause-atpt (&optional arg no-delimiters)
  "Returns py-clause at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-clause arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-clause-atpt 'py-clause-bounds-atpt)

(defun py-clause-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-clause if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-clause no-delimiters (interactive-p)))


(defun py-clause-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-clause at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-clause no-delimiters (interactive-p)))


(defun py-clause-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-clause no-delimiters (interactive-p)))

(defalias 'beginning-of-py-clause-atpt 'py-clause-beginning-atpt)

(defun py-clause-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-clause no-delimiters (interactive-p)))

(defalias 'end-of-py-clause-atpt 'py-clause-end-atpt)

(defun py-clause-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-clause no-delimiters (interactive-p)))

(defalias 'length-of-py-clause-atpt 'py-clause-length-atpt)

(defun py-clause-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-clause no-delimiters (interactive-p)))

(defalias 'copy-py-clause-atpt 'py-clause-copy-atpt)

(defun py-clause-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-clause at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-clause no-delimiters (interactive-p)))

(defalias 'delete-py-clause-in-region 'py-clause-delete-in-region)

(defun py-clause-delete-in-region (beg end)
  "Deletes py-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-clause beg end (interactive-p)))


(defun comment-py-clause-atpt (&optional no-delimiters)
  "Comments py-clause at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-clause no-delimiters (interactive-p)))

(defalias 'mark-py-clause-atpt 'py-clause-mark-atpt)

(defun py-clause-mark-atpt (&optional no-delimiters)
  "Marks py-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-clause))

(defalias 'hide-py-clause-atpt 'py-clause-hide-atpt)

(defun py-clause-hide-atpt ()
  "Hides py-clause at point. "
  (interactive)
  (ar-th-hide 'py-clause))

(defalias 'show-py-clause-atpt 'py-clause-show-atpt)

(defun py-clause-show-atpt ()
  "Shows hidden py-clause at point. "
  (interactive)
  (ar-th-show 'py-clause))

(defalias 'hide-show-py-clause-atpt 'py-clause-hide-show-atpt)

(defun py-clause-hide-show-atpt ()
  "Alternatively hides or shows py-clause at point. "
  (interactive)
  (ar-th-hide-show 'py-clause))

(defalias 'highlight-py-clause-atpt-mode 'py-clause-highlight-atpt-mode)

(defun py-clause-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-clause-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-clause no-delimiters (interactive-p)))

(defalias 'kill-py-clause-atpt 'py-clause-kill-atpt)

(defun py-clause-kill-atpt (&optional no-delimiters)
  "Kills py-clause at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-clause no-delimiters (interactive-p)))

(defalias 'separate-py-clause-atpt 'py-clause-separate-atpt)

(defun py-clause-separate-atpt (&optional no-delimiters)
  "Separates py-clause at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-clause no-delimiters (interactive-p)))

(defalias 'forward-py-clause-atpt 'py-clause-forward-atpt)

(defun py-clause-forward-atpt (&optional arg)
  "Moves forward over py-clause at point if any, does nothing otherwise.
Returns end position of py-clause "
  (interactive "p")
  (ar-th-forward 'py-clause arg (interactive-p)))

(defalias 'backward-py-clause-atpt 'py-clause-backward-atpt)

(defun py-clause-backward-atpt (&optional arg)
  "Moves backward over py-clause before point if any, does nothing otherwise.
Returns beginning position of py-clause "
  (interactive "p")
  (ar-th-backward 'py-clause arg (interactive-p)))

(defalias 'check-py-clause-atpt 'py-clause-check-atpt)

(defun py-clause-check-atpt ()
  "Return t if a py-clause at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-clause-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-clause-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-def-or-class-atpt (&optional arg no-delimiters)
  "Returns py-def-or-class at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-def-or-class arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-def-or-class-atpt 'py-def-or-class-bounds-atpt)

(defun py-def-or-class-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-def-or-class if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-def-or-class no-delimiters (interactive-p)))


(defun py-def-or-class-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-def-or-class at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-def-or-class no-delimiters (interactive-p)))


(defun py-def-or-class-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-def-or-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'beginning-of-py-def-or-class-atpt 'py-def-or-class-beginning-atpt)

(defun py-def-or-class-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-def-or-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'end-of-py-def-or-class-atpt 'py-def-or-class-end-atpt)

(defun py-def-or-class-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-def-or-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'length-of-py-def-or-class-atpt 'py-def-or-class-length-atpt)

(defun py-def-or-class-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-def-or-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'copy-py-def-or-class-atpt 'py-def-or-class-copy-atpt)

(defun py-def-or-class-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-def-or-class at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'delete-py-def-or-class-in-region 'py-def-or-class-delete-in-region)

(defun py-def-or-class-delete-in-region (beg end)
  "Deletes py-def-or-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-def-or-class beg end (interactive-p)))


(defun comment-py-def-or-class-atpt (&optional no-delimiters)
  "Comments py-def-or-class at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'mark-py-def-or-class-atpt 'py-def-or-class-mark-atpt)

(defun py-def-or-class-mark-atpt (&optional no-delimiters)
  "Marks py-def-or-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-def-or-class))

(defalias 'hide-py-def-or-class-atpt 'py-def-or-class-hide-atpt)

(defun py-def-or-class-hide-atpt ()
  "Hides py-def-or-class at point. "
  (interactive)
  (ar-th-hide 'py-def-or-class))

(defalias 'show-py-def-or-class-atpt 'py-def-or-class-show-atpt)

(defun py-def-or-class-show-atpt ()
  "Shows hidden py-def-or-class at point. "
  (interactive)
  (ar-th-show 'py-def-or-class))

(defalias 'hide-show-py-def-or-class-atpt 'py-def-or-class-hide-show-atpt)

(defun py-def-or-class-hide-show-atpt ()
  "Alternatively hides or shows py-def-or-class at point. "
  (interactive)
  (ar-th-hide-show 'py-def-or-class))

(defalias 'highlight-py-def-or-class-atpt-mode 'py-def-or-class-highlight-atpt-mode)

(defun py-def-or-class-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-def-or-class-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'kill-py-def-or-class-atpt 'py-def-or-class-kill-atpt)

(defun py-def-or-class-kill-atpt (&optional no-delimiters)
  "Kills py-def-or-class at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'separate-py-def-or-class-atpt 'py-def-or-class-separate-atpt)

(defun py-def-or-class-separate-atpt (&optional no-delimiters)
  "Separates py-def-or-class at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-def-or-class no-delimiters (interactive-p)))

(defalias 'forward-py-def-or-class-atpt 'py-def-or-class-forward-atpt)

(defun py-def-or-class-forward-atpt (&optional arg)
  "Moves forward over py-def-or-class at point if any, does nothing otherwise.
Returns end position of py-def-or-class "
  (interactive "p")
  (ar-th-forward 'py-def-or-class arg (interactive-p)))

(defalias 'backward-py-def-or-class-atpt 'py-def-or-class-backward-atpt)

(defun py-def-or-class-backward-atpt (&optional arg)
  "Moves backward over py-def-or-class before point if any, does nothing otherwise.
Returns beginning position of py-def-or-class "
  (interactive "p")
  (ar-th-backward 'py-def-or-class arg (interactive-p)))

(defalias 'check-py-def-or-class-atpt 'py-def-or-class-check-atpt)

(defun py-def-or-class-check-atpt ()
  "Return t if a py-def-or-class at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-def-or-class-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-def-or-class-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-def-atpt (&optional arg no-delimiters)
  "Returns py-def at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-def arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-def-atpt 'py-def-bounds-atpt)

(defun py-def-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-def if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-def no-delimiters (interactive-p)))


(defun py-def-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-def at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-def no-delimiters (interactive-p)))


(defun py-def-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-def at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-def no-delimiters (interactive-p)))

(defalias 'beginning-of-py-def-atpt 'py-def-beginning-atpt)

(defun py-def-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-def at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-def no-delimiters (interactive-p)))

(defalias 'end-of-py-def-atpt 'py-def-end-atpt)

(defun py-def-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-def at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-def no-delimiters (interactive-p)))

(defalias 'length-of-py-def-atpt 'py-def-length-atpt)

(defun py-def-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-def at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-def no-delimiters (interactive-p)))

(defalias 'copy-py-def-atpt 'py-def-copy-atpt)

(defun py-def-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-def at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-def no-delimiters (interactive-p)))

(defalias 'delete-py-def-in-region 'py-def-delete-in-region)

(defun py-def-delete-in-region (beg end)
  "Deletes py-def at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-def beg end (interactive-p)))


(defun comment-py-def-atpt (&optional no-delimiters)
  "Comments py-def at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-def no-delimiters (interactive-p)))

(defalias 'mark-py-def-atpt 'py-def-mark-atpt)

(defun py-def-mark-atpt (&optional no-delimiters)
  "Marks py-def at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-def))

(defalias 'hide-py-def-atpt 'py-def-hide-atpt)

(defun py-def-hide-atpt ()
  "Hides py-def at point. "
  (interactive)
  (ar-th-hide 'py-def))

(defalias 'show-py-def-atpt 'py-def-show-atpt)

(defun py-def-show-atpt ()
  "Shows hidden py-def at point. "
  (interactive)
  (ar-th-show 'py-def))

(defalias 'hide-show-py-def-atpt 'py-def-hide-show-atpt)

(defun py-def-hide-show-atpt ()
  "Alternatively hides or shows py-def at point. "
  (interactive)
  (ar-th-hide-show 'py-def))

(defalias 'highlight-py-def-atpt-mode 'py-def-highlight-atpt-mode)

(defun py-def-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-def-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-def no-delimiters (interactive-p)))

(defalias 'kill-py-def-atpt 'py-def-kill-atpt)

(defun py-def-kill-atpt (&optional no-delimiters)
  "Kills py-def at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-def no-delimiters (interactive-p)))

(defalias 'separate-py-def-atpt 'py-def-separate-atpt)

(defun py-def-separate-atpt (&optional no-delimiters)
  "Separates py-def at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-def no-delimiters (interactive-p)))

(defalias 'forward-py-def-atpt 'py-def-forward-atpt)

(defun py-def-forward-atpt (&optional arg)
  "Moves forward over py-def at point if any, does nothing otherwise.
Returns end position of py-def "
  (interactive "p")
  (ar-th-forward 'py-def arg (interactive-p)))

(defalias 'backward-py-def-atpt 'py-def-backward-atpt)

(defun py-def-backward-atpt (&optional arg)
  "Moves backward over py-def before point if any, does nothing otherwise.
Returns beginning position of py-def "
  (interactive "p")
  (ar-th-backward 'py-def arg (interactive-p)))

(defalias 'check-py-def-atpt 'py-def-check-atpt)

(defun py-def-check-atpt ()
  "Return t if a py-def at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-def-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-def-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-expression-atpt (&optional arg no-delimiters)
  "Returns py-expression at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-expression arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-expression-atpt 'py-expression-bounds-atpt)

(defun py-expression-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-expression if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-expression no-delimiters (interactive-p)))


(defun py-expression-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-expression at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-expression no-delimiters (interactive-p)))


(defun py-expression-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-expression no-delimiters (interactive-p)))

(defalias 'beginning-of-py-expression-atpt 'py-expression-beginning-atpt)

(defun py-expression-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-expression no-delimiters (interactive-p)))

(defalias 'end-of-py-expression-atpt 'py-expression-end-atpt)

(defun py-expression-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-expression no-delimiters (interactive-p)))

(defalias 'length-of-py-expression-atpt 'py-expression-length-atpt)

(defun py-expression-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-expression no-delimiters (interactive-p)))

(defalias 'copy-py-expression-atpt 'py-expression-copy-atpt)

(defun py-expression-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-expression no-delimiters (interactive-p)))

(defalias 'delete-py-expression-in-region 'py-expression-delete-in-region)

(defun py-expression-delete-in-region (beg end)
  "Deletes py-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-expression beg end (interactive-p)))


(defun comment-py-expression-atpt (&optional no-delimiters)
  "Comments py-expression at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-expression no-delimiters (interactive-p)))

(defalias 'mark-py-expression-atpt 'py-expression-mark-atpt)

(defun py-expression-mark-atpt (&optional no-delimiters)
  "Marks py-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-expression))

(defalias 'hide-py-expression-atpt 'py-expression-hide-atpt)

(defun py-expression-hide-atpt ()
  "Hides py-expression at point. "
  (interactive)
  (ar-th-hide 'py-expression))

(defalias 'show-py-expression-atpt 'py-expression-show-atpt)

(defun py-expression-show-atpt ()
  "Shows hidden py-expression at point. "
  (interactive)
  (ar-th-show 'py-expression))

(defalias 'hide-show-py-expression-atpt 'py-expression-hide-show-atpt)

(defun py-expression-hide-show-atpt ()
  "Alternatively hides or shows py-expression at point. "
  (interactive)
  (ar-th-hide-show 'py-expression))

(defalias 'highlight-py-expression-atpt-mode 'py-expression-highlight-atpt-mode)

(defun py-expression-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-expression-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-expression no-delimiters (interactive-p)))

(defalias 'kill-py-expression-atpt 'py-expression-kill-atpt)

(defun py-expression-kill-atpt (&optional no-delimiters)
  "Kills py-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-expression no-delimiters (interactive-p)))

(defalias 'separate-py-expression-atpt 'py-expression-separate-atpt)

(defun py-expression-separate-atpt (&optional no-delimiters)
  "Separates py-expression at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-expression no-delimiters (interactive-p)))

(defalias 'forward-py-expression-atpt 'py-expression-forward-atpt)

(defun py-expression-forward-atpt (&optional arg)
  "Moves forward over py-expression at point if any, does nothing otherwise.
Returns end position of py-expression "
  (interactive "p")
  (ar-th-forward 'py-expression arg (interactive-p)))

(defalias 'backward-py-expression-atpt 'py-expression-backward-atpt)

(defun py-expression-backward-atpt (&optional arg)
  "Moves backward over py-expression before point if any, does nothing otherwise.
Returns beginning position of py-expression "
  (interactive "p")
  (ar-th-backward 'py-expression arg (interactive-p)))

(defalias 'check-py-expression-atpt 'py-expression-check-atpt)

(defun py-expression-check-atpt ()
  "Return t if a py-expression at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-expression-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-expression-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-partial-expression-atpt (&optional arg no-delimiters)
  "Returns py-partial-expression at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-partial-expression arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-partial-expression-atpt 'py-partial-expression-bounds-atpt)

(defun py-partial-expression-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-partial-expression if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-partial-expression no-delimiters (interactive-p)))


(defun py-partial-expression-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-partial-expression at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-partial-expression no-delimiters (interactive-p)))


(defun py-partial-expression-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-partial-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'beginning-of-py-partial-expression-atpt 'py-partial-expression-beginning-atpt)

(defun py-partial-expression-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-partial-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'end-of-py-partial-expression-atpt 'py-partial-expression-end-atpt)

(defun py-partial-expression-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-partial-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'length-of-py-partial-expression-atpt 'py-partial-expression-length-atpt)

(defun py-partial-expression-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-partial-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'copy-py-partial-expression-atpt 'py-partial-expression-copy-atpt)

(defun py-partial-expression-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-partial-expression at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'delete-py-partial-expression-in-region 'py-partial-expression-delete-in-region)

(defun py-partial-expression-delete-in-region (beg end)
  "Deletes py-partial-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-partial-expression beg end (interactive-p)))


(defun comment-py-partial-expression-atpt (&optional no-delimiters)
  "Comments py-partial-expression at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'mark-py-partial-expression-atpt 'py-partial-expression-mark-atpt)

(defun py-partial-expression-mark-atpt (&optional no-delimiters)
  "Marks py-partial-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-partial-expression))

(defalias 'hide-py-partial-expression-atpt 'py-partial-expression-hide-atpt)

(defun py-partial-expression-hide-atpt ()
  "Hides py-partial-expression at point. "
  (interactive)
  (ar-th-hide 'py-partial-expression))

(defalias 'show-py-partial-expression-atpt 'py-partial-expression-show-atpt)

(defun py-partial-expression-show-atpt ()
  "Shows hidden py-partial-expression at point. "
  (interactive)
  (ar-th-show 'py-partial-expression))

(defalias 'hide-show-py-partial-expression-atpt 'py-partial-expression-hide-show-atpt)

(defun py-partial-expression-hide-show-atpt ()
  "Alternatively hides or shows py-partial-expression at point. "
  (interactive)
  (ar-th-hide-show 'py-partial-expression))

(defalias 'highlight-py-partial-expression-atpt-mode 'py-partial-expression-highlight-atpt-mode)

(defun py-partial-expression-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-partial-expression-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'kill-py-partial-expression-atpt 'py-partial-expression-kill-atpt)

(defun py-partial-expression-kill-atpt (&optional no-delimiters)
  "Kills py-partial-expression at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'separate-py-partial-expression-atpt 'py-partial-expression-separate-atpt)

(defun py-partial-expression-separate-atpt (&optional no-delimiters)
  "Separates py-partial-expression at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-partial-expression no-delimiters (interactive-p)))

(defalias 'forward-py-partial-expression-atpt 'py-partial-expression-forward-atpt)

(defun py-partial-expression-forward-atpt (&optional arg)
  "Moves forward over py-partial-expression at point if any, does nothing otherwise.
Returns end position of py-partial-expression "
  (interactive "p")
  (ar-th-forward 'py-partial-expression arg (interactive-p)))

(defalias 'backward-py-partial-expression-atpt 'py-partial-expression-backward-atpt)

(defun py-partial-expression-backward-atpt (&optional arg)
  "Moves backward over py-partial-expression before point if any, does nothing otherwise.
Returns beginning position of py-partial-expression "
  (interactive "p")
  (ar-th-backward 'py-partial-expression arg (interactive-p)))

(defalias 'check-py-partial-expression-atpt 'py-partial-expression-check-atpt)

(defun py-partial-expression-check-atpt ()
  "Return t if a py-partial-expression at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-partial-expression-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-partial-expression-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-statement-atpt (&optional arg no-delimiters)
  "Returns py-statement at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-statement arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-statement-atpt 'py-statement-bounds-atpt)

(defun py-statement-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-statement if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-statement no-delimiters (interactive-p)))


(defun py-statement-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-statement at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-statement no-delimiters (interactive-p)))


(defun py-statement-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-statement at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-statement no-delimiters (interactive-p)))

(defalias 'beginning-of-py-statement-atpt 'py-statement-beginning-atpt)

(defun py-statement-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-statement at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-statement no-delimiters (interactive-p)))

(defalias 'end-of-py-statement-atpt 'py-statement-end-atpt)

(defun py-statement-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-statement at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-statement no-delimiters (interactive-p)))

(defalias 'length-of-py-statement-atpt 'py-statement-length-atpt)

(defun py-statement-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-statement at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-statement no-delimiters (interactive-p)))

(defalias 'copy-py-statement-atpt 'py-statement-copy-atpt)

(defun py-statement-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-statement at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-statement no-delimiters (interactive-p)))

(defalias 'delete-py-statement-in-region 'py-statement-delete-in-region)

(defun py-statement-delete-in-region (beg end)
  "Deletes py-statement at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-statement beg end (interactive-p)))


(defun comment-py-statement-atpt (&optional no-delimiters)
  "Comments py-statement at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-statement no-delimiters (interactive-p)))

(defalias 'mark-py-statement-atpt 'py-statement-mark-atpt)

(defun py-statement-mark-atpt (&optional no-delimiters)
  "Marks py-statement at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-statement))

(defalias 'hide-py-statement-atpt 'py-statement-hide-atpt)

(defun py-statement-hide-atpt ()
  "Hides py-statement at point. "
  (interactive)
  (ar-th-hide 'py-statement))

(defalias 'show-py-statement-atpt 'py-statement-show-atpt)

(defun py-statement-show-atpt ()
  "Shows hidden py-statement at point. "
  (interactive)
  (ar-th-show 'py-statement))

(defalias 'hide-show-py-statement-atpt 'py-statement-hide-show-atpt)

(defun py-statement-hide-show-atpt ()
  "Alternatively hides or shows py-statement at point. "
  (interactive)
  (ar-th-hide-show 'py-statement))

(defalias 'highlight-py-statement-atpt-mode 'py-statement-highlight-atpt-mode)

(defun py-statement-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-statement-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-statement no-delimiters (interactive-p)))

(defalias 'kill-py-statement-atpt 'py-statement-kill-atpt)

(defun py-statement-kill-atpt (&optional no-delimiters)
  "Kills py-statement at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-statement no-delimiters (interactive-p)))

(defalias 'separate-py-statement-atpt 'py-statement-separate-atpt)

(defun py-statement-separate-atpt (&optional no-delimiters)
  "Separates py-statement at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-statement no-delimiters (interactive-p)))

(defalias 'forward-py-statement-atpt 'py-statement-forward-atpt)

(defun py-statement-forward-atpt (&optional arg)
  "Moves forward over py-statement at point if any, does nothing otherwise.
Returns end position of py-statement "
  (interactive "p")
  (ar-th-forward 'py-statement arg (interactive-p)))

(defalias 'backward-py-statement-atpt 'py-statement-backward-atpt)

(defun py-statement-backward-atpt (&optional arg)
  "Moves backward over py-statement before point if any, does nothing otherwise.
Returns beginning position of py-statement "
  (interactive "p")
  (ar-th-backward 'py-statement arg (interactive-p)))

(defalias 'check-py-statement-atpt 'py-statement-check-atpt)

(defun py-statement-check-atpt ()
  "Return t if a py-statement at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-statement-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-statement-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun py-string-atpt (&optional arg no-delimiters)
  "Returns py-string at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'py-string arg no-delimiters (interactive-p)))

(defalias 'bounds-of-py-string-atpt 'py-string-bounds-atpt)

(defun py-string-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of py-string if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'py-string no-delimiters (interactive-p)))


(defun py-string-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position py-string at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'py-string no-delimiters (interactive-p)))


(defun py-string-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of py-string at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'py-string no-delimiters (interactive-p)))

(defalias 'beginning-of-py-string-atpt 'py-string-beginning-atpt)

(defun py-string-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class py-string at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'py-string no-delimiters (interactive-p)))

(defalias 'end-of-py-string-atpt 'py-string-end-atpt)

(defun py-string-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class py-string at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'py-string no-delimiters (interactive-p)))

(defalias 'length-of-py-string-atpt 'py-string-length-atpt)

(defun py-string-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class py-string at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'py-string no-delimiters (interactive-p)))

(defalias 'copy-py-string-atpt 'py-string-copy-atpt)

(defun py-string-copy-atpt (&optional no-delimiters)
  "Returns a copy of py-string at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'py-string no-delimiters (interactive-p)))

(defalias 'delete-py-string-in-region 'py-string-delete-in-region)

(defun py-string-delete-in-region (beg end)
  "Deletes py-string at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'py-string beg end (interactive-p)))


(defun comment-py-string-atpt (&optional no-delimiters)
  "Comments py-string at point if any. "
  (interactive "*p")
  (ar-th-comment 'py-string no-delimiters (interactive-p)))

(defalias 'mark-py-string-atpt 'py-string-mark-atpt)

(defun py-string-mark-atpt (&optional no-delimiters)
  "Marks py-string at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'py-string))

(defalias 'hide-py-string-atpt 'py-string-hide-atpt)

(defun py-string-hide-atpt ()
  "Hides py-string at point. "
  (interactive)
  (ar-th-hide 'py-string))

(defalias 'show-py-string-atpt 'py-string-show-atpt)

(defun py-string-show-atpt ()
  "Shows hidden py-string at point. "
  (interactive)
  (ar-th-show 'py-string))

(defalias 'hide-show-py-string-atpt 'py-string-hide-show-atpt)

(defun py-string-hide-show-atpt ()
  "Alternatively hides or shows py-string at point. "
  (interactive)
  (ar-th-hide-show 'py-string))

(defalias 'highlight-py-string-atpt-mode 'py-string-highlight-atpt-mode)

(defun py-string-highlight-atpt-mode (&optional no-delimiters)
  "Toggles py-string-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'py-string no-delimiters (interactive-p)))

(defalias 'kill-py-string-atpt 'py-string-kill-atpt)

(defun py-string-kill-atpt (&optional no-delimiters)
  "Kills py-string at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'py-string no-delimiters (interactive-p)))

(defalias 'separate-py-string-atpt 'py-string-separate-atpt)

(defun py-string-separate-atpt (&optional no-delimiters)
  "Separates py-string at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'py-string no-delimiters (interactive-p)))

(defalias 'forward-py-string-atpt 'py-string-forward-atpt)

(defun py-string-forward-atpt (&optional arg)
  "Moves forward over py-string at point if any, does nothing otherwise.
Returns end position of py-string "
  (interactive "p")
  (ar-th-forward 'py-string arg (interactive-p)))

(defalias 'backward-py-string-atpt 'py-string-backward-atpt)

(defun py-string-backward-atpt (&optional arg)
  "Moves backward over py-string before point if any, does nothing otherwise.
Returns beginning position of py-string "
  (interactive "p")
  (ar-th-backward 'py-string arg (interactive-p)))

(defalias 'check-py-string-atpt 'py-string-check-atpt)

(defun py-string-check-atpt ()
  "Return t if a py-string at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "py-string-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "py-string-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; python-list end

;; tqs start


(defun triplequoted-atpt (&optional arg no-delimiters)
  "Returns triplequoted at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'triplequoted arg no-delimiters (interactive-p)))

(defalias 'bounds-of-triplequoted-atpt 'triplequoted-bounds-atpt)

(defun triplequoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of triplequoted if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'triplequoted no-delimiters (interactive-p)))


(defun triplequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position triplequoted at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'triplequoted no-delimiters (interactive-p)))


(defun triplequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of triplequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'triplequoted no-delimiters (interactive-p)))

(defalias 'beginning-of-triplequoted-atpt 'triplequoted-beginning-atpt)

(defun triplequoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class triplequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'triplequoted no-delimiters (interactive-p)))

(defalias 'end-of-triplequoted-atpt 'triplequoted-end-atpt)

(defun triplequoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class triplequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'triplequoted no-delimiters (interactive-p)))

(defalias 'length-of-triplequoted-atpt 'triplequoted-length-atpt)

(defun triplequoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class triplequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'triplequoted no-delimiters (interactive-p)))

(defalias 'copy-triplequoted-atpt 'triplequoted-copy-atpt)

(defun triplequoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of triplequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'triplequoted no-delimiters (interactive-p)))

(defalias 'delete-triplequoted-in-region 'triplequoted-delete-in-region)

(defun triplequoted-delete-in-region (beg end)
  "Deletes triplequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'triplequoted beg end (interactive-p)))


(defun comment-triplequoted-atpt (&optional no-delimiters)
  "Comments triplequoted at point if any. "
  (interactive "*p")
  (ar-th-comment 'triplequoted no-delimiters (interactive-p)))

(defalias 'mark-triplequoted-atpt 'triplequoted-mark-atpt)

(defun triplequoted-mark-atpt (&optional no-delimiters)
  "Marks triplequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'triplequoted))

(defalias 'hide-triplequoted-atpt 'triplequoted-hide-atpt)

(defun triplequoted-hide-atpt ()
  "Hides triplequoted at point. "
  (interactive)
  (ar-th-hide 'triplequoted))

(defalias 'show-triplequoted-atpt 'triplequoted-show-atpt)

(defun triplequoted-show-atpt ()
  "Shows hidden triplequoted at point. "
  (interactive)
  (ar-th-show 'triplequoted))

(defalias 'hide-show-triplequoted-atpt 'triplequoted-hide-show-atpt)

(defun triplequoted-hide-show-atpt ()
  "Alternatively hides or shows triplequoted at point. "
  (interactive)
  (ar-th-hide-show 'triplequoted))

(defalias 'highlight-triplequoted-atpt-mode 'triplequoted-highlight-atpt-mode)

(defun triplequoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles triplequoted-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'triplequoted no-delimiters (interactive-p)))

(defalias 'kill-triplequoted-atpt 'triplequoted-kill-atpt)

(defun triplequoted-kill-atpt (&optional no-delimiters)
  "Kills triplequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'triplequoted no-delimiters (interactive-p)))

(defalias 'separate-triplequoted-atpt 'triplequoted-separate-atpt)

(defun triplequoted-separate-atpt (&optional no-delimiters)
  "Separates triplequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'triplequoted no-delimiters (interactive-p)))

(defalias 'forward-triplequoted-atpt 'triplequoted-forward-atpt)

(defun triplequoted-forward-atpt (&optional arg)
  "Moves forward over triplequoted at point if any, does nothing otherwise.
Returns end position of triplequoted "
  (interactive "p")
  (ar-th-forward 'triplequoted arg (interactive-p)))

(defalias 'backward-triplequoted-atpt 'triplequoted-backward-atpt)

(defun triplequoted-backward-atpt (&optional arg)
  "Moves backward over triplequoted before point if any, does nothing otherwise.
Returns beginning position of triplequoted "
  (interactive "p")
  (ar-th-backward 'triplequoted arg (interactive-p)))

(defalias 'check-triplequoted-atpt 'triplequoted-check-atpt)

(defun triplequoted-check-atpt ()
  "Return t if a triplequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "triplequoted-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "triplequoted-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun triplequoted-dq-atpt (&optional arg no-delimiters)
  "Returns triplequoted-dq at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'triplequoted-dq arg no-delimiters (interactive-p)))

(defalias 'bounds-of-triplequoted-dq-atpt 'triplequoted-dq-bounds-atpt)

(defun triplequoted-dq-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of triplequoted-dq if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'triplequoted-dq no-delimiters (interactive-p)))


(defun triplequoted-dq-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position triplequoted-dq at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'triplequoted-dq no-delimiters (interactive-p)))


(defun triplequoted-dq-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of triplequoted-dq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'beginning-of-triplequoted-dq-atpt 'triplequoted-dq-beginning-atpt)

(defun triplequoted-dq-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class triplequoted-dq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'end-of-triplequoted-dq-atpt 'triplequoted-dq-end-atpt)

(defun triplequoted-dq-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class triplequoted-dq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'length-of-triplequoted-dq-atpt 'triplequoted-dq-length-atpt)

(defun triplequoted-dq-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class triplequoted-dq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'copy-triplequoted-dq-atpt 'triplequoted-dq-copy-atpt)

(defun triplequoted-dq-copy-atpt (&optional no-delimiters)
  "Returns a copy of triplequoted-dq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'delete-triplequoted-dq-in-region 'triplequoted-dq-delete-in-region)

(defun triplequoted-dq-delete-in-region (beg end)
  "Deletes triplequoted-dq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'triplequoted-dq beg end (interactive-p)))


(defun comment-triplequoted-dq-atpt (&optional no-delimiters)
  "Comments triplequoted-dq at point if any. "
  (interactive "*p")
  (ar-th-comment 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'mark-triplequoted-dq-atpt 'triplequoted-dq-mark-atpt)

(defun triplequoted-dq-mark-atpt (&optional no-delimiters)
  "Marks triplequoted-dq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'triplequoted-dq))

(defalias 'hide-triplequoted-dq-atpt 'triplequoted-dq-hide-atpt)

(defun triplequoted-dq-hide-atpt ()
  "Hides triplequoted-dq at point. "
  (interactive)
  (ar-th-hide 'triplequoted-dq))

(defalias 'show-triplequoted-dq-atpt 'triplequoted-dq-show-atpt)

(defun triplequoted-dq-show-atpt ()
  "Shows hidden triplequoted-dq at point. "
  (interactive)
  (ar-th-show 'triplequoted-dq))

(defalias 'hide-show-triplequoted-dq-atpt 'triplequoted-dq-hide-show-atpt)

(defun triplequoted-dq-hide-show-atpt ()
  "Alternatively hides or shows triplequoted-dq at point. "
  (interactive)
  (ar-th-hide-show 'triplequoted-dq))

(defalias 'highlight-triplequoted-dq-atpt-mode 'triplequoted-dq-highlight-atpt-mode)

(defun triplequoted-dq-highlight-atpt-mode (&optional no-delimiters)
  "Toggles triplequoted-dq-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'kill-triplequoted-dq-atpt 'triplequoted-dq-kill-atpt)

(defun triplequoted-dq-kill-atpt (&optional no-delimiters)
  "Kills triplequoted-dq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'separate-triplequoted-dq-atpt 'triplequoted-dq-separate-atpt)

(defun triplequoted-dq-separate-atpt (&optional no-delimiters)
  "Separates triplequoted-dq at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'triplequoted-dq no-delimiters (interactive-p)))

(defalias 'forward-triplequoted-dq-atpt 'triplequoted-dq-forward-atpt)

(defun triplequoted-dq-forward-atpt (&optional arg)
  "Moves forward over triplequoted-dq at point if any, does nothing otherwise.
Returns end position of triplequoted-dq "
  (interactive "p")
  (ar-th-forward 'triplequoted-dq arg (interactive-p)))

(defalias 'backward-triplequoted-dq-atpt 'triplequoted-dq-backward-atpt)

(defun triplequoted-dq-backward-atpt (&optional arg)
  "Moves backward over triplequoted-dq before point if any, does nothing otherwise.
Returns beginning position of triplequoted-dq "
  (interactive "p")
  (ar-th-backward 'triplequoted-dq arg (interactive-p)))

(defalias 'check-triplequoted-dq-atpt 'triplequoted-dq-check-atpt)

(defun triplequoted-dq-check-atpt ()
  "Return t if a triplequoted-dq at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "triplequoted-dq-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "triplequoted-dq-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))


(defun triplequoted-sq-atpt (&optional arg no-delimiters)
  "Returns triplequoted-sq at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'triplequoted-sq arg no-delimiters (interactive-p)))

(defalias 'bounds-of-triplequoted-sq-atpt 'triplequoted-sq-bounds-atpt)

(defun triplequoted-sq-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of triplequoted-sq if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'triplequoted-sq no-delimiters (interactive-p)))


(defun triplequoted-sq-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position triplequoted-sq at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'triplequoted-sq no-delimiters (interactive-p)))


(defun triplequoted-sq-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of triplequoted-sq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-end 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'beginning-of-triplequoted-sq-atpt 'triplequoted-sq-beginning-atpt)

(defun triplequoted-sq-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class triplequoted-sq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'end-of-triplequoted-sq-atpt 'triplequoted-sq-end-atpt)

(defun triplequoted-sq-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class triplequoted-sq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'length-of-triplequoted-sq-atpt 'triplequoted-sq-length-atpt)

(defun triplequoted-sq-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class triplequoted-sq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-length 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'copy-triplequoted-sq-atpt 'triplequoted-sq-copy-atpt)

(defun triplequoted-sq-copy-atpt (&optional no-delimiters)
  "Returns a copy of triplequoted-sq at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'delete-triplequoted-sq-in-region 'triplequoted-sq-delete-in-region)

(defun triplequoted-sq-delete-in-region (beg end)
  "Deletes triplequoted-sq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'triplequoted-sq beg end (interactive-p)))


(defun comment-triplequoted-sq-atpt (&optional no-delimiters)
  "Comments triplequoted-sq at point if any. "
  (interactive "*p")
  (ar-th-comment 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'mark-triplequoted-sq-atpt 'triplequoted-sq-mark-atpt)

(defun triplequoted-sq-mark-atpt (&optional no-delimiters)
  "Marks triplequoted-sq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'triplequoted-sq))

(defalias 'hide-triplequoted-sq-atpt 'triplequoted-sq-hide-atpt)

(defun triplequoted-sq-hide-atpt ()
  "Hides triplequoted-sq at point. "
  (interactive)
  (ar-th-hide 'triplequoted-sq))

(defalias 'show-triplequoted-sq-atpt 'triplequoted-sq-show-atpt)

(defun triplequoted-sq-show-atpt ()
  "Shows hidden triplequoted-sq at point. "
  (interactive)
  (ar-th-show 'triplequoted-sq))

(defalias 'hide-show-triplequoted-sq-atpt 'triplequoted-sq-hide-show-atpt)

(defun triplequoted-sq-hide-show-atpt ()
  "Alternatively hides or shows triplequoted-sq at point. "
  (interactive)
  (ar-th-hide-show 'triplequoted-sq))

(defalias 'highlight-triplequoted-sq-atpt-mode 'triplequoted-sq-highlight-atpt-mode)

(defun triplequoted-sq-highlight-atpt-mode (&optional no-delimiters)
  "Toggles triplequoted-sq-highlight-atpt-mode .

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-highlight 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'kill-triplequoted-sq-atpt 'triplequoted-sq-kill-atpt)

(defun triplequoted-sq-kill-atpt (&optional no-delimiters)
  "Kills triplequoted-sq at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'separate-triplequoted-sq-atpt 'triplequoted-sq-separate-atpt)

(defun triplequoted-sq-separate-atpt (&optional no-delimiters)
  "Separates triplequoted-sq at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-separate 'triplequoted-sq no-delimiters (interactive-p)))

(defalias 'forward-triplequoted-sq-atpt 'triplequoted-sq-forward-atpt)

(defun triplequoted-sq-forward-atpt (&optional arg)
  "Moves forward over triplequoted-sq at point if any, does nothing otherwise.
Returns end position of triplequoted-sq "
  (interactive "p")
  (ar-th-forward 'triplequoted-sq arg (interactive-p)))

(defalias 'backward-triplequoted-sq-atpt 'triplequoted-sq-backward-atpt)

(defun triplequoted-sq-backward-atpt (&optional arg)
  "Moves backward over triplequoted-sq before point if any, does nothing otherwise.
Returns beginning position of triplequoted-sq "
  (interactive "p")
  (ar-th-backward 'triplequoted-sq arg (interactive-p)))

(defalias 'check-triplequoted-sq-atpt 'triplequoted-sq-check-atpt)

(defun triplequoted-sq-check-atpt ()
  "Return t if a triplequoted-sq at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "triplequoted-sq-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "triplequoted-sq-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; tqs end



(provide 'thingatpt-python-expressions)
;;; thingatpt-python-expressions.el ends here
