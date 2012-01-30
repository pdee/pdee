;;; thing-at-point-utils.el --- more th-at-point edit functions

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

;; ar-thing-at-point-utils-nodelim-core: ar-atpt-classes start

(defun ar-alnum-atpt ()
  "Returns alnum at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alnum nil nil (interactive-p)))

(defalias 'ar-bounds-of-alnum-atpt 'ar-alnum-bounds-atpt)
(defun ar-alnum-bounds-atpt ()
  "Returns a list, borders of alnum if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (interactive-p)))

(defun ar-alnum-beginning-position-atpt ()
  "Returns a number, beginning position ALNUM at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alnum nil (interactive-p)))

(defun ar-alnum-end-position-atpt ()
  "Returns a number, end position of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alnum nil (interactive-p)))

(defalias 'ar-beginning-of-alnum-atpt 'ar-alnum-beginning-atpt)
(defun ar-alnum-beginning-atpt ()
  "Goto beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alnum nil (interactive-p)))

(defalias 'ar-end-of-alnum-atpt 'ar-alnum-end-atpt)
(defun ar-alnum-end-atpt ()
  "Goto end of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alnum nil (interactive-p)))

(defalias 'ar-in-alnum-p-atpt 'ar-alnum-in-p-atpt)
(defun ar-alnum-in-p-atpt ()
  "Returns bounds of ALNUM at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (interactive-p)))

(defalias 'ar-length-of-alnum-atpt 'ar-alnum-length-atpt)
(defun ar-alnum-length-atpt ()
  "Returns beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alnum nil (interactive-p)))

(defalias 'ar-copy-alnum-atpt 'ar-alnum-copy-atpt)
(defun ar-alnum-copy-atpt ()
  "Returns a copy of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alnum nil (interactive-p)))

(defalias 'ar-delete-alnum-in-region 'ar-alnum-delete-in-region)
(defun ar-alnum-delete-in-region (beg end)
  "Deletes ALNUM at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alnum beg end (interactive-p)))

(defun ar-blok-alnum-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alnum.
  Returns blok or nil if no ALNUM at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alnum nil (interactive-p)))

(defalias 'ar-escape-alnum-atpt 'ar-alnum-escape-atpt)
(defun ar-alnum-escape-atpt ()
  "Returns regexp-quoted ALNUM at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'alnum nil))

(defalias 'ar-doublequote-alnum-atpt 'ar-alnum-doublequote-atpt)
(defun ar-alnum-doublequote-atpt ()
  "Doublequotes ALNUM at point if any. "
  (interactive "*")
  (ar-th-doublequote 'alnum nil (interactive-p)))

(defalias 'ar-slash-alnum-atpt 'ar-alnum-slash-atpt)
(defun ar-alnum-slash-atpt ()
  "Doublequotes ALNUM at point if any. "
  (interactive "*")
  (ar-th-slash 'alnum nil (interactive-p)))

(defalias 'ar-double-backslash-alnum-atpt 'ar-alnum-double-backslash-atpt)
(defun ar-alnum-double-backslash-atpt ()
  "Puts doubled backslashes around ALNUM at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'alnum nil (interactive-p)))

(defalias 'ar-doubleslash-alnum-atpt 'ar-alnum-doubleslash-atpt)
(defun ar-alnum-doubleslash-atpt ()
  "Puts doubled slashes around ALNUM at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'alnum nil (interactive-p)))

(defalias 'ar-doubleslash-paren-alnum-atpt 'ar-alnum-doubleslash-paren-atpt)
(defun ar-alnum-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around ALNUM at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'alnum nil (interactive-p)))

(defalias 'ar-slashparen-alnum-atpt 'ar-alnum-slashparen-atpt)
(defun ar-alnum-slashparen-atpt ()
  "Provides slashed parentheses around ALNUM at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'alnum nil (interactive-p)))

(defalias 'ar-dollar-alnum-atpt 'ar-alnum-dollar-atpt)
(defun ar-alnum-dollar-atpt ()
  "Doublequotes ALNUM at point if any. "
  (interactive "*")
  (ar-th-dollar 'alnum nil (interactive-p)))

(defalias 'ar-equalize-alnum-atpt 'ar-alnum-equalize-atpt)
(defun ar-alnum-equalize-atpt ()
  "Puts equal signs `=' around ALNUM at point if any. "
  (interactive "*")
  (ar-th-equalize 'alnum nil (interactive-p)))

(defalias 'ar-greater-angle-alnum-atpt 'ar-alnum-greater-angle-atpt)
(defun ar-alnum-greater-angle-atpt ()
  "Sets angles for ALNUM after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'alnum nil (interactive-p)))

(defalias 'ar-lesser-angle-alnum-atpt 'ar-alnum-lesser-angle-atpt)
(defun ar-alnum-lesser-angle-atpt ()
  "Sets angles for ALNUM after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'alnum nil (interactive-p)))

(defalias 'ar-backslash-alnum-atpt 'ar-alnum-backslash-atpt)
(defun ar-alnum-backslash-atpt ()
  "Backslash ALNUM at point if any. "
  (interactive "*")
  (ar-th-backslash 'alnum nil (interactive-p)))

(defalias 'ar-brace-alnum-atpt 'ar-alnum-brace-atpt)
(defun ar-alnum-brace-atpt ()
  "Braces ALNUM at point if any. "
  (interactive "*")
  (ar-th-brace 'alnum nil (interactive-p)))

(defalias 'ar-bracket-alnum-atpt 'ar-alnum-bracket-atpt)
(defun ar-alnum-bracket-atpt ()
  "Brackets ALNUM after point if any. "
  (interactive "*")
  (ar-th-bracket 'alnum nil (interactive-p)))

(defun ar-comment-alnum-atpt ()
  "Comments ALNUM at point if any. "
  (interactive "*")
  (ar-th-comment 'alnum nil (interactive-p)))

(defun ar-commatize-alnum-atpt ()
  "Put a comma after ALNUM at point if any. "
  (interactive "*")
  (ar-th-commatize 'alnum nil (interactive-p)))

(defun ar-quote-alnum-atpt ()
  "Put a singlequote before ALNUM at point if any. "
  (interactive "*")
  (ar-th-quote 'alnum nil (interactive-p)))

(defalias 'ar-hyphen-alnum-atpt 'ar-alnum-hyphen-atpt)
(defun ar-alnum-hyphen-atpt ()
  "Puts hyphens around ALNUM at point if any. "
  (interactive "*")
  (ar-th-hyphen 'alnum nil (interactive-p)))

(defalias 'ar-mark-alnum-atpt 'ar-alnum-mark-atpt)
(defun ar-alnum-mark-atpt ()
  "Marks ALNUM at point if any. "
  (interactive)
  (ar-th-mark 'alnum))

(defalias 'ar-hide-alnum-atpt 'ar-alnum-hide-atpt)
(defun ar-alnum-hide-atpt ()
  "Hides ALNUM at point. "
  (interactive)
  (ar-th-hide 'alnum))

(defalias 'ar-show-alnum-atpt 'ar-alnum-show-atpt)
(defun ar-alnum-show-atpt ()
  "Shows hidden ALNUM at point. "
  (interactive)
  (ar-th-show 'alnum))

(defalias 'ar-hide-show-alnum-atpt 'ar-alnum-hide-show-atpt)
(defun ar-alnum-hide-show-atpt ()
  "Alternatively hides or shows ALNUM at point. "
  (interactive)
  (ar-th-hide-show 'alnum))

(defalias 'ar-highlight-alnum-atpt-mode 'ar-alnum-highlight-atpt-mode)

(defun ar-alnum-highlight-atpt-mode ()
  "Toggles alnum-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alnum nil (interactive-p)))

(defalias 'ar-kill-alnum-atpt 'ar-alnum-kill-atpt)
(defun ar-alnum-kill-atpt ()
  "Kills ALNUM at point if any. "
  (interactive "*")
  (ar-th-kill 'alnum nil (interactive-p)))

(defalias 'ar-kill-backward-alnum-atpt 'ar-alnum-kill-backward-atpt)
(defun ar-alnum-kill-backward-atpt ()
  "Kills ALNUM at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'alnum nil (interactive-p)))

(defalias 'ar-left-right-singlequote-alnum-atpt 'ar-alnum-left-right-singlequote-atpt)
(defun ar-alnum-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'alnum nil (interactive-p)))

(defalias 'ar-parentize-alnum-atpt 'ar-alnum-parentize-atpt)
(defun ar-alnum-parentize-atpt ()
  "Parentizes ALNUM at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'alnum nil (interactive-p)))

(defalias 'ar-separate-alnum-atpt 'ar-alnum-separate-atpt)
(defun ar-alnum-separate-atpt ()
  "Separates ALNUM at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alnum nil (interactive-p)))

(defalias 'ar-singlequote-alnum-atpt 'ar-alnum-singlequote-atpt)
(defun ar-alnum-singlequote-atpt ()
  "Singlequotes ALNUM at point if any. "
  (interactive "*")
  (ar-th-singlequote 'alnum nil (interactive-p)))

(defalias 'ar-triplequote-dq-alnum-atpt 'ar-alnum-triplequote-dq-atpt)
(defun ar-alnum-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around alnum. "
  (interactive "*")
  (ar-th-triplequote-dq 'alnum nil (interactive-p)))

(defalias 'ar-triplequote-sq-alnum-atpt 'ar-alnum-triplequote-sq-atpt)
(defun ar-alnum-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around alnum. "
  (interactive "*")
  (ar-th-triplequote-sq 'alnum nil (interactive-p)))

(defalias 'ar-alnum-underscore-atpt 'ar-underscore-alnum-atpt)
(defun ar-underscore-alnum-atpt ()
  "Put underscore char around ALNUM. "
  (interactive "*")
  (ar-th-underscore 'alnum nil (interactive-p)))

(defalias 'ar-alnum-whitespace-atpt 'ar-whitespace-alnum-atpt)
(defun ar-whitespace-alnum-atpt ()
  "Put whitespace char around ALNUM. "
  (interactive "*")
  (ar-th-whitespace 'alnum nil t))

(defalias 'ar-forward-alnum-atpt 'ar-alnum-forward-atpt)
(defun ar-alnum-forward-atpt (&optional arg)
  "Moves forward over ALNUM at point if any, does nothing otherwise.
Returns end position of ALNUM "
  (interactive "p")
  (ar-th-forward 'alnum arg (interactive-p)))

(defalias 'ar-backward-alnum-atpt 'ar-alnum-backward-atpt)
(defun ar-alnum-backward-atpt (&optional arg)
  "Moves backward over ALNUM before point if any, does nothing otherwise.
Returns beginning position of ALNUM "
  (interactive "p")
  (ar-th-backward 'alnum arg (interactive-p)))

(defalias 'ar-transpose-alnum-atpt 'ar-alnum-transpose-atpt)
(defun ar-alnum-transpose-atpt (&optional arg)
  "Transposes ALNUM with ALNUM before point if any. "
  (interactive "*p")
  (ar-th-transpose 'alnum arg (interactive-p)))

(defalias 'ar-sort-alnum-atpt 'ar-alnum-sort-atpt)
(defun ar-alnum-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alnums in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'alnum reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-alnum-atpt 'ar-alnum-check-atpt)
(defun ar-alnum-check-atpt ()
  "Return t if a ALNUM at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alnum-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-alnum-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-alpha-atpt ()
  "Returns alpha at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alpha nil nil (interactive-p)))

(defalias 'ar-bounds-of-alpha-atpt 'ar-alpha-bounds-atpt)
(defun ar-alpha-bounds-atpt ()
  "Returns a list, borders of alpha if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (interactive-p)))

(defun ar-alpha-beginning-position-atpt ()
  "Returns a number, beginning position ALPHA at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alpha nil (interactive-p)))

(defun ar-alpha-end-position-atpt ()
  "Returns a number, end position of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alpha nil (interactive-p)))

(defalias 'ar-beginning-of-alpha-atpt 'ar-alpha-beginning-atpt)
(defun ar-alpha-beginning-atpt ()
  "Goto beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alpha nil (interactive-p)))

(defalias 'ar-end-of-alpha-atpt 'ar-alpha-end-atpt)
(defun ar-alpha-end-atpt ()
  "Goto end of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alpha nil (interactive-p)))

(defalias 'ar-in-alpha-p-atpt 'ar-alpha-in-p-atpt)
(defun ar-alpha-in-p-atpt ()
  "Returns bounds of ALPHA at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (interactive-p)))

(defalias 'ar-length-of-alpha-atpt 'ar-alpha-length-atpt)
(defun ar-alpha-length-atpt ()
  "Returns beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alpha nil (interactive-p)))

(defalias 'ar-copy-alpha-atpt 'ar-alpha-copy-atpt)
(defun ar-alpha-copy-atpt ()
  "Returns a copy of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alpha nil (interactive-p)))

(defalias 'ar-delete-alpha-in-region 'ar-alpha-delete-in-region)
(defun ar-alpha-delete-in-region (beg end)
  "Deletes ALPHA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alpha beg end (interactive-p)))

(defun ar-blok-alpha-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alpha.
  Returns blok or nil if no ALPHA at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alpha nil (interactive-p)))

(defalias 'ar-escape-alpha-atpt 'ar-alpha-escape-atpt)
(defun ar-alpha-escape-atpt ()
  "Returns regexp-quoted ALPHA at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'alpha nil))

(defalias 'ar-doublequote-alpha-atpt 'ar-alpha-doublequote-atpt)
(defun ar-alpha-doublequote-atpt ()
  "Doublequotes ALPHA at point if any. "
  (interactive "*")
  (ar-th-doublequote 'alpha nil (interactive-p)))

(defalias 'ar-slash-alpha-atpt 'ar-alpha-slash-atpt)
(defun ar-alpha-slash-atpt ()
  "Doublequotes ALPHA at point if any. "
  (interactive "*")
  (ar-th-slash 'alpha nil (interactive-p)))

(defalias 'ar-double-backslash-alpha-atpt 'ar-alpha-double-backslash-atpt)
(defun ar-alpha-double-backslash-atpt ()
  "Puts doubled backslashes around ALPHA at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'alpha nil (interactive-p)))

(defalias 'ar-doubleslash-alpha-atpt 'ar-alpha-doubleslash-atpt)
(defun ar-alpha-doubleslash-atpt ()
  "Puts doubled slashes around ALPHA at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'alpha nil (interactive-p)))

(defalias 'ar-doubleslash-paren-alpha-atpt 'ar-alpha-doubleslash-paren-atpt)
(defun ar-alpha-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around ALPHA at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'alpha nil (interactive-p)))

(defalias 'ar-slashparen-alpha-atpt 'ar-alpha-slashparen-atpt)
(defun ar-alpha-slashparen-atpt ()
  "Provides slashed parentheses around ALPHA at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'alpha nil (interactive-p)))

(defalias 'ar-dollar-alpha-atpt 'ar-alpha-dollar-atpt)
(defun ar-alpha-dollar-atpt ()
  "Doublequotes ALPHA at point if any. "
  (interactive "*")
  (ar-th-dollar 'alpha nil (interactive-p)))

(defalias 'ar-equalize-alpha-atpt 'ar-alpha-equalize-atpt)
(defun ar-alpha-equalize-atpt ()
  "Puts equal signs `=' around ALPHA at point if any. "
  (interactive "*")
  (ar-th-equalize 'alpha nil (interactive-p)))

(defalias 'ar-greater-angle-alpha-atpt 'ar-alpha-greater-angle-atpt)
(defun ar-alpha-greater-angle-atpt ()
  "Sets angles for ALPHA after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'alpha nil (interactive-p)))

(defalias 'ar-lesser-angle-alpha-atpt 'ar-alpha-lesser-angle-atpt)
(defun ar-alpha-lesser-angle-atpt ()
  "Sets angles for ALPHA after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'alpha nil (interactive-p)))

(defalias 'ar-backslash-alpha-atpt 'ar-alpha-backslash-atpt)
(defun ar-alpha-backslash-atpt ()
  "Backslash ALPHA at point if any. "
  (interactive "*")
  (ar-th-backslash 'alpha nil (interactive-p)))

(defalias 'ar-brace-alpha-atpt 'ar-alpha-brace-atpt)
(defun ar-alpha-brace-atpt ()
  "Braces ALPHA at point if any. "
  (interactive "*")
  (ar-th-brace 'alpha nil (interactive-p)))

(defalias 'ar-bracket-alpha-atpt 'ar-alpha-bracket-atpt)
(defun ar-alpha-bracket-atpt ()
  "Brackets ALPHA after point if any. "
  (interactive "*")
  (ar-th-bracket 'alpha nil (interactive-p)))

(defun ar-comment-alpha-atpt ()
  "Comments ALPHA at point if any. "
  (interactive "*")
  (ar-th-comment 'alpha nil (interactive-p)))

(defun ar-commatize-alpha-atpt ()
  "Put a comma after ALPHA at point if any. "
  (interactive "*")
  (ar-th-commatize 'alpha nil (interactive-p)))

(defun ar-quote-alpha-atpt ()
  "Put a singlequote before ALPHA at point if any. "
  (interactive "*")
  (ar-th-quote 'alpha nil (interactive-p)))

(defalias 'ar-hyphen-alpha-atpt 'ar-alpha-hyphen-atpt)
(defun ar-alpha-hyphen-atpt ()
  "Puts hyphens around ALPHA at point if any. "
  (interactive "*")
  (ar-th-hyphen 'alpha nil (interactive-p)))

(defalias 'ar-mark-alpha-atpt 'ar-alpha-mark-atpt)
(defun ar-alpha-mark-atpt ()
  "Marks ALPHA at point if any. "
  (interactive)
  (ar-th-mark 'alpha))

(defalias 'ar-hide-alpha-atpt 'ar-alpha-hide-atpt)
(defun ar-alpha-hide-atpt ()
  "Hides ALPHA at point. "
  (interactive)
  (ar-th-hide 'alpha))

(defalias 'ar-show-alpha-atpt 'ar-alpha-show-atpt)
(defun ar-alpha-show-atpt ()
  "Shows hidden ALPHA at point. "
  (interactive)
  (ar-th-show 'alpha))

(defalias 'ar-hide-show-alpha-atpt 'ar-alpha-hide-show-atpt)
(defun ar-alpha-hide-show-atpt ()
  "Alternatively hides or shows ALPHA at point. "
  (interactive)
  (ar-th-hide-show 'alpha))

(defalias 'ar-highlight-alpha-atpt-mode 'ar-alpha-highlight-atpt-mode)

(defun ar-alpha-highlight-atpt-mode ()
  "Toggles alpha-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alpha nil (interactive-p)))

(defalias 'ar-kill-alpha-atpt 'ar-alpha-kill-atpt)
(defun ar-alpha-kill-atpt ()
  "Kills ALPHA at point if any. "
  (interactive "*")
  (ar-th-kill 'alpha nil (interactive-p)))

(defalias 'ar-kill-backward-alpha-atpt 'ar-alpha-kill-backward-atpt)
(defun ar-alpha-kill-backward-atpt ()
  "Kills ALPHA at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'alpha nil (interactive-p)))

(defalias 'ar-left-right-singlequote-alpha-atpt 'ar-alpha-left-right-singlequote-atpt)
(defun ar-alpha-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'alpha nil (interactive-p)))

(defalias 'ar-parentize-alpha-atpt 'ar-alpha-parentize-atpt)
(defun ar-alpha-parentize-atpt ()
  "Parentizes ALPHA at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'alpha nil (interactive-p)))

(defalias 'ar-separate-alpha-atpt 'ar-alpha-separate-atpt)
(defun ar-alpha-separate-atpt ()
  "Separates ALPHA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alpha nil (interactive-p)))

(defalias 'ar-singlequote-alpha-atpt 'ar-alpha-singlequote-atpt)
(defun ar-alpha-singlequote-atpt ()
  "Singlequotes ALPHA at point if any. "
  (interactive "*")
  (ar-th-singlequote 'alpha nil (interactive-p)))

(defalias 'ar-triplequote-dq-alpha-atpt 'ar-alpha-triplequote-dq-atpt)
(defun ar-alpha-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around alpha. "
  (interactive "*")
  (ar-th-triplequote-dq 'alpha nil (interactive-p)))

(defalias 'ar-triplequote-sq-alpha-atpt 'ar-alpha-triplequote-sq-atpt)
(defun ar-alpha-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around alpha. "
  (interactive "*")
  (ar-th-triplequote-sq 'alpha nil (interactive-p)))

(defalias 'ar-alpha-underscore-atpt 'ar-underscore-alpha-atpt)
(defun ar-underscore-alpha-atpt ()
  "Put underscore char around ALPHA. "
  (interactive "*")
  (ar-th-underscore 'alpha nil (interactive-p)))

(defalias 'ar-alpha-whitespace-atpt 'ar-whitespace-alpha-atpt)
(defun ar-whitespace-alpha-atpt ()
  "Put whitespace char around ALPHA. "
  (interactive "*")
  (ar-th-whitespace 'alpha nil t))

(defalias 'ar-forward-alpha-atpt 'ar-alpha-forward-atpt)
(defun ar-alpha-forward-atpt (&optional arg)
  "Moves forward over ALPHA at point if any, does nothing otherwise.
Returns end position of ALPHA "
  (interactive "p")
  (ar-th-forward 'alpha arg (interactive-p)))

(defalias 'ar-backward-alpha-atpt 'ar-alpha-backward-atpt)
(defun ar-alpha-backward-atpt (&optional arg)
  "Moves backward over ALPHA before point if any, does nothing otherwise.
Returns beginning position of ALPHA "
  (interactive "p")
  (ar-th-backward 'alpha arg (interactive-p)))

(defalias 'ar-transpose-alpha-atpt 'ar-alpha-transpose-atpt)
(defun ar-alpha-transpose-atpt (&optional arg)
  "Transposes ALPHA with ALPHA before point if any. "
  (interactive "*p")
  (ar-th-transpose 'alpha arg (interactive-p)))

(defalias 'ar-sort-alpha-atpt 'ar-alpha-sort-atpt)
(defun ar-alpha-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alphas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'alpha reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-alpha-atpt 'ar-alpha-check-atpt)
(defun ar-alpha-check-atpt ()
  "Return t if a ALPHA at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alpha-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-alpha-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-ascii-atpt ()
  "Returns ascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'ascii nil nil (interactive-p)))

(defalias 'ar-bounds-of-ascii-atpt 'ar-ascii-bounds-atpt)
(defun ar-ascii-bounds-atpt ()
  "Returns a list, borders of ascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (interactive-p)))

(defun ar-ascii-beginning-position-atpt ()
  "Returns a number, beginning position ASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'ascii nil (interactive-p)))

(defun ar-ascii-end-position-atpt ()
  "Returns a number, end position of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'ascii nil (interactive-p)))

(defalias 'ar-beginning-of-ascii-atpt 'ar-ascii-beginning-atpt)
(defun ar-ascii-beginning-atpt ()
  "Goto beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'ascii nil (interactive-p)))

(defalias 'ar-end-of-ascii-atpt 'ar-ascii-end-atpt)
(defun ar-ascii-end-atpt ()
  "Goto end of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'ascii nil (interactive-p)))

(defalias 'ar-in-ascii-p-atpt 'ar-ascii-in-p-atpt)
(defun ar-ascii-in-p-atpt ()
  "Returns bounds of ASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (interactive-p)))

(defalias 'ar-length-of-ascii-atpt 'ar-ascii-length-atpt)
(defun ar-ascii-length-atpt ()
  "Returns beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'ascii nil (interactive-p)))

(defalias 'ar-copy-ascii-atpt 'ar-ascii-copy-atpt)
(defun ar-ascii-copy-atpt ()
  "Returns a copy of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'ascii nil (interactive-p)))

(defalias 'ar-delete-ascii-in-region 'ar-ascii-delete-in-region)
(defun ar-ascii-delete-in-region (beg end)
  "Deletes ASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ascii beg end (interactive-p)))

(defun ar-blok-ascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ascii.
  Returns blok or nil if no ASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'ascii nil (interactive-p)))

(defalias 'ar-escape-ascii-atpt 'ar-ascii-escape-atpt)
(defun ar-ascii-escape-atpt ()
  "Returns regexp-quoted ASCII at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'ascii nil))

(defalias 'ar-doublequote-ascii-atpt 'ar-ascii-doublequote-atpt)
(defun ar-ascii-doublequote-atpt ()
  "Doublequotes ASCII at point if any. "
  (interactive "*")
  (ar-th-doublequote 'ascii nil (interactive-p)))

(defalias 'ar-slash-ascii-atpt 'ar-ascii-slash-atpt)
(defun ar-ascii-slash-atpt ()
  "Doublequotes ASCII at point if any. "
  (interactive "*")
  (ar-th-slash 'ascii nil (interactive-p)))

(defalias 'ar-double-backslash-ascii-atpt 'ar-ascii-double-backslash-atpt)
(defun ar-ascii-double-backslash-atpt ()
  "Puts doubled backslashes around ASCII at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'ascii nil (interactive-p)))

(defalias 'ar-doubleslash-ascii-atpt 'ar-ascii-doubleslash-atpt)
(defun ar-ascii-doubleslash-atpt ()
  "Puts doubled slashes around ASCII at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'ascii nil (interactive-p)))

(defalias 'ar-doubleslash-paren-ascii-atpt 'ar-ascii-doubleslash-paren-atpt)
(defun ar-ascii-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around ASCII at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'ascii nil (interactive-p)))

(defalias 'ar-slashparen-ascii-atpt 'ar-ascii-slashparen-atpt)
(defun ar-ascii-slashparen-atpt ()
  "Provides slashed parentheses around ASCII at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'ascii nil (interactive-p)))

(defalias 'ar-dollar-ascii-atpt 'ar-ascii-dollar-atpt)
(defun ar-ascii-dollar-atpt ()
  "Doublequotes ASCII at point if any. "
  (interactive "*")
  (ar-th-dollar 'ascii nil (interactive-p)))

(defalias 'ar-equalize-ascii-atpt 'ar-ascii-equalize-atpt)
(defun ar-ascii-equalize-atpt ()
  "Puts equal signs `=' around ASCII at point if any. "
  (interactive "*")
  (ar-th-equalize 'ascii nil (interactive-p)))

(defalias 'ar-greater-angle-ascii-atpt 'ar-ascii-greater-angle-atpt)
(defun ar-ascii-greater-angle-atpt ()
  "Sets angles for ASCII after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'ascii nil (interactive-p)))

(defalias 'ar-lesser-angle-ascii-atpt 'ar-ascii-lesser-angle-atpt)
(defun ar-ascii-lesser-angle-atpt ()
  "Sets angles for ASCII after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'ascii nil (interactive-p)))

(defalias 'ar-backslash-ascii-atpt 'ar-ascii-backslash-atpt)
(defun ar-ascii-backslash-atpt ()
  "Backslash ASCII at point if any. "
  (interactive "*")
  (ar-th-backslash 'ascii nil (interactive-p)))

(defalias 'ar-brace-ascii-atpt 'ar-ascii-brace-atpt)
(defun ar-ascii-brace-atpt ()
  "Braces ASCII at point if any. "
  (interactive "*")
  (ar-th-brace 'ascii nil (interactive-p)))

(defalias 'ar-bracket-ascii-atpt 'ar-ascii-bracket-atpt)
(defun ar-ascii-bracket-atpt ()
  "Brackets ASCII after point if any. "
  (interactive "*")
  (ar-th-bracket 'ascii nil (interactive-p)))

(defun ar-comment-ascii-atpt ()
  "Comments ASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'ascii nil (interactive-p)))

(defun ar-commatize-ascii-atpt ()
  "Put a comma after ASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'ascii nil (interactive-p)))

(defun ar-quote-ascii-atpt ()
  "Put a singlequote before ASCII at point if any. "
  (interactive "*")
  (ar-th-quote 'ascii nil (interactive-p)))

(defalias 'ar-hyphen-ascii-atpt 'ar-ascii-hyphen-atpt)
(defun ar-ascii-hyphen-atpt ()
  "Puts hyphens around ASCII at point if any. "
  (interactive "*")
  (ar-th-hyphen 'ascii nil (interactive-p)))

(defalias 'ar-mark-ascii-atpt 'ar-ascii-mark-atpt)
(defun ar-ascii-mark-atpt ()
  "Marks ASCII at point if any. "
  (interactive)
  (ar-th-mark 'ascii))

(defalias 'ar-hide-ascii-atpt 'ar-ascii-hide-atpt)
(defun ar-ascii-hide-atpt ()
  "Hides ASCII at point. "
  (interactive)
  (ar-th-hide 'ascii))

(defalias 'ar-show-ascii-atpt 'ar-ascii-show-atpt)
(defun ar-ascii-show-atpt ()
  "Shows hidden ASCII at point. "
  (interactive)
  (ar-th-show 'ascii))

(defalias 'ar-hide-show-ascii-atpt 'ar-ascii-hide-show-atpt)
(defun ar-ascii-hide-show-atpt ()
  "Alternatively hides or shows ASCII at point. "
  (interactive)
  (ar-th-hide-show 'ascii))

(defalias 'ar-highlight-ascii-atpt-mode 'ar-ascii-highlight-atpt-mode)

(defun ar-ascii-highlight-atpt-mode ()
  "Toggles ascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'ascii nil (interactive-p)))

(defalias 'ar-kill-ascii-atpt 'ar-ascii-kill-atpt)
(defun ar-ascii-kill-atpt ()
  "Kills ASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'ascii nil (interactive-p)))

(defalias 'ar-kill-backward-ascii-atpt 'ar-ascii-kill-backward-atpt)
(defun ar-ascii-kill-backward-atpt ()
  "Kills ASCII at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'ascii nil (interactive-p)))

(defalias 'ar-left-right-singlequote-ascii-atpt 'ar-ascii-left-right-singlequote-atpt)
(defun ar-ascii-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'ascii nil (interactive-p)))

(defalias 'ar-parentize-ascii-atpt 'ar-ascii-parentize-atpt)
(defun ar-ascii-parentize-atpt ()
  "Parentizes ASCII at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'ascii nil (interactive-p)))

(defalias 'ar-separate-ascii-atpt 'ar-ascii-separate-atpt)
(defun ar-ascii-separate-atpt ()
  "Separates ASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'ascii nil (interactive-p)))

(defalias 'ar-singlequote-ascii-atpt 'ar-ascii-singlequote-atpt)
(defun ar-ascii-singlequote-atpt ()
  "Singlequotes ASCII at point if any. "
  (interactive "*")
  (ar-th-singlequote 'ascii nil (interactive-p)))

(defalias 'ar-triplequote-dq-ascii-atpt 'ar-ascii-triplequote-dq-atpt)
(defun ar-ascii-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around ascii. "
  (interactive "*")
  (ar-th-triplequote-dq 'ascii nil (interactive-p)))

(defalias 'ar-triplequote-sq-ascii-atpt 'ar-ascii-triplequote-sq-atpt)
(defun ar-ascii-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around ascii. "
  (interactive "*")
  (ar-th-triplequote-sq 'ascii nil (interactive-p)))

(defalias 'ar-ascii-underscore-atpt 'ar-underscore-ascii-atpt)
(defun ar-underscore-ascii-atpt ()
  "Put underscore char around ASCII. "
  (interactive "*")
  (ar-th-underscore 'ascii nil (interactive-p)))

(defalias 'ar-ascii-whitespace-atpt 'ar-whitespace-ascii-atpt)
(defun ar-whitespace-ascii-atpt ()
  "Put whitespace char around ASCII. "
  (interactive "*")
  (ar-th-whitespace 'ascii nil t))

(defalias 'ar-forward-ascii-atpt 'ar-ascii-forward-atpt)
(defun ar-ascii-forward-atpt (&optional arg)
  "Moves forward over ASCII at point if any, does nothing otherwise.
Returns end position of ASCII "
  (interactive "p")
  (ar-th-forward 'ascii arg (interactive-p)))

(defalias 'ar-backward-ascii-atpt 'ar-ascii-backward-atpt)
(defun ar-ascii-backward-atpt (&optional arg)
  "Moves backward over ASCII before point if any, does nothing otherwise.
Returns beginning position of ASCII "
  (interactive "p")
  (ar-th-backward 'ascii arg (interactive-p)))

(defalias 'ar-transpose-ascii-atpt 'ar-ascii-transpose-atpt)
(defun ar-ascii-transpose-atpt (&optional arg)
  "Transposes ASCII with ASCII before point if any. "
  (interactive "*p")
  (ar-th-transpose 'ascii arg (interactive-p)))

(defalias 'ar-sort-ascii-atpt 'ar-ascii-sort-atpt)
(defun ar-ascii-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts asciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'ascii reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-ascii-atpt 'ar-ascii-check-atpt)
(defun ar-ascii-check-atpt ()
  "Return t if a ASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-ascii-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-ascii-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-blank-atpt ()
  "Returns blank at point if any, nil otherwise. "
  (interactive)
  (ar-th 'blank nil nil (interactive-p)))

(defalias 'ar-bounds-of-blank-atpt 'ar-blank-bounds-atpt)
(defun ar-blank-bounds-atpt ()
  "Returns a list, borders of blank if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (interactive-p)))

(defun ar-blank-beginning-position-atpt ()
  "Returns a number, beginning position BLANK at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'blank nil (interactive-p)))

(defun ar-blank-end-position-atpt ()
  "Returns a number, end position of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'blank nil (interactive-p)))

(defalias 'ar-beginning-of-blank-atpt 'ar-blank-beginning-atpt)
(defun ar-blank-beginning-atpt ()
  "Goto beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'blank nil (interactive-p)))

(defalias 'ar-end-of-blank-atpt 'ar-blank-end-atpt)
(defun ar-blank-end-atpt ()
  "Goto end of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'blank nil (interactive-p)))

(defalias 'ar-in-blank-p-atpt 'ar-blank-in-p-atpt)
(defun ar-blank-in-p-atpt ()
  "Returns bounds of BLANK at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (interactive-p)))

(defalias 'ar-length-of-blank-atpt 'ar-blank-length-atpt)
(defun ar-blank-length-atpt ()
  "Returns beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'blank nil (interactive-p)))

(defalias 'ar-copy-blank-atpt 'ar-blank-copy-atpt)
(defun ar-blank-copy-atpt ()
  "Returns a copy of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'blank nil (interactive-p)))

(defalias 'ar-delete-blank-in-region 'ar-blank-delete-in-region)
(defun ar-blank-delete-in-region (beg end)
  "Deletes BLANK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blank beg end (interactive-p)))

(defun ar-blok-blank-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around blank.
  Returns blok or nil if no BLANK at cursor-position. "
  (interactive "*")
  (ar-th-blok 'blank nil (interactive-p)))

(defalias 'ar-escape-blank-atpt 'ar-blank-escape-atpt)
(defun ar-blank-escape-atpt ()
  "Returns regexp-quoted BLANK at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'blank nil))

(defalias 'ar-doublequote-blank-atpt 'ar-blank-doublequote-atpt)
(defun ar-blank-doublequote-atpt ()
  "Doublequotes BLANK at point if any. "
  (interactive "*")
  (ar-th-doublequote 'blank nil (interactive-p)))

(defalias 'ar-slash-blank-atpt 'ar-blank-slash-atpt)
(defun ar-blank-slash-atpt ()
  "Doublequotes BLANK at point if any. "
  (interactive "*")
  (ar-th-slash 'blank nil (interactive-p)))

(defalias 'ar-double-backslash-blank-atpt 'ar-blank-double-backslash-atpt)
(defun ar-blank-double-backslash-atpt ()
  "Puts doubled backslashes around BLANK at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'blank nil (interactive-p)))

(defalias 'ar-doubleslash-blank-atpt 'ar-blank-doubleslash-atpt)
(defun ar-blank-doubleslash-atpt ()
  "Puts doubled slashes around BLANK at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'blank nil (interactive-p)))

(defalias 'ar-doubleslash-paren-blank-atpt 'ar-blank-doubleslash-paren-atpt)
(defun ar-blank-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around BLANK at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'blank nil (interactive-p)))

(defalias 'ar-slashparen-blank-atpt 'ar-blank-slashparen-atpt)
(defun ar-blank-slashparen-atpt ()
  "Provides slashed parentheses around BLANK at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'blank nil (interactive-p)))

(defalias 'ar-dollar-blank-atpt 'ar-blank-dollar-atpt)
(defun ar-blank-dollar-atpt ()
  "Doublequotes BLANK at point if any. "
  (interactive "*")
  (ar-th-dollar 'blank nil (interactive-p)))

(defalias 'ar-equalize-blank-atpt 'ar-blank-equalize-atpt)
(defun ar-blank-equalize-atpt ()
  "Puts equal signs `=' around BLANK at point if any. "
  (interactive "*")
  (ar-th-equalize 'blank nil (interactive-p)))

(defalias 'ar-greater-angle-blank-atpt 'ar-blank-greater-angle-atpt)
(defun ar-blank-greater-angle-atpt ()
  "Sets angles for BLANK after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'blank nil (interactive-p)))

(defalias 'ar-lesser-angle-blank-atpt 'ar-blank-lesser-angle-atpt)
(defun ar-blank-lesser-angle-atpt ()
  "Sets angles for BLANK after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'blank nil (interactive-p)))

(defalias 'ar-backslash-blank-atpt 'ar-blank-backslash-atpt)
(defun ar-blank-backslash-atpt ()
  "Backslash BLANK at point if any. "
  (interactive "*")
  (ar-th-backslash 'blank nil (interactive-p)))

(defalias 'ar-brace-blank-atpt 'ar-blank-brace-atpt)
(defun ar-blank-brace-atpt ()
  "Braces BLANK at point if any. "
  (interactive "*")
  (ar-th-brace 'blank nil (interactive-p)))

(defalias 'ar-bracket-blank-atpt 'ar-blank-bracket-atpt)
(defun ar-blank-bracket-atpt ()
  "Brackets BLANK after point if any. "
  (interactive "*")
  (ar-th-bracket 'blank nil (interactive-p)))

(defun ar-comment-blank-atpt ()
  "Comments BLANK at point if any. "
  (interactive "*")
  (ar-th-comment 'blank nil (interactive-p)))

(defun ar-commatize-blank-atpt ()
  "Put a comma after BLANK at point if any. "
  (interactive "*")
  (ar-th-commatize 'blank nil (interactive-p)))

(defun ar-quote-blank-atpt ()
  "Put a singlequote before BLANK at point if any. "
  (interactive "*")
  (ar-th-quote 'blank nil (interactive-p)))

(defalias 'ar-hyphen-blank-atpt 'ar-blank-hyphen-atpt)
(defun ar-blank-hyphen-atpt ()
  "Puts hyphens around BLANK at point if any. "
  (interactive "*")
  (ar-th-hyphen 'blank nil (interactive-p)))

(defalias 'ar-mark-blank-atpt 'ar-blank-mark-atpt)
(defun ar-blank-mark-atpt ()
  "Marks BLANK at point if any. "
  (interactive)
  (ar-th-mark 'blank))

(defalias 'ar-hide-blank-atpt 'ar-blank-hide-atpt)
(defun ar-blank-hide-atpt ()
  "Hides BLANK at point. "
  (interactive)
  (ar-th-hide 'blank))

(defalias 'ar-show-blank-atpt 'ar-blank-show-atpt)
(defun ar-blank-show-atpt ()
  "Shows hidden BLANK at point. "
  (interactive)
  (ar-th-show 'blank))

(defalias 'ar-hide-show-blank-atpt 'ar-blank-hide-show-atpt)
(defun ar-blank-hide-show-atpt ()
  "Alternatively hides or shows BLANK at point. "
  (interactive)
  (ar-th-hide-show 'blank))

(defalias 'ar-highlight-blank-atpt-mode 'ar-blank-highlight-atpt-mode)

(defun ar-blank-highlight-atpt-mode ()
  "Toggles blank-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'blank nil (interactive-p)))

(defalias 'ar-kill-blank-atpt 'ar-blank-kill-atpt)
(defun ar-blank-kill-atpt ()
  "Kills BLANK at point if any. "
  (interactive "*")
  (ar-th-kill 'blank nil (interactive-p)))

(defalias 'ar-kill-backward-blank-atpt 'ar-blank-kill-backward-atpt)
(defun ar-blank-kill-backward-atpt ()
  "Kills BLANK at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'blank nil (interactive-p)))

(defalias 'ar-left-right-singlequote-blank-atpt 'ar-blank-left-right-singlequote-atpt)
(defun ar-blank-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'blank nil (interactive-p)))

(defalias 'ar-parentize-blank-atpt 'ar-blank-parentize-atpt)
(defun ar-blank-parentize-atpt ()
  "Parentizes BLANK at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'blank nil (interactive-p)))

(defalias 'ar-separate-blank-atpt 'ar-blank-separate-atpt)
(defun ar-blank-separate-atpt ()
  "Separates BLANK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'blank nil (interactive-p)))

(defalias 'ar-singlequote-blank-atpt 'ar-blank-singlequote-atpt)
(defun ar-blank-singlequote-atpt ()
  "Singlequotes BLANK at point if any. "
  (interactive "*")
  (ar-th-singlequote 'blank nil (interactive-p)))

(defalias 'ar-triplequote-dq-blank-atpt 'ar-blank-triplequote-dq-atpt)
(defun ar-blank-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around blank. "
  (interactive "*")
  (ar-th-triplequote-dq 'blank nil (interactive-p)))

(defalias 'ar-triplequote-sq-blank-atpt 'ar-blank-triplequote-sq-atpt)
(defun ar-blank-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around blank. "
  (interactive "*")
  (ar-th-triplequote-sq 'blank nil (interactive-p)))

(defalias 'ar-blank-underscore-atpt 'ar-underscore-blank-atpt)
(defun ar-underscore-blank-atpt ()
  "Put underscore char around BLANK. "
  (interactive "*")
  (ar-th-underscore 'blank nil (interactive-p)))

(defalias 'ar-blank-whitespace-atpt 'ar-whitespace-blank-atpt)
(defun ar-whitespace-blank-atpt ()
  "Put whitespace char around BLANK. "
  (interactive "*")
  (ar-th-whitespace 'blank nil t))

(defalias 'ar-forward-blank-atpt 'ar-blank-forward-atpt)
(defun ar-blank-forward-atpt (&optional arg)
  "Moves forward over BLANK at point if any, does nothing otherwise.
Returns end position of BLANK "
  (interactive "p")
  (ar-th-forward 'blank arg (interactive-p)))

(defalias 'ar-backward-blank-atpt 'ar-blank-backward-atpt)
(defun ar-blank-backward-atpt (&optional arg)
  "Moves backward over BLANK before point if any, does nothing otherwise.
Returns beginning position of BLANK "
  (interactive "p")
  (ar-th-backward 'blank arg (interactive-p)))

(defalias 'ar-transpose-blank-atpt 'ar-blank-transpose-atpt)
(defun ar-blank-transpose-atpt (&optional arg)
  "Transposes BLANK with BLANK before point if any. "
  (interactive "*p")
  (ar-th-transpose 'blank arg (interactive-p)))

(defalias 'ar-sort-blank-atpt 'ar-blank-sort-atpt)
(defun ar-blank-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts blanks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'blank reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-blank-atpt 'ar-blank-check-atpt)
(defun ar-blank-check-atpt ()
  "Return t if a BLANK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-blank-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-blank-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-cntrl-atpt ()
  "Returns cntrl at point if any, nil otherwise. "
  (interactive)
  (ar-th 'cntrl nil nil (interactive-p)))

(defalias 'ar-bounds-of-cntrl-atpt 'ar-cntrl-bounds-atpt)
(defun ar-cntrl-bounds-atpt ()
  "Returns a list, borders of cntrl if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (interactive-p)))

(defun ar-cntrl-beginning-position-atpt ()
  "Returns a number, beginning position CNTRL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'cntrl nil (interactive-p)))

(defun ar-cntrl-end-position-atpt ()
  "Returns a number, end position of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'cntrl nil (interactive-p)))

(defalias 'ar-beginning-of-cntrl-atpt 'ar-cntrl-beginning-atpt)
(defun ar-cntrl-beginning-atpt ()
  "Goto beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'cntrl nil (interactive-p)))

(defalias 'ar-end-of-cntrl-atpt 'ar-cntrl-end-atpt)
(defun ar-cntrl-end-atpt ()
  "Goto end of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'cntrl nil (interactive-p)))

(defalias 'ar-in-cntrl-p-atpt 'ar-cntrl-in-p-atpt)
(defun ar-cntrl-in-p-atpt ()
  "Returns bounds of CNTRL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (interactive-p)))

(defalias 'ar-length-of-cntrl-atpt 'ar-cntrl-length-atpt)
(defun ar-cntrl-length-atpt ()
  "Returns beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'cntrl nil (interactive-p)))

(defalias 'ar-copy-cntrl-atpt 'ar-cntrl-copy-atpt)
(defun ar-cntrl-copy-atpt ()
  "Returns a copy of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'cntrl nil (interactive-p)))

(defalias 'ar-delete-cntrl-in-region 'ar-cntrl-delete-in-region)
(defun ar-cntrl-delete-in-region (beg end)
  "Deletes CNTRL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'cntrl beg end (interactive-p)))

(defun ar-blok-cntrl-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around cntrl.
  Returns blok or nil if no CNTRL at cursor-position. "
  (interactive "*")
  (ar-th-blok 'cntrl nil (interactive-p)))

(defalias 'ar-escape-cntrl-atpt 'ar-cntrl-escape-atpt)
(defun ar-cntrl-escape-atpt ()
  "Returns regexp-quoted CNTRL at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'cntrl nil))

(defalias 'ar-doublequote-cntrl-atpt 'ar-cntrl-doublequote-atpt)
(defun ar-cntrl-doublequote-atpt ()
  "Doublequotes CNTRL at point if any. "
  (interactive "*")
  (ar-th-doublequote 'cntrl nil (interactive-p)))

(defalias 'ar-slash-cntrl-atpt 'ar-cntrl-slash-atpt)
(defun ar-cntrl-slash-atpt ()
  "Doublequotes CNTRL at point if any. "
  (interactive "*")
  (ar-th-slash 'cntrl nil (interactive-p)))

(defalias 'ar-double-backslash-cntrl-atpt 'ar-cntrl-double-backslash-atpt)
(defun ar-cntrl-double-backslash-atpt ()
  "Puts doubled backslashes around CNTRL at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'cntrl nil (interactive-p)))

(defalias 'ar-doubleslash-cntrl-atpt 'ar-cntrl-doubleslash-atpt)
(defun ar-cntrl-doubleslash-atpt ()
  "Puts doubled slashes around CNTRL at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'cntrl nil (interactive-p)))

(defalias 'ar-doubleslash-paren-cntrl-atpt 'ar-cntrl-doubleslash-paren-atpt)
(defun ar-cntrl-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around CNTRL at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'cntrl nil (interactive-p)))

(defalias 'ar-slashparen-cntrl-atpt 'ar-cntrl-slashparen-atpt)
(defun ar-cntrl-slashparen-atpt ()
  "Provides slashed parentheses around CNTRL at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'cntrl nil (interactive-p)))

(defalias 'ar-dollar-cntrl-atpt 'ar-cntrl-dollar-atpt)
(defun ar-cntrl-dollar-atpt ()
  "Doublequotes CNTRL at point if any. "
  (interactive "*")
  (ar-th-dollar 'cntrl nil (interactive-p)))

(defalias 'ar-equalize-cntrl-atpt 'ar-cntrl-equalize-atpt)
(defun ar-cntrl-equalize-atpt ()
  "Puts equal signs `=' around CNTRL at point if any. "
  (interactive "*")
  (ar-th-equalize 'cntrl nil (interactive-p)))

(defalias 'ar-greater-angle-cntrl-atpt 'ar-cntrl-greater-angle-atpt)
(defun ar-cntrl-greater-angle-atpt ()
  "Sets angles for CNTRL after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'cntrl nil (interactive-p)))

(defalias 'ar-lesser-angle-cntrl-atpt 'ar-cntrl-lesser-angle-atpt)
(defun ar-cntrl-lesser-angle-atpt ()
  "Sets angles for CNTRL after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'cntrl nil (interactive-p)))

(defalias 'ar-backslash-cntrl-atpt 'ar-cntrl-backslash-atpt)
(defun ar-cntrl-backslash-atpt ()
  "Backslash CNTRL at point if any. "
  (interactive "*")
  (ar-th-backslash 'cntrl nil (interactive-p)))

(defalias 'ar-brace-cntrl-atpt 'ar-cntrl-brace-atpt)
(defun ar-cntrl-brace-atpt ()
  "Braces CNTRL at point if any. "
  (interactive "*")
  (ar-th-brace 'cntrl nil (interactive-p)))

(defalias 'ar-bracket-cntrl-atpt 'ar-cntrl-bracket-atpt)
(defun ar-cntrl-bracket-atpt ()
  "Brackets CNTRL after point if any. "
  (interactive "*")
  (ar-th-bracket 'cntrl nil (interactive-p)))

(defun ar-comment-cntrl-atpt ()
  "Comments CNTRL at point if any. "
  (interactive "*")
  (ar-th-comment 'cntrl nil (interactive-p)))

(defun ar-commatize-cntrl-atpt ()
  "Put a comma after CNTRL at point if any. "
  (interactive "*")
  (ar-th-commatize 'cntrl nil (interactive-p)))

(defun ar-quote-cntrl-atpt ()
  "Put a singlequote before CNTRL at point if any. "
  (interactive "*")
  (ar-th-quote 'cntrl nil (interactive-p)))

(defalias 'ar-hyphen-cntrl-atpt 'ar-cntrl-hyphen-atpt)
(defun ar-cntrl-hyphen-atpt ()
  "Puts hyphens around CNTRL at point if any. "
  (interactive "*")
  (ar-th-hyphen 'cntrl nil (interactive-p)))

(defalias 'ar-mark-cntrl-atpt 'ar-cntrl-mark-atpt)
(defun ar-cntrl-mark-atpt ()
  "Marks CNTRL at point if any. "
  (interactive)
  (ar-th-mark 'cntrl))

(defalias 'ar-hide-cntrl-atpt 'ar-cntrl-hide-atpt)
(defun ar-cntrl-hide-atpt ()
  "Hides CNTRL at point. "
  (interactive)
  (ar-th-hide 'cntrl))

(defalias 'ar-show-cntrl-atpt 'ar-cntrl-show-atpt)
(defun ar-cntrl-show-atpt ()
  "Shows hidden CNTRL at point. "
  (interactive)
  (ar-th-show 'cntrl))

(defalias 'ar-hide-show-cntrl-atpt 'ar-cntrl-hide-show-atpt)
(defun ar-cntrl-hide-show-atpt ()
  "Alternatively hides or shows CNTRL at point. "
  (interactive)
  (ar-th-hide-show 'cntrl))

(defalias 'ar-highlight-cntrl-atpt-mode 'ar-cntrl-highlight-atpt-mode)

(defun ar-cntrl-highlight-atpt-mode ()
  "Toggles cntrl-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'cntrl nil (interactive-p)))

(defalias 'ar-kill-cntrl-atpt 'ar-cntrl-kill-atpt)
(defun ar-cntrl-kill-atpt ()
  "Kills CNTRL at point if any. "
  (interactive "*")
  (ar-th-kill 'cntrl nil (interactive-p)))

(defalias 'ar-kill-backward-cntrl-atpt 'ar-cntrl-kill-backward-atpt)
(defun ar-cntrl-kill-backward-atpt ()
  "Kills CNTRL at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'cntrl nil (interactive-p)))

(defalias 'ar-left-right-singlequote-cntrl-atpt 'ar-cntrl-left-right-singlequote-atpt)
(defun ar-cntrl-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'cntrl nil (interactive-p)))

(defalias 'ar-parentize-cntrl-atpt 'ar-cntrl-parentize-atpt)
(defun ar-cntrl-parentize-atpt ()
  "Parentizes CNTRL at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'cntrl nil (interactive-p)))

(defalias 'ar-separate-cntrl-atpt 'ar-cntrl-separate-atpt)
(defun ar-cntrl-separate-atpt ()
  "Separates CNTRL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'cntrl nil (interactive-p)))

(defalias 'ar-singlequote-cntrl-atpt 'ar-cntrl-singlequote-atpt)
(defun ar-cntrl-singlequote-atpt ()
  "Singlequotes CNTRL at point if any. "
  (interactive "*")
  (ar-th-singlequote 'cntrl nil (interactive-p)))

(defalias 'ar-triplequote-dq-cntrl-atpt 'ar-cntrl-triplequote-dq-atpt)
(defun ar-cntrl-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequote-dq 'cntrl nil (interactive-p)))

(defalias 'ar-triplequote-sq-cntrl-atpt 'ar-cntrl-triplequote-sq-atpt)
(defun ar-cntrl-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequote-sq 'cntrl nil (interactive-p)))

(defalias 'ar-cntrl-underscore-atpt 'ar-underscore-cntrl-atpt)
(defun ar-underscore-cntrl-atpt ()
  "Put underscore char around CNTRL. "
  (interactive "*")
  (ar-th-underscore 'cntrl nil (interactive-p)))

(defalias 'ar-cntrl-whitespace-atpt 'ar-whitespace-cntrl-atpt)
(defun ar-whitespace-cntrl-atpt ()
  "Put whitespace char around CNTRL. "
  (interactive "*")
  (ar-th-whitespace 'cntrl nil t))

(defalias 'ar-forward-cntrl-atpt 'ar-cntrl-forward-atpt)
(defun ar-cntrl-forward-atpt (&optional arg)
  "Moves forward over CNTRL at point if any, does nothing otherwise.
Returns end position of CNTRL "
  (interactive "p")
  (ar-th-forward 'cntrl arg (interactive-p)))

(defalias 'ar-backward-cntrl-atpt 'ar-cntrl-backward-atpt)
(defun ar-cntrl-backward-atpt (&optional arg)
  "Moves backward over CNTRL before point if any, does nothing otherwise.
Returns beginning position of CNTRL "
  (interactive "p")
  (ar-th-backward 'cntrl arg (interactive-p)))

(defalias 'ar-transpose-cntrl-atpt 'ar-cntrl-transpose-atpt)
(defun ar-cntrl-transpose-atpt (&optional arg)
  "Transposes CNTRL with CNTRL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'cntrl arg (interactive-p)))

(defalias 'ar-sort-cntrl-atpt 'ar-cntrl-sort-atpt)
(defun ar-cntrl-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts cntrls in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'cntrl reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-cntrl-atpt 'ar-cntrl-check-atpt)
(defun ar-cntrl-check-atpt ()
  "Return t if a CNTRL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-cntrl-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-cntrl-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-digit-atpt ()
  "Returns digit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'digit nil nil (interactive-p)))

(defalias 'ar-bounds-of-digit-atpt 'ar-digit-bounds-atpt)
(defun ar-digit-bounds-atpt ()
  "Returns a list, borders of digit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (interactive-p)))

(defun ar-digit-beginning-position-atpt ()
  "Returns a number, beginning position DIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'digit nil (interactive-p)))

(defun ar-digit-end-position-atpt ()
  "Returns a number, end position of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'digit nil (interactive-p)))

(defalias 'ar-beginning-of-digit-atpt 'ar-digit-beginning-atpt)
(defun ar-digit-beginning-atpt ()
  "Goto beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'digit nil (interactive-p)))

(defalias 'ar-end-of-digit-atpt 'ar-digit-end-atpt)
(defun ar-digit-end-atpt ()
  "Goto end of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'digit nil (interactive-p)))

(defalias 'ar-in-digit-p-atpt 'ar-digit-in-p-atpt)
(defun ar-digit-in-p-atpt ()
  "Returns bounds of DIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (interactive-p)))

(defalias 'ar-length-of-digit-atpt 'ar-digit-length-atpt)
(defun ar-digit-length-atpt ()
  "Returns beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'digit nil (interactive-p)))

(defalias 'ar-copy-digit-atpt 'ar-digit-copy-atpt)
(defun ar-digit-copy-atpt ()
  "Returns a copy of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'digit nil (interactive-p)))

(defalias 'ar-delete-digit-in-region 'ar-digit-delete-in-region)
(defun ar-digit-delete-in-region (beg end)
  "Deletes DIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'digit beg end (interactive-p)))

(defun ar-blok-digit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around digit.
  Returns blok or nil if no DIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'digit nil (interactive-p)))

(defalias 'ar-escape-digit-atpt 'ar-digit-escape-atpt)
(defun ar-digit-escape-atpt ()
  "Returns regexp-quoted DIGIT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'digit nil))

(defalias 'ar-doublequote-digit-atpt 'ar-digit-doublequote-atpt)
(defun ar-digit-doublequote-atpt ()
  "Doublequotes DIGIT at point if any. "
  (interactive "*")
  (ar-th-doublequote 'digit nil (interactive-p)))

(defalias 'ar-slash-digit-atpt 'ar-digit-slash-atpt)
(defun ar-digit-slash-atpt ()
  "Doublequotes DIGIT at point if any. "
  (interactive "*")
  (ar-th-slash 'digit nil (interactive-p)))

(defalias 'ar-double-backslash-digit-atpt 'ar-digit-double-backslash-atpt)
(defun ar-digit-double-backslash-atpt ()
  "Puts doubled backslashes around DIGIT at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'digit nil (interactive-p)))

(defalias 'ar-doubleslash-digit-atpt 'ar-digit-doubleslash-atpt)
(defun ar-digit-doubleslash-atpt ()
  "Puts doubled slashes around DIGIT at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'digit nil (interactive-p)))

(defalias 'ar-doubleslash-paren-digit-atpt 'ar-digit-doubleslash-paren-atpt)
(defun ar-digit-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around DIGIT at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'digit nil (interactive-p)))

(defalias 'ar-slashparen-digit-atpt 'ar-digit-slashparen-atpt)
(defun ar-digit-slashparen-atpt ()
  "Provides slashed parentheses around DIGIT at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'digit nil (interactive-p)))

(defalias 'ar-dollar-digit-atpt 'ar-digit-dollar-atpt)
(defun ar-digit-dollar-atpt ()
  "Doublequotes DIGIT at point if any. "
  (interactive "*")
  (ar-th-dollar 'digit nil (interactive-p)))

(defalias 'ar-equalize-digit-atpt 'ar-digit-equalize-atpt)
(defun ar-digit-equalize-atpt ()
  "Puts equal signs `=' around DIGIT at point if any. "
  (interactive "*")
  (ar-th-equalize 'digit nil (interactive-p)))

(defalias 'ar-greater-angle-digit-atpt 'ar-digit-greater-angle-atpt)
(defun ar-digit-greater-angle-atpt ()
  "Sets angles for DIGIT after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'digit nil (interactive-p)))

(defalias 'ar-lesser-angle-digit-atpt 'ar-digit-lesser-angle-atpt)
(defun ar-digit-lesser-angle-atpt ()
  "Sets angles for DIGIT after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'digit nil (interactive-p)))

(defalias 'ar-backslash-digit-atpt 'ar-digit-backslash-atpt)
(defun ar-digit-backslash-atpt ()
  "Backslash DIGIT at point if any. "
  (interactive "*")
  (ar-th-backslash 'digit nil (interactive-p)))

(defalias 'ar-brace-digit-atpt 'ar-digit-brace-atpt)
(defun ar-digit-brace-atpt ()
  "Braces DIGIT at point if any. "
  (interactive "*")
  (ar-th-brace 'digit nil (interactive-p)))

(defalias 'ar-bracket-digit-atpt 'ar-digit-bracket-atpt)
(defun ar-digit-bracket-atpt ()
  "Brackets DIGIT after point if any. "
  (interactive "*")
  (ar-th-bracket 'digit nil (interactive-p)))

(defun ar-comment-digit-atpt ()
  "Comments DIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'digit nil (interactive-p)))

(defun ar-commatize-digit-atpt ()
  "Put a comma after DIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'digit nil (interactive-p)))

(defun ar-quote-digit-atpt ()
  "Put a singlequote before DIGIT at point if any. "
  (interactive "*")
  (ar-th-quote 'digit nil (interactive-p)))

(defalias 'ar-hyphen-digit-atpt 'ar-digit-hyphen-atpt)
(defun ar-digit-hyphen-atpt ()
  "Puts hyphens around DIGIT at point if any. "
  (interactive "*")
  (ar-th-hyphen 'digit nil (interactive-p)))

(defalias 'ar-mark-digit-atpt 'ar-digit-mark-atpt)
(defun ar-digit-mark-atpt ()
  "Marks DIGIT at point if any. "
  (interactive)
  (ar-th-mark 'digit))

(defalias 'ar-hide-digit-atpt 'ar-digit-hide-atpt)
(defun ar-digit-hide-atpt ()
  "Hides DIGIT at point. "
  (interactive)
  (ar-th-hide 'digit))

(defalias 'ar-show-digit-atpt 'ar-digit-show-atpt)
(defun ar-digit-show-atpt ()
  "Shows hidden DIGIT at point. "
  (interactive)
  (ar-th-show 'digit))

(defalias 'ar-hide-show-digit-atpt 'ar-digit-hide-show-atpt)
(defun ar-digit-hide-show-atpt ()
  "Alternatively hides or shows DIGIT at point. "
  (interactive)
  (ar-th-hide-show 'digit))

(defalias 'ar-highlight-digit-atpt-mode 'ar-digit-highlight-atpt-mode)

(defun ar-digit-highlight-atpt-mode ()
  "Toggles digit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'digit nil (interactive-p)))

(defalias 'ar-kill-digit-atpt 'ar-digit-kill-atpt)
(defun ar-digit-kill-atpt ()
  "Kills DIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'digit nil (interactive-p)))

(defalias 'ar-kill-backward-digit-atpt 'ar-digit-kill-backward-atpt)
(defun ar-digit-kill-backward-atpt ()
  "Kills DIGIT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'digit nil (interactive-p)))

(defalias 'ar-left-right-singlequote-digit-atpt 'ar-digit-left-right-singlequote-atpt)
(defun ar-digit-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'digit nil (interactive-p)))

(defalias 'ar-parentize-digit-atpt 'ar-digit-parentize-atpt)
(defun ar-digit-parentize-atpt ()
  "Parentizes DIGIT at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'digit nil (interactive-p)))

(defalias 'ar-separate-digit-atpt 'ar-digit-separate-atpt)
(defun ar-digit-separate-atpt ()
  "Separates DIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'digit nil (interactive-p)))

(defalias 'ar-singlequote-digit-atpt 'ar-digit-singlequote-atpt)
(defun ar-digit-singlequote-atpt ()
  "Singlequotes DIGIT at point if any. "
  (interactive "*")
  (ar-th-singlequote 'digit nil (interactive-p)))

(defalias 'ar-triplequote-dq-digit-atpt 'ar-digit-triplequote-dq-atpt)
(defun ar-digit-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around digit. "
  (interactive "*")
  (ar-th-triplequote-dq 'digit nil (interactive-p)))

(defalias 'ar-triplequote-sq-digit-atpt 'ar-digit-triplequote-sq-atpt)
(defun ar-digit-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around digit. "
  (interactive "*")
  (ar-th-triplequote-sq 'digit nil (interactive-p)))

(defalias 'ar-digit-underscore-atpt 'ar-underscore-digit-atpt)
(defun ar-underscore-digit-atpt ()
  "Put underscore char around DIGIT. "
  (interactive "*")
  (ar-th-underscore 'digit nil (interactive-p)))

(defalias 'ar-digit-whitespace-atpt 'ar-whitespace-digit-atpt)
(defun ar-whitespace-digit-atpt ()
  "Put whitespace char around DIGIT. "
  (interactive "*")
  (ar-th-whitespace 'digit nil t))

(defalias 'ar-forward-digit-atpt 'ar-digit-forward-atpt)
(defun ar-digit-forward-atpt (&optional arg)
  "Moves forward over DIGIT at point if any, does nothing otherwise.
Returns end position of DIGIT "
  (interactive "p")
  (ar-th-forward 'digit arg (interactive-p)))

(defalias 'ar-backward-digit-atpt 'ar-digit-backward-atpt)
(defun ar-digit-backward-atpt (&optional arg)
  "Moves backward over DIGIT before point if any, does nothing otherwise.
Returns beginning position of DIGIT "
  (interactive "p")
  (ar-th-backward 'digit arg (interactive-p)))

(defalias 'ar-transpose-digit-atpt 'ar-digit-transpose-atpt)
(defun ar-digit-transpose-atpt (&optional arg)
  "Transposes DIGIT with DIGIT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'digit arg (interactive-p)))

(defalias 'ar-sort-digit-atpt 'ar-digit-sort-atpt)
(defun ar-digit-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts digits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'digit reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-digit-atpt 'ar-digit-check-atpt)
(defun ar-digit-check-atpt ()
  "Return t if a DIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-digit-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-digit-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-graph-atpt ()
  "Returns graph at point if any, nil otherwise. "
  (interactive)
  (ar-th 'graph nil nil (interactive-p)))

(defalias 'ar-bounds-of-graph-atpt 'ar-graph-bounds-atpt)
(defun ar-graph-bounds-atpt ()
  "Returns a list, borders of graph if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (interactive-p)))

(defun ar-graph-beginning-position-atpt ()
  "Returns a number, beginning position GRAPH at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'graph nil (interactive-p)))

(defun ar-graph-end-position-atpt ()
  "Returns a number, end position of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'graph nil (interactive-p)))

(defalias 'ar-beginning-of-graph-atpt 'ar-graph-beginning-atpt)
(defun ar-graph-beginning-atpt ()
  "Goto beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'graph nil (interactive-p)))

(defalias 'ar-end-of-graph-atpt 'ar-graph-end-atpt)
(defun ar-graph-end-atpt ()
  "Goto end of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'graph nil (interactive-p)))

(defalias 'ar-in-graph-p-atpt 'ar-graph-in-p-atpt)
(defun ar-graph-in-p-atpt ()
  "Returns bounds of GRAPH at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (interactive-p)))

(defalias 'ar-length-of-graph-atpt 'ar-graph-length-atpt)
(defun ar-graph-length-atpt ()
  "Returns beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'graph nil (interactive-p)))

(defalias 'ar-copy-graph-atpt 'ar-graph-copy-atpt)
(defun ar-graph-copy-atpt ()
  "Returns a copy of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'graph nil (interactive-p)))

(defalias 'ar-delete-graph-in-region 'ar-graph-delete-in-region)
(defun ar-graph-delete-in-region (beg end)
  "Deletes GRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'graph beg end (interactive-p)))

(defun ar-blok-graph-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around graph.
  Returns blok or nil if no GRAPH at cursor-position. "
  (interactive "*")
  (ar-th-blok 'graph nil (interactive-p)))

(defalias 'ar-escape-graph-atpt 'ar-graph-escape-atpt)
(defun ar-graph-escape-atpt ()
  "Returns regexp-quoted GRAPH at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'graph nil))

(defalias 'ar-doublequote-graph-atpt 'ar-graph-doublequote-atpt)
(defun ar-graph-doublequote-atpt ()
  "Doublequotes GRAPH at point if any. "
  (interactive "*")
  (ar-th-doublequote 'graph nil (interactive-p)))

(defalias 'ar-slash-graph-atpt 'ar-graph-slash-atpt)
(defun ar-graph-slash-atpt ()
  "Doublequotes GRAPH at point if any. "
  (interactive "*")
  (ar-th-slash 'graph nil (interactive-p)))

(defalias 'ar-double-backslash-graph-atpt 'ar-graph-double-backslash-atpt)
(defun ar-graph-double-backslash-atpt ()
  "Puts doubled backslashes around GRAPH at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'graph nil (interactive-p)))

(defalias 'ar-doubleslash-graph-atpt 'ar-graph-doubleslash-atpt)
(defun ar-graph-doubleslash-atpt ()
  "Puts doubled slashes around GRAPH at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'graph nil (interactive-p)))

(defalias 'ar-doubleslash-paren-graph-atpt 'ar-graph-doubleslash-paren-atpt)
(defun ar-graph-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around GRAPH at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'graph nil (interactive-p)))

(defalias 'ar-slashparen-graph-atpt 'ar-graph-slashparen-atpt)
(defun ar-graph-slashparen-atpt ()
  "Provides slashed parentheses around GRAPH at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'graph nil (interactive-p)))

(defalias 'ar-dollar-graph-atpt 'ar-graph-dollar-atpt)
(defun ar-graph-dollar-atpt ()
  "Doublequotes GRAPH at point if any. "
  (interactive "*")
  (ar-th-dollar 'graph nil (interactive-p)))

(defalias 'ar-equalize-graph-atpt 'ar-graph-equalize-atpt)
(defun ar-graph-equalize-atpt ()
  "Puts equal signs `=' around GRAPH at point if any. "
  (interactive "*")
  (ar-th-equalize 'graph nil (interactive-p)))

(defalias 'ar-greater-angle-graph-atpt 'ar-graph-greater-angle-atpt)
(defun ar-graph-greater-angle-atpt ()
  "Sets angles for GRAPH after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'graph nil (interactive-p)))

(defalias 'ar-lesser-angle-graph-atpt 'ar-graph-lesser-angle-atpt)
(defun ar-graph-lesser-angle-atpt ()
  "Sets angles for GRAPH after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'graph nil (interactive-p)))

(defalias 'ar-backslash-graph-atpt 'ar-graph-backslash-atpt)
(defun ar-graph-backslash-atpt ()
  "Backslash GRAPH at point if any. "
  (interactive "*")
  (ar-th-backslash 'graph nil (interactive-p)))

(defalias 'ar-brace-graph-atpt 'ar-graph-brace-atpt)
(defun ar-graph-brace-atpt ()
  "Braces GRAPH at point if any. "
  (interactive "*")
  (ar-th-brace 'graph nil (interactive-p)))

(defalias 'ar-bracket-graph-atpt 'ar-graph-bracket-atpt)
(defun ar-graph-bracket-atpt ()
  "Brackets GRAPH after point if any. "
  (interactive "*")
  (ar-th-bracket 'graph nil (interactive-p)))

(defun ar-comment-graph-atpt ()
  "Comments GRAPH at point if any. "
  (interactive "*")
  (ar-th-comment 'graph nil (interactive-p)))

(defun ar-commatize-graph-atpt ()
  "Put a comma after GRAPH at point if any. "
  (interactive "*")
  (ar-th-commatize 'graph nil (interactive-p)))

(defun ar-quote-graph-atpt ()
  "Put a singlequote before GRAPH at point if any. "
  (interactive "*")
  (ar-th-quote 'graph nil (interactive-p)))

(defalias 'ar-hyphen-graph-atpt 'ar-graph-hyphen-atpt)
(defun ar-graph-hyphen-atpt ()
  "Puts hyphens around GRAPH at point if any. "
  (interactive "*")
  (ar-th-hyphen 'graph nil (interactive-p)))

(defalias 'ar-mark-graph-atpt 'ar-graph-mark-atpt)
(defun ar-graph-mark-atpt ()
  "Marks GRAPH at point if any. "
  (interactive)
  (ar-th-mark 'graph))

(defalias 'ar-hide-graph-atpt 'ar-graph-hide-atpt)
(defun ar-graph-hide-atpt ()
  "Hides GRAPH at point. "
  (interactive)
  (ar-th-hide 'graph))

(defalias 'ar-show-graph-atpt 'ar-graph-show-atpt)
(defun ar-graph-show-atpt ()
  "Shows hidden GRAPH at point. "
  (interactive)
  (ar-th-show 'graph))

(defalias 'ar-hide-show-graph-atpt 'ar-graph-hide-show-atpt)
(defun ar-graph-hide-show-atpt ()
  "Alternatively hides or shows GRAPH at point. "
  (interactive)
  (ar-th-hide-show 'graph))

(defalias 'ar-highlight-graph-atpt-mode 'ar-graph-highlight-atpt-mode)

(defun ar-graph-highlight-atpt-mode ()
  "Toggles graph-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'graph nil (interactive-p)))

(defalias 'ar-kill-graph-atpt 'ar-graph-kill-atpt)
(defun ar-graph-kill-atpt ()
  "Kills GRAPH at point if any. "
  (interactive "*")
  (ar-th-kill 'graph nil (interactive-p)))

(defalias 'ar-kill-backward-graph-atpt 'ar-graph-kill-backward-atpt)
(defun ar-graph-kill-backward-atpt ()
  "Kills GRAPH at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'graph nil (interactive-p)))

(defalias 'ar-left-right-singlequote-graph-atpt 'ar-graph-left-right-singlequote-atpt)
(defun ar-graph-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'graph nil (interactive-p)))

(defalias 'ar-parentize-graph-atpt 'ar-graph-parentize-atpt)
(defun ar-graph-parentize-atpt ()
  "Parentizes GRAPH at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'graph nil (interactive-p)))

(defalias 'ar-separate-graph-atpt 'ar-graph-separate-atpt)
(defun ar-graph-separate-atpt ()
  "Separates GRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'graph nil (interactive-p)))

(defalias 'ar-singlequote-graph-atpt 'ar-graph-singlequote-atpt)
(defun ar-graph-singlequote-atpt ()
  "Singlequotes GRAPH at point if any. "
  (interactive "*")
  (ar-th-singlequote 'graph nil (interactive-p)))

(defalias 'ar-triplequote-dq-graph-atpt 'ar-graph-triplequote-dq-atpt)
(defun ar-graph-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around graph. "
  (interactive "*")
  (ar-th-triplequote-dq 'graph nil (interactive-p)))

(defalias 'ar-triplequote-sq-graph-atpt 'ar-graph-triplequote-sq-atpt)
(defun ar-graph-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around graph. "
  (interactive "*")
  (ar-th-triplequote-sq 'graph nil (interactive-p)))

(defalias 'ar-graph-underscore-atpt 'ar-underscore-graph-atpt)
(defun ar-underscore-graph-atpt ()
  "Put underscore char around GRAPH. "
  (interactive "*")
  (ar-th-underscore 'graph nil (interactive-p)))

(defalias 'ar-graph-whitespace-atpt 'ar-whitespace-graph-atpt)
(defun ar-whitespace-graph-atpt ()
  "Put whitespace char around GRAPH. "
  (interactive "*")
  (ar-th-whitespace 'graph nil t))

(defalias 'ar-forward-graph-atpt 'ar-graph-forward-atpt)
(defun ar-graph-forward-atpt (&optional arg)
  "Moves forward over GRAPH at point if any, does nothing otherwise.
Returns end position of GRAPH "
  (interactive "p")
  (ar-th-forward 'graph arg (interactive-p)))

(defalias 'ar-backward-graph-atpt 'ar-graph-backward-atpt)
(defun ar-graph-backward-atpt (&optional arg)
  "Moves backward over GRAPH before point if any, does nothing otherwise.
Returns beginning position of GRAPH "
  (interactive "p")
  (ar-th-backward 'graph arg (interactive-p)))

(defalias 'ar-transpose-graph-atpt 'ar-graph-transpose-atpt)
(defun ar-graph-transpose-atpt (&optional arg)
  "Transposes GRAPH with GRAPH before point if any. "
  (interactive "*p")
  (ar-th-transpose 'graph arg (interactive-p)))

(defalias 'ar-sort-graph-atpt 'ar-graph-sort-atpt)
(defun ar-graph-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts graphs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'graph reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-graph-atpt 'ar-graph-check-atpt)
(defun ar-graph-check-atpt ()
  "Return t if a GRAPH at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-graph-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-graph-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-lower-atpt ()
  "Returns lower at point if any, nil otherwise. "
  (interactive)
  (ar-th 'lower nil nil (interactive-p)))

(defalias 'ar-bounds-of-lower-atpt 'ar-lower-bounds-atpt)
(defun ar-lower-bounds-atpt ()
  "Returns a list, borders of lower if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (interactive-p)))

(defun ar-lower-beginning-position-atpt ()
  "Returns a number, beginning position LOWER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'lower nil (interactive-p)))

(defun ar-lower-end-position-atpt ()
  "Returns a number, end position of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'lower nil (interactive-p)))

(defalias 'ar-beginning-of-lower-atpt 'ar-lower-beginning-atpt)
(defun ar-lower-beginning-atpt ()
  "Goto beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'lower nil (interactive-p)))

(defalias 'ar-end-of-lower-atpt 'ar-lower-end-atpt)
(defun ar-lower-end-atpt ()
  "Goto end of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'lower nil (interactive-p)))

(defalias 'ar-in-lower-p-atpt 'ar-lower-in-p-atpt)
(defun ar-lower-in-p-atpt ()
  "Returns bounds of LOWER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (interactive-p)))

(defalias 'ar-length-of-lower-atpt 'ar-lower-length-atpt)
(defun ar-lower-length-atpt ()
  "Returns beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'lower nil (interactive-p)))

(defalias 'ar-copy-lower-atpt 'ar-lower-copy-atpt)
(defun ar-lower-copy-atpt ()
  "Returns a copy of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'lower nil (interactive-p)))

(defalias 'ar-delete-lower-in-region 'ar-lower-delete-in-region)
(defun ar-lower-delete-in-region (beg end)
  "Deletes LOWER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lower beg end (interactive-p)))

(defun ar-blok-lower-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lower.
  Returns blok or nil if no LOWER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'lower nil (interactive-p)))

(defalias 'ar-escape-lower-atpt 'ar-lower-escape-atpt)
(defun ar-lower-escape-atpt ()
  "Returns regexp-quoted LOWER at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'lower nil))

(defalias 'ar-doublequote-lower-atpt 'ar-lower-doublequote-atpt)
(defun ar-lower-doublequote-atpt ()
  "Doublequotes LOWER at point if any. "
  (interactive "*")
  (ar-th-doublequote 'lower nil (interactive-p)))

(defalias 'ar-slash-lower-atpt 'ar-lower-slash-atpt)
(defun ar-lower-slash-atpt ()
  "Doublequotes LOWER at point if any. "
  (interactive "*")
  (ar-th-slash 'lower nil (interactive-p)))

(defalias 'ar-double-backslash-lower-atpt 'ar-lower-double-backslash-atpt)
(defun ar-lower-double-backslash-atpt ()
  "Puts doubled backslashes around LOWER at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'lower nil (interactive-p)))

(defalias 'ar-doubleslash-lower-atpt 'ar-lower-doubleslash-atpt)
(defun ar-lower-doubleslash-atpt ()
  "Puts doubled slashes around LOWER at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'lower nil (interactive-p)))

(defalias 'ar-doubleslash-paren-lower-atpt 'ar-lower-doubleslash-paren-atpt)
(defun ar-lower-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around LOWER at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'lower nil (interactive-p)))

(defalias 'ar-slashparen-lower-atpt 'ar-lower-slashparen-atpt)
(defun ar-lower-slashparen-atpt ()
  "Provides slashed parentheses around LOWER at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'lower nil (interactive-p)))

(defalias 'ar-dollar-lower-atpt 'ar-lower-dollar-atpt)
(defun ar-lower-dollar-atpt ()
  "Doublequotes LOWER at point if any. "
  (interactive "*")
  (ar-th-dollar 'lower nil (interactive-p)))

(defalias 'ar-equalize-lower-atpt 'ar-lower-equalize-atpt)
(defun ar-lower-equalize-atpt ()
  "Puts equal signs `=' around LOWER at point if any. "
  (interactive "*")
  (ar-th-equalize 'lower nil (interactive-p)))

(defalias 'ar-greater-angle-lower-atpt 'ar-lower-greater-angle-atpt)
(defun ar-lower-greater-angle-atpt ()
  "Sets angles for LOWER after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'lower nil (interactive-p)))

(defalias 'ar-lesser-angle-lower-atpt 'ar-lower-lesser-angle-atpt)
(defun ar-lower-lesser-angle-atpt ()
  "Sets angles for LOWER after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'lower nil (interactive-p)))

(defalias 'ar-backslash-lower-atpt 'ar-lower-backslash-atpt)
(defun ar-lower-backslash-atpt ()
  "Backslash LOWER at point if any. "
  (interactive "*")
  (ar-th-backslash 'lower nil (interactive-p)))

(defalias 'ar-brace-lower-atpt 'ar-lower-brace-atpt)
(defun ar-lower-brace-atpt ()
  "Braces LOWER at point if any. "
  (interactive "*")
  (ar-th-brace 'lower nil (interactive-p)))

(defalias 'ar-bracket-lower-atpt 'ar-lower-bracket-atpt)
(defun ar-lower-bracket-atpt ()
  "Brackets LOWER after point if any. "
  (interactive "*")
  (ar-th-bracket 'lower nil (interactive-p)))

(defun ar-comment-lower-atpt ()
  "Comments LOWER at point if any. "
  (interactive "*")
  (ar-th-comment 'lower nil (interactive-p)))

(defun ar-commatize-lower-atpt ()
  "Put a comma after LOWER at point if any. "
  (interactive "*")
  (ar-th-commatize 'lower nil (interactive-p)))

(defun ar-quote-lower-atpt ()
  "Put a singlequote before LOWER at point if any. "
  (interactive "*")
  (ar-th-quote 'lower nil (interactive-p)))

(defalias 'ar-hyphen-lower-atpt 'ar-lower-hyphen-atpt)
(defun ar-lower-hyphen-atpt ()
  "Puts hyphens around LOWER at point if any. "
  (interactive "*")
  (ar-th-hyphen 'lower nil (interactive-p)))

(defalias 'ar-mark-lower-atpt 'ar-lower-mark-atpt)
(defun ar-lower-mark-atpt ()
  "Marks LOWER at point if any. "
  (interactive)
  (ar-th-mark 'lower))

(defalias 'ar-hide-lower-atpt 'ar-lower-hide-atpt)
(defun ar-lower-hide-atpt ()
  "Hides LOWER at point. "
  (interactive)
  (ar-th-hide 'lower))

(defalias 'ar-show-lower-atpt 'ar-lower-show-atpt)
(defun ar-lower-show-atpt ()
  "Shows hidden LOWER at point. "
  (interactive)
  (ar-th-show 'lower))

(defalias 'ar-hide-show-lower-atpt 'ar-lower-hide-show-atpt)
(defun ar-lower-hide-show-atpt ()
  "Alternatively hides or shows LOWER at point. "
  (interactive)
  (ar-th-hide-show 'lower))

(defalias 'ar-highlight-lower-atpt-mode 'ar-lower-highlight-atpt-mode)

(defun ar-lower-highlight-atpt-mode ()
  "Toggles lower-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'lower nil (interactive-p)))

(defalias 'ar-kill-lower-atpt 'ar-lower-kill-atpt)
(defun ar-lower-kill-atpt ()
  "Kills LOWER at point if any. "
  (interactive "*")
  (ar-th-kill 'lower nil (interactive-p)))

(defalias 'ar-kill-backward-lower-atpt 'ar-lower-kill-backward-atpt)
(defun ar-lower-kill-backward-atpt ()
  "Kills LOWER at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'lower nil (interactive-p)))

(defalias 'ar-left-right-singlequote-lower-atpt 'ar-lower-left-right-singlequote-atpt)
(defun ar-lower-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'lower nil (interactive-p)))

(defalias 'ar-parentize-lower-atpt 'ar-lower-parentize-atpt)
(defun ar-lower-parentize-atpt ()
  "Parentizes LOWER at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'lower nil (interactive-p)))

(defalias 'ar-separate-lower-atpt 'ar-lower-separate-atpt)
(defun ar-lower-separate-atpt ()
  "Separates LOWER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'lower nil (interactive-p)))

(defalias 'ar-singlequote-lower-atpt 'ar-lower-singlequote-atpt)
(defun ar-lower-singlequote-atpt ()
  "Singlequotes LOWER at point if any. "
  (interactive "*")
  (ar-th-singlequote 'lower nil (interactive-p)))

(defalias 'ar-triplequote-dq-lower-atpt 'ar-lower-triplequote-dq-atpt)
(defun ar-lower-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around lower. "
  (interactive "*")
  (ar-th-triplequote-dq 'lower nil (interactive-p)))

(defalias 'ar-triplequote-sq-lower-atpt 'ar-lower-triplequote-sq-atpt)
(defun ar-lower-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around lower. "
  (interactive "*")
  (ar-th-triplequote-sq 'lower nil (interactive-p)))

(defalias 'ar-lower-underscore-atpt 'ar-underscore-lower-atpt)
(defun ar-underscore-lower-atpt ()
  "Put underscore char around LOWER. "
  (interactive "*")
  (ar-th-underscore 'lower nil (interactive-p)))

(defalias 'ar-lower-whitespace-atpt 'ar-whitespace-lower-atpt)
(defun ar-whitespace-lower-atpt ()
  "Put whitespace char around LOWER. "
  (interactive "*")
  (ar-th-whitespace 'lower nil t))

(defalias 'ar-forward-lower-atpt 'ar-lower-forward-atpt)
(defun ar-lower-forward-atpt (&optional arg)
  "Moves forward over LOWER at point if any, does nothing otherwise.
Returns end position of LOWER "
  (interactive "p")
  (ar-th-forward 'lower arg (interactive-p)))

(defalias 'ar-backward-lower-atpt 'ar-lower-backward-atpt)
(defun ar-lower-backward-atpt (&optional arg)
  "Moves backward over LOWER before point if any, does nothing otherwise.
Returns beginning position of LOWER "
  (interactive "p")
  (ar-th-backward 'lower arg (interactive-p)))

(defalias 'ar-transpose-lower-atpt 'ar-lower-transpose-atpt)
(defun ar-lower-transpose-atpt (&optional arg)
  "Transposes LOWER with LOWER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lower arg (interactive-p)))

(defalias 'ar-sort-lower-atpt 'ar-lower-sort-atpt)
(defun ar-lower-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lowers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'lower reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lower-atpt 'ar-lower-check-atpt)
(defun ar-lower-check-atpt ()
  "Return t if a LOWER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lower-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-lower-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-nonascii-atpt ()
  "Returns nonascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'nonascii nil nil (interactive-p)))

(defalias 'ar-bounds-of-nonascii-atpt 'ar-nonascii-bounds-atpt)
(defun ar-nonascii-bounds-atpt ()
  "Returns a list, borders of nonascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (interactive-p)))

(defun ar-nonascii-beginning-position-atpt ()
  "Returns a number, beginning position NONASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'nonascii nil (interactive-p)))

(defun ar-nonascii-end-position-atpt ()
  "Returns a number, end position of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'nonascii nil (interactive-p)))

(defalias 'ar-beginning-of-nonascii-atpt 'ar-nonascii-beginning-atpt)
(defun ar-nonascii-beginning-atpt ()
  "Goto beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'nonascii nil (interactive-p)))

(defalias 'ar-end-of-nonascii-atpt 'ar-nonascii-end-atpt)
(defun ar-nonascii-end-atpt ()
  "Goto end of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'nonascii nil (interactive-p)))

(defalias 'ar-in-nonascii-p-atpt 'ar-nonascii-in-p-atpt)
(defun ar-nonascii-in-p-atpt ()
  "Returns bounds of NONASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (interactive-p)))

(defalias 'ar-length-of-nonascii-atpt 'ar-nonascii-length-atpt)
(defun ar-nonascii-length-atpt ()
  "Returns beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'nonascii nil (interactive-p)))

(defalias 'ar-copy-nonascii-atpt 'ar-nonascii-copy-atpt)
(defun ar-nonascii-copy-atpt ()
  "Returns a copy of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'nonascii nil (interactive-p)))

(defalias 'ar-delete-nonascii-in-region 'ar-nonascii-delete-in-region)
(defun ar-nonascii-delete-in-region (beg end)
  "Deletes NONASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'nonascii beg end (interactive-p)))

(defun ar-blok-nonascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around nonascii.
  Returns blok or nil if no NONASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'nonascii nil (interactive-p)))

(defalias 'ar-escape-nonascii-atpt 'ar-nonascii-escape-atpt)
(defun ar-nonascii-escape-atpt ()
  "Returns regexp-quoted NONASCII at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'nonascii nil))

(defalias 'ar-doublequote-nonascii-atpt 'ar-nonascii-doublequote-atpt)
(defun ar-nonascii-doublequote-atpt ()
  "Doublequotes NONASCII at point if any. "
  (interactive "*")
  (ar-th-doublequote 'nonascii nil (interactive-p)))

(defalias 'ar-slash-nonascii-atpt 'ar-nonascii-slash-atpt)
(defun ar-nonascii-slash-atpt ()
  "Doublequotes NONASCII at point if any. "
  (interactive "*")
  (ar-th-slash 'nonascii nil (interactive-p)))

(defalias 'ar-double-backslash-nonascii-atpt 'ar-nonascii-double-backslash-atpt)
(defun ar-nonascii-double-backslash-atpt ()
  "Puts doubled backslashes around NONASCII at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'nonascii nil (interactive-p)))

(defalias 'ar-doubleslash-nonascii-atpt 'ar-nonascii-doubleslash-atpt)
(defun ar-nonascii-doubleslash-atpt ()
  "Puts doubled slashes around NONASCII at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'nonascii nil (interactive-p)))

(defalias 'ar-doubleslash-paren-nonascii-atpt 'ar-nonascii-doubleslash-paren-atpt)
(defun ar-nonascii-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around NONASCII at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'nonascii nil (interactive-p)))

(defalias 'ar-slashparen-nonascii-atpt 'ar-nonascii-slashparen-atpt)
(defun ar-nonascii-slashparen-atpt ()
  "Provides slashed parentheses around NONASCII at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'nonascii nil (interactive-p)))

(defalias 'ar-dollar-nonascii-atpt 'ar-nonascii-dollar-atpt)
(defun ar-nonascii-dollar-atpt ()
  "Doublequotes NONASCII at point if any. "
  (interactive "*")
  (ar-th-dollar 'nonascii nil (interactive-p)))

(defalias 'ar-equalize-nonascii-atpt 'ar-nonascii-equalize-atpt)
(defun ar-nonascii-equalize-atpt ()
  "Puts equal signs `=' around NONASCII at point if any. "
  (interactive "*")
  (ar-th-equalize 'nonascii nil (interactive-p)))

(defalias 'ar-greater-angle-nonascii-atpt 'ar-nonascii-greater-angle-atpt)
(defun ar-nonascii-greater-angle-atpt ()
  "Sets angles for NONASCII after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'nonascii nil (interactive-p)))

(defalias 'ar-lesser-angle-nonascii-atpt 'ar-nonascii-lesser-angle-atpt)
(defun ar-nonascii-lesser-angle-atpt ()
  "Sets angles for NONASCII after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'nonascii nil (interactive-p)))

(defalias 'ar-backslash-nonascii-atpt 'ar-nonascii-backslash-atpt)
(defun ar-nonascii-backslash-atpt ()
  "Backslash NONASCII at point if any. "
  (interactive "*")
  (ar-th-backslash 'nonascii nil (interactive-p)))

(defalias 'ar-brace-nonascii-atpt 'ar-nonascii-brace-atpt)
(defun ar-nonascii-brace-atpt ()
  "Braces NONASCII at point if any. "
  (interactive "*")
  (ar-th-brace 'nonascii nil (interactive-p)))

(defalias 'ar-bracket-nonascii-atpt 'ar-nonascii-bracket-atpt)
(defun ar-nonascii-bracket-atpt ()
  "Brackets NONASCII after point if any. "
  (interactive "*")
  (ar-th-bracket 'nonascii nil (interactive-p)))

(defun ar-comment-nonascii-atpt ()
  "Comments NONASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'nonascii nil (interactive-p)))

(defun ar-commatize-nonascii-atpt ()
  "Put a comma after NONASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'nonascii nil (interactive-p)))

(defun ar-quote-nonascii-atpt ()
  "Put a singlequote before NONASCII at point if any. "
  (interactive "*")
  (ar-th-quote 'nonascii nil (interactive-p)))

(defalias 'ar-hyphen-nonascii-atpt 'ar-nonascii-hyphen-atpt)
(defun ar-nonascii-hyphen-atpt ()
  "Puts hyphens around NONASCII at point if any. "
  (interactive "*")
  (ar-th-hyphen 'nonascii nil (interactive-p)))

(defalias 'ar-mark-nonascii-atpt 'ar-nonascii-mark-atpt)
(defun ar-nonascii-mark-atpt ()
  "Marks NONASCII at point if any. "
  (interactive)
  (ar-th-mark 'nonascii))

(defalias 'ar-hide-nonascii-atpt 'ar-nonascii-hide-atpt)
(defun ar-nonascii-hide-atpt ()
  "Hides NONASCII at point. "
  (interactive)
  (ar-th-hide 'nonascii))

(defalias 'ar-show-nonascii-atpt 'ar-nonascii-show-atpt)
(defun ar-nonascii-show-atpt ()
  "Shows hidden NONASCII at point. "
  (interactive)
  (ar-th-show 'nonascii))

(defalias 'ar-hide-show-nonascii-atpt 'ar-nonascii-hide-show-atpt)
(defun ar-nonascii-hide-show-atpt ()
  "Alternatively hides or shows NONASCII at point. "
  (interactive)
  (ar-th-hide-show 'nonascii))

(defalias 'ar-highlight-nonascii-atpt-mode 'ar-nonascii-highlight-atpt-mode)

(defun ar-nonascii-highlight-atpt-mode ()
  "Toggles nonascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'nonascii nil (interactive-p)))

(defalias 'ar-kill-nonascii-atpt 'ar-nonascii-kill-atpt)
(defun ar-nonascii-kill-atpt ()
  "Kills NONASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'nonascii nil (interactive-p)))

(defalias 'ar-kill-backward-nonascii-atpt 'ar-nonascii-kill-backward-atpt)
(defun ar-nonascii-kill-backward-atpt ()
  "Kills NONASCII at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'nonascii nil (interactive-p)))

(defalias 'ar-left-right-singlequote-nonascii-atpt 'ar-nonascii-left-right-singlequote-atpt)
(defun ar-nonascii-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'nonascii nil (interactive-p)))

(defalias 'ar-parentize-nonascii-atpt 'ar-nonascii-parentize-atpt)
(defun ar-nonascii-parentize-atpt ()
  "Parentizes NONASCII at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'nonascii nil (interactive-p)))

(defalias 'ar-separate-nonascii-atpt 'ar-nonascii-separate-atpt)
(defun ar-nonascii-separate-atpt ()
  "Separates NONASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'nonascii nil (interactive-p)))

(defalias 'ar-singlequote-nonascii-atpt 'ar-nonascii-singlequote-atpt)
(defun ar-nonascii-singlequote-atpt ()
  "Singlequotes NONASCII at point if any. "
  (interactive "*")
  (ar-th-singlequote 'nonascii nil (interactive-p)))

(defalias 'ar-triplequote-dq-nonascii-atpt 'ar-nonascii-triplequote-dq-atpt)
(defun ar-nonascii-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequote-dq 'nonascii nil (interactive-p)))

(defalias 'ar-triplequote-sq-nonascii-atpt 'ar-nonascii-triplequote-sq-atpt)
(defun ar-nonascii-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequote-sq 'nonascii nil (interactive-p)))

(defalias 'ar-nonascii-underscore-atpt 'ar-underscore-nonascii-atpt)
(defun ar-underscore-nonascii-atpt ()
  "Put underscore char around NONASCII. "
  (interactive "*")
  (ar-th-underscore 'nonascii nil (interactive-p)))

(defalias 'ar-nonascii-whitespace-atpt 'ar-whitespace-nonascii-atpt)
(defun ar-whitespace-nonascii-atpt ()
  "Put whitespace char around NONASCII. "
  (interactive "*")
  (ar-th-whitespace 'nonascii nil t))

(defalias 'ar-forward-nonascii-atpt 'ar-nonascii-forward-atpt)
(defun ar-nonascii-forward-atpt (&optional arg)
  "Moves forward over NONASCII at point if any, does nothing otherwise.
Returns end position of NONASCII "
  (interactive "p")
  (ar-th-forward 'nonascii arg (interactive-p)))

(defalias 'ar-backward-nonascii-atpt 'ar-nonascii-backward-atpt)
(defun ar-nonascii-backward-atpt (&optional arg)
  "Moves backward over NONASCII before point if any, does nothing otherwise.
Returns beginning position of NONASCII "
  (interactive "p")
  (ar-th-backward 'nonascii arg (interactive-p)))

(defalias 'ar-transpose-nonascii-atpt 'ar-nonascii-transpose-atpt)
(defun ar-nonascii-transpose-atpt (&optional arg)
  "Transposes NONASCII with NONASCII before point if any. "
  (interactive "*p")
  (ar-th-transpose 'nonascii arg (interactive-p)))

(defalias 'ar-sort-nonascii-atpt 'ar-nonascii-sort-atpt)
(defun ar-nonascii-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts nonasciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'nonascii reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-nonascii-atpt 'ar-nonascii-check-atpt)
(defun ar-nonascii-check-atpt ()
  "Return t if a NONASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-nonascii-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-nonascii-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-print-atpt ()
  "Returns print at point if any, nil otherwise. "
  (interactive)
  (ar-th 'print nil nil (interactive-p)))

(defalias 'ar-bounds-of-print-atpt 'ar-print-bounds-atpt)
(defun ar-print-bounds-atpt ()
  "Returns a list, borders of print if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (interactive-p)))

(defun ar-print-beginning-position-atpt ()
  "Returns a number, beginning position PRINT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'print nil (interactive-p)))

(defun ar-print-end-position-atpt ()
  "Returns a number, end position of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'print nil (interactive-p)))

(defalias 'ar-beginning-of-print-atpt 'ar-print-beginning-atpt)
(defun ar-print-beginning-atpt ()
  "Goto beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'print nil (interactive-p)))

(defalias 'ar-end-of-print-atpt 'ar-print-end-atpt)
(defun ar-print-end-atpt ()
  "Goto end of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'print nil (interactive-p)))

(defalias 'ar-in-print-p-atpt 'ar-print-in-p-atpt)
(defun ar-print-in-p-atpt ()
  "Returns bounds of PRINT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (interactive-p)))

(defalias 'ar-length-of-print-atpt 'ar-print-length-atpt)
(defun ar-print-length-atpt ()
  "Returns beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'print nil (interactive-p)))

(defalias 'ar-copy-print-atpt 'ar-print-copy-atpt)
(defun ar-print-copy-atpt ()
  "Returns a copy of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'print nil (interactive-p)))

(defalias 'ar-delete-print-in-region 'ar-print-delete-in-region)
(defun ar-print-delete-in-region (beg end)
  "Deletes PRINT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'print beg end (interactive-p)))

(defun ar-blok-print-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around print.
  Returns blok or nil if no PRINT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'print nil (interactive-p)))

(defalias 'ar-escape-print-atpt 'ar-print-escape-atpt)
(defun ar-print-escape-atpt ()
  "Returns regexp-quoted PRINT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'print nil))

(defalias 'ar-doublequote-print-atpt 'ar-print-doublequote-atpt)
(defun ar-print-doublequote-atpt ()
  "Doublequotes PRINT at point if any. "
  (interactive "*")
  (ar-th-doublequote 'print nil (interactive-p)))

(defalias 'ar-slash-print-atpt 'ar-print-slash-atpt)
(defun ar-print-slash-atpt ()
  "Doublequotes PRINT at point if any. "
  (interactive "*")
  (ar-th-slash 'print nil (interactive-p)))

(defalias 'ar-double-backslash-print-atpt 'ar-print-double-backslash-atpt)
(defun ar-print-double-backslash-atpt ()
  "Puts doubled backslashes around PRINT at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'print nil (interactive-p)))

(defalias 'ar-doubleslash-print-atpt 'ar-print-doubleslash-atpt)
(defun ar-print-doubleslash-atpt ()
  "Puts doubled slashes around PRINT at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'print nil (interactive-p)))

(defalias 'ar-doubleslash-paren-print-atpt 'ar-print-doubleslash-paren-atpt)
(defun ar-print-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around PRINT at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'print nil (interactive-p)))

(defalias 'ar-slashparen-print-atpt 'ar-print-slashparen-atpt)
(defun ar-print-slashparen-atpt ()
  "Provides slashed parentheses around PRINT at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'print nil (interactive-p)))

(defalias 'ar-dollar-print-atpt 'ar-print-dollar-atpt)
(defun ar-print-dollar-atpt ()
  "Doublequotes PRINT at point if any. "
  (interactive "*")
  (ar-th-dollar 'print nil (interactive-p)))

(defalias 'ar-equalize-print-atpt 'ar-print-equalize-atpt)
(defun ar-print-equalize-atpt ()
  "Puts equal signs `=' around PRINT at point if any. "
  (interactive "*")
  (ar-th-equalize 'print nil (interactive-p)))

(defalias 'ar-greater-angle-print-atpt 'ar-print-greater-angle-atpt)
(defun ar-print-greater-angle-atpt ()
  "Sets angles for PRINT after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'print nil (interactive-p)))

(defalias 'ar-lesser-angle-print-atpt 'ar-print-lesser-angle-atpt)
(defun ar-print-lesser-angle-atpt ()
  "Sets angles for PRINT after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'print nil (interactive-p)))

(defalias 'ar-backslash-print-atpt 'ar-print-backslash-atpt)
(defun ar-print-backslash-atpt ()
  "Backslash PRINT at point if any. "
  (interactive "*")
  (ar-th-backslash 'print nil (interactive-p)))

(defalias 'ar-brace-print-atpt 'ar-print-brace-atpt)
(defun ar-print-brace-atpt ()
  "Braces PRINT at point if any. "
  (interactive "*")
  (ar-th-brace 'print nil (interactive-p)))

(defalias 'ar-bracket-print-atpt 'ar-print-bracket-atpt)
(defun ar-print-bracket-atpt ()
  "Brackets PRINT after point if any. "
  (interactive "*")
  (ar-th-bracket 'print nil (interactive-p)))

(defun ar-comment-print-atpt ()
  "Comments PRINT at point if any. "
  (interactive "*")
  (ar-th-comment 'print nil (interactive-p)))

(defun ar-commatize-print-atpt ()
  "Put a comma after PRINT at point if any. "
  (interactive "*")
  (ar-th-commatize 'print nil (interactive-p)))

(defun ar-quote-print-atpt ()
  "Put a singlequote before PRINT at point if any. "
  (interactive "*")
  (ar-th-quote 'print nil (interactive-p)))

(defalias 'ar-hyphen-print-atpt 'ar-print-hyphen-atpt)
(defun ar-print-hyphen-atpt ()
  "Puts hyphens around PRINT at point if any. "
  (interactive "*")
  (ar-th-hyphen 'print nil (interactive-p)))

(defalias 'ar-mark-print-atpt 'ar-print-mark-atpt)
(defun ar-print-mark-atpt ()
  "Marks PRINT at point if any. "
  (interactive)
  (ar-th-mark 'print))

(defalias 'ar-hide-print-atpt 'ar-print-hide-atpt)
(defun ar-print-hide-atpt ()
  "Hides PRINT at point. "
  (interactive)
  (ar-th-hide 'print))

(defalias 'ar-show-print-atpt 'ar-print-show-atpt)
(defun ar-print-show-atpt ()
  "Shows hidden PRINT at point. "
  (interactive)
  (ar-th-show 'print))

(defalias 'ar-hide-show-print-atpt 'ar-print-hide-show-atpt)
(defun ar-print-hide-show-atpt ()
  "Alternatively hides or shows PRINT at point. "
  (interactive)
  (ar-th-hide-show 'print))

(defalias 'ar-highlight-print-atpt-mode 'ar-print-highlight-atpt-mode)

(defun ar-print-highlight-atpt-mode ()
  "Toggles print-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'print nil (interactive-p)))

(defalias 'ar-kill-print-atpt 'ar-print-kill-atpt)
(defun ar-print-kill-atpt ()
  "Kills PRINT at point if any. "
  (interactive "*")
  (ar-th-kill 'print nil (interactive-p)))

(defalias 'ar-kill-backward-print-atpt 'ar-print-kill-backward-atpt)
(defun ar-print-kill-backward-atpt ()
  "Kills PRINT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'print nil (interactive-p)))

(defalias 'ar-left-right-singlequote-print-atpt 'ar-print-left-right-singlequote-atpt)
(defun ar-print-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'print nil (interactive-p)))

(defalias 'ar-parentize-print-atpt 'ar-print-parentize-atpt)
(defun ar-print-parentize-atpt ()
  "Parentizes PRINT at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'print nil (interactive-p)))

(defalias 'ar-separate-print-atpt 'ar-print-separate-atpt)
(defun ar-print-separate-atpt ()
  "Separates PRINT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'print nil (interactive-p)))

(defalias 'ar-singlequote-print-atpt 'ar-print-singlequote-atpt)
(defun ar-print-singlequote-atpt ()
  "Singlequotes PRINT at point if any. "
  (interactive "*")
  (ar-th-singlequote 'print nil (interactive-p)))

(defalias 'ar-triplequote-dq-print-atpt 'ar-print-triplequote-dq-atpt)
(defun ar-print-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around print. "
  (interactive "*")
  (ar-th-triplequote-dq 'print nil (interactive-p)))

(defalias 'ar-triplequote-sq-print-atpt 'ar-print-triplequote-sq-atpt)
(defun ar-print-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around print. "
  (interactive "*")
  (ar-th-triplequote-sq 'print nil (interactive-p)))

(defalias 'ar-print-underscore-atpt 'ar-underscore-print-atpt)
(defun ar-underscore-print-atpt ()
  "Put underscore char around PRINT. "
  (interactive "*")
  (ar-th-underscore 'print nil (interactive-p)))

(defalias 'ar-print-whitespace-atpt 'ar-whitespace-print-atpt)
(defun ar-whitespace-print-atpt ()
  "Put whitespace char around PRINT. "
  (interactive "*")
  (ar-th-whitespace 'print nil t))

(defalias 'ar-forward-print-atpt 'ar-print-forward-atpt)
(defun ar-print-forward-atpt (&optional arg)
  "Moves forward over PRINT at point if any, does nothing otherwise.
Returns end position of PRINT "
  (interactive "p")
  (ar-th-forward 'print arg (interactive-p)))

(defalias 'ar-backward-print-atpt 'ar-print-backward-atpt)
(defun ar-print-backward-atpt (&optional arg)
  "Moves backward over PRINT before point if any, does nothing otherwise.
Returns beginning position of PRINT "
  (interactive "p")
  (ar-th-backward 'print arg (interactive-p)))

(defalias 'ar-transpose-print-atpt 'ar-print-transpose-atpt)
(defun ar-print-transpose-atpt (&optional arg)
  "Transposes PRINT with PRINT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'print arg (interactive-p)))

(defalias 'ar-sort-print-atpt 'ar-print-sort-atpt)
(defun ar-print-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts prints in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'print reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-print-atpt 'ar-print-check-atpt)
(defun ar-print-check-atpt ()
  "Return t if a PRINT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-print-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-print-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-punct-atpt ()
  "Returns punct at point if any, nil otherwise. "
  (interactive)
  (ar-th 'punct nil nil (interactive-p)))

(defalias 'ar-bounds-of-punct-atpt 'ar-punct-bounds-atpt)
(defun ar-punct-bounds-atpt ()
  "Returns a list, borders of punct if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (interactive-p)))

(defun ar-punct-beginning-position-atpt ()
  "Returns a number, beginning position PUNCT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'punct nil (interactive-p)))

(defun ar-punct-end-position-atpt ()
  "Returns a number, end position of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'punct nil (interactive-p)))

(defalias 'ar-beginning-of-punct-atpt 'ar-punct-beginning-atpt)
(defun ar-punct-beginning-atpt ()
  "Goto beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'punct nil (interactive-p)))

(defalias 'ar-end-of-punct-atpt 'ar-punct-end-atpt)
(defun ar-punct-end-atpt ()
  "Goto end of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'punct nil (interactive-p)))

(defalias 'ar-in-punct-p-atpt 'ar-punct-in-p-atpt)
(defun ar-punct-in-p-atpt ()
  "Returns bounds of PUNCT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (interactive-p)))

(defalias 'ar-length-of-punct-atpt 'ar-punct-length-atpt)
(defun ar-punct-length-atpt ()
  "Returns beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'punct nil (interactive-p)))

(defalias 'ar-copy-punct-atpt 'ar-punct-copy-atpt)
(defun ar-punct-copy-atpt ()
  "Returns a copy of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'punct nil (interactive-p)))

(defalias 'ar-delete-punct-in-region 'ar-punct-delete-in-region)
(defun ar-punct-delete-in-region (beg end)
  "Deletes PUNCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'punct beg end (interactive-p)))

(defun ar-blok-punct-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around punct.
  Returns blok or nil if no PUNCT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'punct nil (interactive-p)))

(defalias 'ar-escape-punct-atpt 'ar-punct-escape-atpt)
(defun ar-punct-escape-atpt ()
  "Returns regexp-quoted PUNCT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'punct nil))

(defalias 'ar-doublequote-punct-atpt 'ar-punct-doublequote-atpt)
(defun ar-punct-doublequote-atpt ()
  "Doublequotes PUNCT at point if any. "
  (interactive "*")
  (ar-th-doublequote 'punct nil (interactive-p)))

(defalias 'ar-slash-punct-atpt 'ar-punct-slash-atpt)
(defun ar-punct-slash-atpt ()
  "Doublequotes PUNCT at point if any. "
  (interactive "*")
  (ar-th-slash 'punct nil (interactive-p)))

(defalias 'ar-double-backslash-punct-atpt 'ar-punct-double-backslash-atpt)
(defun ar-punct-double-backslash-atpt ()
  "Puts doubled backslashes around PUNCT at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'punct nil (interactive-p)))

(defalias 'ar-doubleslash-punct-atpt 'ar-punct-doubleslash-atpt)
(defun ar-punct-doubleslash-atpt ()
  "Puts doubled slashes around PUNCT at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'punct nil (interactive-p)))

(defalias 'ar-doubleslash-paren-punct-atpt 'ar-punct-doubleslash-paren-atpt)
(defun ar-punct-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around PUNCT at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'punct nil (interactive-p)))

(defalias 'ar-slashparen-punct-atpt 'ar-punct-slashparen-atpt)
(defun ar-punct-slashparen-atpt ()
  "Provides slashed parentheses around PUNCT at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'punct nil (interactive-p)))

(defalias 'ar-dollar-punct-atpt 'ar-punct-dollar-atpt)
(defun ar-punct-dollar-atpt ()
  "Doublequotes PUNCT at point if any. "
  (interactive "*")
  (ar-th-dollar 'punct nil (interactive-p)))

(defalias 'ar-equalize-punct-atpt 'ar-punct-equalize-atpt)
(defun ar-punct-equalize-atpt ()
  "Puts equal signs `=' around PUNCT at point if any. "
  (interactive "*")
  (ar-th-equalize 'punct nil (interactive-p)))

(defalias 'ar-greater-angle-punct-atpt 'ar-punct-greater-angle-atpt)
(defun ar-punct-greater-angle-atpt ()
  "Sets angles for PUNCT after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'punct nil (interactive-p)))

(defalias 'ar-lesser-angle-punct-atpt 'ar-punct-lesser-angle-atpt)
(defun ar-punct-lesser-angle-atpt ()
  "Sets angles for PUNCT after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'punct nil (interactive-p)))

(defalias 'ar-backslash-punct-atpt 'ar-punct-backslash-atpt)
(defun ar-punct-backslash-atpt ()
  "Backslash PUNCT at point if any. "
  (interactive "*")
  (ar-th-backslash 'punct nil (interactive-p)))

(defalias 'ar-brace-punct-atpt 'ar-punct-brace-atpt)
(defun ar-punct-brace-atpt ()
  "Braces PUNCT at point if any. "
  (interactive "*")
  (ar-th-brace 'punct nil (interactive-p)))

(defalias 'ar-bracket-punct-atpt 'ar-punct-bracket-atpt)
(defun ar-punct-bracket-atpt ()
  "Brackets PUNCT after point if any. "
  (interactive "*")
  (ar-th-bracket 'punct nil (interactive-p)))

(defun ar-comment-punct-atpt ()
  "Comments PUNCT at point if any. "
  (interactive "*")
  (ar-th-comment 'punct nil (interactive-p)))

(defun ar-commatize-punct-atpt ()
  "Put a comma after PUNCT at point if any. "
  (interactive "*")
  (ar-th-commatize 'punct nil (interactive-p)))

(defun ar-quote-punct-atpt ()
  "Put a singlequote before PUNCT at point if any. "
  (interactive "*")
  (ar-th-quote 'punct nil (interactive-p)))

(defalias 'ar-hyphen-punct-atpt 'ar-punct-hyphen-atpt)
(defun ar-punct-hyphen-atpt ()
  "Puts hyphens around PUNCT at point if any. "
  (interactive "*")
  (ar-th-hyphen 'punct nil (interactive-p)))

(defalias 'ar-mark-punct-atpt 'ar-punct-mark-atpt)
(defun ar-punct-mark-atpt ()
  "Marks PUNCT at point if any. "
  (interactive)
  (ar-th-mark 'punct))

(defalias 'ar-hide-punct-atpt 'ar-punct-hide-atpt)
(defun ar-punct-hide-atpt ()
  "Hides PUNCT at point. "
  (interactive)
  (ar-th-hide 'punct))

(defalias 'ar-show-punct-atpt 'ar-punct-show-atpt)
(defun ar-punct-show-atpt ()
  "Shows hidden PUNCT at point. "
  (interactive)
  (ar-th-show 'punct))

(defalias 'ar-hide-show-punct-atpt 'ar-punct-hide-show-atpt)
(defun ar-punct-hide-show-atpt ()
  "Alternatively hides or shows PUNCT at point. "
  (interactive)
  (ar-th-hide-show 'punct))

(defalias 'ar-highlight-punct-atpt-mode 'ar-punct-highlight-atpt-mode)

(defun ar-punct-highlight-atpt-mode ()
  "Toggles punct-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'punct nil (interactive-p)))

(defalias 'ar-kill-punct-atpt 'ar-punct-kill-atpt)
(defun ar-punct-kill-atpt ()
  "Kills PUNCT at point if any. "
  (interactive "*")
  (ar-th-kill 'punct nil (interactive-p)))

(defalias 'ar-kill-backward-punct-atpt 'ar-punct-kill-backward-atpt)
(defun ar-punct-kill-backward-atpt ()
  "Kills PUNCT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'punct nil (interactive-p)))

(defalias 'ar-left-right-singlequote-punct-atpt 'ar-punct-left-right-singlequote-atpt)
(defun ar-punct-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'punct nil (interactive-p)))

(defalias 'ar-parentize-punct-atpt 'ar-punct-parentize-atpt)
(defun ar-punct-parentize-atpt ()
  "Parentizes PUNCT at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'punct nil (interactive-p)))

(defalias 'ar-separate-punct-atpt 'ar-punct-separate-atpt)
(defun ar-punct-separate-atpt ()
  "Separates PUNCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'punct nil (interactive-p)))

(defalias 'ar-singlequote-punct-atpt 'ar-punct-singlequote-atpt)
(defun ar-punct-singlequote-atpt ()
  "Singlequotes PUNCT at point if any. "
  (interactive "*")
  (ar-th-singlequote 'punct nil (interactive-p)))

(defalias 'ar-triplequote-dq-punct-atpt 'ar-punct-triplequote-dq-atpt)
(defun ar-punct-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around punct. "
  (interactive "*")
  (ar-th-triplequote-dq 'punct nil (interactive-p)))

(defalias 'ar-triplequote-sq-punct-atpt 'ar-punct-triplequote-sq-atpt)
(defun ar-punct-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around punct. "
  (interactive "*")
  (ar-th-triplequote-sq 'punct nil (interactive-p)))

(defalias 'ar-punct-underscore-atpt 'ar-underscore-punct-atpt)
(defun ar-underscore-punct-atpt ()
  "Put underscore char around PUNCT. "
  (interactive "*")
  (ar-th-underscore 'punct nil (interactive-p)))

(defalias 'ar-punct-whitespace-atpt 'ar-whitespace-punct-atpt)
(defun ar-whitespace-punct-atpt ()
  "Put whitespace char around PUNCT. "
  (interactive "*")
  (ar-th-whitespace 'punct nil t))

(defalias 'ar-forward-punct-atpt 'ar-punct-forward-atpt)
(defun ar-punct-forward-atpt (&optional arg)
  "Moves forward over PUNCT at point if any, does nothing otherwise.
Returns end position of PUNCT "
  (interactive "p")
  (ar-th-forward 'punct arg (interactive-p)))

(defalias 'ar-backward-punct-atpt 'ar-punct-backward-atpt)
(defun ar-punct-backward-atpt (&optional arg)
  "Moves backward over PUNCT before point if any, does nothing otherwise.
Returns beginning position of PUNCT "
  (interactive "p")
  (ar-th-backward 'punct arg (interactive-p)))

(defalias 'ar-transpose-punct-atpt 'ar-punct-transpose-atpt)
(defun ar-punct-transpose-atpt (&optional arg)
  "Transposes PUNCT with PUNCT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'punct arg (interactive-p)))

(defalias 'ar-sort-punct-atpt 'ar-punct-sort-atpt)
(defun ar-punct-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts puncts in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'punct reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-punct-atpt 'ar-punct-check-atpt)
(defun ar-punct-check-atpt ()
  "Return t if a PUNCT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-punct-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-punct-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-space-atpt ()
  "Returns space at point if any, nil otherwise. "
  (interactive)
  (ar-th 'space nil nil (interactive-p)))

(defalias 'ar-bounds-of-space-atpt 'ar-space-bounds-atpt)
(defun ar-space-bounds-atpt ()
  "Returns a list, borders of space if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (interactive-p)))

(defun ar-space-beginning-position-atpt ()
  "Returns a number, beginning position SPACE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'space nil (interactive-p)))

(defun ar-space-end-position-atpt ()
  "Returns a number, end position of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'space nil (interactive-p)))

(defalias 'ar-beginning-of-space-atpt 'ar-space-beginning-atpt)
(defun ar-space-beginning-atpt ()
  "Goto beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'space nil (interactive-p)))

(defalias 'ar-end-of-space-atpt 'ar-space-end-atpt)
(defun ar-space-end-atpt ()
  "Goto end of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'space nil (interactive-p)))

(defalias 'ar-in-space-p-atpt 'ar-space-in-p-atpt)
(defun ar-space-in-p-atpt ()
  "Returns bounds of SPACE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (interactive-p)))

(defalias 'ar-length-of-space-atpt 'ar-space-length-atpt)
(defun ar-space-length-atpt ()
  "Returns beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'space nil (interactive-p)))

(defalias 'ar-copy-space-atpt 'ar-space-copy-atpt)
(defun ar-space-copy-atpt ()
  "Returns a copy of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'space nil (interactive-p)))

(defalias 'ar-delete-space-in-region 'ar-space-delete-in-region)
(defun ar-space-delete-in-region (beg end)
  "Deletes SPACE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'space beg end (interactive-p)))

(defun ar-blok-space-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around space.
  Returns blok or nil if no SPACE at cursor-position. "
  (interactive "*")
  (ar-th-blok 'space nil (interactive-p)))

(defalias 'ar-escape-space-atpt 'ar-space-escape-atpt)
(defun ar-space-escape-atpt ()
  "Returns regexp-quoted SPACE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'space nil))

(defalias 'ar-doublequote-space-atpt 'ar-space-doublequote-atpt)
(defun ar-space-doublequote-atpt ()
  "Doublequotes SPACE at point if any. "
  (interactive "*")
  (ar-th-doublequote 'space nil (interactive-p)))

(defalias 'ar-slash-space-atpt 'ar-space-slash-atpt)
(defun ar-space-slash-atpt ()
  "Doublequotes SPACE at point if any. "
  (interactive "*")
  (ar-th-slash 'space nil (interactive-p)))

(defalias 'ar-double-backslash-space-atpt 'ar-space-double-backslash-atpt)
(defun ar-space-double-backslash-atpt ()
  "Puts doubled backslashes around SPACE at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'space nil (interactive-p)))

(defalias 'ar-doubleslash-space-atpt 'ar-space-doubleslash-atpt)
(defun ar-space-doubleslash-atpt ()
  "Puts doubled slashes around SPACE at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'space nil (interactive-p)))

(defalias 'ar-doubleslash-paren-space-atpt 'ar-space-doubleslash-paren-atpt)
(defun ar-space-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around SPACE at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'space nil (interactive-p)))

(defalias 'ar-slashparen-space-atpt 'ar-space-slashparen-atpt)
(defun ar-space-slashparen-atpt ()
  "Provides slashed parentheses around SPACE at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'space nil (interactive-p)))

(defalias 'ar-dollar-space-atpt 'ar-space-dollar-atpt)
(defun ar-space-dollar-atpt ()
  "Doublequotes SPACE at point if any. "
  (interactive "*")
  (ar-th-dollar 'space nil (interactive-p)))

(defalias 'ar-equalize-space-atpt 'ar-space-equalize-atpt)
(defun ar-space-equalize-atpt ()
  "Puts equal signs `=' around SPACE at point if any. "
  (interactive "*")
  (ar-th-equalize 'space nil (interactive-p)))

(defalias 'ar-greater-angle-space-atpt 'ar-space-greater-angle-atpt)
(defun ar-space-greater-angle-atpt ()
  "Sets angles for SPACE after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'space nil (interactive-p)))

(defalias 'ar-lesser-angle-space-atpt 'ar-space-lesser-angle-atpt)
(defun ar-space-lesser-angle-atpt ()
  "Sets angles for SPACE after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'space nil (interactive-p)))

(defalias 'ar-backslash-space-atpt 'ar-space-backslash-atpt)
(defun ar-space-backslash-atpt ()
  "Backslash SPACE at point if any. "
  (interactive "*")
  (ar-th-backslash 'space nil (interactive-p)))

(defalias 'ar-brace-space-atpt 'ar-space-brace-atpt)
(defun ar-space-brace-atpt ()
  "Braces SPACE at point if any. "
  (interactive "*")
  (ar-th-brace 'space nil (interactive-p)))

(defalias 'ar-bracket-space-atpt 'ar-space-bracket-atpt)
(defun ar-space-bracket-atpt ()
  "Brackets SPACE after point if any. "
  (interactive "*")
  (ar-th-bracket 'space nil (interactive-p)))

(defun ar-comment-space-atpt ()
  "Comments SPACE at point if any. "
  (interactive "*")
  (ar-th-comment 'space nil (interactive-p)))

(defun ar-commatize-space-atpt ()
  "Put a comma after SPACE at point if any. "
  (interactive "*")
  (ar-th-commatize 'space nil (interactive-p)))

(defun ar-quote-space-atpt ()
  "Put a singlequote before SPACE at point if any. "
  (interactive "*")
  (ar-th-quote 'space nil (interactive-p)))

(defalias 'ar-hyphen-space-atpt 'ar-space-hyphen-atpt)
(defun ar-space-hyphen-atpt ()
  "Puts hyphens around SPACE at point if any. "
  (interactive "*")
  (ar-th-hyphen 'space nil (interactive-p)))

(defalias 'ar-mark-space-atpt 'ar-space-mark-atpt)
(defun ar-space-mark-atpt ()
  "Marks SPACE at point if any. "
  (interactive)
  (ar-th-mark 'space))

(defalias 'ar-hide-space-atpt 'ar-space-hide-atpt)
(defun ar-space-hide-atpt ()
  "Hides SPACE at point. "
  (interactive)
  (ar-th-hide 'space))

(defalias 'ar-show-space-atpt 'ar-space-show-atpt)
(defun ar-space-show-atpt ()
  "Shows hidden SPACE at point. "
  (interactive)
  (ar-th-show 'space))

(defalias 'ar-hide-show-space-atpt 'ar-space-hide-show-atpt)
(defun ar-space-hide-show-atpt ()
  "Alternatively hides or shows SPACE at point. "
  (interactive)
  (ar-th-hide-show 'space))

(defalias 'ar-highlight-space-atpt-mode 'ar-space-highlight-atpt-mode)

(defun ar-space-highlight-atpt-mode ()
  "Toggles space-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'space nil (interactive-p)))

(defalias 'ar-kill-space-atpt 'ar-space-kill-atpt)
(defun ar-space-kill-atpt ()
  "Kills SPACE at point if any. "
  (interactive "*")
  (ar-th-kill 'space nil (interactive-p)))

(defalias 'ar-kill-backward-space-atpt 'ar-space-kill-backward-atpt)
(defun ar-space-kill-backward-atpt ()
  "Kills SPACE at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'space nil (interactive-p)))

(defalias 'ar-left-right-singlequote-space-atpt 'ar-space-left-right-singlequote-atpt)
(defun ar-space-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'space nil (interactive-p)))

(defalias 'ar-parentize-space-atpt 'ar-space-parentize-atpt)
(defun ar-space-parentize-atpt ()
  "Parentizes SPACE at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'space nil (interactive-p)))

(defalias 'ar-separate-space-atpt 'ar-space-separate-atpt)
(defun ar-space-separate-atpt ()
  "Separates SPACE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'space nil (interactive-p)))

(defalias 'ar-singlequote-space-atpt 'ar-space-singlequote-atpt)
(defun ar-space-singlequote-atpt ()
  "Singlequotes SPACE at point if any. "
  (interactive "*")
  (ar-th-singlequote 'space nil (interactive-p)))

(defalias 'ar-triplequote-dq-space-atpt 'ar-space-triplequote-dq-atpt)
(defun ar-space-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around space. "
  (interactive "*")
  (ar-th-triplequote-dq 'space nil (interactive-p)))

(defalias 'ar-triplequote-sq-space-atpt 'ar-space-triplequote-sq-atpt)
(defun ar-space-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around space. "
  (interactive "*")
  (ar-th-triplequote-sq 'space nil (interactive-p)))

(defalias 'ar-space-underscore-atpt 'ar-underscore-space-atpt)
(defun ar-underscore-space-atpt ()
  "Put underscore char around SPACE. "
  (interactive "*")
  (ar-th-underscore 'space nil (interactive-p)))

(defalias 'ar-space-whitespace-atpt 'ar-whitespace-space-atpt)
(defun ar-whitespace-space-atpt ()
  "Put whitespace char around SPACE. "
  (interactive "*")
  (ar-th-whitespace 'space nil t))

(defalias 'ar-forward-space-atpt 'ar-space-forward-atpt)
(defun ar-space-forward-atpt (&optional arg)
  "Moves forward over SPACE at point if any, does nothing otherwise.
Returns end position of SPACE "
  (interactive "p")
  (ar-th-forward 'space arg (interactive-p)))

(defalias 'ar-backward-space-atpt 'ar-space-backward-atpt)
(defun ar-space-backward-atpt (&optional arg)
  "Moves backward over SPACE before point if any, does nothing otherwise.
Returns beginning position of SPACE "
  (interactive "p")
  (ar-th-backward 'space arg (interactive-p)))

(defalias 'ar-transpose-space-atpt 'ar-space-transpose-atpt)
(defun ar-space-transpose-atpt (&optional arg)
  "Transposes SPACE with SPACE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'space arg (interactive-p)))

(defalias 'ar-sort-space-atpt 'ar-space-sort-atpt)
(defun ar-space-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts spaces in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'space reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-space-atpt 'ar-space-check-atpt)
(defun ar-space-check-atpt ()
  "Return t if a SPACE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-space-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-space-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-upper-atpt ()
  "Returns upper at point if any, nil otherwise. "
  (interactive)
  (ar-th 'upper nil nil (interactive-p)))

(defalias 'ar-bounds-of-upper-atpt 'ar-upper-bounds-atpt)
(defun ar-upper-bounds-atpt ()
  "Returns a list, borders of upper if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (interactive-p)))

(defun ar-upper-beginning-position-atpt ()
  "Returns a number, beginning position UPPER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'upper nil (interactive-p)))

(defun ar-upper-end-position-atpt ()
  "Returns a number, end position of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'upper nil (interactive-p)))

(defalias 'ar-beginning-of-upper-atpt 'ar-upper-beginning-atpt)
(defun ar-upper-beginning-atpt ()
  "Goto beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'upper nil (interactive-p)))

(defalias 'ar-end-of-upper-atpt 'ar-upper-end-atpt)
(defun ar-upper-end-atpt ()
  "Goto end of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'upper nil (interactive-p)))

(defalias 'ar-in-upper-p-atpt 'ar-upper-in-p-atpt)
(defun ar-upper-in-p-atpt ()
  "Returns bounds of UPPER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (interactive-p)))

(defalias 'ar-length-of-upper-atpt 'ar-upper-length-atpt)
(defun ar-upper-length-atpt ()
  "Returns beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'upper nil (interactive-p)))

(defalias 'ar-copy-upper-atpt 'ar-upper-copy-atpt)
(defun ar-upper-copy-atpt ()
  "Returns a copy of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'upper nil (interactive-p)))

(defalias 'ar-delete-upper-in-region 'ar-upper-delete-in-region)
(defun ar-upper-delete-in-region (beg end)
  "Deletes UPPER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'upper beg end (interactive-p)))

(defun ar-blok-upper-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around upper.
  Returns blok or nil if no UPPER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'upper nil (interactive-p)))

(defalias 'ar-escape-upper-atpt 'ar-upper-escape-atpt)
(defun ar-upper-escape-atpt ()
  "Returns regexp-quoted UPPER at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'upper nil))

(defalias 'ar-doublequote-upper-atpt 'ar-upper-doublequote-atpt)
(defun ar-upper-doublequote-atpt ()
  "Doublequotes UPPER at point if any. "
  (interactive "*")
  (ar-th-doublequote 'upper nil (interactive-p)))

(defalias 'ar-slash-upper-atpt 'ar-upper-slash-atpt)
(defun ar-upper-slash-atpt ()
  "Doublequotes UPPER at point if any. "
  (interactive "*")
  (ar-th-slash 'upper nil (interactive-p)))

(defalias 'ar-double-backslash-upper-atpt 'ar-upper-double-backslash-atpt)
(defun ar-upper-double-backslash-atpt ()
  "Puts doubled backslashes around UPPER at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'upper nil (interactive-p)))

(defalias 'ar-doubleslash-upper-atpt 'ar-upper-doubleslash-atpt)
(defun ar-upper-doubleslash-atpt ()
  "Puts doubled slashes around UPPER at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'upper nil (interactive-p)))

(defalias 'ar-doubleslash-paren-upper-atpt 'ar-upper-doubleslash-paren-atpt)
(defun ar-upper-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around UPPER at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'upper nil (interactive-p)))

(defalias 'ar-slashparen-upper-atpt 'ar-upper-slashparen-atpt)
(defun ar-upper-slashparen-atpt ()
  "Provides slashed parentheses around UPPER at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'upper nil (interactive-p)))

(defalias 'ar-dollar-upper-atpt 'ar-upper-dollar-atpt)
(defun ar-upper-dollar-atpt ()
  "Doublequotes UPPER at point if any. "
  (interactive "*")
  (ar-th-dollar 'upper nil (interactive-p)))

(defalias 'ar-equalize-upper-atpt 'ar-upper-equalize-atpt)
(defun ar-upper-equalize-atpt ()
  "Puts equal signs `=' around UPPER at point if any. "
  (interactive "*")
  (ar-th-equalize 'upper nil (interactive-p)))

(defalias 'ar-greater-angle-upper-atpt 'ar-upper-greater-angle-atpt)
(defun ar-upper-greater-angle-atpt ()
  "Sets angles for UPPER after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'upper nil (interactive-p)))

(defalias 'ar-lesser-angle-upper-atpt 'ar-upper-lesser-angle-atpt)
(defun ar-upper-lesser-angle-atpt ()
  "Sets angles for UPPER after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'upper nil (interactive-p)))

(defalias 'ar-backslash-upper-atpt 'ar-upper-backslash-atpt)
(defun ar-upper-backslash-atpt ()
  "Backslash UPPER at point if any. "
  (interactive "*")
  (ar-th-backslash 'upper nil (interactive-p)))

(defalias 'ar-brace-upper-atpt 'ar-upper-brace-atpt)
(defun ar-upper-brace-atpt ()
  "Braces UPPER at point if any. "
  (interactive "*")
  (ar-th-brace 'upper nil (interactive-p)))

(defalias 'ar-bracket-upper-atpt 'ar-upper-bracket-atpt)
(defun ar-upper-bracket-atpt ()
  "Brackets UPPER after point if any. "
  (interactive "*")
  (ar-th-bracket 'upper nil (interactive-p)))

(defun ar-comment-upper-atpt ()
  "Comments UPPER at point if any. "
  (interactive "*")
  (ar-th-comment 'upper nil (interactive-p)))

(defun ar-commatize-upper-atpt ()
  "Put a comma after UPPER at point if any. "
  (interactive "*")
  (ar-th-commatize 'upper nil (interactive-p)))

(defun ar-quote-upper-atpt ()
  "Put a singlequote before UPPER at point if any. "
  (interactive "*")
  (ar-th-quote 'upper nil (interactive-p)))

(defalias 'ar-hyphen-upper-atpt 'ar-upper-hyphen-atpt)
(defun ar-upper-hyphen-atpt ()
  "Puts hyphens around UPPER at point if any. "
  (interactive "*")
  (ar-th-hyphen 'upper nil (interactive-p)))

(defalias 'ar-mark-upper-atpt 'ar-upper-mark-atpt)
(defun ar-upper-mark-atpt ()
  "Marks UPPER at point if any. "
  (interactive)
  (ar-th-mark 'upper))

(defalias 'ar-hide-upper-atpt 'ar-upper-hide-atpt)
(defun ar-upper-hide-atpt ()
  "Hides UPPER at point. "
  (interactive)
  (ar-th-hide 'upper))

(defalias 'ar-show-upper-atpt 'ar-upper-show-atpt)
(defun ar-upper-show-atpt ()
  "Shows hidden UPPER at point. "
  (interactive)
  (ar-th-show 'upper))

(defalias 'ar-hide-show-upper-atpt 'ar-upper-hide-show-atpt)
(defun ar-upper-hide-show-atpt ()
  "Alternatively hides or shows UPPER at point. "
  (interactive)
  (ar-th-hide-show 'upper))

(defalias 'ar-highlight-upper-atpt-mode 'ar-upper-highlight-atpt-mode)

(defun ar-upper-highlight-atpt-mode ()
  "Toggles upper-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'upper nil (interactive-p)))

(defalias 'ar-kill-upper-atpt 'ar-upper-kill-atpt)
(defun ar-upper-kill-atpt ()
  "Kills UPPER at point if any. "
  (interactive "*")
  (ar-th-kill 'upper nil (interactive-p)))

(defalias 'ar-kill-backward-upper-atpt 'ar-upper-kill-backward-atpt)
(defun ar-upper-kill-backward-atpt ()
  "Kills UPPER at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'upper nil (interactive-p)))

(defalias 'ar-left-right-singlequote-upper-atpt 'ar-upper-left-right-singlequote-atpt)
(defun ar-upper-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'upper nil (interactive-p)))

(defalias 'ar-parentize-upper-atpt 'ar-upper-parentize-atpt)
(defun ar-upper-parentize-atpt ()
  "Parentizes UPPER at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'upper nil (interactive-p)))

(defalias 'ar-separate-upper-atpt 'ar-upper-separate-atpt)
(defun ar-upper-separate-atpt ()
  "Separates UPPER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'upper nil (interactive-p)))

(defalias 'ar-singlequote-upper-atpt 'ar-upper-singlequote-atpt)
(defun ar-upper-singlequote-atpt ()
  "Singlequotes UPPER at point if any. "
  (interactive "*")
  (ar-th-singlequote 'upper nil (interactive-p)))

(defalias 'ar-triplequote-dq-upper-atpt 'ar-upper-triplequote-dq-atpt)
(defun ar-upper-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around upper. "
  (interactive "*")
  (ar-th-triplequote-dq 'upper nil (interactive-p)))

(defalias 'ar-triplequote-sq-upper-atpt 'ar-upper-triplequote-sq-atpt)
(defun ar-upper-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around upper. "
  (interactive "*")
  (ar-th-triplequote-sq 'upper nil (interactive-p)))

(defalias 'ar-upper-underscore-atpt 'ar-underscore-upper-atpt)
(defun ar-underscore-upper-atpt ()
  "Put underscore char around UPPER. "
  (interactive "*")
  (ar-th-underscore 'upper nil (interactive-p)))

(defalias 'ar-upper-whitespace-atpt 'ar-whitespace-upper-atpt)
(defun ar-whitespace-upper-atpt ()
  "Put whitespace char around UPPER. "
  (interactive "*")
  (ar-th-whitespace 'upper nil t))

(defalias 'ar-forward-upper-atpt 'ar-upper-forward-atpt)
(defun ar-upper-forward-atpt (&optional arg)
  "Moves forward over UPPER at point if any, does nothing otherwise.
Returns end position of UPPER "
  (interactive "p")
  (ar-th-forward 'upper arg (interactive-p)))

(defalias 'ar-backward-upper-atpt 'ar-upper-backward-atpt)
(defun ar-upper-backward-atpt (&optional arg)
  "Moves backward over UPPER before point if any, does nothing otherwise.
Returns beginning position of UPPER "
  (interactive "p")
  (ar-th-backward 'upper arg (interactive-p)))

(defalias 'ar-transpose-upper-atpt 'ar-upper-transpose-atpt)
(defun ar-upper-transpose-atpt (&optional arg)
  "Transposes UPPER with UPPER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'upper arg (interactive-p)))

(defalias 'ar-sort-upper-atpt 'ar-upper-sort-atpt)
(defun ar-upper-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts uppers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'upper reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-upper-atpt 'ar-upper-check-atpt)
(defun ar-upper-check-atpt ()
  "Return t if a UPPER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-upper-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-upper-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-xdigit-atpt ()
  "Returns xdigit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'xdigit nil nil (interactive-p)))

(defalias 'ar-bounds-of-xdigit-atpt 'ar-xdigit-bounds-atpt)
(defun ar-xdigit-bounds-atpt ()
  "Returns a list, borders of xdigit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (interactive-p)))

(defun ar-xdigit-beginning-position-atpt ()
  "Returns a number, beginning position XDIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'xdigit nil (interactive-p)))

(defun ar-xdigit-end-position-atpt ()
  "Returns a number, end position of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'xdigit nil (interactive-p)))

(defalias 'ar-beginning-of-xdigit-atpt 'ar-xdigit-beginning-atpt)
(defun ar-xdigit-beginning-atpt ()
  "Goto beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'xdigit nil (interactive-p)))

(defalias 'ar-end-of-xdigit-atpt 'ar-xdigit-end-atpt)
(defun ar-xdigit-end-atpt ()
  "Goto end of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'xdigit nil (interactive-p)))

(defalias 'ar-in-xdigit-p-atpt 'ar-xdigit-in-p-atpt)
(defun ar-xdigit-in-p-atpt ()
  "Returns bounds of XDIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (interactive-p)))

(defalias 'ar-length-of-xdigit-atpt 'ar-xdigit-length-atpt)
(defun ar-xdigit-length-atpt ()
  "Returns beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'xdigit nil (interactive-p)))

(defalias 'ar-copy-xdigit-atpt 'ar-xdigit-copy-atpt)
(defun ar-xdigit-copy-atpt ()
  "Returns a copy of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'xdigit nil (interactive-p)))

(defalias 'ar-delete-xdigit-in-region 'ar-xdigit-delete-in-region)
(defun ar-xdigit-delete-in-region (beg end)
  "Deletes XDIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xdigit beg end (interactive-p)))

(defun ar-blok-xdigit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xdigit.
  Returns blok or nil if no XDIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'xdigit nil (interactive-p)))

(defalias 'ar-escape-xdigit-atpt 'ar-xdigit-escape-atpt)
(defun ar-xdigit-escape-atpt ()
  "Returns regexp-quoted XDIGIT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'xdigit nil))

(defalias 'ar-doublequote-xdigit-atpt 'ar-xdigit-doublequote-atpt)
(defun ar-xdigit-doublequote-atpt ()
  "Doublequotes XDIGIT at point if any. "
  (interactive "*")
  (ar-th-doublequote 'xdigit nil (interactive-p)))

(defalias 'ar-slash-xdigit-atpt 'ar-xdigit-slash-atpt)
(defun ar-xdigit-slash-atpt ()
  "Doublequotes XDIGIT at point if any. "
  (interactive "*")
  (ar-th-slash 'xdigit nil (interactive-p)))

(defalias 'ar-double-backslash-xdigit-atpt 'ar-xdigit-double-backslash-atpt)
(defun ar-xdigit-double-backslash-atpt ()
  "Puts doubled backslashes around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-double-backslash 'xdigit nil (interactive-p)))

(defalias 'ar-doubleslash-xdigit-atpt 'ar-xdigit-doubleslash-atpt)
(defun ar-xdigit-doubleslash-atpt ()
  "Puts doubled slashes around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-doubleslash 'xdigit nil (interactive-p)))

(defalias 'ar-doubleslash-paren-xdigit-atpt 'ar-xdigit-doubleslash-paren-atpt)
(defun ar-xdigit-doubleslash-paren-atpt ()
  "Provides doubleslashed parentheses around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-doubleslash-paren 'xdigit nil (interactive-p)))

(defalias 'ar-slashparen-xdigit-atpt 'ar-xdigit-slashparen-atpt)
(defun ar-xdigit-slashparen-atpt ()
  "Provides slashed parentheses around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-slash-paren 'xdigit nil (interactive-p)))

(defalias 'ar-dollar-xdigit-atpt 'ar-xdigit-dollar-atpt)
(defun ar-xdigit-dollar-atpt ()
  "Doublequotes XDIGIT at point if any. "
  (interactive "*")
  (ar-th-dollar 'xdigit nil (interactive-p)))

(defalias 'ar-equalize-xdigit-atpt 'ar-xdigit-equalize-atpt)
(defun ar-xdigit-equalize-atpt ()
  "Puts equal signs `=' around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-equalize 'xdigit nil (interactive-p)))

(defalias 'ar-greater-angle-xdigit-atpt 'ar-xdigit-greater-angle-atpt)
(defun ar-xdigit-greater-angle-atpt ()
  "Sets angles for XDIGIT after point if any. "
  (interactive "*")
  (ar-th-greater-angle 'xdigit nil (interactive-p)))

(defalias 'ar-lesser-angle-xdigit-atpt 'ar-xdigit-lesser-angle-atpt)
(defun ar-xdigit-lesser-angle-atpt ()
  "Sets angles for XDIGIT after point if any. "
  (interactive "*")
  (ar-th-lesser-angle 'xdigit nil (interactive-p)))

(defalias 'ar-backslash-xdigit-atpt 'ar-xdigit-backslash-atpt)
(defun ar-xdigit-backslash-atpt ()
  "Backslash XDIGIT at point if any. "
  (interactive "*")
  (ar-th-backslash 'xdigit nil (interactive-p)))

(defalias 'ar-brace-xdigit-atpt 'ar-xdigit-brace-atpt)
(defun ar-xdigit-brace-atpt ()
  "Braces XDIGIT at point if any. "
  (interactive "*")
  (ar-th-brace 'xdigit nil (interactive-p)))

(defalias 'ar-bracket-xdigit-atpt 'ar-xdigit-bracket-atpt)
(defun ar-xdigit-bracket-atpt ()
  "Brackets XDIGIT after point if any. "
  (interactive "*")
  (ar-th-bracket 'xdigit nil (interactive-p)))

(defun ar-comment-xdigit-atpt ()
  "Comments XDIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'xdigit nil (interactive-p)))

(defun ar-commatize-xdigit-atpt ()
  "Put a comma after XDIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'xdigit nil (interactive-p)))

(defun ar-quote-xdigit-atpt ()
  "Put a singlequote before XDIGIT at point if any. "
  (interactive "*")
  (ar-th-quote 'xdigit nil (interactive-p)))

(defalias 'ar-hyphen-xdigit-atpt 'ar-xdigit-hyphen-atpt)
(defun ar-xdigit-hyphen-atpt ()
  "Puts hyphens around XDIGIT at point if any. "
  (interactive "*")
  (ar-th-hyphen 'xdigit nil (interactive-p)))

(defalias 'ar-mark-xdigit-atpt 'ar-xdigit-mark-atpt)
(defun ar-xdigit-mark-atpt ()
  "Marks XDIGIT at point if any. "
  (interactive)
  (ar-th-mark 'xdigit))

(defalias 'ar-hide-xdigit-atpt 'ar-xdigit-hide-atpt)
(defun ar-xdigit-hide-atpt ()
  "Hides XDIGIT at point. "
  (interactive)
  (ar-th-hide 'xdigit))

(defalias 'ar-show-xdigit-atpt 'ar-xdigit-show-atpt)
(defun ar-xdigit-show-atpt ()
  "Shows hidden XDIGIT at point. "
  (interactive)
  (ar-th-show 'xdigit))

(defalias 'ar-hide-show-xdigit-atpt 'ar-xdigit-hide-show-atpt)
(defun ar-xdigit-hide-show-atpt ()
  "Alternatively hides or shows XDIGIT at point. "
  (interactive)
  (ar-th-hide-show 'xdigit))

(defalias 'ar-highlight-xdigit-atpt-mode 'ar-xdigit-highlight-atpt-mode)

(defun ar-xdigit-highlight-atpt-mode ()
  "Toggles xdigit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'xdigit nil (interactive-p)))

(defalias 'ar-kill-xdigit-atpt 'ar-xdigit-kill-atpt)
(defun ar-xdigit-kill-atpt ()
  "Kills XDIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'xdigit nil (interactive-p)))

(defalias 'ar-kill-backward-xdigit-atpt 'ar-xdigit-kill-backward-atpt)
(defun ar-xdigit-kill-backward-atpt ()
  "Kills XDIGIT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'xdigit nil (interactive-p)))

(defalias 'ar-left-right-singlequote-xdigit-atpt 'ar-xdigit-left-right-singlequote-atpt)
(defun ar-xdigit-left-right-singlequote-atpt ()
  "Singlequotes alnum at point if any. "
  (interactive "*")
  (ar-th-left-right-singlequote 'xdigit nil (interactive-p)))

(defalias 'ar-parentize-xdigit-atpt 'ar-xdigit-parentize-atpt)
(defun ar-xdigit-parentize-atpt ()
  "Parentizes XDIGIT at point if any, does nothing otherwise"
  (interactive "*")
  (ar-th-parentize 'xdigit nil (interactive-p)))

(defalias 'ar-separate-xdigit-atpt 'ar-xdigit-separate-atpt)
(defun ar-xdigit-separate-atpt ()
  "Separates XDIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'xdigit nil (interactive-p)))

(defalias 'ar-singlequote-xdigit-atpt 'ar-xdigit-singlequote-atpt)
(defun ar-xdigit-singlequote-atpt ()
  "Singlequotes XDIGIT at point if any. "
  (interactive "*")
  (ar-th-singlequote 'xdigit nil (interactive-p)))

(defalias 'ar-triplequote-dq-xdigit-atpt 'ar-xdigit-triplequote-dq-atpt)
(defun ar-xdigit-triplequote-dq-atpt ()
  "Put triplequotes composed of doublequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequote-dq 'xdigit nil (interactive-p)))

(defalias 'ar-triplequote-sq-xdigit-atpt 'ar-xdigit-triplequote-sq-atpt)
(defun ar-xdigit-triplequote-sq-atpt ()
  "Put triplequotes composed of singlequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequote-sq 'xdigit nil (interactive-p)))

(defalias 'ar-xdigit-underscore-atpt 'ar-underscore-xdigit-atpt)
(defun ar-underscore-xdigit-atpt ()
  "Put underscore char around XDIGIT. "
  (interactive "*")
  (ar-th-underscore 'xdigit nil (interactive-p)))

(defalias 'ar-xdigit-whitespace-atpt 'ar-whitespace-xdigit-atpt)
(defun ar-whitespace-xdigit-atpt ()
  "Put whitespace char around XDIGIT. "
  (interactive "*")
  (ar-th-whitespace 'xdigit nil t))

(defalias 'ar-forward-xdigit-atpt 'ar-xdigit-forward-atpt)
(defun ar-xdigit-forward-atpt (&optional arg)
  "Moves forward over XDIGIT at point if any, does nothing otherwise.
Returns end position of XDIGIT "
  (interactive "p")
  (ar-th-forward 'xdigit arg (interactive-p)))

(defalias 'ar-backward-xdigit-atpt 'ar-xdigit-backward-atpt)
(defun ar-xdigit-backward-atpt (&optional arg)
  "Moves backward over XDIGIT before point if any, does nothing otherwise.
Returns beginning position of XDIGIT "
  (interactive "p")
  (ar-th-backward 'xdigit arg (interactive-p)))

(defalias 'ar-transpose-xdigit-atpt 'ar-xdigit-transpose-atpt)
(defun ar-xdigit-transpose-atpt (&optional arg)
  "Transposes XDIGIT with XDIGIT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'xdigit arg (interactive-p)))

(defalias 'ar-sort-xdigit-atpt 'ar-xdigit-sort-atpt)
(defun ar-xdigit-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xdigits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'xdigit reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-xdigit-atpt 'ar-xdigit-check-atpt)
(defun ar-xdigit-check-atpt ()
  "Return t if a XDIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-xdigit-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-xdigit-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))
;; ar-thing-at-point-utils-nodelim-core: ar-atpt-classes end

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw start

(defun ar-backslashed-atpt (&optional arg no-delimiters)
  "Returns backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backslashed arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-backslashed-atpt 'ar-backslashed-bounds-atpt)
(defun ar-backslashed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of backslashed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'backslashed no-delimiters (interactive-p)))

(defun ar-backslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'backslashed no-delimiters (interactive-p)))

(defun ar-backslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-backslashed-atpt 'ar-backslashed-beginning-atpt)
(defun ar-backslashed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-end-of-backslashed-atpt 'ar-backslashed-end-atpt)
(defun ar-backslashed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-length-of-backslashed-atpt 'ar-backslashed-length-atpt)
(defun ar-backslashed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-copy-backslashed-atpt 'ar-backslashed-copy-atpt)
(defun ar-backslashed-copy-atpt (&optional no-delimiters)
  "Returns a copy of backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-delete-backslashed-in-region 'ar-backslashed-delete-in-region)
(defun ar-backslashed-delete-in-region (beg end)
  "Deletes backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'backslashed beg end (interactive-p)))

(defun ar-blok-backslashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backslashed.

If region is active, do that for all elements \"backslashed\" in region.
  Returns blok or nil if no backslashed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-escape-backslashed-atpt 'ar-backslashed-escape-atpt)
(defun ar-backslashed-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'backslashed no-delimiters))

(defalias 'ar-doublequote-backslashed-atpt 'ar-backslashed-doublequote-atpt)
(defun ar-backslashed-doublequote-atpt (&optional no-delimiters)
  "Doublequotes backslashed at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-slash-backslashed-atpt 'ar-backslashed-slash-atpt)
(defun ar-backslashed-slash-atpt (&optional no-delimiters)
  "Put slashes around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-backslashed-atpt 'ar-backslashed-double-backslash-atpt)
(defun ar-backslashed-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-backslashed-atpt 'ar-backslashed-doubleslash-atpt)
(defun ar-backslashed-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-backslashed-atpt 'ar-backslashed-doubleslash-paren-atpt)
(defun ar-backslashed-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around backslashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-slashparen-backslashed-atpt 'ar-backslashed-slashparen-atpt)
(defun ar-backslashed-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-dollar-backslashed-atpt 'ar-backslashed-dollar-atpt)
(defun ar-backslashed-dollar-atpt (&optional no-delimiters)
  "Doublequotes backslashed at point if any. "
  (interactive "*p")
  (ar-th-dollar 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-equalize-backslashed-atpt 'ar-backslashed-equalize-atpt)
(defun ar-backslashed-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around backslashed at point if any. "
  (interactive "*p")
  (ar-th-equalize 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-backslashed-atpt 'ar-backslashed-greater-angle-atpt)
(defun ar-backslashed-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for backslashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-backslashed-atpt 'ar-backslashed-lesser-angle-atpt)
(defun ar-backslashed-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for backslashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-backslash-backslashed-atpt 'ar-backslashed-backslash-atpt)
(defun ar-backslashed-backslash-atpt (&optional no-delimiters)
  "Backslash backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-brace-backslashed-atpt 'ar-backslashed-brace-atpt)
(defun ar-backslashed-brace-atpt (&optional no-delimiters)
  "Braces backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-bracket-backslashed-atpt 'ar-backslashed-bracket-atpt)
(defun ar-backslashed-bracket-atpt (&optional no-delimiters)
  "Brackets backslashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'backslashed no-delimiters (interactive-p)))

(defun ar-comment-backslashed-atpt (&optional no-delimiters)
  "Comments backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'backslashed no-delimiters (interactive-p)))

(defun ar-commatize-backslashed-atpt (&optional no-delimiters)
  "Put a comma after backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'backslashed no-delimiters (interactive-p)))

(defun ar-quote-backslashed-atpt (&optional no-delimiters)
  "Put a singlequote before backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-hyphen-backslashed-atpt 'ar-backslashed-hyphen-atpt)
(defun ar-backslashed-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-mark-backslashed-atpt 'ar-backslashed-mark-atpt)
(defun ar-backslashed-mark-atpt (&optional no-delimiters)
  "Marks backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backslashed))

(defalias 'ar-hide-backslashed-atpt 'ar-backslashed-hide-atpt)
(defun ar-backslashed-hide-atpt ()
  "Hides backslashed at point. "
  (interactive)
  (ar-th-hide 'backslashed))

(defalias 'ar-show-backslashed-atpt 'ar-backslashed-show-atpt)
(defun ar-backslashed-show-atpt ()
  "Shows hidden backslashed at point. "
  (interactive)
  (ar-th-show 'backslashed))

(defalias 'ar-hide-show-backslashed-atpt 'ar-backslashed-hide-show-atpt)
(defun ar-backslashed-hide-show-atpt ()
  "Alternatively hides or shows backslashed at point. "
  (interactive)
  (ar-th-hide-show 'backslashed))

(defalias 'ar-highlight-backslashed-atpt-mode 'ar-backslashed-highlight-atpt-mode)
(defun ar-backslashed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles backslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-kill-backslashed-atpt 'ar-backslashed-kill-atpt)
(defun ar-backslashed-kill-atpt (&optional no-delimiters)
  "Kills backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-backslashed-atpt 'ar-backslashed-kill-backward-atpt)
(defun ar-backslashed-kill-backward-atpt (&optional no-delimiters)
  "Kills backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-backslashed-atpt 'ar-backslashed-left-right-singlequote-atpt)
(defun ar-backslashed-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-parentize-backslashed-atpt 'ar-backslashed-parentize-atpt)
(defun ar-backslashed-parentize-atpt (&optional no-delimiters)
  "Parentizes backslashed at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-separate-backslashed-atpt 'ar-backslashed-separate-atpt)
(defun ar-backslashed-separate-atpt (&optional no-delimiters)
  "Separates backslashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-singlequote-backslashed-atpt 'ar-backslashed-singlequote-atpt)
(defun ar-backslashed-singlequote-atpt (&optional no-delimiters)
  "Singlequotes backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-trim-backslashed-atpt 'ar-backslashed-trim-atpt)
(defun ar-backslashed-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'backslashed t t))
    (ar-th-trim 'backslashed t t)))

(defalias 'ar-trim-left-backslashed-atpt 'ar-backslashed-left-trim-atpt)
(defun ar-backslashed-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'backslashed t nil))
    (ar-th-trim 'backslashed t nil)))

(defalias 'ar-trim-right-backslashed-atpt 'ar-backslashed-right-trim-atpt)
(defun ar-backslashed-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'backslashed nil t))
    (ar-th-trim 'backslashed nil t)))

(defalias 'ar-backslashed-underscore-atpt 'ar-underscore-backslashed-atpt)
(defun ar-underscore-backslashed-atpt (&optional no-delimiters)
  "Put underscore char around BACKSLASHED. "
  (interactive "*p")
  (ar-th-underscore 'backslashed no-delimiters (interactive-p)))

(defalias 'ar-backslashed-whitespace-atpt 'ar-whitespace-backslashed-atpt)
(defun ar-whitespace-backslashed-atpt (&optional no-delimiters)
  "Put whitespace char around BACKSLASHED. "
  (interactive "*p")
  (ar-th-whitespace 'backslashed nil t))

(defalias 'ar-forward-backslashed-atpt 'ar-backslashed-forward-atpt)
(defun ar-backslashed-forward-atpt (&optional arg)
  "Moves forward over backslashed at point if any, does nothing otherwise.
Returns end position of backslashed "
  (interactive "p")
  (ar-th-forward 'backslashed arg (interactive-p)))

(defalias 'ar-backward-backslashed-atpt 'ar-backslashed-backward-atpt)
(defun ar-backslashed-backward-atpt (&optional arg)
  "Moves backward over backslashed before point if any, does nothing otherwise.
Returns beginning position of backslashed "
  (interactive "p")
  (ar-th-backward 'backslashed arg (interactive-p)))

(defalias 'ar-transpose-backslashed-atpt 'ar-backslashed-transpose-atpt)
(defun ar-backslashed-transpose-atpt (&optional arg)
  "Transposes backslashed with backslashed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'backslashed arg (interactive-p)))

(defalias 'ar-sort-backslashed-atpt 'ar-backslashed-sort-atpt)
(defun ar-backslashed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts backslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'backslashed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-backslashed-atpt 'ar-backslashed-check-atpt)
(defun ar-backslashed-check-atpt ()
  "Return t if a backslashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-backslashed-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-backslashed-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-dollared-atpt (&optional arg no-delimiters)
  "Returns dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'dollared arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-dollared-atpt 'ar-dollared-bounds-atpt)
(defun ar-dollared-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of dollared if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'dollared no-delimiters (interactive-p)))

(defun ar-dollared-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'dollared no-delimiters (interactive-p)))

(defun ar-dollared-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'dollared no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-dollared-atpt 'ar-dollared-beginning-atpt)
(defun ar-dollared-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'dollared no-delimiters (interactive-p)))

(defalias 'ar-end-of-dollared-atpt 'ar-dollared-end-atpt)
(defun ar-dollared-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'dollared no-delimiters (interactive-p)))

(defalias 'ar-length-of-dollared-atpt 'ar-dollared-length-atpt)
(defun ar-dollared-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'dollared no-delimiters (interactive-p)))

(defalias 'ar-copy-dollared-atpt 'ar-dollared-copy-atpt)
(defun ar-dollared-copy-atpt (&optional no-delimiters)
  "Returns a copy of dollared at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'dollared no-delimiters (interactive-p)))

(defalias 'ar-delete-dollared-in-region 'ar-dollared-delete-in-region)
(defun ar-dollared-delete-in-region (beg end)
  "Deletes dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'dollared beg end (interactive-p)))

(defun ar-blok-dollared-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around dollared.

If region is active, do that for all elements \"dollared\" in region.
  Returns blok or nil if no dollared at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'dollared no-delimiters (interactive-p)))

(defalias 'ar-escape-dollared-atpt 'ar-dollared-escape-atpt)
(defun ar-dollared-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted dollared at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'dollared no-delimiters))

(defalias 'ar-doublequote-dollared-atpt 'ar-dollared-doublequote-atpt)
(defun ar-dollared-doublequote-atpt (&optional no-delimiters)
  "Doublequotes dollared at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'dollared no-delimiters (interactive-p)))

(defalias 'ar-slash-dollared-atpt 'ar-dollared-slash-atpt)
(defun ar-dollared-slash-atpt (&optional no-delimiters)
  "Put slashes around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'dollared no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-dollared-atpt 'ar-dollared-double-backslash-atpt)
(defun ar-dollared-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'dollared no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-dollared-atpt 'ar-dollared-doubleslash-atpt)
(defun ar-dollared-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'dollared no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-dollared-atpt 'ar-dollared-doubleslash-paren-atpt)
(defun ar-dollared-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around dollared at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'dollared no-delimiters (interactive-p)))

(defalias 'ar-slashparen-dollared-atpt 'ar-dollared-slashparen-atpt)
(defun ar-dollared-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'dollared no-delimiters (interactive-p)))

(defalias 'ar-dollar-dollared-atpt 'ar-dollared-dollar-atpt)
(defun ar-dollared-dollar-atpt (&optional no-delimiters)
  "Doublequotes dollared at point if any. "
  (interactive "*p")
  (ar-th-dollar 'dollared no-delimiters (interactive-p)))

(defalias 'ar-equalize-dollared-atpt 'ar-dollared-equalize-atpt)
(defun ar-dollared-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around dollared at point if any. "
  (interactive "*p")
  (ar-th-equalize 'dollared no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-dollared-atpt 'ar-dollared-greater-angle-atpt)
(defun ar-dollared-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for dollared after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'dollared no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-dollared-atpt 'ar-dollared-lesser-angle-atpt)
(defun ar-dollared-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for dollared after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'dollared no-delimiters (interactive-p)))

(defalias 'ar-backslash-dollared-atpt 'ar-dollared-backslash-atpt)
(defun ar-dollared-backslash-atpt (&optional no-delimiters)
  "Backslash dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'dollared no-delimiters (interactive-p)))

(defalias 'ar-brace-dollared-atpt 'ar-dollared-brace-atpt)
(defun ar-dollared-brace-atpt (&optional no-delimiters)
  "Braces dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'dollared no-delimiters (interactive-p)))

(defalias 'ar-bracket-dollared-atpt 'ar-dollared-bracket-atpt)
(defun ar-dollared-bracket-atpt (&optional no-delimiters)
  "Brackets dollared after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'dollared no-delimiters (interactive-p)))

(defun ar-comment-dollared-atpt (&optional no-delimiters)
  "Comments dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'dollared no-delimiters (interactive-p)))

(defun ar-commatize-dollared-atpt (&optional no-delimiters)
  "Put a comma after dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'dollared no-delimiters (interactive-p)))

(defun ar-quote-dollared-atpt (&optional no-delimiters)
  "Put a singlequote before dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'dollared no-delimiters (interactive-p)))

(defalias 'ar-hyphen-dollared-atpt 'ar-dollared-hyphen-atpt)
(defun ar-dollared-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'dollared no-delimiters (interactive-p)))

(defalias 'ar-mark-dollared-atpt 'ar-dollared-mark-atpt)
(defun ar-dollared-mark-atpt (&optional no-delimiters)
  "Marks dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'dollared))

(defalias 'ar-hide-dollared-atpt 'ar-dollared-hide-atpt)
(defun ar-dollared-hide-atpt ()
  "Hides dollared at point. "
  (interactive)
  (ar-th-hide 'dollared))

(defalias 'ar-show-dollared-atpt 'ar-dollared-show-atpt)
(defun ar-dollared-show-atpt ()
  "Shows hidden dollared at point. "
  (interactive)
  (ar-th-show 'dollared))

(defalias 'ar-hide-show-dollared-atpt 'ar-dollared-hide-show-atpt)
(defun ar-dollared-hide-show-atpt ()
  "Alternatively hides or shows dollared at point. "
  (interactive)
  (ar-th-hide-show 'dollared))

(defalias 'ar-highlight-dollared-atpt-mode 'ar-dollared-highlight-atpt-mode)
(defun ar-dollared-highlight-atpt-mode (&optional no-delimiters)
  "Toggles dollared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'dollared no-delimiters (interactive-p)))

(defalias 'ar-kill-dollared-atpt 'ar-dollared-kill-atpt)
(defun ar-dollared-kill-atpt (&optional no-delimiters)
  "Kills dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'dollared no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-dollared-atpt 'ar-dollared-kill-backward-atpt)
(defun ar-dollared-kill-backward-atpt (&optional no-delimiters)
  "Kills dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'dollared no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-dollared-atpt 'ar-dollared-left-right-singlequote-atpt)
(defun ar-dollared-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'dollared no-delimiters (interactive-p)))

(defalias 'ar-parentize-dollared-atpt 'ar-dollared-parentize-atpt)
(defun ar-dollared-parentize-atpt (&optional no-delimiters)
  "Parentizes dollared at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'dollared no-delimiters (interactive-p)))

(defalias 'ar-separate-dollared-atpt 'ar-dollared-separate-atpt)
(defun ar-dollared-separate-atpt (&optional no-delimiters)
  "Separates dollared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'dollared no-delimiters (interactive-p)))

(defalias 'ar-singlequote-dollared-atpt 'ar-dollared-singlequote-atpt)
(defun ar-dollared-singlequote-atpt (&optional no-delimiters)
  "Singlequotes dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'dollared no-delimiters (interactive-p)))

(defalias 'ar-trim-dollared-atpt 'ar-dollared-trim-atpt)
(defun ar-dollared-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'dollared t t))
    (ar-th-trim 'dollared t t)))

(defalias 'ar-trim-left-dollared-atpt 'ar-dollared-left-trim-atpt)
(defun ar-dollared-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'dollared t nil))
    (ar-th-trim 'dollared t nil)))

(defalias 'ar-trim-right-dollared-atpt 'ar-dollared-right-trim-atpt)
(defun ar-dollared-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'dollared nil t))
    (ar-th-trim 'dollared nil t)))

(defalias 'ar-dollared-underscore-atpt 'ar-underscore-dollared-atpt)
(defun ar-underscore-dollared-atpt (&optional no-delimiters)
  "Put underscore char around DOLLARED. "
  (interactive "*p")
  (ar-th-underscore 'dollared no-delimiters (interactive-p)))

(defalias 'ar-dollared-whitespace-atpt 'ar-whitespace-dollared-atpt)
(defun ar-whitespace-dollared-atpt (&optional no-delimiters)
  "Put whitespace char around DOLLARED. "
  (interactive "*p")
  (ar-th-whitespace 'dollared nil t))

(defalias 'ar-forward-dollared-atpt 'ar-dollared-forward-atpt)
(defun ar-dollared-forward-atpt (&optional arg)
  "Moves forward over dollared at point if any, does nothing otherwise.
Returns end position of dollared "
  (interactive "p")
  (ar-th-forward 'dollared arg (interactive-p)))

(defalias 'ar-backward-dollared-atpt 'ar-dollared-backward-atpt)
(defun ar-dollared-backward-atpt (&optional arg)
  "Moves backward over dollared before point if any, does nothing otherwise.
Returns beginning position of dollared "
  (interactive "p")
  (ar-th-backward 'dollared arg (interactive-p)))

(defalias 'ar-transpose-dollared-atpt 'ar-dollared-transpose-atpt)
(defun ar-dollared-transpose-atpt (&optional arg)
  "Transposes dollared with dollared before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'dollared arg (interactive-p)))

(defalias 'ar-sort-dollared-atpt 'ar-dollared-sort-atpt)
(defun ar-dollared-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts dollareds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'dollared reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-dollared-atpt 'ar-dollared-check-atpt)
(defun ar-dollared-check-atpt ()
  "Return t if a dollared at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-dollared-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-dollared-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-doublequoted-atpt (&optional arg no-delimiters)
  "Returns doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'doublequoted arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-doublequoted-atpt 'ar-doublequoted-bounds-atpt)
(defun ar-doublequoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of doublequoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-doublequoted-atpt 'ar-doublequoted-beginning-atpt)
(defun ar-doublequoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-end-of-doublequoted-atpt 'ar-doublequoted-end-atpt)
(defun ar-doublequoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-length-of-doublequoted-atpt 'ar-doublequoted-length-atpt)
(defun ar-doublequoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-copy-doublequoted-atpt 'ar-doublequoted-copy-atpt)
(defun ar-doublequoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-delete-doublequoted-in-region 'ar-doublequoted-delete-in-region)
(defun ar-doublequoted-delete-in-region (beg end)
  "Deletes doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'doublequoted beg end (interactive-p)))

(defun ar-blok-doublequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublequoted.

If region is active, do that for all elements \"doublequoted\" in region.
  Returns blok or nil if no doublequoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-escape-doublequoted-atpt 'ar-doublequoted-escape-atpt)
(defun ar-doublequoted-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'doublequoted no-delimiters))

(defalias 'ar-doublequote-doublequoted-atpt 'ar-doublequoted-doublequote-atpt)
(defun ar-doublequoted-doublequote-atpt (&optional no-delimiters)
  "Doublequotes doublequoted at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-slash-doublequoted-atpt 'ar-doublequoted-slash-atpt)
(defun ar-doublequoted-slash-atpt (&optional no-delimiters)
  "Put slashes around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-doublequoted-atpt 'ar-doublequoted-double-backslash-atpt)
(defun ar-doublequoted-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-doublequoted-atpt 'ar-doublequoted-doubleslash-atpt)
(defun ar-doublequoted-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-doublequoted-atpt 'ar-doublequoted-doubleslash-paren-atpt)
(defun ar-doublequoted-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublequoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-slashparen-doublequoted-atpt 'ar-doublequoted-slashparen-atpt)
(defun ar-doublequoted-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-dollar-doublequoted-atpt 'ar-doublequoted-dollar-atpt)
(defun ar-doublequoted-dollar-atpt (&optional no-delimiters)
  "Doublequotes doublequoted at point if any. "
  (interactive "*p")
  (ar-th-dollar 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-equalize-doublequoted-atpt 'ar-doublequoted-equalize-atpt)
(defun ar-doublequoted-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around doublequoted at point if any. "
  (interactive "*p")
  (ar-th-equalize 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-doublequoted-atpt 'ar-doublequoted-greater-angle-atpt)
(defun ar-doublequoted-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for doublequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-doublequoted-atpt 'ar-doublequoted-lesser-angle-atpt)
(defun ar-doublequoted-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for doublequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-backslash-doublequoted-atpt 'ar-doublequoted-backslash-atpt)
(defun ar-doublequoted-backslash-atpt (&optional no-delimiters)
  "Backslash doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-brace-doublequoted-atpt 'ar-doublequoted-brace-atpt)
(defun ar-doublequoted-brace-atpt (&optional no-delimiters)
  "Braces doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-bracket-doublequoted-atpt 'ar-doublequoted-bracket-atpt)
(defun ar-doublequoted-bracket-atpt (&optional no-delimiters)
  "Brackets doublequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'doublequoted no-delimiters (interactive-p)))

(defun ar-comment-doublequoted-atpt (&optional no-delimiters)
  "Comments doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'doublequoted no-delimiters (interactive-p)))

(defun ar-commatize-doublequoted-atpt (&optional no-delimiters)
  "Put a comma after doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'doublequoted no-delimiters (interactive-p)))

(defun ar-quote-doublequoted-atpt (&optional no-delimiters)
  "Put a singlequote before doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-hyphen-doublequoted-atpt 'ar-doublequoted-hyphen-atpt)
(defun ar-doublequoted-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-mark-doublequoted-atpt 'ar-doublequoted-mark-atpt)
(defun ar-doublequoted-mark-atpt (&optional no-delimiters)
  "Marks doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'doublequoted))

(defalias 'ar-hide-doublequoted-atpt 'ar-doublequoted-hide-atpt)
(defun ar-doublequoted-hide-atpt ()
  "Hides doublequoted at point. "
  (interactive)
  (ar-th-hide 'doublequoted))

(defalias 'ar-show-doublequoted-atpt 'ar-doublequoted-show-atpt)
(defun ar-doublequoted-show-atpt ()
  "Shows hidden doublequoted at point. "
  (interactive)
  (ar-th-show 'doublequoted))

(defalias 'ar-hide-show-doublequoted-atpt 'ar-doublequoted-hide-show-atpt)
(defun ar-doublequoted-hide-show-atpt ()
  "Alternatively hides or shows doublequoted at point. "
  (interactive)
  (ar-th-hide-show 'doublequoted))

(defalias 'ar-highlight-doublequoted-atpt-mode 'ar-doublequoted-highlight-atpt-mode)
(defun ar-doublequoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles doublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-doublequoted-atpt 'ar-doublequoted-kill-atpt)
(defun ar-doublequoted-kill-atpt (&optional no-delimiters)
  "Kills doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-doublequoted-atpt 'ar-doublequoted-kill-backward-atpt)
(defun ar-doublequoted-kill-backward-atpt (&optional no-delimiters)
  "Kills doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-doublequoted-atpt 'ar-doublequoted-left-right-singlequote-atpt)
(defun ar-doublequoted-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-parentize-doublequoted-atpt 'ar-doublequoted-parentize-atpt)
(defun ar-doublequoted-parentize-atpt (&optional no-delimiters)
  "Parentizes doublequoted at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-separate-doublequoted-atpt 'ar-doublequoted-separate-atpt)
(defun ar-doublequoted-separate-atpt (&optional no-delimiters)
  "Separates doublequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-singlequote-doublequoted-atpt 'ar-doublequoted-singlequote-atpt)
(defun ar-doublequoted-singlequote-atpt (&optional no-delimiters)
  "Singlequotes doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-trim-doublequoted-atpt 'ar-doublequoted-trim-atpt)
(defun ar-doublequoted-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'doublequoted t t))
    (ar-th-trim 'doublequoted t t)))

(defalias 'ar-trim-left-doublequoted-atpt 'ar-doublequoted-left-trim-atpt)
(defun ar-doublequoted-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'doublequoted t nil))
    (ar-th-trim 'doublequoted t nil)))

(defalias 'ar-trim-right-doublequoted-atpt 'ar-doublequoted-right-trim-atpt)
(defun ar-doublequoted-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'doublequoted nil t))
    (ar-th-trim 'doublequoted nil t)))

(defalias 'ar-doublequoted-underscore-atpt 'ar-underscore-doublequoted-atpt)
(defun ar-underscore-doublequoted-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEQUOTED. "
  (interactive "*p")
  (ar-th-underscore 'doublequoted no-delimiters (interactive-p)))

(defalias 'ar-doublequoted-whitespace-atpt 'ar-whitespace-doublequoted-atpt)
(defun ar-whitespace-doublequoted-atpt (&optional no-delimiters)
  "Put whitespace char around DOUBLEQUOTED. "
  (interactive "*p")
  (ar-th-whitespace 'doublequoted nil t))

(defalias 'ar-forward-doublequoted-atpt 'ar-doublequoted-forward-atpt)
(defun ar-doublequoted-forward-atpt (&optional arg)
  "Moves forward over doublequoted at point if any, does nothing otherwise.
Returns end position of doublequoted "
  (interactive "p")
  (ar-th-forward 'doublequoted arg (interactive-p)))

(defalias 'ar-backward-doublequoted-atpt 'ar-doublequoted-backward-atpt)
(defun ar-doublequoted-backward-atpt (&optional arg)
  "Moves backward over doublequoted before point if any, does nothing otherwise.
Returns beginning position of doublequoted "
  (interactive "p")
  (ar-th-backward 'doublequoted arg (interactive-p)))

(defalias 'ar-transpose-doublequoted-atpt 'ar-doublequoted-transpose-atpt)
(defun ar-doublequoted-transpose-atpt (&optional arg)
  "Transposes doublequoted with doublequoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'doublequoted arg (interactive-p)))

(defalias 'ar-sort-doublequoted-atpt 'ar-doublequoted-sort-atpt)
(defun ar-doublequoted-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'doublequoted reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-doublequoted-atpt 'ar-doublequoted-check-atpt)
(defun ar-doublequoted-check-atpt ()
  "Return t if a doublequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doublequoted-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-doublequoted-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-equalized-atpt (&optional arg no-delimiters)
  "Returns equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'equalized arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-equalized-atpt 'ar-equalized-bounds-atpt)
(defun ar-equalized-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of equalized if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'equalized no-delimiters (interactive-p)))

(defun ar-equalized-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'equalized no-delimiters (interactive-p)))

(defun ar-equalized-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'equalized no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-equalized-atpt 'ar-equalized-beginning-atpt)
(defun ar-equalized-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'equalized no-delimiters (interactive-p)))

(defalias 'ar-end-of-equalized-atpt 'ar-equalized-end-atpt)
(defun ar-equalized-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'equalized no-delimiters (interactive-p)))

(defalias 'ar-length-of-equalized-atpt 'ar-equalized-length-atpt)
(defun ar-equalized-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'equalized no-delimiters (interactive-p)))

(defalias 'ar-copy-equalized-atpt 'ar-equalized-copy-atpt)
(defun ar-equalized-copy-atpt (&optional no-delimiters)
  "Returns a copy of equalized at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'equalized no-delimiters (interactive-p)))

(defalias 'ar-delete-equalized-in-region 'ar-equalized-delete-in-region)
(defun ar-equalized-delete-in-region (beg end)
  "Deletes equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'equalized beg end (interactive-p)))

(defun ar-blok-equalized-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around equalized.

If region is active, do that for all elements \"equalized\" in region.
  Returns blok or nil if no equalized at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'equalized no-delimiters (interactive-p)))

(defalias 'ar-escape-equalized-atpt 'ar-equalized-escape-atpt)
(defun ar-equalized-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted equalized at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'equalized no-delimiters))

(defalias 'ar-doublequote-equalized-atpt 'ar-equalized-doublequote-atpt)
(defun ar-equalized-doublequote-atpt (&optional no-delimiters)
  "Doublequotes equalized at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'equalized no-delimiters (interactive-p)))

(defalias 'ar-slash-equalized-atpt 'ar-equalized-slash-atpt)
(defun ar-equalized-slash-atpt (&optional no-delimiters)
  "Put slashes around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'equalized no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-equalized-atpt 'ar-equalized-double-backslash-atpt)
(defun ar-equalized-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'equalized no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-equalized-atpt 'ar-equalized-doubleslash-atpt)
(defun ar-equalized-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'equalized no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-equalized-atpt 'ar-equalized-doubleslash-paren-atpt)
(defun ar-equalized-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around equalized at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'equalized no-delimiters (interactive-p)))

(defalias 'ar-slashparen-equalized-atpt 'ar-equalized-slashparen-atpt)
(defun ar-equalized-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'equalized no-delimiters (interactive-p)))

(defalias 'ar-dollar-equalized-atpt 'ar-equalized-dollar-atpt)
(defun ar-equalized-dollar-atpt (&optional no-delimiters)
  "Doublequotes equalized at point if any. "
  (interactive "*p")
  (ar-th-dollar 'equalized no-delimiters (interactive-p)))

(defalias 'ar-equalize-equalized-atpt 'ar-equalized-equalize-atpt)
(defun ar-equalized-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around equalized at point if any. "
  (interactive "*p")
  (ar-th-equalize 'equalized no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-equalized-atpt 'ar-equalized-greater-angle-atpt)
(defun ar-equalized-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for equalized after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'equalized no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-equalized-atpt 'ar-equalized-lesser-angle-atpt)
(defun ar-equalized-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for equalized after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'equalized no-delimiters (interactive-p)))

(defalias 'ar-backslash-equalized-atpt 'ar-equalized-backslash-atpt)
(defun ar-equalized-backslash-atpt (&optional no-delimiters)
  "Backslash equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'equalized no-delimiters (interactive-p)))

(defalias 'ar-brace-equalized-atpt 'ar-equalized-brace-atpt)
(defun ar-equalized-brace-atpt (&optional no-delimiters)
  "Braces equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'equalized no-delimiters (interactive-p)))

(defalias 'ar-bracket-equalized-atpt 'ar-equalized-bracket-atpt)
(defun ar-equalized-bracket-atpt (&optional no-delimiters)
  "Brackets equalized after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'equalized no-delimiters (interactive-p)))

(defun ar-comment-equalized-atpt (&optional no-delimiters)
  "Comments equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'equalized no-delimiters (interactive-p)))

(defun ar-commatize-equalized-atpt (&optional no-delimiters)
  "Put a comma after equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'equalized no-delimiters (interactive-p)))

(defun ar-quote-equalized-atpt (&optional no-delimiters)
  "Put a singlequote before equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'equalized no-delimiters (interactive-p)))

(defalias 'ar-hyphen-equalized-atpt 'ar-equalized-hyphen-atpt)
(defun ar-equalized-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'equalized no-delimiters (interactive-p)))

(defalias 'ar-mark-equalized-atpt 'ar-equalized-mark-atpt)
(defun ar-equalized-mark-atpt (&optional no-delimiters)
  "Marks equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'equalized))

(defalias 'ar-hide-equalized-atpt 'ar-equalized-hide-atpt)
(defun ar-equalized-hide-atpt ()
  "Hides equalized at point. "
  (interactive)
  (ar-th-hide 'equalized))

(defalias 'ar-show-equalized-atpt 'ar-equalized-show-atpt)
(defun ar-equalized-show-atpt ()
  "Shows hidden equalized at point. "
  (interactive)
  (ar-th-show 'equalized))

(defalias 'ar-hide-show-equalized-atpt 'ar-equalized-hide-show-atpt)
(defun ar-equalized-hide-show-atpt ()
  "Alternatively hides or shows equalized at point. "
  (interactive)
  (ar-th-hide-show 'equalized))

(defalias 'ar-highlight-equalized-atpt-mode 'ar-equalized-highlight-atpt-mode)
(defun ar-equalized-highlight-atpt-mode (&optional no-delimiters)
  "Toggles equalized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'equalized no-delimiters (interactive-p)))

(defalias 'ar-kill-equalized-atpt 'ar-equalized-kill-atpt)
(defun ar-equalized-kill-atpt (&optional no-delimiters)
  "Kills equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'equalized no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-equalized-atpt 'ar-equalized-kill-backward-atpt)
(defun ar-equalized-kill-backward-atpt (&optional no-delimiters)
  "Kills equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'equalized no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-equalized-atpt 'ar-equalized-left-right-singlequote-atpt)
(defun ar-equalized-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'equalized no-delimiters (interactive-p)))

(defalias 'ar-parentize-equalized-atpt 'ar-equalized-parentize-atpt)
(defun ar-equalized-parentize-atpt (&optional no-delimiters)
  "Parentizes equalized at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'equalized no-delimiters (interactive-p)))

(defalias 'ar-separate-equalized-atpt 'ar-equalized-separate-atpt)
(defun ar-equalized-separate-atpt (&optional no-delimiters)
  "Separates equalized at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'equalized no-delimiters (interactive-p)))

(defalias 'ar-singlequote-equalized-atpt 'ar-equalized-singlequote-atpt)
(defun ar-equalized-singlequote-atpt (&optional no-delimiters)
  "Singlequotes equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'equalized no-delimiters (interactive-p)))

(defalias 'ar-trim-equalized-atpt 'ar-equalized-trim-atpt)
(defun ar-equalized-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'equalized t t))
    (ar-th-trim 'equalized t t)))

(defalias 'ar-trim-left-equalized-atpt 'ar-equalized-left-trim-atpt)
(defun ar-equalized-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'equalized t nil))
    (ar-th-trim 'equalized t nil)))

(defalias 'ar-trim-right-equalized-atpt 'ar-equalized-right-trim-atpt)
(defun ar-equalized-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'equalized nil t))
    (ar-th-trim 'equalized nil t)))

(defalias 'ar-equalized-underscore-atpt 'ar-underscore-equalized-atpt)
(defun ar-underscore-equalized-atpt (&optional no-delimiters)
  "Put underscore char around EQUALIZED. "
  (interactive "*p")
  (ar-th-underscore 'equalized no-delimiters (interactive-p)))

(defalias 'ar-equalized-whitespace-atpt 'ar-whitespace-equalized-atpt)
(defun ar-whitespace-equalized-atpt (&optional no-delimiters)
  "Put whitespace char around EQUALIZED. "
  (interactive "*p")
  (ar-th-whitespace 'equalized nil t))

(defalias 'ar-forward-equalized-atpt 'ar-equalized-forward-atpt)
(defun ar-equalized-forward-atpt (&optional arg)
  "Moves forward over equalized at point if any, does nothing otherwise.
Returns end position of equalized "
  (interactive "p")
  (ar-th-forward 'equalized arg (interactive-p)))

(defalias 'ar-backward-equalized-atpt 'ar-equalized-backward-atpt)
(defun ar-equalized-backward-atpt (&optional arg)
  "Moves backward over equalized before point if any, does nothing otherwise.
Returns beginning position of equalized "
  (interactive "p")
  (ar-th-backward 'equalized arg (interactive-p)))

(defalias 'ar-transpose-equalized-atpt 'ar-equalized-transpose-atpt)
(defun ar-equalized-transpose-atpt (&optional arg)
  "Transposes equalized with equalized before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'equalized arg (interactive-p)))

(defalias 'ar-sort-equalized-atpt 'ar-equalized-sort-atpt)
(defun ar-equalized-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts equalizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'equalized reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-equalized-atpt 'ar-equalized-check-atpt)
(defun ar-equalized-check-atpt ()
  "Return t if a equalized at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-equalized-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-equalized-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-hyphened-atpt (&optional arg no-delimiters)
  "Returns hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'hyphened arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-hyphened-atpt 'ar-hyphened-bounds-atpt)
(defun ar-hyphened-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of hyphened if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'hyphened no-delimiters (interactive-p)))

(defun ar-hyphened-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'hyphened no-delimiters (interactive-p)))

(defun ar-hyphened-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-hyphened-atpt 'ar-hyphened-beginning-atpt)
(defun ar-hyphened-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-end-of-hyphened-atpt 'ar-hyphened-end-atpt)
(defun ar-hyphened-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-length-of-hyphened-atpt 'ar-hyphened-length-atpt)
(defun ar-hyphened-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-copy-hyphened-atpt 'ar-hyphened-copy-atpt)
(defun ar-hyphened-copy-atpt (&optional no-delimiters)
  "Returns a copy of hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-delete-hyphened-in-region 'ar-hyphened-delete-in-region)
(defun ar-hyphened-delete-in-region (beg end)
  "Deletes hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'hyphened beg end (interactive-p)))

(defun ar-blok-hyphened-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around hyphened.

If region is active, do that for all elements \"hyphened\" in region.
  Returns blok or nil if no hyphened at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-escape-hyphened-atpt 'ar-hyphened-escape-atpt)
(defun ar-hyphened-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'hyphened no-delimiters))

(defalias 'ar-doublequote-hyphened-atpt 'ar-hyphened-doublequote-atpt)
(defun ar-hyphened-doublequote-atpt (&optional no-delimiters)
  "Doublequotes hyphened at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-slash-hyphened-atpt 'ar-hyphened-slash-atpt)
(defun ar-hyphened-slash-atpt (&optional no-delimiters)
  "Put slashes around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-hyphened-atpt 'ar-hyphened-double-backslash-atpt)
(defun ar-hyphened-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-hyphened-atpt 'ar-hyphened-doubleslash-atpt)
(defun ar-hyphened-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-hyphened-atpt 'ar-hyphened-doubleslash-paren-atpt)
(defun ar-hyphened-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around hyphened at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-slashparen-hyphened-atpt 'ar-hyphened-slashparen-atpt)
(defun ar-hyphened-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-dollar-hyphened-atpt 'ar-hyphened-dollar-atpt)
(defun ar-hyphened-dollar-atpt (&optional no-delimiters)
  "Doublequotes hyphened at point if any. "
  (interactive "*p")
  (ar-th-dollar 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-equalize-hyphened-atpt 'ar-hyphened-equalize-atpt)
(defun ar-hyphened-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around hyphened at point if any. "
  (interactive "*p")
  (ar-th-equalize 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-hyphened-atpt 'ar-hyphened-greater-angle-atpt)
(defun ar-hyphened-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for hyphened after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-hyphened-atpt 'ar-hyphened-lesser-angle-atpt)
(defun ar-hyphened-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for hyphened after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-backslash-hyphened-atpt 'ar-hyphened-backslash-atpt)
(defun ar-hyphened-backslash-atpt (&optional no-delimiters)
  "Backslash hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-brace-hyphened-atpt 'ar-hyphened-brace-atpt)
(defun ar-hyphened-brace-atpt (&optional no-delimiters)
  "Braces hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-bracket-hyphened-atpt 'ar-hyphened-bracket-atpt)
(defun ar-hyphened-bracket-atpt (&optional no-delimiters)
  "Brackets hyphened after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'hyphened no-delimiters (interactive-p)))

(defun ar-comment-hyphened-atpt (&optional no-delimiters)
  "Comments hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'hyphened no-delimiters (interactive-p)))

(defun ar-commatize-hyphened-atpt (&optional no-delimiters)
  "Put a comma after hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'hyphened no-delimiters (interactive-p)))

(defun ar-quote-hyphened-atpt (&optional no-delimiters)
  "Put a singlequote before hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-hyphen-hyphened-atpt 'ar-hyphened-hyphen-atpt)
(defun ar-hyphened-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-mark-hyphened-atpt 'ar-hyphened-mark-atpt)
(defun ar-hyphened-mark-atpt (&optional no-delimiters)
  "Marks hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'hyphened))

(defalias 'ar-hide-hyphened-atpt 'ar-hyphened-hide-atpt)
(defun ar-hyphened-hide-atpt ()
  "Hides hyphened at point. "
  (interactive)
  (ar-th-hide 'hyphened))

(defalias 'ar-show-hyphened-atpt 'ar-hyphened-show-atpt)
(defun ar-hyphened-show-atpt ()
  "Shows hidden hyphened at point. "
  (interactive)
  (ar-th-show 'hyphened))

(defalias 'ar-hide-show-hyphened-atpt 'ar-hyphened-hide-show-atpt)
(defun ar-hyphened-hide-show-atpt ()
  "Alternatively hides or shows hyphened at point. "
  (interactive)
  (ar-th-hide-show 'hyphened))

(defalias 'ar-highlight-hyphened-atpt-mode 'ar-hyphened-highlight-atpt-mode)
(defun ar-hyphened-highlight-atpt-mode (&optional no-delimiters)
  "Toggles hyphened-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-kill-hyphened-atpt 'ar-hyphened-kill-atpt)
(defun ar-hyphened-kill-atpt (&optional no-delimiters)
  "Kills hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-hyphened-atpt 'ar-hyphened-kill-backward-atpt)
(defun ar-hyphened-kill-backward-atpt (&optional no-delimiters)
  "Kills hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-hyphened-atpt 'ar-hyphened-left-right-singlequote-atpt)
(defun ar-hyphened-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-parentize-hyphened-atpt 'ar-hyphened-parentize-atpt)
(defun ar-hyphened-parentize-atpt (&optional no-delimiters)
  "Parentizes hyphened at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-separate-hyphened-atpt 'ar-hyphened-separate-atpt)
(defun ar-hyphened-separate-atpt (&optional no-delimiters)
  "Separates hyphened at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-singlequote-hyphened-atpt 'ar-hyphened-singlequote-atpt)
(defun ar-hyphened-singlequote-atpt (&optional no-delimiters)
  "Singlequotes hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-trim-hyphened-atpt 'ar-hyphened-trim-atpt)
(defun ar-hyphened-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'hyphened t t))
    (ar-th-trim 'hyphened t t)))

(defalias 'ar-trim-left-hyphened-atpt 'ar-hyphened-left-trim-atpt)
(defun ar-hyphened-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'hyphened t nil))
    (ar-th-trim 'hyphened t nil)))

(defalias 'ar-trim-right-hyphened-atpt 'ar-hyphened-right-trim-atpt)
(defun ar-hyphened-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'hyphened nil t))
    (ar-th-trim 'hyphened nil t)))

(defalias 'ar-hyphened-underscore-atpt 'ar-underscore-hyphened-atpt)
(defun ar-underscore-hyphened-atpt (&optional no-delimiters)
  "Put underscore char around HYPHENED. "
  (interactive "*p")
  (ar-th-underscore 'hyphened no-delimiters (interactive-p)))

(defalias 'ar-hyphened-whitespace-atpt 'ar-whitespace-hyphened-atpt)
(defun ar-whitespace-hyphened-atpt (&optional no-delimiters)
  "Put whitespace char around HYPHENED. "
  (interactive "*p")
  (ar-th-whitespace 'hyphened nil t))

(defalias 'ar-forward-hyphened-atpt 'ar-hyphened-forward-atpt)
(defun ar-hyphened-forward-atpt (&optional arg)
  "Moves forward over hyphened at point if any, does nothing otherwise.
Returns end position of hyphened "
  (interactive "p")
  (ar-th-forward 'hyphened arg (interactive-p)))

(defalias 'ar-backward-hyphened-atpt 'ar-hyphened-backward-atpt)
(defun ar-hyphened-backward-atpt (&optional arg)
  "Moves backward over hyphened before point if any, does nothing otherwise.
Returns beginning position of hyphened "
  (interactive "p")
  (ar-th-backward 'hyphened arg (interactive-p)))

(defalias 'ar-transpose-hyphened-atpt 'ar-hyphened-transpose-atpt)
(defun ar-hyphened-transpose-atpt (&optional arg)
  "Transposes hyphened with hyphened before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'hyphened arg (interactive-p)))

(defalias 'ar-sort-hyphened-atpt 'ar-hyphened-sort-atpt)
(defun ar-hyphened-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts hypheneds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'hyphened reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-hyphened-atpt 'ar-hyphened-check-atpt)
(defun ar-hyphened-check-atpt ()
  "Return t if a hyphened at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-hyphened-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-hyphened-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-quoted-atpt (&optional arg no-delimiters)
  "Returns quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'quoted arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-quoted-atpt 'ar-quoted-bounds-atpt)
(defun ar-quoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of quoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'quoted no-delimiters (interactive-p)))

(defun ar-quoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'quoted no-delimiters (interactive-p)))

(defun ar-quoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'quoted no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-quoted-atpt 'ar-quoted-beginning-atpt)
(defun ar-quoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'quoted no-delimiters (interactive-p)))

(defalias 'ar-end-of-quoted-atpt 'ar-quoted-end-atpt)
(defun ar-quoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'quoted no-delimiters (interactive-p)))

(defalias 'ar-length-of-quoted-atpt 'ar-quoted-length-atpt)
(defun ar-quoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class quoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'quoted no-delimiters (interactive-p)))

(defalias 'ar-copy-quoted-atpt 'ar-quoted-copy-atpt)
(defun ar-quoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of quoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'quoted no-delimiters (interactive-p)))

(defalias 'ar-delete-quoted-in-region 'ar-quoted-delete-in-region)
(defun ar-quoted-delete-in-region (beg end)
  "Deletes quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'quoted beg end (interactive-p)))

(defun ar-blok-quoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around quoted.

If region is active, do that for all elements \"quoted\" in region.
  Returns blok or nil if no quoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'quoted no-delimiters (interactive-p)))

(defalias 'ar-escape-quoted-atpt 'ar-quoted-escape-atpt)
(defun ar-quoted-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted quoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'quoted no-delimiters))

(defalias 'ar-doublequote-quoted-atpt 'ar-quoted-doublequote-atpt)
(defun ar-quoted-doublequote-atpt (&optional no-delimiters)
  "Doublequotes quoted at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'quoted no-delimiters (interactive-p)))

(defalias 'ar-slash-quoted-atpt 'ar-quoted-slash-atpt)
(defun ar-quoted-slash-atpt (&optional no-delimiters)
  "Put slashes around quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'quoted no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-quoted-atpt 'ar-quoted-double-backslash-atpt)
(defun ar-quoted-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'quoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-quoted-atpt 'ar-quoted-doubleslash-atpt)
(defun ar-quoted-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'quoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-quoted-atpt 'ar-quoted-doubleslash-paren-atpt)
(defun ar-quoted-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around quoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'quoted no-delimiters (interactive-p)))

(defalias 'ar-slashparen-quoted-atpt 'ar-quoted-slashparen-atpt)
(defun ar-quoted-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'quoted no-delimiters (interactive-p)))

(defalias 'ar-dollar-quoted-atpt 'ar-quoted-dollar-atpt)
(defun ar-quoted-dollar-atpt (&optional no-delimiters)
  "Doublequotes quoted at point if any. "
  (interactive "*p")
  (ar-th-dollar 'quoted no-delimiters (interactive-p)))

(defalias 'ar-equalize-quoted-atpt 'ar-quoted-equalize-atpt)
(defun ar-quoted-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around quoted at point if any. "
  (interactive "*p")
  (ar-th-equalize 'quoted no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-quoted-atpt 'ar-quoted-greater-angle-atpt)
(defun ar-quoted-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for quoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'quoted no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-quoted-atpt 'ar-quoted-lesser-angle-atpt)
(defun ar-quoted-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for quoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'quoted no-delimiters (interactive-p)))

(defalias 'ar-backslash-quoted-atpt 'ar-quoted-backslash-atpt)
(defun ar-quoted-backslash-atpt (&optional no-delimiters)
  "Backslash quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'quoted no-delimiters (interactive-p)))

(defalias 'ar-brace-quoted-atpt 'ar-quoted-brace-atpt)
(defun ar-quoted-brace-atpt (&optional no-delimiters)
  "Braces quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'quoted no-delimiters (interactive-p)))

(defalias 'ar-bracket-quoted-atpt 'ar-quoted-bracket-atpt)
(defun ar-quoted-bracket-atpt (&optional no-delimiters)
  "Brackets quoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'quoted no-delimiters (interactive-p)))

(defun ar-comment-quoted-atpt (&optional no-delimiters)
  "Comments quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'quoted no-delimiters (interactive-p)))

(defun ar-commatize-quoted-atpt (&optional no-delimiters)
  "Put a comma after quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'quoted no-delimiters (interactive-p)))

(defun ar-quote-quoted-atpt (&optional no-delimiters)
  "Put a singlequote before quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'quoted no-delimiters (interactive-p)))

(defalias 'ar-hyphen-quoted-atpt 'ar-quoted-hyphen-atpt)
(defun ar-quoted-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'quoted no-delimiters (interactive-p)))

(defalias 'ar-mark-quoted-atpt 'ar-quoted-mark-atpt)
(defun ar-quoted-mark-atpt (&optional no-delimiters)
  "Marks quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'quoted))

(defalias 'ar-hide-quoted-atpt 'ar-quoted-hide-atpt)
(defun ar-quoted-hide-atpt ()
  "Hides quoted at point. "
  (interactive)
  (ar-th-hide 'quoted))

(defalias 'ar-show-quoted-atpt 'ar-quoted-show-atpt)
(defun ar-quoted-show-atpt ()
  "Shows hidden quoted at point. "
  (interactive)
  (ar-th-show 'quoted))

(defalias 'ar-hide-show-quoted-atpt 'ar-quoted-hide-show-atpt)
(defun ar-quoted-hide-show-atpt ()
  "Alternatively hides or shows quoted at point. "
  (interactive)
  (ar-th-hide-show 'quoted))

(defalias 'ar-highlight-quoted-atpt-mode 'ar-quoted-highlight-atpt-mode)
(defun ar-quoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles quoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'quoted no-delimiters (interactive-p)))

(defalias 'ar-kill-quoted-atpt 'ar-quoted-kill-atpt)
(defun ar-quoted-kill-atpt (&optional no-delimiters)
  "Kills quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'quoted no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-quoted-atpt 'ar-quoted-kill-backward-atpt)
(defun ar-quoted-kill-backward-atpt (&optional no-delimiters)
  "Kills quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'quoted no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-quoted-atpt 'ar-quoted-left-right-singlequote-atpt)
(defun ar-quoted-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'quoted no-delimiters (interactive-p)))

(defalias 'ar-parentize-quoted-atpt 'ar-quoted-parentize-atpt)
(defun ar-quoted-parentize-atpt (&optional no-delimiters)
  "Parentizes quoted at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'quoted no-delimiters (interactive-p)))

(defalias 'ar-separate-quoted-atpt 'ar-quoted-separate-atpt)
(defun ar-quoted-separate-atpt (&optional no-delimiters)
  "Separates quoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'quoted no-delimiters (interactive-p)))

(defalias 'ar-singlequote-quoted-atpt 'ar-quoted-singlequote-atpt)
(defun ar-quoted-singlequote-atpt (&optional no-delimiters)
  "Singlequotes quoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'quoted no-delimiters (interactive-p)))

(defalias 'ar-trim-quoted-atpt 'ar-quoted-trim-atpt)
(defun ar-quoted-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'quoted t t))
    (ar-th-trim 'quoted t t)))

(defalias 'ar-trim-left-quoted-atpt 'ar-quoted-left-trim-atpt)
(defun ar-quoted-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'quoted t nil))
    (ar-th-trim 'quoted t nil)))

(defalias 'ar-trim-right-quoted-atpt 'ar-quoted-right-trim-atpt)
(defun ar-quoted-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'quoted nil t))
    (ar-th-trim 'quoted nil t)))

(defalias 'ar-quoted-underscore-atpt 'ar-underscore-quoted-atpt)
(defun ar-underscore-quoted-atpt (&optional no-delimiters)
  "Put underscore char around QUOTED. "
  (interactive "*p")
  (ar-th-underscore 'quoted no-delimiters (interactive-p)))

(defalias 'ar-quoted-whitespace-atpt 'ar-whitespace-quoted-atpt)
(defun ar-whitespace-quoted-atpt (&optional no-delimiters)
  "Put whitespace char around QUOTED. "
  (interactive "*p")
  (ar-th-whitespace 'quoted nil t))

(defalias 'ar-forward-quoted-atpt 'ar-quoted-forward-atpt)
(defun ar-quoted-forward-atpt (&optional arg)
  "Moves forward over quoted at point if any, does nothing otherwise.
Returns end position of quoted "
  (interactive "p")
  (ar-th-forward 'quoted arg (interactive-p)))

(defalias 'ar-backward-quoted-atpt 'ar-quoted-backward-atpt)
(defun ar-quoted-backward-atpt (&optional arg)
  "Moves backward over quoted before point if any, does nothing otherwise.
Returns beginning position of quoted "
  (interactive "p")
  (ar-th-backward 'quoted arg (interactive-p)))

(defalias 'ar-transpose-quoted-atpt 'ar-quoted-transpose-atpt)
(defun ar-quoted-transpose-atpt (&optional arg)
  "Transposes quoted with quoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'quoted arg (interactive-p)))

(defalias 'ar-sort-quoted-atpt 'ar-quoted-sort-atpt)
(defun ar-quoted-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts quoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'quoted reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-quoted-atpt 'ar-quoted-check-atpt)
(defun ar-quoted-check-atpt ()
  "Return t if a quoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-quoted-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-quoted-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-singlequoted-atpt (&optional arg no-delimiters)
  "Returns singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'singlequoted arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-singlequoted-atpt 'ar-singlequoted-bounds-atpt)
(defun ar-singlequoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of singlequoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'singlequoted no-delimiters (interactive-p)))

(defun ar-singlequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'singlequoted no-delimiters (interactive-p)))

(defun ar-singlequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-singlequoted-atpt 'ar-singlequoted-beginning-atpt)
(defun ar-singlequoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-end-of-singlequoted-atpt 'ar-singlequoted-end-atpt)
(defun ar-singlequoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-length-of-singlequoted-atpt 'ar-singlequoted-length-atpt)
(defun ar-singlequoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-copy-singlequoted-atpt 'ar-singlequoted-copy-atpt)
(defun ar-singlequoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-delete-singlequoted-in-region 'ar-singlequoted-delete-in-region)
(defun ar-singlequoted-delete-in-region (beg end)
  "Deletes singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'singlequoted beg end (interactive-p)))

(defun ar-blok-singlequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around singlequoted.

If region is active, do that for all elements \"singlequoted\" in region.
  Returns blok or nil if no singlequoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-escape-singlequoted-atpt 'ar-singlequoted-escape-atpt)
(defun ar-singlequoted-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'singlequoted no-delimiters))

(defalias 'ar-doublequote-singlequoted-atpt 'ar-singlequoted-doublequote-atpt)
(defun ar-singlequoted-doublequote-atpt (&optional no-delimiters)
  "Doublequotes singlequoted at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-slash-singlequoted-atpt 'ar-singlequoted-slash-atpt)
(defun ar-singlequoted-slash-atpt (&optional no-delimiters)
  "Put slashes around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-singlequoted-atpt 'ar-singlequoted-double-backslash-atpt)
(defun ar-singlequoted-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-singlequoted-atpt 'ar-singlequoted-doubleslash-atpt)
(defun ar-singlequoted-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-singlequoted-atpt 'ar-singlequoted-doubleslash-paren-atpt)
(defun ar-singlequoted-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around singlequoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-slashparen-singlequoted-atpt 'ar-singlequoted-slashparen-atpt)
(defun ar-singlequoted-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-dollar-singlequoted-atpt 'ar-singlequoted-dollar-atpt)
(defun ar-singlequoted-dollar-atpt (&optional no-delimiters)
  "Doublequotes singlequoted at point if any. "
  (interactive "*p")
  (ar-th-dollar 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-equalize-singlequoted-atpt 'ar-singlequoted-equalize-atpt)
(defun ar-singlequoted-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around singlequoted at point if any. "
  (interactive "*p")
  (ar-th-equalize 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-singlequoted-atpt 'ar-singlequoted-greater-angle-atpt)
(defun ar-singlequoted-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for singlequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-singlequoted-atpt 'ar-singlequoted-lesser-angle-atpt)
(defun ar-singlequoted-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for singlequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-backslash-singlequoted-atpt 'ar-singlequoted-backslash-atpt)
(defun ar-singlequoted-backslash-atpt (&optional no-delimiters)
  "Backslash singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-brace-singlequoted-atpt 'ar-singlequoted-brace-atpt)
(defun ar-singlequoted-brace-atpt (&optional no-delimiters)
  "Braces singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-bracket-singlequoted-atpt 'ar-singlequoted-bracket-atpt)
(defun ar-singlequoted-bracket-atpt (&optional no-delimiters)
  "Brackets singlequoted after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'singlequoted no-delimiters (interactive-p)))

(defun ar-comment-singlequoted-atpt (&optional no-delimiters)
  "Comments singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'singlequoted no-delimiters (interactive-p)))

(defun ar-commatize-singlequoted-atpt (&optional no-delimiters)
  "Put a comma after singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'singlequoted no-delimiters (interactive-p)))

(defun ar-quote-singlequoted-atpt (&optional no-delimiters)
  "Put a singlequote before singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-hyphen-singlequoted-atpt 'ar-singlequoted-hyphen-atpt)
(defun ar-singlequoted-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-mark-singlequoted-atpt 'ar-singlequoted-mark-atpt)
(defun ar-singlequoted-mark-atpt (&optional no-delimiters)
  "Marks singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'singlequoted))

(defalias 'ar-hide-singlequoted-atpt 'ar-singlequoted-hide-atpt)
(defun ar-singlequoted-hide-atpt ()
  "Hides singlequoted at point. "
  (interactive)
  (ar-th-hide 'singlequoted))

(defalias 'ar-show-singlequoted-atpt 'ar-singlequoted-show-atpt)
(defun ar-singlequoted-show-atpt ()
  "Shows hidden singlequoted at point. "
  (interactive)
  (ar-th-show 'singlequoted))

(defalias 'ar-hide-show-singlequoted-atpt 'ar-singlequoted-hide-show-atpt)
(defun ar-singlequoted-hide-show-atpt ()
  "Alternatively hides or shows singlequoted at point. "
  (interactive)
  (ar-th-hide-show 'singlequoted))

(defalias 'ar-highlight-singlequoted-atpt-mode 'ar-singlequoted-highlight-atpt-mode)
(defun ar-singlequoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles singlequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-singlequoted-atpt 'ar-singlequoted-kill-atpt)
(defun ar-singlequoted-kill-atpt (&optional no-delimiters)
  "Kills singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-singlequoted-atpt 'ar-singlequoted-kill-backward-atpt)
(defun ar-singlequoted-kill-backward-atpt (&optional no-delimiters)
  "Kills singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-singlequoted-atpt 'ar-singlequoted-left-right-singlequote-atpt)
(defun ar-singlequoted-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-parentize-singlequoted-atpt 'ar-singlequoted-parentize-atpt)
(defun ar-singlequoted-parentize-atpt (&optional no-delimiters)
  "Parentizes singlequoted at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-separate-singlequoted-atpt 'ar-singlequoted-separate-atpt)
(defun ar-singlequoted-separate-atpt (&optional no-delimiters)
  "Separates singlequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-singlequote-singlequoted-atpt 'ar-singlequoted-singlequote-atpt)
(defun ar-singlequoted-singlequote-atpt (&optional no-delimiters)
  "Singlequotes singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-trim-singlequoted-atpt 'ar-singlequoted-trim-atpt)
(defun ar-singlequoted-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'singlequoted t t))
    (ar-th-trim 'singlequoted t t)))

(defalias 'ar-trim-left-singlequoted-atpt 'ar-singlequoted-left-trim-atpt)
(defun ar-singlequoted-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'singlequoted t nil))
    (ar-th-trim 'singlequoted t nil)))

(defalias 'ar-trim-right-singlequoted-atpt 'ar-singlequoted-right-trim-atpt)
(defun ar-singlequoted-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'singlequoted nil t))
    (ar-th-trim 'singlequoted nil t)))

(defalias 'ar-singlequoted-underscore-atpt 'ar-underscore-singlequoted-atpt)
(defun ar-underscore-singlequoted-atpt (&optional no-delimiters)
  "Put underscore char around SINGLEQUOTED. "
  (interactive "*p")
  (ar-th-underscore 'singlequoted no-delimiters (interactive-p)))

(defalias 'ar-singlequoted-whitespace-atpt 'ar-whitespace-singlequoted-atpt)
(defun ar-whitespace-singlequoted-atpt (&optional no-delimiters)
  "Put whitespace char around SINGLEQUOTED. "
  (interactive "*p")
  (ar-th-whitespace 'singlequoted nil t))

(defalias 'ar-forward-singlequoted-atpt 'ar-singlequoted-forward-atpt)
(defun ar-singlequoted-forward-atpt (&optional arg)
  "Moves forward over singlequoted at point if any, does nothing otherwise.
Returns end position of singlequoted "
  (interactive "p")
  (ar-th-forward 'singlequoted arg (interactive-p)))

(defalias 'ar-backward-singlequoted-atpt 'ar-singlequoted-backward-atpt)
(defun ar-singlequoted-backward-atpt (&optional arg)
  "Moves backward over singlequoted before point if any, does nothing otherwise.
Returns beginning position of singlequoted "
  (interactive "p")
  (ar-th-backward 'singlequoted arg (interactive-p)))

(defalias 'ar-transpose-singlequoted-atpt 'ar-singlequoted-transpose-atpt)
(defun ar-singlequoted-transpose-atpt (&optional arg)
  "Transposes singlequoted with singlequoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'singlequoted arg (interactive-p)))

(defalias 'ar-sort-singlequoted-atpt 'ar-singlequoted-sort-atpt)
(defun ar-singlequoted-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts singlequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'singlequoted reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-singlequoted-atpt 'ar-singlequoted-check-atpt)
(defun ar-singlequoted-check-atpt ()
  "Return t if a singlequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-singlequoted-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-singlequoted-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-slashed-atpt (&optional arg no-delimiters)
  "Returns slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'slashed arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-slashed-atpt 'ar-slashed-bounds-atpt)
(defun ar-slashed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of slashed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'slashed no-delimiters (interactive-p)))

(defun ar-slashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'slashed no-delimiters (interactive-p)))

(defun ar-slashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'slashed no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-slashed-atpt 'ar-slashed-beginning-atpt)
(defun ar-slashed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'slashed no-delimiters (interactive-p)))

(defalias 'ar-end-of-slashed-atpt 'ar-slashed-end-atpt)
(defun ar-slashed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'slashed no-delimiters (interactive-p)))

(defalias 'ar-length-of-slashed-atpt 'ar-slashed-length-atpt)
(defun ar-slashed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'slashed no-delimiters (interactive-p)))

(defalias 'ar-copy-slashed-atpt 'ar-slashed-copy-atpt)
(defun ar-slashed-copy-atpt (&optional no-delimiters)
  "Returns a copy of slashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'slashed no-delimiters (interactive-p)))

(defalias 'ar-delete-slashed-in-region 'ar-slashed-delete-in-region)
(defun ar-slashed-delete-in-region (beg end)
  "Deletes slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'slashed beg end (interactive-p)))

(defun ar-blok-slashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around slashed.

If region is active, do that for all elements \"slashed\" in region.
  Returns blok or nil if no slashed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'slashed no-delimiters (interactive-p)))

(defalias 'ar-escape-slashed-atpt 'ar-slashed-escape-atpt)
(defun ar-slashed-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted slashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'slashed no-delimiters))

(defalias 'ar-doublequote-slashed-atpt 'ar-slashed-doublequote-atpt)
(defun ar-slashed-doublequote-atpt (&optional no-delimiters)
  "Doublequotes slashed at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'slashed no-delimiters (interactive-p)))

(defalias 'ar-slash-slashed-atpt 'ar-slashed-slash-atpt)
(defun ar-slashed-slash-atpt (&optional no-delimiters)
  "Put slashes around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'slashed no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-slashed-atpt 'ar-slashed-double-backslash-atpt)
(defun ar-slashed-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'slashed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-slashed-atpt 'ar-slashed-doubleslash-atpt)
(defun ar-slashed-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'slashed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-slashed-atpt 'ar-slashed-doubleslash-paren-atpt)
(defun ar-slashed-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around slashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'slashed no-delimiters (interactive-p)))

(defalias 'ar-slashparen-slashed-atpt 'ar-slashed-slashparen-atpt)
(defun ar-slashed-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'slashed no-delimiters (interactive-p)))

(defalias 'ar-dollar-slashed-atpt 'ar-slashed-dollar-atpt)
(defun ar-slashed-dollar-atpt (&optional no-delimiters)
  "Doublequotes slashed at point if any. "
  (interactive "*p")
  (ar-th-dollar 'slashed no-delimiters (interactive-p)))

(defalias 'ar-equalize-slashed-atpt 'ar-slashed-equalize-atpt)
(defun ar-slashed-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around slashed at point if any. "
  (interactive "*p")
  (ar-th-equalize 'slashed no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-slashed-atpt 'ar-slashed-greater-angle-atpt)
(defun ar-slashed-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for slashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'slashed no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-slashed-atpt 'ar-slashed-lesser-angle-atpt)
(defun ar-slashed-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for slashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'slashed no-delimiters (interactive-p)))

(defalias 'ar-backslash-slashed-atpt 'ar-slashed-backslash-atpt)
(defun ar-slashed-backslash-atpt (&optional no-delimiters)
  "Backslash slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'slashed no-delimiters (interactive-p)))

(defalias 'ar-brace-slashed-atpt 'ar-slashed-brace-atpt)
(defun ar-slashed-brace-atpt (&optional no-delimiters)
  "Braces slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'slashed no-delimiters (interactive-p)))

(defalias 'ar-bracket-slashed-atpt 'ar-slashed-bracket-atpt)
(defun ar-slashed-bracket-atpt (&optional no-delimiters)
  "Brackets slashed after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'slashed no-delimiters (interactive-p)))

(defun ar-comment-slashed-atpt (&optional no-delimiters)
  "Comments slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'slashed no-delimiters (interactive-p)))

(defun ar-commatize-slashed-atpt (&optional no-delimiters)
  "Put a comma after slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'slashed no-delimiters (interactive-p)))

(defun ar-quote-slashed-atpt (&optional no-delimiters)
  "Put a singlequote before slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'slashed no-delimiters (interactive-p)))

(defalias 'ar-hyphen-slashed-atpt 'ar-slashed-hyphen-atpt)
(defun ar-slashed-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'slashed no-delimiters (interactive-p)))

(defalias 'ar-mark-slashed-atpt 'ar-slashed-mark-atpt)
(defun ar-slashed-mark-atpt (&optional no-delimiters)
  "Marks slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'slashed))

(defalias 'ar-hide-slashed-atpt 'ar-slashed-hide-atpt)
(defun ar-slashed-hide-atpt ()
  "Hides slashed at point. "
  (interactive)
  (ar-th-hide 'slashed))

(defalias 'ar-show-slashed-atpt 'ar-slashed-show-atpt)
(defun ar-slashed-show-atpt ()
  "Shows hidden slashed at point. "
  (interactive)
  (ar-th-show 'slashed))

(defalias 'ar-hide-show-slashed-atpt 'ar-slashed-hide-show-atpt)
(defun ar-slashed-hide-show-atpt ()
  "Alternatively hides or shows slashed at point. "
  (interactive)
  (ar-th-hide-show 'slashed))

(defalias 'ar-highlight-slashed-atpt-mode 'ar-slashed-highlight-atpt-mode)
(defun ar-slashed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles slashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashed no-delimiters (interactive-p)))

(defalias 'ar-kill-slashed-atpt 'ar-slashed-kill-atpt)
(defun ar-slashed-kill-atpt (&optional no-delimiters)
  "Kills slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'slashed no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-slashed-atpt 'ar-slashed-kill-backward-atpt)
(defun ar-slashed-kill-backward-atpt (&optional no-delimiters)
  "Kills slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'slashed no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-slashed-atpt 'ar-slashed-left-right-singlequote-atpt)
(defun ar-slashed-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'slashed no-delimiters (interactive-p)))

(defalias 'ar-parentize-slashed-atpt 'ar-slashed-parentize-atpt)
(defun ar-slashed-parentize-atpt (&optional no-delimiters)
  "Parentizes slashed at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'slashed no-delimiters (interactive-p)))

(defalias 'ar-separate-slashed-atpt 'ar-slashed-separate-atpt)
(defun ar-slashed-separate-atpt (&optional no-delimiters)
  "Separates slashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'slashed no-delimiters (interactive-p)))

(defalias 'ar-singlequote-slashed-atpt 'ar-slashed-singlequote-atpt)
(defun ar-slashed-singlequote-atpt (&optional no-delimiters)
  "Singlequotes slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'slashed no-delimiters (interactive-p)))

(defalias 'ar-trim-slashed-atpt 'ar-slashed-trim-atpt)
(defun ar-slashed-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'slashed t t))
    (ar-th-trim 'slashed t t)))

(defalias 'ar-trim-left-slashed-atpt 'ar-slashed-left-trim-atpt)
(defun ar-slashed-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'slashed t nil))
    (ar-th-trim 'slashed t nil)))

(defalias 'ar-trim-right-slashed-atpt 'ar-slashed-right-trim-atpt)
(defun ar-slashed-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'slashed nil t))
    (ar-th-trim 'slashed nil t)))

(defalias 'ar-slashed-underscore-atpt 'ar-underscore-slashed-atpt)
(defun ar-underscore-slashed-atpt (&optional no-delimiters)
  "Put underscore char around SLASHED. "
  (interactive "*p")
  (ar-th-underscore 'slashed no-delimiters (interactive-p)))

(defalias 'ar-slashed-whitespace-atpt 'ar-whitespace-slashed-atpt)
(defun ar-whitespace-slashed-atpt (&optional no-delimiters)
  "Put whitespace char around SLASHED. "
  (interactive "*p")
  (ar-th-whitespace 'slashed nil t))

(defalias 'ar-forward-slashed-atpt 'ar-slashed-forward-atpt)
(defun ar-slashed-forward-atpt (&optional arg)
  "Moves forward over slashed at point if any, does nothing otherwise.
Returns end position of slashed "
  (interactive "p")
  (ar-th-forward 'slashed arg (interactive-p)))

(defalias 'ar-backward-slashed-atpt 'ar-slashed-backward-atpt)
(defun ar-slashed-backward-atpt (&optional arg)
  "Moves backward over slashed before point if any, does nothing otherwise.
Returns beginning position of slashed "
  (interactive "p")
  (ar-th-backward 'slashed arg (interactive-p)))

(defalias 'ar-transpose-slashed-atpt 'ar-slashed-transpose-atpt)
(defun ar-slashed-transpose-atpt (&optional arg)
  "Transposes slashed with slashed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'slashed arg (interactive-p)))

(defalias 'ar-sort-slashed-atpt 'ar-slashed-sort-atpt)
(defun ar-slashed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts slasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'slashed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-slashed-atpt 'ar-slashed-check-atpt)
(defun ar-slashed-check-atpt ()
  "Return t if a slashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-slashed-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-slashed-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-underscored-atpt (&optional arg no-delimiters)
  "Returns underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'underscored arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-underscored-atpt 'ar-underscored-bounds-atpt)
(defun ar-underscored-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of underscored if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'underscored no-delimiters (interactive-p)))

(defun ar-underscored-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'underscored no-delimiters (interactive-p)))

(defun ar-underscored-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'underscored no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-underscored-atpt 'ar-underscored-beginning-atpt)
(defun ar-underscored-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'underscored no-delimiters (interactive-p)))

(defalias 'ar-end-of-underscored-atpt 'ar-underscored-end-atpt)
(defun ar-underscored-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'underscored no-delimiters (interactive-p)))

(defalias 'ar-length-of-underscored-atpt 'ar-underscored-length-atpt)
(defun ar-underscored-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'underscored no-delimiters (interactive-p)))

(defalias 'ar-copy-underscored-atpt 'ar-underscored-copy-atpt)
(defun ar-underscored-copy-atpt (&optional no-delimiters)
  "Returns a copy of underscored at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'underscored no-delimiters (interactive-p)))

(defalias 'ar-delete-underscored-in-region 'ar-underscored-delete-in-region)
(defun ar-underscored-delete-in-region (beg end)
  "Deletes underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'underscored beg end (interactive-p)))

(defun ar-blok-underscored-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around underscored.

If region is active, do that for all elements \"underscored\" in region.
  Returns blok or nil if no underscored at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'underscored no-delimiters (interactive-p)))

(defalias 'ar-escape-underscored-atpt 'ar-underscored-escape-atpt)
(defun ar-underscored-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted underscored at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'underscored no-delimiters))

(defalias 'ar-doublequote-underscored-atpt 'ar-underscored-doublequote-atpt)
(defun ar-underscored-doublequote-atpt (&optional no-delimiters)
  "Doublequotes underscored at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'underscored no-delimiters (interactive-p)))

(defalias 'ar-slash-underscored-atpt 'ar-underscored-slash-atpt)
(defun ar-underscored-slash-atpt (&optional no-delimiters)
  "Put slashes around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'underscored no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-underscored-atpt 'ar-underscored-double-backslash-atpt)
(defun ar-underscored-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'underscored no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-underscored-atpt 'ar-underscored-doubleslash-atpt)
(defun ar-underscored-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'underscored no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-underscored-atpt 'ar-underscored-doubleslash-paren-atpt)
(defun ar-underscored-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around underscored at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'underscored no-delimiters (interactive-p)))

(defalias 'ar-slashparen-underscored-atpt 'ar-underscored-slashparen-atpt)
(defun ar-underscored-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'underscored no-delimiters (interactive-p)))

(defalias 'ar-dollar-underscored-atpt 'ar-underscored-dollar-atpt)
(defun ar-underscored-dollar-atpt (&optional no-delimiters)
  "Doublequotes underscored at point if any. "
  (interactive "*p")
  (ar-th-dollar 'underscored no-delimiters (interactive-p)))

(defalias 'ar-equalize-underscored-atpt 'ar-underscored-equalize-atpt)
(defun ar-underscored-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around underscored at point if any. "
  (interactive "*p")
  (ar-th-equalize 'underscored no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-underscored-atpt 'ar-underscored-greater-angle-atpt)
(defun ar-underscored-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for underscored after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'underscored no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-underscored-atpt 'ar-underscored-lesser-angle-atpt)
(defun ar-underscored-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for underscored after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'underscored no-delimiters (interactive-p)))

(defalias 'ar-backslash-underscored-atpt 'ar-underscored-backslash-atpt)
(defun ar-underscored-backslash-atpt (&optional no-delimiters)
  "Backslash underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'underscored no-delimiters (interactive-p)))

(defalias 'ar-brace-underscored-atpt 'ar-underscored-brace-atpt)
(defun ar-underscored-brace-atpt (&optional no-delimiters)
  "Braces underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'underscored no-delimiters (interactive-p)))

(defalias 'ar-bracket-underscored-atpt 'ar-underscored-bracket-atpt)
(defun ar-underscored-bracket-atpt (&optional no-delimiters)
  "Brackets underscored after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'underscored no-delimiters (interactive-p)))

(defun ar-comment-underscored-atpt (&optional no-delimiters)
  "Comments underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'underscored no-delimiters (interactive-p)))

(defun ar-commatize-underscored-atpt (&optional no-delimiters)
  "Put a comma after underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'underscored no-delimiters (interactive-p)))

(defun ar-quote-underscored-atpt (&optional no-delimiters)
  "Put a singlequote before underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'underscored no-delimiters (interactive-p)))

(defalias 'ar-hyphen-underscored-atpt 'ar-underscored-hyphen-atpt)
(defun ar-underscored-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'underscored no-delimiters (interactive-p)))

(defalias 'ar-mark-underscored-atpt 'ar-underscored-mark-atpt)
(defun ar-underscored-mark-atpt (&optional no-delimiters)
  "Marks underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'underscored))

(defalias 'ar-hide-underscored-atpt 'ar-underscored-hide-atpt)
(defun ar-underscored-hide-atpt ()
  "Hides underscored at point. "
  (interactive)
  (ar-th-hide 'underscored))

(defalias 'ar-show-underscored-atpt 'ar-underscored-show-atpt)
(defun ar-underscored-show-atpt ()
  "Shows hidden underscored at point. "
  (interactive)
  (ar-th-show 'underscored))

(defalias 'ar-hide-show-underscored-atpt 'ar-underscored-hide-show-atpt)
(defun ar-underscored-hide-show-atpt ()
  "Alternatively hides or shows underscored at point. "
  (interactive)
  (ar-th-hide-show 'underscored))

(defalias 'ar-highlight-underscored-atpt-mode 'ar-underscored-highlight-atpt-mode)
(defun ar-underscored-highlight-atpt-mode (&optional no-delimiters)
  "Toggles underscored-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'underscored no-delimiters (interactive-p)))

(defalias 'ar-kill-underscored-atpt 'ar-underscored-kill-atpt)
(defun ar-underscored-kill-atpt (&optional no-delimiters)
  "Kills underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'underscored no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-underscored-atpt 'ar-underscored-kill-backward-atpt)
(defun ar-underscored-kill-backward-atpt (&optional no-delimiters)
  "Kills underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'underscored no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-underscored-atpt 'ar-underscored-left-right-singlequote-atpt)
(defun ar-underscored-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'underscored no-delimiters (interactive-p)))

(defalias 'ar-parentize-underscored-atpt 'ar-underscored-parentize-atpt)
(defun ar-underscored-parentize-atpt (&optional no-delimiters)
  "Parentizes underscored at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'underscored no-delimiters (interactive-p)))

(defalias 'ar-separate-underscored-atpt 'ar-underscored-separate-atpt)
(defun ar-underscored-separate-atpt (&optional no-delimiters)
  "Separates underscored at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'underscored no-delimiters (interactive-p)))

(defalias 'ar-singlequote-underscored-atpt 'ar-underscored-singlequote-atpt)
(defun ar-underscored-singlequote-atpt (&optional no-delimiters)
  "Singlequotes underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'underscored no-delimiters (interactive-p)))

(defalias 'ar-trim-underscored-atpt 'ar-underscored-trim-atpt)
(defun ar-underscored-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'underscored t t))
    (ar-th-trim 'underscored t t)))

(defalias 'ar-trim-left-underscored-atpt 'ar-underscored-left-trim-atpt)
(defun ar-underscored-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'underscored t nil))
    (ar-th-trim 'underscored t nil)))

(defalias 'ar-trim-right-underscored-atpt 'ar-underscored-right-trim-atpt)
(defun ar-underscored-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'underscored nil t))
    (ar-th-trim 'underscored nil t)))

(defalias 'ar-underscored-underscore-atpt 'ar-underscore-underscored-atpt)
(defun ar-underscore-underscored-atpt (&optional no-delimiters)
  "Put underscore char around UNDERSCORED. "
  (interactive "*p")
  (ar-th-underscore 'underscored no-delimiters (interactive-p)))

(defalias 'ar-underscored-whitespace-atpt 'ar-whitespace-underscored-atpt)
(defun ar-whitespace-underscored-atpt (&optional no-delimiters)
  "Put whitespace char around UNDERSCORED. "
  (interactive "*p")
  (ar-th-whitespace 'underscored nil t))

(defalias 'ar-forward-underscored-atpt 'ar-underscored-forward-atpt)
(defun ar-underscored-forward-atpt (&optional arg)
  "Moves forward over underscored at point if any, does nothing otherwise.
Returns end position of underscored "
  (interactive "p")
  (ar-th-forward 'underscored arg (interactive-p)))

(defalias 'ar-backward-underscored-atpt 'ar-underscored-backward-atpt)
(defun ar-underscored-backward-atpt (&optional arg)
  "Moves backward over underscored before point if any, does nothing otherwise.
Returns beginning position of underscored "
  (interactive "p")
  (ar-th-backward 'underscored arg (interactive-p)))

(defalias 'ar-transpose-underscored-atpt 'ar-underscored-transpose-atpt)
(defun ar-underscored-transpose-atpt (&optional arg)
  "Transposes underscored with underscored before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'underscored arg (interactive-p)))

(defalias 'ar-sort-underscored-atpt 'ar-underscored-sort-atpt)
(defun ar-underscored-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts underscoreds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'underscored reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-underscored-atpt 'ar-underscored-check-atpt)
(defun ar-underscored-check-atpt ()
  "Return t if a underscored at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-underscored-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-underscored-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-whitespaced-atpt (&optional arg no-delimiters)
  "Returns whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'whitespaced arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-whitespaced-atpt 'ar-whitespaced-bounds-atpt)
(defun ar-whitespaced-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of whitespaced if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-bounds 'whitespaced no-delimiters (interactive-p)))

(defun ar-whitespaced-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'whitespaced no-delimiters (interactive-p)))

(defun ar-whitespaced-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-whitespaced-atpt 'ar-whitespaced-beginning-atpt)
(defun ar-whitespaced-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-end-of-whitespaced-atpt 'ar-whitespaced-end-atpt)
(defun ar-whitespaced-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-length-of-whitespaced-atpt 'ar-whitespaced-length-atpt)
(defun ar-whitespaced-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-copy-whitespaced-atpt 'ar-whitespaced-copy-atpt)
(defun ar-whitespaced-copy-atpt (&optional no-delimiters)
  "Returns a copy of whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-delete-whitespaced-in-region 'ar-whitespaced-delete-in-region)
(defun ar-whitespaced-delete-in-region (beg end)
  "Deletes whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'whitespaced beg end (interactive-p)))

(defun ar-blok-whitespaced-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around whitespaced.

If region is active, do that for all elements \"whitespaced\" in region.
  Returns blok or nil if no whitespaced at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-escape-whitespaced-atpt 'ar-whitespaced-escape-atpt)
(defun ar-whitespaced-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-escape 'whitespaced no-delimiters))

(defalias 'ar-doublequote-whitespaced-atpt 'ar-whitespaced-doublequote-atpt)
(defun ar-whitespaced-doublequote-atpt (&optional no-delimiters)
  "Doublequotes whitespaced at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-slash-whitespaced-atpt 'ar-whitespaced-slash-atpt)
(defun ar-whitespaced-slash-atpt (&optional no-delimiters)
  "Put slashes around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-whitespaced-atpt 'ar-whitespaced-double-backslash-atpt)
(defun ar-whitespaced-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-double-backslash 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-whitespaced-atpt 'ar-whitespaced-doubleslash-atpt)
(defun ar-whitespaced-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-whitespaced-atpt 'ar-whitespaced-doubleslash-paren-atpt)
(defun ar-whitespaced-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around whitespaced at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash-paren 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-slashparen-whitespaced-atpt 'ar-whitespaced-slashparen-atpt)
(defun ar-whitespaced-slash-paren-atpt (&optional no-delimiters)
  "Provides slashed parentheses around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-slash-paren 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-dollar-whitespaced-atpt 'ar-whitespaced-dollar-atpt)
(defun ar-whitespaced-dollar-atpt (&optional no-delimiters)
  "Doublequotes whitespaced at point if any. "
  (interactive "*p")
  (ar-th-dollar 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-equalize-whitespaced-atpt 'ar-whitespaced-equalize-atpt)
(defun ar-whitespaced-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around whitespaced at point if any. "
  (interactive "*p")
  (ar-th-equalize 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-whitespaced-atpt 'ar-whitespaced-greater-angle-atpt)
(defun ar-whitespaced-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for whitespaced after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-greater-angle 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-whitespaced-atpt 'ar-whitespaced-lesser-angle-atpt)
(defun ar-whitespaced-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for whitespaced after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-lesser-angle 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-backslash-whitespaced-atpt 'ar-whitespaced-backslash-atpt)
(defun ar-whitespaced-backslash-atpt (&optional no-delimiters)
  "Backslash whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslash 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-brace-whitespaced-atpt 'ar-whitespaced-brace-atpt)
(defun ar-whitespaced-brace-atpt (&optional no-delimiters)
  "Braces whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-brace 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-bracket-whitespaced-atpt 'ar-whitespaced-bracket-atpt)
(defun ar-whitespaced-bracket-atpt (&optional no-delimiters)
  "Brackets whitespaced after point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-bracket 'whitespaced no-delimiters (interactive-p)))

(defun ar-comment-whitespaced-atpt (&optional no-delimiters)
  "Comments whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'whitespaced no-delimiters (interactive-p)))

(defun ar-commatize-whitespaced-atpt (&optional no-delimiters)
  "Put a comma after whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'whitespaced no-delimiters (interactive-p)))

(defun ar-quote-whitespaced-atpt (&optional no-delimiters)
  "Put a singlequote before whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-quote 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-hyphen-whitespaced-atpt 'ar-whitespaced-hyphen-atpt)
(defun ar-whitespaced-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-hyphen 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-mark-whitespaced-atpt 'ar-whitespaced-mark-atpt)
(defun ar-whitespaced-mark-atpt (&optional no-delimiters)
  "Marks whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'whitespaced))

(defalias 'ar-hide-whitespaced-atpt 'ar-whitespaced-hide-atpt)
(defun ar-whitespaced-hide-atpt ()
  "Hides whitespaced at point. "
  (interactive)
  (ar-th-hide 'whitespaced))

(defalias 'ar-show-whitespaced-atpt 'ar-whitespaced-show-atpt)
(defun ar-whitespaced-show-atpt ()
  "Shows hidden whitespaced at point. "
  (interactive)
  (ar-th-show 'whitespaced))

(defalias 'ar-hide-show-whitespaced-atpt 'ar-whitespaced-hide-show-atpt)
(defun ar-whitespaced-hide-show-atpt ()
  "Alternatively hides or shows whitespaced at point. "
  (interactive)
  (ar-th-hide-show 'whitespaced))

(defalias 'ar-highlight-whitespaced-atpt-mode 'ar-whitespaced-highlight-atpt-mode)
(defun ar-whitespaced-highlight-atpt-mode (&optional no-delimiters)
  "Toggles whitespaced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-kill-whitespaced-atpt 'ar-whitespaced-kill-atpt)
(defun ar-whitespaced-kill-atpt (&optional no-delimiters)
  "Kills whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-whitespaced-atpt 'ar-whitespaced-kill-backward-atpt)
(defun ar-whitespaced-kill-backward-atpt (&optional no-delimiters)
  "Kills whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-whitespaced-atpt 'ar-whitespaced-left-right-singlequote-atpt)
(defun ar-whitespaced-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-left-right-singlequote 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-parentize-whitespaced-atpt 'ar-whitespaced-parentize-atpt)
(defun ar-whitespaced-parentize-atpt (&optional no-delimiters)
  "Parentizes whitespaced at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-separate-whitespaced-atpt 'ar-whitespaced-separate-atpt)
(defun ar-whitespaced-separate-atpt (&optional no-delimiters)
  "Separates whitespaced at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-singlequote-whitespaced-atpt 'ar-whitespaced-singlequote-atpt)
(defun ar-whitespaced-singlequote-atpt (&optional no-delimiters)
  "Singlequotes whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-singlequote 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-trim-whitespaced-atpt 'ar-whitespaced-trim-atpt)
(defun ar-whitespaced-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'whitespaced t t))
    (ar-th-trim 'whitespaced t t)))

(defalias 'ar-trim-left-whitespaced-atpt 'ar-whitespaced-left-trim-atpt)
(defun ar-whitespaced-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'whitespaced t nil))
    (ar-th-trim 'whitespaced t nil)))

(defalias 'ar-trim-right-whitespaced-atpt 'ar-whitespaced-right-trim-atpt)
(defun ar-whitespaced-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'whitespaced nil t))
    (ar-th-trim 'whitespaced nil t)))

(defalias 'ar-whitespaced-underscore-atpt 'ar-underscore-whitespaced-atpt)
(defun ar-underscore-whitespaced-atpt (&optional no-delimiters)
  "Put underscore char around WHITESPACED. "
  (interactive "*p")
  (ar-th-underscore 'whitespaced no-delimiters (interactive-p)))

(defalias 'ar-whitespaced-whitespace-atpt 'ar-whitespace-whitespaced-atpt)
(defun ar-whitespace-whitespaced-atpt (&optional no-delimiters)
  "Put whitespace char around WHITESPACED. "
  (interactive "*p")
  (ar-th-whitespace 'whitespaced nil t))

(defalias 'ar-forward-whitespaced-atpt 'ar-whitespaced-forward-atpt)
(defun ar-whitespaced-forward-atpt (&optional arg)
  "Moves forward over whitespaced at point if any, does nothing otherwise.
Returns end position of whitespaced "
  (interactive "p")
  (ar-th-forward 'whitespaced arg (interactive-p)))

(defalias 'ar-backward-whitespaced-atpt 'ar-whitespaced-backward-atpt)
(defun ar-whitespaced-backward-atpt (&optional arg)
  "Moves backward over whitespaced before point if any, does nothing otherwise.
Returns beginning position of whitespaced "
  (interactive "p")
  (ar-th-backward 'whitespaced arg (interactive-p)))

(defalias 'ar-transpose-whitespaced-atpt 'ar-whitespaced-transpose-atpt)
(defun ar-whitespaced-transpose-atpt (&optional arg)
  "Transposes whitespaced with whitespaced before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'whitespaced arg (interactive-p)))

(defalias 'ar-sort-whitespaced-atpt 'ar-whitespaced-sort-atpt)
(defun ar-whitespaced-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts whitespaceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'whitespaced reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-whitespaced-atpt 'ar-whitespaced-check-atpt)
(defun ar-whitespaced-check-atpt ()
  "Return t if a whitespaced at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-whitespaced-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-whitespaced-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-raw end

;; ar-thing-at-point-utils-delimited-unpaired: ar-unpaired-delimited-raw start

(defalias 'ar-in-backslashed-p-atpt 'ar-backslashed-in-p-atpt)
(defun ar-backslashed-in-p-atpt (&optional condition)
  "Returns beginning position of ` backslashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\\\" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-dollared-p-atpt 'ar-dollared-in-p-atpt)
(defun ar-dollared-in-p-atpt (&optional condition)
  "Returns beginning position of ` dollared' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\\\\\$" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-doublequoted-p-atpt 'ar-doublequoted-in-p-atpt)
(defun ar-doublequoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` doublequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-equalized-p-atpt 'ar-equalized-in-p-atpt)
(defun ar-equalized-in-p-atpt (&optional condition)
  "Returns beginning position of ` equalized' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "=" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-hyphened-p-atpt 'ar-hyphened-in-p-atpt)
(defun ar-hyphened-in-p-atpt (&optional condition)
  "Returns beginning position of ` hyphened' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "-" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-quoted-p-atpt 'ar-quoted-in-p-atpt)
(defun ar-quoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` quoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"\\|'" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-singlequoted-p-atpt 'ar-singlequoted-in-p-atpt)
(defun ar-singlequoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` singlequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "'" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-slashed-p-atpt 'ar-slashed-in-p-atpt)
(defun ar-slashed-in-p-atpt (&optional condition)
  "Returns beginning position of ` slashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "/" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-underscored-p-atpt 'ar-underscored-in-p-atpt)
(defun ar-underscored-in-p-atpt (&optional condition)
  "Returns beginning position of ` underscored' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "_" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-whitespaced-p-atpt 'ar-whitespaced-in-p-atpt)
(defun ar-whitespaced-in-p-atpt (&optional condition)
  "Returns beginning position of ` whitespaced' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base " " condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimited-unpaired: ar-unpaired-delimited-raw end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-delimited-list start

(defun ar-braced-atpt (&optional arg no-delimiters)
  "Returns braced at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'braced arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-braced-atpt 'ar-braced-bounds-atpt)
(defun ar-braced-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of braced if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters (interactive-p)))

(defun ar-braced-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BRACED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'braced no-delimiters (interactive-p)))

(defun ar-braced-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'braced no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-braced-atpt 'ar-braced-beginning-atpt)
(defun ar-braced-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'braced no-delimiters (interactive-p)))

(defalias 'ar-end-of-braced-atpt 'ar-braced-end-atpt)
(defun ar-braced-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'braced no-delimiters (interactive-p)))

(defalias 'ar-in-braced-p-atpt 'ar-braced-in-p-atpt)
(defun ar-braced-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BRACED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters (interactive-p)))

(defalias 'ar-length-of-braced-atpt 'ar-braced-length-atpt)
(defun ar-braced-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'braced no-delimiters (interactive-p)))

(defalias 'ar-copy-braced-atpt 'ar-braced-copy-atpt)
(defun ar-braced-copy-atpt (&optional no-delimiters)
  "Returns a copy of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'braced no-delimiters (interactive-p)))

(defalias 'ar-delete-braced-in-region 'ar-braced-delete-in-region)
(defun ar-braced-delete-in-region (beg end)
  "Deletes BRACED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'braced beg end (interactive-p)))

(defun ar-blok-braced-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around braced.
  Returns blok or nil if no BRACED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'braced no-delimiters (interactive-p)))

(defalias 'ar-escape-braced-atpt 'ar-braced-escape-atpt)
(defun ar-braced-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted BRACED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'braced no-delimiters))

(defalias 'ar-doublequote-braced-atpt 'ar-braced-doublequote-atpt)
(defun ar-braced-doublequote-atpt (&optional no-delimiters)
  "Doublequotes BRACED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'braced no-delimiters (interactive-p)))

(defalias 'ar-slash-braced-atpt 'ar-braced-slash-atpt)
(defun ar-braced-slash-atpt (&optional no-delimiters)
  "Doublequotes BRACED at point if any. "
  (interactive "*p")
  (ar-th-slash 'braced no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-braced-atpt 'ar-braced-double-backslash-atpt)
(defun ar-braced-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BRACED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'braced no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-braced-atpt 'ar-braced-doubleslash-atpt)
(defun ar-braced-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around BRACED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'braced no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-braced-atpt 'ar-braced-doubleslash-paren-atpt)
(defun ar-braced-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BRACED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'braced no-delimiters (interactive-p)))

(defalias 'ar-slashparen-braced-atpt 'ar-braced-slashparen-atpt)
(defun ar-braced-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BRACED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'braced no-delimiters (interactive-p)))

(defalias 'ar-dollar-braced-atpt 'ar-braced-dollar-atpt)
(defun ar-braced-dollar-atpt (&optional no-delimiters)
  "Doublequotes BRACED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'braced no-delimiters (interactive-p)))

(defalias 'ar-equalize-braced-atpt 'ar-braced-equalize-atpt)
(defun ar-braced-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around BRACED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'braced no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-braced-atpt 'ar-braced-greater-angle-atpt)
(defun ar-braced-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for BRACED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'braced no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-braced-atpt 'ar-braced-lesser-angle-atpt)
(defun ar-braced-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for BRACED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'braced no-delimiters (interactive-p)))

(defalias 'ar-backslash-braced-atpt 'ar-braced-backslash-atpt)
(defun ar-braced-backslash-atpt (&optional no-delimiters)
  "Backslash BRACED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'braced no-delimiters (interactive-p)))

(defalias 'ar-brace-braced-atpt 'ar-braced-brace-atpt)
(defun ar-braced-brace-atpt (&optional no-delimiters)
  "Braces BRACED at point if any. "
  (interactive "*p")
  (ar-th-brace 'braced no-delimiters (interactive-p)))

(defalias 'ar-bracket-braced-atpt 'ar-braced-bracket-atpt)
(defun ar-braced-bracket-atpt (&optional no-delimiters)
  "Brackets BRACED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'braced no-delimiters (interactive-p)))

(defun ar-comment-braced-atpt (&optional no-delimiters)
  "Comments BRACED at point if any. "
  (interactive "*p")
  (ar-th-comment 'braced no-delimiters (interactive-p)))

(defun ar-commatize-braced-atpt (&optional no-delimiters)
  "Put a comma after BRACED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'braced no-delimiters (interactive-p)))

(defun ar-quote-braced-atpt (&optional no-delimiters)
  "Put a singlequote before BRACED at point if any. "
  (interactive "*p")
  (ar-th-quote 'braced no-delimiters (interactive-p)))

(defalias 'ar-hyphen-braced-atpt 'ar-braced-hyphen-atpt)
(defun ar-braced-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around BRACED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'braced no-delimiters (interactive-p)))

(defalias 'ar-mark-braced-atpt 'ar-braced-mark-atpt)
(defun ar-braced-mark-atpt ()
  "Marks BRACED at point if any. "
  (interactive)
  (ar-th-mark 'braced))

(defalias 'ar-hide-braced-atpt 'ar-braced-hide-atpt)
(defun ar-braced-hide-atpt ()
  "Hides BRACED at point. "
  (interactive)
  (ar-th-hide 'braced))

(defalias 'ar-show-braced-atpt 'ar-braced-show-atpt)
(defun ar-braced-show-atpt ()
  "Shows hidden BRACED at point. "
  (interactive)
  (ar-th-show 'braced))

(defalias 'ar-hide-show-braced-atpt 'ar-braced-hide-show-atpt)
(defun ar-braced-hide-show-atpt ()
  "Alternatively hides or shows BRACED at point. "
  (interactive)
  (ar-th-hide-show 'braced))

(defalias 'ar-highlight-braced-atpt-mode 'ar-braced-highlight-atpt-mode)

(defun ar-braced-highlight-atpt-mode (&optional no-delimiters)
  "Toggles braced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'braced no-delimiters (interactive-p)))

(defalias 'ar-kill-braced-atpt 'ar-braced-kill-atpt)
(defun ar-braced-kill-atpt (&optional no-delimiters)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill 'braced no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-braced-atpt 'ar-braced-kill-backward-atpt)
(defun ar-braced-kill-backward-atpt (&optional no-delimiters)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'braced no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-braced-atpt 'ar-braced-left-right-singlequote-atpt)
(defun ar-braced-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'braced no-delimiters (interactive-p)))

(defalias 'ar-parentize-braced-atpt 'ar-braced-parentize-atpt)
(defun ar-braced-parentize-atpt (&optional no-delimiters)
  "Parentizes BRACED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'braced no-delimiters (interactive-p)))

(defalias 'ar-separate-braced-atpt 'ar-braced-separate-atpt)
(defun ar-braced-separate-atpt (&optional no-delimiters)
  "Separates BRACED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'braced no-delimiters (interactive-p)))

(defalias 'ar-singlequote-braced-atpt 'ar-braced-singlequote-atpt)
(defun ar-braced-singlequote-atpt (&optional no-delimiters)
  "Singlequotes BRACED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'braced no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-braced-atpt 'ar-braced-triplequote-dq-atpt)
(defun ar-braced-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around braced. "
  (interactive "*p")
  (ar-th-triplequote-dq 'braced no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-braced-atpt 'ar-braced-triplequote-sq-atpt)
(defun ar-braced-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around braced. "
  (interactive "*p")
  (ar-th-triplequote-sq 'braced no-delimiters (interactive-p)))

(defalias 'ar-trim-braced-atpt 'ar-braced-trim-atpt)
(defun ar-braced-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'braced t t))
    (ar-th-trim 'braced t t)))

(defalias 'ar-trim-left-braced-atpt 'ar-braced-left-trim-atpt)
(defun ar-braced-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'braced t nil))
    (ar-th-trim 'braced t nil)))

(defalias 'ar-trim-right-braced-atpt 'ar-braced-right-trim-atpt)
(defun ar-braced-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'braced nil t))
    (ar-th-trim 'braced nil t)))

(defalias 'ar-braced-underscore-atpt 'ar-underscore-braced-atpt)
(defun ar-underscore-braced-atpt (&optional no-delimiters)
  "Put underscore char around BRACED. "
  (interactive "*p")
  (ar-th-underscore 'braced no-delimiters (interactive-p)))

(defalias 'ar-braced-whitespace-atpt 'ar-whitespace-braced-atpt)
(defun ar-whitespace-braced-atpt (&optional no-delimiters)
  "Put whitespace char around BRACED. "
  (interactive "*p")
  (ar-th-whitespace 'braced nil t))

(defalias 'ar-forward-braced-atpt 'ar-braced-forward-atpt)
(defun ar-braced-forward-atpt (&optional arg)
  "Moves forward over BRACED at point if any, does nothing otherwise.
Returns end position of BRACED "
  (interactive "p")
  (ar-th-forward 'braced arg (interactive-p)))

(defalias 'ar-backward-braced-atpt 'ar-braced-backward-atpt)
(defun ar-braced-backward-atpt (&optional arg)
  "Moves backward over BRACED before point if any, does nothing otherwise.
Returns beginning position of BRACED "
  (interactive "p")
  (ar-th-backward 'braced arg (interactive-p)))

(defalias 'ar-transpose-braced-atpt 'ar-braced-transpose-atpt)
(defun ar-braced-transpose-atpt (&optional arg)
  "Transposes BRACED with BRACED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'braced arg (interactive-p)))

(defalias 'ar-sort-braced-atpt 'ar-braced-sort-atpt)
(defun ar-braced-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts braceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'braced reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-braced-atpt 'ar-braced-check-atpt)
(defun ar-braced-check-atpt ()
  "Return t if a BRACED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-braced-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-braced-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-bracketed-atpt (&optional arg no-delimiters)
  "Returns bracketed at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'bracketed arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-bracketed-atpt 'ar-bracketed-bounds-atpt)
(defun ar-bracketed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of bracketed if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BRACKETED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-bracketed-atpt 'ar-bracketed-beginning-atpt)
(defun ar-bracketed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-end-of-bracketed-atpt 'ar-bracketed-end-atpt)
(defun ar-bracketed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-in-bracketed-p-atpt 'ar-bracketed-in-p-atpt)
(defun ar-bracketed-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BRACKETED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-length-of-bracketed-atpt 'ar-bracketed-length-atpt)
(defun ar-bracketed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-copy-bracketed-atpt 'ar-bracketed-copy-atpt)
(defun ar-bracketed-copy-atpt (&optional no-delimiters)
  "Returns a copy of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-delete-bracketed-in-region 'ar-bracketed-delete-in-region)
(defun ar-bracketed-delete-in-region (beg end)
  "Deletes BRACKETED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'bracketed beg end (interactive-p)))

(defun ar-blok-bracketed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around bracketed.
  Returns blok or nil if no BRACKETED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-escape-bracketed-atpt 'ar-bracketed-escape-atpt)
(defun ar-bracketed-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted BRACKETED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'bracketed no-delimiters))

(defalias 'ar-doublequote-bracketed-atpt 'ar-bracketed-doublequote-atpt)
(defun ar-bracketed-doublequote-atpt (&optional no-delimiters)
  "Doublequotes BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-slash-bracketed-atpt 'ar-bracketed-slash-atpt)
(defun ar-bracketed-slash-atpt (&optional no-delimiters)
  "Doublequotes BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-slash 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-bracketed-atpt 'ar-bracketed-double-backslash-atpt)
(defun ar-bracketed-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-bracketed-atpt 'ar-bracketed-doubleslash-atpt)
(defun ar-bracketed-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-bracketed-atpt 'ar-bracketed-doubleslash-paren-atpt)
(defun ar-bracketed-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-slashparen-bracketed-atpt 'ar-bracketed-slashparen-atpt)
(defun ar-bracketed-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-dollar-bracketed-atpt 'ar-bracketed-dollar-atpt)
(defun ar-bracketed-dollar-atpt (&optional no-delimiters)
  "Doublequotes BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-equalize-bracketed-atpt 'ar-bracketed-equalize-atpt)
(defun ar-bracketed-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-bracketed-atpt 'ar-bracketed-greater-angle-atpt)
(defun ar-bracketed-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for BRACKETED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-bracketed-atpt 'ar-bracketed-lesser-angle-atpt)
(defun ar-bracketed-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for BRACKETED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-backslash-bracketed-atpt 'ar-bracketed-backslash-atpt)
(defun ar-bracketed-backslash-atpt (&optional no-delimiters)
  "Backslash BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-brace-bracketed-atpt 'ar-bracketed-brace-atpt)
(defun ar-bracketed-brace-atpt (&optional no-delimiters)
  "Braces BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-brace 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-bracket-bracketed-atpt 'ar-bracketed-bracket-atpt)
(defun ar-bracketed-bracket-atpt (&optional no-delimiters)
  "Brackets BRACKETED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'bracketed no-delimiters (interactive-p)))

(defun ar-comment-bracketed-atpt (&optional no-delimiters)
  "Comments BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-comment 'bracketed no-delimiters (interactive-p)))

(defun ar-commatize-bracketed-atpt (&optional no-delimiters)
  "Put a comma after BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'bracketed no-delimiters (interactive-p)))

(defun ar-quote-bracketed-atpt (&optional no-delimiters)
  "Put a singlequote before BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-quote 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-hyphen-bracketed-atpt 'ar-bracketed-hyphen-atpt)
(defun ar-bracketed-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-mark-bracketed-atpt 'ar-bracketed-mark-atpt)
(defun ar-bracketed-mark-atpt ()
  "Marks BRACKETED at point if any. "
  (interactive)
  (ar-th-mark 'bracketed))

(defalias 'ar-hide-bracketed-atpt 'ar-bracketed-hide-atpt)
(defun ar-bracketed-hide-atpt ()
  "Hides BRACKETED at point. "
  (interactive)
  (ar-th-hide 'bracketed))

(defalias 'ar-show-bracketed-atpt 'ar-bracketed-show-atpt)
(defun ar-bracketed-show-atpt ()
  "Shows hidden BRACKETED at point. "
  (interactive)
  (ar-th-show 'bracketed))

(defalias 'ar-hide-show-bracketed-atpt 'ar-bracketed-hide-show-atpt)
(defun ar-bracketed-hide-show-atpt ()
  "Alternatively hides or shows BRACKETED at point. "
  (interactive)
  (ar-th-hide-show 'bracketed))

(defalias 'ar-highlight-bracketed-atpt-mode 'ar-bracketed-highlight-atpt-mode)

(defun ar-bracketed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles bracketed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-kill-bracketed-atpt 'ar-bracketed-kill-atpt)
(defun ar-bracketed-kill-atpt (&optional no-delimiters)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-bracketed-atpt 'ar-bracketed-kill-backward-atpt)
(defun ar-bracketed-kill-backward-atpt (&optional no-delimiters)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-bracketed-atpt 'ar-bracketed-left-right-singlequote-atpt)
(defun ar-bracketed-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-parentize-bracketed-atpt 'ar-bracketed-parentize-atpt)
(defun ar-bracketed-parentize-atpt (&optional no-delimiters)
  "Parentizes BRACKETED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-separate-bracketed-atpt 'ar-bracketed-separate-atpt)
(defun ar-bracketed-separate-atpt (&optional no-delimiters)
  "Separates BRACKETED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-singlequote-bracketed-atpt 'ar-bracketed-singlequote-atpt)
(defun ar-bracketed-singlequote-atpt (&optional no-delimiters)
  "Singlequotes BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-bracketed-atpt 'ar-bracketed-triplequote-dq-atpt)
(defun ar-bracketed-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around bracketed. "
  (interactive "*p")
  (ar-th-triplequote-dq 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-bracketed-atpt 'ar-bracketed-triplequote-sq-atpt)
(defun ar-bracketed-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around bracketed. "
  (interactive "*p")
  (ar-th-triplequote-sq 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-trim-bracketed-atpt 'ar-bracketed-trim-atpt)
(defun ar-bracketed-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'bracketed t t))
    (ar-th-trim 'bracketed t t)))

(defalias 'ar-trim-left-bracketed-atpt 'ar-bracketed-left-trim-atpt)
(defun ar-bracketed-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'bracketed t nil))
    (ar-th-trim 'bracketed t nil)))

(defalias 'ar-trim-right-bracketed-atpt 'ar-bracketed-right-trim-atpt)
(defun ar-bracketed-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'bracketed nil t))
    (ar-th-trim 'bracketed nil t)))

(defalias 'ar-bracketed-underscore-atpt 'ar-underscore-bracketed-atpt)
(defun ar-underscore-bracketed-atpt (&optional no-delimiters)
  "Put underscore char around BRACKETED. "
  (interactive "*p")
  (ar-th-underscore 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-bracketed-whitespace-atpt 'ar-whitespace-bracketed-atpt)
(defun ar-whitespace-bracketed-atpt (&optional no-delimiters)
  "Put whitespace char around BRACKETED. "
  (interactive "*p")
  (ar-th-whitespace 'bracketed nil t))

(defalias 'ar-forward-bracketed-atpt 'ar-bracketed-forward-atpt)
(defun ar-bracketed-forward-atpt (&optional arg)
  "Moves forward over BRACKETED at point if any, does nothing otherwise.
Returns end position of BRACKETED "
  (interactive "p")
  (ar-th-forward 'bracketed arg (interactive-p)))

(defalias 'ar-backward-bracketed-atpt 'ar-bracketed-backward-atpt)
(defun ar-bracketed-backward-atpt (&optional arg)
  "Moves backward over BRACKETED before point if any, does nothing otherwise.
Returns beginning position of BRACKETED "
  (interactive "p")
  (ar-th-backward 'bracketed arg (interactive-p)))

(defalias 'ar-transpose-bracketed-atpt 'ar-bracketed-transpose-atpt)
(defun ar-bracketed-transpose-atpt (&optional arg)
  "Transposes BRACKETED with BRACKETED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'bracketed arg (interactive-p)))

(defalias 'ar-sort-bracketed-atpt 'ar-bracketed-sort-atpt)
(defun ar-bracketed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bracketeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'bracketed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-bracketed-atpt 'ar-bracketed-check-atpt)
(defun ar-bracketed-check-atpt ()
  "Return t if a BRACKETED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-bracketed-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-bracketed-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-lesser-angled-atpt (&optional arg no-delimiters)
  "Returns lesser-angled at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'lesser-angled arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-lesser-angled-atpt 'ar-lesser-angled-bounds-atpt)
(defun ar-lesser-angled-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of lesser-angled if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesser-angled no-delimiters (interactive-p)))

(defun ar-lesser-angled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LESSER-ANGLED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lesser-angled no-delimiters (interactive-p)))

(defun ar-lesser-angled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LESSER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-lesser-angled-atpt 'ar-lesser-angled-beginning-atpt)
(defun ar-lesser-angled-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LESSER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-end-of-lesser-angled-atpt 'ar-lesser-angled-end-atpt)
(defun ar-lesser-angled-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LESSER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-in-lesser-angled-p-atpt 'ar-lesser-angled-in-p-atpt)
(defun ar-lesser-angled-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LESSER-ANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-length-of-lesser-angled-atpt 'ar-lesser-angled-length-atpt)
(defun ar-lesser-angled-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LESSER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-copy-lesser-angled-atpt 'ar-lesser-angled-copy-atpt)
(defun ar-lesser-angled-copy-atpt (&optional no-delimiters)
  "Returns a copy of LESSER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-delete-lesser-angled-in-region 'ar-lesser-angled-delete-in-region)
(defun ar-lesser-angled-delete-in-region (beg end)
  "Deletes LESSER-ANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesser-angled beg end (interactive-p)))

(defun ar-blok-lesser-angled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesser-angled.
  Returns blok or nil if no LESSER-ANGLED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-escape-lesser-angled-atpt 'ar-lesser-angled-escape-atpt)
(defun ar-lesser-angled-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted LESSER-ANGLED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'lesser-angled no-delimiters))

(defalias 'ar-doublequote-lesser-angled-atpt 'ar-lesser-angled-doublequote-atpt)
(defun ar-lesser-angled-doublequote-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-slash-lesser-angled-atpt 'ar-lesser-angled-slash-atpt)
(defun ar-lesser-angled-slash-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-slash 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-lesser-angled-atpt 'ar-lesser-angled-double-backslash-atpt)
(defun ar-lesser-angled-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-lesser-angled-atpt 'ar-lesser-angled-doubleslash-atpt)
(defun ar-lesser-angled-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-lesser-angled-atpt 'ar-lesser-angled-doubleslash-paren-atpt)
(defun ar-lesser-angled-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-slashparen-lesser-angled-atpt 'ar-lesser-angled-slashparen-atpt)
(defun ar-lesser-angled-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-dollar-lesser-angled-atpt 'ar-lesser-angled-dollar-atpt)
(defun ar-lesser-angled-dollar-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-equalize-lesser-angled-atpt 'ar-lesser-angled-equalize-atpt)
(defun ar-lesser-angled-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-lesser-angled-atpt 'ar-lesser-angled-greater-angle-atpt)
(defun ar-lesser-angled-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for LESSER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-lesser-angled-atpt 'ar-lesser-angled-lesser-angle-atpt)
(defun ar-lesser-angled-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for LESSER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-backslash-lesser-angled-atpt 'ar-lesser-angled-backslash-atpt)
(defun ar-lesser-angled-backslash-atpt (&optional no-delimiters)
  "Backslash LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-brace-lesser-angled-atpt 'ar-lesser-angled-brace-atpt)
(defun ar-lesser-angled-brace-atpt (&optional no-delimiters)
  "Braces LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-brace 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-bracket-lesser-angled-atpt 'ar-lesser-angled-bracket-atpt)
(defun ar-lesser-angled-bracket-atpt (&optional no-delimiters)
  "Brackets LESSER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'lesser-angled no-delimiters (interactive-p)))

(defun ar-comment-lesser-angled-atpt (&optional no-delimiters)
  "Comments LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-comment 'lesser-angled no-delimiters (interactive-p)))

(defun ar-commatize-lesser-angled-atpt (&optional no-delimiters)
  "Put a comma after LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'lesser-angled no-delimiters (interactive-p)))

(defun ar-quote-lesser-angled-atpt (&optional no-delimiters)
  "Put a singlequote before LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-quote 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-hyphen-lesser-angled-atpt 'ar-lesser-angled-hyphen-atpt)
(defun ar-lesser-angled-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-mark-lesser-angled-atpt 'ar-lesser-angled-mark-atpt)
(defun ar-lesser-angled-mark-atpt ()
  "Marks LESSER-ANGLED at point if any. "
  (interactive)
  (ar-th-mark 'lesser-angled))

(defalias 'ar-hide-lesser-angled-atpt 'ar-lesser-angled-hide-atpt)
(defun ar-lesser-angled-hide-atpt ()
  "Hides LESSER-ANGLED at point. "
  (interactive)
  (ar-th-hide 'lesser-angled))

(defalias 'ar-show-lesser-angled-atpt 'ar-lesser-angled-show-atpt)
(defun ar-lesser-angled-show-atpt ()
  "Shows hidden LESSER-ANGLED at point. "
  (interactive)
  (ar-th-show 'lesser-angled))

(defalias 'ar-hide-show-lesser-angled-atpt 'ar-lesser-angled-hide-show-atpt)
(defun ar-lesser-angled-hide-show-atpt ()
  "Alternatively hides or shows LESSER-ANGLED at point. "
  (interactive)
  (ar-th-hide-show 'lesser-angled))

(defalias 'ar-highlight-lesser-angled-atpt-mode 'ar-lesser-angled-highlight-atpt-mode)

(defun ar-lesser-angled-highlight-atpt-mode (&optional no-delimiters)
  "Toggles lesser-angled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-kill-lesser-angled-atpt 'ar-lesser-angled-kill-atpt)
(defun ar-lesser-angled-kill-atpt (&optional no-delimiters)
  "Kills LESSER-ANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-lesser-angled-atpt 'ar-lesser-angled-kill-backward-atpt)
(defun ar-lesser-angled-kill-backward-atpt (&optional no-delimiters)
  "Kills LESSER-ANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-lesser-angled-atpt 'ar-lesser-angled-left-right-singlequote-atpt)
(defun ar-lesser-angled-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-parentize-lesser-angled-atpt 'ar-lesser-angled-parentize-atpt)
(defun ar-lesser-angled-parentize-atpt (&optional no-delimiters)
  "Parentizes LESSER-ANGLED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-separate-lesser-angled-atpt 'ar-lesser-angled-separate-atpt)
(defun ar-lesser-angled-separate-atpt (&optional no-delimiters)
  "Separates LESSER-ANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-singlequote-lesser-angled-atpt 'ar-lesser-angled-singlequote-atpt)
(defun ar-lesser-angled-singlequote-atpt (&optional no-delimiters)
  "Singlequotes LESSER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-lesser-angled-atpt 'ar-lesser-angled-triplequote-dq-atpt)
(defun ar-lesser-angled-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around lesser-angled. "
  (interactive "*p")
  (ar-th-triplequote-dq 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-lesser-angled-atpt 'ar-lesser-angled-triplequote-sq-atpt)
(defun ar-lesser-angled-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around lesser-angled. "
  (interactive "*p")
  (ar-th-triplequote-sq 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-trim-lesser-angled-atpt 'ar-lesser-angled-trim-atpt)
(defun ar-lesser-angled-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled t t))
    (ar-th-trim 'lesser-angled t t)))

(defalias 'ar-trim-left-lesser-angled-atpt 'ar-lesser-angled-left-trim-atpt)
(defun ar-lesser-angled-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled t nil))
    (ar-th-trim 'lesser-angled t nil)))

(defalias 'ar-trim-right-lesser-angled-atpt 'ar-lesser-angled-right-trim-atpt)
(defun ar-lesser-angled-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled nil t))
    (ar-th-trim 'lesser-angled nil t)))

(defalias 'ar-lesser-angled-underscore-atpt 'ar-underscore-lesser-angled-atpt)
(defun ar-underscore-lesser-angled-atpt (&optional no-delimiters)
  "Put underscore char around LESSER-ANGLED. "
  (interactive "*p")
  (ar-th-underscore 'lesser-angled no-delimiters (interactive-p)))

(defalias 'ar-lesser-angled-whitespace-atpt 'ar-whitespace-lesser-angled-atpt)
(defun ar-whitespace-lesser-angled-atpt (&optional no-delimiters)
  "Put whitespace char around LESSER-ANGLED. "
  (interactive "*p")
  (ar-th-whitespace 'lesser-angled nil t))

(defalias 'ar-forward-lesser-angled-atpt 'ar-lesser-angled-forward-atpt)
(defun ar-lesser-angled-forward-atpt (&optional arg)
  "Moves forward over LESSER-ANGLED at point if any, does nothing otherwise.
Returns end position of LESSER-ANGLED "
  (interactive "p")
  (ar-th-forward 'lesser-angled arg (interactive-p)))

(defalias 'ar-backward-lesser-angled-atpt 'ar-lesser-angled-backward-atpt)
(defun ar-lesser-angled-backward-atpt (&optional arg)
  "Moves backward over LESSER-ANGLED before point if any, does nothing otherwise.
Returns beginning position of LESSER-ANGLED "
  (interactive "p")
  (ar-th-backward 'lesser-angled arg (interactive-p)))

(defalias 'ar-transpose-lesser-angled-atpt 'ar-lesser-angled-transpose-atpt)
(defun ar-lesser-angled-transpose-atpt (&optional arg)
  "Transposes LESSER-ANGLED with LESSER-ANGLED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lesser-angled arg (interactive-p)))

(defalias 'ar-sort-lesser-angled-atpt 'ar-lesser-angled-sort-atpt)
(defun ar-lesser-angled-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesser-angleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'lesser-angled reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lesser-angled-atpt 'ar-lesser-angled-check-atpt)
(defun ar-lesser-angled-check-atpt ()
  "Return t if a LESSER-ANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesser-angled-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-lesser-angled-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-greater-angled-atpt (&optional arg no-delimiters)
  "Returns greater-angled at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'greater-angled arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-greater-angled-atpt 'ar-greater-angled-bounds-atpt)
(defun ar-greater-angled-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of greater-angled if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greater-angled no-delimiters (interactive-p)))

(defun ar-greater-angled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position GREATER-ANGLED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'greater-angled no-delimiters (interactive-p)))

(defun ar-greater-angled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of GREATER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-greater-angled-atpt 'ar-greater-angled-beginning-atpt)
(defun ar-greater-angled-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class GREATER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-end-of-greater-angled-atpt 'ar-greater-angled-end-atpt)
(defun ar-greater-angled-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class GREATER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-in-greater-angled-p-atpt 'ar-greater-angled-in-p-atpt)
(defun ar-greater-angled-in-p-atpt (&optional no-delimiters)
  "Returns bounds of GREATER-ANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-length-of-greater-angled-atpt 'ar-greater-angled-length-atpt)
(defun ar-greater-angled-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class GREATER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-copy-greater-angled-atpt 'ar-greater-angled-copy-atpt)
(defun ar-greater-angled-copy-atpt (&optional no-delimiters)
  "Returns a copy of GREATER-ANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-delete-greater-angled-in-region 'ar-greater-angled-delete-in-region)
(defun ar-greater-angled-delete-in-region (beg end)
  "Deletes GREATER-ANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greater-angled beg end (interactive-p)))

(defun ar-blok-greater-angled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greater-angled.
  Returns blok or nil if no GREATER-ANGLED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-escape-greater-angled-atpt 'ar-greater-angled-escape-atpt)
(defun ar-greater-angled-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted GREATER-ANGLED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'greater-angled no-delimiters))

(defalias 'ar-doublequote-greater-angled-atpt 'ar-greater-angled-doublequote-atpt)
(defun ar-greater-angled-doublequote-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-slash-greater-angled-atpt 'ar-greater-angled-slash-atpt)
(defun ar-greater-angled-slash-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-slash 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-greater-angled-atpt 'ar-greater-angled-double-backslash-atpt)
(defun ar-greater-angled-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-greater-angled-atpt 'ar-greater-angled-doubleslash-atpt)
(defun ar-greater-angled-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-greater-angled-atpt 'ar-greater-angled-doubleslash-paren-atpt)
(defun ar-greater-angled-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-slashparen-greater-angled-atpt 'ar-greater-angled-slashparen-atpt)
(defun ar-greater-angled-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-dollar-greater-angled-atpt 'ar-greater-angled-dollar-atpt)
(defun ar-greater-angled-dollar-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-equalize-greater-angled-atpt 'ar-greater-angled-equalize-atpt)
(defun ar-greater-angled-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-greater-angled-atpt 'ar-greater-angled-greater-angle-atpt)
(defun ar-greater-angled-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for GREATER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-greater-angled-atpt 'ar-greater-angled-lesser-angle-atpt)
(defun ar-greater-angled-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for GREATER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-backslash-greater-angled-atpt 'ar-greater-angled-backslash-atpt)
(defun ar-greater-angled-backslash-atpt (&optional no-delimiters)
  "Backslash GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-brace-greater-angled-atpt 'ar-greater-angled-brace-atpt)
(defun ar-greater-angled-brace-atpt (&optional no-delimiters)
  "Braces GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-brace 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-bracket-greater-angled-atpt 'ar-greater-angled-bracket-atpt)
(defun ar-greater-angled-bracket-atpt (&optional no-delimiters)
  "Brackets GREATER-ANGLED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'greater-angled no-delimiters (interactive-p)))

(defun ar-comment-greater-angled-atpt (&optional no-delimiters)
  "Comments GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-comment 'greater-angled no-delimiters (interactive-p)))

(defun ar-commatize-greater-angled-atpt (&optional no-delimiters)
  "Put a comma after GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'greater-angled no-delimiters (interactive-p)))

(defun ar-quote-greater-angled-atpt (&optional no-delimiters)
  "Put a singlequote before GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-quote 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-hyphen-greater-angled-atpt 'ar-greater-angled-hyphen-atpt)
(defun ar-greater-angled-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-mark-greater-angled-atpt 'ar-greater-angled-mark-atpt)
(defun ar-greater-angled-mark-atpt ()
  "Marks GREATER-ANGLED at point if any. "
  (interactive)
  (ar-th-mark 'greater-angled))

(defalias 'ar-hide-greater-angled-atpt 'ar-greater-angled-hide-atpt)
(defun ar-greater-angled-hide-atpt ()
  "Hides GREATER-ANGLED at point. "
  (interactive)
  (ar-th-hide 'greater-angled))

(defalias 'ar-show-greater-angled-atpt 'ar-greater-angled-show-atpt)
(defun ar-greater-angled-show-atpt ()
  "Shows hidden GREATER-ANGLED at point. "
  (interactive)
  (ar-th-show 'greater-angled))

(defalias 'ar-hide-show-greater-angled-atpt 'ar-greater-angled-hide-show-atpt)
(defun ar-greater-angled-hide-show-atpt ()
  "Alternatively hides or shows GREATER-ANGLED at point. "
  (interactive)
  (ar-th-hide-show 'greater-angled))

(defalias 'ar-highlight-greater-angled-atpt-mode 'ar-greater-angled-highlight-atpt-mode)

(defun ar-greater-angled-highlight-atpt-mode (&optional no-delimiters)
  "Toggles greater-angled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-kill-greater-angled-atpt 'ar-greater-angled-kill-atpt)
(defun ar-greater-angled-kill-atpt (&optional no-delimiters)
  "Kills GREATER-ANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-greater-angled-atpt 'ar-greater-angled-kill-backward-atpt)
(defun ar-greater-angled-kill-backward-atpt (&optional no-delimiters)
  "Kills GREATER-ANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-greater-angled-atpt 'ar-greater-angled-left-right-singlequote-atpt)
(defun ar-greater-angled-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-parentize-greater-angled-atpt 'ar-greater-angled-parentize-atpt)
(defun ar-greater-angled-parentize-atpt (&optional no-delimiters)
  "Parentizes GREATER-ANGLED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-separate-greater-angled-atpt 'ar-greater-angled-separate-atpt)
(defun ar-greater-angled-separate-atpt (&optional no-delimiters)
  "Separates GREATER-ANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-singlequote-greater-angled-atpt 'ar-greater-angled-singlequote-atpt)
(defun ar-greater-angled-singlequote-atpt (&optional no-delimiters)
  "Singlequotes GREATER-ANGLED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-greater-angled-atpt 'ar-greater-angled-triplequote-dq-atpt)
(defun ar-greater-angled-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around greater-angled. "
  (interactive "*p")
  (ar-th-triplequote-dq 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-greater-angled-atpt 'ar-greater-angled-triplequote-sq-atpt)
(defun ar-greater-angled-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around greater-angled. "
  (interactive "*p")
  (ar-th-triplequote-sq 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-trim-greater-angled-atpt 'ar-greater-angled-trim-atpt)
(defun ar-greater-angled-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled t t))
    (ar-th-trim 'greater-angled t t)))

(defalias 'ar-trim-left-greater-angled-atpt 'ar-greater-angled-left-trim-atpt)
(defun ar-greater-angled-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled t nil))
    (ar-th-trim 'greater-angled t nil)))

(defalias 'ar-trim-right-greater-angled-atpt 'ar-greater-angled-right-trim-atpt)
(defun ar-greater-angled-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled nil t))
    (ar-th-trim 'greater-angled nil t)))

(defalias 'ar-greater-angled-underscore-atpt 'ar-underscore-greater-angled-atpt)
(defun ar-underscore-greater-angled-atpt (&optional no-delimiters)
  "Put underscore char around GREATER-ANGLED. "
  (interactive "*p")
  (ar-th-underscore 'greater-angled no-delimiters (interactive-p)))

(defalias 'ar-greater-angled-whitespace-atpt 'ar-whitespace-greater-angled-atpt)
(defun ar-whitespace-greater-angled-atpt (&optional no-delimiters)
  "Put whitespace char around GREATER-ANGLED. "
  (interactive "*p")
  (ar-th-whitespace 'greater-angled nil t))

(defalias 'ar-forward-greater-angled-atpt 'ar-greater-angled-forward-atpt)
(defun ar-greater-angled-forward-atpt (&optional arg)
  "Moves forward over GREATER-ANGLED at point if any, does nothing otherwise.
Returns end position of GREATER-ANGLED "
  (interactive "p")
  (ar-th-forward 'greater-angled arg (interactive-p)))

(defalias 'ar-backward-greater-angled-atpt 'ar-greater-angled-backward-atpt)
(defun ar-greater-angled-backward-atpt (&optional arg)
  "Moves backward over GREATER-ANGLED before point if any, does nothing otherwise.
Returns beginning position of GREATER-ANGLED "
  (interactive "p")
  (ar-th-backward 'greater-angled arg (interactive-p)))

(defalias 'ar-transpose-greater-angled-atpt 'ar-greater-angled-transpose-atpt)
(defun ar-greater-angled-transpose-atpt (&optional arg)
  "Transposes GREATER-ANGLED with GREATER-ANGLED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'greater-angled arg (interactive-p)))

(defalias 'ar-sort-greater-angled-atpt 'ar-greater-angled-sort-atpt)
(defun ar-greater-angled-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greater-angleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'greater-angled reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-greater-angled-atpt 'ar-greater-angled-check-atpt)
(defun ar-greater-angled-check-atpt ()
  "Return t if a GREATER-ANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greater-angled-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-greater-angled-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-left-right-singlequoted-atpt (&optional arg no-delimiters)
  "Returns left-right-singlequoted at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'left-right-singlequoted arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-left-right-singlequoted-atpt 'ar-left-right-singlequoted-bounds-atpt)
(defun ar-left-right-singlequoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of left-right-singlequoted if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-left-right-singlequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-left-right-singlequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-left-right-singlequoted-atpt 'ar-left-right-singlequoted-beginning-atpt)
(defun ar-left-right-singlequoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-end-of-left-right-singlequoted-atpt 'ar-left-right-singlequoted-end-atpt)
(defun ar-left-right-singlequoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-in-left-right-singlequoted-p-atpt 'ar-left-right-singlequoted-in-p-atpt)
(defun ar-left-right-singlequoted-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LEFT-RIGHT-SINGLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-length-of-left-right-singlequoted-atpt 'ar-left-right-singlequoted-length-atpt)
(defun ar-left-right-singlequoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-copy-left-right-singlequoted-atpt 'ar-left-right-singlequoted-copy-atpt)
(defun ar-left-right-singlequoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-delete-left-right-singlequoted-in-region 'ar-left-right-singlequoted-delete-in-region)
(defun ar-left-right-singlequoted-delete-in-region (beg end)
  "Deletes LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'left-right-singlequoted beg end (interactive-p)))

(defun ar-blok-left-right-singlequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around left-right-singlequoted.
  Returns blok or nil if no LEFT-RIGHT-SINGLEQUOTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-escape-left-right-singlequoted-atpt 'ar-left-right-singlequoted-escape-atpt)
(defun ar-left-right-singlequoted-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'left-right-singlequoted no-delimiters))

(defalias 'ar-doublequote-left-right-singlequoted-atpt 'ar-left-right-singlequoted-doublequote-atpt)
(defun ar-left-right-singlequoted-doublequote-atpt (&optional no-delimiters)
  "Doublequotes LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-slash-left-right-singlequoted-atpt 'ar-left-right-singlequoted-slash-atpt)
(defun ar-left-right-singlequoted-slash-atpt (&optional no-delimiters)
  "Doublequotes LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-slash 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-left-right-singlequoted-atpt 'ar-left-right-singlequoted-double-backslash-atpt)
(defun ar-left-right-singlequoted-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-left-right-singlequoted-atpt 'ar-left-right-singlequoted-doubleslash-atpt)
(defun ar-left-right-singlequoted-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-left-right-singlequoted-atpt 'ar-left-right-singlequoted-doubleslash-paren-atpt)
(defun ar-left-right-singlequoted-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-slashparen-left-right-singlequoted-atpt 'ar-left-right-singlequoted-slashparen-atpt)
(defun ar-left-right-singlequoted-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-dollar-left-right-singlequoted-atpt 'ar-left-right-singlequoted-dollar-atpt)
(defun ar-left-right-singlequoted-dollar-atpt (&optional no-delimiters)
  "Doublequotes LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-equalize-left-right-singlequoted-atpt 'ar-left-right-singlequoted-equalize-atpt)
(defun ar-left-right-singlequoted-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-left-right-singlequoted-atpt 'ar-left-right-singlequoted-greater-angle-atpt)
(defun ar-left-right-singlequoted-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for LEFT-RIGHT-SINGLEQUOTED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-left-right-singlequoted-atpt 'ar-left-right-singlequoted-lesser-angle-atpt)
(defun ar-left-right-singlequoted-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for LEFT-RIGHT-SINGLEQUOTED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-backslash-left-right-singlequoted-atpt 'ar-left-right-singlequoted-backslash-atpt)
(defun ar-left-right-singlequoted-backslash-atpt (&optional no-delimiters)
  "Backslash LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-brace-left-right-singlequoted-atpt 'ar-left-right-singlequoted-brace-atpt)
(defun ar-left-right-singlequoted-brace-atpt (&optional no-delimiters)
  "Braces LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-brace 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-bracket-left-right-singlequoted-atpt 'ar-left-right-singlequoted-bracket-atpt)
(defun ar-left-right-singlequoted-bracket-atpt (&optional no-delimiters)
  "Brackets LEFT-RIGHT-SINGLEQUOTED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-comment-left-right-singlequoted-atpt (&optional no-delimiters)
  "Comments LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-commatize-left-right-singlequoted-atpt (&optional no-delimiters)
  "Put a comma after LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-quote-left-right-singlequoted-atpt (&optional no-delimiters)
  "Put a singlequote before LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-hyphen-left-right-singlequoted-atpt 'ar-left-right-singlequoted-hyphen-atpt)
(defun ar-left-right-singlequoted-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-mark-left-right-singlequoted-atpt 'ar-left-right-singlequoted-mark-atpt)
(defun ar-left-right-singlequoted-mark-atpt ()
  "Marks LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive)
  (ar-th-mark 'left-right-singlequoted))

(defalias 'ar-hide-left-right-singlequoted-atpt 'ar-left-right-singlequoted-hide-atpt)
(defun ar-left-right-singlequoted-hide-atpt ()
  "Hides LEFT-RIGHT-SINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide 'left-right-singlequoted))

(defalias 'ar-show-left-right-singlequoted-atpt 'ar-left-right-singlequoted-show-atpt)
(defun ar-left-right-singlequoted-show-atpt ()
  "Shows hidden LEFT-RIGHT-SINGLEQUOTED at point. "
  (interactive)
  (ar-th-show 'left-right-singlequoted))

(defalias 'ar-hide-show-left-right-singlequoted-atpt 'ar-left-right-singlequoted-hide-show-atpt)
(defun ar-left-right-singlequoted-hide-show-atpt ()
  "Alternatively hides or shows LEFT-RIGHT-SINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide-show 'left-right-singlequoted))

(defalias 'ar-highlight-left-right-singlequoted-atpt-mode 'ar-left-right-singlequoted-highlight-atpt-mode)

(defun ar-left-right-singlequoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles left-right-singlequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-left-right-singlequoted-atpt 'ar-left-right-singlequoted-kill-atpt)
(defun ar-left-right-singlequoted-kill-atpt (&optional no-delimiters)
  "Kills LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-left-right-singlequoted-atpt 'ar-left-right-singlequoted-kill-backward-atpt)
(defun ar-left-right-singlequoted-kill-backward-atpt (&optional no-delimiters)
  "Kills LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-left-right-singlequoted-atpt 'ar-left-right-singlequoted-left-right-singlequote-atpt)
(defun ar-left-right-singlequoted-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-parentize-left-right-singlequoted-atpt 'ar-left-right-singlequoted-parentize-atpt)
(defun ar-left-right-singlequoted-parentize-atpt (&optional no-delimiters)
  "Parentizes LEFT-RIGHT-SINGLEQUOTED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-separate-left-right-singlequoted-atpt 'ar-left-right-singlequoted-separate-atpt)
(defun ar-left-right-singlequoted-separate-atpt (&optional no-delimiters)
  "Separates LEFT-RIGHT-SINGLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-singlequote-left-right-singlequoted-atpt 'ar-left-right-singlequoted-singlequote-atpt)
(defun ar-left-right-singlequoted-singlequote-atpt (&optional no-delimiters)
  "Singlequotes LEFT-RIGHT-SINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-left-right-singlequoted-atpt 'ar-left-right-singlequoted-triplequote-dq-atpt)
(defun ar-left-right-singlequoted-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around left-right-singlequoted. "
  (interactive "*p")
  (ar-th-triplequote-dq 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-left-right-singlequoted-atpt 'ar-left-right-singlequoted-triplequote-sq-atpt)
(defun ar-left-right-singlequoted-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around left-right-singlequoted. "
  (interactive "*p")
  (ar-th-triplequote-sq 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-trim-left-right-singlequoted-atpt 'ar-left-right-singlequoted-trim-atpt)
(defun ar-left-right-singlequoted-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'left-right-singlequoted t t))
    (ar-th-trim 'left-right-singlequoted t t)))

(defalias 'ar-trim-left-left-right-singlequoted-atpt 'ar-left-right-singlequoted-left-trim-atpt)
(defun ar-left-right-singlequoted-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'left-right-singlequoted t nil))
    (ar-th-trim 'left-right-singlequoted t nil)))

(defalias 'ar-trim-right-left-right-singlequoted-atpt 'ar-left-right-singlequoted-right-trim-atpt)
(defun ar-left-right-singlequoted-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'left-right-singlequoted nil t))
    (ar-th-trim 'left-right-singlequoted nil t)))

(defalias 'ar-left-right-singlequoted-underscore-atpt 'ar-underscore-left-right-singlequoted-atpt)
(defun ar-underscore-left-right-singlequoted-atpt (&optional no-delimiters)
  "Put underscore char around LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*p")
  (ar-th-underscore 'left-right-singlequoted no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequoted-whitespace-atpt 'ar-whitespace-left-right-singlequoted-atpt)
(defun ar-whitespace-left-right-singlequoted-atpt (&optional no-delimiters)
  "Put whitespace char around LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*p")
  (ar-th-whitespace 'left-right-singlequoted nil t))

(defalias 'ar-forward-left-right-singlequoted-atpt 'ar-left-right-singlequoted-forward-atpt)
(defun ar-left-right-singlequoted-forward-atpt (&optional arg)
  "Moves forward over LEFT-RIGHT-SINGLEQUOTED at point if any, does nothing otherwise.
Returns end position of LEFT-RIGHT-SINGLEQUOTED "
  (interactive "p")
  (ar-th-forward 'left-right-singlequoted arg (interactive-p)))

(defalias 'ar-backward-left-right-singlequoted-atpt 'ar-left-right-singlequoted-backward-atpt)
(defun ar-left-right-singlequoted-backward-atpt (&optional arg)
  "Moves backward over LEFT-RIGHT-SINGLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of LEFT-RIGHT-SINGLEQUOTED "
  (interactive "p")
  (ar-th-backward 'left-right-singlequoted arg (interactive-p)))

(defalias 'ar-transpose-left-right-singlequoted-atpt 'ar-left-right-singlequoted-transpose-atpt)
(defun ar-left-right-singlequoted-transpose-atpt (&optional arg)
  "Transposes LEFT-RIGHT-SINGLEQUOTED with LEFT-RIGHT-SINGLEQUOTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'left-right-singlequoted arg (interactive-p)))

(defalias 'ar-sort-left-right-singlequoted-atpt 'ar-left-right-singlequoted-sort-atpt)
(defun ar-left-right-singlequoted-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts left-right-singlequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'left-right-singlequoted reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-left-right-singlequoted-atpt 'ar-left-right-singlequoted-check-atpt)
(defun ar-left-right-singlequoted-check-atpt ()
  "Return t if a LEFT-RIGHT-SINGLEQUOTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-left-right-singlequoted-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-left-right-singlequoted-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-parentized-atpt (&optional arg no-delimiters)
  "Returns parentized at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'parentized arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-parentized-atpt 'ar-parentized-bounds-atpt)
(defun ar-parentized-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of parentized if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PARENTIZED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'parentized no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-parentized-atpt 'ar-parentized-beginning-atpt)
(defun ar-parentized-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'parentized no-delimiters (interactive-p)))

(defalias 'ar-end-of-parentized-atpt 'ar-parentized-end-atpt)
(defun ar-parentized-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'parentized no-delimiters (interactive-p)))

(defalias 'ar-in-parentized-p-atpt 'ar-parentized-in-p-atpt)
(defun ar-parentized-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PARENTIZED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters (interactive-p)))

(defalias 'ar-length-of-parentized-atpt 'ar-parentized-length-atpt)
(defun ar-parentized-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'parentized no-delimiters (interactive-p)))

(defalias 'ar-copy-parentized-atpt 'ar-parentized-copy-atpt)
(defun ar-parentized-copy-atpt (&optional no-delimiters)
  "Returns a copy of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'parentized no-delimiters (interactive-p)))

(defalias 'ar-delete-parentized-in-region 'ar-parentized-delete-in-region)
(defun ar-parentized-delete-in-region (beg end)
  "Deletes PARENTIZED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'parentized beg end (interactive-p)))

(defun ar-blok-parentized-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around parentized.
  Returns blok or nil if no PARENTIZED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'parentized no-delimiters (interactive-p)))

(defalias 'ar-escape-parentized-atpt 'ar-parentized-escape-atpt)
(defun ar-parentized-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted PARENTIZED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'parentized no-delimiters))

(defalias 'ar-doublequote-parentized-atpt 'ar-parentized-doublequote-atpt)
(defun ar-parentized-doublequote-atpt (&optional no-delimiters)
  "Doublequotes PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'parentized no-delimiters (interactive-p)))

(defalias 'ar-slash-parentized-atpt 'ar-parentized-slash-atpt)
(defun ar-parentized-slash-atpt (&optional no-delimiters)
  "Doublequotes PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-slash 'parentized no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-parentized-atpt 'ar-parentized-double-backslash-atpt)
(defun ar-parentized-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'parentized no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-parentized-atpt 'ar-parentized-doubleslash-atpt)
(defun ar-parentized-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'parentized no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-parentized-atpt 'ar-parentized-doubleslash-paren-atpt)
(defun ar-parentized-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'parentized no-delimiters (interactive-p)))

(defalias 'ar-slashparen-parentized-atpt 'ar-parentized-slashparen-atpt)
(defun ar-parentized-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'parentized no-delimiters (interactive-p)))

(defalias 'ar-dollar-parentized-atpt 'ar-parentized-dollar-atpt)
(defun ar-parentized-dollar-atpt (&optional no-delimiters)
  "Doublequotes PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'parentized no-delimiters (interactive-p)))

(defalias 'ar-equalize-parentized-atpt 'ar-parentized-equalize-atpt)
(defun ar-parentized-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'parentized no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-parentized-atpt 'ar-parentized-greater-angle-atpt)
(defun ar-parentized-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for PARENTIZED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'parentized no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-parentized-atpt 'ar-parentized-lesser-angle-atpt)
(defun ar-parentized-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for PARENTIZED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'parentized no-delimiters (interactive-p)))

(defalias 'ar-backslash-parentized-atpt 'ar-parentized-backslash-atpt)
(defun ar-parentized-backslash-atpt (&optional no-delimiters)
  "Backslash PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'parentized no-delimiters (interactive-p)))

(defalias 'ar-brace-parentized-atpt 'ar-parentized-brace-atpt)
(defun ar-parentized-brace-atpt (&optional no-delimiters)
  "Braces PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-brace 'parentized no-delimiters (interactive-p)))

(defalias 'ar-bracket-parentized-atpt 'ar-parentized-bracket-atpt)
(defun ar-parentized-bracket-atpt (&optional no-delimiters)
  "Brackets PARENTIZED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'parentized no-delimiters (interactive-p)))

(defun ar-comment-parentized-atpt (&optional no-delimiters)
  "Comments PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-comment 'parentized no-delimiters (interactive-p)))

(defun ar-commatize-parentized-atpt (&optional no-delimiters)
  "Put a comma after PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'parentized no-delimiters (interactive-p)))

(defun ar-quote-parentized-atpt (&optional no-delimiters)
  "Put a singlequote before PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-quote 'parentized no-delimiters (interactive-p)))

(defalias 'ar-hyphen-parentized-atpt 'ar-parentized-hyphen-atpt)
(defun ar-parentized-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'parentized no-delimiters (interactive-p)))

(defalias 'ar-mark-parentized-atpt 'ar-parentized-mark-atpt)
(defun ar-parentized-mark-atpt ()
  "Marks PARENTIZED at point if any. "
  (interactive)
  (ar-th-mark 'parentized))

(defalias 'ar-hide-parentized-atpt 'ar-parentized-hide-atpt)
(defun ar-parentized-hide-atpt ()
  "Hides PARENTIZED at point. "
  (interactive)
  (ar-th-hide 'parentized))

(defalias 'ar-show-parentized-atpt 'ar-parentized-show-atpt)
(defun ar-parentized-show-atpt ()
  "Shows hidden PARENTIZED at point. "
  (interactive)
  (ar-th-show 'parentized))

(defalias 'ar-hide-show-parentized-atpt 'ar-parentized-hide-show-atpt)
(defun ar-parentized-hide-show-atpt ()
  "Alternatively hides or shows PARENTIZED at point. "
  (interactive)
  (ar-th-hide-show 'parentized))

(defalias 'ar-highlight-parentized-atpt-mode 'ar-parentized-highlight-atpt-mode)

(defun ar-parentized-highlight-atpt-mode (&optional no-delimiters)
  "Toggles parentized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'parentized no-delimiters (interactive-p)))

(defalias 'ar-kill-parentized-atpt 'ar-parentized-kill-atpt)
(defun ar-parentized-kill-atpt (&optional no-delimiters)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill 'parentized no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-parentized-atpt 'ar-parentized-kill-backward-atpt)
(defun ar-parentized-kill-backward-atpt (&optional no-delimiters)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'parentized no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-parentized-atpt 'ar-parentized-left-right-singlequote-atpt)
(defun ar-parentized-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'parentized no-delimiters (interactive-p)))

(defalias 'ar-parentize-parentized-atpt 'ar-parentized-parentize-atpt)
(defun ar-parentized-parentize-atpt (&optional no-delimiters)
  "Parentizes PARENTIZED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'parentized no-delimiters (interactive-p)))

(defalias 'ar-separate-parentized-atpt 'ar-parentized-separate-atpt)
(defun ar-parentized-separate-atpt (&optional no-delimiters)
  "Separates PARENTIZED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'parentized no-delimiters (interactive-p)))

(defalias 'ar-singlequote-parentized-atpt 'ar-parentized-singlequote-atpt)
(defun ar-parentized-singlequote-atpt (&optional no-delimiters)
  "Singlequotes PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'parentized no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-parentized-atpt 'ar-parentized-triplequote-dq-atpt)
(defun ar-parentized-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around parentized. "
  (interactive "*p")
  (ar-th-triplequote-dq 'parentized no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-parentized-atpt 'ar-parentized-triplequote-sq-atpt)
(defun ar-parentized-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around parentized. "
  (interactive "*p")
  (ar-th-triplequote-sq 'parentized no-delimiters (interactive-p)))

(defalias 'ar-trim-parentized-atpt 'ar-parentized-trim-atpt)
(defun ar-parentized-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'parentized t t))
    (ar-th-trim 'parentized t t)))

(defalias 'ar-trim-left-parentized-atpt 'ar-parentized-left-trim-atpt)
(defun ar-parentized-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'parentized t nil))
    (ar-th-trim 'parentized t nil)))

(defalias 'ar-trim-right-parentized-atpt 'ar-parentized-right-trim-atpt)
(defun ar-parentized-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'parentized nil t))
    (ar-th-trim 'parentized nil t)))

(defalias 'ar-parentized-underscore-atpt 'ar-underscore-parentized-atpt)
(defun ar-underscore-parentized-atpt (&optional no-delimiters)
  "Put underscore char around PARENTIZED. "
  (interactive "*p")
  (ar-th-underscore 'parentized no-delimiters (interactive-p)))

(defalias 'ar-parentized-whitespace-atpt 'ar-whitespace-parentized-atpt)
(defun ar-whitespace-parentized-atpt (&optional no-delimiters)
  "Put whitespace char around PARENTIZED. "
  (interactive "*p")
  (ar-th-whitespace 'parentized nil t))

(defalias 'ar-forward-parentized-atpt 'ar-parentized-forward-atpt)
(defun ar-parentized-forward-atpt (&optional arg)
  "Moves forward over PARENTIZED at point if any, does nothing otherwise.
Returns end position of PARENTIZED "
  (interactive "p")
  (ar-th-forward 'parentized arg (interactive-p)))

(defalias 'ar-backward-parentized-atpt 'ar-parentized-backward-atpt)
(defun ar-parentized-backward-atpt (&optional arg)
  "Moves backward over PARENTIZED before point if any, does nothing otherwise.
Returns beginning position of PARENTIZED "
  (interactive "p")
  (ar-th-backward 'parentized arg (interactive-p)))

(defalias 'ar-transpose-parentized-atpt 'ar-parentized-transpose-atpt)
(defun ar-parentized-transpose-atpt (&optional arg)
  "Transposes PARENTIZED with PARENTIZED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'parentized arg (interactive-p)))

(defalias 'ar-sort-parentized-atpt 'ar-parentized-sort-atpt)
(defun ar-parentized-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts parentizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'parentized reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-parentized-atpt 'ar-parentized-check-atpt)
(defun ar-parentized-check-atpt ()
  "Return t if a PARENTIZED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-parentized-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-parentized-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-delimited-list end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-rest-list start

(defun ar-abbrev-atpt (&optional arg no-delimiters)
  "Returns abbrev at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'abbrev arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-abbrev-atpt 'ar-abbrev-bounds-atpt)
(defun ar-abbrev-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of abbrev if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'abbrev no-delimiters (interactive-p)))

(defun ar-abbrev-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position ABBREV at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'abbrev no-delimiters (interactive-p)))

(defun ar-abbrev-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of ABBREV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-abbrev-atpt 'ar-abbrev-beginning-atpt)
(defun ar-abbrev-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class ABBREV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-end-of-abbrev-atpt 'ar-abbrev-end-atpt)
(defun ar-abbrev-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class ABBREV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-in-abbrev-p-atpt 'ar-abbrev-in-p-atpt)
(defun ar-abbrev-in-p-atpt (&optional no-delimiters)
  "Returns bounds of ABBREV at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-length-of-abbrev-atpt 'ar-abbrev-length-atpt)
(defun ar-abbrev-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class ABBREV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-copy-abbrev-atpt 'ar-abbrev-copy-atpt)
(defun ar-abbrev-copy-atpt (&optional no-delimiters)
  "Returns a copy of ABBREV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-delete-abbrev-in-region 'ar-abbrev-delete-in-region)
(defun ar-abbrev-delete-in-region (beg end)
  "Deletes ABBREV at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'abbrev beg end (interactive-p)))

(defun ar-blok-abbrev-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around abbrev.
  Returns blok or nil if no ABBREV at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-escape-abbrev-atpt 'ar-abbrev-escape-atpt)
(defun ar-abbrev-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted ABBREV at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'abbrev no-delimiters))

(defalias 'ar-doublequote-abbrev-atpt 'ar-abbrev-doublequote-atpt)
(defun ar-abbrev-doublequote-atpt (&optional no-delimiters)
  "Doublequotes ABBREV at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-slash-abbrev-atpt 'ar-abbrev-slash-atpt)
(defun ar-abbrev-slash-atpt (&optional no-delimiters)
  "Doublequotes ABBREV at point if any. "
  (interactive "*p")
  (ar-th-slash 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-abbrev-atpt 'ar-abbrev-double-backslash-atpt)
(defun ar-abbrev-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-abbrev-atpt 'ar-abbrev-doubleslash-atpt)
(defun ar-abbrev-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-abbrev-atpt 'ar-abbrev-doubleslash-paren-atpt)
(defun ar-abbrev-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-slashparen-abbrev-atpt 'ar-abbrev-slashparen-atpt)
(defun ar-abbrev-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-dollar-abbrev-atpt 'ar-abbrev-dollar-atpt)
(defun ar-abbrev-dollar-atpt (&optional no-delimiters)
  "Doublequotes ABBREV at point if any. "
  (interactive "*p")
  (ar-th-dollar 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-equalize-abbrev-atpt 'ar-abbrev-equalize-atpt)
(defun ar-abbrev-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-equalize 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-abbrev-atpt 'ar-abbrev-greater-angle-atpt)
(defun ar-abbrev-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for ABBREV after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-abbrev-atpt 'ar-abbrev-lesser-angle-atpt)
(defun ar-abbrev-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for ABBREV after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-backslash-abbrev-atpt 'ar-abbrev-backslash-atpt)
(defun ar-abbrev-backslash-atpt (&optional no-delimiters)
  "Backslash ABBREV at point if any. "
  (interactive "*p")
  (ar-th-backslash 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-brace-abbrev-atpt 'ar-abbrev-brace-atpt)
(defun ar-abbrev-brace-atpt (&optional no-delimiters)
  "Braces ABBREV at point if any. "
  (interactive "*p")
  (ar-th-brace 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-bracket-abbrev-atpt 'ar-abbrev-bracket-atpt)
(defun ar-abbrev-bracket-atpt (&optional no-delimiters)
  "Brackets ABBREV after point if any. "
  (interactive "*p")
  (ar-th-bracket 'abbrev no-delimiters (interactive-p)))

(defun ar-comment-abbrev-atpt (&optional no-delimiters)
  "Comments ABBREV at point if any. "
  (interactive "*p")
  (ar-th-comment 'abbrev no-delimiters (interactive-p)))

(defun ar-commatize-abbrev-atpt (&optional no-delimiters)
  "Put a comma after ABBREV at point if any. "
  (interactive "*p")
  (ar-th-commatize 'abbrev no-delimiters (interactive-p)))

(defun ar-quote-abbrev-atpt (&optional no-delimiters)
  "Put a singlequote before ABBREV at point if any. "
  (interactive "*p")
  (ar-th-quote 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-hyphen-abbrev-atpt 'ar-abbrev-hyphen-atpt)
(defun ar-abbrev-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around ABBREV at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-mark-abbrev-atpt 'ar-abbrev-mark-atpt)
(defun ar-abbrev-mark-atpt ()
  "Marks ABBREV at point if any. "
  (interactive)
  (ar-th-mark 'abbrev))

(defalias 'ar-hide-abbrev-atpt 'ar-abbrev-hide-atpt)
(defun ar-abbrev-hide-atpt ()
  "Hides ABBREV at point. "
  (interactive)
  (ar-th-hide 'abbrev))

(defalias 'ar-show-abbrev-atpt 'ar-abbrev-show-atpt)
(defun ar-abbrev-show-atpt ()
  "Shows hidden ABBREV at point. "
  (interactive)
  (ar-th-show 'abbrev))

(defalias 'ar-hide-show-abbrev-atpt 'ar-abbrev-hide-show-atpt)
(defun ar-abbrev-hide-show-atpt ()
  "Alternatively hides or shows ABBREV at point. "
  (interactive)
  (ar-th-hide-show 'abbrev))

(defalias 'ar-highlight-abbrev-atpt-mode 'ar-abbrev-highlight-atpt-mode)

(defun ar-abbrev-highlight-atpt-mode (&optional no-delimiters)
  "Toggles abbrev-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-kill-abbrev-atpt 'ar-abbrev-kill-atpt)
(defun ar-abbrev-kill-atpt (&optional no-delimiters)
  "Kills ABBREV at point if any. "
  (interactive "*P")
  (ar-th-kill 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-abbrev-atpt 'ar-abbrev-kill-backward-atpt)
(defun ar-abbrev-kill-backward-atpt (&optional no-delimiters)
  "Kills ABBREV at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-abbrev-atpt 'ar-abbrev-left-right-singlequote-atpt)
(defun ar-abbrev-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-parentize-abbrev-atpt 'ar-abbrev-parentize-atpt)
(defun ar-abbrev-parentize-atpt (&optional no-delimiters)
  "Parentizes ABBREV at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-separate-abbrev-atpt 'ar-abbrev-separate-atpt)
(defun ar-abbrev-separate-atpt (&optional no-delimiters)
  "Separates ABBREV at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-singlequote-abbrev-atpt 'ar-abbrev-singlequote-atpt)
(defun ar-abbrev-singlequote-atpt (&optional no-delimiters)
  "Singlequotes ABBREV at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-abbrev-atpt 'ar-abbrev-triplequote-dq-atpt)
(defun ar-abbrev-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around abbrev. "
  (interactive "*p")
  (ar-th-triplequote-dq 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-abbrev-atpt 'ar-abbrev-triplequote-sq-atpt)
(defun ar-abbrev-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around abbrev. "
  (interactive "*p")
  (ar-th-triplequote-sq 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-trim-abbrev-atpt 'ar-abbrev-trim-atpt)
(defun ar-abbrev-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'abbrev t t))
    (ar-th-trim 'abbrev t t)))

(defalias 'ar-trim-left-abbrev-atpt 'ar-abbrev-left-trim-atpt)
(defun ar-abbrev-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'abbrev t nil))
    (ar-th-trim 'abbrev t nil)))

(defalias 'ar-trim-right-abbrev-atpt 'ar-abbrev-right-trim-atpt)
(defun ar-abbrev-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'abbrev nil t))
    (ar-th-trim 'abbrev nil t)))

(defalias 'ar-abbrev-underscore-atpt 'ar-underscore-abbrev-atpt)
(defun ar-underscore-abbrev-atpt (&optional no-delimiters)
  "Put underscore char around ABBREV. "
  (interactive "*p")
  (ar-th-underscore 'abbrev no-delimiters (interactive-p)))

(defalias 'ar-abbrev-whitespace-atpt 'ar-whitespace-abbrev-atpt)
(defun ar-whitespace-abbrev-atpt (&optional no-delimiters)
  "Put whitespace char around ABBREV. "
  (interactive "*p")
  (ar-th-whitespace 'abbrev nil t))

(defalias 'ar-forward-abbrev-atpt 'ar-abbrev-forward-atpt)
(defun ar-abbrev-forward-atpt (&optional arg)
  "Moves forward over ABBREV at point if any, does nothing otherwise.
Returns end position of ABBREV "
  (interactive "p")
  (ar-th-forward 'abbrev arg (interactive-p)))

(defalias 'ar-backward-abbrev-atpt 'ar-abbrev-backward-atpt)
(defun ar-abbrev-backward-atpt (&optional arg)
  "Moves backward over ABBREV before point if any, does nothing otherwise.
Returns beginning position of ABBREV "
  (interactive "p")
  (ar-th-backward 'abbrev arg (interactive-p)))

(defalias 'ar-transpose-abbrev-atpt 'ar-abbrev-transpose-atpt)
(defun ar-abbrev-transpose-atpt (&optional arg)
  "Transposes ABBREV with ABBREV before point if any. "
  (interactive "*p")
  (ar-th-transpose 'abbrev arg (interactive-p)))

(defalias 'ar-sort-abbrev-atpt 'ar-abbrev-sort-atpt)
(defun ar-abbrev-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts abbrevs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'abbrev reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-abbrev-atpt 'ar-abbrev-check-atpt)
(defun ar-abbrev-check-atpt ()
  "Return t if a ABBREV at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-abbrev-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-abbrev-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-acronym-atpt (&optional arg no-delimiters)
  "Returns acronym at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'acronym arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-acronym-atpt 'ar-acronym-bounds-atpt)
(defun ar-acronym-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of acronym if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'acronym no-delimiters (interactive-p)))

(defun ar-acronym-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position ACRONYM at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'acronym no-delimiters (interactive-p)))

(defun ar-acronym-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of ACRONYM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'acronym no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-acronym-atpt 'ar-acronym-beginning-atpt)
(defun ar-acronym-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class ACRONYM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'acronym no-delimiters (interactive-p)))

(defalias 'ar-end-of-acronym-atpt 'ar-acronym-end-atpt)
(defun ar-acronym-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class ACRONYM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'acronym no-delimiters (interactive-p)))

(defalias 'ar-in-acronym-p-atpt 'ar-acronym-in-p-atpt)
(defun ar-acronym-in-p-atpt (&optional no-delimiters)
  "Returns bounds of ACRONYM at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'acronym no-delimiters (interactive-p)))

(defalias 'ar-length-of-acronym-atpt 'ar-acronym-length-atpt)
(defun ar-acronym-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class ACRONYM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'acronym no-delimiters (interactive-p)))

(defalias 'ar-copy-acronym-atpt 'ar-acronym-copy-atpt)
(defun ar-acronym-copy-atpt (&optional no-delimiters)
  "Returns a copy of ACRONYM at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'acronym no-delimiters (interactive-p)))

(defalias 'ar-delete-acronym-in-region 'ar-acronym-delete-in-region)
(defun ar-acronym-delete-in-region (beg end)
  "Deletes ACRONYM at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'acronym beg end (interactive-p)))

(defun ar-blok-acronym-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around acronym.
  Returns blok or nil if no ACRONYM at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'acronym no-delimiters (interactive-p)))

(defalias 'ar-escape-acronym-atpt 'ar-acronym-escape-atpt)
(defun ar-acronym-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted ACRONYM at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'acronym no-delimiters))

(defalias 'ar-doublequote-acronym-atpt 'ar-acronym-doublequote-atpt)
(defun ar-acronym-doublequote-atpt (&optional no-delimiters)
  "Doublequotes ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'acronym no-delimiters (interactive-p)))

(defalias 'ar-slash-acronym-atpt 'ar-acronym-slash-atpt)
(defun ar-acronym-slash-atpt (&optional no-delimiters)
  "Doublequotes ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-slash 'acronym no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-acronym-atpt 'ar-acronym-double-backslash-atpt)
(defun ar-acronym-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'acronym no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-acronym-atpt 'ar-acronym-doubleslash-atpt)
(defun ar-acronym-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'acronym no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-acronym-atpt 'ar-acronym-doubleslash-paren-atpt)
(defun ar-acronym-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'acronym no-delimiters (interactive-p)))

(defalias 'ar-slashparen-acronym-atpt 'ar-acronym-slashparen-atpt)
(defun ar-acronym-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'acronym no-delimiters (interactive-p)))

(defalias 'ar-dollar-acronym-atpt 'ar-acronym-dollar-atpt)
(defun ar-acronym-dollar-atpt (&optional no-delimiters)
  "Doublequotes ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-dollar 'acronym no-delimiters (interactive-p)))

(defalias 'ar-equalize-acronym-atpt 'ar-acronym-equalize-atpt)
(defun ar-acronym-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-equalize 'acronym no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-acronym-atpt 'ar-acronym-greater-angle-atpt)
(defun ar-acronym-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for ACRONYM after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'acronym no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-acronym-atpt 'ar-acronym-lesser-angle-atpt)
(defun ar-acronym-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for ACRONYM after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'acronym no-delimiters (interactive-p)))

(defalias 'ar-backslash-acronym-atpt 'ar-acronym-backslash-atpt)
(defun ar-acronym-backslash-atpt (&optional no-delimiters)
  "Backslash ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-backslash 'acronym no-delimiters (interactive-p)))

(defalias 'ar-brace-acronym-atpt 'ar-acronym-brace-atpt)
(defun ar-acronym-brace-atpt (&optional no-delimiters)
  "Braces ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-brace 'acronym no-delimiters (interactive-p)))

(defalias 'ar-bracket-acronym-atpt 'ar-acronym-bracket-atpt)
(defun ar-acronym-bracket-atpt (&optional no-delimiters)
  "Brackets ACRONYM after point if any. "
  (interactive "*p")
  (ar-th-bracket 'acronym no-delimiters (interactive-p)))

(defun ar-comment-acronym-atpt (&optional no-delimiters)
  "Comments ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-comment 'acronym no-delimiters (interactive-p)))

(defun ar-commatize-acronym-atpt (&optional no-delimiters)
  "Put a comma after ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-commatize 'acronym no-delimiters (interactive-p)))

(defun ar-quote-acronym-atpt (&optional no-delimiters)
  "Put a singlequote before ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-quote 'acronym no-delimiters (interactive-p)))

(defalias 'ar-hyphen-acronym-atpt 'ar-acronym-hyphen-atpt)
(defun ar-acronym-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'acronym no-delimiters (interactive-p)))

(defalias 'ar-mark-acronym-atpt 'ar-acronym-mark-atpt)
(defun ar-acronym-mark-atpt ()
  "Marks ACRONYM at point if any. "
  (interactive)
  (ar-th-mark 'acronym))

(defalias 'ar-hide-acronym-atpt 'ar-acronym-hide-atpt)
(defun ar-acronym-hide-atpt ()
  "Hides ACRONYM at point. "
  (interactive)
  (ar-th-hide 'acronym))

(defalias 'ar-show-acronym-atpt 'ar-acronym-show-atpt)
(defun ar-acronym-show-atpt ()
  "Shows hidden ACRONYM at point. "
  (interactive)
  (ar-th-show 'acronym))

(defalias 'ar-hide-show-acronym-atpt 'ar-acronym-hide-show-atpt)
(defun ar-acronym-hide-show-atpt ()
  "Alternatively hides or shows ACRONYM at point. "
  (interactive)
  (ar-th-hide-show 'acronym))

(defalias 'ar-highlight-acronym-atpt-mode 'ar-acronym-highlight-atpt-mode)

(defun ar-acronym-highlight-atpt-mode (&optional no-delimiters)
  "Toggles acronym-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'acronym no-delimiters (interactive-p)))

(defalias 'ar-kill-acronym-atpt 'ar-acronym-kill-atpt)
(defun ar-acronym-kill-atpt (&optional no-delimiters)
  "Kills ACRONYM at point if any. "
  (interactive "*P")
  (ar-th-kill 'acronym no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-acronym-atpt 'ar-acronym-kill-backward-atpt)
(defun ar-acronym-kill-backward-atpt (&optional no-delimiters)
  "Kills ACRONYM at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'acronym no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-acronym-atpt 'ar-acronym-left-right-singlequote-atpt)
(defun ar-acronym-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'acronym no-delimiters (interactive-p)))

(defalias 'ar-parentize-acronym-atpt 'ar-acronym-parentize-atpt)
(defun ar-acronym-parentize-atpt (&optional no-delimiters)
  "Parentizes ACRONYM at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'acronym no-delimiters (interactive-p)))

(defalias 'ar-separate-acronym-atpt 'ar-acronym-separate-atpt)
(defun ar-acronym-separate-atpt (&optional no-delimiters)
  "Separates ACRONYM at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'acronym no-delimiters (interactive-p)))

(defalias 'ar-singlequote-acronym-atpt 'ar-acronym-singlequote-atpt)
(defun ar-acronym-singlequote-atpt (&optional no-delimiters)
  "Singlequotes ACRONYM at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'acronym no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-acronym-atpt 'ar-acronym-triplequote-dq-atpt)
(defun ar-acronym-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around acronym. "
  (interactive "*p")
  (ar-th-triplequote-dq 'acronym no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-acronym-atpt 'ar-acronym-triplequote-sq-atpt)
(defun ar-acronym-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around acronym. "
  (interactive "*p")
  (ar-th-triplequote-sq 'acronym no-delimiters (interactive-p)))

(defalias 'ar-trim-acronym-atpt 'ar-acronym-trim-atpt)
(defun ar-acronym-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'acronym t t))
    (ar-th-trim 'acronym t t)))

(defalias 'ar-trim-left-acronym-atpt 'ar-acronym-left-trim-atpt)
(defun ar-acronym-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'acronym t nil))
    (ar-th-trim 'acronym t nil)))

(defalias 'ar-trim-right-acronym-atpt 'ar-acronym-right-trim-atpt)
(defun ar-acronym-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'acronym nil t))
    (ar-th-trim 'acronym nil t)))

(defalias 'ar-acronym-underscore-atpt 'ar-underscore-acronym-atpt)
(defun ar-underscore-acronym-atpt (&optional no-delimiters)
  "Put underscore char around ACRONYM. "
  (interactive "*p")
  (ar-th-underscore 'acronym no-delimiters (interactive-p)))

(defalias 'ar-acronym-whitespace-atpt 'ar-whitespace-acronym-atpt)
(defun ar-whitespace-acronym-atpt (&optional no-delimiters)
  "Put whitespace char around ACRONYM. "
  (interactive "*p")
  (ar-th-whitespace 'acronym nil t))

(defalias 'ar-forward-acronym-atpt 'ar-acronym-forward-atpt)
(defun ar-acronym-forward-atpt (&optional arg)
  "Moves forward over ACRONYM at point if any, does nothing otherwise.
Returns end position of ACRONYM "
  (interactive "p")
  (ar-th-forward 'acronym arg (interactive-p)))

(defalias 'ar-backward-acronym-atpt 'ar-acronym-backward-atpt)
(defun ar-acronym-backward-atpt (&optional arg)
  "Moves backward over ACRONYM before point if any, does nothing otherwise.
Returns beginning position of ACRONYM "
  (interactive "p")
  (ar-th-backward 'acronym arg (interactive-p)))

(defalias 'ar-transpose-acronym-atpt 'ar-acronym-transpose-atpt)
(defun ar-acronym-transpose-atpt (&optional arg)
  "Transposes ACRONYM with ACRONYM before point if any. "
  (interactive "*p")
  (ar-th-transpose 'acronym arg (interactive-p)))

(defalias 'ar-sort-acronym-atpt 'ar-acronym-sort-atpt)
(defun ar-acronym-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts acronyms in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'acronym reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-acronym-atpt 'ar-acronym-check-atpt)
(defun ar-acronym-check-atpt ()
  "Return t if a ACRONYM at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-acronym-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-acronym-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-angled-no-nest-atpt (&optional arg no-delimiters)
  "Returns angled-no-nest at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'angled-no-nest arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-angled-no-nest-atpt 'ar-angled-no-nest-bounds-atpt)
(defun ar-angled-no-nest-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of angled-no-nest if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-angled-no-nest-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position ANGLED-NO-NEST at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-angled-no-nest-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of ANGLED-NO-NEST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-angled-no-nest-atpt 'ar-angled-no-nest-beginning-atpt)
(defun ar-angled-no-nest-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class ANGLED-NO-NEST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-end-of-angled-no-nest-atpt 'ar-angled-no-nest-end-atpt)
(defun ar-angled-no-nest-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class ANGLED-NO-NEST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-in-angled-no-nest-p-atpt 'ar-angled-no-nest-in-p-atpt)
(defun ar-angled-no-nest-in-p-atpt (&optional no-delimiters)
  "Returns bounds of ANGLED-NO-NEST at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-length-of-angled-no-nest-atpt 'ar-angled-no-nest-length-atpt)
(defun ar-angled-no-nest-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class ANGLED-NO-NEST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-copy-angled-no-nest-atpt 'ar-angled-no-nest-copy-atpt)
(defun ar-angled-no-nest-copy-atpt (&optional no-delimiters)
  "Returns a copy of ANGLED-NO-NEST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-delete-angled-no-nest-in-region 'ar-angled-no-nest-delete-in-region)
(defun ar-angled-no-nest-delete-in-region (beg end)
  "Deletes ANGLED-NO-NEST at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'angled-no-nest beg end (interactive-p)))

(defun ar-blok-angled-no-nest-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around angled-no-nest.
  Returns blok or nil if no ANGLED-NO-NEST at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-escape-angled-no-nest-atpt 'ar-angled-no-nest-escape-atpt)
(defun ar-angled-no-nest-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted ANGLED-NO-NEST at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'angled-no-nest no-delimiters))

(defalias 'ar-doublequote-angled-no-nest-atpt 'ar-angled-no-nest-doublequote-atpt)
(defun ar-angled-no-nest-doublequote-atpt (&optional no-delimiters)
  "Doublequotes ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-slash-angled-no-nest-atpt 'ar-angled-no-nest-slash-atpt)
(defun ar-angled-no-nest-slash-atpt (&optional no-delimiters)
  "Doublequotes ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-slash 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-angled-no-nest-atpt 'ar-angled-no-nest-double-backslash-atpt)
(defun ar-angled-no-nest-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-angled-no-nest-atpt 'ar-angled-no-nest-doubleslash-atpt)
(defun ar-angled-no-nest-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-angled-no-nest-atpt 'ar-angled-no-nest-doubleslash-paren-atpt)
(defun ar-angled-no-nest-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-slashparen-angled-no-nest-atpt 'ar-angled-no-nest-slashparen-atpt)
(defun ar-angled-no-nest-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-dollar-angled-no-nest-atpt 'ar-angled-no-nest-dollar-atpt)
(defun ar-angled-no-nest-dollar-atpt (&optional no-delimiters)
  "Doublequotes ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-dollar 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-equalize-angled-no-nest-atpt 'ar-angled-no-nest-equalize-atpt)
(defun ar-angled-no-nest-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-equalize 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-angled-no-nest-atpt 'ar-angled-no-nest-greater-angle-atpt)
(defun ar-angled-no-nest-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for ANGLED-NO-NEST after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-angled-no-nest-atpt 'ar-angled-no-nest-lesser-angle-atpt)
(defun ar-angled-no-nest-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for ANGLED-NO-NEST after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-backslash-angled-no-nest-atpt 'ar-angled-no-nest-backslash-atpt)
(defun ar-angled-no-nest-backslash-atpt (&optional no-delimiters)
  "Backslash ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-backslash 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-brace-angled-no-nest-atpt 'ar-angled-no-nest-brace-atpt)
(defun ar-angled-no-nest-brace-atpt (&optional no-delimiters)
  "Braces ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-brace 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-bracket-angled-no-nest-atpt 'ar-angled-no-nest-bracket-atpt)
(defun ar-angled-no-nest-bracket-atpt (&optional no-delimiters)
  "Brackets ANGLED-NO-NEST after point if any. "
  (interactive "*p")
  (ar-th-bracket 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-comment-angled-no-nest-atpt (&optional no-delimiters)
  "Comments ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-comment 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-commatize-angled-no-nest-atpt (&optional no-delimiters)
  "Put a comma after ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-commatize 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-quote-angled-no-nest-atpt (&optional no-delimiters)
  "Put a singlequote before ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-quote 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-hyphen-angled-no-nest-atpt 'ar-angled-no-nest-hyphen-atpt)
(defun ar-angled-no-nest-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-mark-angled-no-nest-atpt 'ar-angled-no-nest-mark-atpt)
(defun ar-angled-no-nest-mark-atpt ()
  "Marks ANGLED-NO-NEST at point if any. "
  (interactive)
  (ar-th-mark 'angled-no-nest))

(defalias 'ar-hide-angled-no-nest-atpt 'ar-angled-no-nest-hide-atpt)
(defun ar-angled-no-nest-hide-atpt ()
  "Hides ANGLED-NO-NEST at point. "
  (interactive)
  (ar-th-hide 'angled-no-nest))

(defalias 'ar-show-angled-no-nest-atpt 'ar-angled-no-nest-show-atpt)
(defun ar-angled-no-nest-show-atpt ()
  "Shows hidden ANGLED-NO-NEST at point. "
  (interactive)
  (ar-th-show 'angled-no-nest))

(defalias 'ar-hide-show-angled-no-nest-atpt 'ar-angled-no-nest-hide-show-atpt)
(defun ar-angled-no-nest-hide-show-atpt ()
  "Alternatively hides or shows ANGLED-NO-NEST at point. "
  (interactive)
  (ar-th-hide-show 'angled-no-nest))

(defalias 'ar-highlight-angled-no-nest-atpt-mode 'ar-angled-no-nest-highlight-atpt-mode)

(defun ar-angled-no-nest-highlight-atpt-mode (&optional no-delimiters)
  "Toggles angled-no-nest-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-kill-angled-no-nest-atpt 'ar-angled-no-nest-kill-atpt)
(defun ar-angled-no-nest-kill-atpt (&optional no-delimiters)
  "Kills ANGLED-NO-NEST at point if any. "
  (interactive "*P")
  (ar-th-kill 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-angled-no-nest-atpt 'ar-angled-no-nest-kill-backward-atpt)
(defun ar-angled-no-nest-kill-backward-atpt (&optional no-delimiters)
  "Kills ANGLED-NO-NEST at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-angled-no-nest-atpt 'ar-angled-no-nest-left-right-singlequote-atpt)
(defun ar-angled-no-nest-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-parentize-angled-no-nest-atpt 'ar-angled-no-nest-parentize-atpt)
(defun ar-angled-no-nest-parentize-atpt (&optional no-delimiters)
  "Parentizes ANGLED-NO-NEST at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-separate-angled-no-nest-atpt 'ar-angled-no-nest-separate-atpt)
(defun ar-angled-no-nest-separate-atpt (&optional no-delimiters)
  "Separates ANGLED-NO-NEST at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-singlequote-angled-no-nest-atpt 'ar-angled-no-nest-singlequote-atpt)
(defun ar-angled-no-nest-singlequote-atpt (&optional no-delimiters)
  "Singlequotes ANGLED-NO-NEST at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-angled-no-nest-atpt 'ar-angled-no-nest-triplequote-dq-atpt)
(defun ar-angled-no-nest-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around angled-no-nest. "
  (interactive "*p")
  (ar-th-triplequote-dq 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-angled-no-nest-atpt 'ar-angled-no-nest-triplequote-sq-atpt)
(defun ar-angled-no-nest-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around angled-no-nest. "
  (interactive "*p")
  (ar-th-triplequote-sq 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-trim-angled-no-nest-atpt 'ar-angled-no-nest-trim-atpt)
(defun ar-angled-no-nest-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'angled-no-nest t t))
    (ar-th-trim 'angled-no-nest t t)))

(defalias 'ar-trim-left-angled-no-nest-atpt 'ar-angled-no-nest-left-trim-atpt)
(defun ar-angled-no-nest-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'angled-no-nest t nil))
    (ar-th-trim 'angled-no-nest t nil)))

(defalias 'ar-trim-right-angled-no-nest-atpt 'ar-angled-no-nest-right-trim-atpt)
(defun ar-angled-no-nest-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'angled-no-nest nil t))
    (ar-th-trim 'angled-no-nest nil t)))

(defalias 'ar-angled-no-nest-underscore-atpt 'ar-underscore-angled-no-nest-atpt)
(defun ar-underscore-angled-no-nest-atpt (&optional no-delimiters)
  "Put underscore char around ANGLED-NO-NEST. "
  (interactive "*p")
  (ar-th-underscore 'angled-no-nest no-delimiters (interactive-p)))

(defalias 'ar-angled-no-nest-whitespace-atpt 'ar-whitespace-angled-no-nest-atpt)
(defun ar-whitespace-angled-no-nest-atpt (&optional no-delimiters)
  "Put whitespace char around ANGLED-NO-NEST. "
  (interactive "*p")
  (ar-th-whitespace 'angled-no-nest nil t))

(defalias 'ar-forward-angled-no-nest-atpt 'ar-angled-no-nest-forward-atpt)
(defun ar-angled-no-nest-forward-atpt (&optional arg)
  "Moves forward over ANGLED-NO-NEST at point if any, does nothing otherwise.
Returns end position of ANGLED-NO-NEST "
  (interactive "p")
  (ar-th-forward 'angled-no-nest arg (interactive-p)))

(defalias 'ar-backward-angled-no-nest-atpt 'ar-angled-no-nest-backward-atpt)
(defun ar-angled-no-nest-backward-atpt (&optional arg)
  "Moves backward over ANGLED-NO-NEST before point if any, does nothing otherwise.
Returns beginning position of ANGLED-NO-NEST "
  (interactive "p")
  (ar-th-backward 'angled-no-nest arg (interactive-p)))

(defalias 'ar-transpose-angled-no-nest-atpt 'ar-angled-no-nest-transpose-atpt)
(defun ar-angled-no-nest-transpose-atpt (&optional arg)
  "Transposes ANGLED-NO-NEST with ANGLED-NO-NEST before point if any. "
  (interactive "*p")
  (ar-th-transpose 'angled-no-nest arg (interactive-p)))

(defalias 'ar-sort-angled-no-nest-atpt 'ar-angled-no-nest-sort-atpt)
(defun ar-angled-no-nest-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts angled-no-nests in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'angled-no-nest reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-angled-no-nest-atpt 'ar-angled-no-nest-check-atpt)
(defun ar-angled-no-nest-check-atpt ()
  "Return t if a ANGLED-NO-NEST at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-angled-no-nest-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-angled-no-nest-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-greater-angled-nested-atpt (&optional arg no-delimiters)
  "Returns greater-angled-nested at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'greater-angled-nested arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-greater-angled-nested-atpt 'ar-greater-angled-nested-bounds-atpt)
(defun ar-greater-angled-nested-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of greater-angled-nested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-greater-angled-nested-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position GREATER-ANGLED-NESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-greater-angled-nested-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of GREATER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-greater-angled-nested-atpt 'ar-greater-angled-nested-beginning-atpt)
(defun ar-greater-angled-nested-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class GREATER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-end-of-greater-angled-nested-atpt 'ar-greater-angled-nested-end-atpt)
(defun ar-greater-angled-nested-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class GREATER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-in-greater-angled-nested-p-atpt 'ar-greater-angled-nested-in-p-atpt)
(defun ar-greater-angled-nested-in-p-atpt (&optional no-delimiters)
  "Returns bounds of GREATER-ANGLED-NESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-length-of-greater-angled-nested-atpt 'ar-greater-angled-nested-length-atpt)
(defun ar-greater-angled-nested-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class GREATER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-copy-greater-angled-nested-atpt 'ar-greater-angled-nested-copy-atpt)
(defun ar-greater-angled-nested-copy-atpt (&optional no-delimiters)
  "Returns a copy of GREATER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-delete-greater-angled-nested-in-region 'ar-greater-angled-nested-delete-in-region)
(defun ar-greater-angled-nested-delete-in-region (beg end)
  "Deletes GREATER-ANGLED-NESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greater-angled-nested beg end (interactive-p)))

(defun ar-blok-greater-angled-nested-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greater-angled-nested.
  Returns blok or nil if no GREATER-ANGLED-NESTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-escape-greater-angled-nested-atpt 'ar-greater-angled-nested-escape-atpt)
(defun ar-greater-angled-nested-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted GREATER-ANGLED-NESTED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'greater-angled-nested no-delimiters))

(defalias 'ar-doublequote-greater-angled-nested-atpt 'ar-greater-angled-nested-doublequote-atpt)
(defun ar-greater-angled-nested-doublequote-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-slash-greater-angled-nested-atpt 'ar-greater-angled-nested-slash-atpt)
(defun ar-greater-angled-nested-slash-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-slash 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-greater-angled-nested-atpt 'ar-greater-angled-nested-double-backslash-atpt)
(defun ar-greater-angled-nested-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-greater-angled-nested-atpt 'ar-greater-angled-nested-doubleslash-atpt)
(defun ar-greater-angled-nested-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-greater-angled-nested-atpt 'ar-greater-angled-nested-doubleslash-paren-atpt)
(defun ar-greater-angled-nested-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-slashparen-greater-angled-nested-atpt 'ar-greater-angled-nested-slashparen-atpt)
(defun ar-greater-angled-nested-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-dollar-greater-angled-nested-atpt 'ar-greater-angled-nested-dollar-atpt)
(defun ar-greater-angled-nested-dollar-atpt (&optional no-delimiters)
  "Doublequotes GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-equalize-greater-angled-nested-atpt 'ar-greater-angled-nested-equalize-atpt)
(defun ar-greater-angled-nested-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-greater-angled-nested-atpt 'ar-greater-angled-nested-greater-angle-atpt)
(defun ar-greater-angled-nested-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for GREATER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-greater-angled-nested-atpt 'ar-greater-angled-nested-lesser-angle-atpt)
(defun ar-greater-angled-nested-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for GREATER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-backslash-greater-angled-nested-atpt 'ar-greater-angled-nested-backslash-atpt)
(defun ar-greater-angled-nested-backslash-atpt (&optional no-delimiters)
  "Backslash GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-brace-greater-angled-nested-atpt 'ar-greater-angled-nested-brace-atpt)
(defun ar-greater-angled-nested-brace-atpt (&optional no-delimiters)
  "Braces GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-brace 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-bracket-greater-angled-nested-atpt 'ar-greater-angled-nested-bracket-atpt)
(defun ar-greater-angled-nested-bracket-atpt (&optional no-delimiters)
  "Brackets GREATER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-comment-greater-angled-nested-atpt (&optional no-delimiters)
  "Comments GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-commatize-greater-angled-nested-atpt (&optional no-delimiters)
  "Put a comma after GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-quote-greater-angled-nested-atpt (&optional no-delimiters)
  "Put a singlequote before GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-hyphen-greater-angled-nested-atpt 'ar-greater-angled-nested-hyphen-atpt)
(defun ar-greater-angled-nested-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-mark-greater-angled-nested-atpt 'ar-greater-angled-nested-mark-atpt)
(defun ar-greater-angled-nested-mark-atpt ()
  "Marks GREATER-ANGLED-NESTED at point if any. "
  (interactive)
  (ar-th-mark 'greater-angled-nested))

(defalias 'ar-hide-greater-angled-nested-atpt 'ar-greater-angled-nested-hide-atpt)
(defun ar-greater-angled-nested-hide-atpt ()
  "Hides GREATER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-hide 'greater-angled-nested))

(defalias 'ar-show-greater-angled-nested-atpt 'ar-greater-angled-nested-show-atpt)
(defun ar-greater-angled-nested-show-atpt ()
  "Shows hidden GREATER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-show 'greater-angled-nested))

(defalias 'ar-hide-show-greater-angled-nested-atpt 'ar-greater-angled-nested-hide-show-atpt)
(defun ar-greater-angled-nested-hide-show-atpt ()
  "Alternatively hides or shows GREATER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-hide-show 'greater-angled-nested))

(defalias 'ar-highlight-greater-angled-nested-atpt-mode 'ar-greater-angled-nested-highlight-atpt-mode)

(defun ar-greater-angled-nested-highlight-atpt-mode (&optional no-delimiters)
  "Toggles greater-angled-nested-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-kill-greater-angled-nested-atpt 'ar-greater-angled-nested-kill-atpt)
(defun ar-greater-angled-nested-kill-atpt (&optional no-delimiters)
  "Kills GREATER-ANGLED-NESTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-greater-angled-nested-atpt 'ar-greater-angled-nested-kill-backward-atpt)
(defun ar-greater-angled-nested-kill-backward-atpt (&optional no-delimiters)
  "Kills GREATER-ANGLED-NESTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-greater-angled-nested-atpt 'ar-greater-angled-nested-left-right-singlequote-atpt)
(defun ar-greater-angled-nested-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-parentize-greater-angled-nested-atpt 'ar-greater-angled-nested-parentize-atpt)
(defun ar-greater-angled-nested-parentize-atpt (&optional no-delimiters)
  "Parentizes GREATER-ANGLED-NESTED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-separate-greater-angled-nested-atpt 'ar-greater-angled-nested-separate-atpt)
(defun ar-greater-angled-nested-separate-atpt (&optional no-delimiters)
  "Separates GREATER-ANGLED-NESTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-singlequote-greater-angled-nested-atpt 'ar-greater-angled-nested-singlequote-atpt)
(defun ar-greater-angled-nested-singlequote-atpt (&optional no-delimiters)
  "Singlequotes GREATER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-greater-angled-nested-atpt 'ar-greater-angled-nested-triplequote-dq-atpt)
(defun ar-greater-angled-nested-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around greater-angled-nested. "
  (interactive "*p")
  (ar-th-triplequote-dq 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-greater-angled-nested-atpt 'ar-greater-angled-nested-triplequote-sq-atpt)
(defun ar-greater-angled-nested-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around greater-angled-nested. "
  (interactive "*p")
  (ar-th-triplequote-sq 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-trim-greater-angled-nested-atpt 'ar-greater-angled-nested-trim-atpt)
(defun ar-greater-angled-nested-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled-nested t t))
    (ar-th-trim 'greater-angled-nested t t)))

(defalias 'ar-trim-left-greater-angled-nested-atpt 'ar-greater-angled-nested-left-trim-atpt)
(defun ar-greater-angled-nested-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled-nested t nil))
    (ar-th-trim 'greater-angled-nested t nil)))

(defalias 'ar-trim-right-greater-angled-nested-atpt 'ar-greater-angled-nested-right-trim-atpt)
(defun ar-greater-angled-nested-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'greater-angled-nested nil t))
    (ar-th-trim 'greater-angled-nested nil t)))

(defalias 'ar-greater-angled-nested-underscore-atpt 'ar-underscore-greater-angled-nested-atpt)
(defun ar-underscore-greater-angled-nested-atpt (&optional no-delimiters)
  "Put underscore char around GREATER-ANGLED-NESTED. "
  (interactive "*p")
  (ar-th-underscore 'greater-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-greater-angled-nested-whitespace-atpt 'ar-whitespace-greater-angled-nested-atpt)
(defun ar-whitespace-greater-angled-nested-atpt (&optional no-delimiters)
  "Put whitespace char around GREATER-ANGLED-NESTED. "
  (interactive "*p")
  (ar-th-whitespace 'greater-angled-nested nil t))

(defalias 'ar-forward-greater-angled-nested-atpt 'ar-greater-angled-nested-forward-atpt)
(defun ar-greater-angled-nested-forward-atpt (&optional arg)
  "Moves forward over GREATER-ANGLED-NESTED at point if any, does nothing otherwise.
Returns end position of GREATER-ANGLED-NESTED "
  (interactive "p")
  (ar-th-forward 'greater-angled-nested arg (interactive-p)))

(defalias 'ar-backward-greater-angled-nested-atpt 'ar-greater-angled-nested-backward-atpt)
(defun ar-greater-angled-nested-backward-atpt (&optional arg)
  "Moves backward over GREATER-ANGLED-NESTED before point if any, does nothing otherwise.
Returns beginning position of GREATER-ANGLED-NESTED "
  (interactive "p")
  (ar-th-backward 'greater-angled-nested arg (interactive-p)))

(defalias 'ar-transpose-greater-angled-nested-atpt 'ar-greater-angled-nested-transpose-atpt)
(defun ar-greater-angled-nested-transpose-atpt (&optional arg)
  "Transposes GREATER-ANGLED-NESTED with GREATER-ANGLED-NESTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'greater-angled-nested arg (interactive-p)))

(defalias 'ar-sort-greater-angled-nested-atpt 'ar-greater-angled-nested-sort-atpt)
(defun ar-greater-angled-nested-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greater-angled-nesteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'greater-angled-nested reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-greater-angled-nested-atpt 'ar-greater-angled-nested-check-atpt)
(defun ar-greater-angled-nested-check-atpt ()
  "Return t if a GREATER-ANGLED-NESTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greater-angled-nested-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-greater-angled-nested-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-lesser-angled-nested-atpt (&optional arg no-delimiters)
  "Returns lesser-angled-nested at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'lesser-angled-nested arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-lesser-angled-nested-atpt 'ar-lesser-angled-nested-bounds-atpt)
(defun ar-lesser-angled-nested-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of lesser-angled-nested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-lesser-angled-nested-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LESSER-ANGLED-NESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-lesser-angled-nested-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LESSER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-lesser-angled-nested-atpt 'ar-lesser-angled-nested-beginning-atpt)
(defun ar-lesser-angled-nested-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LESSER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-end-of-lesser-angled-nested-atpt 'ar-lesser-angled-nested-end-atpt)
(defun ar-lesser-angled-nested-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LESSER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-in-lesser-angled-nested-p-atpt 'ar-lesser-angled-nested-in-p-atpt)
(defun ar-lesser-angled-nested-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LESSER-ANGLED-NESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-length-of-lesser-angled-nested-atpt 'ar-lesser-angled-nested-length-atpt)
(defun ar-lesser-angled-nested-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LESSER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-copy-lesser-angled-nested-atpt 'ar-lesser-angled-nested-copy-atpt)
(defun ar-lesser-angled-nested-copy-atpt (&optional no-delimiters)
  "Returns a copy of LESSER-ANGLED-NESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-delete-lesser-angled-nested-in-region 'ar-lesser-angled-nested-delete-in-region)
(defun ar-lesser-angled-nested-delete-in-region (beg end)
  "Deletes LESSER-ANGLED-NESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesser-angled-nested beg end (interactive-p)))

(defun ar-blok-lesser-angled-nested-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesser-angled-nested.
  Returns blok or nil if no LESSER-ANGLED-NESTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-escape-lesser-angled-nested-atpt 'ar-lesser-angled-nested-escape-atpt)
(defun ar-lesser-angled-nested-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted LESSER-ANGLED-NESTED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'lesser-angled-nested no-delimiters))

(defalias 'ar-doublequote-lesser-angled-nested-atpt 'ar-lesser-angled-nested-doublequote-atpt)
(defun ar-lesser-angled-nested-doublequote-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-slash-lesser-angled-nested-atpt 'ar-lesser-angled-nested-slash-atpt)
(defun ar-lesser-angled-nested-slash-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-slash 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-lesser-angled-nested-atpt 'ar-lesser-angled-nested-double-backslash-atpt)
(defun ar-lesser-angled-nested-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-lesser-angled-nested-atpt 'ar-lesser-angled-nested-doubleslash-atpt)
(defun ar-lesser-angled-nested-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-lesser-angled-nested-atpt 'ar-lesser-angled-nested-doubleslash-paren-atpt)
(defun ar-lesser-angled-nested-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-slashparen-lesser-angled-nested-atpt 'ar-lesser-angled-nested-slashparen-atpt)
(defun ar-lesser-angled-nested-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-dollar-lesser-angled-nested-atpt 'ar-lesser-angled-nested-dollar-atpt)
(defun ar-lesser-angled-nested-dollar-atpt (&optional no-delimiters)
  "Doublequotes LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-equalize-lesser-angled-nested-atpt 'ar-lesser-angled-nested-equalize-atpt)
(defun ar-lesser-angled-nested-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-lesser-angled-nested-atpt 'ar-lesser-angled-nested-greater-angle-atpt)
(defun ar-lesser-angled-nested-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for LESSER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-lesser-angled-nested-atpt 'ar-lesser-angled-nested-lesser-angle-atpt)
(defun ar-lesser-angled-nested-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for LESSER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-backslash-lesser-angled-nested-atpt 'ar-lesser-angled-nested-backslash-atpt)
(defun ar-lesser-angled-nested-backslash-atpt (&optional no-delimiters)
  "Backslash LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-brace-lesser-angled-nested-atpt 'ar-lesser-angled-nested-brace-atpt)
(defun ar-lesser-angled-nested-brace-atpt (&optional no-delimiters)
  "Braces LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-brace 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-bracket-lesser-angled-nested-atpt 'ar-lesser-angled-nested-bracket-atpt)
(defun ar-lesser-angled-nested-bracket-atpt (&optional no-delimiters)
  "Brackets LESSER-ANGLED-NESTED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-comment-lesser-angled-nested-atpt (&optional no-delimiters)
  "Comments LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-commatize-lesser-angled-nested-atpt (&optional no-delimiters)
  "Put a comma after LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-quote-lesser-angled-nested-atpt (&optional no-delimiters)
  "Put a singlequote before LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-hyphen-lesser-angled-nested-atpt 'ar-lesser-angled-nested-hyphen-atpt)
(defun ar-lesser-angled-nested-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-mark-lesser-angled-nested-atpt 'ar-lesser-angled-nested-mark-atpt)
(defun ar-lesser-angled-nested-mark-atpt ()
  "Marks LESSER-ANGLED-NESTED at point if any. "
  (interactive)
  (ar-th-mark 'lesser-angled-nested))

(defalias 'ar-hide-lesser-angled-nested-atpt 'ar-lesser-angled-nested-hide-atpt)
(defun ar-lesser-angled-nested-hide-atpt ()
  "Hides LESSER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-hide 'lesser-angled-nested))

(defalias 'ar-show-lesser-angled-nested-atpt 'ar-lesser-angled-nested-show-atpt)
(defun ar-lesser-angled-nested-show-atpt ()
  "Shows hidden LESSER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-show 'lesser-angled-nested))

(defalias 'ar-hide-show-lesser-angled-nested-atpt 'ar-lesser-angled-nested-hide-show-atpt)
(defun ar-lesser-angled-nested-hide-show-atpt ()
  "Alternatively hides or shows LESSER-ANGLED-NESTED at point. "
  (interactive)
  (ar-th-hide-show 'lesser-angled-nested))

(defalias 'ar-highlight-lesser-angled-nested-atpt-mode 'ar-lesser-angled-nested-highlight-atpt-mode)

(defun ar-lesser-angled-nested-highlight-atpt-mode (&optional no-delimiters)
  "Toggles lesser-angled-nested-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-kill-lesser-angled-nested-atpt 'ar-lesser-angled-nested-kill-atpt)
(defun ar-lesser-angled-nested-kill-atpt (&optional no-delimiters)
  "Kills LESSER-ANGLED-NESTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-lesser-angled-nested-atpt 'ar-lesser-angled-nested-kill-backward-atpt)
(defun ar-lesser-angled-nested-kill-backward-atpt (&optional no-delimiters)
  "Kills LESSER-ANGLED-NESTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-lesser-angled-nested-atpt 'ar-lesser-angled-nested-left-right-singlequote-atpt)
(defun ar-lesser-angled-nested-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-parentize-lesser-angled-nested-atpt 'ar-lesser-angled-nested-parentize-atpt)
(defun ar-lesser-angled-nested-parentize-atpt (&optional no-delimiters)
  "Parentizes LESSER-ANGLED-NESTED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-separate-lesser-angled-nested-atpt 'ar-lesser-angled-nested-separate-atpt)
(defun ar-lesser-angled-nested-separate-atpt (&optional no-delimiters)
  "Separates LESSER-ANGLED-NESTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-singlequote-lesser-angled-nested-atpt 'ar-lesser-angled-nested-singlequote-atpt)
(defun ar-lesser-angled-nested-singlequote-atpt (&optional no-delimiters)
  "Singlequotes LESSER-ANGLED-NESTED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-lesser-angled-nested-atpt 'ar-lesser-angled-nested-triplequote-dq-atpt)
(defun ar-lesser-angled-nested-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around lesser-angled-nested. "
  (interactive "*p")
  (ar-th-triplequote-dq 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-lesser-angled-nested-atpt 'ar-lesser-angled-nested-triplequote-sq-atpt)
(defun ar-lesser-angled-nested-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around lesser-angled-nested. "
  (interactive "*p")
  (ar-th-triplequote-sq 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-trim-lesser-angled-nested-atpt 'ar-lesser-angled-nested-trim-atpt)
(defun ar-lesser-angled-nested-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled-nested t t))
    (ar-th-trim 'lesser-angled-nested t t)))

(defalias 'ar-trim-left-lesser-angled-nested-atpt 'ar-lesser-angled-nested-left-trim-atpt)
(defun ar-lesser-angled-nested-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled-nested t nil))
    (ar-th-trim 'lesser-angled-nested t nil)))

(defalias 'ar-trim-right-lesser-angled-nested-atpt 'ar-lesser-angled-nested-right-trim-atpt)
(defun ar-lesser-angled-nested-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'lesser-angled-nested nil t))
    (ar-th-trim 'lesser-angled-nested nil t)))

(defalias 'ar-lesser-angled-nested-underscore-atpt 'ar-underscore-lesser-angled-nested-atpt)
(defun ar-underscore-lesser-angled-nested-atpt (&optional no-delimiters)
  "Put underscore char around LESSER-ANGLED-NESTED. "
  (interactive "*p")
  (ar-th-underscore 'lesser-angled-nested no-delimiters (interactive-p)))

(defalias 'ar-lesser-angled-nested-whitespace-atpt 'ar-whitespace-lesser-angled-nested-atpt)
(defun ar-whitespace-lesser-angled-nested-atpt (&optional no-delimiters)
  "Put whitespace char around LESSER-ANGLED-NESTED. "
  (interactive "*p")
  (ar-th-whitespace 'lesser-angled-nested nil t))

(defalias 'ar-forward-lesser-angled-nested-atpt 'ar-lesser-angled-nested-forward-atpt)
(defun ar-lesser-angled-nested-forward-atpt (&optional arg)
  "Moves forward over LESSER-ANGLED-NESTED at point if any, does nothing otherwise.
Returns end position of LESSER-ANGLED-NESTED "
  (interactive "p")
  (ar-th-forward 'lesser-angled-nested arg (interactive-p)))

(defalias 'ar-backward-lesser-angled-nested-atpt 'ar-lesser-angled-nested-backward-atpt)
(defun ar-lesser-angled-nested-backward-atpt (&optional arg)
  "Moves backward over LESSER-ANGLED-NESTED before point if any, does nothing otherwise.
Returns beginning position of LESSER-ANGLED-NESTED "
  (interactive "p")
  (ar-th-backward 'lesser-angled-nested arg (interactive-p)))

(defalias 'ar-transpose-lesser-angled-nested-atpt 'ar-lesser-angled-nested-transpose-atpt)
(defun ar-lesser-angled-nested-transpose-atpt (&optional arg)
  "Transposes LESSER-ANGLED-NESTED with LESSER-ANGLED-NESTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lesser-angled-nested arg (interactive-p)))

(defalias 'ar-sort-lesser-angled-nested-atpt 'ar-lesser-angled-nested-sort-atpt)
(defun ar-lesser-angled-nested-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesser-angled-nesteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'lesser-angled-nested reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lesser-angled-nested-atpt 'ar-lesser-angled-nested-check-atpt)
(defun ar-lesser-angled-nested-check-atpt ()
  "Return t if a LESSER-ANGLED-NESTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesser-angled-nested-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-lesser-angled-nested-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-buffer-atpt (&optional arg no-delimiters)
  "Returns buffer at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'buffer arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-buffer-atpt 'ar-buffer-bounds-atpt)
(defun ar-buffer-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of buffer if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer no-delimiters (interactive-p)))

(defun ar-buffer-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BUFFER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'buffer no-delimiters (interactive-p)))

(defun ar-buffer-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'buffer no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-buffer-atpt 'ar-buffer-beginning-atpt)
(defun ar-buffer-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'buffer no-delimiters (interactive-p)))

(defalias 'ar-end-of-buffer-atpt 'ar-buffer-end-atpt)
(defun ar-buffer-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'buffer no-delimiters (interactive-p)))

(defalias 'ar-in-buffer-p-atpt 'ar-buffer-in-p-atpt)
(defun ar-buffer-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BUFFER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer no-delimiters (interactive-p)))

(defalias 'ar-length-of-buffer-atpt 'ar-buffer-length-atpt)
(defun ar-buffer-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'buffer no-delimiters (interactive-p)))

(defalias 'ar-copy-buffer-atpt 'ar-buffer-copy-atpt)
(defun ar-buffer-copy-atpt (&optional no-delimiters)
  "Returns a copy of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'buffer no-delimiters (interactive-p)))

(defalias 'ar-delete-buffer-in-region 'ar-buffer-delete-in-region)
(defun ar-buffer-delete-in-region (beg end)
  "Deletes BUFFER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'buffer beg end (interactive-p)))

(defun ar-blok-buffer-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around buffer.
  Returns blok or nil if no BUFFER at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'buffer no-delimiters (interactive-p)))

(defalias 'ar-escape-buffer-atpt 'ar-buffer-escape-atpt)
(defun ar-buffer-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted BUFFER at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'buffer no-delimiters))

(defalias 'ar-doublequote-buffer-atpt 'ar-buffer-doublequote-atpt)
(defun ar-buffer-doublequote-atpt (&optional no-delimiters)
  "Doublequotes BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'buffer no-delimiters (interactive-p)))

(defalias 'ar-slash-buffer-atpt 'ar-buffer-slash-atpt)
(defun ar-buffer-slash-atpt (&optional no-delimiters)
  "Doublequotes BUFFER at point if any. "
  (interactive "*p")
  (ar-th-slash 'buffer no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-buffer-atpt 'ar-buffer-double-backslash-atpt)
(defun ar-buffer-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'buffer no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-buffer-atpt 'ar-buffer-doubleslash-atpt)
(defun ar-buffer-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'buffer no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-buffer-atpt 'ar-buffer-doubleslash-paren-atpt)
(defun ar-buffer-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'buffer no-delimiters (interactive-p)))

(defalias 'ar-slashparen-buffer-atpt 'ar-buffer-slashparen-atpt)
(defun ar-buffer-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'buffer no-delimiters (interactive-p)))

(defalias 'ar-dollar-buffer-atpt 'ar-buffer-dollar-atpt)
(defun ar-buffer-dollar-atpt (&optional no-delimiters)
  "Doublequotes BUFFER at point if any. "
  (interactive "*p")
  (ar-th-dollar 'buffer no-delimiters (interactive-p)))

(defalias 'ar-equalize-buffer-atpt 'ar-buffer-equalize-atpt)
(defun ar-buffer-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-equalize 'buffer no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-buffer-atpt 'ar-buffer-greater-angle-atpt)
(defun ar-buffer-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for BUFFER after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'buffer no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-buffer-atpt 'ar-buffer-lesser-angle-atpt)
(defun ar-buffer-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for BUFFER after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'buffer no-delimiters (interactive-p)))

(defalias 'ar-backslash-buffer-atpt 'ar-buffer-backslash-atpt)
(defun ar-buffer-backslash-atpt (&optional no-delimiters)
  "Backslash BUFFER at point if any. "
  (interactive "*p")
  (ar-th-backslash 'buffer no-delimiters (interactive-p)))

(defalias 'ar-brace-buffer-atpt 'ar-buffer-brace-atpt)
(defun ar-buffer-brace-atpt (&optional no-delimiters)
  "Braces BUFFER at point if any. "
  (interactive "*p")
  (ar-th-brace 'buffer no-delimiters (interactive-p)))

(defalias 'ar-bracket-buffer-atpt 'ar-buffer-bracket-atpt)
(defun ar-buffer-bracket-atpt (&optional no-delimiters)
  "Brackets BUFFER after point if any. "
  (interactive "*p")
  (ar-th-bracket 'buffer no-delimiters (interactive-p)))

(defun ar-comment-buffer-atpt (&optional no-delimiters)
  "Comments BUFFER at point if any. "
  (interactive "*p")
  (ar-th-comment 'buffer no-delimiters (interactive-p)))

(defun ar-commatize-buffer-atpt (&optional no-delimiters)
  "Put a comma after BUFFER at point if any. "
  (interactive "*p")
  (ar-th-commatize 'buffer no-delimiters (interactive-p)))

(defun ar-quote-buffer-atpt (&optional no-delimiters)
  "Put a singlequote before BUFFER at point if any. "
  (interactive "*p")
  (ar-th-quote 'buffer no-delimiters (interactive-p)))

(defalias 'ar-hyphen-buffer-atpt 'ar-buffer-hyphen-atpt)
(defun ar-buffer-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'buffer no-delimiters (interactive-p)))

(defalias 'ar-mark-buffer-atpt 'ar-buffer-mark-atpt)
(defun ar-buffer-mark-atpt ()
  "Marks BUFFER at point if any. "
  (interactive)
  (ar-th-mark 'buffer))

(defalias 'ar-hide-buffer-atpt 'ar-buffer-hide-atpt)
(defun ar-buffer-hide-atpt ()
  "Hides BUFFER at point. "
  (interactive)
  (ar-th-hide 'buffer))

(defalias 'ar-show-buffer-atpt 'ar-buffer-show-atpt)
(defun ar-buffer-show-atpt ()
  "Shows hidden BUFFER at point. "
  (interactive)
  (ar-th-show 'buffer))

(defalias 'ar-hide-show-buffer-atpt 'ar-buffer-hide-show-atpt)
(defun ar-buffer-hide-show-atpt ()
  "Alternatively hides or shows BUFFER at point. "
  (interactive)
  (ar-th-hide-show 'buffer))

(defalias 'ar-highlight-buffer-atpt-mode 'ar-buffer-highlight-atpt-mode)

(defun ar-buffer-highlight-atpt-mode (&optional no-delimiters)
  "Toggles buffer-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'buffer no-delimiters (interactive-p)))

(defalias 'ar-kill-buffer-atpt 'ar-buffer-kill-atpt)
(defun ar-buffer-kill-atpt (&optional no-delimiters)
  "Kills BUFFER at point if any. "
  (interactive "*P")
  (ar-th-kill 'buffer no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-buffer-atpt 'ar-buffer-kill-backward-atpt)
(defun ar-buffer-kill-backward-atpt (&optional no-delimiters)
  "Kills BUFFER at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'buffer no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-buffer-atpt 'ar-buffer-left-right-singlequote-atpt)
(defun ar-buffer-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'buffer no-delimiters (interactive-p)))

(defalias 'ar-parentize-buffer-atpt 'ar-buffer-parentize-atpt)
(defun ar-buffer-parentize-atpt (&optional no-delimiters)
  "Parentizes BUFFER at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'buffer no-delimiters (interactive-p)))

(defalias 'ar-separate-buffer-atpt 'ar-buffer-separate-atpt)
(defun ar-buffer-separate-atpt (&optional no-delimiters)
  "Separates BUFFER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'buffer no-delimiters (interactive-p)))

(defalias 'ar-singlequote-buffer-atpt 'ar-buffer-singlequote-atpt)
(defun ar-buffer-singlequote-atpt (&optional no-delimiters)
  "Singlequotes BUFFER at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'buffer no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-buffer-atpt 'ar-buffer-triplequote-dq-atpt)
(defun ar-buffer-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around buffer. "
  (interactive "*p")
  (ar-th-triplequote-dq 'buffer no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-buffer-atpt 'ar-buffer-triplequote-sq-atpt)
(defun ar-buffer-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around buffer. "
  (interactive "*p")
  (ar-th-triplequote-sq 'buffer no-delimiters (interactive-p)))

(defalias 'ar-trim-buffer-atpt 'ar-buffer-trim-atpt)
(defun ar-buffer-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'buffer t t))
    (ar-th-trim 'buffer t t)))

(defalias 'ar-trim-left-buffer-atpt 'ar-buffer-left-trim-atpt)
(defun ar-buffer-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'buffer t nil))
    (ar-th-trim 'buffer t nil)))

(defalias 'ar-trim-right-buffer-atpt 'ar-buffer-right-trim-atpt)
(defun ar-buffer-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'buffer nil t))
    (ar-th-trim 'buffer nil t)))

(defalias 'ar-buffer-underscore-atpt 'ar-underscore-buffer-atpt)
(defun ar-underscore-buffer-atpt (&optional no-delimiters)
  "Put underscore char around BUFFER. "
  (interactive "*p")
  (ar-th-underscore 'buffer no-delimiters (interactive-p)))

(defalias 'ar-buffer-whitespace-atpt 'ar-whitespace-buffer-atpt)
(defun ar-whitespace-buffer-atpt (&optional no-delimiters)
  "Put whitespace char around BUFFER. "
  (interactive "*p")
  (ar-th-whitespace 'buffer nil t))

(defalias 'ar-forward-buffer-atpt 'ar-buffer-forward-atpt)
(defun ar-buffer-forward-atpt (&optional arg)
  "Moves forward over BUFFER at point if any, does nothing otherwise.
Returns end position of BUFFER "
  (interactive "p")
  (ar-th-forward 'buffer arg (interactive-p)))

(defalias 'ar-backward-buffer-atpt 'ar-buffer-backward-atpt)
(defun ar-buffer-backward-atpt (&optional arg)
  "Moves backward over BUFFER before point if any, does nothing otherwise.
Returns beginning position of BUFFER "
  (interactive "p")
  (ar-th-backward 'buffer arg (interactive-p)))

(defalias 'ar-transpose-buffer-atpt 'ar-buffer-transpose-atpt)
(defun ar-buffer-transpose-atpt (&optional arg)
  "Transposes BUFFER with BUFFER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'buffer arg (interactive-p)))

(defalias 'ar-sort-buffer-atpt 'ar-buffer-sort-atpt)
(defun ar-buffer-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts buffers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'buffer reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-buffer-atpt 'ar-buffer-check-atpt)
(defun ar-buffer-check-atpt ()
  "Return t if a BUFFER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-buffer-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-buffer-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-comment-atpt (&optional arg no-delimiters)
  "Returns comment at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'comment arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-comment-atpt 'ar-comment-bounds-atpt)
(defun ar-comment-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of comment if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment no-delimiters (interactive-p)))

(defun ar-comment-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position COMMENT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'comment no-delimiters (interactive-p)))

(defun ar-comment-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'comment no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-comment-atpt 'ar-comment-beginning-atpt)
(defun ar-comment-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'comment no-delimiters (interactive-p)))

(defalias 'ar-end-of-comment-atpt 'ar-comment-end-atpt)
(defun ar-comment-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'comment no-delimiters (interactive-p)))

(defalias 'ar-in-comment-p-atpt 'ar-comment-in-p-atpt)
(defun ar-comment-in-p-atpt (&optional no-delimiters)
  "Returns bounds of COMMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment no-delimiters (interactive-p)))

(defalias 'ar-length-of-comment-atpt 'ar-comment-length-atpt)
(defun ar-comment-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'comment no-delimiters (interactive-p)))

(defalias 'ar-copy-comment-atpt 'ar-comment-copy-atpt)
(defun ar-comment-copy-atpt (&optional no-delimiters)
  "Returns a copy of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'comment no-delimiters (interactive-p)))

(defalias 'ar-delete-comment-in-region 'ar-comment-delete-in-region)
(defun ar-comment-delete-in-region (beg end)
  "Deletes COMMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'comment beg end (interactive-p)))

(defun ar-blok-comment-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around comment.
  Returns blok or nil if no COMMENT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'comment no-delimiters (interactive-p)))

(defalias 'ar-escape-comment-atpt 'ar-comment-escape-atpt)
(defun ar-comment-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted COMMENT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'comment no-delimiters))

(defalias 'ar-doublequote-comment-atpt 'ar-comment-doublequote-atpt)
(defun ar-comment-doublequote-atpt (&optional no-delimiters)
  "Doublequotes COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'comment no-delimiters (interactive-p)))

(defalias 'ar-slash-comment-atpt 'ar-comment-slash-atpt)
(defun ar-comment-slash-atpt (&optional no-delimiters)
  "Doublequotes COMMENT at point if any. "
  (interactive "*p")
  (ar-th-slash 'comment no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-comment-atpt 'ar-comment-double-backslash-atpt)
(defun ar-comment-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'comment no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-comment-atpt 'ar-comment-doubleslash-atpt)
(defun ar-comment-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'comment no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-comment-atpt 'ar-comment-doubleslash-paren-atpt)
(defun ar-comment-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'comment no-delimiters (interactive-p)))

(defalias 'ar-slashparen-comment-atpt 'ar-comment-slashparen-atpt)
(defun ar-comment-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'comment no-delimiters (interactive-p)))

(defalias 'ar-dollar-comment-atpt 'ar-comment-dollar-atpt)
(defun ar-comment-dollar-atpt (&optional no-delimiters)
  "Doublequotes COMMENT at point if any. "
  (interactive "*p")
  (ar-th-dollar 'comment no-delimiters (interactive-p)))

(defalias 'ar-equalize-comment-atpt 'ar-comment-equalize-atpt)
(defun ar-comment-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-equalize 'comment no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-comment-atpt 'ar-comment-greater-angle-atpt)
(defun ar-comment-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for COMMENT after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'comment no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-comment-atpt 'ar-comment-lesser-angle-atpt)
(defun ar-comment-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for COMMENT after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'comment no-delimiters (interactive-p)))

(defalias 'ar-backslash-comment-atpt 'ar-comment-backslash-atpt)
(defun ar-comment-backslash-atpt (&optional no-delimiters)
  "Backslash COMMENT at point if any. "
  (interactive "*p")
  (ar-th-backslash 'comment no-delimiters (interactive-p)))

(defalias 'ar-brace-comment-atpt 'ar-comment-brace-atpt)
(defun ar-comment-brace-atpt (&optional no-delimiters)
  "Braces COMMENT at point if any. "
  (interactive "*p")
  (ar-th-brace 'comment no-delimiters (interactive-p)))

(defalias 'ar-bracket-comment-atpt 'ar-comment-bracket-atpt)
(defun ar-comment-bracket-atpt (&optional no-delimiters)
  "Brackets COMMENT after point if any. "
  (interactive "*p")
  (ar-th-bracket 'comment no-delimiters (interactive-p)))

(defun ar-comment-comment-atpt (&optional no-delimiters)
  "Comments COMMENT at point if any. "
  (interactive "*p")
  (ar-th-comment 'comment no-delimiters (interactive-p)))

(defun ar-commatize-comment-atpt (&optional no-delimiters)
  "Put a comma after COMMENT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'comment no-delimiters (interactive-p)))

(defun ar-quote-comment-atpt (&optional no-delimiters)
  "Put a singlequote before COMMENT at point if any. "
  (interactive "*p")
  (ar-th-quote 'comment no-delimiters (interactive-p)))

(defalias 'ar-hyphen-comment-atpt 'ar-comment-hyphen-atpt)
(defun ar-comment-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'comment no-delimiters (interactive-p)))

(defalias 'ar-mark-comment-atpt 'ar-comment-mark-atpt)
(defun ar-comment-mark-atpt ()
  "Marks COMMENT at point if any. "
  (interactive)
  (ar-th-mark 'comment))

(defalias 'ar-hide-comment-atpt 'ar-comment-hide-atpt)
(defun ar-comment-hide-atpt ()
  "Hides COMMENT at point. "
  (interactive)
  (ar-th-hide 'comment))

(defalias 'ar-show-comment-atpt 'ar-comment-show-atpt)
(defun ar-comment-show-atpt ()
  "Shows hidden COMMENT at point. "
  (interactive)
  (ar-th-show 'comment))

(defalias 'ar-hide-show-comment-atpt 'ar-comment-hide-show-atpt)
(defun ar-comment-hide-show-atpt ()
  "Alternatively hides or shows COMMENT at point. "
  (interactive)
  (ar-th-hide-show 'comment))

(defalias 'ar-highlight-comment-atpt-mode 'ar-comment-highlight-atpt-mode)

(defun ar-comment-highlight-atpt-mode (&optional no-delimiters)
  "Toggles comment-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'comment no-delimiters (interactive-p)))

(defalias 'ar-kill-comment-atpt 'ar-comment-kill-atpt)
(defun ar-comment-kill-atpt (&optional no-delimiters)
  "Kills COMMENT at point if any. "
  (interactive "*P")
  (ar-th-kill 'comment no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-comment-atpt 'ar-comment-kill-backward-atpt)
(defun ar-comment-kill-backward-atpt (&optional no-delimiters)
  "Kills COMMENT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'comment no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-comment-atpt 'ar-comment-left-right-singlequote-atpt)
(defun ar-comment-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'comment no-delimiters (interactive-p)))

(defalias 'ar-parentize-comment-atpt 'ar-comment-parentize-atpt)
(defun ar-comment-parentize-atpt (&optional no-delimiters)
  "Parentizes COMMENT at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'comment no-delimiters (interactive-p)))

(defalias 'ar-separate-comment-atpt 'ar-comment-separate-atpt)
(defun ar-comment-separate-atpt (&optional no-delimiters)
  "Separates COMMENT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'comment no-delimiters (interactive-p)))

(defalias 'ar-singlequote-comment-atpt 'ar-comment-singlequote-atpt)
(defun ar-comment-singlequote-atpt (&optional no-delimiters)
  "Singlequotes COMMENT at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'comment no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-comment-atpt 'ar-comment-triplequote-dq-atpt)
(defun ar-comment-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around comment. "
  (interactive "*p")
  (ar-th-triplequote-dq 'comment no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-comment-atpt 'ar-comment-triplequote-sq-atpt)
(defun ar-comment-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around comment. "
  (interactive "*p")
  (ar-th-triplequote-sq 'comment no-delimiters (interactive-p)))

(defalias 'ar-trim-comment-atpt 'ar-comment-trim-atpt)
(defun ar-comment-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'comment t t))
    (ar-th-trim 'comment t t)))

(defalias 'ar-trim-left-comment-atpt 'ar-comment-left-trim-atpt)
(defun ar-comment-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'comment t nil))
    (ar-th-trim 'comment t nil)))

(defalias 'ar-trim-right-comment-atpt 'ar-comment-right-trim-atpt)
(defun ar-comment-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'comment nil t))
    (ar-th-trim 'comment nil t)))

(defalias 'ar-comment-underscore-atpt 'ar-underscore-comment-atpt)
(defun ar-underscore-comment-atpt (&optional no-delimiters)
  "Put underscore char around COMMENT. "
  (interactive "*p")
  (ar-th-underscore 'comment no-delimiters (interactive-p)))

(defalias 'ar-comment-whitespace-atpt 'ar-whitespace-comment-atpt)
(defun ar-whitespace-comment-atpt (&optional no-delimiters)
  "Put whitespace char around COMMENT. "
  (interactive "*p")
  (ar-th-whitespace 'comment nil t))

(defalias 'ar-forward-comment-atpt 'ar-comment-forward-atpt)
(defun ar-comment-forward-atpt (&optional arg)
  "Moves forward over COMMENT at point if any, does nothing otherwise.
Returns end position of COMMENT "
  (interactive "p")
  (ar-th-forward 'comment arg (interactive-p)))

(defalias 'ar-backward-comment-atpt 'ar-comment-backward-atpt)
(defun ar-comment-backward-atpt (&optional arg)
  "Moves backward over COMMENT before point if any, does nothing otherwise.
Returns beginning position of COMMENT "
  (interactive "p")
  (ar-th-backward 'comment arg (interactive-p)))

(defalias 'ar-transpose-comment-atpt 'ar-comment-transpose-atpt)
(defun ar-comment-transpose-atpt (&optional arg)
  "Transposes COMMENT with COMMENT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'comment arg (interactive-p)))

(defalias 'ar-sort-comment-atpt 'ar-comment-sort-atpt)
(defun ar-comment-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts comments in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'comment reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-comment-atpt 'ar-comment-check-atpt)
(defun ar-comment-check-atpt ()
  "Return t if a COMMENT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-comment-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-comment-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-csv-atpt (&optional arg no-delimiters)
  "Returns csv at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'csv arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-csv-atpt 'ar-csv-bounds-atpt)
(defun ar-csv-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of csv if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv no-delimiters (interactive-p)))

(defun ar-csv-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position CSV at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'csv no-delimiters (interactive-p)))

(defun ar-csv-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'csv no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-csv-atpt 'ar-csv-beginning-atpt)
(defun ar-csv-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'csv no-delimiters (interactive-p)))

(defalias 'ar-end-of-csv-atpt 'ar-csv-end-atpt)
(defun ar-csv-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'csv no-delimiters (interactive-p)))

(defalias 'ar-in-csv-p-atpt 'ar-csv-in-p-atpt)
(defun ar-csv-in-p-atpt (&optional no-delimiters)
  "Returns bounds of CSV at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv no-delimiters (interactive-p)))

(defalias 'ar-length-of-csv-atpt 'ar-csv-length-atpt)
(defun ar-csv-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'csv no-delimiters (interactive-p)))

(defalias 'ar-copy-csv-atpt 'ar-csv-copy-atpt)
(defun ar-csv-copy-atpt (&optional no-delimiters)
  "Returns a copy of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'csv no-delimiters (interactive-p)))

(defalias 'ar-delete-csv-in-region 'ar-csv-delete-in-region)
(defun ar-csv-delete-in-region (beg end)
  "Deletes CSV at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'csv beg end (interactive-p)))

(defun ar-blok-csv-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around csv.
  Returns blok or nil if no CSV at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'csv no-delimiters (interactive-p)))

(defalias 'ar-escape-csv-atpt 'ar-csv-escape-atpt)
(defun ar-csv-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted CSV at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'csv no-delimiters))

(defalias 'ar-doublequote-csv-atpt 'ar-csv-doublequote-atpt)
(defun ar-csv-doublequote-atpt (&optional no-delimiters)
  "Doublequotes CSV at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'csv no-delimiters (interactive-p)))

(defalias 'ar-slash-csv-atpt 'ar-csv-slash-atpt)
(defun ar-csv-slash-atpt (&optional no-delimiters)
  "Doublequotes CSV at point if any. "
  (interactive "*p")
  (ar-th-slash 'csv no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-csv-atpt 'ar-csv-double-backslash-atpt)
(defun ar-csv-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around CSV at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'csv no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-csv-atpt 'ar-csv-doubleslash-atpt)
(defun ar-csv-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around CSV at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'csv no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-csv-atpt 'ar-csv-doubleslash-paren-atpt)
(defun ar-csv-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around CSV at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'csv no-delimiters (interactive-p)))

(defalias 'ar-slashparen-csv-atpt 'ar-csv-slashparen-atpt)
(defun ar-csv-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around CSV at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'csv no-delimiters (interactive-p)))

(defalias 'ar-dollar-csv-atpt 'ar-csv-dollar-atpt)
(defun ar-csv-dollar-atpt (&optional no-delimiters)
  "Doublequotes CSV at point if any. "
  (interactive "*p")
  (ar-th-dollar 'csv no-delimiters (interactive-p)))

(defalias 'ar-equalize-csv-atpt 'ar-csv-equalize-atpt)
(defun ar-csv-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around CSV at point if any. "
  (interactive "*p")
  (ar-th-equalize 'csv no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-csv-atpt 'ar-csv-greater-angle-atpt)
(defun ar-csv-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for CSV after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'csv no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-csv-atpt 'ar-csv-lesser-angle-atpt)
(defun ar-csv-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for CSV after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'csv no-delimiters (interactive-p)))

(defalias 'ar-backslash-csv-atpt 'ar-csv-backslash-atpt)
(defun ar-csv-backslash-atpt (&optional no-delimiters)
  "Backslash CSV at point if any. "
  (interactive "*p")
  (ar-th-backslash 'csv no-delimiters (interactive-p)))

(defalias 'ar-brace-csv-atpt 'ar-csv-brace-atpt)
(defun ar-csv-brace-atpt (&optional no-delimiters)
  "Braces CSV at point if any. "
  (interactive "*p")
  (ar-th-brace 'csv no-delimiters (interactive-p)))

(defalias 'ar-bracket-csv-atpt 'ar-csv-bracket-atpt)
(defun ar-csv-bracket-atpt (&optional no-delimiters)
  "Brackets CSV after point if any. "
  (interactive "*p")
  (ar-th-bracket 'csv no-delimiters (interactive-p)))

(defun ar-comment-csv-atpt (&optional no-delimiters)
  "Comments CSV at point if any. "
  (interactive "*p")
  (ar-th-comment 'csv no-delimiters (interactive-p)))

(defun ar-commatize-csv-atpt (&optional no-delimiters)
  "Put a comma after CSV at point if any. "
  (interactive "*p")
  (ar-th-commatize 'csv no-delimiters (interactive-p)))

(defun ar-quote-csv-atpt (&optional no-delimiters)
  "Put a singlequote before CSV at point if any. "
  (interactive "*p")
  (ar-th-quote 'csv no-delimiters (interactive-p)))

(defalias 'ar-hyphen-csv-atpt 'ar-csv-hyphen-atpt)
(defun ar-csv-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around CSV at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'csv no-delimiters (interactive-p)))

(defalias 'ar-mark-csv-atpt 'ar-csv-mark-atpt)
(defun ar-csv-mark-atpt ()
  "Marks CSV at point if any. "
  (interactive)
  (ar-th-mark 'csv))

(defalias 'ar-hide-csv-atpt 'ar-csv-hide-atpt)
(defun ar-csv-hide-atpt ()
  "Hides CSV at point. "
  (interactive)
  (ar-th-hide 'csv))

(defalias 'ar-show-csv-atpt 'ar-csv-show-atpt)
(defun ar-csv-show-atpt ()
  "Shows hidden CSV at point. "
  (interactive)
  (ar-th-show 'csv))

(defalias 'ar-hide-show-csv-atpt 'ar-csv-hide-show-atpt)
(defun ar-csv-hide-show-atpt ()
  "Alternatively hides or shows CSV at point. "
  (interactive)
  (ar-th-hide-show 'csv))

(defalias 'ar-highlight-csv-atpt-mode 'ar-csv-highlight-atpt-mode)

(defun ar-csv-highlight-atpt-mode (&optional no-delimiters)
  "Toggles csv-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'csv no-delimiters (interactive-p)))

(defalias 'ar-kill-csv-atpt 'ar-csv-kill-atpt)
(defun ar-csv-kill-atpt (&optional no-delimiters)
  "Kills CSV at point if any. "
  (interactive "*P")
  (ar-th-kill 'csv no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-csv-atpt 'ar-csv-kill-backward-atpt)
(defun ar-csv-kill-backward-atpt (&optional no-delimiters)
  "Kills CSV at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'csv no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-csv-atpt 'ar-csv-left-right-singlequote-atpt)
(defun ar-csv-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'csv no-delimiters (interactive-p)))

(defalias 'ar-parentize-csv-atpt 'ar-csv-parentize-atpt)
(defun ar-csv-parentize-atpt (&optional no-delimiters)
  "Parentizes CSV at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'csv no-delimiters (interactive-p)))

(defalias 'ar-separate-csv-atpt 'ar-csv-separate-atpt)
(defun ar-csv-separate-atpt (&optional no-delimiters)
  "Separates CSV at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'csv no-delimiters (interactive-p)))

(defalias 'ar-singlequote-csv-atpt 'ar-csv-singlequote-atpt)
(defun ar-csv-singlequote-atpt (&optional no-delimiters)
  "Singlequotes CSV at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'csv no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-csv-atpt 'ar-csv-triplequote-dq-atpt)
(defun ar-csv-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around csv. "
  (interactive "*p")
  (ar-th-triplequote-dq 'csv no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-csv-atpt 'ar-csv-triplequote-sq-atpt)
(defun ar-csv-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around csv. "
  (interactive "*p")
  (ar-th-triplequote-sq 'csv no-delimiters (interactive-p)))

(defalias 'ar-trim-csv-atpt 'ar-csv-trim-atpt)
(defun ar-csv-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'csv t t))
    (ar-th-trim 'csv t t)))

(defalias 'ar-trim-left-csv-atpt 'ar-csv-left-trim-atpt)
(defun ar-csv-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'csv t nil))
    (ar-th-trim 'csv t nil)))

(defalias 'ar-trim-right-csv-atpt 'ar-csv-right-trim-atpt)
(defun ar-csv-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'csv nil t))
    (ar-th-trim 'csv nil t)))

(defalias 'ar-csv-underscore-atpt 'ar-underscore-csv-atpt)
(defun ar-underscore-csv-atpt (&optional no-delimiters)
  "Put underscore char around CSV. "
  (interactive "*p")
  (ar-th-underscore 'csv no-delimiters (interactive-p)))

(defalias 'ar-csv-whitespace-atpt 'ar-whitespace-csv-atpt)
(defun ar-whitespace-csv-atpt (&optional no-delimiters)
  "Put whitespace char around CSV. "
  (interactive "*p")
  (ar-th-whitespace 'csv nil t))

(defalias 'ar-forward-csv-atpt 'ar-csv-forward-atpt)
(defun ar-csv-forward-atpt (&optional arg)
  "Moves forward over CSV at point if any, does nothing otherwise.
Returns end position of CSV "
  (interactive "p")
  (ar-th-forward 'csv arg (interactive-p)))

(defalias 'ar-backward-csv-atpt 'ar-csv-backward-atpt)
(defun ar-csv-backward-atpt (&optional arg)
  "Moves backward over CSV before point if any, does nothing otherwise.
Returns beginning position of CSV "
  (interactive "p")
  (ar-th-backward 'csv arg (interactive-p)))

(defalias 'ar-transpose-csv-atpt 'ar-csv-transpose-atpt)
(defun ar-csv-transpose-atpt (&optional arg)
  "Transposes CSV with CSV before point if any. "
  (interactive "*p")
  (ar-th-transpose 'csv arg (interactive-p)))

(defalias 'ar-sort-csv-atpt 'ar-csv-sort-atpt)
(defun ar-csv-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts csvs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'csv reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-csv-atpt 'ar-csv-check-atpt)
(defun ar-csv-check-atpt ()
  "Return t if a CSV at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-csv-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-csv-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-date-atpt (&optional arg no-delimiters)
  "Returns date at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'date arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-date-atpt 'ar-date-bounds-atpt)
(defun ar-date-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of date if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date no-delimiters (interactive-p)))

(defun ar-date-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DATE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'date no-delimiters (interactive-p)))

(defun ar-date-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'date no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-date-atpt 'ar-date-beginning-atpt)
(defun ar-date-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'date no-delimiters (interactive-p)))

(defalias 'ar-end-of-date-atpt 'ar-date-end-atpt)
(defun ar-date-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'date no-delimiters (interactive-p)))

(defalias 'ar-in-date-p-atpt 'ar-date-in-p-atpt)
(defun ar-date-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DATE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date no-delimiters (interactive-p)))

(defalias 'ar-length-of-date-atpt 'ar-date-length-atpt)
(defun ar-date-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'date no-delimiters (interactive-p)))

(defalias 'ar-copy-date-atpt 'ar-date-copy-atpt)
(defun ar-date-copy-atpt (&optional no-delimiters)
  "Returns a copy of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'date no-delimiters (interactive-p)))

(defalias 'ar-delete-date-in-region 'ar-date-delete-in-region)
(defun ar-date-delete-in-region (beg end)
  "Deletes DATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'date beg end (interactive-p)))

(defun ar-blok-date-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around date.
  Returns blok or nil if no DATE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'date no-delimiters (interactive-p)))

(defalias 'ar-escape-date-atpt 'ar-date-escape-atpt)
(defun ar-date-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted DATE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'date no-delimiters))

(defalias 'ar-doublequote-date-atpt 'ar-date-doublequote-atpt)
(defun ar-date-doublequote-atpt (&optional no-delimiters)
  "Doublequotes DATE at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'date no-delimiters (interactive-p)))

(defalias 'ar-slash-date-atpt 'ar-date-slash-atpt)
(defun ar-date-slash-atpt (&optional no-delimiters)
  "Doublequotes DATE at point if any. "
  (interactive "*p")
  (ar-th-slash 'date no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-date-atpt 'ar-date-double-backslash-atpt)
(defun ar-date-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DATE at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'date no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-date-atpt 'ar-date-doubleslash-atpt)
(defun ar-date-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DATE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'date no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-date-atpt 'ar-date-doubleslash-paren-atpt)
(defun ar-date-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DATE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'date no-delimiters (interactive-p)))

(defalias 'ar-slashparen-date-atpt 'ar-date-slashparen-atpt)
(defun ar-date-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DATE at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'date no-delimiters (interactive-p)))

(defalias 'ar-dollar-date-atpt 'ar-date-dollar-atpt)
(defun ar-date-dollar-atpt (&optional no-delimiters)
  "Doublequotes DATE at point if any. "
  (interactive "*p")
  (ar-th-dollar 'date no-delimiters (interactive-p)))

(defalias 'ar-equalize-date-atpt 'ar-date-equalize-atpt)
(defun ar-date-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around DATE at point if any. "
  (interactive "*p")
  (ar-th-equalize 'date no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-date-atpt 'ar-date-greater-angle-atpt)
(defun ar-date-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for DATE after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'date no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-date-atpt 'ar-date-lesser-angle-atpt)
(defun ar-date-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for DATE after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'date no-delimiters (interactive-p)))

(defalias 'ar-backslash-date-atpt 'ar-date-backslash-atpt)
(defun ar-date-backslash-atpt (&optional no-delimiters)
  "Backslash DATE at point if any. "
  (interactive "*p")
  (ar-th-backslash 'date no-delimiters (interactive-p)))

(defalias 'ar-brace-date-atpt 'ar-date-brace-atpt)
(defun ar-date-brace-atpt (&optional no-delimiters)
  "Braces DATE at point if any. "
  (interactive "*p")
  (ar-th-brace 'date no-delimiters (interactive-p)))

(defalias 'ar-bracket-date-atpt 'ar-date-bracket-atpt)
(defun ar-date-bracket-atpt (&optional no-delimiters)
  "Brackets DATE after point if any. "
  (interactive "*p")
  (ar-th-bracket 'date no-delimiters (interactive-p)))

(defun ar-comment-date-atpt (&optional no-delimiters)
  "Comments DATE at point if any. "
  (interactive "*p")
  (ar-th-comment 'date no-delimiters (interactive-p)))

(defun ar-commatize-date-atpt (&optional no-delimiters)
  "Put a comma after DATE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'date no-delimiters (interactive-p)))

(defun ar-quote-date-atpt (&optional no-delimiters)
  "Put a singlequote before DATE at point if any. "
  (interactive "*p")
  (ar-th-quote 'date no-delimiters (interactive-p)))

(defalias 'ar-hyphen-date-atpt 'ar-date-hyphen-atpt)
(defun ar-date-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around DATE at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'date no-delimiters (interactive-p)))

(defalias 'ar-mark-date-atpt 'ar-date-mark-atpt)
(defun ar-date-mark-atpt ()
  "Marks DATE at point if any. "
  (interactive)
  (ar-th-mark 'date))

(defalias 'ar-hide-date-atpt 'ar-date-hide-atpt)
(defun ar-date-hide-atpt ()
  "Hides DATE at point. "
  (interactive)
  (ar-th-hide 'date))

(defalias 'ar-show-date-atpt 'ar-date-show-atpt)
(defun ar-date-show-atpt ()
  "Shows hidden DATE at point. "
  (interactive)
  (ar-th-show 'date))

(defalias 'ar-hide-show-date-atpt 'ar-date-hide-show-atpt)
(defun ar-date-hide-show-atpt ()
  "Alternatively hides or shows DATE at point. "
  (interactive)
  (ar-th-hide-show 'date))

(defalias 'ar-highlight-date-atpt-mode 'ar-date-highlight-atpt-mode)

(defun ar-date-highlight-atpt-mode (&optional no-delimiters)
  "Toggles date-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'date no-delimiters (interactive-p)))

(defalias 'ar-kill-date-atpt 'ar-date-kill-atpt)
(defun ar-date-kill-atpt (&optional no-delimiters)
  "Kills DATE at point if any. "
  (interactive "*P")
  (ar-th-kill 'date no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-date-atpt 'ar-date-kill-backward-atpt)
(defun ar-date-kill-backward-atpt (&optional no-delimiters)
  "Kills DATE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'date no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-date-atpt 'ar-date-left-right-singlequote-atpt)
(defun ar-date-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'date no-delimiters (interactive-p)))

(defalias 'ar-parentize-date-atpt 'ar-date-parentize-atpt)
(defun ar-date-parentize-atpt (&optional no-delimiters)
  "Parentizes DATE at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'date no-delimiters (interactive-p)))

(defalias 'ar-separate-date-atpt 'ar-date-separate-atpt)
(defun ar-date-separate-atpt (&optional no-delimiters)
  "Separates DATE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'date no-delimiters (interactive-p)))

(defalias 'ar-singlequote-date-atpt 'ar-date-singlequote-atpt)
(defun ar-date-singlequote-atpt (&optional no-delimiters)
  "Singlequotes DATE at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'date no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-date-atpt 'ar-date-triplequote-dq-atpt)
(defun ar-date-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around date. "
  (interactive "*p")
  (ar-th-triplequote-dq 'date no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-date-atpt 'ar-date-triplequote-sq-atpt)
(defun ar-date-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around date. "
  (interactive "*p")
  (ar-th-triplequote-sq 'date no-delimiters (interactive-p)))

(defalias 'ar-trim-date-atpt 'ar-date-trim-atpt)
(defun ar-date-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'date t t))
    (ar-th-trim 'date t t)))

(defalias 'ar-trim-left-date-atpt 'ar-date-left-trim-atpt)
(defun ar-date-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'date t nil))
    (ar-th-trim 'date t nil)))

(defalias 'ar-trim-right-date-atpt 'ar-date-right-trim-atpt)
(defun ar-date-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'date nil t))
    (ar-th-trim 'date nil t)))

(defalias 'ar-date-underscore-atpt 'ar-underscore-date-atpt)
(defun ar-underscore-date-atpt (&optional no-delimiters)
  "Put underscore char around DATE. "
  (interactive "*p")
  (ar-th-underscore 'date no-delimiters (interactive-p)))

(defalias 'ar-date-whitespace-atpt 'ar-whitespace-date-atpt)
(defun ar-whitespace-date-atpt (&optional no-delimiters)
  "Put whitespace char around DATE. "
  (interactive "*p")
  (ar-th-whitespace 'date nil t))

(defalias 'ar-forward-date-atpt 'ar-date-forward-atpt)
(defun ar-date-forward-atpt (&optional arg)
  "Moves forward over DATE at point if any, does nothing otherwise.
Returns end position of DATE "
  (interactive "p")
  (ar-th-forward 'date arg (interactive-p)))

(defalias 'ar-backward-date-atpt 'ar-date-backward-atpt)
(defun ar-date-backward-atpt (&optional arg)
  "Moves backward over DATE before point if any, does nothing otherwise.
Returns beginning position of DATE "
  (interactive "p")
  (ar-th-backward 'date arg (interactive-p)))

(defalias 'ar-transpose-date-atpt 'ar-date-transpose-atpt)
(defun ar-date-transpose-atpt (&optional arg)
  "Transposes DATE with DATE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'date arg (interactive-p)))

(defalias 'ar-sort-date-atpt 'ar-date-sort-atpt)
(defun ar-date-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts dates in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'date reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-date-atpt 'ar-date-check-atpt)
(defun ar-date-check-atpt ()
  "Return t if a DATE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-date-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-date-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-defun-atpt (&optional arg no-delimiters)
  "Returns defun at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'defun arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-defun-atpt 'ar-defun-bounds-atpt)
(defun ar-defun-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of defun if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'defun no-delimiters (interactive-p)))

(defun ar-defun-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DEFUN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'defun no-delimiters (interactive-p)))

(defun ar-defun-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DEFUN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'defun no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-defun-atpt 'ar-defun-beginning-atpt)
(defun ar-defun-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DEFUN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'defun no-delimiters (interactive-p)))

(defalias 'ar-end-of-defun-atpt 'ar-defun-end-atpt)
(defun ar-defun-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DEFUN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'defun no-delimiters (interactive-p)))

(defalias 'ar-in-defun-p-atpt 'ar-defun-in-p-atpt)
(defun ar-defun-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DEFUN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'defun no-delimiters (interactive-p)))

(defalias 'ar-length-of-defun-atpt 'ar-defun-length-atpt)
(defun ar-defun-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DEFUN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'defun no-delimiters (interactive-p)))

(defalias 'ar-copy-defun-atpt 'ar-defun-copy-atpt)
(defun ar-defun-copy-atpt (&optional no-delimiters)
  "Returns a copy of DEFUN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'defun no-delimiters (interactive-p)))

(defalias 'ar-delete-defun-in-region 'ar-defun-delete-in-region)
(defun ar-defun-delete-in-region (beg end)
  "Deletes DEFUN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'defun beg end (interactive-p)))

(defun ar-blok-defun-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around defun.
  Returns blok or nil if no DEFUN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'defun no-delimiters (interactive-p)))

(defalias 'ar-escape-defun-atpt 'ar-defun-escape-atpt)
(defun ar-defun-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted DEFUN at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'defun no-delimiters))

(defalias 'ar-doublequote-defun-atpt 'ar-defun-doublequote-atpt)
(defun ar-defun-doublequote-atpt (&optional no-delimiters)
  "Doublequotes DEFUN at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'defun no-delimiters (interactive-p)))

(defalias 'ar-slash-defun-atpt 'ar-defun-slash-atpt)
(defun ar-defun-slash-atpt (&optional no-delimiters)
  "Doublequotes DEFUN at point if any. "
  (interactive "*p")
  (ar-th-slash 'defun no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-defun-atpt 'ar-defun-double-backslash-atpt)
(defun ar-defun-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'defun no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-defun-atpt 'ar-defun-doubleslash-atpt)
(defun ar-defun-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'defun no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-defun-atpt 'ar-defun-doubleslash-paren-atpt)
(defun ar-defun-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'defun no-delimiters (interactive-p)))

(defalias 'ar-slashparen-defun-atpt 'ar-defun-slashparen-atpt)
(defun ar-defun-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'defun no-delimiters (interactive-p)))

(defalias 'ar-dollar-defun-atpt 'ar-defun-dollar-atpt)
(defun ar-defun-dollar-atpt (&optional no-delimiters)
  "Doublequotes DEFUN at point if any. "
  (interactive "*p")
  (ar-th-dollar 'defun no-delimiters (interactive-p)))

(defalias 'ar-equalize-defun-atpt 'ar-defun-equalize-atpt)
(defun ar-defun-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-equalize 'defun no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-defun-atpt 'ar-defun-greater-angle-atpt)
(defun ar-defun-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for DEFUN after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'defun no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-defun-atpt 'ar-defun-lesser-angle-atpt)
(defun ar-defun-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for DEFUN after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'defun no-delimiters (interactive-p)))

(defalias 'ar-backslash-defun-atpt 'ar-defun-backslash-atpt)
(defun ar-defun-backslash-atpt (&optional no-delimiters)
  "Backslash DEFUN at point if any. "
  (interactive "*p")
  (ar-th-backslash 'defun no-delimiters (interactive-p)))

(defalias 'ar-brace-defun-atpt 'ar-defun-brace-atpt)
(defun ar-defun-brace-atpt (&optional no-delimiters)
  "Braces DEFUN at point if any. "
  (interactive "*p")
  (ar-th-brace 'defun no-delimiters (interactive-p)))

(defalias 'ar-bracket-defun-atpt 'ar-defun-bracket-atpt)
(defun ar-defun-bracket-atpt (&optional no-delimiters)
  "Brackets DEFUN after point if any. "
  (interactive "*p")
  (ar-th-bracket 'defun no-delimiters (interactive-p)))

(defun ar-comment-defun-atpt (&optional no-delimiters)
  "Comments DEFUN at point if any. "
  (interactive "*p")
  (ar-th-comment 'defun no-delimiters (interactive-p)))

(defun ar-commatize-defun-atpt (&optional no-delimiters)
  "Put a comma after DEFUN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'defun no-delimiters (interactive-p)))

(defun ar-quote-defun-atpt (&optional no-delimiters)
  "Put a singlequote before DEFUN at point if any. "
  (interactive "*p")
  (ar-th-quote 'defun no-delimiters (interactive-p)))

(defalias 'ar-hyphen-defun-atpt 'ar-defun-hyphen-atpt)
(defun ar-defun-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around DEFUN at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'defun no-delimiters (interactive-p)))

(defalias 'ar-mark-defun-atpt 'ar-defun-mark-atpt)
(defun ar-defun-mark-atpt ()
  "Marks DEFUN at point if any. "
  (interactive)
  (ar-th-mark 'defun))

(defalias 'ar-hide-defun-atpt 'ar-defun-hide-atpt)
(defun ar-defun-hide-atpt ()
  "Hides DEFUN at point. "
  (interactive)
  (ar-th-hide 'defun))

(defalias 'ar-show-defun-atpt 'ar-defun-show-atpt)
(defun ar-defun-show-atpt ()
  "Shows hidden DEFUN at point. "
  (interactive)
  (ar-th-show 'defun))

(defalias 'ar-hide-show-defun-atpt 'ar-defun-hide-show-atpt)
(defun ar-defun-hide-show-atpt ()
  "Alternatively hides or shows DEFUN at point. "
  (interactive)
  (ar-th-hide-show 'defun))

(defalias 'ar-highlight-defun-atpt-mode 'ar-defun-highlight-atpt-mode)

(defun ar-defun-highlight-atpt-mode (&optional no-delimiters)
  "Toggles defun-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'defun no-delimiters (interactive-p)))

(defalias 'ar-kill-defun-atpt 'ar-defun-kill-atpt)
(defun ar-defun-kill-atpt (&optional no-delimiters)
  "Kills DEFUN at point if any. "
  (interactive "*P")
  (ar-th-kill 'defun no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-defun-atpt 'ar-defun-kill-backward-atpt)
(defun ar-defun-kill-backward-atpt (&optional no-delimiters)
  "Kills DEFUN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'defun no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-defun-atpt 'ar-defun-left-right-singlequote-atpt)
(defun ar-defun-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'defun no-delimiters (interactive-p)))

(defalias 'ar-parentize-defun-atpt 'ar-defun-parentize-atpt)
(defun ar-defun-parentize-atpt (&optional no-delimiters)
  "Parentizes DEFUN at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'defun no-delimiters (interactive-p)))

(defalias 'ar-separate-defun-atpt 'ar-defun-separate-atpt)
(defun ar-defun-separate-atpt (&optional no-delimiters)
  "Separates DEFUN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'defun no-delimiters (interactive-p)))

(defalias 'ar-singlequote-defun-atpt 'ar-defun-singlequote-atpt)
(defun ar-defun-singlequote-atpt (&optional no-delimiters)
  "Singlequotes DEFUN at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'defun no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-defun-atpt 'ar-defun-triplequote-dq-atpt)
(defun ar-defun-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around defun. "
  (interactive "*p")
  (ar-th-triplequote-dq 'defun no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-defun-atpt 'ar-defun-triplequote-sq-atpt)
(defun ar-defun-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around defun. "
  (interactive "*p")
  (ar-th-triplequote-sq 'defun no-delimiters (interactive-p)))

(defalias 'ar-trim-defun-atpt 'ar-defun-trim-atpt)
(defun ar-defun-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'defun t t))
    (ar-th-trim 'defun t t)))

(defalias 'ar-trim-left-defun-atpt 'ar-defun-left-trim-atpt)
(defun ar-defun-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'defun t nil))
    (ar-th-trim 'defun t nil)))

(defalias 'ar-trim-right-defun-atpt 'ar-defun-right-trim-atpt)
(defun ar-defun-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'defun nil t))
    (ar-th-trim 'defun nil t)))

(defalias 'ar-defun-underscore-atpt 'ar-underscore-defun-atpt)
(defun ar-underscore-defun-atpt (&optional no-delimiters)
  "Put underscore char around DEFUN. "
  (interactive "*p")
  (ar-th-underscore 'defun no-delimiters (interactive-p)))

(defalias 'ar-defun-whitespace-atpt 'ar-whitespace-defun-atpt)
(defun ar-whitespace-defun-atpt (&optional no-delimiters)
  "Put whitespace char around DEFUN. "
  (interactive "*p")
  (ar-th-whitespace 'defun nil t))

(defalias 'ar-forward-defun-atpt 'ar-defun-forward-atpt)
(defun ar-defun-forward-atpt (&optional arg)
  "Moves forward over DEFUN at point if any, does nothing otherwise.
Returns end position of DEFUN "
  (interactive "p")
  (ar-th-forward 'defun arg (interactive-p)))

(defalias 'ar-backward-defun-atpt 'ar-defun-backward-atpt)
(defun ar-defun-backward-atpt (&optional arg)
  "Moves backward over DEFUN before point if any, does nothing otherwise.
Returns beginning position of DEFUN "
  (interactive "p")
  (ar-th-backward 'defun arg (interactive-p)))

(defalias 'ar-transpose-defun-atpt 'ar-defun-transpose-atpt)
(defun ar-defun-transpose-atpt (&optional arg)
  "Transposes DEFUN with DEFUN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'defun arg (interactive-p)))

(defalias 'ar-sort-defun-atpt 'ar-defun-sort-atpt)
(defun ar-defun-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts defuns in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'defun reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-defun-atpt 'ar-defun-check-atpt)
(defun ar-defun-check-atpt ()
  "Return t if a DEFUN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-defun-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-defun-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-delimited-atpt (&optional arg no-delimiters)
  "Returns delimited at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'delimited arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-delimited-atpt 'ar-delimited-bounds-atpt)
(defun ar-delimited-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of delimited if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters (interactive-p)))

(defun ar-delimited-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DELIMITED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'delimited no-delimiters (interactive-p)))

(defun ar-delimited-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'delimited no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-delimited-atpt 'ar-delimited-beginning-atpt)
(defun ar-delimited-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'delimited no-delimiters (interactive-p)))

(defalias 'ar-end-of-delimited-atpt 'ar-delimited-end-atpt)
(defun ar-delimited-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'delimited no-delimiters (interactive-p)))

(defalias 'ar-in-delimited-p-atpt 'ar-delimited-in-p-atpt)
(defun ar-delimited-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DELIMITED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters (interactive-p)))

(defalias 'ar-length-of-delimited-atpt 'ar-delimited-length-atpt)
(defun ar-delimited-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'delimited no-delimiters (interactive-p)))

(defalias 'ar-copy-delimited-atpt 'ar-delimited-copy-atpt)
(defun ar-delimited-copy-atpt (&optional no-delimiters)
  "Returns a copy of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'delimited no-delimiters (interactive-p)))

(defalias 'ar-delete-delimited-in-region 'ar-delimited-delete-in-region)
(defun ar-delimited-delete-in-region (beg end)
  "Deletes DELIMITED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'delimited beg end (interactive-p)))

(defun ar-blok-delimited-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around delimited.
  Returns blok or nil if no DELIMITED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'delimited no-delimiters (interactive-p)))

(defalias 'ar-escape-delimited-atpt 'ar-delimited-escape-atpt)
(defun ar-delimited-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted DELIMITED at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'delimited no-delimiters))

(defalias 'ar-doublequote-delimited-atpt 'ar-delimited-doublequote-atpt)
(defun ar-delimited-doublequote-atpt (&optional no-delimiters)
  "Doublequotes DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'delimited no-delimiters (interactive-p)))

(defalias 'ar-slash-delimited-atpt 'ar-delimited-slash-atpt)
(defun ar-delimited-slash-atpt (&optional no-delimiters)
  "Doublequotes DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-slash 'delimited no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-delimited-atpt 'ar-delimited-double-backslash-atpt)
(defun ar-delimited-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'delimited no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-delimited-atpt 'ar-delimited-doubleslash-atpt)
(defun ar-delimited-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'delimited no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-delimited-atpt 'ar-delimited-doubleslash-paren-atpt)
(defun ar-delimited-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'delimited no-delimiters (interactive-p)))

(defalias 'ar-slashparen-delimited-atpt 'ar-delimited-slashparen-atpt)
(defun ar-delimited-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'delimited no-delimiters (interactive-p)))

(defalias 'ar-dollar-delimited-atpt 'ar-delimited-dollar-atpt)
(defun ar-delimited-dollar-atpt (&optional no-delimiters)
  "Doublequotes DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-dollar 'delimited no-delimiters (interactive-p)))

(defalias 'ar-equalize-delimited-atpt 'ar-delimited-equalize-atpt)
(defun ar-delimited-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-equalize 'delimited no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-delimited-atpt 'ar-delimited-greater-angle-atpt)
(defun ar-delimited-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for DELIMITED after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'delimited no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-delimited-atpt 'ar-delimited-lesser-angle-atpt)
(defun ar-delimited-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for DELIMITED after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'delimited no-delimiters (interactive-p)))

(defalias 'ar-backslash-delimited-atpt 'ar-delimited-backslash-atpt)
(defun ar-delimited-backslash-atpt (&optional no-delimiters)
  "Backslash DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-backslash 'delimited no-delimiters (interactive-p)))

(defalias 'ar-brace-delimited-atpt 'ar-delimited-brace-atpt)
(defun ar-delimited-brace-atpt (&optional no-delimiters)
  "Braces DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-brace 'delimited no-delimiters (interactive-p)))

(defalias 'ar-bracket-delimited-atpt 'ar-delimited-bracket-atpt)
(defun ar-delimited-bracket-atpt (&optional no-delimiters)
  "Brackets DELIMITED after point if any. "
  (interactive "*p")
  (ar-th-bracket 'delimited no-delimiters (interactive-p)))

(defun ar-comment-delimited-atpt (&optional no-delimiters)
  "Comments DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-comment 'delimited no-delimiters (interactive-p)))

(defun ar-commatize-delimited-atpt (&optional no-delimiters)
  "Put a comma after DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'delimited no-delimiters (interactive-p)))

(defun ar-quote-delimited-atpt (&optional no-delimiters)
  "Put a singlequote before DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-quote 'delimited no-delimiters (interactive-p)))

(defalias 'ar-hyphen-delimited-atpt 'ar-delimited-hyphen-atpt)
(defun ar-delimited-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'delimited no-delimiters (interactive-p)))

(defalias 'ar-mark-delimited-atpt 'ar-delimited-mark-atpt)
(defun ar-delimited-mark-atpt ()
  "Marks DELIMITED at point if any. "
  (interactive)
  (ar-th-mark 'delimited))

(defalias 'ar-hide-delimited-atpt 'ar-delimited-hide-atpt)
(defun ar-delimited-hide-atpt ()
  "Hides DELIMITED at point. "
  (interactive)
  (ar-th-hide 'delimited))

(defalias 'ar-show-delimited-atpt 'ar-delimited-show-atpt)
(defun ar-delimited-show-atpt ()
  "Shows hidden DELIMITED at point. "
  (interactive)
  (ar-th-show 'delimited))

(defalias 'ar-hide-show-delimited-atpt 'ar-delimited-hide-show-atpt)
(defun ar-delimited-hide-show-atpt ()
  "Alternatively hides or shows DELIMITED at point. "
  (interactive)
  (ar-th-hide-show 'delimited))

(defalias 'ar-highlight-delimited-atpt-mode 'ar-delimited-highlight-atpt-mode)

(defun ar-delimited-highlight-atpt-mode (&optional no-delimiters)
  "Toggles delimited-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'delimited no-delimiters (interactive-p)))

(defalias 'ar-kill-delimited-atpt 'ar-delimited-kill-atpt)
(defun ar-delimited-kill-atpt (&optional no-delimiters)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill 'delimited no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-delimited-atpt 'ar-delimited-kill-backward-atpt)
(defun ar-delimited-kill-backward-atpt (&optional no-delimiters)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'delimited no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-delimited-atpt 'ar-delimited-left-right-singlequote-atpt)
(defun ar-delimited-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'delimited no-delimiters (interactive-p)))

(defalias 'ar-parentize-delimited-atpt 'ar-delimited-parentize-atpt)
(defun ar-delimited-parentize-atpt (&optional no-delimiters)
  "Parentizes DELIMITED at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'delimited no-delimiters (interactive-p)))

(defalias 'ar-separate-delimited-atpt 'ar-delimited-separate-atpt)
(defun ar-delimited-separate-atpt (&optional no-delimiters)
  "Separates DELIMITED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'delimited no-delimiters (interactive-p)))

(defalias 'ar-singlequote-delimited-atpt 'ar-delimited-singlequote-atpt)
(defun ar-delimited-singlequote-atpt (&optional no-delimiters)
  "Singlequotes DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'delimited no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-delimited-atpt 'ar-delimited-triplequote-dq-atpt)
(defun ar-delimited-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around delimited. "
  (interactive "*p")
  (ar-th-triplequote-dq 'delimited no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-delimited-atpt 'ar-delimited-triplequote-sq-atpt)
(defun ar-delimited-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around delimited. "
  (interactive "*p")
  (ar-th-triplequote-sq 'delimited no-delimiters (interactive-p)))

(defalias 'ar-trim-delimited-atpt 'ar-delimited-trim-atpt)
(defun ar-delimited-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'delimited t t))
    (ar-th-trim 'delimited t t)))

(defalias 'ar-trim-left-delimited-atpt 'ar-delimited-left-trim-atpt)
(defun ar-delimited-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'delimited t nil))
    (ar-th-trim 'delimited t nil)))

(defalias 'ar-trim-right-delimited-atpt 'ar-delimited-right-trim-atpt)
(defun ar-delimited-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'delimited nil t))
    (ar-th-trim 'delimited nil t)))

(defalias 'ar-delimited-underscore-atpt 'ar-underscore-delimited-atpt)
(defun ar-underscore-delimited-atpt (&optional no-delimiters)
  "Put underscore char around DELIMITED. "
  (interactive "*p")
  (ar-th-underscore 'delimited no-delimiters (interactive-p)))

(defalias 'ar-delimited-whitespace-atpt 'ar-whitespace-delimited-atpt)
(defun ar-whitespace-delimited-atpt (&optional no-delimiters)
  "Put whitespace char around DELIMITED. "
  (interactive "*p")
  (ar-th-whitespace 'delimited nil t))

(defalias 'ar-forward-delimited-atpt 'ar-delimited-forward-atpt)
(defun ar-delimited-forward-atpt (&optional arg)
  "Moves forward over DELIMITED at point if any, does nothing otherwise.
Returns end position of DELIMITED "
  (interactive "p")
  (ar-th-forward 'delimited arg (interactive-p)))

(defalias 'ar-backward-delimited-atpt 'ar-delimited-backward-atpt)
(defun ar-delimited-backward-atpt (&optional arg)
  "Moves backward over DELIMITED before point if any, does nothing otherwise.
Returns beginning position of DELIMITED "
  (interactive "p")
  (ar-th-backward 'delimited arg (interactive-p)))

(defalias 'ar-transpose-delimited-atpt 'ar-delimited-transpose-atpt)
(defun ar-delimited-transpose-atpt (&optional arg)
  "Transposes DELIMITED with DELIMITED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'delimited arg (interactive-p)))

(defalias 'ar-sort-delimited-atpt 'ar-delimited-sort-atpt)
(defun ar-delimited-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts delimiteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'delimited reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-delimited-atpt 'ar-delimited-check-atpt)
(defun ar-delimited-check-atpt ()
  "Return t if a DELIMITED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-delimited-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-delimited-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-email-atpt (&optional arg no-delimiters)
  "Returns email at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'email arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-email-atpt 'ar-email-bounds-atpt)
(defun ar-email-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of email if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email no-delimiters (interactive-p)))

(defun ar-email-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position EMAIL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'email no-delimiters (interactive-p)))

(defun ar-email-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'email no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-email-atpt 'ar-email-beginning-atpt)
(defun ar-email-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'email no-delimiters (interactive-p)))

(defalias 'ar-end-of-email-atpt 'ar-email-end-atpt)
(defun ar-email-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'email no-delimiters (interactive-p)))

(defalias 'ar-in-email-p-atpt 'ar-email-in-p-atpt)
(defun ar-email-in-p-atpt (&optional no-delimiters)
  "Returns bounds of EMAIL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email no-delimiters (interactive-p)))

(defalias 'ar-length-of-email-atpt 'ar-email-length-atpt)
(defun ar-email-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'email no-delimiters (interactive-p)))

(defalias 'ar-copy-email-atpt 'ar-email-copy-atpt)
(defun ar-email-copy-atpt (&optional no-delimiters)
  "Returns a copy of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'email no-delimiters (interactive-p)))

(defalias 'ar-delete-email-in-region 'ar-email-delete-in-region)
(defun ar-email-delete-in-region (beg end)
  "Deletes EMAIL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'email beg end (interactive-p)))

(defun ar-blok-email-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around email.
  Returns blok or nil if no EMAIL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'email no-delimiters (interactive-p)))

(defalias 'ar-escape-email-atpt 'ar-email-escape-atpt)
(defun ar-email-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted EMAIL at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'email no-delimiters))

(defalias 'ar-doublequote-email-atpt 'ar-email-doublequote-atpt)
(defun ar-email-doublequote-atpt (&optional no-delimiters)
  "Doublequotes EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'email no-delimiters (interactive-p)))

(defalias 'ar-slash-email-atpt 'ar-email-slash-atpt)
(defun ar-email-slash-atpt (&optional no-delimiters)
  "Doublequotes EMAIL at point if any. "
  (interactive "*p")
  (ar-th-slash 'email no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-email-atpt 'ar-email-double-backslash-atpt)
(defun ar-email-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'email no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-email-atpt 'ar-email-doubleslash-atpt)
(defun ar-email-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'email no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-email-atpt 'ar-email-doubleslash-paren-atpt)
(defun ar-email-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'email no-delimiters (interactive-p)))

(defalias 'ar-slashparen-email-atpt 'ar-email-slashparen-atpt)
(defun ar-email-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'email no-delimiters (interactive-p)))

(defalias 'ar-dollar-email-atpt 'ar-email-dollar-atpt)
(defun ar-email-dollar-atpt (&optional no-delimiters)
  "Doublequotes EMAIL at point if any. "
  (interactive "*p")
  (ar-th-dollar 'email no-delimiters (interactive-p)))

(defalias 'ar-equalize-email-atpt 'ar-email-equalize-atpt)
(defun ar-email-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-equalize 'email no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-email-atpt 'ar-email-greater-angle-atpt)
(defun ar-email-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for EMAIL after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'email no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-email-atpt 'ar-email-lesser-angle-atpt)
(defun ar-email-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for EMAIL after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'email no-delimiters (interactive-p)))

(defalias 'ar-backslash-email-atpt 'ar-email-backslash-atpt)
(defun ar-email-backslash-atpt (&optional no-delimiters)
  "Backslash EMAIL at point if any. "
  (interactive "*p")
  (ar-th-backslash 'email no-delimiters (interactive-p)))

(defalias 'ar-brace-email-atpt 'ar-email-brace-atpt)
(defun ar-email-brace-atpt (&optional no-delimiters)
  "Braces EMAIL at point if any. "
  (interactive "*p")
  (ar-th-brace 'email no-delimiters (interactive-p)))

(defalias 'ar-bracket-email-atpt 'ar-email-bracket-atpt)
(defun ar-email-bracket-atpt (&optional no-delimiters)
  "Brackets EMAIL after point if any. "
  (interactive "*p")
  (ar-th-bracket 'email no-delimiters (interactive-p)))

(defun ar-comment-email-atpt (&optional no-delimiters)
  "Comments EMAIL at point if any. "
  (interactive "*p")
  (ar-th-comment 'email no-delimiters (interactive-p)))

(defun ar-commatize-email-atpt (&optional no-delimiters)
  "Put a comma after EMAIL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'email no-delimiters (interactive-p)))

(defun ar-quote-email-atpt (&optional no-delimiters)
  "Put a singlequote before EMAIL at point if any. "
  (interactive "*p")
  (ar-th-quote 'email no-delimiters (interactive-p)))

(defalias 'ar-hyphen-email-atpt 'ar-email-hyphen-atpt)
(defun ar-email-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'email no-delimiters (interactive-p)))

(defalias 'ar-mark-email-atpt 'ar-email-mark-atpt)
(defun ar-email-mark-atpt ()
  "Marks EMAIL at point if any. "
  (interactive)
  (ar-th-mark 'email))

(defalias 'ar-hide-email-atpt 'ar-email-hide-atpt)
(defun ar-email-hide-atpt ()
  "Hides EMAIL at point. "
  (interactive)
  (ar-th-hide 'email))

(defalias 'ar-show-email-atpt 'ar-email-show-atpt)
(defun ar-email-show-atpt ()
  "Shows hidden EMAIL at point. "
  (interactive)
  (ar-th-show 'email))

(defalias 'ar-hide-show-email-atpt 'ar-email-hide-show-atpt)
(defun ar-email-hide-show-atpt ()
  "Alternatively hides or shows EMAIL at point. "
  (interactive)
  (ar-th-hide-show 'email))

(defalias 'ar-highlight-email-atpt-mode 'ar-email-highlight-atpt-mode)

(defun ar-email-highlight-atpt-mode (&optional no-delimiters)
  "Toggles email-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'email no-delimiters (interactive-p)))

(defalias 'ar-kill-email-atpt 'ar-email-kill-atpt)
(defun ar-email-kill-atpt (&optional no-delimiters)
  "Kills EMAIL at point if any. "
  (interactive "*P")
  (ar-th-kill 'email no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-email-atpt 'ar-email-kill-backward-atpt)
(defun ar-email-kill-backward-atpt (&optional no-delimiters)
  "Kills EMAIL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'email no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-email-atpt 'ar-email-left-right-singlequote-atpt)
(defun ar-email-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'email no-delimiters (interactive-p)))

(defalias 'ar-parentize-email-atpt 'ar-email-parentize-atpt)
(defun ar-email-parentize-atpt (&optional no-delimiters)
  "Parentizes EMAIL at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'email no-delimiters (interactive-p)))

(defalias 'ar-separate-email-atpt 'ar-email-separate-atpt)
(defun ar-email-separate-atpt (&optional no-delimiters)
  "Separates EMAIL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'email no-delimiters (interactive-p)))

(defalias 'ar-singlequote-email-atpt 'ar-email-singlequote-atpt)
(defun ar-email-singlequote-atpt (&optional no-delimiters)
  "Singlequotes EMAIL at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'email no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-email-atpt 'ar-email-triplequote-dq-atpt)
(defun ar-email-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around email. "
  (interactive "*p")
  (ar-th-triplequote-dq 'email no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-email-atpt 'ar-email-triplequote-sq-atpt)
(defun ar-email-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around email. "
  (interactive "*p")
  (ar-th-triplequote-sq 'email no-delimiters (interactive-p)))

(defalias 'ar-trim-email-atpt 'ar-email-trim-atpt)
(defun ar-email-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'email t t))
    (ar-th-trim 'email t t)))

(defalias 'ar-trim-left-email-atpt 'ar-email-left-trim-atpt)
(defun ar-email-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'email t nil))
    (ar-th-trim 'email t nil)))

(defalias 'ar-trim-right-email-atpt 'ar-email-right-trim-atpt)
(defun ar-email-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'email nil t))
    (ar-th-trim 'email nil t)))

(defalias 'ar-email-underscore-atpt 'ar-underscore-email-atpt)
(defun ar-underscore-email-atpt (&optional no-delimiters)
  "Put underscore char around EMAIL. "
  (interactive "*p")
  (ar-th-underscore 'email no-delimiters (interactive-p)))

(defalias 'ar-email-whitespace-atpt 'ar-whitespace-email-atpt)
(defun ar-whitespace-email-atpt (&optional no-delimiters)
  "Put whitespace char around EMAIL. "
  (interactive "*p")
  (ar-th-whitespace 'email nil t))

(defalias 'ar-forward-email-atpt 'ar-email-forward-atpt)
(defun ar-email-forward-atpt (&optional arg)
  "Moves forward over EMAIL at point if any, does nothing otherwise.
Returns end position of EMAIL "
  (interactive "p")
  (ar-th-forward 'email arg (interactive-p)))

(defalias 'ar-backward-email-atpt 'ar-email-backward-atpt)
(defun ar-email-backward-atpt (&optional arg)
  "Moves backward over EMAIL before point if any, does nothing otherwise.
Returns beginning position of EMAIL "
  (interactive "p")
  (ar-th-backward 'email arg (interactive-p)))

(defalias 'ar-transpose-email-atpt 'ar-email-transpose-atpt)
(defun ar-email-transpose-atpt (&optional arg)
  "Transposes EMAIL with EMAIL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'email arg (interactive-p)))

(defalias 'ar-sort-email-atpt 'ar-email-sort-atpt)
(defun ar-email-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts emails in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'email reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-email-atpt 'ar-email-check-atpt)
(defun ar-email-check-atpt ()
  "Return t if a EMAIL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-email-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-email-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-filename-atpt (&optional arg no-delimiters)
  "Returns filename at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'filename arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-filename-atpt 'ar-filename-bounds-atpt)
(defun ar-filename-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of filename if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename no-delimiters (interactive-p)))

(defun ar-filename-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FILENAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'filename no-delimiters (interactive-p)))

(defun ar-filename-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'filename no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-filename-atpt 'ar-filename-beginning-atpt)
(defun ar-filename-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'filename no-delimiters (interactive-p)))

(defalias 'ar-end-of-filename-atpt 'ar-filename-end-atpt)
(defun ar-filename-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'filename no-delimiters (interactive-p)))

(defalias 'ar-in-filename-p-atpt 'ar-filename-in-p-atpt)
(defun ar-filename-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FILENAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename no-delimiters (interactive-p)))

(defalias 'ar-length-of-filename-atpt 'ar-filename-length-atpt)
(defun ar-filename-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'filename no-delimiters (interactive-p)))

(defalias 'ar-copy-filename-atpt 'ar-filename-copy-atpt)
(defun ar-filename-copy-atpt (&optional no-delimiters)
  "Returns a copy of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'filename no-delimiters (interactive-p)))

(defalias 'ar-delete-filename-in-region 'ar-filename-delete-in-region)
(defun ar-filename-delete-in-region (beg end)
  "Deletes FILENAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'filename beg end (interactive-p)))

(defun ar-blok-filename-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around filename.
  Returns blok or nil if no FILENAME at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'filename no-delimiters (interactive-p)))

(defalias 'ar-escape-filename-atpt 'ar-filename-escape-atpt)
(defun ar-filename-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted FILENAME at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'filename no-delimiters))

(defalias 'ar-doublequote-filename-atpt 'ar-filename-doublequote-atpt)
(defun ar-filename-doublequote-atpt (&optional no-delimiters)
  "Doublequotes FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'filename no-delimiters (interactive-p)))

(defalias 'ar-slash-filename-atpt 'ar-filename-slash-atpt)
(defun ar-filename-slash-atpt (&optional no-delimiters)
  "Doublequotes FILENAME at point if any. "
  (interactive "*p")
  (ar-th-slash 'filename no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-filename-atpt 'ar-filename-double-backslash-atpt)
(defun ar-filename-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'filename no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-filename-atpt 'ar-filename-doubleslash-atpt)
(defun ar-filename-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'filename no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-filename-atpt 'ar-filename-doubleslash-paren-atpt)
(defun ar-filename-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'filename no-delimiters (interactive-p)))

(defalias 'ar-slashparen-filename-atpt 'ar-filename-slashparen-atpt)
(defun ar-filename-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'filename no-delimiters (interactive-p)))

(defalias 'ar-dollar-filename-atpt 'ar-filename-dollar-atpt)
(defun ar-filename-dollar-atpt (&optional no-delimiters)
  "Doublequotes FILENAME at point if any. "
  (interactive "*p")
  (ar-th-dollar 'filename no-delimiters (interactive-p)))

(defalias 'ar-equalize-filename-atpt 'ar-filename-equalize-atpt)
(defun ar-filename-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-equalize 'filename no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-filename-atpt 'ar-filename-greater-angle-atpt)
(defun ar-filename-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for FILENAME after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'filename no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-filename-atpt 'ar-filename-lesser-angle-atpt)
(defun ar-filename-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for FILENAME after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'filename no-delimiters (interactive-p)))

(defalias 'ar-backslash-filename-atpt 'ar-filename-backslash-atpt)
(defun ar-filename-backslash-atpt (&optional no-delimiters)
  "Backslash FILENAME at point if any. "
  (interactive "*p")
  (ar-th-backslash 'filename no-delimiters (interactive-p)))

(defalias 'ar-brace-filename-atpt 'ar-filename-brace-atpt)
(defun ar-filename-brace-atpt (&optional no-delimiters)
  "Braces FILENAME at point if any. "
  (interactive "*p")
  (ar-th-brace 'filename no-delimiters (interactive-p)))

(defalias 'ar-bracket-filename-atpt 'ar-filename-bracket-atpt)
(defun ar-filename-bracket-atpt (&optional no-delimiters)
  "Brackets FILENAME after point if any. "
  (interactive "*p")
  (ar-th-bracket 'filename no-delimiters (interactive-p)))

(defun ar-comment-filename-atpt (&optional no-delimiters)
  "Comments FILENAME at point if any. "
  (interactive "*p")
  (ar-th-comment 'filename no-delimiters (interactive-p)))

(defun ar-commatize-filename-atpt (&optional no-delimiters)
  "Put a comma after FILENAME at point if any. "
  (interactive "*p")
  (ar-th-commatize 'filename no-delimiters (interactive-p)))

(defun ar-quote-filename-atpt (&optional no-delimiters)
  "Put a singlequote before FILENAME at point if any. "
  (interactive "*p")
  (ar-th-quote 'filename no-delimiters (interactive-p)))

(defalias 'ar-hyphen-filename-atpt 'ar-filename-hyphen-atpt)
(defun ar-filename-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'filename no-delimiters (interactive-p)))

(defalias 'ar-mark-filename-atpt 'ar-filename-mark-atpt)
(defun ar-filename-mark-atpt ()
  "Marks FILENAME at point if any. "
  (interactive)
  (ar-th-mark 'filename))

(defalias 'ar-hide-filename-atpt 'ar-filename-hide-atpt)
(defun ar-filename-hide-atpt ()
  "Hides FILENAME at point. "
  (interactive)
  (ar-th-hide 'filename))

(defalias 'ar-show-filename-atpt 'ar-filename-show-atpt)
(defun ar-filename-show-atpt ()
  "Shows hidden FILENAME at point. "
  (interactive)
  (ar-th-show 'filename))

(defalias 'ar-hide-show-filename-atpt 'ar-filename-hide-show-atpt)
(defun ar-filename-hide-show-atpt ()
  "Alternatively hides or shows FILENAME at point. "
  (interactive)
  (ar-th-hide-show 'filename))

(defalias 'ar-highlight-filename-atpt-mode 'ar-filename-highlight-atpt-mode)

(defun ar-filename-highlight-atpt-mode (&optional no-delimiters)
  "Toggles filename-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'filename no-delimiters (interactive-p)))

(defalias 'ar-kill-filename-atpt 'ar-filename-kill-atpt)
(defun ar-filename-kill-atpt (&optional no-delimiters)
  "Kills FILENAME at point if any. "
  (interactive "*P")
  (ar-th-kill 'filename no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-filename-atpt 'ar-filename-kill-backward-atpt)
(defun ar-filename-kill-backward-atpt (&optional no-delimiters)
  "Kills FILENAME at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'filename no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-filename-atpt 'ar-filename-left-right-singlequote-atpt)
(defun ar-filename-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'filename no-delimiters (interactive-p)))

(defalias 'ar-parentize-filename-atpt 'ar-filename-parentize-atpt)
(defun ar-filename-parentize-atpt (&optional no-delimiters)
  "Parentizes FILENAME at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'filename no-delimiters (interactive-p)))

(defalias 'ar-separate-filename-atpt 'ar-filename-separate-atpt)
(defun ar-filename-separate-atpt (&optional no-delimiters)
  "Separates FILENAME at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'filename no-delimiters (interactive-p)))

(defalias 'ar-singlequote-filename-atpt 'ar-filename-singlequote-atpt)
(defun ar-filename-singlequote-atpt (&optional no-delimiters)
  "Singlequotes FILENAME at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'filename no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-filename-atpt 'ar-filename-triplequote-dq-atpt)
(defun ar-filename-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around filename. "
  (interactive "*p")
  (ar-th-triplequote-dq 'filename no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-filename-atpt 'ar-filename-triplequote-sq-atpt)
(defun ar-filename-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around filename. "
  (interactive "*p")
  (ar-th-triplequote-sq 'filename no-delimiters (interactive-p)))

(defalias 'ar-trim-filename-atpt 'ar-filename-trim-atpt)
(defun ar-filename-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'filename t t))
    (ar-th-trim 'filename t t)))

(defalias 'ar-trim-left-filename-atpt 'ar-filename-left-trim-atpt)
(defun ar-filename-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'filename t nil))
    (ar-th-trim 'filename t nil)))

(defalias 'ar-trim-right-filename-atpt 'ar-filename-right-trim-atpt)
(defun ar-filename-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'filename nil t))
    (ar-th-trim 'filename nil t)))

(defalias 'ar-filename-underscore-atpt 'ar-underscore-filename-atpt)
(defun ar-underscore-filename-atpt (&optional no-delimiters)
  "Put underscore char around FILENAME. "
  (interactive "*p")
  (ar-th-underscore 'filename no-delimiters (interactive-p)))

(defalias 'ar-filename-whitespace-atpt 'ar-whitespace-filename-atpt)
(defun ar-whitespace-filename-atpt (&optional no-delimiters)
  "Put whitespace char around FILENAME. "
  (interactive "*p")
  (ar-th-whitespace 'filename nil t))

(defalias 'ar-forward-filename-atpt 'ar-filename-forward-atpt)
(defun ar-filename-forward-atpt (&optional arg)
  "Moves forward over FILENAME at point if any, does nothing otherwise.
Returns end position of FILENAME "
  (interactive "p")
  (ar-th-forward 'filename arg (interactive-p)))

(defalias 'ar-backward-filename-atpt 'ar-filename-backward-atpt)
(defun ar-filename-backward-atpt (&optional arg)
  "Moves backward over FILENAME before point if any, does nothing otherwise.
Returns beginning position of FILENAME "
  (interactive "p")
  (ar-th-backward 'filename arg (interactive-p)))

(defalias 'ar-transpose-filename-atpt 'ar-filename-transpose-atpt)
(defun ar-filename-transpose-atpt (&optional arg)
  "Transposes FILENAME with FILENAME before point if any. "
  (interactive "*p")
  (ar-th-transpose 'filename arg (interactive-p)))

(defalias 'ar-sort-filename-atpt 'ar-filename-sort-atpt)
(defun ar-filename-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts filenames in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'filename reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-filename-atpt 'ar-filename-check-atpt)
(defun ar-filename-check-atpt ()
  "Return t if a FILENAME at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-filename-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-filename-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-float-atpt (&optional arg no-delimiters)
  "Returns float at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'float arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-float-atpt 'ar-float-bounds-atpt)
(defun ar-float-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of float if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float no-delimiters (interactive-p)))

(defun ar-float-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FLOAT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'float no-delimiters (interactive-p)))

(defun ar-float-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'float no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-float-atpt 'ar-float-beginning-atpt)
(defun ar-float-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'float no-delimiters (interactive-p)))

(defalias 'ar-end-of-float-atpt 'ar-float-end-atpt)
(defun ar-float-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'float no-delimiters (interactive-p)))

(defalias 'ar-in-float-p-atpt 'ar-float-in-p-atpt)
(defun ar-float-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FLOAT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float no-delimiters (interactive-p)))

(defalias 'ar-length-of-float-atpt 'ar-float-length-atpt)
(defun ar-float-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'float no-delimiters (interactive-p)))

(defalias 'ar-copy-float-atpt 'ar-float-copy-atpt)
(defun ar-float-copy-atpt (&optional no-delimiters)
  "Returns a copy of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'float no-delimiters (interactive-p)))

(defalias 'ar-delete-float-in-region 'ar-float-delete-in-region)
(defun ar-float-delete-in-region (beg end)
  "Deletes FLOAT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'float beg end (interactive-p)))

(defun ar-blok-float-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around float.
  Returns blok or nil if no FLOAT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'float no-delimiters (interactive-p)))

(defalias 'ar-escape-float-atpt 'ar-float-escape-atpt)
(defun ar-float-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted FLOAT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'float no-delimiters))

(defalias 'ar-doublequote-float-atpt 'ar-float-doublequote-atpt)
(defun ar-float-doublequote-atpt (&optional no-delimiters)
  "Doublequotes FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'float no-delimiters (interactive-p)))

(defalias 'ar-slash-float-atpt 'ar-float-slash-atpt)
(defun ar-float-slash-atpt (&optional no-delimiters)
  "Doublequotes FLOAT at point if any. "
  (interactive "*p")
  (ar-th-slash 'float no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-float-atpt 'ar-float-double-backslash-atpt)
(defun ar-float-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'float no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-float-atpt 'ar-float-doubleslash-atpt)
(defun ar-float-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'float no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-float-atpt 'ar-float-doubleslash-paren-atpt)
(defun ar-float-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'float no-delimiters (interactive-p)))

(defalias 'ar-slashparen-float-atpt 'ar-float-slashparen-atpt)
(defun ar-float-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'float no-delimiters (interactive-p)))

(defalias 'ar-dollar-float-atpt 'ar-float-dollar-atpt)
(defun ar-float-dollar-atpt (&optional no-delimiters)
  "Doublequotes FLOAT at point if any. "
  (interactive "*p")
  (ar-th-dollar 'float no-delimiters (interactive-p)))

(defalias 'ar-equalize-float-atpt 'ar-float-equalize-atpt)
(defun ar-float-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-equalize 'float no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-float-atpt 'ar-float-greater-angle-atpt)
(defun ar-float-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for FLOAT after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'float no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-float-atpt 'ar-float-lesser-angle-atpt)
(defun ar-float-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for FLOAT after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'float no-delimiters (interactive-p)))

(defalias 'ar-backslash-float-atpt 'ar-float-backslash-atpt)
(defun ar-float-backslash-atpt (&optional no-delimiters)
  "Backslash FLOAT at point if any. "
  (interactive "*p")
  (ar-th-backslash 'float no-delimiters (interactive-p)))

(defalias 'ar-brace-float-atpt 'ar-float-brace-atpt)
(defun ar-float-brace-atpt (&optional no-delimiters)
  "Braces FLOAT at point if any. "
  (interactive "*p")
  (ar-th-brace 'float no-delimiters (interactive-p)))

(defalias 'ar-bracket-float-atpt 'ar-float-bracket-atpt)
(defun ar-float-bracket-atpt (&optional no-delimiters)
  "Brackets FLOAT after point if any. "
  (interactive "*p")
  (ar-th-bracket 'float no-delimiters (interactive-p)))

(defun ar-comment-float-atpt (&optional no-delimiters)
  "Comments FLOAT at point if any. "
  (interactive "*p")
  (ar-th-comment 'float no-delimiters (interactive-p)))

(defun ar-commatize-float-atpt (&optional no-delimiters)
  "Put a comma after FLOAT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'float no-delimiters (interactive-p)))

(defun ar-quote-float-atpt (&optional no-delimiters)
  "Put a singlequote before FLOAT at point if any. "
  (interactive "*p")
  (ar-th-quote 'float no-delimiters (interactive-p)))

(defalias 'ar-hyphen-float-atpt 'ar-float-hyphen-atpt)
(defun ar-float-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'float no-delimiters (interactive-p)))

(defalias 'ar-mark-float-atpt 'ar-float-mark-atpt)
(defun ar-float-mark-atpt ()
  "Marks FLOAT at point if any. "
  (interactive)
  (ar-th-mark 'float))

(defalias 'ar-hide-float-atpt 'ar-float-hide-atpt)
(defun ar-float-hide-atpt ()
  "Hides FLOAT at point. "
  (interactive)
  (ar-th-hide 'float))

(defalias 'ar-show-float-atpt 'ar-float-show-atpt)
(defun ar-float-show-atpt ()
  "Shows hidden FLOAT at point. "
  (interactive)
  (ar-th-show 'float))

(defalias 'ar-hide-show-float-atpt 'ar-float-hide-show-atpt)
(defun ar-float-hide-show-atpt ()
  "Alternatively hides or shows FLOAT at point. "
  (interactive)
  (ar-th-hide-show 'float))

(defalias 'ar-highlight-float-atpt-mode 'ar-float-highlight-atpt-mode)

(defun ar-float-highlight-atpt-mode (&optional no-delimiters)
  "Toggles float-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'float no-delimiters (interactive-p)))

(defalias 'ar-kill-float-atpt 'ar-float-kill-atpt)
(defun ar-float-kill-atpt (&optional no-delimiters)
  "Kills FLOAT at point if any. "
  (interactive "*P")
  (ar-th-kill 'float no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-float-atpt 'ar-float-kill-backward-atpt)
(defun ar-float-kill-backward-atpt (&optional no-delimiters)
  "Kills FLOAT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'float no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-float-atpt 'ar-float-left-right-singlequote-atpt)
(defun ar-float-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'float no-delimiters (interactive-p)))

(defalias 'ar-parentize-float-atpt 'ar-float-parentize-atpt)
(defun ar-float-parentize-atpt (&optional no-delimiters)
  "Parentizes FLOAT at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'float no-delimiters (interactive-p)))

(defalias 'ar-separate-float-atpt 'ar-float-separate-atpt)
(defun ar-float-separate-atpt (&optional no-delimiters)
  "Separates FLOAT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'float no-delimiters (interactive-p)))

(defalias 'ar-singlequote-float-atpt 'ar-float-singlequote-atpt)
(defun ar-float-singlequote-atpt (&optional no-delimiters)
  "Singlequotes FLOAT at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'float no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-float-atpt 'ar-float-triplequote-dq-atpt)
(defun ar-float-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around float. "
  (interactive "*p")
  (ar-th-triplequote-dq 'float no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-float-atpt 'ar-float-triplequote-sq-atpt)
(defun ar-float-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around float. "
  (interactive "*p")
  (ar-th-triplequote-sq 'float no-delimiters (interactive-p)))

(defalias 'ar-trim-float-atpt 'ar-float-trim-atpt)
(defun ar-float-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'float t t))
    (ar-th-trim 'float t t)))

(defalias 'ar-trim-left-float-atpt 'ar-float-left-trim-atpt)
(defun ar-float-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'float t nil))
    (ar-th-trim 'float t nil)))

(defalias 'ar-trim-right-float-atpt 'ar-float-right-trim-atpt)
(defun ar-float-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'float nil t))
    (ar-th-trim 'float nil t)))

(defalias 'ar-float-underscore-atpt 'ar-underscore-float-atpt)
(defun ar-underscore-float-atpt (&optional no-delimiters)
  "Put underscore char around FLOAT. "
  (interactive "*p")
  (ar-th-underscore 'float no-delimiters (interactive-p)))

(defalias 'ar-float-whitespace-atpt 'ar-whitespace-float-atpt)
(defun ar-whitespace-float-atpt (&optional no-delimiters)
  "Put whitespace char around FLOAT. "
  (interactive "*p")
  (ar-th-whitespace 'float nil t))

(defalias 'ar-forward-float-atpt 'ar-float-forward-atpt)
(defun ar-float-forward-atpt (&optional arg)
  "Moves forward over FLOAT at point if any, does nothing otherwise.
Returns end position of FLOAT "
  (interactive "p")
  (ar-th-forward 'float arg (interactive-p)))

(defalias 'ar-backward-float-atpt 'ar-float-backward-atpt)
(defun ar-float-backward-atpt (&optional arg)
  "Moves backward over FLOAT before point if any, does nothing otherwise.
Returns beginning position of FLOAT "
  (interactive "p")
  (ar-th-backward 'float arg (interactive-p)))

(defalias 'ar-transpose-float-atpt 'ar-float-transpose-atpt)
(defun ar-float-transpose-atpt (&optional arg)
  "Transposes FLOAT with FLOAT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'float arg (interactive-p)))

(defalias 'ar-sort-float-atpt 'ar-float-sort-atpt)
(defun ar-float-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts floats in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'float reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-float-atpt 'ar-float-check-atpt)
(defun ar-float-check-atpt ()
  "Return t if a FLOAT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-float-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-float-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-function-atpt (&optional arg no-delimiters)
  "Returns function at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'function arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-function-atpt 'ar-function-bounds-atpt)
(defun ar-function-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of function if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function no-delimiters (interactive-p)))

(defun ar-function-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FUNCTION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'function no-delimiters (interactive-p)))

(defun ar-function-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'function no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-function-atpt 'ar-function-beginning-atpt)
(defun ar-function-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'function no-delimiters (interactive-p)))

(defalias 'ar-end-of-function-atpt 'ar-function-end-atpt)
(defun ar-function-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'function no-delimiters (interactive-p)))

(defalias 'ar-in-function-p-atpt 'ar-function-in-p-atpt)
(defun ar-function-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FUNCTION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function no-delimiters (interactive-p)))

(defalias 'ar-length-of-function-atpt 'ar-function-length-atpt)
(defun ar-function-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'function no-delimiters (interactive-p)))

(defalias 'ar-copy-function-atpt 'ar-function-copy-atpt)
(defun ar-function-copy-atpt (&optional no-delimiters)
  "Returns a copy of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'function no-delimiters (interactive-p)))

(defalias 'ar-delete-function-in-region 'ar-function-delete-in-region)
(defun ar-function-delete-in-region (beg end)
  "Deletes FUNCTION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'function beg end (interactive-p)))

(defun ar-blok-function-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around function.
  Returns blok or nil if no FUNCTION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'function no-delimiters (interactive-p)))

(defalias 'ar-escape-function-atpt 'ar-function-escape-atpt)
(defun ar-function-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted FUNCTION at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'function no-delimiters))

(defalias 'ar-doublequote-function-atpt 'ar-function-doublequote-atpt)
(defun ar-function-doublequote-atpt (&optional no-delimiters)
  "Doublequotes FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'function no-delimiters (interactive-p)))

(defalias 'ar-slash-function-atpt 'ar-function-slash-atpt)
(defun ar-function-slash-atpt (&optional no-delimiters)
  "Doublequotes FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-slash 'function no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-function-atpt 'ar-function-double-backslash-atpt)
(defun ar-function-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'function no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-function-atpt 'ar-function-doubleslash-atpt)
(defun ar-function-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'function no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-function-atpt 'ar-function-doubleslash-paren-atpt)
(defun ar-function-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'function no-delimiters (interactive-p)))

(defalias 'ar-slashparen-function-atpt 'ar-function-slashparen-atpt)
(defun ar-function-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'function no-delimiters (interactive-p)))

(defalias 'ar-dollar-function-atpt 'ar-function-dollar-atpt)
(defun ar-function-dollar-atpt (&optional no-delimiters)
  "Doublequotes FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-dollar 'function no-delimiters (interactive-p)))

(defalias 'ar-equalize-function-atpt 'ar-function-equalize-atpt)
(defun ar-function-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-equalize 'function no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-function-atpt 'ar-function-greater-angle-atpt)
(defun ar-function-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for FUNCTION after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'function no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-function-atpt 'ar-function-lesser-angle-atpt)
(defun ar-function-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for FUNCTION after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'function no-delimiters (interactive-p)))

(defalias 'ar-backslash-function-atpt 'ar-function-backslash-atpt)
(defun ar-function-backslash-atpt (&optional no-delimiters)
  "Backslash FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-backslash 'function no-delimiters (interactive-p)))

(defalias 'ar-brace-function-atpt 'ar-function-brace-atpt)
(defun ar-function-brace-atpt (&optional no-delimiters)
  "Braces FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-brace 'function no-delimiters (interactive-p)))

(defalias 'ar-bracket-function-atpt 'ar-function-bracket-atpt)
(defun ar-function-bracket-atpt (&optional no-delimiters)
  "Brackets FUNCTION after point if any. "
  (interactive "*p")
  (ar-th-bracket 'function no-delimiters (interactive-p)))

(defun ar-comment-function-atpt (&optional no-delimiters)
  "Comments FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-comment 'function no-delimiters (interactive-p)))

(defun ar-commatize-function-atpt (&optional no-delimiters)
  "Put a comma after FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'function no-delimiters (interactive-p)))

(defun ar-quote-function-atpt (&optional no-delimiters)
  "Put a singlequote before FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-quote 'function no-delimiters (interactive-p)))

(defalias 'ar-hyphen-function-atpt 'ar-function-hyphen-atpt)
(defun ar-function-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'function no-delimiters (interactive-p)))

(defalias 'ar-mark-function-atpt 'ar-function-mark-atpt)
(defun ar-function-mark-atpt ()
  "Marks FUNCTION at point if any. "
  (interactive)
  (ar-th-mark 'function))

(defalias 'ar-hide-function-atpt 'ar-function-hide-atpt)
(defun ar-function-hide-atpt ()
  "Hides FUNCTION at point. "
  (interactive)
  (ar-th-hide 'function))

(defalias 'ar-show-function-atpt 'ar-function-show-atpt)
(defun ar-function-show-atpt ()
  "Shows hidden FUNCTION at point. "
  (interactive)
  (ar-th-show 'function))

(defalias 'ar-hide-show-function-atpt 'ar-function-hide-show-atpt)
(defun ar-function-hide-show-atpt ()
  "Alternatively hides or shows FUNCTION at point. "
  (interactive)
  (ar-th-hide-show 'function))

(defalias 'ar-highlight-function-atpt-mode 'ar-function-highlight-atpt-mode)

(defun ar-function-highlight-atpt-mode (&optional no-delimiters)
  "Toggles function-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'function no-delimiters (interactive-p)))

(defalias 'ar-kill-function-atpt 'ar-function-kill-atpt)
(defun ar-function-kill-atpt (&optional no-delimiters)
  "Kills FUNCTION at point if any. "
  (interactive "*P")
  (ar-th-kill 'function no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-function-atpt 'ar-function-kill-backward-atpt)
(defun ar-function-kill-backward-atpt (&optional no-delimiters)
  "Kills FUNCTION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'function no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-function-atpt 'ar-function-left-right-singlequote-atpt)
(defun ar-function-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'function no-delimiters (interactive-p)))

(defalias 'ar-parentize-function-atpt 'ar-function-parentize-atpt)
(defun ar-function-parentize-atpt (&optional no-delimiters)
  "Parentizes FUNCTION at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'function no-delimiters (interactive-p)))

(defalias 'ar-separate-function-atpt 'ar-function-separate-atpt)
(defun ar-function-separate-atpt (&optional no-delimiters)
  "Separates FUNCTION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'function no-delimiters (interactive-p)))

(defalias 'ar-singlequote-function-atpt 'ar-function-singlequote-atpt)
(defun ar-function-singlequote-atpt (&optional no-delimiters)
  "Singlequotes FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'function no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-function-atpt 'ar-function-triplequote-dq-atpt)
(defun ar-function-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around function. "
  (interactive "*p")
  (ar-th-triplequote-dq 'function no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-function-atpt 'ar-function-triplequote-sq-atpt)
(defun ar-function-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around function. "
  (interactive "*p")
  (ar-th-triplequote-sq 'function no-delimiters (interactive-p)))

(defalias 'ar-trim-function-atpt 'ar-function-trim-atpt)
(defun ar-function-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'function t t))
    (ar-th-trim 'function t t)))

(defalias 'ar-trim-left-function-atpt 'ar-function-left-trim-atpt)
(defun ar-function-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'function t nil))
    (ar-th-trim 'function t nil)))

(defalias 'ar-trim-right-function-atpt 'ar-function-right-trim-atpt)
(defun ar-function-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'function nil t))
    (ar-th-trim 'function nil t)))

(defalias 'ar-function-underscore-atpt 'ar-underscore-function-atpt)
(defun ar-underscore-function-atpt (&optional no-delimiters)
  "Put underscore char around FUNCTION. "
  (interactive "*p")
  (ar-th-underscore 'function no-delimiters (interactive-p)))

(defalias 'ar-function-whitespace-atpt 'ar-whitespace-function-atpt)
(defun ar-whitespace-function-atpt (&optional no-delimiters)
  "Put whitespace char around FUNCTION. "
  (interactive "*p")
  (ar-th-whitespace 'function nil t))

(defalias 'ar-forward-function-atpt 'ar-function-forward-atpt)
(defun ar-function-forward-atpt (&optional arg)
  "Moves forward over FUNCTION at point if any, does nothing otherwise.
Returns end position of FUNCTION "
  (interactive "p")
  (ar-th-forward 'function arg (interactive-p)))

(defalias 'ar-backward-function-atpt 'ar-function-backward-atpt)
(defun ar-function-backward-atpt (&optional arg)
  "Moves backward over FUNCTION before point if any, does nothing otherwise.
Returns beginning position of FUNCTION "
  (interactive "p")
  (ar-th-backward 'function arg (interactive-p)))

(defalias 'ar-transpose-function-atpt 'ar-function-transpose-atpt)
(defun ar-function-transpose-atpt (&optional arg)
  "Transposes FUNCTION with FUNCTION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'function arg (interactive-p)))

(defalias 'ar-sort-function-atpt 'ar-function-sort-atpt)
(defun ar-function-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts functions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'function reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-function-atpt 'ar-function-check-atpt)
(defun ar-function-check-atpt ()
  "Return t if a FUNCTION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-function-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-function-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-ip-atpt (&optional arg no-delimiters)
  "Returns ip at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'ip arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-ip-atpt 'ar-ip-bounds-atpt)
(defun ar-ip-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of ip if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip no-delimiters (interactive-p)))

(defun ar-ip-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position IP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'ip no-delimiters (interactive-p)))

(defun ar-ip-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'ip no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-ip-atpt 'ar-ip-beginning-atpt)
(defun ar-ip-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'ip no-delimiters (interactive-p)))

(defalias 'ar-end-of-ip-atpt 'ar-ip-end-atpt)
(defun ar-ip-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'ip no-delimiters (interactive-p)))

(defalias 'ar-in-ip-p-atpt 'ar-ip-in-p-atpt)
(defun ar-ip-in-p-atpt (&optional no-delimiters)
  "Returns bounds of IP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip no-delimiters (interactive-p)))

(defalias 'ar-length-of-ip-atpt 'ar-ip-length-atpt)
(defun ar-ip-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'ip no-delimiters (interactive-p)))

(defalias 'ar-copy-ip-atpt 'ar-ip-copy-atpt)
(defun ar-ip-copy-atpt (&optional no-delimiters)
  "Returns a copy of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'ip no-delimiters (interactive-p)))

(defalias 'ar-delete-ip-in-region 'ar-ip-delete-in-region)
(defun ar-ip-delete-in-region (beg end)
  "Deletes IP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ip beg end (interactive-p)))

(defun ar-blok-ip-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ip.
  Returns blok or nil if no IP at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'ip no-delimiters (interactive-p)))

(defalias 'ar-escape-ip-atpt 'ar-ip-escape-atpt)
(defun ar-ip-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted IP at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'ip no-delimiters))

(defalias 'ar-doublequote-ip-atpt 'ar-ip-doublequote-atpt)
(defun ar-ip-doublequote-atpt (&optional no-delimiters)
  "Doublequotes IP at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'ip no-delimiters (interactive-p)))

(defalias 'ar-slash-ip-atpt 'ar-ip-slash-atpt)
(defun ar-ip-slash-atpt (&optional no-delimiters)
  "Doublequotes IP at point if any. "
  (interactive "*p")
  (ar-th-slash 'ip no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-ip-atpt 'ar-ip-double-backslash-atpt)
(defun ar-ip-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around IP at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'ip no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-ip-atpt 'ar-ip-doubleslash-atpt)
(defun ar-ip-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around IP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'ip no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-ip-atpt 'ar-ip-doubleslash-paren-atpt)
(defun ar-ip-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around IP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'ip no-delimiters (interactive-p)))

(defalias 'ar-slashparen-ip-atpt 'ar-ip-slashparen-atpt)
(defun ar-ip-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around IP at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'ip no-delimiters (interactive-p)))

(defalias 'ar-dollar-ip-atpt 'ar-ip-dollar-atpt)
(defun ar-ip-dollar-atpt (&optional no-delimiters)
  "Doublequotes IP at point if any. "
  (interactive "*p")
  (ar-th-dollar 'ip no-delimiters (interactive-p)))

(defalias 'ar-equalize-ip-atpt 'ar-ip-equalize-atpt)
(defun ar-ip-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around IP at point if any. "
  (interactive "*p")
  (ar-th-equalize 'ip no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-ip-atpt 'ar-ip-greater-angle-atpt)
(defun ar-ip-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for IP after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'ip no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-ip-atpt 'ar-ip-lesser-angle-atpt)
(defun ar-ip-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for IP after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'ip no-delimiters (interactive-p)))

(defalias 'ar-backslash-ip-atpt 'ar-ip-backslash-atpt)
(defun ar-ip-backslash-atpt (&optional no-delimiters)
  "Backslash IP at point if any. "
  (interactive "*p")
  (ar-th-backslash 'ip no-delimiters (interactive-p)))

(defalias 'ar-brace-ip-atpt 'ar-ip-brace-atpt)
(defun ar-ip-brace-atpt (&optional no-delimiters)
  "Braces IP at point if any. "
  (interactive "*p")
  (ar-th-brace 'ip no-delimiters (interactive-p)))

(defalias 'ar-bracket-ip-atpt 'ar-ip-bracket-atpt)
(defun ar-ip-bracket-atpt (&optional no-delimiters)
  "Brackets IP after point if any. "
  (interactive "*p")
  (ar-th-bracket 'ip no-delimiters (interactive-p)))

(defun ar-comment-ip-atpt (&optional no-delimiters)
  "Comments IP at point if any. "
  (interactive "*p")
  (ar-th-comment 'ip no-delimiters (interactive-p)))

(defun ar-commatize-ip-atpt (&optional no-delimiters)
  "Put a comma after IP at point if any. "
  (interactive "*p")
  (ar-th-commatize 'ip no-delimiters (interactive-p)))

(defun ar-quote-ip-atpt (&optional no-delimiters)
  "Put a singlequote before IP at point if any. "
  (interactive "*p")
  (ar-th-quote 'ip no-delimiters (interactive-p)))

(defalias 'ar-hyphen-ip-atpt 'ar-ip-hyphen-atpt)
(defun ar-ip-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around IP at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'ip no-delimiters (interactive-p)))

(defalias 'ar-mark-ip-atpt 'ar-ip-mark-atpt)
(defun ar-ip-mark-atpt ()
  "Marks IP at point if any. "
  (interactive)
  (ar-th-mark 'ip))

(defalias 'ar-hide-ip-atpt 'ar-ip-hide-atpt)
(defun ar-ip-hide-atpt ()
  "Hides IP at point. "
  (interactive)
  (ar-th-hide 'ip))

(defalias 'ar-show-ip-atpt 'ar-ip-show-atpt)
(defun ar-ip-show-atpt ()
  "Shows hidden IP at point. "
  (interactive)
  (ar-th-show 'ip))

(defalias 'ar-hide-show-ip-atpt 'ar-ip-hide-show-atpt)
(defun ar-ip-hide-show-atpt ()
  "Alternatively hides or shows IP at point. "
  (interactive)
  (ar-th-hide-show 'ip))

(defalias 'ar-highlight-ip-atpt-mode 'ar-ip-highlight-atpt-mode)

(defun ar-ip-highlight-atpt-mode (&optional no-delimiters)
  "Toggles ip-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'ip no-delimiters (interactive-p)))

(defalias 'ar-kill-ip-atpt 'ar-ip-kill-atpt)
(defun ar-ip-kill-atpt (&optional no-delimiters)
  "Kills IP at point if any. "
  (interactive "*P")
  (ar-th-kill 'ip no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-ip-atpt 'ar-ip-kill-backward-atpt)
(defun ar-ip-kill-backward-atpt (&optional no-delimiters)
  "Kills IP at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'ip no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-ip-atpt 'ar-ip-left-right-singlequote-atpt)
(defun ar-ip-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'ip no-delimiters (interactive-p)))

(defalias 'ar-parentize-ip-atpt 'ar-ip-parentize-atpt)
(defun ar-ip-parentize-atpt (&optional no-delimiters)
  "Parentizes IP at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'ip no-delimiters (interactive-p)))

(defalias 'ar-separate-ip-atpt 'ar-ip-separate-atpt)
(defun ar-ip-separate-atpt (&optional no-delimiters)
  "Separates IP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'ip no-delimiters (interactive-p)))

(defalias 'ar-singlequote-ip-atpt 'ar-ip-singlequote-atpt)
(defun ar-ip-singlequote-atpt (&optional no-delimiters)
  "Singlequotes IP at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'ip no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-ip-atpt 'ar-ip-triplequote-dq-atpt)
(defun ar-ip-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around ip. "
  (interactive "*p")
  (ar-th-triplequote-dq 'ip no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-ip-atpt 'ar-ip-triplequote-sq-atpt)
(defun ar-ip-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around ip. "
  (interactive "*p")
  (ar-th-triplequote-sq 'ip no-delimiters (interactive-p)))

(defalias 'ar-trim-ip-atpt 'ar-ip-trim-atpt)
(defun ar-ip-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'ip t t))
    (ar-th-trim 'ip t t)))

(defalias 'ar-trim-left-ip-atpt 'ar-ip-left-trim-atpt)
(defun ar-ip-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'ip t nil))
    (ar-th-trim 'ip t nil)))

(defalias 'ar-trim-right-ip-atpt 'ar-ip-right-trim-atpt)
(defun ar-ip-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'ip nil t))
    (ar-th-trim 'ip nil t)))

(defalias 'ar-ip-underscore-atpt 'ar-underscore-ip-atpt)
(defun ar-underscore-ip-atpt (&optional no-delimiters)
  "Put underscore char around IP. "
  (interactive "*p")
  (ar-th-underscore 'ip no-delimiters (interactive-p)))

(defalias 'ar-ip-whitespace-atpt 'ar-whitespace-ip-atpt)
(defun ar-whitespace-ip-atpt (&optional no-delimiters)
  "Put whitespace char around IP. "
  (interactive "*p")
  (ar-th-whitespace 'ip nil t))

(defalias 'ar-forward-ip-atpt 'ar-ip-forward-atpt)
(defun ar-ip-forward-atpt (&optional arg)
  "Moves forward over IP at point if any, does nothing otherwise.
Returns end position of IP "
  (interactive "p")
  (ar-th-forward 'ip arg (interactive-p)))

(defalias 'ar-backward-ip-atpt 'ar-ip-backward-atpt)
(defun ar-ip-backward-atpt (&optional arg)
  "Moves backward over IP before point if any, does nothing otherwise.
Returns beginning position of IP "
  (interactive "p")
  (ar-th-backward 'ip arg (interactive-p)))

(defalias 'ar-transpose-ip-atpt 'ar-ip-transpose-atpt)
(defun ar-ip-transpose-atpt (&optional arg)
  "Transposes IP with IP before point if any. "
  (interactive "*p")
  (ar-th-transpose 'ip arg (interactive-p)))

(defalias 'ar-sort-ip-atpt 'ar-ip-sort-atpt)
(defun ar-ip-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts ips in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'ip reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-ip-atpt 'ar-ip-check-atpt)
(defun ar-ip-check-atpt ()
  "Return t if a IP at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-ip-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-ip-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-isbn-atpt (&optional arg no-delimiters)
  "Returns isbn at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'isbn arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-isbn-atpt 'ar-isbn-bounds-atpt)
(defun ar-isbn-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of isbn if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn no-delimiters (interactive-p)))

(defun ar-isbn-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position ISBN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'isbn no-delimiters (interactive-p)))

(defun ar-isbn-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'isbn no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-isbn-atpt 'ar-isbn-beginning-atpt)
(defun ar-isbn-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'isbn no-delimiters (interactive-p)))

(defalias 'ar-end-of-isbn-atpt 'ar-isbn-end-atpt)
(defun ar-isbn-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'isbn no-delimiters (interactive-p)))

(defalias 'ar-in-isbn-p-atpt 'ar-isbn-in-p-atpt)
(defun ar-isbn-in-p-atpt (&optional no-delimiters)
  "Returns bounds of ISBN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn no-delimiters (interactive-p)))

(defalias 'ar-length-of-isbn-atpt 'ar-isbn-length-atpt)
(defun ar-isbn-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'isbn no-delimiters (interactive-p)))

(defalias 'ar-copy-isbn-atpt 'ar-isbn-copy-atpt)
(defun ar-isbn-copy-atpt (&optional no-delimiters)
  "Returns a copy of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'isbn no-delimiters (interactive-p)))

(defalias 'ar-delete-isbn-in-region 'ar-isbn-delete-in-region)
(defun ar-isbn-delete-in-region (beg end)
  "Deletes ISBN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'isbn beg end (interactive-p)))

(defun ar-blok-isbn-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around isbn.
  Returns blok or nil if no ISBN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'isbn no-delimiters (interactive-p)))

(defalias 'ar-escape-isbn-atpt 'ar-isbn-escape-atpt)
(defun ar-isbn-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted ISBN at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'isbn no-delimiters))

(defalias 'ar-doublequote-isbn-atpt 'ar-isbn-doublequote-atpt)
(defun ar-isbn-doublequote-atpt (&optional no-delimiters)
  "Doublequotes ISBN at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'isbn no-delimiters (interactive-p)))

(defalias 'ar-slash-isbn-atpt 'ar-isbn-slash-atpt)
(defun ar-isbn-slash-atpt (&optional no-delimiters)
  "Doublequotes ISBN at point if any. "
  (interactive "*p")
  (ar-th-slash 'isbn no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-isbn-atpt 'ar-isbn-double-backslash-atpt)
(defun ar-isbn-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around ISBN at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'isbn no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-isbn-atpt 'ar-isbn-doubleslash-atpt)
(defun ar-isbn-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around ISBN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'isbn no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-isbn-atpt 'ar-isbn-doubleslash-paren-atpt)
(defun ar-isbn-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around ISBN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'isbn no-delimiters (interactive-p)))

(defalias 'ar-slashparen-isbn-atpt 'ar-isbn-slashparen-atpt)
(defun ar-isbn-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around ISBN at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'isbn no-delimiters (interactive-p)))

(defalias 'ar-dollar-isbn-atpt 'ar-isbn-dollar-atpt)
(defun ar-isbn-dollar-atpt (&optional no-delimiters)
  "Doublequotes ISBN at point if any. "
  (interactive "*p")
  (ar-th-dollar 'isbn no-delimiters (interactive-p)))

(defalias 'ar-equalize-isbn-atpt 'ar-isbn-equalize-atpt)
(defun ar-isbn-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around ISBN at point if any. "
  (interactive "*p")
  (ar-th-equalize 'isbn no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-isbn-atpt 'ar-isbn-greater-angle-atpt)
(defun ar-isbn-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for ISBN after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'isbn no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-isbn-atpt 'ar-isbn-lesser-angle-atpt)
(defun ar-isbn-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for ISBN after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'isbn no-delimiters (interactive-p)))

(defalias 'ar-backslash-isbn-atpt 'ar-isbn-backslash-atpt)
(defun ar-isbn-backslash-atpt (&optional no-delimiters)
  "Backslash ISBN at point if any. "
  (interactive "*p")
  (ar-th-backslash 'isbn no-delimiters (interactive-p)))

(defalias 'ar-brace-isbn-atpt 'ar-isbn-brace-atpt)
(defun ar-isbn-brace-atpt (&optional no-delimiters)
  "Braces ISBN at point if any. "
  (interactive "*p")
  (ar-th-brace 'isbn no-delimiters (interactive-p)))

(defalias 'ar-bracket-isbn-atpt 'ar-isbn-bracket-atpt)
(defun ar-isbn-bracket-atpt (&optional no-delimiters)
  "Brackets ISBN after point if any. "
  (interactive "*p")
  (ar-th-bracket 'isbn no-delimiters (interactive-p)))

(defun ar-comment-isbn-atpt (&optional no-delimiters)
  "Comments ISBN at point if any. "
  (interactive "*p")
  (ar-th-comment 'isbn no-delimiters (interactive-p)))

(defun ar-commatize-isbn-atpt (&optional no-delimiters)
  "Put a comma after ISBN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'isbn no-delimiters (interactive-p)))

(defun ar-quote-isbn-atpt (&optional no-delimiters)
  "Put a singlequote before ISBN at point if any. "
  (interactive "*p")
  (ar-th-quote 'isbn no-delimiters (interactive-p)))

(defalias 'ar-hyphen-isbn-atpt 'ar-isbn-hyphen-atpt)
(defun ar-isbn-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around ISBN at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'isbn no-delimiters (interactive-p)))

(defalias 'ar-mark-isbn-atpt 'ar-isbn-mark-atpt)
(defun ar-isbn-mark-atpt ()
  "Marks ISBN at point if any. "
  (interactive)
  (ar-th-mark 'isbn))

(defalias 'ar-hide-isbn-atpt 'ar-isbn-hide-atpt)
(defun ar-isbn-hide-atpt ()
  "Hides ISBN at point. "
  (interactive)
  (ar-th-hide 'isbn))

(defalias 'ar-show-isbn-atpt 'ar-isbn-show-atpt)
(defun ar-isbn-show-atpt ()
  "Shows hidden ISBN at point. "
  (interactive)
  (ar-th-show 'isbn))

(defalias 'ar-hide-show-isbn-atpt 'ar-isbn-hide-show-atpt)
(defun ar-isbn-hide-show-atpt ()
  "Alternatively hides or shows ISBN at point. "
  (interactive)
  (ar-th-hide-show 'isbn))

(defalias 'ar-highlight-isbn-atpt-mode 'ar-isbn-highlight-atpt-mode)

(defun ar-isbn-highlight-atpt-mode (&optional no-delimiters)
  "Toggles isbn-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'isbn no-delimiters (interactive-p)))

(defalias 'ar-kill-isbn-atpt 'ar-isbn-kill-atpt)
(defun ar-isbn-kill-atpt (&optional no-delimiters)
  "Kills ISBN at point if any. "
  (interactive "*P")
  (ar-th-kill 'isbn no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-isbn-atpt 'ar-isbn-kill-backward-atpt)
(defun ar-isbn-kill-backward-atpt (&optional no-delimiters)
  "Kills ISBN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'isbn no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-isbn-atpt 'ar-isbn-left-right-singlequote-atpt)
(defun ar-isbn-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'isbn no-delimiters (interactive-p)))

(defalias 'ar-parentize-isbn-atpt 'ar-isbn-parentize-atpt)
(defun ar-isbn-parentize-atpt (&optional no-delimiters)
  "Parentizes ISBN at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'isbn no-delimiters (interactive-p)))

(defalias 'ar-separate-isbn-atpt 'ar-isbn-separate-atpt)
(defun ar-isbn-separate-atpt (&optional no-delimiters)
  "Separates ISBN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'isbn no-delimiters (interactive-p)))

(defalias 'ar-singlequote-isbn-atpt 'ar-isbn-singlequote-atpt)
(defun ar-isbn-singlequote-atpt (&optional no-delimiters)
  "Singlequotes ISBN at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'isbn no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-isbn-atpt 'ar-isbn-triplequote-dq-atpt)
(defun ar-isbn-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around isbn. "
  (interactive "*p")
  (ar-th-triplequote-dq 'isbn no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-isbn-atpt 'ar-isbn-triplequote-sq-atpt)
(defun ar-isbn-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around isbn. "
  (interactive "*p")
  (ar-th-triplequote-sq 'isbn no-delimiters (interactive-p)))

(defalias 'ar-trim-isbn-atpt 'ar-isbn-trim-atpt)
(defun ar-isbn-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'isbn t t))
    (ar-th-trim 'isbn t t)))

(defalias 'ar-trim-left-isbn-atpt 'ar-isbn-left-trim-atpt)
(defun ar-isbn-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'isbn t nil))
    (ar-th-trim 'isbn t nil)))

(defalias 'ar-trim-right-isbn-atpt 'ar-isbn-right-trim-atpt)
(defun ar-isbn-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'isbn nil t))
    (ar-th-trim 'isbn nil t)))

(defalias 'ar-isbn-underscore-atpt 'ar-underscore-isbn-atpt)
(defun ar-underscore-isbn-atpt (&optional no-delimiters)
  "Put underscore char around ISBN. "
  (interactive "*p")
  (ar-th-underscore 'isbn no-delimiters (interactive-p)))

(defalias 'ar-isbn-whitespace-atpt 'ar-whitespace-isbn-atpt)
(defun ar-whitespace-isbn-atpt (&optional no-delimiters)
  "Put whitespace char around ISBN. "
  (interactive "*p")
  (ar-th-whitespace 'isbn nil t))

(defalias 'ar-forward-isbn-atpt 'ar-isbn-forward-atpt)
(defun ar-isbn-forward-atpt (&optional arg)
  "Moves forward over ISBN at point if any, does nothing otherwise.
Returns end position of ISBN "
  (interactive "p")
  (ar-th-forward 'isbn arg (interactive-p)))

(defalias 'ar-backward-isbn-atpt 'ar-isbn-backward-atpt)
(defun ar-isbn-backward-atpt (&optional arg)
  "Moves backward over ISBN before point if any, does nothing otherwise.
Returns beginning position of ISBN "
  (interactive "p")
  (ar-th-backward 'isbn arg (interactive-p)))

(defalias 'ar-transpose-isbn-atpt 'ar-isbn-transpose-atpt)
(defun ar-isbn-transpose-atpt (&optional arg)
  "Transposes ISBN with ISBN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'isbn arg (interactive-p)))

(defalias 'ar-sort-isbn-atpt 'ar-isbn-sort-atpt)
(defun ar-isbn-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts isbns in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'isbn reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-isbn-atpt 'ar-isbn-check-atpt)
(defun ar-isbn-check-atpt ()
  "Return t if a ISBN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-isbn-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-isbn-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-line-atpt (&optional arg no-delimiters)
  "Returns line at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'line arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-line-atpt 'ar-line-bounds-atpt)
(defun ar-line-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of line if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line no-delimiters (interactive-p)))

(defun ar-line-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LINE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'line no-delimiters (interactive-p)))

(defun ar-line-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'line no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-line-atpt 'ar-line-beginning-atpt)
(defun ar-line-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'line no-delimiters (interactive-p)))

(defalias 'ar-end-of-line-atpt 'ar-line-end-atpt)
(defun ar-line-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'line no-delimiters (interactive-p)))

(defalias 'ar-in-line-p-atpt 'ar-line-in-p-atpt)
(defun ar-line-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LINE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line no-delimiters (interactive-p)))

(defalias 'ar-length-of-line-atpt 'ar-line-length-atpt)
(defun ar-line-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'line no-delimiters (interactive-p)))

(defalias 'ar-copy-line-atpt 'ar-line-copy-atpt)
(defun ar-line-copy-atpt (&optional no-delimiters)
  "Returns a copy of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'line no-delimiters (interactive-p)))

(defalias 'ar-delete-line-in-region 'ar-line-delete-in-region)
(defun ar-line-delete-in-region (beg end)
  "Deletes LINE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'line beg end (interactive-p)))

(defun ar-blok-line-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around line.
  Returns blok or nil if no LINE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'line no-delimiters (interactive-p)))

(defalias 'ar-escape-line-atpt 'ar-line-escape-atpt)
(defun ar-line-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted LINE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'line no-delimiters))

(defalias 'ar-doublequote-line-atpt 'ar-line-doublequote-atpt)
(defun ar-line-doublequote-atpt (&optional no-delimiters)
  "Doublequotes LINE at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'line no-delimiters (interactive-p)))

(defalias 'ar-slash-line-atpt 'ar-line-slash-atpt)
(defun ar-line-slash-atpt (&optional no-delimiters)
  "Doublequotes LINE at point if any. "
  (interactive "*p")
  (ar-th-slash 'line no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-line-atpt 'ar-line-double-backslash-atpt)
(defun ar-line-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LINE at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'line no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-line-atpt 'ar-line-doubleslash-atpt)
(defun ar-line-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around LINE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'line no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-line-atpt 'ar-line-doubleslash-paren-atpt)
(defun ar-line-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LINE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'line no-delimiters (interactive-p)))

(defalias 'ar-slashparen-line-atpt 'ar-line-slashparen-atpt)
(defun ar-line-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around LINE at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'line no-delimiters (interactive-p)))

(defalias 'ar-dollar-line-atpt 'ar-line-dollar-atpt)
(defun ar-line-dollar-atpt (&optional no-delimiters)
  "Doublequotes LINE at point if any. "
  (interactive "*p")
  (ar-th-dollar 'line no-delimiters (interactive-p)))

(defalias 'ar-equalize-line-atpt 'ar-line-equalize-atpt)
(defun ar-line-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around LINE at point if any. "
  (interactive "*p")
  (ar-th-equalize 'line no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-line-atpt 'ar-line-greater-angle-atpt)
(defun ar-line-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for LINE after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'line no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-line-atpt 'ar-line-lesser-angle-atpt)
(defun ar-line-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for LINE after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'line no-delimiters (interactive-p)))

(defalias 'ar-backslash-line-atpt 'ar-line-backslash-atpt)
(defun ar-line-backslash-atpt (&optional no-delimiters)
  "Backslash LINE at point if any. "
  (interactive "*p")
  (ar-th-backslash 'line no-delimiters (interactive-p)))

(defalias 'ar-brace-line-atpt 'ar-line-brace-atpt)
(defun ar-line-brace-atpt (&optional no-delimiters)
  "Braces LINE at point if any. "
  (interactive "*p")
  (ar-th-brace 'line no-delimiters (interactive-p)))

(defalias 'ar-bracket-line-atpt 'ar-line-bracket-atpt)
(defun ar-line-bracket-atpt (&optional no-delimiters)
  "Brackets LINE after point if any. "
  (interactive "*p")
  (ar-th-bracket 'line no-delimiters (interactive-p)))

(defun ar-comment-line-atpt (&optional no-delimiters)
  "Comments LINE at point if any. "
  (interactive "*p")
  (ar-th-comment 'line no-delimiters (interactive-p)))

(defun ar-commatize-line-atpt (&optional no-delimiters)
  "Put a comma after LINE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'line no-delimiters (interactive-p)))

(defun ar-quote-line-atpt (&optional no-delimiters)
  "Put a singlequote before LINE at point if any. "
  (interactive "*p")
  (ar-th-quote 'line no-delimiters (interactive-p)))

(defalias 'ar-hyphen-line-atpt 'ar-line-hyphen-atpt)
(defun ar-line-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around LINE at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'line no-delimiters (interactive-p)))

(defalias 'ar-mark-line-atpt 'ar-line-mark-atpt)
(defun ar-line-mark-atpt ()
  "Marks LINE at point if any. "
  (interactive)
  (ar-th-mark 'line))

(defalias 'ar-hide-line-atpt 'ar-line-hide-atpt)
(defun ar-line-hide-atpt ()
  "Hides LINE at point. "
  (interactive)
  (ar-th-hide 'line))

(defalias 'ar-show-line-atpt 'ar-line-show-atpt)
(defun ar-line-show-atpt ()
  "Shows hidden LINE at point. "
  (interactive)
  (ar-th-show 'line))

(defalias 'ar-hide-show-line-atpt 'ar-line-hide-show-atpt)
(defun ar-line-hide-show-atpt ()
  "Alternatively hides or shows LINE at point. "
  (interactive)
  (ar-th-hide-show 'line))

(defalias 'ar-highlight-line-atpt-mode 'ar-line-highlight-atpt-mode)

(defun ar-line-highlight-atpt-mode (&optional no-delimiters)
  "Toggles line-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'line no-delimiters (interactive-p)))

(defalias 'ar-kill-line-atpt 'ar-line-kill-atpt)
(defun ar-line-kill-atpt (&optional no-delimiters)
  "Kills LINE at point if any. "
  (interactive "*P")
  (ar-th-kill 'line no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-line-atpt 'ar-line-kill-backward-atpt)
(defun ar-line-kill-backward-atpt (&optional no-delimiters)
  "Kills LINE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'line no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-line-atpt 'ar-line-left-right-singlequote-atpt)
(defun ar-line-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'line no-delimiters (interactive-p)))

(defalias 'ar-parentize-line-atpt 'ar-line-parentize-atpt)
(defun ar-line-parentize-atpt (&optional no-delimiters)
  "Parentizes LINE at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'line no-delimiters (interactive-p)))

(defalias 'ar-separate-line-atpt 'ar-line-separate-atpt)
(defun ar-line-separate-atpt (&optional no-delimiters)
  "Separates LINE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'line no-delimiters (interactive-p)))

(defalias 'ar-singlequote-line-atpt 'ar-line-singlequote-atpt)
(defun ar-line-singlequote-atpt (&optional no-delimiters)
  "Singlequotes LINE at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'line no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-line-atpt 'ar-line-triplequote-dq-atpt)
(defun ar-line-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around line. "
  (interactive "*p")
  (ar-th-triplequote-dq 'line no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-line-atpt 'ar-line-triplequote-sq-atpt)
(defun ar-line-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around line. "
  (interactive "*p")
  (ar-th-triplequote-sq 'line no-delimiters (interactive-p)))

(defalias 'ar-trim-line-atpt 'ar-line-trim-atpt)
(defun ar-line-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'line t t))
    (ar-th-trim 'line t t)))

(defalias 'ar-trim-left-line-atpt 'ar-line-left-trim-atpt)
(defun ar-line-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'line t nil))
    (ar-th-trim 'line t nil)))

(defalias 'ar-trim-right-line-atpt 'ar-line-right-trim-atpt)
(defun ar-line-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'line nil t))
    (ar-th-trim 'line nil t)))

(defalias 'ar-line-underscore-atpt 'ar-underscore-line-atpt)
(defun ar-underscore-line-atpt (&optional no-delimiters)
  "Put underscore char around LINE. "
  (interactive "*p")
  (ar-th-underscore 'line no-delimiters (interactive-p)))

(defalias 'ar-line-whitespace-atpt 'ar-whitespace-line-atpt)
(defun ar-whitespace-line-atpt (&optional no-delimiters)
  "Put whitespace char around LINE. "
  (interactive "*p")
  (ar-th-whitespace 'line nil t))

(defalias 'ar-forward-line-atpt 'ar-line-forward-atpt)
(defun ar-line-forward-atpt (&optional arg)
  "Moves forward over LINE at point if any, does nothing otherwise.
Returns end position of LINE "
  (interactive "p")
  (ar-th-forward 'line arg (interactive-p)))

(defalias 'ar-backward-line-atpt 'ar-line-backward-atpt)
(defun ar-line-backward-atpt (&optional arg)
  "Moves backward over LINE before point if any, does nothing otherwise.
Returns beginning position of LINE "
  (interactive "p")
  (ar-th-backward 'line arg (interactive-p)))

(defalias 'ar-transpose-line-atpt 'ar-line-transpose-atpt)
(defun ar-line-transpose-atpt (&optional arg)
  "Transposes LINE with LINE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'line arg (interactive-p)))

(defalias 'ar-sort-line-atpt 'ar-line-sort-atpt)
(defun ar-line-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lines in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'line reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-line-atpt 'ar-line-check-atpt)
(defun ar-line-check-atpt ()
  "Return t if a LINE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-line-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-line-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-name-atpt (&optional arg no-delimiters)
  "Returns name at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'name arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-name-atpt 'ar-name-bounds-atpt)
(defun ar-name-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of name if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name no-delimiters (interactive-p)))

(defun ar-name-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position NAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'name no-delimiters (interactive-p)))

(defun ar-name-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'name no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-name-atpt 'ar-name-beginning-atpt)
(defun ar-name-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'name no-delimiters (interactive-p)))

(defalias 'ar-end-of-name-atpt 'ar-name-end-atpt)
(defun ar-name-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'name no-delimiters (interactive-p)))

(defalias 'ar-in-name-p-atpt 'ar-name-in-p-atpt)
(defun ar-name-in-p-atpt (&optional no-delimiters)
  "Returns bounds of NAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name no-delimiters (interactive-p)))

(defalias 'ar-length-of-name-atpt 'ar-name-length-atpt)
(defun ar-name-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'name no-delimiters (interactive-p)))

(defalias 'ar-copy-name-atpt 'ar-name-copy-atpt)
(defun ar-name-copy-atpt (&optional no-delimiters)
  "Returns a copy of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'name no-delimiters (interactive-p)))

(defalias 'ar-delete-name-in-region 'ar-name-delete-in-region)
(defun ar-name-delete-in-region (beg end)
  "Deletes NAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'name beg end (interactive-p)))

(defun ar-blok-name-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around name.
  Returns blok or nil if no NAME at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'name no-delimiters (interactive-p)))

(defalias 'ar-escape-name-atpt 'ar-name-escape-atpt)
(defun ar-name-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted NAME at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'name no-delimiters))

(defalias 'ar-doublequote-name-atpt 'ar-name-doublequote-atpt)
(defun ar-name-doublequote-atpt (&optional no-delimiters)
  "Doublequotes NAME at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'name no-delimiters (interactive-p)))

(defalias 'ar-slash-name-atpt 'ar-name-slash-atpt)
(defun ar-name-slash-atpt (&optional no-delimiters)
  "Doublequotes NAME at point if any. "
  (interactive "*p")
  (ar-th-slash 'name no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-name-atpt 'ar-name-double-backslash-atpt)
(defun ar-name-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around NAME at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'name no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-name-atpt 'ar-name-doubleslash-atpt)
(defun ar-name-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around NAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'name no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-name-atpt 'ar-name-doubleslash-paren-atpt)
(defun ar-name-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around NAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'name no-delimiters (interactive-p)))

(defalias 'ar-slashparen-name-atpt 'ar-name-slashparen-atpt)
(defun ar-name-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around NAME at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'name no-delimiters (interactive-p)))

(defalias 'ar-dollar-name-atpt 'ar-name-dollar-atpt)
(defun ar-name-dollar-atpt (&optional no-delimiters)
  "Doublequotes NAME at point if any. "
  (interactive "*p")
  (ar-th-dollar 'name no-delimiters (interactive-p)))

(defalias 'ar-equalize-name-atpt 'ar-name-equalize-atpt)
(defun ar-name-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around NAME at point if any. "
  (interactive "*p")
  (ar-th-equalize 'name no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-name-atpt 'ar-name-greater-angle-atpt)
(defun ar-name-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for NAME after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'name no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-name-atpt 'ar-name-lesser-angle-atpt)
(defun ar-name-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for NAME after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'name no-delimiters (interactive-p)))

(defalias 'ar-backslash-name-atpt 'ar-name-backslash-atpt)
(defun ar-name-backslash-atpt (&optional no-delimiters)
  "Backslash NAME at point if any. "
  (interactive "*p")
  (ar-th-backslash 'name no-delimiters (interactive-p)))

(defalias 'ar-brace-name-atpt 'ar-name-brace-atpt)
(defun ar-name-brace-atpt (&optional no-delimiters)
  "Braces NAME at point if any. "
  (interactive "*p")
  (ar-th-brace 'name no-delimiters (interactive-p)))

(defalias 'ar-bracket-name-atpt 'ar-name-bracket-atpt)
(defun ar-name-bracket-atpt (&optional no-delimiters)
  "Brackets NAME after point if any. "
  (interactive "*p")
  (ar-th-bracket 'name no-delimiters (interactive-p)))

(defun ar-comment-name-atpt (&optional no-delimiters)
  "Comments NAME at point if any. "
  (interactive "*p")
  (ar-th-comment 'name no-delimiters (interactive-p)))

(defun ar-commatize-name-atpt (&optional no-delimiters)
  "Put a comma after NAME at point if any. "
  (interactive "*p")
  (ar-th-commatize 'name no-delimiters (interactive-p)))

(defun ar-quote-name-atpt (&optional no-delimiters)
  "Put a singlequote before NAME at point if any. "
  (interactive "*p")
  (ar-th-quote 'name no-delimiters (interactive-p)))

(defalias 'ar-hyphen-name-atpt 'ar-name-hyphen-atpt)
(defun ar-name-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around NAME at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'name no-delimiters (interactive-p)))

(defalias 'ar-mark-name-atpt 'ar-name-mark-atpt)
(defun ar-name-mark-atpt ()
  "Marks NAME at point if any. "
  (interactive)
  (ar-th-mark 'name))

(defalias 'ar-hide-name-atpt 'ar-name-hide-atpt)
(defun ar-name-hide-atpt ()
  "Hides NAME at point. "
  (interactive)
  (ar-th-hide 'name))

(defalias 'ar-show-name-atpt 'ar-name-show-atpt)
(defun ar-name-show-atpt ()
  "Shows hidden NAME at point. "
  (interactive)
  (ar-th-show 'name))

(defalias 'ar-hide-show-name-atpt 'ar-name-hide-show-atpt)
(defun ar-name-hide-show-atpt ()
  "Alternatively hides or shows NAME at point. "
  (interactive)
  (ar-th-hide-show 'name))

(defalias 'ar-highlight-name-atpt-mode 'ar-name-highlight-atpt-mode)

(defun ar-name-highlight-atpt-mode (&optional no-delimiters)
  "Toggles name-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'name no-delimiters (interactive-p)))

(defalias 'ar-kill-name-atpt 'ar-name-kill-atpt)
(defun ar-name-kill-atpt (&optional no-delimiters)
  "Kills NAME at point if any. "
  (interactive "*P")
  (ar-th-kill 'name no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-name-atpt 'ar-name-kill-backward-atpt)
(defun ar-name-kill-backward-atpt (&optional no-delimiters)
  "Kills NAME at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'name no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-name-atpt 'ar-name-left-right-singlequote-atpt)
(defun ar-name-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'name no-delimiters (interactive-p)))

(defalias 'ar-parentize-name-atpt 'ar-name-parentize-atpt)
(defun ar-name-parentize-atpt (&optional no-delimiters)
  "Parentizes NAME at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'name no-delimiters (interactive-p)))

(defalias 'ar-separate-name-atpt 'ar-name-separate-atpt)
(defun ar-name-separate-atpt (&optional no-delimiters)
  "Separates NAME at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'name no-delimiters (interactive-p)))

(defalias 'ar-singlequote-name-atpt 'ar-name-singlequote-atpt)
(defun ar-name-singlequote-atpt (&optional no-delimiters)
  "Singlequotes NAME at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'name no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-name-atpt 'ar-name-triplequote-dq-atpt)
(defun ar-name-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around name. "
  (interactive "*p")
  (ar-th-triplequote-dq 'name no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-name-atpt 'ar-name-triplequote-sq-atpt)
(defun ar-name-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around name. "
  (interactive "*p")
  (ar-th-triplequote-sq 'name no-delimiters (interactive-p)))

(defalias 'ar-trim-name-atpt 'ar-name-trim-atpt)
(defun ar-name-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'name t t))
    (ar-th-trim 'name t t)))

(defalias 'ar-trim-left-name-atpt 'ar-name-left-trim-atpt)
(defun ar-name-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'name t nil))
    (ar-th-trim 'name t nil)))

(defalias 'ar-trim-right-name-atpt 'ar-name-right-trim-atpt)
(defun ar-name-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'name nil t))
    (ar-th-trim 'name nil t)))

(defalias 'ar-name-underscore-atpt 'ar-underscore-name-atpt)
(defun ar-underscore-name-atpt (&optional no-delimiters)
  "Put underscore char around NAME. "
  (interactive "*p")
  (ar-th-underscore 'name no-delimiters (interactive-p)))

(defalias 'ar-name-whitespace-atpt 'ar-whitespace-name-atpt)
(defun ar-whitespace-name-atpt (&optional no-delimiters)
  "Put whitespace char around NAME. "
  (interactive "*p")
  (ar-th-whitespace 'name nil t))

(defalias 'ar-forward-name-atpt 'ar-name-forward-atpt)
(defun ar-name-forward-atpt (&optional arg)
  "Moves forward over NAME at point if any, does nothing otherwise.
Returns end position of NAME "
  (interactive "p")
  (ar-th-forward 'name arg (interactive-p)))

(defalias 'ar-backward-name-atpt 'ar-name-backward-atpt)
(defun ar-name-backward-atpt (&optional arg)
  "Moves backward over NAME before point if any, does nothing otherwise.
Returns beginning position of NAME "
  (interactive "p")
  (ar-th-backward 'name arg (interactive-p)))

(defalias 'ar-transpose-name-atpt 'ar-name-transpose-atpt)
(defun ar-name-transpose-atpt (&optional arg)
  "Transposes NAME with NAME before point if any. "
  (interactive "*p")
  (ar-th-transpose 'name arg (interactive-p)))

(defalias 'ar-sort-name-atpt 'ar-name-sort-atpt)
(defun ar-name-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts names in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'name reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-name-atpt 'ar-name-check-atpt)
(defun ar-name-check-atpt ()
  "Return t if a NAME at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-name-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-name-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-number-atpt (&optional arg no-delimiters)
  "Returns number at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'number arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-number-atpt 'ar-number-bounds-atpt)
(defun ar-number-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of number if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number no-delimiters (interactive-p)))

(defun ar-number-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position NUMBER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'number no-delimiters (interactive-p)))

(defun ar-number-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'number no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-number-atpt 'ar-number-beginning-atpt)
(defun ar-number-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'number no-delimiters (interactive-p)))

(defalias 'ar-end-of-number-atpt 'ar-number-end-atpt)
(defun ar-number-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'number no-delimiters (interactive-p)))

(defalias 'ar-in-number-p-atpt 'ar-number-in-p-atpt)
(defun ar-number-in-p-atpt (&optional no-delimiters)
  "Returns bounds of NUMBER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number no-delimiters (interactive-p)))

(defalias 'ar-length-of-number-atpt 'ar-number-length-atpt)
(defun ar-number-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'number no-delimiters (interactive-p)))

(defalias 'ar-copy-number-atpt 'ar-number-copy-atpt)
(defun ar-number-copy-atpt (&optional no-delimiters)
  "Returns a copy of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'number no-delimiters (interactive-p)))

(defalias 'ar-delete-number-in-region 'ar-number-delete-in-region)
(defun ar-number-delete-in-region (beg end)
  "Deletes NUMBER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'number beg end (interactive-p)))

(defun ar-blok-number-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around number.
  Returns blok or nil if no NUMBER at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'number no-delimiters (interactive-p)))

(defalias 'ar-escape-number-atpt 'ar-number-escape-atpt)
(defun ar-number-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted NUMBER at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'number no-delimiters))

(defalias 'ar-doublequote-number-atpt 'ar-number-doublequote-atpt)
(defun ar-number-doublequote-atpt (&optional no-delimiters)
  "Doublequotes NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'number no-delimiters (interactive-p)))

(defalias 'ar-slash-number-atpt 'ar-number-slash-atpt)
(defun ar-number-slash-atpt (&optional no-delimiters)
  "Doublequotes NUMBER at point if any. "
  (interactive "*p")
  (ar-th-slash 'number no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-number-atpt 'ar-number-double-backslash-atpt)
(defun ar-number-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'number no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-number-atpt 'ar-number-doubleslash-atpt)
(defun ar-number-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'number no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-number-atpt 'ar-number-doubleslash-paren-atpt)
(defun ar-number-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'number no-delimiters (interactive-p)))

(defalias 'ar-slashparen-number-atpt 'ar-number-slashparen-atpt)
(defun ar-number-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'number no-delimiters (interactive-p)))

(defalias 'ar-dollar-number-atpt 'ar-number-dollar-atpt)
(defun ar-number-dollar-atpt (&optional no-delimiters)
  "Doublequotes NUMBER at point if any. "
  (interactive "*p")
  (ar-th-dollar 'number no-delimiters (interactive-p)))

(defalias 'ar-equalize-number-atpt 'ar-number-equalize-atpt)
(defun ar-number-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-equalize 'number no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-number-atpt 'ar-number-greater-angle-atpt)
(defun ar-number-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for NUMBER after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'number no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-number-atpt 'ar-number-lesser-angle-atpt)
(defun ar-number-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for NUMBER after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'number no-delimiters (interactive-p)))

(defalias 'ar-backslash-number-atpt 'ar-number-backslash-atpt)
(defun ar-number-backslash-atpt (&optional no-delimiters)
  "Backslash NUMBER at point if any. "
  (interactive "*p")
  (ar-th-backslash 'number no-delimiters (interactive-p)))

(defalias 'ar-brace-number-atpt 'ar-number-brace-atpt)
(defun ar-number-brace-atpt (&optional no-delimiters)
  "Braces NUMBER at point if any. "
  (interactive "*p")
  (ar-th-brace 'number no-delimiters (interactive-p)))

(defalias 'ar-bracket-number-atpt 'ar-number-bracket-atpt)
(defun ar-number-bracket-atpt (&optional no-delimiters)
  "Brackets NUMBER after point if any. "
  (interactive "*p")
  (ar-th-bracket 'number no-delimiters (interactive-p)))

(defun ar-comment-number-atpt (&optional no-delimiters)
  "Comments NUMBER at point if any. "
  (interactive "*p")
  (ar-th-comment 'number no-delimiters (interactive-p)))

(defun ar-commatize-number-atpt (&optional no-delimiters)
  "Put a comma after NUMBER at point if any. "
  (interactive "*p")
  (ar-th-commatize 'number no-delimiters (interactive-p)))

(defun ar-quote-number-atpt (&optional no-delimiters)
  "Put a singlequote before NUMBER at point if any. "
  (interactive "*p")
  (ar-th-quote 'number no-delimiters (interactive-p)))

(defalias 'ar-hyphen-number-atpt 'ar-number-hyphen-atpt)
(defun ar-number-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'number no-delimiters (interactive-p)))

(defalias 'ar-mark-number-atpt 'ar-number-mark-atpt)
(defun ar-number-mark-atpt ()
  "Marks NUMBER at point if any. "
  (interactive)
  (ar-th-mark 'number))

(defalias 'ar-hide-number-atpt 'ar-number-hide-atpt)
(defun ar-number-hide-atpt ()
  "Hides NUMBER at point. "
  (interactive)
  (ar-th-hide 'number))

(defalias 'ar-show-number-atpt 'ar-number-show-atpt)
(defun ar-number-show-atpt ()
  "Shows hidden NUMBER at point. "
  (interactive)
  (ar-th-show 'number))

(defalias 'ar-hide-show-number-atpt 'ar-number-hide-show-atpt)
(defun ar-number-hide-show-atpt ()
  "Alternatively hides or shows NUMBER at point. "
  (interactive)
  (ar-th-hide-show 'number))

(defalias 'ar-highlight-number-atpt-mode 'ar-number-highlight-atpt-mode)

(defun ar-number-highlight-atpt-mode (&optional no-delimiters)
  "Toggles number-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'number no-delimiters (interactive-p)))

(defalias 'ar-kill-number-atpt 'ar-number-kill-atpt)
(defun ar-number-kill-atpt (&optional no-delimiters)
  "Kills NUMBER at point if any. "
  (interactive "*P")
  (ar-th-kill 'number no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-number-atpt 'ar-number-kill-backward-atpt)
(defun ar-number-kill-backward-atpt (&optional no-delimiters)
  "Kills NUMBER at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'number no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-number-atpt 'ar-number-left-right-singlequote-atpt)
(defun ar-number-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'number no-delimiters (interactive-p)))

(defalias 'ar-parentize-number-atpt 'ar-number-parentize-atpt)
(defun ar-number-parentize-atpt (&optional no-delimiters)
  "Parentizes NUMBER at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'number no-delimiters (interactive-p)))

(defalias 'ar-separate-number-atpt 'ar-number-separate-atpt)
(defun ar-number-separate-atpt (&optional no-delimiters)
  "Separates NUMBER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'number no-delimiters (interactive-p)))

(defalias 'ar-singlequote-number-atpt 'ar-number-singlequote-atpt)
(defun ar-number-singlequote-atpt (&optional no-delimiters)
  "Singlequotes NUMBER at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'number no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-number-atpt 'ar-number-triplequote-dq-atpt)
(defun ar-number-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around number. "
  (interactive "*p")
  (ar-th-triplequote-dq 'number no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-number-atpt 'ar-number-triplequote-sq-atpt)
(defun ar-number-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around number. "
  (interactive "*p")
  (ar-th-triplequote-sq 'number no-delimiters (interactive-p)))

(defalias 'ar-trim-number-atpt 'ar-number-trim-atpt)
(defun ar-number-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'number t t))
    (ar-th-trim 'number t t)))

(defalias 'ar-trim-left-number-atpt 'ar-number-left-trim-atpt)
(defun ar-number-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'number t nil))
    (ar-th-trim 'number t nil)))

(defalias 'ar-trim-right-number-atpt 'ar-number-right-trim-atpt)
(defun ar-number-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'number nil t))
    (ar-th-trim 'number nil t)))

(defalias 'ar-number-underscore-atpt 'ar-underscore-number-atpt)
(defun ar-underscore-number-atpt (&optional no-delimiters)
  "Put underscore char around NUMBER. "
  (interactive "*p")
  (ar-th-underscore 'number no-delimiters (interactive-p)))

(defalias 'ar-number-whitespace-atpt 'ar-whitespace-number-atpt)
(defun ar-whitespace-number-atpt (&optional no-delimiters)
  "Put whitespace char around NUMBER. "
  (interactive "*p")
  (ar-th-whitespace 'number nil t))

(defalias 'ar-forward-number-atpt 'ar-number-forward-atpt)
(defun ar-number-forward-atpt (&optional arg)
  "Moves forward over NUMBER at point if any, does nothing otherwise.
Returns end position of NUMBER "
  (interactive "p")
  (ar-th-forward 'number arg (interactive-p)))

(defalias 'ar-backward-number-atpt 'ar-number-backward-atpt)
(defun ar-number-backward-atpt (&optional arg)
  "Moves backward over NUMBER before point if any, does nothing otherwise.
Returns beginning position of NUMBER "
  (interactive "p")
  (ar-th-backward 'number arg (interactive-p)))

(defalias 'ar-transpose-number-atpt 'ar-number-transpose-atpt)
(defun ar-number-transpose-atpt (&optional arg)
  "Transposes NUMBER with NUMBER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'number arg (interactive-p)))

(defalias 'ar-sort-number-atpt 'ar-number-sort-atpt)
(defun ar-number-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts numbers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'number reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-number-atpt 'ar-number-check-atpt)
(defun ar-number-check-atpt ()
  "Return t if a NUMBER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-number-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-number-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-page-atpt (&optional arg no-delimiters)
  "Returns page at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'page arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-page-atpt 'ar-page-bounds-atpt)
(defun ar-page-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of page if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page no-delimiters (interactive-p)))

(defun ar-page-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PAGE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'page no-delimiters (interactive-p)))

(defun ar-page-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'page no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-page-atpt 'ar-page-beginning-atpt)
(defun ar-page-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'page no-delimiters (interactive-p)))

(defalias 'ar-end-of-page-atpt 'ar-page-end-atpt)
(defun ar-page-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'page no-delimiters (interactive-p)))

(defalias 'ar-in-page-p-atpt 'ar-page-in-p-atpt)
(defun ar-page-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PAGE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page no-delimiters (interactive-p)))

(defalias 'ar-length-of-page-atpt 'ar-page-length-atpt)
(defun ar-page-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'page no-delimiters (interactive-p)))

(defalias 'ar-copy-page-atpt 'ar-page-copy-atpt)
(defun ar-page-copy-atpt (&optional no-delimiters)
  "Returns a copy of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'page no-delimiters (interactive-p)))

(defalias 'ar-delete-page-in-region 'ar-page-delete-in-region)
(defun ar-page-delete-in-region (beg end)
  "Deletes PAGE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'page beg end (interactive-p)))

(defun ar-blok-page-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around page.
  Returns blok or nil if no PAGE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'page no-delimiters (interactive-p)))

(defalias 'ar-escape-page-atpt 'ar-page-escape-atpt)
(defun ar-page-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted PAGE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'page no-delimiters))

(defalias 'ar-doublequote-page-atpt 'ar-page-doublequote-atpt)
(defun ar-page-doublequote-atpt (&optional no-delimiters)
  "Doublequotes PAGE at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'page no-delimiters (interactive-p)))

(defalias 'ar-slash-page-atpt 'ar-page-slash-atpt)
(defun ar-page-slash-atpt (&optional no-delimiters)
  "Doublequotes PAGE at point if any. "
  (interactive "*p")
  (ar-th-slash 'page no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-page-atpt 'ar-page-double-backslash-atpt)
(defun ar-page-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PAGE at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'page no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-page-atpt 'ar-page-doubleslash-atpt)
(defun ar-page-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around PAGE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'page no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-page-atpt 'ar-page-doubleslash-paren-atpt)
(defun ar-page-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PAGE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'page no-delimiters (interactive-p)))

(defalias 'ar-slashparen-page-atpt 'ar-page-slashparen-atpt)
(defun ar-page-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around PAGE at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'page no-delimiters (interactive-p)))

(defalias 'ar-dollar-page-atpt 'ar-page-dollar-atpt)
(defun ar-page-dollar-atpt (&optional no-delimiters)
  "Doublequotes PAGE at point if any. "
  (interactive "*p")
  (ar-th-dollar 'page no-delimiters (interactive-p)))

(defalias 'ar-equalize-page-atpt 'ar-page-equalize-atpt)
(defun ar-page-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around PAGE at point if any. "
  (interactive "*p")
  (ar-th-equalize 'page no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-page-atpt 'ar-page-greater-angle-atpt)
(defun ar-page-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for PAGE after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'page no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-page-atpt 'ar-page-lesser-angle-atpt)
(defun ar-page-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for PAGE after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'page no-delimiters (interactive-p)))

(defalias 'ar-backslash-page-atpt 'ar-page-backslash-atpt)
(defun ar-page-backslash-atpt (&optional no-delimiters)
  "Backslash PAGE at point if any. "
  (interactive "*p")
  (ar-th-backslash 'page no-delimiters (interactive-p)))

(defalias 'ar-brace-page-atpt 'ar-page-brace-atpt)
(defun ar-page-brace-atpt (&optional no-delimiters)
  "Braces PAGE at point if any. "
  (interactive "*p")
  (ar-th-brace 'page no-delimiters (interactive-p)))

(defalias 'ar-bracket-page-atpt 'ar-page-bracket-atpt)
(defun ar-page-bracket-atpt (&optional no-delimiters)
  "Brackets PAGE after point if any. "
  (interactive "*p")
  (ar-th-bracket 'page no-delimiters (interactive-p)))

(defun ar-comment-page-atpt (&optional no-delimiters)
  "Comments PAGE at point if any. "
  (interactive "*p")
  (ar-th-comment 'page no-delimiters (interactive-p)))

(defun ar-commatize-page-atpt (&optional no-delimiters)
  "Put a comma after PAGE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'page no-delimiters (interactive-p)))

(defun ar-quote-page-atpt (&optional no-delimiters)
  "Put a singlequote before PAGE at point if any. "
  (interactive "*p")
  (ar-th-quote 'page no-delimiters (interactive-p)))

(defalias 'ar-hyphen-page-atpt 'ar-page-hyphen-atpt)
(defun ar-page-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around PAGE at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'page no-delimiters (interactive-p)))

(defalias 'ar-mark-page-atpt 'ar-page-mark-atpt)
(defun ar-page-mark-atpt ()
  "Marks PAGE at point if any. "
  (interactive)
  (ar-th-mark 'page))

(defalias 'ar-hide-page-atpt 'ar-page-hide-atpt)
(defun ar-page-hide-atpt ()
  "Hides PAGE at point. "
  (interactive)
  (ar-th-hide 'page))

(defalias 'ar-show-page-atpt 'ar-page-show-atpt)
(defun ar-page-show-atpt ()
  "Shows hidden PAGE at point. "
  (interactive)
  (ar-th-show 'page))

(defalias 'ar-hide-show-page-atpt 'ar-page-hide-show-atpt)
(defun ar-page-hide-show-atpt ()
  "Alternatively hides or shows PAGE at point. "
  (interactive)
  (ar-th-hide-show 'page))

(defalias 'ar-highlight-page-atpt-mode 'ar-page-highlight-atpt-mode)

(defun ar-page-highlight-atpt-mode (&optional no-delimiters)
  "Toggles page-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'page no-delimiters (interactive-p)))

(defalias 'ar-kill-page-atpt 'ar-page-kill-atpt)
(defun ar-page-kill-atpt (&optional no-delimiters)
  "Kills PAGE at point if any. "
  (interactive "*P")
  (ar-th-kill 'page no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-page-atpt 'ar-page-kill-backward-atpt)
(defun ar-page-kill-backward-atpt (&optional no-delimiters)
  "Kills PAGE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'page no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-page-atpt 'ar-page-left-right-singlequote-atpt)
(defun ar-page-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'page no-delimiters (interactive-p)))

(defalias 'ar-parentize-page-atpt 'ar-page-parentize-atpt)
(defun ar-page-parentize-atpt (&optional no-delimiters)
  "Parentizes PAGE at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'page no-delimiters (interactive-p)))

(defalias 'ar-separate-page-atpt 'ar-page-separate-atpt)
(defun ar-page-separate-atpt (&optional no-delimiters)
  "Separates PAGE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'page no-delimiters (interactive-p)))

(defalias 'ar-singlequote-page-atpt 'ar-page-singlequote-atpt)
(defun ar-page-singlequote-atpt (&optional no-delimiters)
  "Singlequotes PAGE at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'page no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-page-atpt 'ar-page-triplequote-dq-atpt)
(defun ar-page-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around page. "
  (interactive "*p")
  (ar-th-triplequote-dq 'page no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-page-atpt 'ar-page-triplequote-sq-atpt)
(defun ar-page-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around page. "
  (interactive "*p")
  (ar-th-triplequote-sq 'page no-delimiters (interactive-p)))

(defalias 'ar-trim-page-atpt 'ar-page-trim-atpt)
(defun ar-page-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'page t t))
    (ar-th-trim 'page t t)))

(defalias 'ar-trim-left-page-atpt 'ar-page-left-trim-atpt)
(defun ar-page-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'page t nil))
    (ar-th-trim 'page t nil)))

(defalias 'ar-trim-right-page-atpt 'ar-page-right-trim-atpt)
(defun ar-page-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'page nil t))
    (ar-th-trim 'page nil t)))

(defalias 'ar-page-underscore-atpt 'ar-underscore-page-atpt)
(defun ar-underscore-page-atpt (&optional no-delimiters)
  "Put underscore char around PAGE. "
  (interactive "*p")
  (ar-th-underscore 'page no-delimiters (interactive-p)))

(defalias 'ar-page-whitespace-atpt 'ar-whitespace-page-atpt)
(defun ar-whitespace-page-atpt (&optional no-delimiters)
  "Put whitespace char around PAGE. "
  (interactive "*p")
  (ar-th-whitespace 'page nil t))

(defalias 'ar-forward-page-atpt 'ar-page-forward-atpt)
(defun ar-page-forward-atpt (&optional arg)
  "Moves forward over PAGE at point if any, does nothing otherwise.
Returns end position of PAGE "
  (interactive "p")
  (ar-th-forward 'page arg (interactive-p)))

(defalias 'ar-backward-page-atpt 'ar-page-backward-atpt)
(defun ar-page-backward-atpt (&optional arg)
  "Moves backward over PAGE before point if any, does nothing otherwise.
Returns beginning position of PAGE "
  (interactive "p")
  (ar-th-backward 'page arg (interactive-p)))

(defalias 'ar-transpose-page-atpt 'ar-page-transpose-atpt)
(defun ar-page-transpose-atpt (&optional arg)
  "Transposes PAGE with PAGE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'page arg (interactive-p)))

(defalias 'ar-sort-page-atpt 'ar-page-sort-atpt)
(defun ar-page-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts pages in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'page reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-page-atpt 'ar-page-check-atpt)
(defun ar-page-check-atpt ()
  "Return t if a PAGE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-page-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-page-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-paragraph-atpt (&optional arg no-delimiters)
  "Returns paragraph at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'paragraph arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-paragraph-atpt 'ar-paragraph-bounds-atpt)
(defun ar-paragraph-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of paragraph if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph no-delimiters (interactive-p)))

(defun ar-paragraph-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PARAGRAPH at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'paragraph no-delimiters (interactive-p)))

(defun ar-paragraph-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-paragraph-atpt 'ar-paragraph-beginning-atpt)
(defun ar-paragraph-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-end-of-paragraph-atpt 'ar-paragraph-end-atpt)
(defun ar-paragraph-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-in-paragraph-p-atpt 'ar-paragraph-in-p-atpt)
(defun ar-paragraph-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PARAGRAPH at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-length-of-paragraph-atpt 'ar-paragraph-length-atpt)
(defun ar-paragraph-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-copy-paragraph-atpt 'ar-paragraph-copy-atpt)
(defun ar-paragraph-copy-atpt (&optional no-delimiters)
  "Returns a copy of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-delete-paragraph-in-region 'ar-paragraph-delete-in-region)
(defun ar-paragraph-delete-in-region (beg end)
  "Deletes PARAGRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'paragraph beg end (interactive-p)))

(defun ar-blok-paragraph-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around paragraph.
  Returns blok or nil if no PARAGRAPH at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-escape-paragraph-atpt 'ar-paragraph-escape-atpt)
(defun ar-paragraph-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted PARAGRAPH at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'paragraph no-delimiters))

(defalias 'ar-doublequote-paragraph-atpt 'ar-paragraph-doublequote-atpt)
(defun ar-paragraph-doublequote-atpt (&optional no-delimiters)
  "Doublequotes PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-slash-paragraph-atpt 'ar-paragraph-slash-atpt)
(defun ar-paragraph-slash-atpt (&optional no-delimiters)
  "Doublequotes PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-slash 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-paragraph-atpt 'ar-paragraph-double-backslash-atpt)
(defun ar-paragraph-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paragraph-atpt 'ar-paragraph-doubleslash-atpt)
(defun ar-paragraph-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-paragraph-atpt 'ar-paragraph-doubleslash-paren-atpt)
(defun ar-paragraph-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-slashparen-paragraph-atpt 'ar-paragraph-slashparen-atpt)
(defun ar-paragraph-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-dollar-paragraph-atpt 'ar-paragraph-dollar-atpt)
(defun ar-paragraph-dollar-atpt (&optional no-delimiters)
  "Doublequotes PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-dollar 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-equalize-paragraph-atpt 'ar-paragraph-equalize-atpt)
(defun ar-paragraph-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-equalize 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-paragraph-atpt 'ar-paragraph-greater-angle-atpt)
(defun ar-paragraph-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for PARAGRAPH after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-paragraph-atpt 'ar-paragraph-lesser-angle-atpt)
(defun ar-paragraph-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for PARAGRAPH after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-backslash-paragraph-atpt 'ar-paragraph-backslash-atpt)
(defun ar-paragraph-backslash-atpt (&optional no-delimiters)
  "Backslash PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-backslash 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-brace-paragraph-atpt 'ar-paragraph-brace-atpt)
(defun ar-paragraph-brace-atpt (&optional no-delimiters)
  "Braces PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-brace 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-bracket-paragraph-atpt 'ar-paragraph-bracket-atpt)
(defun ar-paragraph-bracket-atpt (&optional no-delimiters)
  "Brackets PARAGRAPH after point if any. "
  (interactive "*p")
  (ar-th-bracket 'paragraph no-delimiters (interactive-p)))

(defun ar-comment-paragraph-atpt (&optional no-delimiters)
  "Comments PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-comment 'paragraph no-delimiters (interactive-p)))

(defun ar-commatize-paragraph-atpt (&optional no-delimiters)
  "Put a comma after PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-commatize 'paragraph no-delimiters (interactive-p)))

(defun ar-quote-paragraph-atpt (&optional no-delimiters)
  "Put a singlequote before PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-quote 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-hyphen-paragraph-atpt 'ar-paragraph-hyphen-atpt)
(defun ar-paragraph-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-mark-paragraph-atpt 'ar-paragraph-mark-atpt)
(defun ar-paragraph-mark-atpt ()
  "Marks PARAGRAPH at point if any. "
  (interactive)
  (ar-th-mark 'paragraph))

(defalias 'ar-hide-paragraph-atpt 'ar-paragraph-hide-atpt)
(defun ar-paragraph-hide-atpt ()
  "Hides PARAGRAPH at point. "
  (interactive)
  (ar-th-hide 'paragraph))

(defalias 'ar-show-paragraph-atpt 'ar-paragraph-show-atpt)
(defun ar-paragraph-show-atpt ()
  "Shows hidden PARAGRAPH at point. "
  (interactive)
  (ar-th-show 'paragraph))

(defalias 'ar-hide-show-paragraph-atpt 'ar-paragraph-hide-show-atpt)
(defun ar-paragraph-hide-show-atpt ()
  "Alternatively hides or shows PARAGRAPH at point. "
  (interactive)
  (ar-th-hide-show 'paragraph))

(defalias 'ar-highlight-paragraph-atpt-mode 'ar-paragraph-highlight-atpt-mode)

(defun ar-paragraph-highlight-atpt-mode (&optional no-delimiters)
  "Toggles paragraph-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-kill-paragraph-atpt 'ar-paragraph-kill-atpt)
(defun ar-paragraph-kill-atpt (&optional no-delimiters)
  "Kills PARAGRAPH at point if any. "
  (interactive "*P")
  (ar-th-kill 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-paragraph-atpt 'ar-paragraph-kill-backward-atpt)
(defun ar-paragraph-kill-backward-atpt (&optional no-delimiters)
  "Kills PARAGRAPH at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-paragraph-atpt 'ar-paragraph-left-right-singlequote-atpt)
(defun ar-paragraph-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-parentize-paragraph-atpt 'ar-paragraph-parentize-atpt)
(defun ar-paragraph-parentize-atpt (&optional no-delimiters)
  "Parentizes PARAGRAPH at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-separate-paragraph-atpt 'ar-paragraph-separate-atpt)
(defun ar-paragraph-separate-atpt (&optional no-delimiters)
  "Separates PARAGRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-singlequote-paragraph-atpt 'ar-paragraph-singlequote-atpt)
(defun ar-paragraph-singlequote-atpt (&optional no-delimiters)
  "Singlequotes PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-paragraph-atpt 'ar-paragraph-triplequote-dq-atpt)
(defun ar-paragraph-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around paragraph. "
  (interactive "*p")
  (ar-th-triplequote-dq 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-paragraph-atpt 'ar-paragraph-triplequote-sq-atpt)
(defun ar-paragraph-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around paragraph. "
  (interactive "*p")
  (ar-th-triplequote-sq 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-trim-paragraph-atpt 'ar-paragraph-trim-atpt)
(defun ar-paragraph-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paragraph t t))
    (ar-th-trim 'paragraph t t)))

(defalias 'ar-trim-left-paragraph-atpt 'ar-paragraph-left-trim-atpt)
(defun ar-paragraph-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paragraph t nil))
    (ar-th-trim 'paragraph t nil)))

(defalias 'ar-trim-right-paragraph-atpt 'ar-paragraph-right-trim-atpt)
(defun ar-paragraph-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paragraph nil t))
    (ar-th-trim 'paragraph nil t)))

(defalias 'ar-paragraph-underscore-atpt 'ar-underscore-paragraph-atpt)
(defun ar-underscore-paragraph-atpt (&optional no-delimiters)
  "Put underscore char around PARAGRAPH. "
  (interactive "*p")
  (ar-th-underscore 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-paragraph-whitespace-atpt 'ar-whitespace-paragraph-atpt)
(defun ar-whitespace-paragraph-atpt (&optional no-delimiters)
  "Put whitespace char around PARAGRAPH. "
  (interactive "*p")
  (ar-th-whitespace 'paragraph nil t))

(defalias 'ar-forward-paragraph-atpt 'ar-paragraph-forward-atpt)
(defun ar-paragraph-forward-atpt (&optional arg)
  "Moves forward over PARAGRAPH at point if any, does nothing otherwise.
Returns end position of PARAGRAPH "
  (interactive "p")
  (ar-th-forward 'paragraph arg (interactive-p)))

(defalias 'ar-backward-paragraph-atpt 'ar-paragraph-backward-atpt)
(defun ar-paragraph-backward-atpt (&optional arg)
  "Moves backward over PARAGRAPH before point if any, does nothing otherwise.
Returns beginning position of PARAGRAPH "
  (interactive "p")
  (ar-th-backward 'paragraph arg (interactive-p)))

(defalias 'ar-transpose-paragraph-atpt 'ar-paragraph-transpose-atpt)
(defun ar-paragraph-transpose-atpt (&optional arg)
  "Transposes PARAGRAPH with PARAGRAPH before point if any. "
  (interactive "*p")
  (ar-th-transpose 'paragraph arg (interactive-p)))

(defalias 'ar-sort-paragraph-atpt 'ar-paragraph-sort-atpt)
(defun ar-paragraph-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts paragraphs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'paragraph reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-paragraph-atpt 'ar-paragraph-check-atpt)
(defun ar-paragraph-check-atpt ()
  "Return t if a PARAGRAPH at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-paragraph-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-paragraph-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-paren-atpt (&optional arg no-delimiters)
  "Returns paren at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'paren arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-paren-atpt 'ar-paren-bounds-atpt)
(defun ar-paren-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of paren if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paren no-delimiters (interactive-p)))

(defun ar-paren-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PAREN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'paren no-delimiters (interactive-p)))

(defun ar-paren-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'paren no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-paren-atpt 'ar-paren-beginning-atpt)
(defun ar-paren-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'paren no-delimiters (interactive-p)))

(defalias 'ar-end-of-paren-atpt 'ar-paren-end-atpt)
(defun ar-paren-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'paren no-delimiters (interactive-p)))

(defalias 'ar-in-paren-p-atpt 'ar-paren-in-p-atpt)
(defun ar-paren-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PAREN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paren no-delimiters (interactive-p)))

(defalias 'ar-length-of-paren-atpt 'ar-paren-length-atpt)
(defun ar-paren-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'paren no-delimiters (interactive-p)))

(defalias 'ar-copy-paren-atpt 'ar-paren-copy-atpt)
(defun ar-paren-copy-atpt (&optional no-delimiters)
  "Returns a copy of PAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'paren no-delimiters (interactive-p)))

(defalias 'ar-delete-paren-in-region 'ar-paren-delete-in-region)
(defun ar-paren-delete-in-region (beg end)
  "Deletes PAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'paren beg end (interactive-p)))

(defun ar-blok-paren-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around paren.
  Returns blok or nil if no PAREN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'paren no-delimiters (interactive-p)))

(defalias 'ar-escape-paren-atpt 'ar-paren-escape-atpt)
(defun ar-paren-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted PAREN at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'paren no-delimiters))

(defalias 'ar-doublequote-paren-atpt 'ar-paren-doublequote-atpt)
(defun ar-paren-doublequote-atpt (&optional no-delimiters)
  "Doublequotes PAREN at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'paren no-delimiters (interactive-p)))

(defalias 'ar-slash-paren-atpt 'ar-paren-slash-atpt)
(defun ar-paren-slash-atpt (&optional no-delimiters)
  "Doublequotes PAREN at point if any. "
  (interactive "*p")
  (ar-th-slash 'paren no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-paren-atpt 'ar-paren-double-backslash-atpt)
(defun ar-paren-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PAREN at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'paren no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-atpt 'ar-paren-doubleslash-atpt)
(defun ar-paren-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around PAREN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'paren no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-paren-atpt 'ar-paren-doubleslash-paren-atpt)
(defun ar-paren-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PAREN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'paren no-delimiters (interactive-p)))

(defalias 'ar-slashparen-paren-atpt 'ar-paren-slashparen-atpt)
(defun ar-paren-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around PAREN at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'paren no-delimiters (interactive-p)))

(defalias 'ar-dollar-paren-atpt 'ar-paren-dollar-atpt)
(defun ar-paren-dollar-atpt (&optional no-delimiters)
  "Doublequotes PAREN at point if any. "
  (interactive "*p")
  (ar-th-dollar 'paren no-delimiters (interactive-p)))

(defalias 'ar-equalize-paren-atpt 'ar-paren-equalize-atpt)
(defun ar-paren-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around PAREN at point if any. "
  (interactive "*p")
  (ar-th-equalize 'paren no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-paren-atpt 'ar-paren-greater-angle-atpt)
(defun ar-paren-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for PAREN after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'paren no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-paren-atpt 'ar-paren-lesser-angle-atpt)
(defun ar-paren-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for PAREN after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'paren no-delimiters (interactive-p)))

(defalias 'ar-backslash-paren-atpt 'ar-paren-backslash-atpt)
(defun ar-paren-backslash-atpt (&optional no-delimiters)
  "Backslash PAREN at point if any. "
  (interactive "*p")
  (ar-th-backslash 'paren no-delimiters (interactive-p)))

(defalias 'ar-brace-paren-atpt 'ar-paren-brace-atpt)
(defun ar-paren-brace-atpt (&optional no-delimiters)
  "Braces PAREN at point if any. "
  (interactive "*p")
  (ar-th-brace 'paren no-delimiters (interactive-p)))

(defalias 'ar-bracket-paren-atpt 'ar-paren-bracket-atpt)
(defun ar-paren-bracket-atpt (&optional no-delimiters)
  "Brackets PAREN after point if any. "
  (interactive "*p")
  (ar-th-bracket 'paren no-delimiters (interactive-p)))

(defun ar-comment-paren-atpt (&optional no-delimiters)
  "Comments PAREN at point if any. "
  (interactive "*p")
  (ar-th-comment 'paren no-delimiters (interactive-p)))

(defun ar-commatize-paren-atpt (&optional no-delimiters)
  "Put a comma after PAREN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'paren no-delimiters (interactive-p)))

(defun ar-quote-paren-atpt (&optional no-delimiters)
  "Put a singlequote before PAREN at point if any. "
  (interactive "*p")
  (ar-th-quote 'paren no-delimiters (interactive-p)))

(defalias 'ar-hyphen-paren-atpt 'ar-paren-hyphen-atpt)
(defun ar-paren-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around PAREN at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'paren no-delimiters (interactive-p)))

(defalias 'ar-mark-paren-atpt 'ar-paren-mark-atpt)
(defun ar-paren-mark-atpt ()
  "Marks PAREN at point if any. "
  (interactive)
  (ar-th-mark 'paren))

(defalias 'ar-hide-paren-atpt 'ar-paren-hide-atpt)
(defun ar-paren-hide-atpt ()
  "Hides PAREN at point. "
  (interactive)
  (ar-th-hide 'paren))

(defalias 'ar-show-paren-atpt 'ar-paren-show-atpt)
(defun ar-paren-show-atpt ()
  "Shows hidden PAREN at point. "
  (interactive)
  (ar-th-show 'paren))

(defalias 'ar-hide-show-paren-atpt 'ar-paren-hide-show-atpt)
(defun ar-paren-hide-show-atpt ()
  "Alternatively hides or shows PAREN at point. "
  (interactive)
  (ar-th-hide-show 'paren))

(defalias 'ar-highlight-paren-atpt-mode 'ar-paren-highlight-atpt-mode)

(defun ar-paren-highlight-atpt-mode (&optional no-delimiters)
  "Toggles paren-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'paren no-delimiters (interactive-p)))

(defalias 'ar-kill-paren-atpt 'ar-paren-kill-atpt)
(defun ar-paren-kill-atpt (&optional no-delimiters)
  "Kills PAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'paren no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-paren-atpt 'ar-paren-kill-backward-atpt)
(defun ar-paren-kill-backward-atpt (&optional no-delimiters)
  "Kills PAREN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'paren no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-paren-atpt 'ar-paren-left-right-singlequote-atpt)
(defun ar-paren-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'paren no-delimiters (interactive-p)))

(defalias 'ar-parentize-paren-atpt 'ar-paren-parentize-atpt)
(defun ar-paren-parentize-atpt (&optional no-delimiters)
  "Parentizes PAREN at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'paren no-delimiters (interactive-p)))

(defalias 'ar-separate-paren-atpt 'ar-paren-separate-atpt)
(defun ar-paren-separate-atpt (&optional no-delimiters)
  "Separates PAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'paren no-delimiters (interactive-p)))

(defalias 'ar-singlequote-paren-atpt 'ar-paren-singlequote-atpt)
(defun ar-paren-singlequote-atpt (&optional no-delimiters)
  "Singlequotes PAREN at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'paren no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-paren-atpt 'ar-paren-triplequote-dq-atpt)
(defun ar-paren-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around paren. "
  (interactive "*p")
  (ar-th-triplequote-dq 'paren no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-paren-atpt 'ar-paren-triplequote-sq-atpt)
(defun ar-paren-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around paren. "
  (interactive "*p")
  (ar-th-triplequote-sq 'paren no-delimiters (interactive-p)))

(defalias 'ar-trim-paren-atpt 'ar-paren-trim-atpt)
(defun ar-paren-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paren t t))
    (ar-th-trim 'paren t t)))

(defalias 'ar-trim-left-paren-atpt 'ar-paren-left-trim-atpt)
(defun ar-paren-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paren t nil))
    (ar-th-trim 'paren t nil)))

(defalias 'ar-trim-right-paren-atpt 'ar-paren-right-trim-atpt)
(defun ar-paren-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'paren nil t))
    (ar-th-trim 'paren nil t)))

(defalias 'ar-paren-underscore-atpt 'ar-underscore-paren-atpt)
(defun ar-underscore-paren-atpt (&optional no-delimiters)
  "Put underscore char around PAREN. "
  (interactive "*p")
  (ar-th-underscore 'paren no-delimiters (interactive-p)))

(defalias 'ar-paren-whitespace-atpt 'ar-whitespace-paren-atpt)
(defun ar-whitespace-paren-atpt (&optional no-delimiters)
  "Put whitespace char around PAREN. "
  (interactive "*p")
  (ar-th-whitespace 'paren nil t))

(defalias 'ar-forward-paren-atpt 'ar-paren-forward-atpt)
(defun ar-paren-forward-atpt (&optional arg)
  "Moves forward over PAREN at point if any, does nothing otherwise.
Returns end position of PAREN "
  (interactive "p")
  (ar-th-forward 'paren arg (interactive-p)))

(defalias 'ar-backward-paren-atpt 'ar-paren-backward-atpt)
(defun ar-paren-backward-atpt (&optional arg)
  "Moves backward over PAREN before point if any, does nothing otherwise.
Returns beginning position of PAREN "
  (interactive "p")
  (ar-th-backward 'paren arg (interactive-p)))

(defalias 'ar-transpose-paren-atpt 'ar-paren-transpose-atpt)
(defun ar-paren-transpose-atpt (&optional arg)
  "Transposes PAREN with PAREN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'paren arg (interactive-p)))

(defalias 'ar-sort-paren-atpt 'ar-paren-sort-atpt)
(defun ar-paren-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts parens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'paren reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-paren-atpt 'ar-paren-check-atpt)
(defun ar-paren-check-atpt ()
  "Return t if a PAREN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-paren-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-paren-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-phone-atpt (&optional arg no-delimiters)
  "Returns phone at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'phone arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-phone-atpt 'ar-phone-bounds-atpt)
(defun ar-phone-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of phone if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone no-delimiters (interactive-p)))

(defun ar-phone-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PHONE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'phone no-delimiters (interactive-p)))

(defun ar-phone-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'phone no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-phone-atpt 'ar-phone-beginning-atpt)
(defun ar-phone-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'phone no-delimiters (interactive-p)))

(defalias 'ar-end-of-phone-atpt 'ar-phone-end-atpt)
(defun ar-phone-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'phone no-delimiters (interactive-p)))

(defalias 'ar-in-phone-p-atpt 'ar-phone-in-p-atpt)
(defun ar-phone-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PHONE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone no-delimiters (interactive-p)))

(defalias 'ar-length-of-phone-atpt 'ar-phone-length-atpt)
(defun ar-phone-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'phone no-delimiters (interactive-p)))

(defalias 'ar-copy-phone-atpt 'ar-phone-copy-atpt)
(defun ar-phone-copy-atpt (&optional no-delimiters)
  "Returns a copy of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'phone no-delimiters (interactive-p)))

(defalias 'ar-delete-phone-in-region 'ar-phone-delete-in-region)
(defun ar-phone-delete-in-region (beg end)
  "Deletes PHONE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'phone beg end (interactive-p)))

(defun ar-blok-phone-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around phone.
  Returns blok or nil if no PHONE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'phone no-delimiters (interactive-p)))

(defalias 'ar-escape-phone-atpt 'ar-phone-escape-atpt)
(defun ar-phone-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted PHONE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'phone no-delimiters))

(defalias 'ar-doublequote-phone-atpt 'ar-phone-doublequote-atpt)
(defun ar-phone-doublequote-atpt (&optional no-delimiters)
  "Doublequotes PHONE at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'phone no-delimiters (interactive-p)))

(defalias 'ar-slash-phone-atpt 'ar-phone-slash-atpt)
(defun ar-phone-slash-atpt (&optional no-delimiters)
  "Doublequotes PHONE at point if any. "
  (interactive "*p")
  (ar-th-slash 'phone no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-phone-atpt 'ar-phone-double-backslash-atpt)
(defun ar-phone-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PHONE at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'phone no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-phone-atpt 'ar-phone-doubleslash-atpt)
(defun ar-phone-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around PHONE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'phone no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-phone-atpt 'ar-phone-doubleslash-paren-atpt)
(defun ar-phone-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PHONE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'phone no-delimiters (interactive-p)))

(defalias 'ar-slashparen-phone-atpt 'ar-phone-slashparen-atpt)
(defun ar-phone-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around PHONE at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'phone no-delimiters (interactive-p)))

(defalias 'ar-dollar-phone-atpt 'ar-phone-dollar-atpt)
(defun ar-phone-dollar-atpt (&optional no-delimiters)
  "Doublequotes PHONE at point if any. "
  (interactive "*p")
  (ar-th-dollar 'phone no-delimiters (interactive-p)))

(defalias 'ar-equalize-phone-atpt 'ar-phone-equalize-atpt)
(defun ar-phone-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around PHONE at point if any. "
  (interactive "*p")
  (ar-th-equalize 'phone no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-phone-atpt 'ar-phone-greater-angle-atpt)
(defun ar-phone-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for PHONE after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'phone no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-phone-atpt 'ar-phone-lesser-angle-atpt)
(defun ar-phone-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for PHONE after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'phone no-delimiters (interactive-p)))

(defalias 'ar-backslash-phone-atpt 'ar-phone-backslash-atpt)
(defun ar-phone-backslash-atpt (&optional no-delimiters)
  "Backslash PHONE at point if any. "
  (interactive "*p")
  (ar-th-backslash 'phone no-delimiters (interactive-p)))

(defalias 'ar-brace-phone-atpt 'ar-phone-brace-atpt)
(defun ar-phone-brace-atpt (&optional no-delimiters)
  "Braces PHONE at point if any. "
  (interactive "*p")
  (ar-th-brace 'phone no-delimiters (interactive-p)))

(defalias 'ar-bracket-phone-atpt 'ar-phone-bracket-atpt)
(defun ar-phone-bracket-atpt (&optional no-delimiters)
  "Brackets PHONE after point if any. "
  (interactive "*p")
  (ar-th-bracket 'phone no-delimiters (interactive-p)))

(defun ar-comment-phone-atpt (&optional no-delimiters)
  "Comments PHONE at point if any. "
  (interactive "*p")
  (ar-th-comment 'phone no-delimiters (interactive-p)))

(defun ar-commatize-phone-atpt (&optional no-delimiters)
  "Put a comma after PHONE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'phone no-delimiters (interactive-p)))

(defun ar-quote-phone-atpt (&optional no-delimiters)
  "Put a singlequote before PHONE at point if any. "
  (interactive "*p")
  (ar-th-quote 'phone no-delimiters (interactive-p)))

(defalias 'ar-hyphen-phone-atpt 'ar-phone-hyphen-atpt)
(defun ar-phone-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around PHONE at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'phone no-delimiters (interactive-p)))

(defalias 'ar-mark-phone-atpt 'ar-phone-mark-atpt)
(defun ar-phone-mark-atpt ()
  "Marks PHONE at point if any. "
  (interactive)
  (ar-th-mark 'phone))

(defalias 'ar-hide-phone-atpt 'ar-phone-hide-atpt)
(defun ar-phone-hide-atpt ()
  "Hides PHONE at point. "
  (interactive)
  (ar-th-hide 'phone))

(defalias 'ar-show-phone-atpt 'ar-phone-show-atpt)
(defun ar-phone-show-atpt ()
  "Shows hidden PHONE at point. "
  (interactive)
  (ar-th-show 'phone))

(defalias 'ar-hide-show-phone-atpt 'ar-phone-hide-show-atpt)
(defun ar-phone-hide-show-atpt ()
  "Alternatively hides or shows PHONE at point. "
  (interactive)
  (ar-th-hide-show 'phone))

(defalias 'ar-highlight-phone-atpt-mode 'ar-phone-highlight-atpt-mode)

(defun ar-phone-highlight-atpt-mode (&optional no-delimiters)
  "Toggles phone-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'phone no-delimiters (interactive-p)))

(defalias 'ar-kill-phone-atpt 'ar-phone-kill-atpt)
(defun ar-phone-kill-atpt (&optional no-delimiters)
  "Kills PHONE at point if any. "
  (interactive "*P")
  (ar-th-kill 'phone no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-phone-atpt 'ar-phone-kill-backward-atpt)
(defun ar-phone-kill-backward-atpt (&optional no-delimiters)
  "Kills PHONE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'phone no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-phone-atpt 'ar-phone-left-right-singlequote-atpt)
(defun ar-phone-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'phone no-delimiters (interactive-p)))

(defalias 'ar-parentize-phone-atpt 'ar-phone-parentize-atpt)
(defun ar-phone-parentize-atpt (&optional no-delimiters)
  "Parentizes PHONE at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'phone no-delimiters (interactive-p)))

(defalias 'ar-separate-phone-atpt 'ar-phone-separate-atpt)
(defun ar-phone-separate-atpt (&optional no-delimiters)
  "Separates PHONE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'phone no-delimiters (interactive-p)))

(defalias 'ar-singlequote-phone-atpt 'ar-phone-singlequote-atpt)
(defun ar-phone-singlequote-atpt (&optional no-delimiters)
  "Singlequotes PHONE at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'phone no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-phone-atpt 'ar-phone-triplequote-dq-atpt)
(defun ar-phone-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around phone. "
  (interactive "*p")
  (ar-th-triplequote-dq 'phone no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-phone-atpt 'ar-phone-triplequote-sq-atpt)
(defun ar-phone-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around phone. "
  (interactive "*p")
  (ar-th-triplequote-sq 'phone no-delimiters (interactive-p)))

(defalias 'ar-trim-phone-atpt 'ar-phone-trim-atpt)
(defun ar-phone-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'phone t t))
    (ar-th-trim 'phone t t)))

(defalias 'ar-trim-left-phone-atpt 'ar-phone-left-trim-atpt)
(defun ar-phone-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'phone t nil))
    (ar-th-trim 'phone t nil)))

(defalias 'ar-trim-right-phone-atpt 'ar-phone-right-trim-atpt)
(defun ar-phone-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'phone nil t))
    (ar-th-trim 'phone nil t)))

(defalias 'ar-phone-underscore-atpt 'ar-underscore-phone-atpt)
(defun ar-underscore-phone-atpt (&optional no-delimiters)
  "Put underscore char around PHONE. "
  (interactive "*p")
  (ar-th-underscore 'phone no-delimiters (interactive-p)))

(defalias 'ar-phone-whitespace-atpt 'ar-whitespace-phone-atpt)
(defun ar-whitespace-phone-atpt (&optional no-delimiters)
  "Put whitespace char around PHONE. "
  (interactive "*p")
  (ar-th-whitespace 'phone nil t))

(defalias 'ar-forward-phone-atpt 'ar-phone-forward-atpt)
(defun ar-phone-forward-atpt (&optional arg)
  "Moves forward over PHONE at point if any, does nothing otherwise.
Returns end position of PHONE "
  (interactive "p")
  (ar-th-forward 'phone arg (interactive-p)))

(defalias 'ar-backward-phone-atpt 'ar-phone-backward-atpt)
(defun ar-phone-backward-atpt (&optional arg)
  "Moves backward over PHONE before point if any, does nothing otherwise.
Returns beginning position of PHONE "
  (interactive "p")
  (ar-th-backward 'phone arg (interactive-p)))

(defalias 'ar-transpose-phone-atpt 'ar-phone-transpose-atpt)
(defun ar-phone-transpose-atpt (&optional arg)
  "Transposes PHONE with PHONE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'phone arg (interactive-p)))

(defalias 'ar-sort-phone-atpt 'ar-phone-sort-atpt)
(defun ar-phone-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts phones in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'phone reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-phone-atpt 'ar-phone-check-atpt)
(defun ar-phone-check-atpt ()
  "Return t if a PHONE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-phone-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-phone-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-region-atpt (&optional arg no-delimiters)
  "Returns region at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'region arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-region-atpt 'ar-region-bounds-atpt)
(defun ar-region-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of region if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region no-delimiters (interactive-p)))

(defun ar-region-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position REGION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'region no-delimiters (interactive-p)))

(defun ar-region-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'region no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-region-atpt 'ar-region-beginning-atpt)
(defun ar-region-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'region no-delimiters (interactive-p)))

(defalias 'ar-end-of-region-atpt 'ar-region-end-atpt)
(defun ar-region-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'region no-delimiters (interactive-p)))

(defalias 'ar-in-region-p-atpt 'ar-region-in-p-atpt)
(defun ar-region-in-p-atpt (&optional no-delimiters)
  "Returns bounds of REGION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region no-delimiters (interactive-p)))

(defalias 'ar-length-of-region-atpt 'ar-region-length-atpt)
(defun ar-region-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'region no-delimiters (interactive-p)))

(defalias 'ar-copy-region-atpt 'ar-region-copy-atpt)
(defun ar-region-copy-atpt (&optional no-delimiters)
  "Returns a copy of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'region no-delimiters (interactive-p)))

(defalias 'ar-delete-region-in-region 'ar-region-delete-in-region)
(defun ar-region-delete-in-region (beg end)
  "Deletes REGION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'region beg end (interactive-p)))

(defun ar-blok-region-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around region.
  Returns blok or nil if no REGION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'region no-delimiters (interactive-p)))

(defalias 'ar-escape-region-atpt 'ar-region-escape-atpt)
(defun ar-region-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted REGION at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'region no-delimiters))

(defalias 'ar-doublequote-region-atpt 'ar-region-doublequote-atpt)
(defun ar-region-doublequote-atpt (&optional no-delimiters)
  "Doublequotes REGION at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'region no-delimiters (interactive-p)))

(defalias 'ar-slash-region-atpt 'ar-region-slash-atpt)
(defun ar-region-slash-atpt (&optional no-delimiters)
  "Doublequotes REGION at point if any. "
  (interactive "*p")
  (ar-th-slash 'region no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-region-atpt 'ar-region-double-backslash-atpt)
(defun ar-region-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around REGION at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'region no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-region-atpt 'ar-region-doubleslash-atpt)
(defun ar-region-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around REGION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'region no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-region-atpt 'ar-region-doubleslash-paren-atpt)
(defun ar-region-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around REGION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'region no-delimiters (interactive-p)))

(defalias 'ar-slashparen-region-atpt 'ar-region-slashparen-atpt)
(defun ar-region-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around REGION at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'region no-delimiters (interactive-p)))

(defalias 'ar-dollar-region-atpt 'ar-region-dollar-atpt)
(defun ar-region-dollar-atpt (&optional no-delimiters)
  "Doublequotes REGION at point if any. "
  (interactive "*p")
  (ar-th-dollar 'region no-delimiters (interactive-p)))

(defalias 'ar-equalize-region-atpt 'ar-region-equalize-atpt)
(defun ar-region-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around REGION at point if any. "
  (interactive "*p")
  (ar-th-equalize 'region no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-region-atpt 'ar-region-greater-angle-atpt)
(defun ar-region-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for REGION after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'region no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-region-atpt 'ar-region-lesser-angle-atpt)
(defun ar-region-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for REGION after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'region no-delimiters (interactive-p)))

(defalias 'ar-backslash-region-atpt 'ar-region-backslash-atpt)
(defun ar-region-backslash-atpt (&optional no-delimiters)
  "Backslash REGION at point if any. "
  (interactive "*p")
  (ar-th-backslash 'region no-delimiters (interactive-p)))

(defalias 'ar-brace-region-atpt 'ar-region-brace-atpt)
(defun ar-region-brace-atpt (&optional no-delimiters)
  "Braces REGION at point if any. "
  (interactive "*p")
  (ar-th-brace 'region no-delimiters (interactive-p)))

(defalias 'ar-bracket-region-atpt 'ar-region-bracket-atpt)
(defun ar-region-bracket-atpt (&optional no-delimiters)
  "Brackets REGION after point if any. "
  (interactive "*p")
  (ar-th-bracket 'region no-delimiters (interactive-p)))

(defun ar-comment-region-atpt (&optional no-delimiters)
  "Comments REGION at point if any. "
  (interactive "*p")
  (ar-th-comment 'region no-delimiters (interactive-p)))

(defun ar-commatize-region-atpt (&optional no-delimiters)
  "Put a comma after REGION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'region no-delimiters (interactive-p)))

(defun ar-quote-region-atpt (&optional no-delimiters)
  "Put a singlequote before REGION at point if any. "
  (interactive "*p")
  (ar-th-quote 'region no-delimiters (interactive-p)))

(defalias 'ar-hyphen-region-atpt 'ar-region-hyphen-atpt)
(defun ar-region-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around REGION at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'region no-delimiters (interactive-p)))

(defalias 'ar-mark-region-atpt 'ar-region-mark-atpt)
(defun ar-region-mark-atpt ()
  "Marks REGION at point if any. "
  (interactive)
  (ar-th-mark 'region))

(defalias 'ar-hide-region-atpt 'ar-region-hide-atpt)
(defun ar-region-hide-atpt ()
  "Hides REGION at point. "
  (interactive)
  (ar-th-hide 'region))

(defalias 'ar-show-region-atpt 'ar-region-show-atpt)
(defun ar-region-show-atpt ()
  "Shows hidden REGION at point. "
  (interactive)
  (ar-th-show 'region))

(defalias 'ar-hide-show-region-atpt 'ar-region-hide-show-atpt)
(defun ar-region-hide-show-atpt ()
  "Alternatively hides or shows REGION at point. "
  (interactive)
  (ar-th-hide-show 'region))

(defalias 'ar-highlight-region-atpt-mode 'ar-region-highlight-atpt-mode)

(defun ar-region-highlight-atpt-mode (&optional no-delimiters)
  "Toggles region-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'region no-delimiters (interactive-p)))

(defalias 'ar-kill-region-atpt 'ar-region-kill-atpt)
(defun ar-region-kill-atpt (&optional no-delimiters)
  "Kills REGION at point if any. "
  (interactive "*P")
  (ar-th-kill 'region no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-region-atpt 'ar-region-kill-backward-atpt)
(defun ar-region-kill-backward-atpt (&optional no-delimiters)
  "Kills REGION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'region no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-region-atpt 'ar-region-left-right-singlequote-atpt)
(defun ar-region-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'region no-delimiters (interactive-p)))

(defalias 'ar-parentize-region-atpt 'ar-region-parentize-atpt)
(defun ar-region-parentize-atpt (&optional no-delimiters)
  "Parentizes REGION at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'region no-delimiters (interactive-p)))

(defalias 'ar-separate-region-atpt 'ar-region-separate-atpt)
(defun ar-region-separate-atpt (&optional no-delimiters)
  "Separates REGION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'region no-delimiters (interactive-p)))

(defalias 'ar-singlequote-region-atpt 'ar-region-singlequote-atpt)
(defun ar-region-singlequote-atpt (&optional no-delimiters)
  "Singlequotes REGION at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'region no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-region-atpt 'ar-region-triplequote-dq-atpt)
(defun ar-region-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around region. "
  (interactive "*p")
  (ar-th-triplequote-dq 'region no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-region-atpt 'ar-region-triplequote-sq-atpt)
(defun ar-region-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around region. "
  (interactive "*p")
  (ar-th-triplequote-sq 'region no-delimiters (interactive-p)))

(defalias 'ar-trim-region-atpt 'ar-region-trim-atpt)
(defun ar-region-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'region t t))
    (ar-th-trim 'region t t)))

(defalias 'ar-trim-left-region-atpt 'ar-region-left-trim-atpt)
(defun ar-region-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'region t nil))
    (ar-th-trim 'region t nil)))

(defalias 'ar-trim-right-region-atpt 'ar-region-right-trim-atpt)
(defun ar-region-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'region nil t))
    (ar-th-trim 'region nil t)))

(defalias 'ar-region-underscore-atpt 'ar-underscore-region-atpt)
(defun ar-underscore-region-atpt (&optional no-delimiters)
  "Put underscore char around REGION. "
  (interactive "*p")
  (ar-th-underscore 'region no-delimiters (interactive-p)))

(defalias 'ar-region-whitespace-atpt 'ar-whitespace-region-atpt)
(defun ar-whitespace-region-atpt (&optional no-delimiters)
  "Put whitespace char around REGION. "
  (interactive "*p")
  (ar-th-whitespace 'region nil t))

(defalias 'ar-forward-region-atpt 'ar-region-forward-atpt)
(defun ar-region-forward-atpt (&optional arg)
  "Moves forward over REGION at point if any, does nothing otherwise.
Returns end position of REGION "
  (interactive "p")
  (ar-th-forward 'region arg (interactive-p)))

(defalias 'ar-backward-region-atpt 'ar-region-backward-atpt)
(defun ar-region-backward-atpt (&optional arg)
  "Moves backward over REGION before point if any, does nothing otherwise.
Returns beginning position of REGION "
  (interactive "p")
  (ar-th-backward 'region arg (interactive-p)))

(defalias 'ar-transpose-region-atpt 'ar-region-transpose-atpt)
(defun ar-region-transpose-atpt (&optional arg)
  "Transposes REGION with REGION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'region arg (interactive-p)))

(defalias 'ar-sort-region-atpt 'ar-region-sort-atpt)
(defun ar-region-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts regions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'region reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-region-atpt 'ar-region-check-atpt)
(defun ar-region-check-atpt ()
  "Return t if a REGION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-region-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-region-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-sentence-atpt (&optional arg no-delimiters)
  "Returns sentence at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'sentence arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-sentence-atpt 'ar-sentence-bounds-atpt)
(defun ar-sentence-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of sentence if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence no-delimiters (interactive-p)))

(defun ar-sentence-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SENTENCE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sentence no-delimiters (interactive-p)))

(defun ar-sentence-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sentence no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-sentence-atpt 'ar-sentence-beginning-atpt)
(defun ar-sentence-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sentence no-delimiters (interactive-p)))

(defalias 'ar-end-of-sentence-atpt 'ar-sentence-end-atpt)
(defun ar-sentence-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sentence no-delimiters (interactive-p)))

(defalias 'ar-in-sentence-p-atpt 'ar-sentence-in-p-atpt)
(defun ar-sentence-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SENTENCE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence no-delimiters (interactive-p)))

(defalias 'ar-length-of-sentence-atpt 'ar-sentence-length-atpt)
(defun ar-sentence-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sentence no-delimiters (interactive-p)))

(defalias 'ar-copy-sentence-atpt 'ar-sentence-copy-atpt)
(defun ar-sentence-copy-atpt (&optional no-delimiters)
  "Returns a copy of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sentence no-delimiters (interactive-p)))

(defalias 'ar-delete-sentence-in-region 'ar-sentence-delete-in-region)
(defun ar-sentence-delete-in-region (beg end)
  "Deletes SENTENCE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sentence beg end (interactive-p)))

(defun ar-blok-sentence-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around sentence.
  Returns blok or nil if no SENTENCE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'sentence no-delimiters (interactive-p)))

(defalias 'ar-escape-sentence-atpt 'ar-sentence-escape-atpt)
(defun ar-sentence-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted SENTENCE at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'sentence no-delimiters))

(defalias 'ar-doublequote-sentence-atpt 'ar-sentence-doublequote-atpt)
(defun ar-sentence-doublequote-atpt (&optional no-delimiters)
  "Doublequotes SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'sentence no-delimiters (interactive-p)))

(defalias 'ar-slash-sentence-atpt 'ar-sentence-slash-atpt)
(defun ar-sentence-slash-atpt (&optional no-delimiters)
  "Doublequotes SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-slash 'sentence no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-sentence-atpt 'ar-sentence-double-backslash-atpt)
(defun ar-sentence-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'sentence no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-sentence-atpt 'ar-sentence-doubleslash-atpt)
(defun ar-sentence-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'sentence no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-sentence-atpt 'ar-sentence-doubleslash-paren-atpt)
(defun ar-sentence-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'sentence no-delimiters (interactive-p)))

(defalias 'ar-slashparen-sentence-atpt 'ar-sentence-slashparen-atpt)
(defun ar-sentence-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'sentence no-delimiters (interactive-p)))

(defalias 'ar-dollar-sentence-atpt 'ar-sentence-dollar-atpt)
(defun ar-sentence-dollar-atpt (&optional no-delimiters)
  "Doublequotes SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-dollar 'sentence no-delimiters (interactive-p)))

(defalias 'ar-equalize-sentence-atpt 'ar-sentence-equalize-atpt)
(defun ar-sentence-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-equalize 'sentence no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-sentence-atpt 'ar-sentence-greater-angle-atpt)
(defun ar-sentence-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for SENTENCE after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'sentence no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-sentence-atpt 'ar-sentence-lesser-angle-atpt)
(defun ar-sentence-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for SENTENCE after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'sentence no-delimiters (interactive-p)))

(defalias 'ar-backslash-sentence-atpt 'ar-sentence-backslash-atpt)
(defun ar-sentence-backslash-atpt (&optional no-delimiters)
  "Backslash SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-backslash 'sentence no-delimiters (interactive-p)))

(defalias 'ar-brace-sentence-atpt 'ar-sentence-brace-atpt)
(defun ar-sentence-brace-atpt (&optional no-delimiters)
  "Braces SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-brace 'sentence no-delimiters (interactive-p)))

(defalias 'ar-bracket-sentence-atpt 'ar-sentence-bracket-atpt)
(defun ar-sentence-bracket-atpt (&optional no-delimiters)
  "Brackets SENTENCE after point if any. "
  (interactive "*p")
  (ar-th-bracket 'sentence no-delimiters (interactive-p)))

(defun ar-comment-sentence-atpt (&optional no-delimiters)
  "Comments SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-comment 'sentence no-delimiters (interactive-p)))

(defun ar-commatize-sentence-atpt (&optional no-delimiters)
  "Put a comma after SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'sentence no-delimiters (interactive-p)))

(defun ar-quote-sentence-atpt (&optional no-delimiters)
  "Put a singlequote before SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-quote 'sentence no-delimiters (interactive-p)))

(defalias 'ar-hyphen-sentence-atpt 'ar-sentence-hyphen-atpt)
(defun ar-sentence-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'sentence no-delimiters (interactive-p)))

(defalias 'ar-mark-sentence-atpt 'ar-sentence-mark-atpt)
(defun ar-sentence-mark-atpt ()
  "Marks SENTENCE at point if any. "
  (interactive)
  (ar-th-mark 'sentence))

(defalias 'ar-hide-sentence-atpt 'ar-sentence-hide-atpt)
(defun ar-sentence-hide-atpt ()
  "Hides SENTENCE at point. "
  (interactive)
  (ar-th-hide 'sentence))

(defalias 'ar-show-sentence-atpt 'ar-sentence-show-atpt)
(defun ar-sentence-show-atpt ()
  "Shows hidden SENTENCE at point. "
  (interactive)
  (ar-th-show 'sentence))

(defalias 'ar-hide-show-sentence-atpt 'ar-sentence-hide-show-atpt)
(defun ar-sentence-hide-show-atpt ()
  "Alternatively hides or shows SENTENCE at point. "
  (interactive)
  (ar-th-hide-show 'sentence))

(defalias 'ar-highlight-sentence-atpt-mode 'ar-sentence-highlight-atpt-mode)

(defun ar-sentence-highlight-atpt-mode (&optional no-delimiters)
  "Toggles sentence-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'sentence no-delimiters (interactive-p)))

(defalias 'ar-kill-sentence-atpt 'ar-sentence-kill-atpt)
(defun ar-sentence-kill-atpt (&optional no-delimiters)
  "Kills SENTENCE at point if any. "
  (interactive "*P")
  (ar-th-kill 'sentence no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-sentence-atpt 'ar-sentence-kill-backward-atpt)
(defun ar-sentence-kill-backward-atpt (&optional no-delimiters)
  "Kills SENTENCE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'sentence no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-sentence-atpt 'ar-sentence-left-right-singlequote-atpt)
(defun ar-sentence-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'sentence no-delimiters (interactive-p)))

(defalias 'ar-parentize-sentence-atpt 'ar-sentence-parentize-atpt)
(defun ar-sentence-parentize-atpt (&optional no-delimiters)
  "Parentizes SENTENCE at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'sentence no-delimiters (interactive-p)))

(defalias 'ar-separate-sentence-atpt 'ar-sentence-separate-atpt)
(defun ar-sentence-separate-atpt (&optional no-delimiters)
  "Separates SENTENCE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'sentence no-delimiters (interactive-p)))

(defalias 'ar-singlequote-sentence-atpt 'ar-sentence-singlequote-atpt)
(defun ar-sentence-singlequote-atpt (&optional no-delimiters)
  "Singlequotes SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'sentence no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-sentence-atpt 'ar-sentence-triplequote-dq-atpt)
(defun ar-sentence-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around sentence. "
  (interactive "*p")
  (ar-th-triplequote-dq 'sentence no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-sentence-atpt 'ar-sentence-triplequote-sq-atpt)
(defun ar-sentence-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around sentence. "
  (interactive "*p")
  (ar-th-triplequote-sq 'sentence no-delimiters (interactive-p)))

(defalias 'ar-trim-sentence-atpt 'ar-sentence-trim-atpt)
(defun ar-sentence-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sentence t t))
    (ar-th-trim 'sentence t t)))

(defalias 'ar-trim-left-sentence-atpt 'ar-sentence-left-trim-atpt)
(defun ar-sentence-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sentence t nil))
    (ar-th-trim 'sentence t nil)))

(defalias 'ar-trim-right-sentence-atpt 'ar-sentence-right-trim-atpt)
(defun ar-sentence-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sentence nil t))
    (ar-th-trim 'sentence nil t)))

(defalias 'ar-sentence-underscore-atpt 'ar-underscore-sentence-atpt)
(defun ar-underscore-sentence-atpt (&optional no-delimiters)
  "Put underscore char around SENTENCE. "
  (interactive "*p")
  (ar-th-underscore 'sentence no-delimiters (interactive-p)))

(defalias 'ar-sentence-whitespace-atpt 'ar-whitespace-sentence-atpt)
(defun ar-whitespace-sentence-atpt (&optional no-delimiters)
  "Put whitespace char around SENTENCE. "
  (interactive "*p")
  (ar-th-whitespace 'sentence nil t))

(defalias 'ar-forward-sentence-atpt 'ar-sentence-forward-atpt)
(defun ar-sentence-forward-atpt (&optional arg)
  "Moves forward over SENTENCE at point if any, does nothing otherwise.
Returns end position of SENTENCE "
  (interactive "p")
  (ar-th-forward 'sentence arg (interactive-p)))

(defalias 'ar-backward-sentence-atpt 'ar-sentence-backward-atpt)
(defun ar-sentence-backward-atpt (&optional arg)
  "Moves backward over SENTENCE before point if any, does nothing otherwise.
Returns beginning position of SENTENCE "
  (interactive "p")
  (ar-th-backward 'sentence arg (interactive-p)))

(defalias 'ar-transpose-sentence-atpt 'ar-sentence-transpose-atpt)
(defun ar-sentence-transpose-atpt (&optional arg)
  "Transposes SENTENCE with SENTENCE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'sentence arg (interactive-p)))

(defalias 'ar-sort-sentence-atpt 'ar-sentence-sort-atpt)
(defun ar-sentence-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts sentences in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'sentence reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-sentence-atpt 'ar-sentence-check-atpt)
(defun ar-sentence-check-atpt ()
  "Return t if a SENTENCE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-sentence-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-sentence-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-sexp-atpt (&optional arg no-delimiters)
  "Returns sexp at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'sexp arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-sexp-atpt 'ar-sexp-bounds-atpt)
(defun ar-sexp-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of sexp if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp no-delimiters (interactive-p)))

(defun ar-sexp-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SEXP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sexp no-delimiters (interactive-p)))

(defun ar-sexp-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sexp no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-sexp-atpt 'ar-sexp-beginning-atpt)
(defun ar-sexp-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sexp no-delimiters (interactive-p)))

(defalias 'ar-end-of-sexp-atpt 'ar-sexp-end-atpt)
(defun ar-sexp-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sexp no-delimiters (interactive-p)))

(defalias 'ar-in-sexp-p-atpt 'ar-sexp-in-p-atpt)
(defun ar-sexp-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SEXP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp no-delimiters (interactive-p)))

(defalias 'ar-length-of-sexp-atpt 'ar-sexp-length-atpt)
(defun ar-sexp-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sexp no-delimiters (interactive-p)))

(defalias 'ar-copy-sexp-atpt 'ar-sexp-copy-atpt)
(defun ar-sexp-copy-atpt (&optional no-delimiters)
  "Returns a copy of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sexp no-delimiters (interactive-p)))

(defalias 'ar-delete-sexp-in-region 'ar-sexp-delete-in-region)
(defun ar-sexp-delete-in-region (beg end)
  "Deletes SEXP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sexp beg end (interactive-p)))

(defun ar-blok-sexp-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around sexp.
  Returns blok or nil if no SEXP at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'sexp no-delimiters (interactive-p)))

(defalias 'ar-escape-sexp-atpt 'ar-sexp-escape-atpt)
(defun ar-sexp-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted SEXP at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'sexp no-delimiters))

(defalias 'ar-doublequote-sexp-atpt 'ar-sexp-doublequote-atpt)
(defun ar-sexp-doublequote-atpt (&optional no-delimiters)
  "Doublequotes SEXP at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'sexp no-delimiters (interactive-p)))

(defalias 'ar-slash-sexp-atpt 'ar-sexp-slash-atpt)
(defun ar-sexp-slash-atpt (&optional no-delimiters)
  "Doublequotes SEXP at point if any. "
  (interactive "*p")
  (ar-th-slash 'sexp no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-sexp-atpt 'ar-sexp-double-backslash-atpt)
(defun ar-sexp-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SEXP at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'sexp no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-sexp-atpt 'ar-sexp-doubleslash-atpt)
(defun ar-sexp-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around SEXP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'sexp no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-sexp-atpt 'ar-sexp-doubleslash-paren-atpt)
(defun ar-sexp-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SEXP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'sexp no-delimiters (interactive-p)))

(defalias 'ar-slashparen-sexp-atpt 'ar-sexp-slashparen-atpt)
(defun ar-sexp-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SEXP at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'sexp no-delimiters (interactive-p)))

(defalias 'ar-dollar-sexp-atpt 'ar-sexp-dollar-atpt)
(defun ar-sexp-dollar-atpt (&optional no-delimiters)
  "Doublequotes SEXP at point if any. "
  (interactive "*p")
  (ar-th-dollar 'sexp no-delimiters (interactive-p)))

(defalias 'ar-equalize-sexp-atpt 'ar-sexp-equalize-atpt)
(defun ar-sexp-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around SEXP at point if any. "
  (interactive "*p")
  (ar-th-equalize 'sexp no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-sexp-atpt 'ar-sexp-greater-angle-atpt)
(defun ar-sexp-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for SEXP after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'sexp no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-sexp-atpt 'ar-sexp-lesser-angle-atpt)
(defun ar-sexp-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for SEXP after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'sexp no-delimiters (interactive-p)))

(defalias 'ar-backslash-sexp-atpt 'ar-sexp-backslash-atpt)
(defun ar-sexp-backslash-atpt (&optional no-delimiters)
  "Backslash SEXP at point if any. "
  (interactive "*p")
  (ar-th-backslash 'sexp no-delimiters (interactive-p)))

(defalias 'ar-brace-sexp-atpt 'ar-sexp-brace-atpt)
(defun ar-sexp-brace-atpt (&optional no-delimiters)
  "Braces SEXP at point if any. "
  (interactive "*p")
  (ar-th-brace 'sexp no-delimiters (interactive-p)))

(defalias 'ar-bracket-sexp-atpt 'ar-sexp-bracket-atpt)
(defun ar-sexp-bracket-atpt (&optional no-delimiters)
  "Brackets SEXP after point if any. "
  (interactive "*p")
  (ar-th-bracket 'sexp no-delimiters (interactive-p)))

(defun ar-comment-sexp-atpt (&optional no-delimiters)
  "Comments SEXP at point if any. "
  (interactive "*p")
  (ar-th-comment 'sexp no-delimiters (interactive-p)))

(defun ar-commatize-sexp-atpt (&optional no-delimiters)
  "Put a comma after SEXP at point if any. "
  (interactive "*p")
  (ar-th-commatize 'sexp no-delimiters (interactive-p)))

(defun ar-quote-sexp-atpt (&optional no-delimiters)
  "Put a singlequote before SEXP at point if any. "
  (interactive "*p")
  (ar-th-quote 'sexp no-delimiters (interactive-p)))

(defalias 'ar-hyphen-sexp-atpt 'ar-sexp-hyphen-atpt)
(defun ar-sexp-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around SEXP at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'sexp no-delimiters (interactive-p)))

(defalias 'ar-mark-sexp-atpt 'ar-sexp-mark-atpt)
(defun ar-sexp-mark-atpt ()
  "Marks SEXP at point if any. "
  (interactive)
  (ar-th-mark 'sexp))

(defalias 'ar-hide-sexp-atpt 'ar-sexp-hide-atpt)
(defun ar-sexp-hide-atpt ()
  "Hides SEXP at point. "
  (interactive)
  (ar-th-hide 'sexp))

(defalias 'ar-show-sexp-atpt 'ar-sexp-show-atpt)
(defun ar-sexp-show-atpt ()
  "Shows hidden SEXP at point. "
  (interactive)
  (ar-th-show 'sexp))

(defalias 'ar-hide-show-sexp-atpt 'ar-sexp-hide-show-atpt)
(defun ar-sexp-hide-show-atpt ()
  "Alternatively hides or shows SEXP at point. "
  (interactive)
  (ar-th-hide-show 'sexp))

(defalias 'ar-highlight-sexp-atpt-mode 'ar-sexp-highlight-atpt-mode)

(defun ar-sexp-highlight-atpt-mode (&optional no-delimiters)
  "Toggles sexp-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'sexp no-delimiters (interactive-p)))

(defalias 'ar-kill-sexp-atpt 'ar-sexp-kill-atpt)
(defun ar-sexp-kill-atpt (&optional no-delimiters)
  "Kills SEXP at point if any. "
  (interactive "*P")
  (ar-th-kill 'sexp no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-sexp-atpt 'ar-sexp-kill-backward-atpt)
(defun ar-sexp-kill-backward-atpt (&optional no-delimiters)
  "Kills SEXP at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'sexp no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-sexp-atpt 'ar-sexp-left-right-singlequote-atpt)
(defun ar-sexp-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'sexp no-delimiters (interactive-p)))

(defalias 'ar-parentize-sexp-atpt 'ar-sexp-parentize-atpt)
(defun ar-sexp-parentize-atpt (&optional no-delimiters)
  "Parentizes SEXP at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'sexp no-delimiters (interactive-p)))

(defalias 'ar-separate-sexp-atpt 'ar-sexp-separate-atpt)
(defun ar-sexp-separate-atpt (&optional no-delimiters)
  "Separates SEXP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'sexp no-delimiters (interactive-p)))

(defalias 'ar-singlequote-sexp-atpt 'ar-sexp-singlequote-atpt)
(defun ar-sexp-singlequote-atpt (&optional no-delimiters)
  "Singlequotes SEXP at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'sexp no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-sexp-atpt 'ar-sexp-triplequote-dq-atpt)
(defun ar-sexp-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around sexp. "
  (interactive "*p")
  (ar-th-triplequote-dq 'sexp no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-sexp-atpt 'ar-sexp-triplequote-sq-atpt)
(defun ar-sexp-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around sexp. "
  (interactive "*p")
  (ar-th-triplequote-sq 'sexp no-delimiters (interactive-p)))

(defalias 'ar-trim-sexp-atpt 'ar-sexp-trim-atpt)
(defun ar-sexp-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sexp t t))
    (ar-th-trim 'sexp t t)))

(defalias 'ar-trim-left-sexp-atpt 'ar-sexp-left-trim-atpt)
(defun ar-sexp-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sexp t nil))
    (ar-th-trim 'sexp t nil)))

(defalias 'ar-trim-right-sexp-atpt 'ar-sexp-right-trim-atpt)
(defun ar-sexp-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sexp nil t))
    (ar-th-trim 'sexp nil t)))

(defalias 'ar-sexp-underscore-atpt 'ar-underscore-sexp-atpt)
(defun ar-underscore-sexp-atpt (&optional no-delimiters)
  "Put underscore char around SEXP. "
  (interactive "*p")
  (ar-th-underscore 'sexp no-delimiters (interactive-p)))

(defalias 'ar-sexp-whitespace-atpt 'ar-whitespace-sexp-atpt)
(defun ar-whitespace-sexp-atpt (&optional no-delimiters)
  "Put whitespace char around SEXP. "
  (interactive "*p")
  (ar-th-whitespace 'sexp nil t))

(defalias 'ar-forward-sexp-atpt 'ar-sexp-forward-atpt)
(defun ar-sexp-forward-atpt (&optional arg)
  "Moves forward over SEXP at point if any, does nothing otherwise.
Returns end position of SEXP "
  (interactive "p")
  (ar-th-forward 'sexp arg (interactive-p)))

(defalias 'ar-backward-sexp-atpt 'ar-sexp-backward-atpt)
(defun ar-sexp-backward-atpt (&optional arg)
  "Moves backward over SEXP before point if any, does nothing otherwise.
Returns beginning position of SEXP "
  (interactive "p")
  (ar-th-backward 'sexp arg (interactive-p)))

(defalias 'ar-transpose-sexp-atpt 'ar-sexp-transpose-atpt)
(defun ar-sexp-transpose-atpt (&optional arg)
  "Transposes SEXP with SEXP before point if any. "
  (interactive "*p")
  (ar-th-transpose 'sexp arg (interactive-p)))

(defalias 'ar-sort-sexp-atpt 'ar-sexp-sort-atpt)
(defun ar-sexp-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts sexps in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'sexp reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-sexp-atpt 'ar-sexp-check-atpt)
(defun ar-sexp-check-atpt ()
  "Return t if a SEXP at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-sexp-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-sexp-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-string-atpt (&optional arg no-delimiters)
  "Returns string at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'string arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-string-atpt 'ar-string-bounds-atpt)
(defun ar-string-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of string if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defun ar-string-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position STRING at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'string no-delimiters (interactive-p)))

(defun ar-string-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'string no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-string-atpt 'ar-string-beginning-atpt)
(defun ar-string-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'string no-delimiters (interactive-p)))

(defalias 'ar-end-of-string-atpt 'ar-string-end-atpt)
(defun ar-string-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'string no-delimiters (interactive-p)))

(defalias 'ar-in-string-p-atpt 'ar-string-in-p-atpt)
(defun ar-string-in-p-atpt (&optional no-delimiters)
  "Returns bounds of STRING at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defalias 'ar-length-of-string-atpt 'ar-string-length-atpt)
(defun ar-string-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'string no-delimiters (interactive-p)))

(defalias 'ar-copy-string-atpt 'ar-string-copy-atpt)
(defun ar-string-copy-atpt (&optional no-delimiters)
  "Returns a copy of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'string no-delimiters (interactive-p)))

(defalias 'ar-delete-string-in-region 'ar-string-delete-in-region)
(defun ar-string-delete-in-region (beg end)
  "Deletes STRING at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'string beg end (interactive-p)))

(defun ar-blok-string-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around string.
  Returns blok or nil if no STRING at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'string no-delimiters (interactive-p)))

(defalias 'ar-escape-string-atpt 'ar-string-escape-atpt)
(defun ar-string-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted STRING at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'string no-delimiters))

(defalias 'ar-doublequote-string-atpt 'ar-string-doublequote-atpt)
(defun ar-string-doublequote-atpt (&optional no-delimiters)
  "Doublequotes STRING at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'string no-delimiters (interactive-p)))

(defalias 'ar-slash-string-atpt 'ar-string-slash-atpt)
(defun ar-string-slash-atpt (&optional no-delimiters)
  "Doublequotes STRING at point if any. "
  (interactive "*p")
  (ar-th-slash 'string no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-string-atpt 'ar-string-double-backslash-atpt)
(defun ar-string-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'string no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-string-atpt 'ar-string-doubleslash-atpt)
(defun ar-string-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'string no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-string-atpt 'ar-string-doubleslash-paren-atpt)
(defun ar-string-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around STRING at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'string no-delimiters (interactive-p)))

(defalias 'ar-slashparen-string-atpt 'ar-string-slashparen-atpt)
(defun ar-string-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around STRING at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'string no-delimiters (interactive-p)))

(defalias 'ar-dollar-string-atpt 'ar-string-dollar-atpt)
(defun ar-string-dollar-atpt (&optional no-delimiters)
  "Doublequotes STRING at point if any. "
  (interactive "*p")
  (ar-th-dollar 'string no-delimiters (interactive-p)))

(defalias 'ar-equalize-string-atpt 'ar-string-equalize-atpt)
(defun ar-string-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around STRING at point if any. "
  (interactive "*p")
  (ar-th-equalize 'string no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-string-atpt 'ar-string-greater-angle-atpt)
(defun ar-string-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for STRING after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'string no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-string-atpt 'ar-string-lesser-angle-atpt)
(defun ar-string-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for STRING after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'string no-delimiters (interactive-p)))

(defalias 'ar-backslash-string-atpt 'ar-string-backslash-atpt)
(defun ar-string-backslash-atpt (&optional no-delimiters)
  "Backslash STRING at point if any. "
  (interactive "*p")
  (ar-th-backslash 'string no-delimiters (interactive-p)))

(defalias 'ar-brace-string-atpt 'ar-string-brace-atpt)
(defun ar-string-brace-atpt (&optional no-delimiters)
  "Braces STRING at point if any. "
  (interactive "*p")
  (ar-th-brace 'string no-delimiters (interactive-p)))

(defalias 'ar-bracket-string-atpt 'ar-string-bracket-atpt)
(defun ar-string-bracket-atpt (&optional no-delimiters)
  "Brackets STRING after point if any. "
  (interactive "*p")
  (ar-th-bracket 'string no-delimiters (interactive-p)))

(defun ar-comment-string-atpt (&optional no-delimiters)
  "Comments STRING at point if any. "
  (interactive "*p")
  (ar-th-comment 'string no-delimiters (interactive-p)))

(defun ar-commatize-string-atpt (&optional no-delimiters)
  "Put a comma after STRING at point if any. "
  (interactive "*p")
  (ar-th-commatize 'string no-delimiters (interactive-p)))

(defun ar-quote-string-atpt (&optional no-delimiters)
  "Put a singlequote before STRING at point if any. "
  (interactive "*p")
  (ar-th-quote 'string no-delimiters (interactive-p)))

(defalias 'ar-hyphen-string-atpt 'ar-string-hyphen-atpt)
(defun ar-string-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around STRING at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'string no-delimiters (interactive-p)))

(defalias 'ar-mark-string-atpt 'ar-string-mark-atpt)
(defun ar-string-mark-atpt ()
  "Marks STRING at point if any. "
  (interactive)
  (ar-th-mark 'string))

(defalias 'ar-hide-string-atpt 'ar-string-hide-atpt)
(defun ar-string-hide-atpt ()
  "Hides STRING at point. "
  (interactive)
  (ar-th-hide 'string))

(defalias 'ar-show-string-atpt 'ar-string-show-atpt)
(defun ar-string-show-atpt ()
  "Shows hidden STRING at point. "
  (interactive)
  (ar-th-show 'string))

(defalias 'ar-hide-show-string-atpt 'ar-string-hide-show-atpt)
(defun ar-string-hide-show-atpt ()
  "Alternatively hides or shows STRING at point. "
  (interactive)
  (ar-th-hide-show 'string))

(defalias 'ar-highlight-string-atpt-mode 'ar-string-highlight-atpt-mode)

(defun ar-string-highlight-atpt-mode (&optional no-delimiters)
  "Toggles string-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-string-atpt 'ar-string-kill-atpt)
(defun ar-string-kill-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-string-atpt 'ar-string-kill-backward-atpt)
(defun ar-string-kill-backward-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'string no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-string-atpt 'ar-string-left-right-singlequote-atpt)
(defun ar-string-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'string no-delimiters (interactive-p)))

(defalias 'ar-parentize-string-atpt 'ar-string-parentize-atpt)
(defun ar-string-parentize-atpt (&optional no-delimiters)
  "Parentizes STRING at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'string no-delimiters (interactive-p)))

(defalias 'ar-separate-string-atpt 'ar-string-separate-atpt)
(defun ar-string-separate-atpt (&optional no-delimiters)
  "Separates STRING at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'string no-delimiters (interactive-p)))

(defalias 'ar-singlequote-string-atpt 'ar-string-singlequote-atpt)
(defun ar-string-singlequote-atpt (&optional no-delimiters)
  "Singlequotes STRING at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-string-atpt 'ar-string-triplequote-dq-atpt)
(defun ar-string-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around string. "
  (interactive "*p")
  (ar-th-triplequote-dq 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-string-atpt 'ar-string-triplequote-sq-atpt)
(defun ar-string-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around string. "
  (interactive "*p")
  (ar-th-triplequote-sq 'string no-delimiters (interactive-p)))

(defalias 'ar-trim-string-atpt 'ar-string-trim-atpt)
(defun ar-string-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'string t t))
    (ar-th-trim 'string t t)))

(defalias 'ar-trim-left-string-atpt 'ar-string-left-trim-atpt)
(defun ar-string-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'string t nil))
    (ar-th-trim 'string t nil)))

(defalias 'ar-trim-right-string-atpt 'ar-string-right-trim-atpt)
(defun ar-string-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'string nil t))
    (ar-th-trim 'string nil t)))

(defalias 'ar-string-underscore-atpt 'ar-underscore-string-atpt)
(defun ar-underscore-string-atpt (&optional no-delimiters)
  "Put underscore char around STRING. "
  (interactive "*p")
  (ar-th-underscore 'string no-delimiters (interactive-p)))

(defalias 'ar-string-whitespace-atpt 'ar-whitespace-string-atpt)
(defun ar-whitespace-string-atpt (&optional no-delimiters)
  "Put whitespace char around STRING. "
  (interactive "*p")
  (ar-th-whitespace 'string nil t))

(defalias 'ar-forward-string-atpt 'ar-string-forward-atpt)
(defun ar-string-forward-atpt (&optional arg)
  "Moves forward over STRING at point if any, does nothing otherwise.
Returns end position of STRING "
  (interactive "p")
  (ar-th-forward 'string arg (interactive-p)))

(defalias 'ar-backward-string-atpt 'ar-string-backward-atpt)
(defun ar-string-backward-atpt (&optional arg)
  "Moves backward over STRING before point if any, does nothing otherwise.
Returns beginning position of STRING "
  (interactive "p")
  (ar-th-backward 'string arg (interactive-p)))

(defalias 'ar-transpose-string-atpt 'ar-string-transpose-atpt)
(defun ar-string-transpose-atpt (&optional arg)
  "Transposes STRING with STRING before point if any. "
  (interactive "*p")
  (ar-th-transpose 'string arg (interactive-p)))

(defalias 'ar-sort-string-atpt 'ar-string-sort-atpt)
(defun ar-string-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts strings in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'string reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-string-atpt 'ar-string-check-atpt)
(defun ar-string-check-atpt ()
  "Return t if a STRING at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-string-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-string-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-sh-struct-atpt (&optional arg no-delimiters)
  "Returns sh-struct at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'sh-struct arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-sh-struct-atpt 'ar-sh-struct-bounds-atpt)
(defun ar-sh-struct-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of sh-struct if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sh-struct no-delimiters (interactive-p)))

(defun ar-sh-struct-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SH-STRUCT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sh-struct no-delimiters (interactive-p)))

(defun ar-sh-struct-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SH-STRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-sh-struct-atpt 'ar-sh-struct-beginning-atpt)
(defun ar-sh-struct-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SH-STRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-end-of-sh-struct-atpt 'ar-sh-struct-end-atpt)
(defun ar-sh-struct-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SH-STRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-in-sh-struct-p-atpt 'ar-sh-struct-in-p-atpt)
(defun ar-sh-struct-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SH-STRUCT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-length-of-sh-struct-atpt 'ar-sh-struct-length-atpt)
(defun ar-sh-struct-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SH-STRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-copy-sh-struct-atpt 'ar-sh-struct-copy-atpt)
(defun ar-sh-struct-copy-atpt (&optional no-delimiters)
  "Returns a copy of SH-STRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-delete-sh-struct-in-region 'ar-sh-struct-delete-in-region)
(defun ar-sh-struct-delete-in-region (beg end)
  "Deletes SH-STRUCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sh-struct beg end (interactive-p)))

(defun ar-blok-sh-struct-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around sh-struct.
  Returns blok or nil if no SH-STRUCT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-escape-sh-struct-atpt 'ar-sh-struct-escape-atpt)
(defun ar-sh-struct-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted SH-STRUCT at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'sh-struct no-delimiters))

(defalias 'ar-doublequote-sh-struct-atpt 'ar-sh-struct-doublequote-atpt)
(defun ar-sh-struct-doublequote-atpt (&optional no-delimiters)
  "Doublequotes SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-slash-sh-struct-atpt 'ar-sh-struct-slash-atpt)
(defun ar-sh-struct-slash-atpt (&optional no-delimiters)
  "Doublequotes SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-slash 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-sh-struct-atpt 'ar-sh-struct-double-backslash-atpt)
(defun ar-sh-struct-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-sh-struct-atpt 'ar-sh-struct-doubleslash-atpt)
(defun ar-sh-struct-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-sh-struct-atpt 'ar-sh-struct-doubleslash-paren-atpt)
(defun ar-sh-struct-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-slashparen-sh-struct-atpt 'ar-sh-struct-slashparen-atpt)
(defun ar-sh-struct-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-dollar-sh-struct-atpt 'ar-sh-struct-dollar-atpt)
(defun ar-sh-struct-dollar-atpt (&optional no-delimiters)
  "Doublequotes SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-dollar 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-equalize-sh-struct-atpt 'ar-sh-struct-equalize-atpt)
(defun ar-sh-struct-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-equalize 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-sh-struct-atpt 'ar-sh-struct-greater-angle-atpt)
(defun ar-sh-struct-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for SH-STRUCT after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-sh-struct-atpt 'ar-sh-struct-lesser-angle-atpt)
(defun ar-sh-struct-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for SH-STRUCT after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-backslash-sh-struct-atpt 'ar-sh-struct-backslash-atpt)
(defun ar-sh-struct-backslash-atpt (&optional no-delimiters)
  "Backslash SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-backslash 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-brace-sh-struct-atpt 'ar-sh-struct-brace-atpt)
(defun ar-sh-struct-brace-atpt (&optional no-delimiters)
  "Braces SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-brace 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-bracket-sh-struct-atpt 'ar-sh-struct-bracket-atpt)
(defun ar-sh-struct-bracket-atpt (&optional no-delimiters)
  "Brackets SH-STRUCT after point if any. "
  (interactive "*p")
  (ar-th-bracket 'sh-struct no-delimiters (interactive-p)))

(defun ar-comment-sh-struct-atpt (&optional no-delimiters)
  "Comments SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-comment 'sh-struct no-delimiters (interactive-p)))

(defun ar-commatize-sh-struct-atpt (&optional no-delimiters)
  "Put a comma after SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'sh-struct no-delimiters (interactive-p)))

(defun ar-quote-sh-struct-atpt (&optional no-delimiters)
  "Put a singlequote before SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-quote 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-hyphen-sh-struct-atpt 'ar-sh-struct-hyphen-atpt)
(defun ar-sh-struct-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-mark-sh-struct-atpt 'ar-sh-struct-mark-atpt)
(defun ar-sh-struct-mark-atpt ()
  "Marks SH-STRUCT at point if any. "
  (interactive)
  (ar-th-mark 'sh-struct))

(defalias 'ar-hide-sh-struct-atpt 'ar-sh-struct-hide-atpt)
(defun ar-sh-struct-hide-atpt ()
  "Hides SH-STRUCT at point. "
  (interactive)
  (ar-th-hide 'sh-struct))

(defalias 'ar-show-sh-struct-atpt 'ar-sh-struct-show-atpt)
(defun ar-sh-struct-show-atpt ()
  "Shows hidden SH-STRUCT at point. "
  (interactive)
  (ar-th-show 'sh-struct))

(defalias 'ar-hide-show-sh-struct-atpt 'ar-sh-struct-hide-show-atpt)
(defun ar-sh-struct-hide-show-atpt ()
  "Alternatively hides or shows SH-STRUCT at point. "
  (interactive)
  (ar-th-hide-show 'sh-struct))

(defalias 'ar-highlight-sh-struct-atpt-mode 'ar-sh-struct-highlight-atpt-mode)

(defun ar-sh-struct-highlight-atpt-mode (&optional no-delimiters)
  "Toggles sh-struct-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-kill-sh-struct-atpt 'ar-sh-struct-kill-atpt)
(defun ar-sh-struct-kill-atpt (&optional no-delimiters)
  "Kills SH-STRUCT at point if any. "
  (interactive "*P")
  (ar-th-kill 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-sh-struct-atpt 'ar-sh-struct-kill-backward-atpt)
(defun ar-sh-struct-kill-backward-atpt (&optional no-delimiters)
  "Kills SH-STRUCT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-sh-struct-atpt 'ar-sh-struct-left-right-singlequote-atpt)
(defun ar-sh-struct-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-parentize-sh-struct-atpt 'ar-sh-struct-parentize-atpt)
(defun ar-sh-struct-parentize-atpt (&optional no-delimiters)
  "Parentizes SH-STRUCT at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-separate-sh-struct-atpt 'ar-sh-struct-separate-atpt)
(defun ar-sh-struct-separate-atpt (&optional no-delimiters)
  "Separates SH-STRUCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-singlequote-sh-struct-atpt 'ar-sh-struct-singlequote-atpt)
(defun ar-sh-struct-singlequote-atpt (&optional no-delimiters)
  "Singlequotes SH-STRUCT at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-sh-struct-atpt 'ar-sh-struct-triplequote-dq-atpt)
(defun ar-sh-struct-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around sh-struct. "
  (interactive "*p")
  (ar-th-triplequote-dq 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-sh-struct-atpt 'ar-sh-struct-triplequote-sq-atpt)
(defun ar-sh-struct-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around sh-struct. "
  (interactive "*p")
  (ar-th-triplequote-sq 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-trim-sh-struct-atpt 'ar-sh-struct-trim-atpt)
(defun ar-sh-struct-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sh-struct t t))
    (ar-th-trim 'sh-struct t t)))

(defalias 'ar-trim-left-sh-struct-atpt 'ar-sh-struct-left-trim-atpt)
(defun ar-sh-struct-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sh-struct t nil))
    (ar-th-trim 'sh-struct t nil)))

(defalias 'ar-trim-right-sh-struct-atpt 'ar-sh-struct-right-trim-atpt)
(defun ar-sh-struct-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'sh-struct nil t))
    (ar-th-trim 'sh-struct nil t)))

(defalias 'ar-sh-struct-underscore-atpt 'ar-underscore-sh-struct-atpt)
(defun ar-underscore-sh-struct-atpt (&optional no-delimiters)
  "Put underscore char around SH-STRUCT. "
  (interactive "*p")
  (ar-th-underscore 'sh-struct no-delimiters (interactive-p)))

(defalias 'ar-sh-struct-whitespace-atpt 'ar-whitespace-sh-struct-atpt)
(defun ar-whitespace-sh-struct-atpt (&optional no-delimiters)
  "Put whitespace char around SH-STRUCT. "
  (interactive "*p")
  (ar-th-whitespace 'sh-struct nil t))

(defalias 'ar-forward-sh-struct-atpt 'ar-sh-struct-forward-atpt)
(defun ar-sh-struct-forward-atpt (&optional arg)
  "Moves forward over SH-STRUCT at point if any, does nothing otherwise.
Returns end position of SH-STRUCT "
  (interactive "p")
  (ar-th-forward 'sh-struct arg (interactive-p)))

(defalias 'ar-backward-sh-struct-atpt 'ar-sh-struct-backward-atpt)
(defun ar-sh-struct-backward-atpt (&optional arg)
  "Moves backward over SH-STRUCT before point if any, does nothing otherwise.
Returns beginning position of SH-STRUCT "
  (interactive "p")
  (ar-th-backward 'sh-struct arg (interactive-p)))

(defalias 'ar-transpose-sh-struct-atpt 'ar-sh-struct-transpose-atpt)
(defun ar-sh-struct-transpose-atpt (&optional arg)
  "Transposes SH-STRUCT with SH-STRUCT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'sh-struct arg (interactive-p)))

(defalias 'ar-sort-sh-struct-atpt 'ar-sh-struct-sort-atpt)
(defun ar-sh-struct-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts sh-structs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'sh-struct reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-sh-struct-atpt 'ar-sh-struct-check-atpt)
(defun ar-sh-struct-check-atpt ()
  "Return t if a SH-STRUCT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-sh-struct-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-sh-struct-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-symbol-atpt (&optional arg no-delimiters)
  "Returns symbol at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'symbol arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-symbol-atpt 'ar-symbol-bounds-atpt)
(defun ar-symbol-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of symbol if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol no-delimiters (interactive-p)))

(defun ar-symbol-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SYMBOL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'symbol no-delimiters (interactive-p)))

(defun ar-symbol-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'symbol no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-symbol-atpt 'ar-symbol-beginning-atpt)
(defun ar-symbol-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'symbol no-delimiters (interactive-p)))

(defalias 'ar-end-of-symbol-atpt 'ar-symbol-end-atpt)
(defun ar-symbol-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'symbol no-delimiters (interactive-p)))

(defalias 'ar-in-symbol-p-atpt 'ar-symbol-in-p-atpt)
(defun ar-symbol-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SYMBOL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol no-delimiters (interactive-p)))

(defalias 'ar-length-of-symbol-atpt 'ar-symbol-length-atpt)
(defun ar-symbol-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'symbol no-delimiters (interactive-p)))

(defalias 'ar-copy-symbol-atpt 'ar-symbol-copy-atpt)
(defun ar-symbol-copy-atpt (&optional no-delimiters)
  "Returns a copy of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'symbol no-delimiters (interactive-p)))

(defalias 'ar-delete-symbol-in-region 'ar-symbol-delete-in-region)
(defun ar-symbol-delete-in-region (beg end)
  "Deletes SYMBOL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'symbol beg end (interactive-p)))

(defun ar-blok-symbol-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around symbol.
  Returns blok or nil if no SYMBOL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'symbol no-delimiters (interactive-p)))

(defalias 'ar-escape-symbol-atpt 'ar-symbol-escape-atpt)
(defun ar-symbol-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted SYMBOL at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'symbol no-delimiters))

(defalias 'ar-doublequote-symbol-atpt 'ar-symbol-doublequote-atpt)
(defun ar-symbol-doublequote-atpt (&optional no-delimiters)
  "Doublequotes SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'symbol no-delimiters (interactive-p)))

(defalias 'ar-slash-symbol-atpt 'ar-symbol-slash-atpt)
(defun ar-symbol-slash-atpt (&optional no-delimiters)
  "Doublequotes SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-slash 'symbol no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-symbol-atpt 'ar-symbol-double-backslash-atpt)
(defun ar-symbol-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'symbol no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-symbol-atpt 'ar-symbol-doubleslash-atpt)
(defun ar-symbol-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'symbol no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-symbol-atpt 'ar-symbol-doubleslash-paren-atpt)
(defun ar-symbol-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'symbol no-delimiters (interactive-p)))

(defalias 'ar-slashparen-symbol-atpt 'ar-symbol-slashparen-atpt)
(defun ar-symbol-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'symbol no-delimiters (interactive-p)))

(defalias 'ar-dollar-symbol-atpt 'ar-symbol-dollar-atpt)
(defun ar-symbol-dollar-atpt (&optional no-delimiters)
  "Doublequotes SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-dollar 'symbol no-delimiters (interactive-p)))

(defalias 'ar-equalize-symbol-atpt 'ar-symbol-equalize-atpt)
(defun ar-symbol-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-equalize 'symbol no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-symbol-atpt 'ar-symbol-greater-angle-atpt)
(defun ar-symbol-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for SYMBOL after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'symbol no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-symbol-atpt 'ar-symbol-lesser-angle-atpt)
(defun ar-symbol-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for SYMBOL after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'symbol no-delimiters (interactive-p)))

(defalias 'ar-backslash-symbol-atpt 'ar-symbol-backslash-atpt)
(defun ar-symbol-backslash-atpt (&optional no-delimiters)
  "Backslash SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-backslash 'symbol no-delimiters (interactive-p)))

(defalias 'ar-brace-symbol-atpt 'ar-symbol-brace-atpt)
(defun ar-symbol-brace-atpt (&optional no-delimiters)
  "Braces SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-brace 'symbol no-delimiters (interactive-p)))

(defalias 'ar-bracket-symbol-atpt 'ar-symbol-bracket-atpt)
(defun ar-symbol-bracket-atpt (&optional no-delimiters)
  "Brackets SYMBOL after point if any. "
  (interactive "*p")
  (ar-th-bracket 'symbol no-delimiters (interactive-p)))

(defun ar-comment-symbol-atpt (&optional no-delimiters)
  "Comments SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-comment 'symbol no-delimiters (interactive-p)))

(defun ar-commatize-symbol-atpt (&optional no-delimiters)
  "Put a comma after SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'symbol no-delimiters (interactive-p)))

(defun ar-quote-symbol-atpt (&optional no-delimiters)
  "Put a singlequote before SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-quote 'symbol no-delimiters (interactive-p)))

(defalias 'ar-hyphen-symbol-atpt 'ar-symbol-hyphen-atpt)
(defun ar-symbol-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'symbol no-delimiters (interactive-p)))

(defalias 'ar-mark-symbol-atpt 'ar-symbol-mark-atpt)
(defun ar-symbol-mark-atpt ()
  "Marks SYMBOL at point if any. "
  (interactive)
  (ar-th-mark 'symbol))

(defalias 'ar-hide-symbol-atpt 'ar-symbol-hide-atpt)
(defun ar-symbol-hide-atpt ()
  "Hides SYMBOL at point. "
  (interactive)
  (ar-th-hide 'symbol))

(defalias 'ar-show-symbol-atpt 'ar-symbol-show-atpt)
(defun ar-symbol-show-atpt ()
  "Shows hidden SYMBOL at point. "
  (interactive)
  (ar-th-show 'symbol))

(defalias 'ar-hide-show-symbol-atpt 'ar-symbol-hide-show-atpt)
(defun ar-symbol-hide-show-atpt ()
  "Alternatively hides or shows SYMBOL at point. "
  (interactive)
  (ar-th-hide-show 'symbol))

(defalias 'ar-highlight-symbol-atpt-mode 'ar-symbol-highlight-atpt-mode)

(defun ar-symbol-highlight-atpt-mode (&optional no-delimiters)
  "Toggles symbol-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'symbol no-delimiters (interactive-p)))

(defalias 'ar-kill-symbol-atpt 'ar-symbol-kill-atpt)
(defun ar-symbol-kill-atpt (&optional no-delimiters)
  "Kills SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-kill 'symbol no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-symbol-atpt 'ar-symbol-kill-backward-atpt)
(defun ar-symbol-kill-backward-atpt (&optional no-delimiters)
  "Kills SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'symbol no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-symbol-atpt 'ar-symbol-left-right-singlequote-atpt)
(defun ar-symbol-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'symbol no-delimiters (interactive-p)))

(defalias 'ar-parentize-symbol-atpt 'ar-symbol-parentize-atpt)
(defun ar-symbol-parentize-atpt (&optional no-delimiters)
  "Parentizes SYMBOL at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'symbol no-delimiters (interactive-p)))

(defalias 'ar-separate-symbol-atpt 'ar-symbol-separate-atpt)
(defun ar-symbol-separate-atpt (&optional no-delimiters)
  "Separates SYMBOL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'symbol no-delimiters (interactive-p)))

(defalias 'ar-singlequote-symbol-atpt 'ar-symbol-singlequote-atpt)
(defun ar-symbol-singlequote-atpt (&optional no-delimiters)
  "Singlequotes SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'symbol no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-symbol-atpt 'ar-symbol-triplequote-dq-atpt)
(defun ar-symbol-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around symbol. "
  (interactive "*p")
  (ar-th-triplequote-dq 'symbol no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-symbol-atpt 'ar-symbol-triplequote-sq-atpt)
(defun ar-symbol-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around symbol. "
  (interactive "*p")
  (ar-th-triplequote-sq 'symbol no-delimiters (interactive-p)))

(defalias 'ar-trim-symbol-atpt 'ar-symbol-trim-atpt)
(defun ar-symbol-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'symbol t t))
    (ar-th-trim 'symbol t t)))

(defalias 'ar-trim-left-symbol-atpt 'ar-symbol-left-trim-atpt)
(defun ar-symbol-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'symbol t nil))
    (ar-th-trim 'symbol t nil)))

(defalias 'ar-trim-right-symbol-atpt 'ar-symbol-right-trim-atpt)
(defun ar-symbol-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'symbol nil t))
    (ar-th-trim 'symbol nil t)))

(defalias 'ar-symbol-underscore-atpt 'ar-underscore-symbol-atpt)
(defun ar-underscore-symbol-atpt (&optional no-delimiters)
  "Put underscore char around SYMBOL. "
  (interactive "*p")
  (ar-th-underscore 'symbol no-delimiters (interactive-p)))

(defalias 'ar-symbol-whitespace-atpt 'ar-whitespace-symbol-atpt)
(defun ar-whitespace-symbol-atpt (&optional no-delimiters)
  "Put whitespace char around SYMBOL. "
  (interactive "*p")
  (ar-th-whitespace 'symbol nil t))

(defalias 'ar-forward-symbol-atpt 'ar-symbol-forward-atpt)
(defun ar-symbol-forward-atpt (&optional arg)
  "Moves forward over SYMBOL at point if any, does nothing otherwise.
Returns end position of SYMBOL "
  (interactive "p")
  (ar-th-forward 'symbol arg (interactive-p)))

(defalias 'ar-backward-symbol-atpt 'ar-symbol-backward-atpt)
(defun ar-symbol-backward-atpt (&optional arg)
  "Moves backward over SYMBOL before point if any, does nothing otherwise.
Returns beginning position of SYMBOL "
  (interactive "p")
  (ar-th-backward 'symbol arg (interactive-p)))

(defalias 'ar-transpose-symbol-atpt 'ar-symbol-transpose-atpt)
(defun ar-symbol-transpose-atpt (&optional arg)
  "Transposes SYMBOL with SYMBOL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'symbol arg (interactive-p)))

(defalias 'ar-sort-symbol-atpt 'ar-symbol-sort-atpt)
(defun ar-symbol-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts symbols in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'symbol reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-symbol-atpt 'ar-symbol-check-atpt)
(defun ar-symbol-check-atpt ()
  "Return t if a SYMBOL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-symbol-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-symbol-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-url-atpt (&optional arg no-delimiters)
  "Returns url at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'url arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-url-atpt 'ar-url-bounds-atpt)
(defun ar-url-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of url if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url no-delimiters (interactive-p)))

(defun ar-url-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position URL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'url no-delimiters (interactive-p)))

(defun ar-url-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'url no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-url-atpt 'ar-url-beginning-atpt)
(defun ar-url-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'url no-delimiters (interactive-p)))

(defalias 'ar-end-of-url-atpt 'ar-url-end-atpt)
(defun ar-url-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'url no-delimiters (interactive-p)))

(defalias 'ar-in-url-p-atpt 'ar-url-in-p-atpt)
(defun ar-url-in-p-atpt (&optional no-delimiters)
  "Returns bounds of URL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url no-delimiters (interactive-p)))

(defalias 'ar-length-of-url-atpt 'ar-url-length-atpt)
(defun ar-url-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'url no-delimiters (interactive-p)))

(defalias 'ar-copy-url-atpt 'ar-url-copy-atpt)
(defun ar-url-copy-atpt (&optional no-delimiters)
  "Returns a copy of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'url no-delimiters (interactive-p)))

(defalias 'ar-delete-url-in-region 'ar-url-delete-in-region)
(defun ar-url-delete-in-region (beg end)
  "Deletes URL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'url beg end (interactive-p)))

(defun ar-blok-url-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around url.
  Returns blok or nil if no URL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'url no-delimiters (interactive-p)))

(defalias 'ar-escape-url-atpt 'ar-url-escape-atpt)
(defun ar-url-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted URL at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'url no-delimiters))

(defalias 'ar-doublequote-url-atpt 'ar-url-doublequote-atpt)
(defun ar-url-doublequote-atpt (&optional no-delimiters)
  "Doublequotes URL at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'url no-delimiters (interactive-p)))

(defalias 'ar-slash-url-atpt 'ar-url-slash-atpt)
(defun ar-url-slash-atpt (&optional no-delimiters)
  "Doublequotes URL at point if any. "
  (interactive "*p")
  (ar-th-slash 'url no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-url-atpt 'ar-url-double-backslash-atpt)
(defun ar-url-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around URL at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'url no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-url-atpt 'ar-url-doubleslash-atpt)
(defun ar-url-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around URL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'url no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-url-atpt 'ar-url-doubleslash-paren-atpt)
(defun ar-url-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around URL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'url no-delimiters (interactive-p)))

(defalias 'ar-slashparen-url-atpt 'ar-url-slashparen-atpt)
(defun ar-url-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around URL at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'url no-delimiters (interactive-p)))

(defalias 'ar-dollar-url-atpt 'ar-url-dollar-atpt)
(defun ar-url-dollar-atpt (&optional no-delimiters)
  "Doublequotes URL at point if any. "
  (interactive "*p")
  (ar-th-dollar 'url no-delimiters (interactive-p)))

(defalias 'ar-equalize-url-atpt 'ar-url-equalize-atpt)
(defun ar-url-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around URL at point if any. "
  (interactive "*p")
  (ar-th-equalize 'url no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-url-atpt 'ar-url-greater-angle-atpt)
(defun ar-url-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for URL after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'url no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-url-atpt 'ar-url-lesser-angle-atpt)
(defun ar-url-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for URL after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'url no-delimiters (interactive-p)))

(defalias 'ar-backslash-url-atpt 'ar-url-backslash-atpt)
(defun ar-url-backslash-atpt (&optional no-delimiters)
  "Backslash URL at point if any. "
  (interactive "*p")
  (ar-th-backslash 'url no-delimiters (interactive-p)))

(defalias 'ar-brace-url-atpt 'ar-url-brace-atpt)
(defun ar-url-brace-atpt (&optional no-delimiters)
  "Braces URL at point if any. "
  (interactive "*p")
  (ar-th-brace 'url no-delimiters (interactive-p)))

(defalias 'ar-bracket-url-atpt 'ar-url-bracket-atpt)
(defun ar-url-bracket-atpt (&optional no-delimiters)
  "Brackets URL after point if any. "
  (interactive "*p")
  (ar-th-bracket 'url no-delimiters (interactive-p)))

(defun ar-comment-url-atpt (&optional no-delimiters)
  "Comments URL at point if any. "
  (interactive "*p")
  (ar-th-comment 'url no-delimiters (interactive-p)))

(defun ar-commatize-url-atpt (&optional no-delimiters)
  "Put a comma after URL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'url no-delimiters (interactive-p)))

(defun ar-quote-url-atpt (&optional no-delimiters)
  "Put a singlequote before URL at point if any. "
  (interactive "*p")
  (ar-th-quote 'url no-delimiters (interactive-p)))

(defalias 'ar-hyphen-url-atpt 'ar-url-hyphen-atpt)
(defun ar-url-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around URL at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'url no-delimiters (interactive-p)))

(defalias 'ar-mark-url-atpt 'ar-url-mark-atpt)
(defun ar-url-mark-atpt ()
  "Marks URL at point if any. "
  (interactive)
  (ar-th-mark 'url))

(defalias 'ar-hide-url-atpt 'ar-url-hide-atpt)
(defun ar-url-hide-atpt ()
  "Hides URL at point. "
  (interactive)
  (ar-th-hide 'url))

(defalias 'ar-show-url-atpt 'ar-url-show-atpt)
(defun ar-url-show-atpt ()
  "Shows hidden URL at point. "
  (interactive)
  (ar-th-show 'url))

(defalias 'ar-hide-show-url-atpt 'ar-url-hide-show-atpt)
(defun ar-url-hide-show-atpt ()
  "Alternatively hides or shows URL at point. "
  (interactive)
  (ar-th-hide-show 'url))

(defalias 'ar-highlight-url-atpt-mode 'ar-url-highlight-atpt-mode)

(defun ar-url-highlight-atpt-mode (&optional no-delimiters)
  "Toggles url-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'url no-delimiters (interactive-p)))

(defalias 'ar-kill-url-atpt 'ar-url-kill-atpt)
(defun ar-url-kill-atpt (&optional no-delimiters)
  "Kills URL at point if any. "
  (interactive "*P")
  (ar-th-kill 'url no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-url-atpt 'ar-url-kill-backward-atpt)
(defun ar-url-kill-backward-atpt (&optional no-delimiters)
  "Kills URL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'url no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-url-atpt 'ar-url-left-right-singlequote-atpt)
(defun ar-url-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'url no-delimiters (interactive-p)))

(defalias 'ar-parentize-url-atpt 'ar-url-parentize-atpt)
(defun ar-url-parentize-atpt (&optional no-delimiters)
  "Parentizes URL at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'url no-delimiters (interactive-p)))

(defalias 'ar-separate-url-atpt 'ar-url-separate-atpt)
(defun ar-url-separate-atpt (&optional no-delimiters)
  "Separates URL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'url no-delimiters (interactive-p)))

(defalias 'ar-singlequote-url-atpt 'ar-url-singlequote-atpt)
(defun ar-url-singlequote-atpt (&optional no-delimiters)
  "Singlequotes URL at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'url no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-url-atpt 'ar-url-triplequote-dq-atpt)
(defun ar-url-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around url. "
  (interactive "*p")
  (ar-th-triplequote-dq 'url no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-url-atpt 'ar-url-triplequote-sq-atpt)
(defun ar-url-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around url. "
  (interactive "*p")
  (ar-th-triplequote-sq 'url no-delimiters (interactive-p)))

(defalias 'ar-trim-url-atpt 'ar-url-trim-atpt)
(defun ar-url-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'url t t))
    (ar-th-trim 'url t t)))

(defalias 'ar-trim-left-url-atpt 'ar-url-left-trim-atpt)
(defun ar-url-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'url t nil))
    (ar-th-trim 'url t nil)))

(defalias 'ar-trim-right-url-atpt 'ar-url-right-trim-atpt)
(defun ar-url-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'url nil t))
    (ar-th-trim 'url nil t)))

(defalias 'ar-url-underscore-atpt 'ar-underscore-url-atpt)
(defun ar-underscore-url-atpt (&optional no-delimiters)
  "Put underscore char around URL. "
  (interactive "*p")
  (ar-th-underscore 'url no-delimiters (interactive-p)))

(defalias 'ar-url-whitespace-atpt 'ar-whitespace-url-atpt)
(defun ar-whitespace-url-atpt (&optional no-delimiters)
  "Put whitespace char around URL. "
  (interactive "*p")
  (ar-th-whitespace 'url nil t))

(defalias 'ar-forward-url-atpt 'ar-url-forward-atpt)
(defun ar-url-forward-atpt (&optional arg)
  "Moves forward over URL at point if any, does nothing otherwise.
Returns end position of URL "
  (interactive "p")
  (ar-th-forward 'url arg (interactive-p)))

(defalias 'ar-backward-url-atpt 'ar-url-backward-atpt)
(defun ar-url-backward-atpt (&optional arg)
  "Moves backward over URL before point if any, does nothing otherwise.
Returns beginning position of URL "
  (interactive "p")
  (ar-th-backward 'url arg (interactive-p)))

(defalias 'ar-transpose-url-atpt 'ar-url-transpose-atpt)
(defun ar-url-transpose-atpt (&optional arg)
  "Transposes URL with URL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'url arg (interactive-p)))

(defalias 'ar-sort-url-atpt 'ar-url-sort-atpt)
(defun ar-url-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts urls in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'url reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-url-atpt 'ar-url-check-atpt)
(defun ar-url-check-atpt ()
  "Return t if a URL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-url-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-url-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-word-atpt (&optional arg no-delimiters)
  "Returns word at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'word arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-word-atpt 'ar-word-bounds-atpt)
(defun ar-word-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of word if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word no-delimiters (interactive-p)))

(defun ar-word-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position WORD at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'word no-delimiters (interactive-p)))

(defun ar-word-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'word no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-word-atpt 'ar-word-beginning-atpt)
(defun ar-word-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'word no-delimiters (interactive-p)))

(defalias 'ar-end-of-word-atpt 'ar-word-end-atpt)
(defun ar-word-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'word no-delimiters (interactive-p)))

(defalias 'ar-in-word-p-atpt 'ar-word-in-p-atpt)
(defun ar-word-in-p-atpt (&optional no-delimiters)
  "Returns bounds of WORD at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word no-delimiters (interactive-p)))

(defalias 'ar-length-of-word-atpt 'ar-word-length-atpt)
(defun ar-word-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'word no-delimiters (interactive-p)))

(defalias 'ar-copy-word-atpt 'ar-word-copy-atpt)
(defun ar-word-copy-atpt (&optional no-delimiters)
  "Returns a copy of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'word no-delimiters (interactive-p)))

(defalias 'ar-delete-word-in-region 'ar-word-delete-in-region)
(defun ar-word-delete-in-region (beg end)
  "Deletes WORD at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'word beg end (interactive-p)))

(defun ar-blok-word-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around word.
  Returns blok or nil if no WORD at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'word no-delimiters (interactive-p)))

(defalias 'ar-escape-word-atpt 'ar-word-escape-atpt)
(defun ar-word-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted WORD at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'word no-delimiters))

(defalias 'ar-doublequote-word-atpt 'ar-word-doublequote-atpt)
(defun ar-word-doublequote-atpt (&optional no-delimiters)
  "Doublequotes WORD at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'word no-delimiters (interactive-p)))

(defalias 'ar-slash-word-atpt 'ar-word-slash-atpt)
(defun ar-word-slash-atpt (&optional no-delimiters)
  "Doublequotes WORD at point if any. "
  (interactive "*p")
  (ar-th-slash 'word no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-word-atpt 'ar-word-double-backslash-atpt)
(defun ar-word-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around WORD at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'word no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-word-atpt 'ar-word-doubleslash-atpt)
(defun ar-word-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around WORD at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'word no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-word-atpt 'ar-word-doubleslash-paren-atpt)
(defun ar-word-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around WORD at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'word no-delimiters (interactive-p)))

(defalias 'ar-slashparen-word-atpt 'ar-word-slashparen-atpt)
(defun ar-word-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around WORD at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'word no-delimiters (interactive-p)))

(defalias 'ar-dollar-word-atpt 'ar-word-dollar-atpt)
(defun ar-word-dollar-atpt (&optional no-delimiters)
  "Doublequotes WORD at point if any. "
  (interactive "*p")
  (ar-th-dollar 'word no-delimiters (interactive-p)))

(defalias 'ar-equalize-word-atpt 'ar-word-equalize-atpt)
(defun ar-word-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around WORD at point if any. "
  (interactive "*p")
  (ar-th-equalize 'word no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-word-atpt 'ar-word-greater-angle-atpt)
(defun ar-word-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for WORD after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'word no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-word-atpt 'ar-word-lesser-angle-atpt)
(defun ar-word-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for WORD after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'word no-delimiters (interactive-p)))

(defalias 'ar-backslash-word-atpt 'ar-word-backslash-atpt)
(defun ar-word-backslash-atpt (&optional no-delimiters)
  "Backslash WORD at point if any. "
  (interactive "*p")
  (ar-th-backslash 'word no-delimiters (interactive-p)))

(defalias 'ar-brace-word-atpt 'ar-word-brace-atpt)
(defun ar-word-brace-atpt (&optional no-delimiters)
  "Braces WORD at point if any. "
  (interactive "*p")
  (ar-th-brace 'word no-delimiters (interactive-p)))

(defalias 'ar-bracket-word-atpt 'ar-word-bracket-atpt)
(defun ar-word-bracket-atpt (&optional no-delimiters)
  "Brackets WORD after point if any. "
  (interactive "*p")
  (ar-th-bracket 'word no-delimiters (interactive-p)))

(defun ar-comment-word-atpt (&optional no-delimiters)
  "Comments WORD at point if any. "
  (interactive "*p")
  (ar-th-comment 'word no-delimiters (interactive-p)))

(defun ar-commatize-word-atpt (&optional no-delimiters)
  "Put a comma after WORD at point if any. "
  (interactive "*p")
  (ar-th-commatize 'word no-delimiters (interactive-p)))

(defun ar-quote-word-atpt (&optional no-delimiters)
  "Put a singlequote before WORD at point if any. "
  (interactive "*p")
  (ar-th-quote 'word no-delimiters (interactive-p)))

(defalias 'ar-hyphen-word-atpt 'ar-word-hyphen-atpt)
(defun ar-word-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around WORD at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'word no-delimiters (interactive-p)))

(defalias 'ar-mark-word-atpt 'ar-word-mark-atpt)
(defun ar-word-mark-atpt ()
  "Marks WORD at point if any. "
  (interactive)
  (ar-th-mark 'word))

(defalias 'ar-hide-word-atpt 'ar-word-hide-atpt)
(defun ar-word-hide-atpt ()
  "Hides WORD at point. "
  (interactive)
  (ar-th-hide 'word))

(defalias 'ar-show-word-atpt 'ar-word-show-atpt)
(defun ar-word-show-atpt ()
  "Shows hidden WORD at point. "
  (interactive)
  (ar-th-show 'word))

(defalias 'ar-hide-show-word-atpt 'ar-word-hide-show-atpt)
(defun ar-word-hide-show-atpt ()
  "Alternatively hides or shows WORD at point. "
  (interactive)
  (ar-th-hide-show 'word))

(defalias 'ar-highlight-word-atpt-mode 'ar-word-highlight-atpt-mode)

(defun ar-word-highlight-atpt-mode (&optional no-delimiters)
  "Toggles word-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'word no-delimiters (interactive-p)))

(defalias 'ar-kill-word-atpt 'ar-word-kill-atpt)
(defun ar-word-kill-atpt (&optional no-delimiters)
  "Kills WORD at point if any. "
  (interactive "*P")
  (ar-th-kill 'word no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-word-atpt 'ar-word-kill-backward-atpt)
(defun ar-word-kill-backward-atpt (&optional no-delimiters)
  "Kills WORD at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'word no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-word-atpt 'ar-word-left-right-singlequote-atpt)
(defun ar-word-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'word no-delimiters (interactive-p)))

(defalias 'ar-parentize-word-atpt 'ar-word-parentize-atpt)
(defun ar-word-parentize-atpt (&optional no-delimiters)
  "Parentizes WORD at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'word no-delimiters (interactive-p)))

(defalias 'ar-separate-word-atpt 'ar-word-separate-atpt)
(defun ar-word-separate-atpt (&optional no-delimiters)
  "Separates WORD at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'word no-delimiters (interactive-p)))

(defalias 'ar-singlequote-word-atpt 'ar-word-singlequote-atpt)
(defun ar-word-singlequote-atpt (&optional no-delimiters)
  "Singlequotes WORD at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'word no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-word-atpt 'ar-word-triplequote-dq-atpt)
(defun ar-word-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around word. "
  (interactive "*p")
  (ar-th-triplequote-dq 'word no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-word-atpt 'ar-word-triplequote-sq-atpt)
(defun ar-word-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around word. "
  (interactive "*p")
  (ar-th-triplequote-sq 'word no-delimiters (interactive-p)))

(defalias 'ar-trim-word-atpt 'ar-word-trim-atpt)
(defun ar-word-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word t t))
    (ar-th-trim 'word t t)))

(defalias 'ar-trim-left-word-atpt 'ar-word-left-trim-atpt)
(defun ar-word-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word t nil))
    (ar-th-trim 'word t nil)))

(defalias 'ar-trim-right-word-atpt 'ar-word-right-trim-atpt)
(defun ar-word-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word nil t))
    (ar-th-trim 'word nil t)))

(defalias 'ar-word-underscore-atpt 'ar-underscore-word-atpt)
(defun ar-underscore-word-atpt (&optional no-delimiters)
  "Put underscore char around WORD. "
  (interactive "*p")
  (ar-th-underscore 'word no-delimiters (interactive-p)))

(defalias 'ar-word-whitespace-atpt 'ar-whitespace-word-atpt)
(defun ar-whitespace-word-atpt (&optional no-delimiters)
  "Put whitespace char around WORD. "
  (interactive "*p")
  (ar-th-whitespace 'word nil t))

(defalias 'ar-forward-word-atpt 'ar-word-forward-atpt)
(defun ar-word-forward-atpt (&optional arg)
  "Moves forward over WORD at point if any, does nothing otherwise.
Returns end position of WORD "
  (interactive "p")
  (ar-th-forward 'word arg (interactive-p)))

(defalias 'ar-backward-word-atpt 'ar-word-backward-atpt)
(defun ar-word-backward-atpt (&optional arg)
  "Moves backward over WORD before point if any, does nothing otherwise.
Returns beginning position of WORD "
  (interactive "p")
  (ar-th-backward 'word arg (interactive-p)))

(defalias 'ar-transpose-word-atpt 'ar-word-transpose-atpt)
(defun ar-word-transpose-atpt (&optional arg)
  "Transposes WORD with WORD before point if any. "
  (interactive "*p")
  (ar-th-transpose 'word arg (interactive-p)))

(defalias 'ar-sort-word-atpt 'ar-word-sort-atpt)
(defun ar-word-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts words in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'word reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-word-atpt 'ar-word-check-atpt)
(defun ar-word-check-atpt ()
  "Return t if a WORD at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-word-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-word-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-word-alpha-only-atpt (&optional arg no-delimiters)
  "Returns word-alpha-only at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'word-alpha-only arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-word-alpha-only-atpt 'ar-word-alpha-only-bounds-atpt)
(defun ar-word-alpha-only-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of word-alpha-only if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word-alpha-only no-delimiters (interactive-p)))

(defun ar-word-alpha-only-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position WORD-ALPHA-ONLY at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'word-alpha-only no-delimiters (interactive-p)))

(defun ar-word-alpha-only-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of WORD-ALPHA-ONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-word-alpha-only-atpt 'ar-word-alpha-only-beginning-atpt)
(defun ar-word-alpha-only-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class WORD-ALPHA-ONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-end-of-word-alpha-only-atpt 'ar-word-alpha-only-end-atpt)
(defun ar-word-alpha-only-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class WORD-ALPHA-ONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-in-word-alpha-only-p-atpt 'ar-word-alpha-only-in-p-atpt)
(defun ar-word-alpha-only-in-p-atpt (&optional no-delimiters)
  "Returns bounds of WORD-ALPHA-ONLY at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-length-of-word-alpha-only-atpt 'ar-word-alpha-only-length-atpt)
(defun ar-word-alpha-only-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class WORD-ALPHA-ONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-copy-word-alpha-only-atpt 'ar-word-alpha-only-copy-atpt)
(defun ar-word-alpha-only-copy-atpt (&optional no-delimiters)
  "Returns a copy of WORD-ALPHA-ONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-delete-word-alpha-only-in-region 'ar-word-alpha-only-delete-in-region)
(defun ar-word-alpha-only-delete-in-region (beg end)
  "Deletes WORD-ALPHA-ONLY at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'word-alpha-only beg end (interactive-p)))

(defun ar-blok-word-alpha-only-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around word-alpha-only.
  Returns blok or nil if no WORD-ALPHA-ONLY at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-escape-word-alpha-only-atpt 'ar-word-alpha-only-escape-atpt)
(defun ar-word-alpha-only-escape-atpt (&optional no-delimiters)
  "Returns regexp-quoted WORD-ALPHA-ONLY at point if any, nil otherwise "
  (interactive "*p")
  (ar-th-escape 'word-alpha-only no-delimiters))

(defalias 'ar-doublequote-word-alpha-only-atpt 'ar-word-alpha-only-doublequote-atpt)
(defun ar-word-alpha-only-doublequote-atpt (&optional no-delimiters)
  "Doublequotes WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-doublequote 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-slash-word-alpha-only-atpt 'ar-word-alpha-only-slash-atpt)
(defun ar-word-alpha-only-slash-atpt (&optional no-delimiters)
  "Doublequotes WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-slash 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-double-backslash-word-alpha-only-atpt 'ar-word-alpha-only-double-backslash-atpt)
(defun ar-word-alpha-only-double-backslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-double-backslash 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-word-alpha-only-atpt 'ar-word-alpha-only-doubleslash-atpt)
(defun ar-word-alpha-only-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-doubleslash-paren-word-alpha-only-atpt 'ar-word-alpha-only-doubleslash-paren-atpt)
(defun ar-word-alpha-only-doubleslash-paren-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-doubleslash-paren 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-slashparen-word-alpha-only-atpt 'ar-word-alpha-only-slashparen-atpt)
(defun ar-word-alpha-only-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-slash-paren 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-dollar-word-alpha-only-atpt 'ar-word-alpha-only-dollar-atpt)
(defun ar-word-alpha-only-dollar-atpt (&optional no-delimiters)
  "Doublequotes WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-dollar 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-equalize-word-alpha-only-atpt 'ar-word-alpha-only-equalize-atpt)
(defun ar-word-alpha-only-equalize-atpt (&optional no-delimiters)
  "Puts equal signs `=' around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-equalize 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-greater-angle-word-alpha-only-atpt 'ar-word-alpha-only-greater-angle-atpt)
(defun ar-word-alpha-only-greater-angle-atpt (&optional no-delimiters)
  "Sets angles for WORD-ALPHA-ONLY after point if any. "
  (interactive "*p")
  (ar-th-greater-angle 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-lesser-angle-word-alpha-only-atpt 'ar-word-alpha-only-lesser-angle-atpt)
(defun ar-word-alpha-only-lesser-angle-atpt (&optional no-delimiters)
  "Sets angles for WORD-ALPHA-ONLY after point if any. "
  (interactive "*p")
  (ar-th-lesser-angle 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-backslash-word-alpha-only-atpt 'ar-word-alpha-only-backslash-atpt)
(defun ar-word-alpha-only-backslash-atpt (&optional no-delimiters)
  "Backslash WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-backslash 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-brace-word-alpha-only-atpt 'ar-word-alpha-only-brace-atpt)
(defun ar-word-alpha-only-brace-atpt (&optional no-delimiters)
  "Braces WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-brace 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-bracket-word-alpha-only-atpt 'ar-word-alpha-only-bracket-atpt)
(defun ar-word-alpha-only-bracket-atpt (&optional no-delimiters)
  "Brackets WORD-ALPHA-ONLY after point if any. "
  (interactive "*p")
  (ar-th-bracket 'word-alpha-only no-delimiters (interactive-p)))

(defun ar-comment-word-alpha-only-atpt (&optional no-delimiters)
  "Comments WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-comment 'word-alpha-only no-delimiters (interactive-p)))

(defun ar-commatize-word-alpha-only-atpt (&optional no-delimiters)
  "Put a comma after WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-commatize 'word-alpha-only no-delimiters (interactive-p)))

(defun ar-quote-word-alpha-only-atpt (&optional no-delimiters)
  "Put a singlequote before WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-quote 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-hyphen-word-alpha-only-atpt 'ar-word-alpha-only-hyphen-atpt)
(defun ar-word-alpha-only-hyphen-atpt (&optional no-delimiters)
  "Puts hyphens around WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-hyphen 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-mark-word-alpha-only-atpt 'ar-word-alpha-only-mark-atpt)
(defun ar-word-alpha-only-mark-atpt ()
  "Marks WORD-ALPHA-ONLY at point if any. "
  (interactive)
  (ar-th-mark 'word-alpha-only))

(defalias 'ar-hide-word-alpha-only-atpt 'ar-word-alpha-only-hide-atpt)
(defun ar-word-alpha-only-hide-atpt ()
  "Hides WORD-ALPHA-ONLY at point. "
  (interactive)
  (ar-th-hide 'word-alpha-only))

(defalias 'ar-show-word-alpha-only-atpt 'ar-word-alpha-only-show-atpt)
(defun ar-word-alpha-only-show-atpt ()
  "Shows hidden WORD-ALPHA-ONLY at point. "
  (interactive)
  (ar-th-show 'word-alpha-only))

(defalias 'ar-hide-show-word-alpha-only-atpt 'ar-word-alpha-only-hide-show-atpt)
(defun ar-word-alpha-only-hide-show-atpt ()
  "Alternatively hides or shows WORD-ALPHA-ONLY at point. "
  (interactive)
  (ar-th-hide-show 'word-alpha-only))

(defalias 'ar-highlight-word-alpha-only-atpt-mode 'ar-word-alpha-only-highlight-atpt-mode)

(defun ar-word-alpha-only-highlight-atpt-mode (&optional no-delimiters)
  "Toggles word-alpha-only-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-kill-word-alpha-only-atpt 'ar-word-alpha-only-kill-atpt)
(defun ar-word-alpha-only-kill-atpt (&optional no-delimiters)
  "Kills WORD-ALPHA-ONLY at point if any. "
  (interactive "*P")
  (ar-th-kill 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-word-alpha-only-atpt 'ar-word-alpha-only-kill-backward-atpt)
(defun ar-word-alpha-only-kill-backward-atpt (&optional no-delimiters)
  "Kills WORD-ALPHA-ONLY at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-left-right-singlequote-word-alpha-only-atpt 'ar-word-alpha-only-left-right-singlequote-atpt)
(defun ar-word-alpha-only-left-right-singlequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-left-right-singlequote 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-parentize-word-alpha-only-atpt 'ar-word-alpha-only-parentize-atpt)
(defun ar-word-alpha-only-parentize-atpt (&optional no-delimiters)
  "Parentizes WORD-ALPHA-ONLY at point if any, does nothing otherwise"
  (interactive "*p")
  (ar-th-parentize 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-separate-word-alpha-only-atpt 'ar-word-alpha-only-separate-atpt)
(defun ar-word-alpha-only-separate-atpt (&optional no-delimiters)
  "Separates WORD-ALPHA-ONLY at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-singlequote-word-alpha-only-atpt 'ar-word-alpha-only-singlequote-atpt)
(defun ar-word-alpha-only-singlequote-atpt (&optional no-delimiters)
  "Singlequotes WORD-ALPHA-ONLY at point if any. "
  (interactive "*p")
  (ar-th-singlequote 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-triplequote-dq-word-alpha-only-atpt 'ar-word-alpha-only-triplequote-dq-atpt)
(defun ar-word-alpha-only-triplequote-dq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around word-alpha-only. "
  (interactive "*p")
  (ar-th-triplequote-dq 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-triplequote-sq-word-alpha-only-atpt 'ar-word-alpha-only-triplequote-sq-atpt)
(defun ar-word-alpha-only-triplequote-sq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around word-alpha-only. "
  (interactive "*p")
  (ar-th-triplequote-sq 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-trim-word-alpha-only-atpt 'ar-word-alpha-only-trim-atpt)
(defun ar-word-alpha-only-trim-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word-alpha-only t t))
    (ar-th-trim 'word-alpha-only t t)))

(defalias 'ar-trim-left-word-alpha-only-atpt 'ar-word-alpha-only-left-trim-atpt)
(defun ar-word-alpha-only-left-trim-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word-alpha-only t nil))
    (ar-th-trim 'word-alpha-only t nil)))

(defalias 'ar-trim-right-word-alpha-only-atpt 'ar-word-alpha-only-right-trim-atpt)
(defun ar-word-alpha-only-right-trim-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*p")
  (if no-delimiters
      (save-excursion (ar-th-trim 'word-alpha-only nil t))
    (ar-th-trim 'word-alpha-only nil t)))

(defalias 'ar-word-alpha-only-underscore-atpt 'ar-underscore-word-alpha-only-atpt)
(defun ar-underscore-word-alpha-only-atpt (&optional no-delimiters)
  "Put underscore char around WORD-ALPHA-ONLY. "
  (interactive "*p")
  (ar-th-underscore 'word-alpha-only no-delimiters (interactive-p)))

(defalias 'ar-word-alpha-only-whitespace-atpt 'ar-whitespace-word-alpha-only-atpt)
(defun ar-whitespace-word-alpha-only-atpt (&optional no-delimiters)
  "Put whitespace char around WORD-ALPHA-ONLY. "
  (interactive "*p")
  (ar-th-whitespace 'word-alpha-only nil t))

(defalias 'ar-forward-word-alpha-only-atpt 'ar-word-alpha-only-forward-atpt)
(defun ar-word-alpha-only-forward-atpt (&optional arg)
  "Moves forward over WORD-ALPHA-ONLY at point if any, does nothing otherwise.
Returns end position of WORD-ALPHA-ONLY "
  (interactive "p")
  (ar-th-forward 'word-alpha-only arg (interactive-p)))

(defalias 'ar-backward-word-alpha-only-atpt 'ar-word-alpha-only-backward-atpt)
(defun ar-word-alpha-only-backward-atpt (&optional arg)
  "Moves backward over WORD-ALPHA-ONLY before point if any, does nothing otherwise.
Returns beginning position of WORD-ALPHA-ONLY "
  (interactive "p")
  (ar-th-backward 'word-alpha-only arg (interactive-p)))

(defalias 'ar-transpose-word-alpha-only-atpt 'ar-word-alpha-only-transpose-atpt)
(defun ar-word-alpha-only-transpose-atpt (&optional arg)
  "Transposes WORD-ALPHA-ONLY with WORD-ALPHA-ONLY before point if any. "
  (interactive "*p")
  (ar-th-transpose 'word-alpha-only arg (interactive-p)))

(defalias 'ar-sort-word-alpha-only-atpt 'ar-word-alpha-only-sort-atpt)
(defun ar-word-alpha-only-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts word-alpha-onlys in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
    (unless (region-active-p) (message "%s" "Region must be active!"))
    (ar-th-sort 'word-alpha-only reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-word-alpha-only-atpt 'ar-word-alpha-only-check-atpt)
(defun ar-word-alpha-only-check-atpt ()
  "Return t if a WORD-ALPHA-ONLY at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-word-alpha-only-beginning-position-atpt"))))
         (end (funcall (intern-soft (concat "ar-word-alpha-only-end-position-atpt"))))
         (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-rest-list end


(defun ar-backslash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with backslash(s),
  otherwise copy backslash(ed) at point.
  With NO-DELIMITERS, copy backslash(ed) without delimiters.
  With negative argument kill backslash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'backslash no-delimiters (interactive-p)))

(defun ar-dollar-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with dollar(s),
  otherwise copy dollar(ed) at point.
  With NO-DELIMITERS, copy dollar(ed) without delimiters.
  With negative argument kill dollar(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'dollar no-delimiters (interactive-p)))

(defun ar-doublequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with doublequote(s),
  otherwise copy doublequote(ed) at point.
  With NO-DELIMITERS, copy doublequote(ed) without delimiters.
  With negative argument kill doublequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'doublequote no-delimiters (interactive-p)))

(defun ar-equalize-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with equalize(s),
  otherwise copy equalize(ed) at point.
  With NO-DELIMITERS, copy equalize(ed) without delimiters.
  With negative argument kill equalize(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'equalize no-delimiters (interactive-p)))

(defun ar-hyphen-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with hyphen(s),
  otherwise copy hyphen(ed) at point.
  With NO-DELIMITERS, copy hyphen(ed) without delimiters.
  With negative argument kill hyphen(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'hyphen no-delimiters (interactive-p)))

(defun ar-singlequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with singlequote(s),
  otherwise copy singlequote(ed) at point.
  With NO-DELIMITERS, copy singlequote(ed) without delimiters.
  With negative argument kill singlequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'singlequote no-delimiters (interactive-p)))

(defun ar-slash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with slash(s),
  otherwise copy slash(ed) at point.
  With NO-DELIMITERS, copy slash(ed) without delimiters.
  With negative argument kill slash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'slash no-delimiters (interactive-p)))

(defun ar-underscore-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with underscore(s),
  otherwise copy underscore(ed) at point.
  With NO-DELIMITERS, copy underscore(ed) without delimiters.
  With negative argument kill underscore(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'underscore no-delimiters (interactive-p)))

(defun ar-whitespace-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with whitespace(s),
  otherwise copy whitespace(ed) at point.
  With NO-DELIMITERS, copy whitespace(ed) without delimiters.
  With negative argument kill whitespace(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'whitespace no-delimiters (interactive-p)))

(defun ar-brace-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with brace(s),
  otherwise copy brace(ed) at point.
  With NO-DELIMITERS, copy brace(ed) without delimiters.
  With negative argument kill brace(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'brace no-delimiters (interactive-p)))

(defun ar-bracket-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with bracket(s),
  otherwise copy bracket(ed) at point.
  With NO-DELIMITERS, copy bracket(ed) without delimiters.
  With negative argument kill bracket(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'bracket no-delimiters (interactive-p)))

(defun ar-lesser-angle-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with lesser-angle(s),
  otherwise copy lesser-angle(ed) at point.
  With NO-DELIMITERS, copy lesser-angle(ed) without delimiters.
  With negative argument kill lesser-angle(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'lesser-angle no-delimiters (interactive-p)))

(defun ar-greater-angle-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with greater-angle(s),
  otherwise copy greater-angle(ed) at point.
  With NO-DELIMITERS, copy greater-angle(ed) without delimiters.
  With negative argument kill greater-angle(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'greater-angle no-delimiters (interactive-p)))

(defun ar-left-right-singlequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with left-right-singlequote(s),
  otherwise copy left-right-singlequote(ed) at point.
  With NO-DELIMITERS, copy left-right-singlequote(ed) without delimiters.
  With negative argument kill left-right-singlequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'left-right-singlequote no-delimiters (interactive-p)))

(defun ar-parentize-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with parentize(s),
  otherwise copy parentize(ed) at point.
  With NO-DELIMITERS, copy parentize(ed) without delimiters.
  With negative argument kill parentize(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'parentize no-delimiters (interactive-p)))

(defun ar-braced-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with braced(s),
  otherwise copy braced(ed) at point.
  With NO-DELIMITERS, copy braced(ed) without delimiters.
  With negative argument kill braced(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'braced no-delimiters (interactive-p)))

(defun ar-bracketed-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with bracketed(s),
  otherwise copy bracketed(ed) at point.
  With NO-DELIMITERS, copy bracketed(ed) without delimiters.
  With negative argument kill bracketed(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'bracketed no-delimiters (interactive-p)))

(defun ar-lesser-angled-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with lesser-angled(s),
  otherwise copy lesser-angled(ed) at point.
  With NO-DELIMITERS, copy lesser-angled(ed) without delimiters.
  With negative argument kill lesser-angled(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'lesser-angled no-delimiters (interactive-p)))

(defun ar-greater-angled-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with greater-angled(s),
  otherwise copy greater-angled(ed) at point.
  With NO-DELIMITERS, copy greater-angled(ed) without delimiters.
  With negative argument kill greater-angled(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'greater-angled no-delimiters (interactive-p)))

(defun ar-left-right-singlequoted-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with left-right-singlequoted(s),
  otherwise copy left-right-singlequoted(ed) at point.
  With NO-DELIMITERS, copy left-right-singlequoted(ed) without delimiters.
  With negative argument kill left-right-singlequoted(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'left-right-singlequoted no-delimiters (interactive-p)))

(defun ar-parentized-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with parentized(s),
  otherwise copy parentized(ed) at point.
  With NO-DELIMITERS, copy parentized(ed) without delimiters.
  With negative argument kill parentized(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'parentized no-delimiters (interactive-p)))

(defun ar-abbrev-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with abbrev(s),
  otherwise copy abbrev(ed) at point.
  With NO-DELIMITERS, copy abbrev(ed) without delimiters.
  With negative argument kill abbrev(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'abbrev no-delimiters (interactive-p)))

(defun ar-acronym-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with acronym(s),
  otherwise copy acronym(ed) at point.
  With NO-DELIMITERS, copy acronym(ed) without delimiters.
  With negative argument kill acronym(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'acronym no-delimiters (interactive-p)))

(defun ar-angled-no-nest-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with angled-no-nest(s),
  otherwise copy angled-no-nest(ed) at point.
  With NO-DELIMITERS, copy angled-no-nest(ed) without delimiters.
  With negative argument kill angled-no-nest(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'angled-no-nest no-delimiters (interactive-p)))

(defun ar-greater-angled-nested-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with greater-angled-nested(s),
  otherwise copy greater-angled-nested(ed) at point.
  With NO-DELIMITERS, copy greater-angled-nested(ed) without delimiters.
  With negative argument kill greater-angled-nested(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'greater-angled-nested no-delimiters (interactive-p)))

(defun ar-lesser-angled-nested-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with lesser-angled-nested(s),
  otherwise copy lesser-angled-nested(ed) at point.
  With NO-DELIMITERS, copy lesser-angled-nested(ed) without delimiters.
  With negative argument kill lesser-angled-nested(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'lesser-angled-nested no-delimiters (interactive-p)))

(defun ar-buffer-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with buffer(s),
  otherwise copy buffer(ed) at point.
  With NO-DELIMITERS, copy buffer(ed) without delimiters.
  With negative argument kill buffer(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'buffer no-delimiters (interactive-p)))

(defun ar-comment-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with comment(s),
  otherwise copy comment(ed) at point.
  With NO-DELIMITERS, copy comment(ed) without delimiters.
  With negative argument kill comment(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'comment no-delimiters (interactive-p)))

(defun ar-csv-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with csv(s),
  otherwise copy csv(ed) at point.
  With NO-DELIMITERS, copy csv(ed) without delimiters.
  With negative argument kill csv(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'csv no-delimiters (interactive-p)))

(defun ar-date-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with date(s),
  otherwise copy date(ed) at point.
  With NO-DELIMITERS, copy date(ed) without delimiters.
  With negative argument kill date(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'date no-delimiters (interactive-p)))

(defun ar-defun-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with defun(s),
  otherwise copy defun(ed) at point.
  With NO-DELIMITERS, copy defun(ed) without delimiters.
  With negative argument kill defun(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'defun no-delimiters (interactive-p)))

(defun ar-delimited-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with delimited(s),
  otherwise copy delimited(ed) at point.
  With NO-DELIMITERS, copy delimited(ed) without delimiters.
  With negative argument kill delimited(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'delimited no-delimiters (interactive-p)))

(defun ar-email-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with email(s),
  otherwise copy email(ed) at point.
  With NO-DELIMITERS, copy email(ed) without delimiters.
  With negative argument kill email(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'email no-delimiters (interactive-p)))

(defun ar-filename-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with filename(s),
  otherwise copy filename(ed) at point.
  With NO-DELIMITERS, copy filename(ed) without delimiters.
  With negative argument kill filename(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'filename no-delimiters (interactive-p)))

(defun ar-float-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with float(s),
  otherwise copy float(ed) at point.
  With NO-DELIMITERS, copy float(ed) without delimiters.
  With negative argument kill float(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'float no-delimiters (interactive-p)))

(defun ar-function-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with function(s),
  otherwise copy function(ed) at point.
  With NO-DELIMITERS, copy function(ed) without delimiters.
  With negative argument kill function(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'function no-delimiters (interactive-p)))

(defun ar-ip-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with ip(s),
  otherwise copy ip(ed) at point.
  With NO-DELIMITERS, copy ip(ed) without delimiters.
  With negative argument kill ip(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'ip no-delimiters (interactive-p)))

(defun ar-isbn-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with isbn(s),
  otherwise copy isbn(ed) at point.
  With NO-DELIMITERS, copy isbn(ed) without delimiters.
  With negative argument kill isbn(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'isbn no-delimiters (interactive-p)))

(defun ar-line-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with line(s),
  otherwise copy line(ed) at point.
  With NO-DELIMITERS, copy line(ed) without delimiters.
  With negative argument kill line(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'line no-delimiters (interactive-p)))

(defun ar-name-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with name(s),
  otherwise copy name(ed) at point.
  With NO-DELIMITERS, copy name(ed) without delimiters.
  With negative argument kill name(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'name no-delimiters (interactive-p)))

(defun ar-number-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with number(s),
  otherwise copy number(ed) at point.
  With NO-DELIMITERS, copy number(ed) without delimiters.
  With negative argument kill number(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'number no-delimiters (interactive-p)))

(defun ar-page-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with page(s),
  otherwise copy page(ed) at point.
  With NO-DELIMITERS, copy page(ed) without delimiters.
  With negative argument kill page(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'page no-delimiters (interactive-p)))

(defun ar-paragraph-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with paragraph(s),
  otherwise copy paragraph(ed) at point.
  With NO-DELIMITERS, copy paragraph(ed) without delimiters.
  With negative argument kill paragraph(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'paragraph no-delimiters (interactive-p)))

(defun ar-paren-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with paren(s),
  otherwise copy paren(ed) at point.
  With NO-DELIMITERS, copy paren(ed) without delimiters.
  With negative argument kill paren(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'paren no-delimiters (interactive-p)))

(defun ar-phone-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with phone(s),
  otherwise copy phone(ed) at point.
  With NO-DELIMITERS, copy phone(ed) without delimiters.
  With negative argument kill phone(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'phone no-delimiters (interactive-p)))

(defun ar-region-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with region(s),
  otherwise copy region(ed) at point.
  With NO-DELIMITERS, copy region(ed) without delimiters.
  With negative argument kill region(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'region no-delimiters (interactive-p)))

(defun ar-sentence-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with sentence(s),
  otherwise copy sentence(ed) at point.
  With NO-DELIMITERS, copy sentence(ed) without delimiters.
  With negative argument kill sentence(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'sentence no-delimiters (interactive-p)))

(defun ar-sexp-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with sexp(s),
  otherwise copy sexp(ed) at point.
  With NO-DELIMITERS, copy sexp(ed) without delimiters.
  With negative argument kill sexp(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'sexp no-delimiters (interactive-p)))

(defun ar-string-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with string(s),
  otherwise copy string(ed) at point.
  With NO-DELIMITERS, copy string(ed) without delimiters.
  With negative argument kill string(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'string no-delimiters (interactive-p)))

(defun ar-sh-struct-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with sh-struct(s),
  otherwise copy sh-struct(ed) at point.
  With NO-DELIMITERS, copy sh-struct(ed) without delimiters.
  With negative argument kill sh-struct(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'sh-struct no-delimiters (interactive-p)))

(defun ar-symbol-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with symbol(s),
  otherwise copy symbol(ed) at point.
  With NO-DELIMITERS, copy symbol(ed) without delimiters.
  With negative argument kill symbol(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'symbol no-delimiters (interactive-p)))

(defun ar-url-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with url(s),
  otherwise copy url(ed) at point.
  With NO-DELIMITERS, copy url(ed) without delimiters.
  With negative argument kill url(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'url no-delimiters (interactive-p)))

(defun ar-word-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with word(s),
  otherwise copy word(ed) at point.
  With NO-DELIMITERS, copy word(ed) without delimiters.
  With negative argument kill word(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'word no-delimiters (interactive-p)))

(defun ar-word-alpha-only-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with word-alpha-only(s),
  otherwise copy word-alpha-only(ed) at point.
  With NO-DELIMITERS, copy word-alpha-only(ed) without delimiters.
  With negative argument kill word-alpha-only(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'word-alpha-only no-delimiters (interactive-p)))
(defun emacs-batch-expression ()
  "Copy and highlight an expression starting with \"eval\" or \"load\". "
  (interactive)
  (unless (looking-back "[ \t\r\n\f]")
    (skip-chars-backward " \t\r\n\f"))
  
  (let ((beg (cond ((or (looking-at "--eval")(looking-at "-load"))
                    (match-beginning 0))
                   ((re-search-backward "--eval\\|-load\\|--funcall" (line-beginning-position) 'move)
                    (match-beginning 0)))))
    (if beg
        (progn
          (push-mark (point) t t)
          (setq end
                (progn
                  (skip-chars-forward "^ \t\r\n\f")
                  (skip-chars-forward " \t\r\n\f")
                  (if (looking-at "\"")
                      (progn
                        (forward-char 1)
                        (ar-end-of-doublequoted-atpt)
                        (forward-char 1)
                        (point))
                    (skip-chars-forward "^ \t\r\n\f")
                    (point))))
          (exchange-point-and-mark)
          (kill-new (buffer-substring-no-properties beg end)))
      (message "%s" "Can't detect beginning of emacs-batch-expression"))))



(provide 'thing-at-point-utils)
;;; thing-at-point-utils.el ends here
