;;; python-components-shift-forms.el --- Move forms left or right

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Code:

(defalias 'py-shift-region-left 'py-shift-left)
(defun py-shift-left (&optional count start end)
  "Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "
  (interactive "p")
  (setq count (or count 1))
  (let ((erg (py--shift-intern (- count) start end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defalias 'py-shift-region-right 'py-shift-right)
(defun py-shift-right (&optional count beg end)
  "Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "
  (interactive "p")
  (setq count (or count 1))
  (let ((erg (py--shift-intern count beg end)))
    (when (and (interactive-p) py-verbose-p) (message "%s" erg))
    erg))

(defun py--shift-intern (count &optional start end)
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
      (if (< 0 count)
          (indent-rigidly beg end py-indent-offset)
        (indent-rigidly beg end (- py-indent-offset)))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward " \t\r\n\f"))
    (py-indentation-of-statement)))

(defun py--shift-forms-base (form arg &optional beg end)
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
         (erg (py--shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "paragraph" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "paragraph" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block" (or arg py-indent-offset))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-minor-block-left (&optional arg)
  "Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "minor-block" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-minor-block-right (&optional arg)
  "Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached.
A minor block is started by a `for', `if', `try' or `with'. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "minor-block" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "clause" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-right (&optional arg)
  "Indent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block-or-clause" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-block-or-clause-left (&optional arg)
  "Dedent block-or-clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "block-or-clause" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "class" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-right (&optional arg)
  "Indent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def-or-class" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-def-or-class-left (&optional arg)
  "Dedent def-or-class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "def-or-class" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-right (&optional arg)
  "Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "line" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-line-left (&optional arg)
  "Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "line" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "statement" (or arg py-indent-offset))))
        (when (interactive-p) (message "%s" erg))
    erg))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py--shift-forms-base "statement" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(provide 'python-components-shift-forms)
;;; python-components-shift-forms.el ends here
