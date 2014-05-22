;;; python-components-forms.el -- Forms start/end at beginning of line

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

;; Mark
(defun py--base (form &optional py-mark-decorators)
  "Returns boundaries of FORM, a cons. "
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and py-mark-decorators
         (and (setq erg (py-beginning-of-decorator))
              (setq beg erg)))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-mark-base (form &optional py-mark-decorators)
  "Calls py--base, returns bounds of form, a cons. "
  (let* ((bounds (py--base form py-mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

(defun py-mark-paragraph ()
  "Mark paragraph at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "paragraph"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-minor-block ()
  "Mark minor-block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "minor-block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-clause ()
  "Mark clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def (&optional arg)
  "Mark def at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (setq erg (py-mark-base "def" py-mark-decorators))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-class (&optional arg)
  "Mark class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class at point.

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of marked area, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-line ()
  "Mark line at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "line"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-top-level ()
  "Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "top-level"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "partial-expression"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;; Beginning of line forms
(defun py--beginning-of-block-p ()
  "Returns position, if cursor is at the beginning of a block, nil otherwise. "
  (when (and (looking-at py-block-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-block-lc 'py-end-of-block)
(defun py-end-of-block (&optional indent)
  "Go to end of block.

Returns end of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-block-lc 'py-end-of-block-lc)
(defun py-end-of-block-lc ()
  "Goto beginning of line following end of block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-end-of-block)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-beginning-of-minor-block-p ()
  "Returns position, if cursor is at the beginning of a minor-block, nil otherwise.

A minor block is started by a `for', `if', `try' or `with'.
"
  (when (and (looking-at py-minor-block-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-minor-block-lc 'py-end-of-minor-block)
(defun py-end-of-minor-block (&optional indent)
  "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-minor-block-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-minor-block-lc 'py-end-of-minor-block-lc)
(defun py-end-of-minor-block-lc ()
  "Goto beginning of line following end of minor-block.
  Returns position reached, if successful, nil otherwise.

See also `py-down-minor-block': down from current definition to next beginning of minor-block below. "
  (interactive)
  (let ((erg (py-end-of-minor-block)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py--beginning-of-clause-p ()
  "Returns position, if cursor is at the beginning of a clause, nil otherwise. "
  (when (and (looking-at py-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-clause-lc 'py-end-of-clause)
(defun py-end-of-clause (&optional indent)
  "Go to end of clause.

Returns end of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-clause-lc 'py-end-of-clause-lc)
(defun py-end-of-clause-lc ()
  "Goto beginning of line following end of clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-end-of-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py--beginning-of-block-or-clause-p ()
  "Returns position, if cursor is at the beginning of a block-or-clause, nil otherwise. "
  (when (and (looking-at py-block-or-clause-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-block-or-clause-lc 'py-end-of-block-or-clause)
(defun py-end-of-block-or-clause (&optional indent)
  "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-block-or-clause-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-block-or-clause-lc 'py-end-of-block-or-clause-lc)
(defun py-end-of-block-or-clause-lc ()
  "Goto beginning of line following end of block-or-clause.
  Returns position reached, if successful, nil otherwise.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below. "
  (interactive)
  (let ((erg (py-end-of-block-or-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py--beginning-of-def-p ()
  "Returns position, if cursor is at the beginning of a def, nil otherwise. "
  (when (and (looking-at py-def-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-def-lc 'py-end-of-def)
(defun py-end-of-def (&optional indent)
  "Go to end of def.

Returns end of def if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-def-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-def-lc 'py-end-of-def-lc)
(defun py-end-of-def-lc ()
  "Goto beginning of line following end of def.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-end-of-def)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-kill-def ()
  "Delete def  at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "ele")))
    (kill-region (car erg) (cdr erg))))

(defun py--beginning-of-class-p ()
  "Returns position, if cursor is at the beginning of a class, nil otherwise. "
  (when (and (looking-at py-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-class-lc 'py-end-of-class)
(defun py-end-of-class (&optional indent)
  "Go to end of class.

Returns end of class if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         (erg (py--end-base 'py-class-re orig)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-down-class-lc 'py-end-of-class-lc)
(defun py-end-of-class-lc ()
  "Goto beginning of line following end of class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-end-of-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-mark-class ()
  "Mark class at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "class"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-kill-class ()
  "Delete class  at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "ele")))
    (kill-region (car erg) (cdr erg))))

(defun py--beginning-of-def-or-class-p ()
  "Returns position, if cursor is at the beginning of a def-or-class, nil otherwise. "
  (when (and (looking-at py-def-or-class-re)
             (not (py-in-string-or-comment-p)))
    (point)))

(defalias 'py-down-def-or-class-lc 'py-end-of-def-or-class-lc)
(defun py-end-of-def-or-class-lc ()
  "Goto beginning of line following end of def-or-class.
  Returns position reached, if successful, nil otherwise.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below. "
  (interactive)
  (let ((erg (py-end-of-def-or-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-mark-def-or-class ()
  "Mark def-or-class at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base "def-or-class"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-kill-def-or-class ()
  "Delete def-or-class  at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "ele")))
    (kill-region (car erg) (cdr erg))))

;; python-components-forms.el ends here
(provide 'python-components-forms)
