;;; python-components-re-forms.el --- Forms start described by a regular-expression

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

;;; Beg-end forms
(defun py-beginning-of-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let ((orig (point))
        erg last)
    (unless (bobp)
      (while (and (setq last (re-search-backward py-top-level-form-re nil 'move 1))(nth 8 (syntax-ppss))))
      ;; (while (and (not (bobp)) (setq erg (py-beginning-of-statement))
      ;;             (< 0 (current-indentation))))
      (and last (< last orig)(setq erg (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py-beginning-of-statement-p)
        (py-beginning-of-statement))
      (unless (eq 0 (current-column))
        (py-beginning-of-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-end-of-def)))
            ((looking-at py-class-re)
             (setq erg (py-end-of-class)))
            ((looking-at py-block-re)
             (setq erg (py-end-of-block))
             (setq erg (py-end-of-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-end-of-block))
          (setq erg (py-end-of-statement))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py-beginning ()
  "Go to beginning of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (py-beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))

(defun py-end (&optional indent)
 "Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-extended-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-up (&optional indent)
  "Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to it's beginning.
If not at beginning of a statement or block, go to it's beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py-beginning-of-statement-p) (py-beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))
          (t (py-beginning-of-statement)))))

(defun py-down (&optional indent)

  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to it's beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         erg
         (indent (if
                     (py-beginning-of-statement-p)
                     (current-indentation)
                   (progn
                     (py-beginning-of-statement)
                     (current-indentation))))
         last)
    (while (and (setq last (point)) (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement) (eq (current-indentation) indent)))
    (if (< indent (current-indentation))
        (setq erg (point))
      (goto-char last))
    (when (< (point) orig)
      (goto-char orig))
    (when (and (eq (point) orig)
               (progn (forward-char 1)
                      (skip-chars-forward "^\"'[({" (line-end-position))
                      (member (char-after) (list ?\( ?\" ?\' ?\[ ?\{)))
               (setq erg (point))))
    (unless erg
      (goto-char orig))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-end-of-block (&optional indent)
 "Go to end of block.

Returns end of block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-block-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-clause (&optional indent)
 "Go to end of clause.

Returns end of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-block-or-clause (&optional indent)
 "Go to end of block-or-clause.

Returns end of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))


(defun py-end-of-def (&optional indent)
 "Go to end of def.

Returns end of def if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-def-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-class (&optional indent)
 "Go to end of class.

Returns end of class if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-class-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-def-or-class (&optional indent)
 "Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too. "
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-def-or-class-re orig)))
      (when (and py-verbose-p (interactive-p))
          (message "%s" erg))
      erg))

(defun py-end-of-if-block (&optional indent)
 "Go to end of if-block.

Returns end of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-if-block-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-elif-block (&optional indent)
 "Go to end of if-block.

Returns end of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-elif-block-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-try-block (&optional indent)
 "Go to end of try-block.

Returns end of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-try-block-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

(defun py-end-of-minor-block (&optional indent)
 "Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'.
"
  (interactive "P")
    (let* ((orig (point))
           (erg (py-end-base 'py-minor-block-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))


;; Buffer
(defun py-beginning-of-buffer ()
  "Go to beginning-of-buffer, return position. "
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  "Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. "
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)

(provide 'python-components-re-forms)
;;; python-components-re-forms.el ends here
