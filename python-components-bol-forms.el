;;; python-components-bol-forms.el -- Forms start/end at beginning of line

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

;;; Beginning of line forms
(defun py-mark-base-bol (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form "-bol")))
         (endform (intern-soft (concat "py-end-of-" form "-bol")))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-bol-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator-bol))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-beginning-of-block-bol-p ()
  "Returns position, if cursor is at the beginning of block, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-block-bol)
      (py-beginning-of-block-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-block-lc 'py-end-of-block-bol)
(defun py-end-of-block-bol ()
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

(defun py-mark-block-bol ()
  "Mark block, take beginning of line positions. 

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "block"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-bol ()
  "Delete block bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-bol ()
  "Delete block bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-clause-bol-p ()
  "Returns position, if cursor is at the beginning of clause, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-clause-bol)
      (py-beginning-of-clause-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-clause-lc 'py-end-of-clause-bol)
(defun py-end-of-clause-bol ()
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

(defun py-mark-clause-bol ()
  "Mark clause, take beginning of line positions. 

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-clause-bol ()
  "Delete clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-clause-bol ()
  "Delete clause bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-block-or-clause-bol-p ()
  "Returns position, if cursor is at the beginning of block-or-clause, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-block-or-clause-bol)
      (py-beginning-of-block-or-clause-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-block-or-clause-lc 'py-end-of-block-or-clause-bol)
(defun py-end-of-block-or-clause-bol ()
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

(defun py-mark-block-or-clause-bol ()
  "Mark block-or-clause, take beginning of line positions. 

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "block-or-clause"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block-or-clause")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-block-or-clause-bol ()
  "Delete block-or-clause bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-def-bol-p ()
  "Returns position, if cursor is at the beginning of def, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-def-bol)
      (py-beginning-of-def-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-def-lc 'py-end-of-def-bol)
(defun py-end-of-def-bol ()
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

(defun py-mark-def-bol (&optional arg)
  "Mark def, take beginning of line positions. 

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "def" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "def")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-bol ()
  "Delete def bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-bol ()
  "Delete def bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-class-bol-p ()
  "Returns position, if cursor is at the beginning of class, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-class-bol)
      (py-beginning-of-class-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-class-lc 'py-end-of-class-bol)
(defun py-end-of-class-bol ()
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

(defun py-mark-class-bol (&optional arg)
  "Mark class, take beginning of line positions. 

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-class-bol ()
  "Delete class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-class-bol ()
  "Delete class bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-def-or-class-bol-p ()
  "Returns position, if cursor is at the beginning of def-or-class, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-def-or-class-bol)
      (py-beginning-of-def-or-class-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-down-def-or-class-lc 'py-end-of-def-or-class-bol)
(defun py-end-of-def-or-class-bol ()
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

(defun py-mark-def-or-class-bol (&optional arg)
  "Mark def-or-class, take beginning of line positions. 

With \\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
Returns beginning and end positions of region, a cons. "
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base-bol "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "def-or-class")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-def-or-class-bol ()
  "Delete def-or-class bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-def-or-class-bol ()
  "Delete def-or-class bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

(defun py-beginning-of-statement-bol-p ()
  "Returns position, if cursor is at the beginning of statement, at beginning of line, nil otherwise. "
  (interactive) 
  (let ((orig (point))
        (indent (current-indentation))
        erg)
    (save-excursion
      (py-end-of-statement-bol)
      (py-beginning-of-statement-bol indent)
      (when (eq orig (point))
        (setq erg orig))
      erg)))

(defalias 'py-beginning-of-statement-lc 'py-beginning-of-statement-bol)
(defun py-beginning-of-statement-bol (&optional indent)
  "Goto beginning of line where statement starts.
  Returns position reached, if successful, nil otherwise.

See also `py-up-statement': up from current definition to next beginning of statement above. "
  (interactive)
  (let* ((indent (or indent (when (eq 'py-end-of-statement-bol (car py-bol-forms-last-indent))(cdr py-bol-forms-last-indent))))
         erg)
    (if indent
        (while (and (setq erg (py-beginning-of-statement)) (< indent (current-indentation))(not (bobp))))
      (setq erg (py-beginning-of-statement)))
    ;; reset
    (setq py-bol-forms-last-indent nil)
    (when erg
      (unless (eobp)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-down-statement-lc 'py-end-of-statement-bol)
(defun py-end-of-statement-bol ()
  "Goto beginning of line following end of statement.
  Returns position reached, if successful, nil otherwise.

See also `py-down-statement': down from current definition to next beginning of statement below. "
  (interactive)
  (let ((erg (py-end-of-statement)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-mark-statement-bol ()
  "Mark statement, take beginning of line positions. 

Returns beginning and end positions of region, a cons. "
  (interactive)
  (let (erg)
    (setq erg (py-mark-base-bol "statement"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-copy-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "statement")))
    (copy-region-as-kill (car erg) (cdr erg))))

(defun py-kill-statement-bol ()
  "Delete statement bol at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-delete-statement-bol ()
  "Delete statement bol at point.

Don't store data in kill ring. "
  (interactive "*")
  (let ((erg (py-mark-base-bol "block")))
    (delete-region (car erg) (cdr erg))))

;; python-components-bol-forms.el ends here
(provide 'python-components-bol-forms)
