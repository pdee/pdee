;;; python-components-up-down.el -- Searching up/downwards in buffer

;; Copyright (C) 2015-2016  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


(defun py-up-statement ()
  "Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let ((orig (point))
        erg)
    (if (py--beginning-of-statement-p)
	(setq erg (py-backward-statement))
      (setq erg (and (py-backward-statement) (py-backward-statement))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
	  (erg
	   (cond ((py--end-of-statement-p)
		  (setq erg (and (py-forward-statement) (py-backward-statement))))
		 ((< orig (progn (py-forward-statement) (py-backward-statement)))
		  (point))
		 (t (and (py-forward-statement) (py-forward-statement)(py-backward-statement))))))
	   (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	   erg))

(defun py-up-base (regexp &optional indent orig decorator bol repeat)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise.
REGEXP is a quoted symbol "
  (unless (bobp)
    (let* ((orig (or orig (point)))
	   (repeat (or (and repeat (1+ repeat)) 999))
	   erg name command)
      (if (< py-max-specpdl-size repeat)
	  (message "`py-up-base' reached loops max.")
	(if indent
	    (progn
	      (while (and (re-search-backward (symbol-value regexp) nil 'move 1)
			  (or (nth 8 (parse-partial-sexp (point-min) (point)))
			      (<= indent (current-indentation))))))
	  (unless (py--beginning-of-statement-p)
	    (py-backward-statement))
	  (if (looking-at (symbol-value regexp))
	      (py-up-base regexp (current-indentation) orig decorator bol repeat)
	    (setq name (symbol-name regexp))
	    (setq command (intern-soft (concat "py-backward-" (substring name (string-match "minor\\|block\\|def\\|class" name) (string-match "-re" name)))))
	    (funcall command)
	    (py-up-base regexp (current-indentation) orig decorator bol repeat)))
	(when bol (beginning-of-line))
	(and (looking-at (symbol-value regexp)) (< (point) orig) (setq erg (point)))
	(when py-verbose-p (message "%s" erg))
	erg))))

(defun py-down-base (regexp &optional orig indent decorator bol)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise.
Expects a quoted symbol 'REGEXP"
  (unless (eobp)
    (let* ((orig (point))
	   (name (substring (symbol-name regexp) 3 -3))
	   (p-command (car (read-from-string (concat "py--beginning-of-" name "-p"))))
	   (backward-command (car (read-from-string (concat "py-backward-" name))))
	   (up-command (car (read-from-string (concat "py-up-" name))))
	   (down-command (car (read-from-string (concat "py-down-" name))))
           (forward-command (car (read-from-string (concat "py-forward-" name))))
           erg last done start)
      (if (funcall p-command)
	  (setq indent (current-indentation))
	(save-excursion
	  (funcall backward-command indent decorator bol)
	  (setq indent (current-indentation))
	  (setq start (point))))
      ;; (setq done (funcall forward-command indent decorator bol))
      (while (and
	      (py-down-statement)
	      (<= indent (current-indentation))
	      (when (looking-at (symbol-value regexp))
		(setq last (point)))))
    (if (looking-at (symbol-value regexp))
	(setq erg (point))
      (when last
	(progn (goto-char last)
	       (if (looking-at (symbol-value regexp))
		   (progn
		     (when bol (beginning-of-line))
		     (setq erg (point)))
		 (end-of-line)
		 (unless (eobp)
		   (forward-line 1)
		   (beginning-of-line))))))
    ;; Go to next end of next block upward instead
    (unless (or erg last)
      (goto-char orig)
      (or
       (if
	   (and
	    (funcall up-command)
	    ;; up should not result to backward
	    (not (eq (point) start))
	    (funcall forward-command indent decorator bol)
	    (< orig (point))
	    (setq erg (point)))
	   (when bol (setq erg (py--beginning-of-line-form erg)))
	 (goto-char (point-max)))))
    (when py-verbose-p (message "%s" erg))
    erg)))

(defalias 'py-up-block 'py-block-up)
(defun py-up-block (&optional indent decorator bol)
  "Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base 'py-extended-block-or-clause-re indent (point) decorator bol))

(defalias 'py-up-class 'py-class-up)
(defun py-up-class (&optional indent decorator bol)
  "Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base 'py-class-re indent (point) decorator bol))

(defalias 'py-up-def 'py-def-up)
(defun py-up-def (&optional indent decorator bol)
  "Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base 'py-def-re indent (point) decorator bol))

(defalias 'py-up-def-or-class 'py-def-or-class-up)
(defun py-up-def-or-class (&optional indent decorator bol)
  "Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base 'py-def-or-class-re indent (point) decorator bol))

(defalias 'py-up-minor-block 'py-minor-block-up)
(defun py-up-minor-block (&optional indent decorator bol)
  "Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base 'py-extended-block-or-clause-re indent (point) decorator bol))

(defalias 'py-down-block 'py-block-down)
(defun py-down-block (&optional orig indent decorator bol)
  "Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise. "
  (interactive)
  (py-down-base 'py-block-re (or orig (point)) indent decorator))

(defalias 'py-down-class 'py-class-down)
(defun py-down-class (&optional orig indent decorator bol)
  "Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise. "
  (interactive)
  (py-down-base 'py-class-re (or orig (point)) indent decorator))

(defalias 'py-down-def 'py-def-down)
(defun py-down-def (&optional orig indent decorator bol)
  "Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise. "
  (interactive)
  (py-down-base 'py-def-re (or orig (point)) indent decorator))

(defalias 'py-down-def-or-class 'py-def-or-class-down)
(defun py-down-def-or-class (&optional orig indent decorator bol)
  "Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-down-base 'py-def-or-class-re (or orig (point)) indent decorator))

(defalias 'py-down-minor-block 'py-minor-block-down)
(defun py-down-minor-block (&optional orig indent decorator bol)
  "Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-down-base 'py-minor-block-re (or orig (point)) indent decorator))

(defun py-up-block-bol (&optional indent decorator)
  "Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise. "
  (interactive)
  (py-up-base 'py-block-re indent (point) decorator t))

(defun py-up-class-bol (&optional indent decorator)
  "Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise. "
  (interactive)
  (py-up-base 'py-class-re indent (point) decorator t))

(defun py-up-def-bol (&optional indent decorator)
  "Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise. "
  (interactive)
  (py-up-base 'py-def-re indent (point) decorator t))

(defun py-up-def-or-class-bol (&optional indent decorator)
  "Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise. "
  (interactive)
  (py-up-base 'py-def-or-class-re indent (point) decorator t))

(defun py-up-minor-block-bol (&optional indent decorator)
  "Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise. "
  (interactive)
  (py-up-base 'py-minor-block-re indent (point) decorator t))

(defun py-down-block-bol (&optional orig indent decorator bol)
  "Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise "
  (interactive)
  (py-down-base 'py-block-re (point) indent decorator t))

(defun py-down-class-bol (&optional orig indent decorator bol)
  "Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise "
  (interactive)
  (py-down-base 'py-class-re (point) indent decorator t))

(defun py-down-def-bol (&optional orig indent decorator bol)
  "Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise "
  (interactive)
  (py-down-base 'py-def-re (point) indent decorator t))

(defun py-down-def-or-class-bol (&optional orig indent decorator bol)
  "Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise "
  (interactive)
  (py-down-base 'py-def-or-class-re (point) indent decorator t))

(defun py-down-minor-block-bol (&optional orig indent decorator bol)
  "Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise "
  (interactive)
  (py-down-base 'py-minor-block-re (point) indent decorator t))

;; python-components-up-down.el ends here
(provide 'python-components-up-down)
