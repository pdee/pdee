;;; python-components-start2.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

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

;; Includes a minor mode for handling a Python/IPython shell, and can
;; take advantage of Pymacs when installed.

;; See documentation in README.org, README.DEVEL.org

;; Please report bugs at
;; https://gitlab.com/python-mode-devs/python-mode/issues

;; available commands are documented in directory "doc" as
;; commands-python-mode.org

;; As for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; `py-expression' composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; `py-partial-expression' beginns with a "(", which is
;; not taken as proposal.

;;; Code:


(defun py--fix-start (strg)
  "Internal use by py-execute... functions.

Takes STRG
Avoid empty lines at the beginning."
  ;; (when py-debug-p (message "py--fix-start:"))
  (let (py--imenu-create-index-p
	py-guess-py-install-directory-p
	py-autopair-mode
	py-complete-function
	py-load-pymacs-p
	py-load-skeletons-p
	erg)
    (with-temp-buffer
      (with-current-buffer (current-buffer)
	(when py-debug-p
	  (switch-to-buffer (current-buffer)))
	;; (python-mode)
	(insert strg)
	(goto-char (point-min))
	(when (< 0 (setq erg (skip-chars-forward " \t\r\n\f" (line-end-position))))
	  (dotimes (_ erg)
	    (indent-rigidly-left (point-min) (point-max))))
	(unless (py--beginning-of-statement-p)
	  (py-forward-statement))
	(while (not (eq (current-indentation) 0))
	  (py-shift-left py-indent-offset))
	(goto-char (point-max))
	(unless (py-empty-line-p)
	  (newline 1))
	(buffer-substring-no-properties 1 (point-max))))))

(defun py-fast-send-string (strg  &optional proc output-buffer result no-output argprompt args dedicated shell exception-buffer)
  (interactive
   (list (read-string "Python command: ")))
  (py-execute-string strg proc result no-output nil output-buffer t argprompt args dedicated shell exception-buffer))

(defun py--fast-send-string-no-output (strg  &optional proc output-buffer result)
  (py-fast-send-string strg proc output-buffer result t))

(defun py--send-to-fast-process (strg proc output-buffer result)
  "Called inside of `py--execute-base-intern'.

Optional STRG PROC OUTPUT-BUFFER RETURN"
  (let ((output-buffer (or output-buffer (process-buffer proc)))
	(inhibit-read-only t))
    ;; (switch-to-buffer (current-buffer))
    (with-current-buffer output-buffer
      ;; (erase-buffer)
      (py-fast-send-string strg
			   proc
			   output-buffer result))))

(defun py--point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (save-excursion
    (progn
      (cond
       ((eq position 'bol) (beginning-of-line))
       ((eq position 'eol) (end-of-line))
       ((eq position 'bod) (py-backward-def-or-class))
       ((eq position 'eod) (py-forward-def-or-class))
       ;; Kind of funny, I know, but useful for py-up-exception.
       ((eq position 'bob) (goto-char (point-min)))
       ((eq position 'eob) (goto-char (point-max)))
       ((eq position 'boi) (back-to-indentation))
       ((eq position 'bos) (py-backward-statement))
       (t (error "Unknown buffer position requested: %s" position))))))

(defun py-backward-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg done)
    (unless (bobp)
      (while (and (not done)(not (bobp))
                  (setq erg (re-search-backward "^[[:alpha:]_'\"]" nil t 1)))
        (if
            (nth 8 (parse-partial-sexp (point-min) (point)))
            (setq erg nil)
          (setq done t)))
      erg)))

;; might be slow due to repeated calls of `py-down-statement'
(defun py-forward-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-backward-statement))
      (unless (eq 0 (current-column))
        (py-backward-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-forward-def)))
            ((looking-at py-class-re)
             (setq erg (py-forward-class)))
            ((looking-at py-block-re)
             (setq erg (py-forward-block)))
            (t (setq erg (py-forward-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-forward-block))
          (setq erg (py-forward-statement))))
      erg)))

(provide 'python-components-start2)
;;; python-components-start2.el ends here
