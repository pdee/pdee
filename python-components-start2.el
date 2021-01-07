;;; python-components-start2.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.2.3

;; Keywords: languages, processes, python, oop

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2020 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

(defun py--end-base (regexp &optional orig bol repeat)
  "Used internal by functions going to the end FORM.

Returns the indentation of FORM-start
Arg REGEXP, a symbol"
  (unless (eobp)
    (let (;; not looking for an assignment
	  (use-regexp (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-backward-statement))
	(let* (;; when at block-start, be specific
	       (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (symbol-value regexp))
               ;; (regexp (or regexp (symbol-value 'py-extended-block-or-clause-re)))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
			   (if (bolp) 0
			     (abs
			      (- (current-indentation) py-indent-offset)))
			 (current-indentation)))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (py-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
				(looking-at py-decorator-re)
				(py-down-def-or-class (current-indentation)))
			   (and (member regexp (list 'py-minor-block-re 'py-if-re 'py-for-re 'py-try-re))
				(looking-at py-minor-clause-re))))
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((eq 0 (current-indentation))
		  (py--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (py--go-to-keyword regexp))))
	       (secondvalue (ignore-errors (nth 2 res)))
	       erg)
	  ;; (py-for-block-p (looking-at py-for-re))
	  (setq indent (or (and res (car-safe res)) indent))
	  (cond
	   (res (setq erg
		      (and
		       (py--down-according-to-indent regexp secondvalue (current-indentation))
		       ;; (if (>= indent (current-indentation))
		       (py--down-end-form)
		       ;; (py--end-base regexp orig bol repeat)
		       ;; )
		       )))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (py--forward-regexp (symbol-value regexp))
	      (beginning-of-line)
	      (setq erg (and
			 (py--down-according-to-indent regexp secondvalue (current-indentation) t)
			 (py--down-end-form)))))
	  (cond ((< orig (point))
		 (setq erg (point))
		 (progn
		   (and erg bol (setq erg (py--beginning-of-line-form)))
		   (and erg (cons (current-indentation) erg))))
		((eq (point) orig)
		 (unless (eobp)
		   (cond
		    ((and (< repeat 1)
			  (or
			   ;; looking next indent as part of body
			   (py--down-according-to-indent regexp secondvalue
							 indent
							 ;; if expected indent is 0,
							 ;; search for new start,
							 ;; search for regexp only
							 (eq 0 indent))
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent py-indent-offset)) indent))
			    (py--down-according-to-indent regexp secondvalue
							  indent t))))
		     (py--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (py--down-according-to-indent regexp secondvalue nil t)
		   (py--end-base regexp (point) bol (1+ repeat))))))))))

(defun py--fix-start (strg)
  "Internal use by py-execute... functions.

Takes STRG
Avoid empty lines at the beginning."
  ;; (when py--debug-p (message "py--fix-start:"))
  (let (py--imenu-create-index-p
	py-guess-py-install-directory-p
	py-autopair-mode
	py-complete-function
	py-load-pymacs-p
	py-load-skeletons-p
	erg)
    (with-temp-buffer
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
      (buffer-substring-no-properties 1 (point-max)))))

(defun py-execute-string (strg &optional process result no-output orig output-buffer fast argprompt args dedicated shell exception-buffer split switch internal)
  "Evaluate STRG in Python PROCESS.

With optional Arg PROCESS send to process.
With optional Arg RESULT store result in var ‘py-result’, also return it.
With optional Arg NO-OUTPUT don't display any output
With optional Arg ORIG deliver original position.
With optional Arg OUTPUT-BUFFER specify output-buffer"
  (interactive "sPython command: ")
  (save-excursion
    (let* ((buffer (or output-buffer (or (and process (buffer-name (process-buffer process))) (buffer-name (py-shell argprompt args dedicated shell output-buffer fast exception-buffer split switch internal)))))
	   (proc (or process (get-buffer-process buffer)))
	   ;; nil nil nil nil (buffer-name buffer))))
	   (orig (or orig (point)))
   	   (limit (ignore-errors (marker-position (process-mark proc)))))
      (cond ((and no-output fast)
	     (py--fast-send-string-no-output-intern strg proc limit buffer no-output))
	    (no-output
	     (py-send-string-no-output strg proc))
	    ((and (string-match ".\n+." strg) (string-match "^[Ii]"
							    ;; (buffer-name buffer)
							    buffer
							    ))  ;; multiline
	     (let* ((temp-file-name (py-temp-file-name strg))
		    (file-name (or (buffer-file-name) temp-file-name)))
	       (py-execute-file file-name proc)))
	    (t (with-current-buffer buffer
		 (comint-send-string proc strg)
		 (when (or (not (string-match "\n\\'" strg))
			   (string-match "\n[ \t].*\n?\\'" strg))
		   (comint-send-string proc "\n"))
		 (sit-for py-python-send-delay)
		 (cond (result
			(setq py-result
			      (py--fetch-result buffer limit strg)))
		       (no-output
			(and orig (py--cleanup-shell orig buffer))))))))))

(provide 'python-components-start2)
;;; python-components-start2.el ends here
