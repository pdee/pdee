;;; python-components-fast-forms.el --- Execute forms at point -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Andreas Röhler

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

;; Process forms fast

(defun py-fast-process (&optional buffer)
  "Connect am (I)Python process suitable for large output.

Output buffer displays \"Fast\"  by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907"
  (interactive)
  (let ((this-buffer
         (set-buffer (or (and buffer (get-buffer-create buffer))
                         (get-buffer-create py-shell-name)))))
    (let ((proc (start-process py-shell-name this-buffer py-shell-name)))
      (with-current-buffer this-buffer
        (erase-buffer))
      proc)))

(defun py-fast-send-string (strg proc output-buffer &optional result no-output)
  (let ((inhibit-read-only t)
	(limit (marker-position (process-mark proc)))
	erg)
    (with-current-buffer output-buffer
      ;; (switch-to-buffer (current-buffer))
      ;; (erase-buffer)
      (process-send-string proc strg)
      (or (string-match "\n$" strg)
	  (process-send-string proc "\n"))
      (cond (no-output
	     (erase-buffer))
	    (result
	     (if
		 (setq erg (py--fetch-result output-buffer limit strg))
		 (setq py-result (py--filter-result erg))
	       (dotimes (_ 3) (unless (setq erg (py--fetch-result output-buffer proc))(sit-for 1 t)))
	       (unless (setq erg (py--fetch-result output-buffer limit))
		 (setq py-result nil)
		 (error "py-fast-send-string: py--fetch-result: no result")))))
      py-result)))

(defun py--send-to-fast-process (strg proc output-buffer result)
  "Called inside of ‘py--execute-base-intern’.

Optional STRG PROC OUTPUT-BUFFER RETURN"
  (let ((output-buffer (or output-buffer (process-buffer proc)))
	(inhibit-read-only t))
    ;; (switch-to-buffer (current-buffer))
    (with-current-buffer output-buffer
      ;; (erase-buffer)
      (py-fast-send-string strg
			   proc
			   output-buffer result))))

(defalias 'py-process-region-fast 'py-execute-region-fast)
(defun py-execute-region-fast (beg end &optional shell dedicated split switch proc)
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end shell dedicated t split switch proc)))

(defun py-execute-block-fast (&optional shell dedicated switch beg end file fast)
  "Process block at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare block shell dedicated switch beg end file t))

(defun py-execute-block-or-clause-fast (&optional shell dedicated switch beg end file fast)
  "Process block-or-clause at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare block-or-clause shell dedicated switch beg end file t))

(defun py-execute-class-fast (&optional shell dedicated switch beg end file)
  "Process class at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'class shell dedicated switch beg end file t))

(defun py-execute-clause-fast (&optional shell dedicated switch beg end file)
  "Process clause at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'clause shell dedicated switch beg end file t))

(defun py-execute-def-fast (&optional shell dedicated switch beg end file)
  "Process def at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'def shell dedicated switch beg end file t))

(defun py-execute-def-or-class-fast (&optional shell dedicated switch beg end file)
  "Process def-or-class at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'def-or-class shell dedicated switch beg end file t))

(defun py-execute-expression-fast (&optional shell dedicated switch beg end file)
  "Process expression at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'expression shell dedicated switch beg end file t))

(defun py-execute-partial-expression-fast (&optional shell dedicated switch beg end file)
  "Process partial-expression at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'partial-expression shell dedicated switch beg end file t))

(defun py-execute-section-fast (&optional shell dedicated switch beg end file)
  "Process section at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'section shell dedicated switch beg end file t))

(defun py-execute-statement-fast (&optional shell dedicated switch beg end file)
  "Process statement at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'statement shell dedicated switch beg end file t))

(defun py-execute-top-level-fast (&optional shell dedicated switch beg end file)
  "Process top-level at point by a Python interpreter.

Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (py--execute-prepare 'top-level shell dedicated switch beg end file t))

(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here
