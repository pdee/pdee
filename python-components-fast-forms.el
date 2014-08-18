;;; python-components-fast-forms.el --- Execute forms at point

;; Copyright (C) 2011-2014  Andreas Roehler
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

;;; Process forms fast

(defun py-fast-process (&optional buffer)
  "Connect am (I)Python process suitable for large output.

Output arrives in py-fast-output-buffer, \"\*Python Fast Output\*\" by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907

Return the process"
  (interactive)
  (let ((this-buffer
	 (set-buffer (or (and buffer (get-buffer-create buffer))
			 (get-buffer-create (default-value 'py-fast-output-buffer))))))
    (erase-buffer)
    (let ((proc
	   (or (get-buffer-process this-buffer)

	       (start-process py-shell-name this-buffer py-shell-name))))
      (with-current-buffer this-buffer
        (erase-buffer))
      ;; (setq py-fast-output-buffer this-buffer)
      proc)))

(defun py--fast-send-string (string &optional proc windows-config)
  "Process Python strings, being prepared for large output.

Result arrives in py-fast-output-buffer, \"\*Python Fast Output\*\" by default
See also `py-fast-shell'

"
  (let* ((proc (or proc
		   (ignore-errors (get-buffer-process (get-buffer py-fast-output-buffer)))
		   (py-fast-process)))
	 (buffer (process-buffer proc)))
    (py--fast-send-string-intern string proc buffer)))

(defun py-fast-send-string (string)
  "Evaluate STRING in Python process which is not in comint-mode.

From a programm use `py--fast-send-string'"
  (interactive "sPython command: ")
  (py--fast-send-string string))

(defun py--fast-send-string-no-output (string &optional proc)
  "Process Python string, ignore output.

Used to update Python process

Result arrives in py-fast-output-buffer, \"\*Python Fast Output\*\" by default
See also `py-fast-shell'

"
  (let* ((proc (or proc
		   (ignore-errors (get-buffer-process (get-buffer py-fast-output)))
		   (py-fast-process)))
	 (buffer (process-buffer proc))
	 py-store-result-p)
    (py--fast-send-string-intern string proc buffer)))

(defun py--filter-result ()
  "Set `py-result' according to `py-fast-filter-re'.

Remove trailing newline"
  (setq py-result (replace-regexp-in-string py-fast-filter-re "" (buffer-substring-no-properties orig (point-max))))
  ;; remove trailing newline
  (and (string-match "\n$" py-result)
       (setq py-result (substring py-result 0 (match-beginning 0))))
  (and py-store-result-p
       (not (string= "" py-result))
       (kill-new py-result))
  (setq py-result (split-string py-result "\n")))

(defun py--fast-send-string-intern (string proc output-buffer)
  (with-current-buffer output-buffer
    (widen)
    (erase-buffer)
    (process-send-string proc "\n")
    (let ((orig (point)))
      (process-send-string proc string)
      (process-send-string proc "\n")
      (accept-process-output proc 5)
      ;; `py--fast-send-string-no-output' sets `py-store-result-p' to
      ;; nil
      (when (or py-store-result-p py-return-result-p)
	(py--filter-result))
      (when py-return-result-p
	py-result))))

(defun py-process-region-fast (beg end)
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end)))

(defun py-execute-statement-fast ()
  "Process statement at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "statement")))

(defun py-execute-block-fast ()
  "Process block at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block")))

(defun py-execute-block-or-clause-fast ()
  "Process block-or-clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block-or-clause")))

(defun py-execute-def-fast ()
  "Process def at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def")))

(defun py-execute-class-fast ()
  "Process class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "class")))

(defun py-execute-def-or-class-fast ()
  "Process def-or-class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def-or-class")))

(defun py-execute-expression-fast ()
  "Process expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "expression")))

(defun py-execute-partial-expression-fast ()
  "Process partial-expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "partial-expression")))

(defun py-execute-top-level-fast ()
  "Process top-level at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "top-level")))

(defun py-execute-clause-fast ()
  "Process clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Result arrives in `py-fast-output-buffer', which is not in
comint-mode"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "clause")))

(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here
