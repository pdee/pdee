;;; python-components-fast-forms.el --- Execute forms at point

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

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

;;  Process forms fast

(defun py-fast-process (&optional argprompt dedicated shell buffer-name)
  "Connect am (I)Python process suitable for large output.

Output buffer displays \"Fast\" in name by default
It is not in interactive, i.e. comint-mode, as its bookkeepings seem linked to the freeze reported by lp:1253907

Return the process"
  (interactive "P")
  (py-shell argprompt dedicated shell buffer-name t))

(defun py--filter-result (string)
  "Set `py-result' according to `py-fast-filter-re'.

Remove trailing newline"
    (replace-regexp-in-string (format "[ \n]*%s[ \n]*" py-fast-filter-re) "" (ansi-color-filter-apply string)))

;; (defun py--filter-result (string)
;;   "Set `py-result' according to `py-fast-filter-re'.

;; Remove trailing newline"
;;   (let* ((erg (ansi-color-filter-apply string)))
;;     (setq py-result (replace-regexp-in-string (format "[ \n]*%s[ \n]*" py-fast-filter-re) "" erg))
;;     py-result))

(defun py--fast-send-string-no-output (string proc output-buffer)
  (with-current-buffer output-buffer
    ;; in comint-mode, prompt might be read-only
    ;; delete-region would fail
    ;; (let ((comint-prompt-read-only-old comint-prompt-read-only)
    ;; comint-prompt-read-only)
    ;; (switch-to-buffer (current-buffer))
    (process-send-string proc "\n")
    (let ((orig (point-max)))
      (sit-for 1 t)
      (process-send-string proc string)
      (process-send-string proc "\n")
      (accept-process-output proc 5)
      (sit-for 1 t)
      ;; (when py-verbose-p (message "py--fast-send-string-intern comint-prompt-read-only: %s" comint-prompt-read-only))
      (delete-region orig (point-max))
      ;; (setq comint-prompt-read-only comint-prompt-read-only-old)
      ;;)
      )))

(defun py--fast-send-string-intern (string proc output-buffer store return)
  (with-current-buffer output-buffer
    (process-send-string proc "\n")
    (let ((orig (point)))
      (process-send-string proc string)
      (process-send-string proc "\n")
      (accept-process-output proc 5)
      (sit-for py-fast-completion-delay t)
      ;; sets py-result
      (unless py-ignore-result-p
	(setq py-result (py--filter-result (py--fetch-result orig))))

      (when return
	py-result))))

(defun py--fast-send-string (string &optional proc windows-config)
  "Process Python strings, being prepared for large output.

Output buffer displays \"Fast\" in name by default
See also `py-fast-shell'

"
  (let* ((proc (or proc (get-buffer-process (py-fast-process))))
	 (buffer (process-buffer proc)))
    (if (or py-store-result-p py-return-result-p)
	(py--fast-send-string-intern string proc buffer py-store-result-p py-return-result-p)
      (py--fast-send-string-no-output string proc buffer))))

(defun py-execute-string-fast (string)
  "Evaluate STRING in Python process which is not in comint-mode.

From a programm use `py--fast-send-string'"
  (interactive "sPython command: ")
  (py--fast-send-string string))

(defun py-process-region-fast (beg end)
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end)))

(defun py-execute-statement-fast ()
  "Process statement at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "statement")))

(defun py-execute-block-fast ()
  "Process block at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block")))

(defun py-execute-block-or-clause-fast ()
  "Process block-or-clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "block-or-clause")))

(defun py-execute-def-fast ()
  "Process def at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def")))

(defun py-execute-class-fast ()
  "Process class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "class")))

(defun py-execute-def-or-class-fast ()
  "Process def-or-class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "def-or-class")))

(defun py-execute-expression-fast ()
  "Process expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "expression")))

(defun py-execute-partial-expression-fast ()
  "Process partial-expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "partial-expression")))

(defun py-execute-top-level-fast ()
  "Process top-level at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "top-level")))

(defun py-execute-clause-fast ()
  "Process clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output-buffer is not in comint-mode "
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare "clause")))

(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here
