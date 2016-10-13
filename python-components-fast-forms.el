;;; python-components-fast-forms.el --- Execute forms at point

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
                         (get-buffer-create py-buffer-name)))))
    (let ((proc (start-process py-shell-name this-buffer py-shell-name)))
      (with-current-buffer this-buffer
        (erase-buffer))
      proc)))

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

(defun py--filter-result (string)
  "Set `py-result' according to `py-fast-filter-re'.

Remove trailing newline"
    (replace-regexp-in-string (format "[ \n]*%s[ \n]*" py-fast-filter-re) "" (ansi-color-filter-apply string)))

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

(defun py--fast-send-string (string)
  "Process Python strings, being prepared for large output.

Output buffer displays \"Fast\"  by default
See also `py-fast-shell'

"
  (let ((proc (or (get-buffer-process (get-buffer py-fast-output-buffer))
                  (py-fast-process))))
    ;;    (with-current-buffer py-fast-output-buffer
    ;;      (erase-buffer))
    (process-send-string proc string)
    (or (string-match "\n$" string)
        (process-send-string proc "\n"))
    (accept-process-output proc 1)
    (switch-to-buffer py-fast-output-buffer)
    (beginning-of-line)
    (skip-chars-backward "\r\n")
    (delete-region (point) (point-max))))

(defun py-process-region-fast (beg end)
  (interactive "r")
  (let ((py-fast-process-p t))
    (py-execute-region beg end)))

(defun py-execute-block-fast ()
  "Process block at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'block)))

(defun py-execute-block-or-clause-fast ()
  "Process block-or-clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'block-or-clause)))

(defun py-execute-class-fast ()
  "Process class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'class)))

(defun py-execute-clause-fast ()
  "Process clause at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'clause)))

(defun py-execute-def-fast ()
  "Process def at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'def)))

(defun py-execute-def-or-class-fast ()
  "Process def-or-class at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'def-or-class)))

(defun py-execute-expression-fast ()
  "Process expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'expression)))

(defun py-execute-partial-expression-fast ()
  "Process partial-expression at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'partial-expression)))

(defun py-execute-section-fast ()
  "Process section at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'section)))

(defun py-execute-statement-fast ()
  "Process statement at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'statement)))

(defun py-execute-top-level-fast ()
  "Process top-level at point by a Python interpreter.

Suitable for large output, doesn't mess up interactive shell.
Output buffer not in comint-mode, displays \"Fast\"  by default"
  (interactive)
  (let ((py-fast-process-p t))
    (py--execute-prepare 'top-level)))

(provide 'python-components-fast-forms)
;;; python-components-fast-forms.el ends here
