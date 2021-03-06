;;; python-components-fast-complete.el -- Don't touch interactive shell when completing -*- lexical-binding: t; -*-

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

;;; Code:

(defun py--fast-completion-get-completions (input process completion-code buffer)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py-fast-send-string
	  (format completion-code input) process buffer t)))
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

(defun py--fast--do-completion-at-point (process imports input code buffer)
  "Do completion at point for PROCESS."
  ;; send setup-code
  (let (py-store-result-p)
    (when imports
      ;; (message "%s" imports)
      (py-fast-send-string imports process buffer nil t)))
  (let* ((completion
	  (py--fast-completion-get-completions input process code buffer)))
    (sit-for 0.1)
    (cond ((eq completion t)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "`t' is returned, not completion. Might be a bug.")))
	  ((null completion)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "Don't see a completion"))
	   (set-window-configuration py-last-window-configuration))
	  ((and completion
		(or (and (listp completion)
			 (string= input (car completion)))
		    (and (stringp completion)
			 (string= input completion))))
	   (set-window-configuration py-last-window-configuration))
	  ((and completion (stringp completion) (not (string= input completion)))
	   (progn (delete-char (- (length input)))
		  (insert completion)
		  ;; (move-marker orig (point))
		  ;; minibuffer.el expects a list
		  ))
	  (t (py--try-completion input completion)))))

(defun py--fast-complete-base (shell word imports)
  (let* (py-split-window-on-execute py-switch-buffers-on-execute-p
	 (shell (or shell py-shell-name))
	 (buffer (py-shell nil nil nil shell nil t))
 	 (proc (get-buffer-process buffer))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 py-shell-module-completion-code)))
    (py--python-send-completion-setup-code buffer)
    (py--fast--do-completion-at-point proc imports word code buffer)))

(defun py-fast-complete (&optional shell word imports)
  "Complete word before point, if any.

Use `py-fast-process' "
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (setq py-last-window-configuration
  	(current-window-configuration))
  (py-shell-complete shell nil nil word 1)
  (py-restore-window-configuration)
  )

(provide 'python-components-fast-complete)
;;; python-components-fast-complete.el here
