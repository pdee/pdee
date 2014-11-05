;;; python-components-fast-complete.el -- Don't touch interactive shell when completing

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

;;; Code

;;;

(defun py--fast-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py--fast-send-string-intern
	  (format completion-code input) process py-buffer-name nil t)))
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

(defun py--fast--do-completion-at-point (process imports input orig py-exception-buffer code output-buffer)
  "Do completion at point for PROCESS."
  ;; send setup-code
  (let (py-return-result-p)
    (py--fast-send-string-no-output py-shell-completion-setup-code process output-buffer)
    (when imports
      ;; (message "%s" imports)
      (py--fast-send-string-no-output imports process output-buffer)))
  (let* ((completion
	  (py--fast-completion-get-completions input process code))
	 ;; (completion (when completions
	 ;; (try-completion input completions)))
	 newlist erg)
    ;; (message "%s" (current-buffer))
    (set-buffer py-exception-buffer)
    ;; (sit-for 1 t)
    (cond ((eq completion t)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "`t' is returned, not completion. Might be a bug."))
	   nil)
	  ((null completion)
	   (and py-verbose-p (message "py--fast--do-completion-at-point %s" "Don't see a completion"))
	   nil)
	  ((and completion
		(or (and (listp completion)
			 (string= input (car completion)))
		    (and (stringp completion)
			 (string= input completion))))
	   nil)
	  ((and completion (stringp completion)(not (string= input completion)))
	   (progn (delete-char (- (length input)))
		  (insert completion)
		  ;; (move-marker orig (point))
		  ;; minibuffer.el expects a list
		  nil))
	  (t (py--try-completion input completion)))

    nil))

(defun py--fast-complete-base (shell pos beg end word imports debug py-exception-buffer)
  (let* ((shell (or shell (py-choose-shell)))
	 (py-buffer-name (py-shell nil nil shell nil t))
	 (proc (get-buffer-process py-buffer-name))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 python-shell-module-completion-string-code)))
    (with-current-buffer py-buffer-name
      (erase-buffer))
    (py--fast--do-completion-at-point proc imports word pos py-exception-buffer code py-buffer-name)))

(defun py-fast-complete (&optional shell debug beg end word)
  "Complete word before point, if any.

Use `py-fast-process' "
  (interactive)
  (setq py-completion-last-window-configuration
        (current-window-configuration))
  (let (py-switch-buffers-on-execute-p
	(py-fast-process-p t)
	(py-fast-complete-p t)
	(py-return-result-p t))
    (py--complete-prepare shell debug beg end word t)))

(provide 'python-components-fast-complete)
;; python-components-fast-complete.el here
