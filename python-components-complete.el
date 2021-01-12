;;; python-components-complete.el -- Don't touch interactive shell when completing -*- lexical-binding: t; -*-

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

(defalias 'py-script-complete 'py-shell-complete)
(defalias 'py-python2-shell-complete 'py-shell-complete)
(defalias 'py-python3-shell-complete 'py-shell-complete)

(defun py--shell-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((erg
	 (py-send-string-no-output (format completion-code input) process)))
    (if (and erg (> (length erg) 2))
	(setq erg (split-string erg "^'\\|^\"\\|;\\|'$\\|\"$" t))
      (and py-verbose-p (message "py--shell-completion-get-completions: %s" "Don't see a completion")))
    erg))

;; post-command-hook
;; caused insert-file-contents error lp:1293172
(defun py--after-change-function (end)
  "Restore window-confiuration after completion.

Takes END"
  (when
      (and (or
            (eq this-command 'completion-at-point)
            (eq this-command 'choose-completion)
            (eq this-command 'choose-completion)
            (eq this-command 'py-shell-complete)
            (and (or
                  (eq last-command 'completion-at-point)
                  (eq last-command 'choose-completion)
                  (eq last-command 'choose-completion)
                  (eq last-command 'py-shell-complete))
                 (eq this-command 'self-insert-command))))
    (py-restore-window-configuration)
    )

  (goto-char end))

(defalias 'ipython-complete 'py-shell-complete)

(defun py--shell-insert-completion-maybe (completion input)
  (cond ((eq completion t)
	 (and py-verbose-p (message "py--shell-do-completion-at-point %s" "`t' is returned, not completion. Might be a bug.")))
	((null completion)
	 (and py-verbose-p (message "py--shell-do-completion-at-point %s" "Don't see a completion")))
	((and completion
	      (or (and (listp completion)
		       (string= input (car completion)))
		  (and (stringp completion)
		       (string= input completion)))))
	((and completion (stringp completion)(or (string= input completion) (string= "''" completion))))
	((and completion (stringp completion))
	 (progn (delete-char (- (length input)))
		(insert completion)))
	(t (py--try-completion input completion)))
  )

(defun py--shell-do-completion-at-point (process imports input exception-buffer code)
  "Do completion at point for PROCESS.

Takes PROCESS IMPORTS INPUT EXCEPTION-BUFFER CODE"
  (when imports
    (py-execute-string imports process))
  (sit-for 0.1 t)
  (let* ((completion
	  (py--shell-completion-get-completions
	   input process code)))
    (set-buffer exception-buffer)
    (when completion
      (py--shell-insert-completion-maybe completion input))))

(defun py--complete-base (shell word imports buffer)
  (let* ((proc (or
		;; completing inside a shell
		(get-buffer-process buffer)
		(and (comint-check-proc shell)
		     (get-process shell))
		(prog1
		    (get-buffer-process (py-shell nil nil nil shell))
		  (sit-for py-new-shell-delay t))))
	 ;; (buffer (process-buffer proc))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 py-shell-module-completion-code)))
    (py--shell-do-completion-at-point proc imports word buffer code)))

(defun py--try-completion-intern (input completion buffer)
  (with-current-buffer buffer
    (let (erg)
      (and (setq erg (try-completion input completion))
	   (sit-for 0.1)
	   (looking-back input (line-beginning-position))
	   (not (string= input erg))
	   (setq erg (completion-in-region (match-beginning 0) (match-end 0) completion)))))
  ;; (set-window-configuration py-last-window-configuration)
  )

(defun py--try-completion (input completion)
  "Repeat `try-completion' as long as match are found.

Interal used. Takes INPUT COMPLETION"
  (let (erg newlist)
    (unless (py--try-completion-intern input completion (current-buffer))
      (dolist (elt completion)
	(unless (string= erg elt)
	  (push elt newlist)))
      (if (< 1 (length newlist))
	  (with-output-to-temp-buffer py-python-completions
	    (display-completion-list
	     (all-completions input (or newlist completion))))))))

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

(defun py-shell-complete (&optional shell beg end word fast imports)
  (interactive)
  (let* ((exception-buffer (current-buffer))
	 (pps (parse-partial-sexp
	       (or
		(ignore-errors (cdr-safe comint-last-prompt))
		(ignore-errors comint-last-prompt)
		(line-beginning-position))
	       (point)))
	 (in-string (when (nth 3 pps) (nth 8 pps)))
         (beg
	  (save-excursion
	    (or beg
	 	(and in-string
	 	     ;; possible completion of filenames
	 	     (progn
	 	       (goto-char in-string)
	 	       (and
	 		(save-excursion
	 		  (skip-chars-backward "^ \t\r\n\f") (looking-at "open")))

	 	       (skip-chars-forward "\"'") (point)))
	 	(progn (and (eq (char-before) ?\()(forward-char -1))
	 	       (skip-chars-backward "a-zA-Z0-9_.'") (point)))))
         (end (or end (point)))
	 (word (or word (buffer-substring-no-properties beg end)))
	 (ausdruck (and (string-match "^/" word) (setq word (substring-no-properties word 1))(concat "\"" word "*\"")))
	 ;; when in string, assume looking for filename
	 (filenames (and in-string ausdruck
			 (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
         (imports (or imports (py-find-imports)))
         py-fontify-shell-buffer-p erg)
    (cond (fast (py--fast-complete-base shell word imports))
	  ((and in-string filenames)
	   (when (setq erg (try-completion (concat "/" word) filenames))
	     (delete-region beg end)
	     (insert erg)))
	  (t (py--complete-base shell word imports exception-buffer)))
    nil))

(defun py-fast-complete (&optional shell word imports)
  "Complete word before point, if any.

Use `py-fast-process' "
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  (setq py-last-window-configuration
  	(current-window-configuration))
  (py-shell-complete shell nil nil word 1 imports)
  (py-restore-window-configuration)
  )

(defun py-indent-or-complete ()
  "Complete or indent depending on the context.

If cursor is at end of a symbol, try to complete
Otherwise call `py-indent-line'

If `(use-region-p)' returns t, indent region.
Use `C-q TAB' to insert a literally TAB-character

In ‘python-mode’ `py-complete-function' is called,
in (I)Python shell-modes `py-shell-complete'"
  (interactive "*")
  (window-configuration-to-register py--windows-config-register)
  ;; (setq py-last-window-configuration
  ;;       (current-window-configuration))
  (cond ((use-region-p)
	 (py-indent-region (region-beginning) (region-end)))
	((or (bolp)
	     (member (char-before) (list 9 10 12 13 32 ?: ?\) ?\] ?\}))
	     (not (looking-at "[ \t]*$")))
	 (py-indent-line))
	((comint-check-proc (current-buffer))
	 ;; (let* ((shell (process-name (get-buffer-process (current-buffer)))))
	 (ignore-errors (completion-at-point)))
	(t
	 ;; (py-fast-complete)
	 (completion-at-point))))

(provide 'python-components-complete)
;;; python-components-complete.el here
