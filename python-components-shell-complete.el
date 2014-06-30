;;; python-components-shell-complete.el -- Add suport for completion in py-shell

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

(defalias 'py-script-complete 'py-shell-complete)
(defalias 'py-python2-shell-complete 'py-shell-complete)
(defalias 'py-python3-shell-complete 'py-shell-complete)

(defun py--shell-completion-get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (let ((completions
	 (py--send-string-no-output
	  (format completion-code input) process)))
    (sit-for 0.1)
    (when (> (length completions) 2)
      (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t))))

;; post-command-hook
;; caused insert-file-contents error lp:1293172
(defun py--after-change-function (beg end len)
  "Restore window-confiuration after completion. "
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
    (set-window-configuration
     py-completion-last-window-configuration))
  (goto-char end))

(defalias 'ipython-complete 'py-shell-complete)
(defun py--shell--do-completion-at-point (process imports input orig oldbuf code)
  "Do completion at point for PROCESS."
    (when imports
      (py--send-string-no-output imports process))
    (let* ((completion
            (py--shell-completion-get-completions
             input process code))
           ;; (completion (when completions
	   ;; (try-completion input completions)))
	   newlist erg)
      (with-current-buffer oldbuf
        (cond ((eq completion t)
	       (and py-verbose-p (message "py--shell--do-completion-at-point %s" "`t' is returned, not completion. Might be a bug."))
               nil)
              ((null completion)
	       (and py-verbose-p (message "py--shell--do-completion-at-point %s" "Don't see a completion"))
               nil)
              ((ignore-errors (not (string= input completion)))
               (progn (delete-char (- (length input)))
                      (insert completion)
                      (move-marker orig (point))
                      ;; minibuffer.el expects a list, a bug IMO
                      nil))
              (t
	       (when (and (stringp (setq erg (try-completion input completion)))
			  (looking-back input)
			  (not (string= input erg)))
		 (delete-region (match-beginning 0) (match-end 0))
		 (insert erg)
		 (dolist (elt completion)
		   (unless (string= erg elt)
		     (add-to-list 'newlist elt))))
               (with-output-to-temp-buffer py-python-completions
                 (display-completion-list
                  (all-completions input (or newlist completion))))
               (move-marker orig (point))
               nil))
	(and (goto-char orig)
	     nil))))

(defun py--complete-base (shell pos beg end word imports debug oldbuf)
  (let* ((shell (or shell (py-choose-shell)))
         (proc (or (get-process shell)
		   (prog1
		       (get-buffer-process (py-shell nil nil shell))
		     (sit-for py-new-shell-delay))))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 python-shell-module-completion-string-code)))
    (py--shell--do-completion-at-point proc imports word pos oldbuf code)))

(defun py-shell-complete (&optional shell debug beg end word)
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (setq py-completion-last-window-configuration
        (current-window-configuration))
  (when debug (setq py-shell-complete-debug nil))
  (let* ((oldbuf (current-buffer))
         (pos (copy-marker (point)))
	 (pps (syntax-ppss))
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
			  (skip-chars-backward "^ \t\r\n\f")(looking-at "open")))

		       (skip-chars-forward "\"'")(point)))
		(progn (and (eq (char-before) ?\()(forward-char -1))
		       (skip-chars-backward "a-zA-Z0-9_.'") (point)))))
         (end (or end (point)))
	 ;;
         (word (or word (buffer-substring-no-properties beg end)))
	 (ausdruck (and (string-match "^/" word)(setq word (substring-no-properties word 1))(concat "\"" word "*\"")))
	 ;; when in string, assume looking for filename
	 (filenames (and in-string ausdruck
			 (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
         (imports (py-find-imports))
         py-fontify-shell-buffer-p completion-buffer erg)
    (sit-for 0.1)
    (cond ((and in-string filenames)
	   (when (setq erg (try-completion (concat "/" word) filenames))
	     (delete-region beg end)
	     (insert erg)))
	  (t (py--complete-base shell pos beg end word imports debug oldbuf)))
    nil))

(defun py-indent-or-complete ()
  "Complete or indent depending on the context.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

Use `C-q TAB' to insert a literally TAB-character "
  (interactive "*")
  (if (eolp)
      (py-shell-complete)
    (py-indent-line)))

(provide 'python-components-shell-complete)
;; pyshell-complete.el ends here
