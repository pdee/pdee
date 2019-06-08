;;; python-components-shell-complete.el -- Add suport for completion in py-shell -*- lexical-binding: t; -*-

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
	 (py--send-string-return-output
	  (format completion-code input) process)))
    ;; (sit-for 0.2 t)
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
    (set-window-configuration
     py-last-window-configuration))
  (goto-char end))

(defalias 'ipython-complete 'py-shell-complete)

(defun py--try-completion-intern (input completion)
  (let (erg)
    (when (and (setq erg (try-completion input completion))
	       (looking-back input (line-beginning-position))
	       (not (string= input erg)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert erg))
    erg))

(defun py--try-completion (input completion)
  "Repeat `try-completion' as long as match are found.

Interal used. Takes INPUT COMPLETION"
  (let (erg newlist)
    (setq erg (py--try-completion-intern input completion))
    (when erg
      (dolist (elt completion)
	(unless (string= erg elt)
	  (push elt newlist)))
      (if (< 1 (length newlist))
	  (with-output-to-temp-buffer py-python-completions
	    (display-completion-list
	     (all-completions input (or newlist completion))))
	(set-window-configuration py-last-window-configuration))
      ;; (skip-chars-forward "^ \t\r\n\f")
)))

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
    (py--send-string-no-output imports process))
  ;; (py--delay-process-dependent process)
  (sit-for 0.1 t)
  (let* ((completion
	  (py--shell-completion-get-completions
	   input process code)))
    (set-buffer exception-buffer)
    ;; (py--delay-process-dependent process)
    ;; (sit-for 1 t)
    (when (and completion 
	       ;; (not (or (string= "''" completion) (string= "" completion)))
)
      (py--shell-insert-completion-maybe completion input))))

(defun py--complete-base (shell word imports buffer)
  (let* ((shell (or shell (py-choose-shell)))
         (proc (or
		;; completing inside a shell
		(get-buffer-process buffer)
		(and (comint-check-proc shell)
		     (get-process shell))
		(prog1
		    (get-buffer-process (py-shell nil nil shell))
		  (sit-for py-new-shell-delay))))
	 ;; (buffer (process-buffer proc))
	 (code (if (string-match "[Ii][Pp]ython*" shell)
		   (py-set-ipython-completion-command-string shell)
		 py-shell-module-completion-code)))
    (py--python-send-completion-setup-code buffer)
    (py--shell-do-completion-at-point proc imports word buffer code)))

(defun py-shell-complete (&optional shell beg end word fast-complete)
  (interactive) 
  (let* ((exception-buffer (current-buffer))
         ;; (pos (copy-marker (point)))
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
	 		  (skip-chars-backward "^ \t\r\n\f")(looking-at "open")))

	 	       (skip-chars-forward "\"'")(point)))
	 	(progn (and (eq (char-before) ?\()(forward-char -1))
	 	       (skip-chars-backward "a-zA-Z0-9_.'") (point)))))
         (end (or end (point)))
	 ;; (thing-at-point 'word t) fails with Emacs 24.3
	 ;; https://travis-ci.org/pdee/pdee/builds/527856403
	 ;; Test py-complete-empty-string-result-test condition:
	 ;;  (wrong-number-of-arguments
	 ;;   #[(thing)
	 ;;     \302N\203\302N \207\30!\211\205	@	A{)\207"
	 ;;     [thing bounds thing-at-point bounds-of-thing-at-point]
	 ;;     3
	 ;;     ("/usr/share/emacs/24.3/lisp/thingatpt.elc" . 2305)]
	 ;;   2)
	 ;; FAILED    19/1343  py-complete-empty-string-result-test
         (word (or word (buffer-substring-no-properties beg end)))
	 ;; (word (save-excursion (skip-chars-backward " \t\r\n\f") (unless (bolp) (forward-char -1) (thing-at-point 'word t))))  
	 (ausdruck (and (string-match "^/" word)(setq word (substring-no-properties word 1))(concat "\"" word "*\"")))
	 ;; when in string, assume looking for filename
	 (filenames (and in-string ausdruck
			 (list (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "find / -maxdepth 1 -name " ausdruck))))))
         (imports (py-find-imports))
         py-fontify-shell-buffer-p erg)
    (cond (fast-complete (py--fast-complete-base shell word imports))
	  ((and in-string filenames)
	   (when (setq erg (try-completion (concat "/" word) filenames))
	     (delete-region beg end)
	     (insert erg)))
	  (t (py--complete-base shell word imports exception-buffer)))
    nil))

;; (defun py-shell-complete (&optional shell beg end word)
;;   "Complete word before point, if any.

;; Optional SHELL BEG END WORD"
;;   (interactive)
;;   ;; (save-excursion
;;   ;;   (and (buffer-live-p (get-buffer "*Python Completions*"))
;;   ;; 	 (py-kill-buffer-unconditional "*Python Completions*")))
;;   ;; fast-complete is called
;;   (py-shell-complete shell beg end word))

(defun py-indent-or-complete ()
  "Complete or indent depending on the context.

If cursor is at end of a symbol, try to complete
Otherwise call `py-indent-line'

If `(use-region-p)' returns t, indent region.
Use `C-q TAB' to insert a literally TAB-character

In ‘python-mode’ `py-complete-function' is called,
in (I)Python shell-modes `py-shell-complete'"
  (interactive "*")
  (setq py-last-window-configuration
        (current-window-configuration))
  (cond ((use-region-p)
	 (py-indent-region (region-beginning) (region-end)))
	((or (bolp)
	     (member (char-before)(list 9 10 12 13 32 ?: ?\) ?\] ?\}))
	     (not (looking-at "[ \t]*$")))
	 (py-indent-line))
	;; ((or (eq major-mode 'python-mode)(derived-mode-p 'python-mode))	(if (string-match "ipython" (py-choose-shell))
	;;      (py-shell-complete)
	;;    (funcall py-complete-function)))
	((comint-check-proc (current-buffer))
	 (py-shell-complete (substring (process-name (get-buffer-process (current-buffer))) 0 (string-match "<" (process-name (get-buffer-process (current-buffer)))))))
	(t
	 (py-fast-complete))))

(provide 'python-components-shell-complete)
;;; python-components-shell-complete.el ends here
