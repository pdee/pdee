;;; python-components-start3.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

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

;; As for ‘py-add-abbrev’:
;; Similar to ‘add-mode-abbrev’, but uses
;; ‘py-partial-expression’ before point for expansion to
;; store, not ‘word’.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; ‘py-expression’ composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; ‘py-partial-expression’ beginns with a "(", which is
;; not taken as proposal.

;;; Code:

(defun toggle-force-py-shell-name-p (&optional arg)
  "If customized default ‘py-shell-name’ should be enforced upon execution.

If ‘py-force-py-shell-name-p’ should be on or off.
Returns value of ‘py-force-py-shell-name-p’ switched to.

Optional ARG
See also commands
‘force-py-shell-name-p-on’
‘force-py-shell-name-p-off’

Caveat: Completion might not work that way."
  (interactive)
  (let ((arg (or arg (if py-force-py-shell-name-p -1 1))))
    (if (< 0 arg)
        (setq py-force-py-shell-name-p t)
      (setq py-force-py-shell-name-p nil))
    (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
    py-force-py-shell-name-p))

(defun force-py-shell-name-p-on ()
  "Switch ‘py-force-py-shell-name-p’ on.

Customized default ‘py-shell-name’ will be enforced upon execution.
Returns value of ‘py-force-py-shell-name-p’.

Caveat: Completion might not work that way."
  (interactive)
  (toggle-force-py-shell-name-p 1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun force-py-shell-name-p-off ()
  "Make sure, ‘py-force-py-shell-name-p’ is off.

Function to use by executes will be guessed from environment.
Returns value of ‘py-force-py-shell-name-p’."
  (interactive)
  (toggle-force-py-shell-name-p -1)
  (when (or py-verbose-p (called-interactively-p 'any)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun py--fix-if-name-main-permission (strg)
  "Remove \"if __name__ == '__main__ '\" STRG from code to execute.

See ‘py-if-name-main-permission-p’"
  (let ((strg (if py-if-name-main-permission-p strg
		(replace-regexp-in-string
		 "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
		 ;; space after __main__, i.e. will not be executed
		 "if __name__ == '__main__ ':" strg))))
    strg))

(defun py-symbol-at-point ()
  "Return the current Python symbol.

When interactively called, copy and message it"
  (interactive)
  (let ((erg (with-syntax-table
                 py-dotted-expression-syntax-table
               (current-word))))
    (when (called-interactively-p 'interactive) (kill-new erg)
	  (message "%s" erg))
    erg))

(defun py--line-backward-maybe ()
  "Return result of (< 0 (abs (skip-chars-backward \" \\t\\r\\n\\f\"))) "
  (skip-chars-backward " \t\f" (line-beginning-position))
  (< 0 (abs (skip-chars-backward " \t\r\n\f"))))

(defun py--after-empty-line ()
  "Return ‘t’ if line before contains only whitespace characters. "
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py--guess-indent-final (indents)
  "Calculate and do sanity-check.

Expects INDENTS, a cons"
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      (- first second)
                    (- second first))
                (default-value (quote py-indent-offset)))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))

(defun py--guess-indent-forward ()
  "Called when moving to end of a form and ‘py-smart-indentation’ is on."
  (let* ((first (if
                    (py--beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (py-forward-statement)
                    (py-backward-statement)
                    (current-indentation))))
         (second (if (or (looking-at py-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (py-forward-statement)
                       (py-forward-statement)
                       (py-backward-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward py-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (parse-partial-sexp (point-min) (point))))))
                   (current-indentation))))
    (list first second)))

(defun py--guess-indent-backward ()
  "Called when moving to beginning of a form and ‘py-smart-indentation’ is on."
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward py-extended-block-or-clause-re nil 'move 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (parse-partial-sexp (point-min) (point))))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (py-forward-statement) (py-forward-statement) (py-backward-statement)(current-indentation))))
    (list first second)))

(defun py-guess-indent-offset (&optional direction)
  "Guess ‘py-indent-offset’.

Set local value of ‘py-indent-offset’, return it

Might change local value of ‘py-indent-offset’ only when called
downwards from beginning of block followed by a statement.
Otherwise ‘default-value’ is returned.
Unless DIRECTION is symbol \\='forward, go backward first"
  (interactive)
  (save-excursion
    (let* ((indents
            (cond (direction
                   (if (eq 'forward direction)
                       (py--guess-indent-forward)
                     (py--guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (py--guess-indent-forward))
                  (t (py--guess-indent-backward))))
           (erg (py--guess-indent-final indents)))
      (if erg (setq py-indent-offset erg)
        (setq py-indent-offset
              (default-value (quote py-indent-offset))))
      (when (called-interactively-p 'any) (message "%s" py-indent-offset))
      py-indent-offset)))

(defun py--execute-buffer-finally (strg proc procbuf origline filename fast wholebuf)
  (if (and filename wholebuf (not (buffer-modified-p)))
      (py--execute-file-base filename proc nil procbuf origline fast)
    (let* ((tempfile (concat (expand-file-name py-temp-directory) py-separator-char "temp" (md5 (format "%s" (nth 3 (current-time)))) ".py")))
      (with-temp-buffer
	(insert strg)
	(write-file tempfile)
        (sit-for 0.1))
      (unwind-protect
	  (py--execute-file-base tempfile proc nil procbuf origline fast)
	(and (file-readable-p tempfile) (delete-file tempfile py-debug-p))))))

(defun py--postprocess-intern (&optional origline exception-buffer output-buffer)
  "Highlight exceptions found in BUF.

Optional ORIGLINE EXCEPTION-BUFFER
If an exception occurred return error-string,
otherwise return nil.
BUF must exist.

Indicate LINE if code wasn't run from a file,
thus remember line of source buffer"
  (save-excursion
    (with-current-buffer output-buffer
      (let* (estring ecode erg)
	;; (switch-to-buffer (current-buffer))
	(goto-char (point-max))
	(sit-for 0.1)
	(save-excursion
	  (unless (looking-back py-pdbtrack-input-prompt (line-beginning-position))
	    (forward-line -1)
	    (end-of-line)
	    (when (re-search-backward py-shell-prompt-regexp t 1)
		;; (or (re-search-backward py-shell-prompt-regexp nil t 1)
		;; (re-search-backward (concat py-ipython-input-prompt-re "\\|" py-ipython-output-prompt-re) nil t 1))
	      (save-excursion
		(when (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
		  (setq erg (copy-marker (point)))
		  (delete-region (progn (beginning-of-line)
					(save-match-data
					  (when (looking-at
						 ;; all prompt-regexp known
						 py-shell-prompt-regexp)
					    (goto-char (match-end 0)))))

					(progn (skip-chars-forward " \t\r\n\f"   (line-end-position))(point)))
		  (insert (concat "    File " (buffer-name exception-buffer) ", line "
				  (prin1-to-string origline)))))
	      ;; these are let-bound as ‘tempbuf’
	      ;; (and (boundp 'tempbuf)
	      ;;      (search-forward (buffer-name tempbuf) nil t)
	      ;;      (delete-region (line-beginning-position) (1+ (line-end-position))))
	      ;; if no buffer-file exists, signal "Buffer", not "File(when
	      (when erg
		(goto-char erg)
		;; (forward-char -1)
		;; (skip-chars-backward "^\t\r\n\f")
		;; (skip-chars-forward " \t")
		(save-match-data
		  (and (not (py--buffer-filename-remote-maybe
			     (or
			      (get-buffer exception-buffer)
			      (get-buffer (file-name-nondirectory exception-buffer)))))
		       (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
		       (looking-at "[ \t]*File")
		       (replace-match " Buffer")))
		(push origline py-error)
		(push (buffer-name exception-buffer) py-error)
		(forward-line 1)
		(when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
		  (setq estring (match-string-no-properties 1))
		  (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " estring))
		  (push (quote py-error) ecode))))))
	py-error))))

(defun py-execute-python-mode-v5 (start end origline filename)
  "Take START END &optional EXCEPTION-BUFFER ORIGLINE."
  (interactive "r")
  (let ((output-buffer "*Python Output*")
	(py-split-window-on-execute 'just-two)
	(pcmd (concat py-shell-name (if (string-equal py-which-bufname
                                                      "Jython")
                                        " -"
                                      ;; " -c "
                                      ""))))
    (save-excursion
      (shell-command-on-region start end
                               pcmd output-buffer))
    (if (not (get-buffer output-buffer))
        (message "No output.")
      (setq py-result (py--fetch-result (get-buffer  output-buffer) nil))
      (if (string-match "Traceback" py-result)
	  (message "%s" (setq py-error (py--fetch-error output-buffer origline filename)))
	py-result))))

(defun py--execute-ge24.3 (start end execute-directory which-shell &optional exception-buffer proc file origline)
  "An alternative way to do it.

According to START END EXECUTE-DIRECTORY WHICH-SHELL
Optional EXCEPTION-BUFFER PROC FILE ORIGLINE
May we get rid of the temporary file?"
  (and (py--buffer-filename-remote-maybe) buffer-offer-save (buffer-modified-p (py--buffer-filename-remote-maybe)) (y-or-n-p "Save buffer before executing? ")
       (write-file (py--buffer-filename-remote-maybe)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (exception-buffer (or exception-buffer (current-buffer)))
         (line (py-count-lines (point-min) (if (eq start (line-beginning-position)) (1+ start) start)))
         (strg (buffer-substring-no-properties start end))
         (tempfile (or (py--buffer-filename-remote-maybe) (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" "temp") ".py")))

         (proc (or proc (if py-dedicated-process-p
                            (get-buffer-process (py-shell nil nil t which-shell))
                          (or (get-buffer-process py-buffer-name)
                              (get-buffer-process (py-shell nil nil py-dedicated-process-p which-shell py-buffer-name))))))
         (procbuf (process-buffer proc))
         (file (or file (with-current-buffer py-buffer-name
                          (concat (file-remote-p default-directory) tempfile))))
         (filebuf (get-buffer-create file)))
    (set-buffer filebuf)
    (erase-buffer)
    (newline line)
    (save-excursion
      (insert strg))
    (py--fix-start (buffer-substring-no-properties (point) (point-max)))
    (unless (string-match "[jJ]ython" which-shell)
      ;; (when (and execute-directory py-use-current-dir-when-execute-p
      ;; (not (string= execute-directory default-directory)))
      ;; (message "Warning: options ‘execute-directory’ and ‘py-use-current-dir-when-execute-p’ may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    (sit-for 0.1 t)
    (if (and (setq py-error (save-excursion (py--postprocess-intern origline exception-buffer)))
             (car py-error)
             (not (markerp py-error)))
        (py--jump-to-exception py-error origline)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defun py--execute-base-intern (strg filename proc wholebuf buffer origline execute-directory start end &optional fast)
  "Select the handler according to:

STRG FILENAME PROC FILE WHOLEBUF
BUFFER ORIGLINE EXECUTE-DIRECTORY START END WHICH-SHELL
Optional FAST RETURN"
  (setq py-error nil)
  (cond ;; (fast (py-fast-send-string strg proc buffer result))
   ;; enforce proceeding as python-mode.el v5
   (python-mode-v5-behavior-p
    (py-execute-python-mode-v5 start end origline filename))
   (py-execute-no-temp-p
    (py--execute-ge24.3 start end execute-directory py-shell-name py-exception-buffer proc filename origline))
   ((and filename wholebuf)
    (py--execute-file-base filename proc nil buffer origline fast))
   (t
    ;; (message "(current-buffer) %s" (current-buffer))
    (py--execute-buffer-finally strg proc buffer origline filename fast wholebuf)
    ;; (py--delete-temp-file tempfile)
    )))

(defun py--execute-base (&optional start end shell filename proc wholebuf fast dedicated split switch)
  "Update optional variables.
START END SHELL FILENAME PROC FILE WHOLEBUF FAST DEDICATED SPLIT SWITCH."
  (setq py-error nil)
  (when py-debug-p (message "py--execute-base: (current-buffer): %s" (current-buffer)))
  ;; (when (or fast py-fast-process-p) (ignore-errors (py-kill-buffer-unconditional py-output-buffer)))
  (let* ((orig (point))
	 (fast (or fast py-fast-process-p))
	 (exception-buffer (current-buffer))
	 (start (or start (and (use-region-p) (region-beginning)) (point-min)))
	 (end (or end (and (use-region-p) (region-end)) (point-max)))
	 (strg-raw (if py-if-name-main-permission-p
		       (buffer-substring-no-properties start end)
		     (py--fix-if-name-main-permission (buffer-substring-no-properties start end))))
	 (strg (py--fix-start strg-raw))
	 (wholebuf (unless filename (or wholebuf (and (eq (buffer-size) (- end start))))))
	 ;; error messages may mention differently when running from a temp-file
	 (origline
	  (format "%s" (save-restriction
			 (widen)
			 (py-count-lines (point-min) orig))))
	 ;; argument SHELL might be a string like "python", "IPython" "python3", a symbol holding PATH/TO/EXECUTABLE or just a symbol like 'python3
	 (shell (or
		 (and shell
		      ;; shell might be specified in different ways
		      (or (and (stringp shell) shell)
			  (ignore-errors (eval shell))
			  (and (symbolp shell) (format "%s" shell))))
		 ;; (save-excursion
		 (py-choose-shell)
		 ;;)
		 ))
	 (shell (or shell (py-choose-shell)))
	 (buffer-name
	  (py--choose-buffer-name shell dedicated fast))
	 (execute-directory
	  (cond ((ignore-errors (file-name-directory (file-remote-p (buffer-file-name) 'localname))))
		((and py-use-current-dir-when-execute-p (buffer-file-name))
		 (file-name-directory (buffer-file-name)))
		((and py-use-current-dir-when-execute-p
		      py-fileless-buffer-use-default-directory-p)
		 (expand-file-name default-directory))
		((stringp py-execute-directory)
		 py-execute-directory)
		((getenv "VIRTUAL_ENV"))
		(t (getenv "HOME"))))
	 (filename (or (and filename (expand-file-name filename))
		       (py--buffer-filename-remote-maybe)))
	 (py-orig-buffer-or-file (or filename (current-buffer)))
	 (proc-raw (or proc (get-buffer-process buffer-name)))

	 (proc (or proc-raw (get-buffer-process buffer-name)
		   (prog1
		       (get-buffer-process (py-shell nil nil dedicated shell buffer-name fast exception-buffer split switch))
		     (sit-for 1))))

         ;; (split (if python-mode-v5-behavior-p 'just-two split))
         )
    (setq py-output-buffer (or (and python-mode-v5-behavior-p py-output-buffer) (and proc (buffer-name (process-buffer proc)))
			       (py--choose-buffer-name shell dedicated fast)))
    (py--execute-base-intern strg filename proc wholebuf py-output-buffer origline execute-directory start end fast)))

(provide 'python-components-start3)
;;; python-components-start3.el ends here
