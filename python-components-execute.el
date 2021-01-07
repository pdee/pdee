;;; python-components-execute.el --- Part of python-components-mode -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs

;; Keywords: languages, processes

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

;;

;;; Code:

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With EOB-P, go to end of buffer."
  (interactive "p")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
  (when eob-p
    (goto-char (point-max))))

;;  Split-Windows-On-Execute forms
(defalias 'toggle-py-split-windows-on-execute 'py-toggle-split-windows-on-execute)
(defun py-toggle-split-windows-on-execute (&optional arg)
  "If ‘py-split-window-on-execute’ should be on or off.

optional ARG
  Returns value of ‘py-split-window-on-execute’ switched to."
  (interactive)
  (let ((arg (or arg (if py-split-window-on-execute -1 1))))
    (if (< 0 arg)
        (setq py-split-window-on-execute t)
      (setq py-split-window-on-execute nil))
    (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    py-split-window-on-execute))

(defun py-split-windows-on-execute-on (&optional arg)
  "Make sure, ‘py-split-window-on-execute’ according to ARG.

Returns value of ‘py-split-window-on-execute’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-split-windows-on-execute arg))
  (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

(defun py-split-windows-on-execute-off ()
  "Make sure, ‘py-split-window-on-execute’ is off.

Returns value of ‘py-split-window-on-execute’."
  (interactive)
  (toggle-py-split-windows-on-execute -1)
  (when (called-interactively-p 'any) (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  py-split-window-on-execute)

;;  Shell-Switch-Buffers-On-Execute forms
(defalias 'py-toggle-switch-buffers-on-execute 'py-toggle-shell-switch-buffers-on-execute)
(defalias 'toggle-py-shell-switch-buffers-on-execute 'py-toggle-shell-switch-buffers-on-execute)
(defun py-toggle-shell-switch-buffers-on-execute (&optional arg)
  "If ‘py-switch-buffers-on-execute-p’ according to ARG.

  Returns value of ‘py-switch-buffers-on-execute-p’ switched to."
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-shell-switch-buffers-on-execute-on (&optional arg)
  "Make sure, ‘py-switch-buffers-on-execute-p’ according to ARG.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-shell-switch-buffers-on-execute arg))
  (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-shell-switch-buffers-on-execute-off ()
  "Make sure, ‘py-switch-buffers-on-execute-p’ is off.

Returns value of ‘py-switch-buffers-on-execute-p’."
  (interactive)
  (toggle-py-shell-switch-buffers-on-execute -1)
  (when (called-interactively-p 'any) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-guess-default-python ()
  "Defaults to \"python\", if guessing didn't succeed."
  (interactive)
  (let* ((ptn (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p ptn (executable-find ptn))))
    (when (called-interactively-p 'any)
      (if erg
          (message "%s" ptn)
        (message "%s" "Could not detect Python on your system")))))

;;  from ipython.el
(defun py-dirstack-hook ()
  "To synchronize dir-changes."
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
  "Start an interpreter in another window according to ARGPROMPT.

With optional \\[universal-argument] user is prompted by
‘py-choose-shell’ for command and options to pass to the Python
interpreter."
  (interactive "P")
  (py-shell argprompt nil t))




(defun py-kill-shell-unconditional (&optional shell)
  "With optional argument SHELL.

Otherwise kill default (I)Python shell.
Kill buffer and its process.
Receives a ‘buffer-name’ as argument"
  (interactive)
  (let ((shell (or shell (py-shell))))
    (ignore-errors (py-kill-buffer-unconditional shell))))

(defun py-kill-default-shell-unconditional ()
  "Kill buffer \"\*Python\*\" and its process."
  (interactive)
  (ignore-errors (py-kill-buffer-unconditional "*Python*")))

(defun py--report-executable (buffer)
  (let ((erg (downcase (replace-regexp-in-string
                        "<\\([0-9]+\\)>" ""
                        (replace-regexp-in-string
                         "\*" ""
                         (if
                             (string-match " " buffer)
                             (substring buffer (1+ (string-match " " buffer)))
                           buffer))))))
    (when (string-match "-" erg)
      (setq erg (substring erg 0 (string-match "-" erg))))
    erg))

(defun py--guess-buffer-name (argprompt dedicated)
  "Guess the ‘buffer-name’ core string according to ARGPROMPT DEDICATED."
  (when (and (not dedicated) argprompt
	     (eq 4 (prefix-numeric-value argprompt)))
    (read-buffer "Py-Shell buffer: "
		 (generate-new-buffer-name (py--choose-buffer-name)))))

(defun py--configured-shell (name)
  "Return the configured PATH/TO/STRING if any according to NAME."
  (if (string-match "//\\|\\\\" name)
      name
    (cond ((string-match "^[Ii]" name)
	   (or py-ipython-command name))
	  ((string-match "[Pp]ython3" name)
	   (or py-python3-command name))
	  ((string-match "[Pp]ython2" name)
	   (or py-python2-command name))
	  ((string-match "[Jj]ython" name)
	   (or py-jython-command name))
	  (t (or py-python-command name)))))

(defun py--determine-local-default ()
  (if (not (string= "" py-shell-local-path))
      (expand-file-name py-shell-local-path)
    (when py-use-local-default
      (error "Abort: ‘py-use-local-default’ is set to t but ‘py-shell-local-path’ is empty. Maybe call ‘py-toggle-local-default-use’"))))


(defun py-shell-get-process (&optional argprompt args dedicated shell buffer)
  "Get appropriate Python process for current buffer and return it.

Optional ARGPROMPT DEDICATED SHELL BUFFER"
  (interactive)
  (or (and buffer (get-buffer-process buffer))
      (get-buffer-process (current-buffer))
      (get-buffer-process (py-shell argprompt args dedicated shell buffer))))

(defun py-switch-to-shell ()
  "Switch to Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;;  Code execution commands

(defun py--store-result-maybe (erg)
  "If no error occurred and ‘py-store-result-p’ store ERG for yank."
  (and (not py-error) erg (or py--debug-p py-store-result-p) (kill-new erg)))

(defun py-current-working-directory ()
  "Return the directory of current python SHELL."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 erg)
    (if proc
	(setq erg (py-send-string (concat "import os\;os.getcwd()") proc nil t))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.getcwd())\"")))))
    (when (called-interactively-p 'interactive)
      (message "CWD: %s" erg))
    erg))

(defun py-set-working-directory (&optional directory)
  "Set working directory according to optional DIRECTORY

when given, to value of ‘py-default-working-directory’ otherwise"
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (dir (or directory py-default-working-directory))
	 erg)
    ;; (py-send-string (concat "import os\;os.chdir(\"" dir "\")") proc nil t)
    (py-execute-string (concat "import os\;os.chdir(\"" dir "\")") proc nil t)
    (setq erg (py-send-string "os.getcwd()" proc nil t))
    (when (called-interactively-p 'interactive)
      (message "CWD changed to: %s" erg))
    erg))

(defun py--update-execute-directory-intern (dir proc procbuf fast)
  (let ((strg (concat "import os\;os.chdir(\"" dir "\")")))
    (if fast
	(py-fast-send-string strg proc procbuf t t)
      (py-send-string strg proc nil t))))
;; (comint-send-string proc (concat "import os;os.chdir(\"" dir "\")\n")))

(defun py--update-execute-directory (proc procbuf execute-directory fast)
  (with-current-buffer procbuf
    (let ((cwd (py-current-working-directory)))
      (unless (string= execute-directory (concat cwd "/"))
	(py--update-execute-directory-intern (or py-execute-directory execute-directory) proc procbuf fast)))))

(defun py--close-execution (tempbuf tempfile)
  "Delete TEMPBUF and TEMPFILE."
  (unless py--debug-p
    (when tempfile (py-delete-temporary tempfile tempbuf))))

(defun py-shell-send-file (file-name &optional process temp-file-name
                                     delete)
  "Send FILE-NAME to Python PROCESS.

If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument."
  (interactive
   (list
    (read-file-name "File to send: ")))
  (let* ((proc (or process (py-shell-get-process)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (py-info-encoding)))
         (file-name (expand-file-name (file-local-name file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (file-local-name temp-file-name)))))
    (py-shell-send-string
     (format
      (concat
       "import codecs, os;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "exec(compile(__code, '''%s''', 'exec'));")
      (or temp-file-name file-name) encoding encoding file-name)
     proc)))

;; (defun py-execute-region (&optional beg end shell dedicated fast split switch proc)
;;   "Send region at point to  interpreter."
;;   (interactive "r")
;;   (let ((beg (or beg (region-beginning)))
;; 	(end (or end (region-end))))
;;     (if (and beg end)
;;     (py--execute-base beg end shell dedicated fast split switch proc)
;;     (error "py-execute-region: Don't any region"))))

(defun py--python-send-setup-code-intern (name buffer)
  "Send setup code to BUFFER according to NAME, a string."
  (save-excursion
    (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-" name "-setup-code.py"))
	  py-return-result-p py-store-result-p)
      (unless (file-readable-p setup-file)
	(with-temp-buffer
	  (insert (eval (car (read-from-string (concat "py-" name "-setup-code")))))
	  (write-file setup-file)))
      (py--execute-file-base setup-file (get-buffer-process buffer) nil buffer nil t)
      ;; (when py-verbose-p (message "%s" (concat name " setup-code sent to " (process-name (get-buffer-process buffer)))))
      )))

(defun py--python-send-completion-setup-code (buffer)
  "For Python see py--python-send-setup-code.
Argument BUFFER the buffer completion code is sent to."
  (py--python-send-setup-code-intern "shell-completion" buffer))

(defun py--ipython-import-module-completion ()
  "Setup IPython v0.11 or greater.

Used by `py-ipython-module-completion-string'"
  (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-ipython-module-completion.py")))
    (unless (file-readable-p setup-file)
      (with-temp-buffer
	(insert py-ipython-module-completion-code)
	(write-file setup-file)))
    (py--execute-file-base setup-file nil nil (current-buffer) nil t)))



(defun py-delete-temporary (&optional file filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf)))

(defun py--insert-offset-lines (line)
  "Fix offline amount, make error point at the correct LINE."
  (insert (make-string (- line (py-count-lines (point-min) (point))) 10)))

(defun py-execute-file (filename &optional proc)
  "When called interactively, user is prompted for FILENAME."
  (interactive "fFilename: ")
  (let (;; postprocess-output-buffer might want origline
        (origline 1)
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base (expand-file-name filename) nil nil nil origline))
          (py--execute-file-base (expand-file-name filename) proc))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
    (py--shell-manage-windows py-output-buffer py-exception-buffer nil
                              (or (called-interactively-p 'interactive)))
    erg))



(defun py-execute-string-dedicated (&optional strg shell switch fast)
  "Send the argument STRG to an unique Python interpreter.

Optional SHELL SWITCH FAST
See also ‘py-execute-region’."
  (interactive)
  (let ((strg (or strg (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert strg)
      (py-execute-region (point-min) (point-max) shell t switch fast))))

(defun py--insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-forward-statement)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless (eq 9 (char-after)) (newline 1))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

;; ‘py-execute-line’ calls void function, lp:1492054,  lp:1519859
(or (functionp 'indent-rigidly-left)
    (defun indent-rigidly--pop-undo ()
      (and (memq last-command '(indent-rigidly-left indent-rigidly-right
						    indent-rigidly-left-to-tab-stop
						    indent-rigidly-right-to-tab-stop))
	   (consp buffer-undo-list)
	   (eq (car buffer-undo-list) nil)
	   (pop buffer-undo-list)))

    (defun indent-rigidly-left (beg end)
      "Indent all lines between BEG and END leftward by one space."
      (interactive "r")
      (indent-rigidly--pop-undo)
      (indent-rigidly
       beg end
       (if (eq (current-bidi-paragraph-direction) 'right-to-left) 1 -1))))





(defun py--qualified-module-name (file)
  "Return the fully qualified Python module name for FILE.

FILE is a string.  It may be an absolute or a relative path to
any file stored inside a Python package directory, although
typically it would be a (absolute or relative) path to a Python
source code file stored inside a Python package directory.

This collects all directories names that have a __init__.py
file in them, starting with the directory of FILE and moving up."
  (let ((module-name (file-name-sans-extension (file-name-nondirectory file)))
        (dirname     (file-name-directory (expand-file-name file))))
    (while (file-exists-p (expand-file-name "__init__.py" dirname))
      (setq module-name
            (concat
             (file-name-nondirectory (directory-file-name dirname))
             "."
             module-name))
      (setq dirname (file-name-directory (directory-file-name dirname))))
    module-name))

(defun py-execute-import-or-reload (&optional shell)
  "Import the current buffer's file in a Python interpreter.

Optional SHELL
If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do ‘py-execute-buffer’
instead.

If the file local variable ‘py-master-file’ is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of ‘py-execute-import-or-reload-save-p’.

See also ‘\\[py-execute-region]’.

This may be preferable to ‘\\[py-execute-buffer]’ because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive)
  ;; Check file local variable py-master-file
  (when py-master-file
    (let* ((filename (expand-file-name py-master-file))
           (buffer (or (get-file-buffer filename)
                       (find-file-noselect filename))))
      (set-buffer buffer)))
  (let ((py-shell-name (or shell (py-choose-shell)))
        (file (py--buffer-filename-remote-maybe)))
    (if file
        (let ((proc (or
                     (ignore-errors (get-process (file-name-directory shell)))
                     (get-buffer-process (py-shell nil nil py-dedicated-process-p shell (or shell (default-value 'py-shell-name)))))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py--execute-file-base file proc
                                (if (string-match "\\.py$" file)
                                    (let ((m (py--qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" py-shell-name)
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-which-execute-file-command file))))
      (py-execute-buffer))))

;;  Fixme: Try to define the function or class within the relevant
;;  module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the Python process."
  (interactive)
  (save-excursion (py-execute-region (progn (beginning-of-defun) (point))
                                     (progn (end-of-defun) (point)))))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python FILENAME\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given."
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output")))
        (pcmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat pcmd " " filename) output-buffer error-buffer)
    (when py-switch-buffers-on-execute-p (switch-to-buffer output-buffer))))

(defvar py-last-exeption-buffer nil
  "Internal use only - when ‘py-up-exception’ is called.

In source-buffer, this will deliver the exception-buffer again.")

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when ‘py-highlight-error-source-p’ is non-nil."
  (interactive "*")
  (delete-overlay (car (overlays-at (point)))))

(defun py-mouseto-exception (event)
  "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point)
    ;; XEmacs
    (let* ((point (event-point event))
           (buffer (event-buffer event))
           (e (and point buffer (extent-at point buffer 'py-exc-info)))
           (info (and e (extent-property e 'py-exc-info))))
      (message "Event point: %d, info: %s" point info)
      (and info
           (py--jump-to-exception (car info) nil (cdr info)))))))

(defun py-goto-exception (&optional file line)
  "Go to FILE and LINE indicated by the traceback."
  (interactive)
  (let ((file file)
        (line line))
    (unless (and file line)
      (save-excursion
        (beginning-of-line)
        (if (looking-at py-traceback-line-re)
            (setq file (substring-no-properties (match-string 1))
                  line (string-to-number (match-string 2))))))
    (if (not file)
        (error "Not on a traceback line"))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun py--find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
‘re-search-backward’ or ‘re-search-forward’ indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (with-current-buffer buffer
	(goto-char (py--point start))
	(if (funcall searchdir py-traceback-line-re nil t)
	    (setq file (match-string 1)
		  line (string-to-number (match-string 2))))))
    (if (and file line)
        (py-goto-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if bottom
        (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py--find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((buffer py-output-buffer))
    (if top
        (py--find-next-exception 'bob buffer 're-search-forward "Top")
      (py--find-next-exception 'bol buffer 're-search-backward "Top"))))
;; ;
;;  obsolete by py--fetch-result
;;  followed by py--fetch-error
;;  still used by py--execute-ge24.3


(defun py--find-next-exception-prepare (direction start)
  "According to DIRECTION and START setup exception regexps.

Depends from kind of Python shell."
  (let* ((name (get-process (substring (buffer-name (current-buffer)) 1 -1)))
         (buffer (cond (name (buffer-name (current-buffer)))
                       ((buffer-live-p (get-buffer py-output-buffer))
                        py-output-buffer)
                       (py-last-exeption-buffer (buffer-name py-last-exeption-buffer))
                       (t (error "Don't see exeption buffer")))))
    (when buffer (set-buffer (get-buffer buffer)))
    (if (eq direction 'up)
        (if (string= start "TOP")
            (py--find-next-exception 'bob buffer 're-search-forward "Top")
          (py--find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py--find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defalias 'ipython-send-and-indent 'py-execute-line-ipython)
(defalias 'py-execute-region-in-shell 'py-execute-region)
(defalias 'py-ipython-shell-command-on-region 'py-execute-region-ipython)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)
(defalias 'py-send-region 'py-execute-region)

(provide 'python-components-execute);
;;;  python-components-execute.el ends here
