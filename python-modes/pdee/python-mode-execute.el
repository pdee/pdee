;;; python-mode-execute.el --- Part of python-components-mode

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>

;; Python-components-mode started from python-mode.el
;; and python.el, where Tim Peters, Barry A. Warsaw,
;; Skip Montanaro, Ken Manheimer, Dave Love and many
;; others wrote major parts.

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

(defvar py-execute-keep-temporary-file-p nil
  "For tests only. Excute functions delete temporary files default. ")

(defun py-toggle-execute-keep-temporary-file-p ()
  "Toggle py-execute-keep-temporary-file-p "
  (interactive)
  (setq py-execute-keep-temporary-file-p
        (not py-execute-keep-temporary-file-p))
  (when (interactive-p) (message "py-execute-keep-temporary-file-p: %s" py-execute-keep-temporary-file-p)))

(defcustom py-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python)

(defcustom py-shell-prompt-alist
  '(("ipython" . "^In \\[[0-9]+\\]: *")
    (t . "^>>> "))
  "Alist of Python input prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python
  :version "24.1")

(defcustom py-shell-continuation-prompt-alist
  '(("ipython" . "^   [.][.][.]+: *")
    (t . "^[.][.][.] "))
  "Alist of Python continued-line prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt for
continued lines.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python
  :version "24.1")

(defcustom py-cleanup-temporary  t
 "If temporary buffers and files used by functions executing region  should be deleted afterwards. "

:type 'boolean
:group 'python
)

(defvar py-prev-dir/file nil
  "Caches (directory . file) pair used in the last `py-load-file' command.
Used for determining the default in the next one.")

(autoload 'comint-get-source "comint")

(defvar py-exception-buffer nil)

(defvar py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
             py-file-queue)
    (if py-shell-switch-buffers-on-execute
        (pop-to-buffer (current-buffer)))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (py-execute-file pyproc (car py-file-queue))))))

(defun py-guess-default-python ()
  "If any Python is installed. Used by `py-shell' if `py-shell-name' is neither selected nor has a customized default value. "
  (interactive)
  (let* ((cmd (or (py-choose-shell) "python"))
         (erg (executable-find cmd)))
    (when (interactive-p)
      (if erg
          (message "%s" cmd)
        (message "%s" "Could not detect Python on your system")))))

(defun py-process-name (&optional name dedicated)
  "Return the name of the running Python process, `get-process' willsee it. "
  (let* ((name (cond (dedicated
                      (make-temp-name (concat (or name py-shell-name) "-")))
                     ;; ((string-match "\*" (buffer-name))
                     ;; (replace-regexp-in-string "\*" "" (buffer-name)))
                     (t (or name py-shell-name))))
         (erg (if (string= "ipython" name)
                  "IPython"
                (capitalize name))))
    erg))

;; from ipython.el
(defun py-dirstack-hook ()
  ;; the following is to synchronize dir-changes
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
   "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
"
  (interactive "P")
  (py-shell argprompt t))

(defun py-shell (&optional argprompt dedicated)
  "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
Returns variable `py-process-name' used by function `get-process'.
"
  (interactive "P")
  ;; Set or select the shell if not ready
  (if (eq 4 (prefix-numeric-value argprompt))
      (py-choose-shell '(4))
    (when (null py-shell-name)
      (py-guess-default-python)))
  (let ((args py-python-command-args)
        (py-process-name (py-process-name py-shell-name dedicated)))
    ;; comint
    (if (not (equal (buffer-name) py-process-name))
        (switch-to-buffer-other-window
         (apply 'make-comint py-process-name py-shell-name nil args))
      (apply 'make-comint py-process-name py-shell-name nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat py-shell-input-prompt-1-regexp "\\|"
                                       py-shell-input-prompt-2-regexp "\\|"
                                       "^([Pp]db) "))
    (add-hook 'comint-output-filter-functions
              'py-comint-output-filter-function)
    (setq comint-input-sender 'py-shell-simple-send)
    (setq comint-input-ring-file-name
          (if (string-equal py-shell-name "ipython")
              (if (getenv "IPYTHONDIR")
                  (concat (getenv "IPYTHONDIR") "/history") "~/.ipython/history")
            (if (getenv "PYTHONHISTORY")
                (concat (getenv "PYTHONHISTORY") "/" py-shell-name "_history")
              (concat "~/." py-shell-name "_history"))))
    ;; (message "comint-input-ring-file-name: %s" comint-input-ring-file-name)
    (comint-read-input-ring t)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          #'shell-write-history-on-exit)
    ;; pdbtrack
    (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
    (setq py-pdbtrack-do-tracking-p t)
    ;;
    (set-syntax-table py-mode-syntax-table)
    (ansi-color-for-comint-mode-on)
    (use-local-map py-shell-map)
    ;; ToDo: has only effect \w IPython
    (add-hook 'py-shell-hook 'py-dirstack-hook)
    (run-hooks 'py-shell-hook)
    (when (interactive-p) (message "%s" py-process-name))
    py-process-name))

(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python
  :version "23.3")


;; Code execution commands
(defvar py-execute-directory nil
  "Stores the file's directory-name py-execute-... functions act upon. ")

(defun py-which-execute-file-command (filename)
  "Return the command appropriate to Python version.
Per default it's \"(format \"execfile(r'%s') # PYTHON-MODE\\n\" filename)\" for Python 2 series."
  (interactive)
  (let* ((erg (py-which-python))
         (cmd (if (< erg 3)
                  (format "execfile(r'%s') # PYTHON-MODE\n" filename)
                (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename))))
    (when (interactive-p) (message "%s" (prin1-to-string cmd)))
    cmd))

(defun py-execute-region-no-switch (start end &optional async)
  "Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', buffer with region stays current.
 "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute nil))
    (py-execute-base start end async)))

(defun py-execute-region-switch (start end &optional async)
  "Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to.
"
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
  (py-execute-base start end async)))

(defun py-execute-region (start end &optional async)
  "Send the region to a common shell calling a Python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async))

(defun py-execute-region-dedicated (start end &optional async shell)
  "Get the region processed by an unique Python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async shell t))

(defun py-execute-base (start end &optional async shell dedicated)
  "Adapt the variables used in the process. "
  (let* ((regbuf (current-buffer))
         (strg (buffer-substring-no-properties start end))
	 (name-raw (or shell (py-choose-shell)))
         (name (py-process-name name-raw))
         (temp (make-temp-name name))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (filebuf (get-buffer-create file))
         (proc (if dedicated
                   (get-process (py-shell nil dedicated))
                 (get-process (py-shell))))
         (procbuf (if dedicated
                      (buffer-name (get-buffer (current-buffer)))
                    (buffer-name (get-buffer (concat "*" name "*"))))))
    ;; py-shell might kill temp-buffer, bug?
    ;; (set-buffer regbuf)
    (py-execute-intern start end strg procbuf proc temp file filebuf name)))

(defun py-execute-intern (start end &optional strg procbuf proc temp file filebuf name)
  (let ((py-execute-directory (or py-execute-directory (ignore-errors (file-name-directory (buffer-file-name))) (getenv "HOME")))
        (pec (if (string-match "Python3" name)
                 (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file)
               (format "execfile(r'%s') # PYTHON-MODE\n" file)))
        shell)
    (set-buffer filebuf)
    (erase-buffer)
    ;; (insert-buffer-substring regbuf start end)
    (insert strg)
    (setq start (py-fix-start (point-min)(point-max)))
    (py-insert-coding)
    (py-if-needed-insert-shell name)
    (cond
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
        (set-buffer filebuf)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((tempbuf (generate-new-buffer-name py-output-buffer))
             (arg (if (string-match name "Python")
                      "-u" "")))
        (start-process name tempbuf shell arg file)
        (pop-to-buffer tempbuf)
        (py-postprocess-output-buffer tempbuf)
        ;; TBD: clean up the temporary file!
        ))
     (proc
      ;; use the existing python shell
      (set-buffer filebuf)
      (write-region (point-min) (point-max) file nil t nil 'ask)
      (set-buffer-modified-p 'nil)
      (kill-buffer filebuf)
      (sit-for 0.1)
      (if (file-readable-p file)
          (progn
            (py-execute-file proc file pec)
            (setq py-exception-buffer (cons file (current-buffer)))
            (if py-shell-switch-buffers-on-execute
                (progn
                  (pop-to-buffer procbuf)
                  (goto-char (point-max)))
              (when (buffer-live-p regbuf) (pop-to-buffer regbuf))
              (message "Output buffer: %s" procbuf))
            (sit-for 0.1)
            (unless py-execute-keep-temporary-file-p
              (delete-file file)
              (when (buffer-live-p file)
                (kill-buffer file))))
        (message "File not readable: %s" "Do you have write permissions?"))))))

(defun py-execute-string (string &optional async)
  "Send the argument STRING to a Python interpreter.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Python command: ")
  (with-temp-buffer
    (insert string)
    (py-execute-region (point-min) (point-max) async)))

(defun py-shell-command-on-region (start end)
  "Execute region in a shell.
Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\xA9' "
  (interactive "r")
  (let* ((regbuf (current-buffer))
         (shell (or (py-choose-shell-by-shebang)
                    (py-choose-shell-by-import)
                    py-shell-name))
         (cmd (if (string-equal shell
                                "Jython")
                  "jython -" "python")))
    (with-temp-buffer
      (insert-buffer-substring regbuf start end)
      (shell-command-on-region (point-min) (point-max)
                               cmd py-output-buffer)
      ;; shell-command-on-region kills the output buffer if it never
      ;; existed and there's no output from the command
      (if (not (get-buffer py-output-buffer))
          (message "No output.")
        (setq py-exception-buffer py-output-buffer)
        (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
          (when py-shell-switch-buffers-on-execute
            (pop-to-buffer py-output-buffer))
          (if err-p
              (pop-to-buffer py-exception-buffer)))))))

(defun py-ipython-shell-command-on-region (start end)
  "Execute region in a shell.
Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\xA9' "
  (interactive "r")
  (let* ((regbuf (current-buffer))
         (shell "ipython")
         (cmd "ipython")
         (prompt_in1 ""))
    (with-temp-buffer
      (insert-buffer-substring regbuf start end)
      (shell-command-on-region (point-min) (point-max)
                               cmd py-output-buffer)
      ;; shell-command-on-region kills the output buffer if it never
      ;; existed and there's no output from the command
      (if (not (get-buffer py-output-buffer))
          (message "No output.")
        (setq py-exception-buffer py-output-buffer)
        (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
          (when py-shell-switch-buffers-on-execute
            (pop-to-buffer py-output-buffer))
          (if err-p
              (pop-to-buffer py-exception-buffer)))))))

(defun py-send-region-ipython (start end)
  "Execute the region through an ipython shell. "
  (interactive "r")
  ;; Skip ahead to the first non-blank line
  (let* ((name (concat "*" "IPython" "*"))
         (py-shell-name "ipython")
         (regbuf (current-buffer))
         (first (progn (and (buffer-live-p (get-buffer name))
                            (processp (get-process name))
                            (buffer-name (get-buffer name)))))
         (procbuf (or first (progn
                              (py-shell)
                              (buffer-name (get-buffer name)))))
         (cmd "#-*- coding: utf-8 -*-\n")
         ;; (lines (count-lines start end))
         shell)
    (setq cmd (concat cmd (buffer-substring-no-properties start end)))
    ;; Set the shell either to the #! line command, or to the
    ;; py-shell-name buffer local variable.
    (setq shell (or (py-choose-shell-by-shebang)
                    (py-choose-shell-by-import)
                    py-shell-name))
    (set-buffer procbuf)
    (goto-char (point-max))
    (switch-to-buffer procbuf)
    (insert cmd)
    (comint-send-input)
;;    (ipython-send-and-indent)
    (when (< 1 lines)
;;      (goto-char (point-max))
      (comint-send-input))
    ))

(defun py-execute-region-in-shell (start end &optional async)
  "Execute the region in a Python shell. "
  (interactive "r\nP")
  (let* ((regbuf (current-buffer))
         (name (concat "*" py-shell-name "*"))
         (first (progn (and (buffer-live-p (get-buffer name))
                            (processp (get-process py-shell-name))
                            (buffer-name (get-buffer name)))))
         (procbuf (or first (progn
                              (py-shell)
                              (buffer-name (get-buffer name)))))
         (proc (get-process py-shell-name))
         (temp (make-temp-name py-shell-name))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (temp (get-buffer-create file))
         (py-line-number-offset 0)
         shell cmd)
    ;; Write the contents of the buffer, watching out for indented regions.
    (save-excursion
      (set-buffer regbuf)
      (goto-char start)
      (beginning-of-line)
      (while (and (looking-at "\\s *$")
                  (< (point) end))
        (forward-line 1))
      (setq start (point))
      (or (< start end)
          (error "Region is empty"))
      (setq py-line-number-offset (count-lines 1 start))
      (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
        (setq cmd "#-*- coding: utf-8 -*-\n")
        (when needs-if
          (setq cmd (concat cmd "if 1:\n"))
          (setq py-line-number-offset (- py-line-number-offset 1)))
        (setq cmd (concat cmd (buffer-substring-no-properties start end)))
        ;; Set the shell either to the #! line command, or to the
        ;; py-shell-name buffer local variable.
        (setq shell (or (py-choose-shell-by-shebang)
                        (py-choose-shell-by-import)
                        py-shell-name))))
    (cond
     ;; always run the code in its own asynchronous subprocess
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
        (set-buffer temp)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((temp (generate-new-buffer-name py-output-buffer))
             ;; TBD: a horrible hack, but why create new Custom variables?
             (arg (if (string-match py-shell-name "Python")
                      "-u" "")))
        (start-process py-shell-name temp shell arg file)
        (pop-to-buffer temp)
        (py-postprocess-output-buffer temp)
        ;; TBD: clean up the temporary file!
))
     ;; if the Python interpreter shell is running, queue it up for
     ;; execution there.
     (proc
      ;; use the existing python shell
      (set-buffer procbuf)
      (goto-char (point-max))
      (insert cmd)
      (switch-to-buffer (current-buffer))
      (if (functionp 'ipython-send-and-indent)
          (ipython-send-and-indent)
        (comint-send-input))
      (setq py-exception-buffer (cons file (current-buffer)))
      (switch-to-buffer procbuf))
     (t
      ;; this part is in py-shell-command-on-region now.
      (let ((cmd
             (concat shell (if (string-equal py-shell-name
                                             "Jython")
                               " -" ""))))
        ;; otherwise either run it synchronously in a subprocess
        (save-excursion
          (set-buffer temp)
          (shell-command-on-region (point-min) (point-max)
                                   cmd py-output-buffer))
        ;; shell-command-on-region kills the output buffer if it never
        ;; existed and there's no output from the command
        (if (not (get-buffer py-output-buffer))
            (message "No output.")
          (setq py-exception-buffer (current-buffer))
          (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
            (pop-to-buffer py-output-buffer)
            (if err-p
                (pop-to-buffer py-exception-buffer)))))))
    ;; Clean up after ourselves.
    (kill-buffer temp)))

(defun py-if-needed-insert-shell (&optional name)
  (let ((erg (if name (downcase name)
               (or (py-choose-shell-by-shebang)
                   (py-choose-shell-by-import)
                   py-shell-name))))
    (goto-char (point-min))
    (if (string-match (concat "^" erg) "ipython")
        (progn
          (shell-command "type ipython" t)
          (switch-to-buffer (current-buffer))
          (when (looking-at "[^/\n\r]+")
            (replace-match "#! ")))
      (insert (concat py-shebang-startstring " " erg "\n")))
    (end-of-line)
    (newline)
    (insert (concat "import os; os.chdir(\"" (or py-execute-directory (file-name-directory (buffer-file-name)) (getenv "HOME")) "\")"))))

(defun py-insert-coding ()
  (goto-char (point-min))
  (unless (re-search-forward (concat "^" (regexp-quote py-encoding-string)) nil t 1)
    (if (re-search-forward py-shebang-regexp nil t 1)
        (progn
          (newline)
          (insert (concat py-encoding-string "\n")))
      (insert (concat py-encoding-string "\n")))))

(defun py-if-needed-insert-if ()
  "Internal use by py-execute... functions.
Inserts an incentive true form \"if 1:\\n.\" "
  (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
    (when needs-if
      (insert "if 1:\n")
      (setq py-line-number-offset (- py-line-number-offset 1)))))

(defun py-fix-start (start end)
  "Internal use by py-execute... functions.
Avoid empty lines at the beginning. "
  (goto-char start)
  (let ((beg (copy-marker start)))
    (while (empty-line-p)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (back-to-indentation)
    (unless (eq (current-indentation) 0)
      (py-shift-left (current-indentation) start end))
    (setq py-line-number-offset (count-lines 1 start))
    beg))

(defun py-fetch-py-master-file ()
  "Lookup if a `py-master-file' is specified.
See also doku of variable `py-master-file' "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  (when (interactive-p) (message "%s" py-master-file)))

(defun py-execute-import-or-reload (&optional async)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-string
           (if (string-match "\\.py$" file)
               (let ((m (py-qualified-module-name (expand-file-name file))))
		 (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n"
                         m m m))
             ;; (format "execfile(r'%s')\n" file)
             (py-which-execute-file-command file))
           async))
      ;; else
      (py-execute-buffer async))))

(defun py-qualified-module-name (file)
  "Find the qualified module name for filename FILE.

Basically, this goes down the directory tree as long as there are __init__.py files there."
  (let ((rec #'(lambda (d f)
		 (let* ((dir (file-name-directory d))
			(initpy (concat dir "__init__.py")))
		   (if (file-exists-p initpy)
		       (let ((d2 (directory-file-name d)))
			 (funcall rec (file-name-directory d2)
				(concat (file-name-nondirectory d2) "." f)))
		     f)))))
    (funcall rec (file-name-directory file)
	     (file-name-sans-extension (file-name-nondirectory file)))))

(defun py-execute-buffer (&optional async)
  "Send the contents of the buffer to a Python interpreter.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Python* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region (point-min) (point-max) async))))

(defun py-execute-buffer-no-switch (&optional async)
  "Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute'.

Buffer called from is current afterwards again."
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region-no-switch (point-min) (point-max) async))))

(defun py-execute-buffer-switch (&optional async)
  "Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region-switch (point-min) (point-max) async))))

;; Specifying shells start

(defun py-execute-region-python (start end &optional async)
  "Send the region to a common shell calling the python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python"))

(defun py-execute-region-python-switch (start end &optional async)
  "Send the region to a common shell calling the python interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python")))

(defun py-execute-region-python-no-switch (start end &optional async)
  "Send the region to a common shell calling the python interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python")))

(defun py-execute-region-python2 (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python2"))

(defun py-execute-region-python2-switch (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python2")))

(defun py-execute-region-python2-no-switch (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python2")))

(defun py-execute-region-python2.7 (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python2.7"))

(defun py-execute-region-python2.7-switch (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python2.7")))

(defun py-execute-region-python2.7-no-switch (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python2.7")))

(defun py-execute-region-python3 (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python3"))

(defun py-execute-region-python3-switch (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python3")))

(defun py-execute-region-python3-no-switch (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python3")))

(defun py-execute-region-python3.2 (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python3.2"))

(defun py-execute-region-python3.2-switch (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python3.2")))

(defun py-execute-region-python3.2-no-switch (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python3.2")))

(defun py-execute-region-ipython (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "ipython"))

(defun py-execute-region-ipython-switch (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "ipython")))

(defun py-execute-region-ipython-no-switch (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "ipython")))

(defun py-execute-region-jython (start end &optional async)
  "Send the region to a common shell calling the jython interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "jython"))

(defun py-execute-region-jython-switch (start end &optional async)
  "Send the region to a common shell calling the jython interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "jython")))

(defun py-execute-region-jython-no-switch (start end &optional async)
  "Send the region to a common shell calling the jython interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "jython")))

;; Specifying shells end

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-execute-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python filename\",
Optional OUTPUT-BUFFER and ERROR-BUFFER might be given.')
"
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (if output-buffer
        (progn
          (set-buffer (get-buffer-create output-buffer))
          (erase-buffer)
          (shell-command (concat "python " filename) output-buffer error-buffer))
      (with-temp-buffer
        (shell-command (concat "python " filename) output-buffer error-buffer)))))

(defun py-exec-execfile-region (start end &optional async)
  "Execute the region in a Python interpreter. "
  (interactive "r\nP")
  (let ((strg (buffer-substring-no-properties start end)))
    (py-exec-execfile-base strg async (interactive-p))))

(defun py-exec-execfile-base (strg async iact)
  (let* ((temp (make-temp-name (concat (buffer-name) "-")))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (imports (py-find-imports))
         shell cmd header)
    (with-temp-buffer
      (insert imports)
      (insert strg)
;;      (py-if-needed-insert-if)
      (setq shell (py-choose-shell))
      (py-insert-coding)
      (py-if-needed-insert-shell shell)
      (setq header (buffer-substring-no-properties (point-min) (point)))
      (switch-to-buffer (current-buffer))
      (setq cmd (py-which-execute-file-command file))
      (write-file file))
    (py-exec-execfile file cmd header (concat temp "-output"))
    (set-buffer (concat temp "-output"))
    (when iact (switch-to-buffer (current-buffer)))
    (when (file-readable-p file)
      (delete-file file))
    (when iact (message "Output goes to buffer: %s" temp))
    (concat temp "-output")))

(defun py-exec-execfile (filename cmd header &optional output-buffer error-buffer)
  "Process \"python filename\",
Optional OUTPUT-BUFFER and ERROR-BUFFER might be given.')
"
  (interactive "fDatei:")
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (exec-execfile (concat (make-temp-name (concat filename "-exec-execfile.py")))))
    (set-buffer (get-buffer-create exec-execfile))
    (insert header)
    (insert cmd)
    (write-file exec-execfile)
    (if output-buffer
        (progn
          (set-buffer (get-buffer-create output-buffer))
          (erase-buffer)
          (switch-to-buffer (current-buffer))
          (shell-command (concat "python " exec-execfile) output-buffer error-buffer))
      (with-temp-buffer
        (shell-command (concat "python " exec-execfile) output-buffer error-buffer)))))

;; Execute forms at point
(defun py-execute-block ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-p)
                       (py-beginning-of-block))
                 (push-mark)))
          (end (py-end-of-block)))
      (py-execute-region beg end))))

(defun py-execute-block-or-clause ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-or-clause-p)
                       (py-beginning-of-block-or-clause))
                 (push-mark)))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end))))

(defun py-execute-class ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-class-p)
                       (py-beginning-of-class))
                 (push-mark)))
          (end (py-end-of-class)))
      (py-execute-region beg end))))

(defun py-execute-clause ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-clause-p)
                       (py-beginning-of-clause))
                 (push-mark)))
          (end (py-end-of-clause)))
      (py-execute-region beg end))))

(defun py-execute-def ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-p)
                       (py-beginning-of-def))
                 (push-mark)))
          (end (py-end-of-def)))
      (py-execute-region beg end))))

(defun py-execute-def-or-class ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-or-class-p)
                       (py-beginning-of-def-or-class))
                 (push-mark)))
          (end (py-end-of-def-or-class)))
      (py-execute-region beg end))))

(defun py-execute-expression ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-expression-p)
                       (py-beginning-of-expression))
                 (push-mark)))
          (end (py-end-of-expression)))
      (py-execute-region beg end))))

(defun py-execute-partial-expression ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-partial-expression-p)
                       (py-beginning-of-partial-expression))
                 (push-mark)))
          (end (py-end-of-partial-expression)))
      (py-execute-region beg end))))

(defun py-execute-statement ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-statement-p)
                       (py-beginning-of-statement))
                 (push-mark)))
          (end (py-end-of-statement)))
      (py-execute-region beg end))))

;; Python subprocess utilities and filters
(defun py-execute-file (proc filename &optional cmd)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
        (procbuf (process-buffer proc))
                                        ;       (comint-scroll-to-bottom-on-output t)
        (msg (format "## executing temporary file %s...\n" filename))
        (cmd (cond (cmd)
                   (py-exec-command)
                   (t (py-which-execute-file-command filename)))))
    ;;    (unwind-protect
    ;;        (save-excursion
    ;;          (set-buffer procbuf)
    ;;          (goto-char (point-max))
    ;;          (move-marker (process-mark proc) (point))
    ;;          (funcall (process-filter proc) proc msg))
    ;;      (set-buffer curbuf)
    (set-buffer procbuf)
    (process-send-string proc cmd)))

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward py-traceback-line-re nil t)
        (setq file (match-string 1)
              line (string-to-number (match-string 2))
              bol (py-point 'bol))
        (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                     'face 'highlight)))
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line py-line-number-offset)
      (setq err-p t))
    err-p))

(defun py-jump-to-exception (file line py-line-number-offset)
  "Jump to the Python code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
                       (if (consp py-exception-buffer)
                           (cdr py-exception-buffer)
                         py-exception-buffer))
                      ((and (consp py-exception-buffer)
                            (string-equal file (car py-exception-buffer)))
                       (cdr py-exception-buffer))
                      ((ignore-errors (find-file-noselect file)))
                      ;; could not figure out what file the exception
                      ;; is pointing to, so prompt for it
                      (t (find-file (read-file-name "Exception file: "
                                                    nil
                                                    file t))))))
    ;; Fiddle about with line number
    (setq line (+ py-line-number-offset line))

    (pop-to-buffer buffer)
    ;; Force Python mode
    (unless(eq major-mode 'python-mode)
        (python-mode))
    (goto-char (point-min))
    (forward-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
        (py-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if top
        (py-find-next-exception 'bob buffer 're-search-forward "Top")
      (py-find-next-exception 'bol buffer 're-search-backward "Top"))))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (py-jump-to-exception file line py-line-number-offset)
      (error "%s of traceback" errwhere))))

(provide 'python-mode-execute)
;;; python-mode-execute.el ends here
