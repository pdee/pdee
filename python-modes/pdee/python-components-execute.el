;;; python-components-execute.el --- Part of python-components-mode

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>

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

;;; Split-Windows-On-Execute forms
(defalias 'toggle-py-split-windows-on-execute 'py-toggle-split-windows-on-execute)
(defun py-toggle-split-windows-on-execute (&optional arg)
  "If `py-split-windows-on-execute-p' should be on or off.

  Returns value of `py-split-windows-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-split-windows-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-split-windows-on-execute-p t)
      (setq py-split-windows-on-execute-p nil))
    (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
    py-split-windows-on-execute-p))

(defun py-split-windows-on-execute-on (&optional arg)
  "Make sure, `py-split-windows-on-execute-p' is on.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-split-windows-on-execute arg))
  (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

(defun py-split-windows-on-execute-off ()
  "Make sure, `py-split-windows-on-execute-p' is off.

Returns value of `py-split-windows-on-execute-p'. "
  (interactive)
  (toggle-py-split-windows-on-execute -1)
  (when (interactive-p) (message "py-split-windows-on-execute-p: %s" py-split-windows-on-execute-p))
  py-split-windows-on-execute-p)

;;; Shell-Switch-Buffers-On-Execute forms
(defalias 'toggle-py-shell-switch-buffers-on-execute 'py-toggle-shell-switch-buffers-on-execute)
(defun py-toggle-shell-switch-buffers-on-execute (&optional arg)
  "If `py-shell-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-shell-switch-buffers-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-shell-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-shell-switch-buffers-on-execute-p t)
      (setq py-shell-switch-buffers-on-execute-p nil))
    (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-shell-switch-buffers-on-execute-p))
    py-shell-switch-buffers-on-execute-p))

(defun py-shell-switch-buffers-on-execute-on (&optional arg)
  "Make sure, `py-shell-switch-buffers-on-execute-p' is on.

Returns value of `py-shell-switch-buffers-on-execute-p'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-shell-switch-buffers-on-execute arg))
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-shell-switch-buffers-on-execute-p))
  py-shell-switch-buffers-on-execute-p)

(defun py-shell-switch-buffers-on-execute-off ()
  "Make sure, `py-shell-switch-buffers-on-execute-p' is off.

Returns value of `py-shell-switch-buffers-on-execute-p'. "
  (interactive)
  (toggle-py-shell-switch-buffers-on-execute -1)
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-shell-switch-buffers-on-execute-p))
  py-shell-switch-buffers-on-execute-p)

;;;
(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
             py-file-queue)
    (if py-shell-switch-buffers-on-execute-p
        (pop-to-buffer (current-buffer)))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (py-execute-file-base pyproc (car py-file-queue))))))

(defun py-guess-default-python ()
  "Defaults to \"python\", if guessing didn't succeed. "
  (interactive)
  (let* ((cmd (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p cmd (executable-find cmd))))
    (when (interactive-p)
      (if erg
          (message "%s" cmd)
        (message "%s" "Could not detect Python on your system")))))

(defun py-separator-char ()
  "Return the file-path separator char from current machine.
Returns char found. "
  (interactive)
  (let ((erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))))
    (when (interactive-p) (message "Separator-char: %s" erg))
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

(defun py-set-shell-completion-environment (&optional pyshellname)
  "Sets `...-completion-command-string' and `py-complete-function'. "
  (interactive)
  (let ((pyshellname (or pyshellname py-shell-name)))
    (local-unset-key [tab])
    (cond ((string-match "ipython" pyshellname)
           (setq ipython-version (string-to-number (substring (shell-command-to-string (concat py-shell-name " -V")) 2 -1)))
           (setq ipython-completion-command-string (if (< ipython-version 11) ipython0.10-completion-command-string ipython0.11-completion-command-string))
           (define-key py-shell-map [tab] ipython-complete-function))
          ((string-match "python3" pyshellname)
           (define-key py-shell-map [tab] 'py-completion-at-point))
          (t (define-key py-shell-map [tab] 'py-shell-complete)))))

(defun py-set-ipython-completion-command-string (&optional pyshellname)
  "Set and return `ipython-completion-command-string'. "
  (interactive)
  (let* ((pyshellname (or pyshellname py-shell-name))
         (ipython-version
          (when (string-match "ipython" pyshellname)
            (string-to-number (substring (shell-command-to-string (concat pyshellname " -V")) 2 -1)))))
    (when ipython-version
      (setq ipython-completion-command-string (if (< ipython-version 11) ipython0.10-completion-command-string ipython0.11-completion-command-string))
      ipython-completion-command-string)))

(defun py-process-name (&optional name dedicated nostars sepchar)
  "Return the name of the running Python process, `get-process' willsee it. "
  (let* ((sepchar (or sepchar (py-separator-char)))
         (thisname (if name
                       (if (string-match sepchar name)
                           (substring name (progn (string-match (concat "\\(.+\\)" sepchar "\\(.+\\)$") name) (match-beginning 2)))

                         name)
                     (substring py-shell-name (or (string-match (concat sepchar ".+$") py-shell-name) 0))))
         (name (cond (dedicated
                      (make-temp-name (concat thisname "-")))
                     ;; ((string-match "\*" (buffer-name))
                     ;; (replace-regexp-in-string "\*" "" (buffer-name)))
                     (t thisname)))
         (erg (cond ((or (string-match "ipython" name)
                         (string-match "IPython" name))
                     "IPython")
                    (name)
                    )))
    (unless (or nostars (string-match "^\*" erg))(setq erg (concat "*" erg "*")))
    erg))

(defun py-buffer-name-prepare (name &optional sepchar)
  "Return an appropriate name to display in modeline.
SEPCHAR is the file-path separator of your system. "
  (let ((sepchar (or sepchar (py-separator-char)))
        prefix erg)
    (when (string-match sepchar name)
      (setq prefix "ND")
      (setq name (py-python-version name t)))
    (setq erg
          (cond ((string-match "ipython" name)
                 (replace-regexp-in-string "ipython" "IPython" name))
                ((string-match "jython" name)
                 (replace-regexp-in-string "jython" "Jython" name))
                ((string-match "python" name)
                 (replace-regexp-in-string "python" "Python" name))
                (t name)))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))

          (t (setq erg (concat "*" erg "*"))))
    erg))

(defun py-delete-numbers-and-stars-from-string (string)
  "Delete numbering and star chars from string, return result.

Needed when file-path names are contructed from maybe numbered buffer names like \"\*Python\*<2> \""
  (replace-regexp-in-string
   "<\\([0-9]+\\)>" ""
   (replace-regexp-in-string
    "\*" ""
    string)))

(defun py-shell (&optional argprompt dedicated pyshellname switch sepchar)
  "Start an interactive Python interpreter in another window.

Interactively, \\[universal-argument] 4 prompts for a buffer.
\\[universal-argument] 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

Returns variable `py-process-name' used by function `get-process'.
Optional string PYSHELLNAME overrides default `py-shell-name'.
Optional symbol SWITCH ('switch/'noswitch) precedes `py-shell-switch-buffers-on-execute-p'
"
  (interactive "P")
  (let ((sepchar (or sepchar (py-separator-char)))
        (args py-python-command-args)
        (oldbuf (current-buffer))
        proc)
    (let* ((buffer
            (when argprompt
              (cond
               ((eq 4 (prefix-numeric-value argprompt))
                (setq buffer
                      (prog1
                          (read-buffer "Py-Shell buffer: "
                                       (generate-new-buffer-name (py-buffer-name-prepare (or pyshellname py-shell-name) sepchar)))
                        (if (file-remote-p default-directory)
                            ;; It must be possible to declare a local default-directory.
                            (setq default-directory
                                  (expand-file-name
                                   (read-file-name
                                    "Default directory: " default-directory default-directory
                                    t nil 'file-directory-p)))))))
               ((and (eq 2 (prefix-numeric-value argprompt))
                     (fboundp 'split-string))
                (setq args (split-string
                            (read-string "Py-Shell arguments: "
                                         (concat
                                          (mapconcat 'identity py-python-command-args " ") " "))))))))
           (py-process-name
            (cond (buffer
                   (if
                       (get-process
                        (replace-regexp-in-string
                         "<\\([0-9]+\\)>" ""
                         buffer))
                       (py-delete-numbers-and-stars-from-string buffer)))
                  (pyshellname pyshellname)
                  ((stringp py-shell-name)
                   py-shell-name)
                  ((or (string= "" py-shell-name)(null py-shell-name))
                   (py-choose-shell))))
           ;; already in py-choose-shell
           (py-use-local-default
            (if (not (string= "" py-shell-local-path))
                (expand-file-name py-shell-local-path)
              (when py-use-local-default
                (error "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'"))))
           (py-buffer-name-prepare (unless buffer
                                     (py-buffer-name-prepare py-process-name sepchar)))
           (py-buffer-name (or buffer py-buffer-name-prepare))
           (executable (cond (buffer
                              (downcase (replace-regexp-in-string
                                         "<\\([0-9]+\\)>" ""
                                         (replace-regexp-in-string
                                          "\*" ""
                                          py-buffer-name buffer))))
                             (pyshellname pyshellname)
                             (t py-shell-name))))
      (py-set-shell-completion-environment executable)
      ;; comint
      (if buffer
          (set-buffer (get-buffer-create
                       (apply 'make-comint-in-buffer executable py-buffer-name executable nil args)))
        (set-buffer (apply 'make-comint-in-buffer executable py-buffer-name executable nil args)))
      (set (make-local-variable 'comint-prompt-regexp)
           (concat "\\("
                   (mapconcat 'identity
                              (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                              "\\|")
                   "\\)"))
      (add-hook 'comint-output-filter-functions
                'py-comint-output-filter-function)
      (setq comint-input-sender 'py-shell-simple-send)
      (setq comint-input-ring-file-name
            (if (or (string-match "ipython" py-buffer-name)
                    (string-match "IPython" py-buffer-name))
                (if (getenv "IPYTHONDIR")
                    (concat (getenv "IPYTHONDIR") "/history") "~/.ipython/history")
              (if (getenv "PYTHONHISTORY")
                  (concat (getenv "PYTHONHISTORY") "/" py-buffer-name "_history")
                (concat "~/." py-buffer-name "_history"))))
      (comint-read-input-ring t)
      (set-process-sentinel (get-buffer-process (current-buffer))
                            #'shell-write-history-on-exit)
      (setq proc (get-buffer-process (current-buffer)))
      ;; pdbtrack
      (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
      (setq py-pdbtrack-do-tracking-p t)
      ;;
      (set-syntax-table python-mode-syntax-table)
      (ansi-color-for-comint-mode-on)
      (use-local-map py-shell-map)
      (add-hook 'py-shell-hook 'py-dirstack-hook)
      (run-hooks 'py-shell-hook)
      (cond ((or (eq switch 'switch)
                 (and (not (eq switch 'noswitch))
                      py-shell-switch-buffers-on-execute-p))
             (switch-to-buffer (current-buffer)))
            ((and py-split-windows-on-execute-p
                  (or (eq switch 'noswitch)
                      (not (eq switch 'switch))))
             (pop-to-buffer (current-buffer))
             (delete-other-windows)
             (when (and py-split-windows-on-execute-p
                        (or (and (eq py-split-windows-on-execute-function 'split-window-vertically) (window-full-height-p))
                            (and (eq py-split-windows-on-execute-function 'split-window-horizontally) (window-full-width-p))))
               (funcall py-split-windows-on-execute-function))
             ;; (message (buffer-name (current-buffer)))
             (set-buffer oldbuf)
             (switch-to-buffer (current-buffer))))
      (goto-char (point-max))
      (when (and py-verbose-p (interactive-p)) (message py-buffer-name))
      proc)))

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

(defun py-execute-region-no-switch (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', buffer with region stays current.
 "
  (interactive "r\nP")
  (py-execute-base start end py-shell-name dedicated 'noswitch))

;;; execute region
(defun py-execute-region (start end &optional shell dedicated switch)
  "Send the region to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)
"
  (interactive "r\nP")
  (let ((shell (cond ((or py-force-py-shell-name-p (eq 4 (prefix-numeric-value shell))) (default-value 'py-shell-name))
                     ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                      (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                     (t shell))))
    (py-execute-base start end shell dedicated switch)))

(defun py-execute-region-default (start end &optional dedicated)
  "Send the region to the systems default Python interpreter.
See also `py-execute-region'. "
  (interactive "r\nP")
  (py-execute-base start end (default-value 'py-shell-name) dedicated))

(defun py-execute-region-dedicated (start end &optional shell)
  "Get the region processed by an unique Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument. "
  (interactive "r\nP")
  (let ((shell (cond ((eq 4 (prefix-numeric-value shell)) (default-value 'py-shell-name))
                     ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                      (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                     (t shell))))
    (py-execute-base start end shell t)))

(defun py-execute-region-switch (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to.
"
  (interactive "r\nP")
  (py-execute-base start end py-shell-name dedicated 'switch))

(defalias 'py-execute-region-dedicated-default 'py-execute-region-default-dedicated)
(defun py-execute-region-default-dedicated (start end)
  "Send the region to an unique shell of systems default Python. "
  (interactive "r")
  (py-execute-base start end (default-value 'py-shell-name) t))

(defun py-execute-base (start end &optional shell dedicated switch nostars sepchar)
  "Adapt the variables used in the process. "
  (let* ((shell (or shell (py-choose-shell)))
         (regbuf (current-buffer))
         (py-execute-directory (or (ignore-errors (file-name-directory (buffer-file-name)))(getenv "WORKON_HOME")(getenv "HOME")))
         (strg (buffer-substring-no-properties start end))
         (sepchar (or sepchar (py-separator-char)))
         ;; (name-raw (or shell (py-choose-shell)))
         (name (py-buffer-name-prepare shell sepchar))
         (temp (make-temp-name shell))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (filebuf (get-buffer-create file))
         (process-connection-type t)
         (proc (or (get-process shell)
                   (get-process (process-name (py-shell nil dedicated (or shell (downcase shell)) switch sepchar)))))
         (procbuf (if dedicated
                      (buffer-name (get-buffer (current-buffer)))
                    (buffer-name (get-buffer (py-process-name name dedicated nostars sepchar)))))
         (pec (if (string-match "Python *3" name)
                  (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file)
                (format "execfile(r'%s') # PYTHON-MODE\n" file)))
         (wholebuf (when (boundp 'wholebuf) wholebuf))
         (comint-scroll-to-bottom-on-output t)
         erg)
    (set-buffer filebuf)
    (erase-buffer)
    (insert strg)
    (unless wholebuf
      (py-fix-start (point-min)(point-max))
      (py-if-needed-insert-shell name)
      (py-insert-coding)
      (py-insert-execute-directory))
    (set-buffer filebuf)
    (write-region (point-min) (point-max) file nil t nil 'ask)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf)
    (if (file-readable-p file)
        (progn
          (when (string-match "ipython" (process-name proc))
            (sit-for py-ipython-execute-delay))
          (setq erg (py-execute-file-base proc file pec))
          (setq py-exception-buffer (cons file (current-buffer)))
          ;; (set-buffer regbuf)
          (cond ((eq switch 'switch)
                 (if py-split-windows-on-execute-p
                     (progn
                       (delete-other-windows)
                       (funcall py-split-windows-on-execute-function))
                   (set-buffer regbuf))
                 (set-buffer procbuf)
                 (switch-to-buffer (current-buffer))
                 (goto-char (point-max)))
                ((eq switch 'noswitch)
                 (when py-split-windows-on-execute-p
                   (delete-other-windows)
                   (funcall py-split-windows-on-execute-function))
                 (set-buffer regbuf)
                 (switch-to-buffer (current-buffer)))
                ((and py-shell-switch-buffers-on-execute-p py-split-windows-on-execute-p)
                 ;; (delete-other-windows)
                 (funcall py-split-windows-on-execute-function)
                 (switch-to-buffer (current-buffer))
                 (switch-to-buffer regbuf)
                 (pop-to-buffer procbuf))
                (py-split-windows-on-execute-p
                 (delete-other-windows)
                 (pop-to-buffer procbuf)
                 (set-buffer procbuf)
                 (funcall py-split-windows-on-execute-function)
                 (switch-to-buffer regbuf))
                (py-shell-switch-buffers-on-execute-p
                 (set-buffer procbuf)
                 (switch-to-buffer (current-buffer))
                 ;; (delete-other-windows)
                 ))
          (unless (string= (buffer-name (current-buffer)) procbuf)
            (when py-verbose-p (message "Output buffer: %s" procbuf)))
          (sit-for 0.1)
          (unless py-execute-keep-temporary-file-p
            (delete-file file)
            (when (buffer-live-p file)
              (kill-buffer file)))
          erg)
      (message "%s not readable. %s" file "Do you have write permissions?"))))

(defun py-execute-string (&optional string shell dedicated)
  "Send the argument STRING to a Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell dedicated))))

(defun py-execute-string-dedicated (&optional string shell)
  "Send the argument STRING to an unique Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell t))))

(defun py-if-needed-insert-shell (&optional name)
  (let ((erg (if name (downcase name)
               (or (py-choose-shell-by-shebang)
                   (py-choose-shell-by-import)
                   py-shell-name))))
    (goto-char (point-min))
    (while (empty-line-p) (delete-region (point) (1+ (line-end-position))))
    (unless (looking-at py-shebang-regexp)
      (if (string-match (concat "^" erg) "ipython")
          (progn
            (shell-command "type ipython" t)
            (switch-to-buffer (current-buffer))
            (when (looking-at "[^/\n\r]+")
              (replace-match "#! ")))
        (insert (concat py-shebang-startstring " " erg "\n"))))
    (end-of-line)
    (newline)))

(defun py-insert-execute-directory ()
  (goto-char (point-min))
  (if (re-search-forward py-encoding-string-re nil (quote move))
      (progn
        (newline)
        (insert (concat "import os; os.chdir(\"" py-execute-directory "\")\n")))
    (goto-char (point-min))
    (forward-line 2)
    (newline)
    (insert (concat "import os; os.chdir(\"" py-execute-directory "\")\n"))))

(defun py-insert-coding ()
  ;; (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (unless (re-search-forward py-encoding-string-re nil t)
    (goto-char (point-min))
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

(defun py-execute-import-or-reload (&optional argprompt shell dedicated)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See also `\\[py-execute-region]'.

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
  (let ((shell (or shell (py-choose-shell argprompt shell dedicated)))
        (file (buffer-file-name (current-buffer))))
    (if file
        (let ((proc (or
                     (ignore-errors (get-process (file-name-directory shell)))
                     (get-process (py-shell argprompt dedicated (or shell (default-value 'py-shell-name)))))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-file-base proc file
                                (if (string-match "\\.py$" file)
                                    (let ((m (py-qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" (file-name-nondirectory shell))
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-which-execute-file-command file))))
      (py-execute-buffer py-shell-name))))

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

;;; execute buffer
(defun py-execute-buffer (&optional shell dedicated switch)
  "Send the contents of the buffer to a Python interpreter.

When called with \\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch) "
  (interactive "P")
  (save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end shell dedicated switch))))

(defun py-execute-buffer-base (&optional shell dedicated switch)
  "Honor `py-master-file'. "
  (save-excursion
    (let ((py-master-file (or py-master-file (py-fetch-py-master-file))))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region (point-min) (point-max) shell dedicated switch))))

(defun py-execute-buffer-dedicated (&optional shell)
  "Send the contents of the buffer to a unique Python interpreter.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[univeral-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell t))

(defun py-execute-buffer-switch (&optional shell dedicated)
  "Send the contents of the buffer to a Python interpreter and switches to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[univeral-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell dedicated 'switch))

(defun py-execute-buffer-no-switch (&optional shell dedicated)
  "Send the contents of the buffer to a Python interpreter but don't switch to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[univeral-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell dedicated 'noswitch))

(defalias 'py-execute-buffer-switch-dedicated 'py-execute-buffer-dedicated-switch)
(defun py-execute-buffer-dedicated-switch (&optional shell)
  "Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p'.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[univeral-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell t 'switch))

;;; Specifying shells start
(defun py-execute-region-python (start end)
  "Send the region to a common shell calling the python interpreter. "
  (interactive "r")
  (py-execute-base start end "python"))

(defun py-execute-region-python-switch (start end)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "python")))

(defun py-execute-region-python-no-switch (start end)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "python")))

(defun py-execute-region-python2 (start end)
  "Send the region to a common shell calling the python2 interpreter. "
  (interactive "r")
  (py-execute-base start end "python2"))

(defun py-execute-region-python2-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "python2")))

(defun py-execute-region-python2-no-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "python2")))

(defun py-execute-region-python2.7 (start end)
  "Send the region to a common shell calling the python2.7 interpreter. "
  (interactive "r")
  (py-execute-base start end "python2.7"))

(defun py-execute-region-python2.7-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "python2.7")))

(defun py-execute-region-python2.7-no-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "python2.7")))

(defun py-execute-region-python3 (start end)
  "Send the region to a common shell calling the python3 interpreter. "
  (interactive "r")
  (py-execute-base start end "python3"))

(defun py-execute-region-python3-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "python3")))

(defun py-execute-region-python3-no-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "python3")))

(defun py-execute-region-python3.2 (start end)
  "Send the region to a common shell calling the python3.2 interpreter. "
  (interactive "r")
  (py-execute-base start end "python3.2"))

(defun py-execute-region-python3.2-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "python3.2")))

(defun py-execute-region-python3.2-no-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "python3.2")))

(defun py-execute-region-ipython (start end)
  "Send the region to a common shell calling the ipython interpreter. "
  (interactive "r")
  (py-execute-base start end "ipython"))

(defun py-execute-region-ipython-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "ipython")))

(defun py-execute-region-ipython-no-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "ipython")))

(defun py-execute-region-jython (start end)
  "Send the region to a common shell calling the jython interpreter. "
  (interactive "r")
  (py-execute-base start end "jython"))

(defun py-execute-region-jython-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end "jython")))

(defun py-execute-region-jython-no-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end "jython")))

;; Specifying shells end

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-execute-region (progn (beginning-of-defun) (point))
                                     (progn (end-of-defun) (point)))))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python filename\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. "
  (interactive "fDatei:")
  (let ((coding-system-or-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output"))))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat "python " filename) output-buffer error-buffer)
    (when (interactive-p) (switch-to-buffer output-buffer))))

(defun py-exec-execfile-region (start end &optional shell)
  "Execute the region in a Python interpreter. "
  (interactive "r\nP")
  (let ((shell (if (eq 4 (prefix-numeric-value arg))
                   (read-from-minibuffer "Shell: " (default-value 'py-shell-name))
                 py-shell-name)))
    (let ((strg (buffer-substring-no-properties start end)))
      (py-exec-execfile-base strg shell (interactive-p)))))

(defun py-exec-execfile-base (strg shell iact)
  (let* ((temp (make-temp-name (concat (buffer-name) "-")))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (imports (py-find-imports))
         (shell shell)
         cmd header)
    (with-temp-buffer
      (insert imports)
      (insert strg)
      ;;      (py-if-needed-insert-if)
      (or shell (setq shell (py-choose-shell)))
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

;;;
(defun py-execute-line ()
  "Send current line from beginning of indent to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (progn (back-to-indentation)
                      (point))))
      (py-execute-region beg (line-end-position)))))

(defun py-execute-file (&optional filename shell dedicated switch)
  "When called interactively, user is prompted for filename. "
  (interactive "fFile: ")
  (let* ((regbuf (current-buffer))
         (file (or (expand-file-name filename) (when (ignore-errors (file-readable-p (buffer-file-name))) (buffer-file-name))))
         (shell (or shell (progn (with-temp-buffer (insert-file file)(py-choose-shell)))))
         (name (py-process-name shell dedicated nostars sepchar))
         (proc (get-process (py-shell nil dedicated (or shell (downcase name)))))
         (procbuf (if dedicated
                      (buffer-name (get-buffer (current-buffer)))
                    (buffer-name (get-buffer (concat "*" name "*")))))
         (pec (if (string-match "Python3" name)
                  (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file)
                (format "execfile(r'%s') # PYTHON-MODE\n" file)))
         (comint-scroll-to-bottom-on-output t))
    (if (file-readable-p file)
        (progn
          (setq erg (py-execute-file-base proc file pec))
          (setq py-exception-buffer (cons file (current-buffer)))
          (if (or (eq switch 'switch)
                  (and (not (eq switch 'noswitch)) py-shell-switch-buffers-on-execute-p))
              (progn
                (pop-to-buffer procbuf)
                (goto-char (point-max)))
            (when (buffer-live-p regbuf) (pop-to-buffer regbuf))
            (when py-verbose-p (message "Output buffer: %s" procbuf)))
          (sit-for 0.1)
          erg)
      (message "File not readable: %s" "Do you have write permissions?"))))

(defun py-execute-file-base (proc filename &optional cmd)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing.
Returns position where output starts. "
  (let ((curbuf (current-buffer))
        (procbuf (process-buffer proc))
        (comint-scroll-to-bottom-on-output t)
        (msg (format "## executing %s...\n" filename))
        (cmd (cond (cmd)
                   (py-exec-command)
                   (t (py-which-execute-file-command filename))))
        erg)
    (when py-verbose-p
      (unwind-protect
          (save-excursion
            (set-buffer procbuf)
            (funcall (process-filter proc) proc msg))))
    (set-buffer procbuf)
    (process-send-string proc cmd)
    ;; (setq erg (progn (looking-at "[^\n\t\f\r ]+")(match-string-no-properties 0)))
    ;; (setq erg (point))
    (setq erg (goto-char (process-mark proc)))
    erg))

(defun python-send-command (command)
  "Like `python-send-string' but resets `compilation-shell-minor-mode'."
  (when (python-check-comint-prompt)
    (with-current-buffer (process-buffer (python-proc))
      (goto-char (point-max))
      (compilation-forget-errors)
      (python-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

(defun py-send-region (start end)
  "Send the region to the inferior Python process."
  ;; The region is evaluated from a temporary file.  This avoids
  ;; problems with blank lines, which have different semantics
  ;; interactively and in files.  It also saves the inferior process
  ;; buffer filling up with interpreter prompts.  We need a Python
  ;; function to remove the temporary file when it has been evaluated
  ;; (though we could probably do it in Lisp with a Comint output
  ;; filter).  This function also catches exceptions and truncates
  ;; tracebacks not to mention the frame of the function itself.
  ;;
  ;; The `compilation-shell-minor-mode' parsing takes care of relating
  ;; the reference to the temporary file to the source.
  ;;
  ;; Fixme: Write a `coding' header to the temp file if the region is
  ;; non-ASCII.
  (interactive "r")
  (let* ((f (make-temp-file "py"))
	 (command
          ;; IPython puts the FakeModule module into __main__ so
          ;; emacs.eexecfile becomes useless.
          (if (string-match "^ipython" python-command)
              (format "execfile %S" f)
            (format "emacs.eexecfile(%S)" f)))
	 (orig-start (copy-marker start)))
    (when (save-excursion
	    (goto-char start)
	    (/= 0 (current-indentation))) ; need dummy block
      (save-excursion
	(goto-char orig-start)
	;; Wrong if we had indented code at buffer start.
	(set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f nil 'nomsg))
    (write-region start end f t 'nomsg)
    (python-send-command command)
    (with-current-buffer (process-buffer (python-proc))
      ;; Tell compile.el to redirect error locations in file `f' to
      ;; positions past marker `orig-start'.  It has to be done *after*
      ;; `python-send-command''s call to `compilation-forget-errors'.
      (compilation-fake-loc orig-start f))))

(defun python-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (python-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (python-proc) "\n")))

(defun py-send-buffer ()
  "Send the current buffer to the inferior Python process."
  (interactive)
  (py-send-region (point-min) (point-max)))

;;; Subprocess utilities and filters
(defvar py-last-exeption-buffer nil
  "Internal use only - when `py-up-exception' is called in
  source-buffer, this will deliver the exception-buffer again. ")

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
    (forward-line (1- line))
    (message "Jumping to exception in file %s on line %d" file line)))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.

With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (py-find-next-exception-prepare 'down (when (eq 4 (prefix-numeric-value bottom)) "BOTTOM")))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.

With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (unless py-last-exeption-buffer (setq py-last-exeption-buffer (current-buffer)))
  (py-find-next-exception-prepare 'up (when (eq 4 (prefix-numeric-value top)) "TOP")))

(defun py-find-next-exception-prepare (direction start)
  "Setup exception regexps depending from kind of Python shell. "
  (let* ((name (get-process (substring (buffer-name (current-buffer)) 1 -1)))
         (buffer (cond (name (buffer-name (current-buffer)))
                       ((buffer-live-p (get-buffer py-output-buffer))
                        py-output-buffer)
                       (py-last-exeption-buffer (buffer-name py-last-exeption-buffer))
                       (t (error "Don't see exeption buffer")))))
    (when buffer (set-buffer (get-buffer buffer)))
    (switch-to-buffer (current-buffer))
    (if (eq direction 'up)
        (if (string= start "TOP")
            (py-find-next-exception 'bob buffer 're-search-forward "Top")
          (py-find-next-exception 'bol buffer 're-search-backward "Top"))
      (if (string= start "BOTTOM")
          (py-find-next-exception 'eob buffer 're-search-backward "Bottom")
        (py-find-next-exception 'eol buffer 're-search-forward "Bottom")))))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let ((orig (point))
        (origline (py-count-lines))
        file line pos)
    (goto-char (py-point start))
    (if (funcall searchdir py-traceback-line-re nil t)
        (if (save-match-data (eq (py-count-lines) origline))
            (progn
              (forward-line (if (string= errwhere "Top") -1 1))
              (py-find-next-exception start buffer searchdir errwhere))
          (if (not (save-match-data (string-match "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>" (match-string-no-properties 0))))
              (progn
                (setq py-last-exeption-buffer (current-buffer))
                (if (save-match-data (string-match "File" (match-string-no-properties 0)))
                    (progn
                      (setq file (match-string-no-properties 2)
                            pos (point)
                            line (string-to-number (match-string-no-properties 3))))
                  (save-excursion
                    ;; file and line-number are in different lines
                    (setq line (string-to-number (match-string-no-properties 1))
                          pos (point)
                          file (progn
                                 (when (and (re-search-backward "\\(^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^\t >]+\\)>?[ \t]+in[ \t]+\\([^ \t\n]+\\)" nil t 1)
                                            (not (save-match-data (string-match "<\\|^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>" (match-string-no-properties 1)))))
                                   (match-string-no-properties 1))))))
                (if file
                    (when (string-match ".+\.pyc" file)
                      (setq file (substring file 0 -1)))
                  (error "%s of traceback" errwhere))
                (if (and file line)
                    (if
                        (and (string= "<stdin>" file) (eq 1 line))
                        (error "%s of traceback" errwhere)
                      (py-jump-to-exception file line py-line-number-offset))
                  (error "%s of traceback" errwhere)))
            (goto-char orig)
            (error "%s of traceback" errwhere))))))

(defalias 'ipython-send-and-indent 'py-execute-line-ipython)
(defalias 'py-execute-region-in-shell 'py-execute-region)
(defalias 'py-ipython-shell-command-on-region 'py-execute-region-ipython)
(defalias 'py-shell-command-on-region 'py-execute-region-region)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)

(provide 'python-components-execute);
;; python-components-execute.el ends here
