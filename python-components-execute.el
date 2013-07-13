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
(require 'python-components-macros)

(defun toggle-force-local-shell (&optional arg)
  "If locally indicated Python shell should be taken and
enforced upon sessions execute commands.

Toggles boolean `py-force-local-shell-p' along with `py-force-py-shell-name-p'
Returns value of `toggle-force-local-shell' switched to.

When on, kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards.

See also commands
`py-force-local-shell-on'
`py-force-local-shell-off'
 "
  (interactive (list arg))
  (let ((arg (or arg (if py-force-local-shell-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-shell-name (or py-local-command (py-choose-shell)))
          (setq py-force-local-shell-p t))
      (setq py-shell-name (default-value 'py-shell-name))
      (setq py-force-local-shell-p nil))
    (when (interactive-p)
      (if py-force-local-shell-p
          (when py-verbose-p (message "Enforce %s"  py-shell-name))
        (when py-verbose-p (message "py-shell-name default restored to: %s" py-shell-name))))
    py-shell-name))

(defun py-force-local-shell-on ()
  "Make sure, `py-py-force-local-shell-p' is on.

Returns value of `py-force-local-shell-p'.

Kind of an option 'follow', local shell sets `py-shell-name', enforces its use afterwards "
  (interactive "p")
  (let* ((erg (toggle-force-local-shell 1)))
    (when (or py-verbose-p (interactive-p))
      (message "Enforce %s" py-shell-name))))

(defun py-force-local-shell-off ()
  "Restore `py-shell-name' default value and `behaviour'. "
  (interactive "p")
  (let* ((erg (toggle-force-local-shell 1)))
    (when (or py-verbose-p (interactive-p))
      (message "py-shell-name default restored to: %s" py-shell-name)
      (message "Enforce %s" py-shell-name))))

(defun toggle-force-py-shell-name-p (&optional arg)
  "If customized default `py-shell-name' should be enforced upon execution.

If `py-force-py-shell-name-p' should be on or off.
Returns value of `py-force-py-shell-name-p' switched to.

See also commands
force-py-shell-name-p-on
force-py-shell-name-p-off

Caveat: Completion might not work that way.
"
  (interactive)
  (let ((arg (or arg (if py-force-py-shell-name-p -1 1))))
    (if (< 0 arg)
        (setq py-force-py-shell-name-p t)
      (setq py-force-py-shell-name-p nil))
    (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
    py-force-py-shell-name-p))

(defun force-py-shell-name-p-on (&optional arg)
  "Switches `py-force-py-shell-name-p' on.

Customized default `py-shell-name' will be enforced upon execution.
Returns value of `py-force-py-shell-name-p'.

Caveat: Completion might not work that way.
"
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-force-py-shell-name-p arg))
  (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

(defun force-py-shell-name-p-off ()
  "Make sure, `py-force-py-shell-name-p' is off.

Function to use by executes will be guessed from environment.
Returns value of `py-force-py-shell-name-p'. "
  (interactive)
  (toggle-force-py-shell-name-p -1)
  (when (or py-verbose-p (interactive-p)) (message "py-force-py-shell-name-p: %s" py-force-py-shell-name-p))
  py-force-py-shell-name-p)

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
  "If `py-switch-buffers-on-execute-p' should be on or off.

  Returns value of `py-switch-buffers-on-execute-p' switched to. "
  (interactive)
  (let ((arg (or arg (if py-switch-buffers-on-execute-p -1 1))))
    (if (< 0 arg)
        (setq py-switch-buffers-on-execute-p t)
      (setq py-switch-buffers-on-execute-p nil))
    (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
    py-switch-buffers-on-execute-p))

(defun py-shell-switch-buffers-on-execute-on (&optional arg)
  "Make sure, `py-switch-buffers-on-execute-p' is on.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (toggle-py-shell-switch-buffers-on-execute arg))
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-shell-switch-buffers-on-execute-off ()
  "Make sure, `py-switch-buffers-on-execute-p' is off.

Returns value of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (toggle-py-shell-switch-buffers-on-execute -1)
  (when (interactive-p) (message "py-shell-switch-buffers-on-execute: %s" py-switch-buffers-on-execute-p))
  py-switch-buffers-on-execute-p)

(defun py-guess-default-python ()
  "Defaults to \"python\", if guessing didn't succeed. "
  (interactive)
  (let* ((cmd (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p cmd (executable-find cmd))))
    (when (interactive-p)
      (if erg
          (message "%s" cmd)
        (message "%s" "Could not detect Python on your system")))))

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

(defun py-set-ipython-completion-command-string (&optional pyshellname)
  "Set and return `ipython-completion-command-string'. "
  (interactive)
  (let* ((pyshellname (or pyshellname py-shell-name))
         (ipython-version
          (if (string-match "ipython" pyshellname)
              (string-to-number (substring (shell-command-to-string (concat pyshellname " -V")) 2 -1))
            ;; choose default installed IPython
            (string-to-number (substring (shell-command-to-string (concat "ipython" " -V")) 2 -1))
            )))
    (when ipython-version
      (setq ipython-completion-command-string (if (< ipython-version 11) ipython0.10-completion-command-string ipython0.11-completion-command-string))
      ipython-completion-command-string)))

(defun py-process-name (&optional name dedicated nostars sepchar)
  "Return the name of the running Python process, `get-process' willsee it. "
  (let* ((sepchar (or sepchar (char-to-string py-separator-char)))
         (thisname (if name
                       (if (string-match sepchar name)
                           (substring name (progn (string-match (concat "\\(.+\\)" sepchar "\\(.+\\)$") name) (match-beginning 2)))

                         name)
                     (substring py-shell-name (or (string-match (concat sepchar ".+$") py-shell-name) 0))))
         (nname (cond (dedicated
                       (make-temp-name (concat thisname "-")))
                      ;; ((string-match "\*" (buffer-name))
                      ;; (replace-regexp-in-string "\*" "" (buffer-name)))
                      (t thisname)))
         (erg (cond ((or (string-match "ipython" nname)
                         (string-match "IPython" nname))
                     "IPython")
                    (nname))))
    (unless (or nostars (string-match "^\*" erg))(setq erg (concat "*" erg "*")))
    erg))

(defun py-buffer-name-prepare (name &optional sepchar dedicated)
  "Return an appropriate name to display in modeline.
SEPCHAR is the file-path separator of your system. "
  (let ((sepchar (or sepchar (char-to-string py-separator-char)))
        prefix erg suffix)
    (when (string-match (regexp-quote sepchar) name)
      (unless py-modeline-acronym-display-home-p
        (when (string-match (concat "^" (expand-file-name "~")) name)
          (setq name (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" name))))
      (save-match-data
        (setq liste (split-string name sepchar)))
      (dolist (ele liste)
        (unless (string= "" ele)
          (setq prefix (concat prefix (char-to-string (aref ele 0))))))
      (unless py-modeline-display-full-path-p

        (setq name (substring name (1+ (string-match (concat sepchar "[^" sepchar "]+$") name))))))
    (setq erg
          (cond ((string= "ipython" name)
                 (replace-regexp-in-string "ipython" "IPython" name))
                ((string= "jython" name)
                 (replace-regexp-in-string "jython" "Jython" name))
                ((string= "python" name)
                 (replace-regexp-in-string "python" "Python" name))
                ((string-match "python2" name)
                 (replace-regexp-in-string "python2" "Python2" name))
                ((string-match "python3" name)
                 (replace-regexp-in-string "python3" "Python3" name))
                (t name)))
    (when dedicated
      (setq erg (make-temp-name (concat erg "-"))))
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

(defun py-shell-manage-windows (switch split oldbuf output-buffer &optional windows-displayed windows-config err-p)
  (let ((exception-buffer (or py-exception-buffer oldbuf)))
    (cond (err-p
           (and (eq 1 (length windows-displayed))
                (funcall py-split-windows-on-execute-function)
                (display-buffer output-buffer))
           (py-jump-to-exception err-p py-exception-buffer))
          ;; split and switch
          ((and (not (eq split 'nosplit))
                py-split-windows-on-execute-p
                (not (eq switch 'noswitch))
                (or (eq switch 'switch)
                    py-switch-buffers-on-execute-p))
           (when (< (count-windows) py-max-split-windows)
             (funcall py-split-windows-on-execute-function))
           (set-buffer output-buffer)
           (switch-to-buffer (current-buffer))
           (display-buffer oldbuf))
          ;; split, not switch
          ((and
            (not (eq split 'nosplit))
            py-split-windows-on-execute-p
            (or (eq switch 'noswitch)
                (not (eq switch 'switch))))
           (delete-other-windows)
           (if (< (count-windows) py-max-split-windows)
               (progn
                 (funcall py-split-windows-on-execute-function)
                 ;; (pop-to-buffer oldbuf)
                 (set-buffer oldbuf)
                 (switch-to-buffer (current-buffer))
                 (display-buffer output-buffer 'display-buffer-reuse-window))
             (display-buffer output-buffer 'display-buffer-reuse-window)))
          ;; no split, switch
          ((or (eq switch 'switch)
               (and (not (eq switch 'noswitch))
                    py-switch-buffers-on-execute-p))
           (let (pop-up-windows)

             (set-buffer output-buffer)
             (switch-to-buffer (current-buffer))))
          ;; no split, no switch
          ((or (eq switch 'noswitch)
               (not py-switch-buffers-on-execute-p))
           (if (equal (window-list-1) windows-displayed)
               (jump-to-register 313465889)
             (let (pop-up-windows)
               (set-buffer oldbuf)
               (switch-to-buffer (current-buffer))))))))

(defun py-report-executable (py-buffer-name)
  (let ((erg (downcase (replace-regexp-in-string
                        "<\\([0-9]+\\)>" ""
                        (replace-regexp-in-string
                         "\*" ""
                         (if
                             (string-match " " py-buffer-name)
                             (substring py-buffer-name (1+ (string-match " " py-buffer-name)))
                           py-buffer-name))))))
    (when (string-match "-" erg)
      (setq erg (substring erg 0 (string-match "-" erg))))
    erg))

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
             py-file-queue)
    (if py-switch-buffers-on-execute-p
        (pop-to-buffer (current-buffer)))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (py-execute-file-base pyproc (car py-file-queue))))))

(defun py-shell (&optional argprompt dedicated pyshellname switch sepchar py-buffer-name done split)
  "Start an interactive Python interpreter in another window.
Interactively, \\[universal-argument] 4 prompts for a buffer.
\\[universal-argument] 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

Returns py-shell's buffer-name.
Optional string PYSHELLNAME overrides default `py-shell-name'.
Optional symbol SWITCH ('switch/'noswitch) precedes `py-switch-buffers-on-execute-p'
When SEPCHAR is given, `py-shell' must not detect the file-separator.
BUFFER allows specifying a name, the Python process is connected to
When DONE is `t', `py-shell-manage-windows' is omitted
Optional symbol SPLIT ('split/'nosplit) precedes `py-split-buffers-on-execute-p'
"
  (interactive "P")
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (switch (or switch py-switch-buffers-on-execute-p))
         (split (or split py-split-windows-on-execute-p))
         (sepchar (or sepchar (char-to-string py-separator-char)))
         (args py-python-command-args)
         (oldbuf (current-buffer))
         (path (getenv "PYTHONPATH"))
         ;; reset later on
         (py-buffer-name
          (or py-buffer-name
              (when argprompt
                (cond
                 ((eq 4 (prefix-numeric-value argprompt))
                  (setq py-buffer-name
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
                                            (mapconcat 'identity py-python-command-args " ") " ")))))))))
         (pyshellname (or pyshellname (py-choose-shell)))
         ;; If we use a pipe, Unicode characters are not printed
         ;; correctly (Bug#5794) and IPython does not work at
         ;; all (Bug#5390). python.el
         (process-connection-type t)
         ;; already in py-choose-shell
         (py-use-local-default
          (if (not (string= "" py-shell-local-path))
              (expand-file-name py-shell-local-path)
            (when py-use-local-default
              (error "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'"))))
         (py-buffer-name-prepare (unless (and py-buffer-name (not dedicated))
                                   (py-buffer-name-prepare (or pyshellname py-shell-name) sepchar dedicated)))
         (py-buffer-name (or py-buffer-name-prepare py-buffer-name))
         (executable (cond (pyshellname)
                           (py-buffer-name
                            (py-report-executable py-buffer-name))))
         proc py-smart-indentation)
    ;; lp:1169687, if called from within an existing py-shell, open a new one
    (and (string= py-buffer-name (buffer-name oldbuf))
         (setq py-buffer-name (generate-new-buffer-name py-buffer-name)))

    (unless (comint-check-proc py-buffer-name)
      (set-buffer (apply 'make-comint-in-buffer executable py-buffer-name executable nil args))
      (unless (interactive-p) (sit-for 0.3))
      (set (make-local-variable 'comint-prompt-regexp)
           (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
                  (concat "\\("
                          (mapconcat 'identity
                                     (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                     "\\|")
                          "\\)"))
                 (t (concat "\\("
                            (mapconcat 'identity
                                       (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                       "\\|")
                            "\\)"))))
      (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
      (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
      (set (make-local-variable 'comint-use-prompt-regexp) nil)
      (set (make-local-variable 'compilation-error-regexp-alist)
           python-compilation-regexp-alist)
      ;; (setq completion-at-point-functions nil)
      (and py-fontify-shell-buffer-p
           (set (make-local-variable 'font-lock-defaults)
                '(py-font-lock-keywords nil nil nil nil
                                        (font-lock-syntactic-keywords
                                         . py-font-lock-syntactic-keywords))))
      (set (make-local-variable 'comment-start) "# ")
      (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
      (set (make-local-variable 'comment-column) 40)
      (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
      (set (make-local-variable 'indent-region-function) 'py-indent-region)
      (set (make-local-variable 'indent-line-function) 'py-indent-line)
      (setq proc (get-buffer-process py-buffer-name))
      (move-marker (process-mark proc) (point-max))
      (py-shell-send-setup-code proc)
      (compilation-shell-minor-mode 1)
      (setq comint-input-sender 'py-shell-simple-send)
      (sit-for 1)
      (setq comint-input-ring-file-name
            (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
                   (if py-honor-IPYTHONDIR-p
                       (if (getenv "IPYTHONDIR")
                           (concat (getenv "IPYTHONDIR") "/history")
                         py-ipython-history)
                     py-ipython-history))
                  (t
                   (if py-honor-PYTHONHISTORY-p
                       (if (getenv "PYTHONHISTORY")
                           (concat (getenv "PYTHONHISTORY") "/" (py-report-executable py-buffer-name) "_history")
                         py-ipython-history)
                     py-ipython-history))))
      (comint-read-input-ring t)
      (set-process-sentinel (get-buffer-process py-buffer-name)
                            #'shell-write-history-on-exit)
      (add-hook 'comint-output-filter-functions
                'ansi-color-process-output)
      (use-local-map py-shell-map)
      ;; pdbtrack
      (and py-pdbtrack-do-tracking-p
           (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file t)
           (remove-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file t))
      (set-syntax-table python-mode-syntax-table))
    (goto-char (point-max))
    ;; (add-hook 'py-shell-hook 'py-dirstack-hook)
    (and py-fontify-shell-buffer-p (font-lock-fontify-buffer))
    (when py-shell-hook (run-hooks 'py-shell-hook))
    (unless done (py-shell-manage-windows switch split oldbuf py-buffer-name))
    py-buffer-name))

(defun py-shell-get-process (&optional argprompt dedicated pyshellname switch sepchar py-buffer-name done)
  "Get appropriate Python process for current buffer and return it."
  (interactive)
  (let ((erg (get-buffer-process (py-shell argprompt dedicated pyshellname switch sepchar py-buffer-name done))))
    (when (interactive-p) (message "%S" erg))
    erg))

(defalias 'py-switch-to-python 'py-switch-to-shell)
(defun py-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;; Code execution commands
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

Ignores setting of `py-switch-buffers-on-execute-p', buffer with region stays current.
 "
  (interactive "r\nP")
  (py-execute-base start end py-shell-name dedicated 'noswitch))

(defalias 'py-send-region 'py-execute-region)
;;; execute region
(defun py-execute-region (start end &optional shell dedicated switch nostars sepchar split file)
  "Send the region to a Python interpreter.

When called with \\[universal-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[universal-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)
"
  (interactive "r\nP")
  (let ((shell (cond ((or py-force-local-shell-p py-force-py-shell-name-p)
                      py-shell-name)
                     ((or py-force-py-shell-name-p (eq 4 (prefix-numeric-value shell))) (default-value 'py-shell-name))
                     ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                      (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                     (t shell))))
    (py-execute-base start end shell dedicated switch nostars sepchar split file)))

(defun py-execute-region-default (start end &optional dedicated)
  "Send the region to the systems default Python interpreter.
See also `py-execute-region'. "
  (interactive "r\nP")
  (py-execute-base start end (default-value 'py-shell-name) dedicated))

(defun py-execute-region-dedicated (start end &optional shell)
  "Get the region processed by an unique Python interpreter.

When called with \\[universal-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[universal-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument. "
  (interactive "r\nP")
  (let ((shell (cond ((eq 4 (prefix-numeric-value shell)) (default-value 'py-shell-name))
                     ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
                      (read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
                     (t shell))))
    (py-execute-base start end shell t)))

(defun py-execute-region-switch (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to.
"
  (interactive "r\nP")
  (py-execute-base start end py-shell-name dedicated 'switch))

(defalias 'py-execute-region-dedicated-default 'py-execute-region-default-dedicated)
(defun py-execute-region-default-dedicated (start end)
  "Send the region to an unique shell of systems default Python. "
  (interactive "r")
  (py-execute-base start end (default-value 'py-shell-name) t))

(defun py-delete-temporary (file localname filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf))
  (when (buffer-live-p localname)
    (kill-buffer localname)))

(defun py-execute-python-mode-v5 (start end &optional pyshellname)
  (interactive "r")
  (let ((py-exception-buffer (current-buffer))
        (cmd (concat (or pyshellname py-shell-name) (if (string-equal py-which-bufname
                                                                      "Jython")
                                                        " -"
                                                      ;; " -c "
                                                      ""))))
    (save-excursion
      (shell-command-on-region start end
                               cmd py-output-buffer))
    (if (not (get-buffer py-output-buffer))
        (message "No output.")

      (let* ((err-p (py-postprocess-output-buffer py-output-buffer py-exception-buffer))
             (line (cadr err-p)))
        (if err-p
            (when (and py-jump-on-exception line)
              (pop-to-buffer py-exception-buffer))
          (pop-to-buffer py-output-buffer)
          (goto-char (point-max))
          (setq erg (copy-marker (point))))))))

(defun py-execute-buffer-file (start end pyshellname dedicated switch nostars sepchar split file)
  (let* ((oldbuf (current-buffer))
         (pyshellname (or pyshellname (py-choose-shell)))
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
         (strg (buffer-substring-no-properties start end))
         (sepchar (or sepchar (char-to-string py-separator-char)))
         (py-buffer-name (py-buffer-name-prepare pyshellname sepchar))
         (localname file)
         (switch (or switch py-switch-buffers-on-execute-p))
         (split (or split py-split-windows-on-execute-p))
         (proc (if dedicated
                   (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t))
                 (or (get-buffer-process py-buffer-name)
                     (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t)))))
         (procbuf (process-buffer proc))
         ;; (filebuf (get-buffer file))
         (pec (if (string-match "[pP]ython ?3" py-buffer-name)
                  (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" localname localname)
                (format "execfile(r'%s') # PYTHON-MODE\n" localname))))
    (if (file-readable-p file)
        (progn
          (when (string-match "ipython" (process-name proc))
            (sit-for py-ipython-execute-delay))
          (setq erg (py-execute-file-base proc file pec procbuf))
          (sit-for 0.2)
          (setq err-p (py-postprocess-output-buffer procbuf py-exception-buffer))
          (if err-p
              (progn
                (setnth 1 err-p (1- (nth 1 err-p)))
                (py-jump-to-exception err-p py-exception-buffer))
            (py-shell-manage-windows switch split oldbuf py-buffer-name))
          (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
            (when py-verbose-p (message "Output buffer: %s" procbuf))))
      (message "%s not readable. %s" file "Do you have permissions?"))
    erg))

(defun py-insert-offset-lines (line)
  "Fix offline amount, make error point at the corect line. "
  (insert (make-string (- line (count-lines (point-min) (point))) 10)))

(defun py-execute-buffer-finally (start end &optional pyshell--name dedicated switch nostars sepchar split)
  (let* ((windows-config (window-configuration-to-register 313465889))
         (windows-displayed (window-list-1))
         (oldbuf (current-buffer))
         (exception-buffer (or py-exception-buffer oldbuf))
         (pyshellname (or pyshell--name (py-choose-shell)))
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
         (strg (buffer-substring-no-properties start end))
         (line (save-restriction (widen) (count-lines (point-min) start)))
         (sepchar (or sepchar (char-to-string py-separator-char)))
         (py-buffer-name (py-buffer-name-prepare pyshellname sepchar))
         (temp (make-temp-name
                (concat (replace-regexp-in-string (regexp-quote sepchar) "-" (replace-regexp-in-string (concat "^" (regexp-quote sepchar)) "" (replace-regexp-in-string ":" "-" pyshellname))) "-")))
         (localname (concat (expand-file-name py-temp-directory) sepchar (replace-regexp-in-string (regexp-quote sepchar) "-" temp) ".py"))
         (switch (or switch py-switch-buffers-on-execute-p))
         (split (or split py-split-windows-on-execute-p))
         (proc (if dedicated
                   (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t))
                 (or (get-buffer-process py-buffer-name)
                     (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t)))))
         (procbuf (process-buffer proc))
         (file (with-current-buffer py-buffer-name ; create the file to be executed in context of the shell
                 (concat (file-remote-p default-directory) localname)))
         (filebuf (get-buffer-create file))
         (pec (if (string-match "[pP]ython ?3" py-buffer-name)
                  (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" localname localname)
                (format "execfile(r'%s') # PYTHON-MODE\n" localname)))
         (wholebuf (when (boundp 'wholebuf) wholebuf))
         (comint-scroll-to-bottom-on-output t)
         erg err-p lineadd output-buffer)
    (set-buffer filebuf)
    (erase-buffer)
    (unless py-if-name-main-permission-p
      (setq strg (replace-regexp-in-string
                  "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
                  ;; space after __main__, i.e. will not be executed
                  "if __name__ == '__main__ ':" strg)))
    (insert strg)
    (switch-to-buffer (current-buffer))
    (py-fix-start (point-min)(point-max))
    (py-if-needed-insert-shell pyshellname sepchar)
    (unless wholebuf (py-insert-coding))
    (unless (string-match "[jJ]ython" pyshellname) (py-insert-execute-directory execute-directory))
    ;; fix offline amount, make erorr point at the corect line
    (setq lineadd (- line (+ 2 (count-lines (point-min) (point)))))
    (and (< 0 lineadd) (insert (make-string lineadd 10)))
    (set-buffer filebuf)
    (write-region (point-min) (point-max) file nil t nil 'ask)
    (set-buffer-modified-p 'nil)
    (unwind-protect
        (if (file-readable-p file)
            (progn
              (and (string-match "ipython" (process-name proc))
                   (sit-for py-ipython-execute-delay))
              ;; if only a part of buffer is executed, messaging `executing file...' is pointless
              (let (py-verbose-p)
                (setq erg (py-execute-file-base proc file pec procbuf
                                                (if (buffer-file-name oldbuf)
                                                    (buffer-file-name oldbuf)
                                                  (buffer-name oldbuf))))
                (sit-for 0.1)
                (setq err-p (py-postprocess-output-buffer py-buffer-name (or (buffer-file-name exception-buffer) (buffer-name exception-buffer))))
                (when py-enforce-output-buffer-p
                  (setq output-buffer py-output-buffer)
                  ;; maybe (or py-execute-python-mode-v5 ?
                  (set-buffer (get-buffer-create output-buffer))
                  (erase-buffer)
                  (insert erg))
                (py-shell-manage-windows switch split oldbuf (or output-buffer py-buffer-name) windows-displayed windows-config err-p))
              (unless (or (string= (buffer-name (current-buffer)) (buffer-name procbuf))
                          err-p)
                (when py-verbose-p (message "Output buffer: %s" (or output-buffer procbuf))))
              (sit-for 0.1))
          ;;)
          (message "%s not readable. %s" file "Do you have write permissions?")))
    (and py-cleanup-temporary
         (py-delete-temporary file localname filebuf))
    (and py-store-result-p (kill-new erg))
    erg))


(defun py-execute-ge24.3 (start end &optional pyshellname dedicated switch nostars sepchar split file)
  "Select the handler. "
  (and (buffer-file-name) buffer-offer-save (buffer-modified-p) (y-or-n-p "Save buffer before executing? ")
       (write-file (buffer-file-name)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (py-exception-buffer (current-buffer))
         (line (count-lines (point-min) start))
         (pyshellname (or pyshellname (py-choose-shell)))
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
         (strg (buffer-substring-no-properties start end))
         (sepchar (or sepchar (char-to-string py-separator-char)))
         (py-buffer-name (py-buffer-name-prepare pyshellname sepchar))
         (temp (make-temp-name
                (concat (replace-regexp-in-string (regexp-quote sepchar) "-" (replace-regexp-in-string (concat "^" (regexp-quote sepchar)) "" (replace-regexp-in-string ":" "-" pyshellname))) "-")))
         (localname (or (buffer-file-name) (concat (expand-file-name py-temp-directory) sepchar (replace-regexp-in-string (regexp-quote sepchar) "-" temp) ".py")))
         (switch (or switch py-switch-buffers-on-execute-p))
         (split (or split py-split-windows-on-execute-p))
         (proc (if dedicated
                   (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t))
                 (or (get-buffer-process py-buffer-name)
                     (get-buffer-process (py-shell nil dedicated pyshellname switch sepchar py-buffer-name t)))))
         (procbuf (process-buffer proc))
         (file (or file (with-current-buffer py-buffer-name ; create the file to be executed in context of the shell
                          (concat (file-remote-p default-directory) localname))))
         (filebuf (get-buffer-create file))
         ;; (pec (if (string-match "[pP]ython ?3" py-buffer-name)
         ;;          (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" localname localname)
         ;;        (format "execfile(r'%s') # PYTHON-MODE\n" localname)))
         (comint-scroll-to-bottom-on-output t)
         err-p)
    (set-buffer filebuf)
    (erase-buffer)
    ;; (switch-to-buffer (current-buffer))
    (newline line)
    (save-excursion
      (insert strg))
    (py-fix-start (point) (point-max))
    (unless (string-match "[jJ]ython" pyshellname)
      (when (and execute-directory py-use-current-dir-when-execute-p
                 (not (string= execute-directory default-directory)))
        (message "Warning: options `execute-directory' and `py-use-current-dir-when-execute-p' may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))
           ;; (py-send-string-no-output (concat "import os; os.chdir(\"" execute-directory "\")\n") proc)
           ))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    ;; (write-file (buffer-name filebuf))
    ;; (py-execute-file-base proc file pec procbuf)
    ;; (py-shell-send-string (buffer-substring-no-properties (point-min) (point-max)) proc t file)
    (sit-for 0.1)
    (if (and (setq err-string (py-postprocess-output-buffer procbuf py-exception-buffer))
             (car err-string)
             (not (markerp err-string)))
        (py-jump-to-exception err-string beg end)
      (py-shell-manage-windows switch split py-exception-buffer py-buffer-name)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defun py-execute-base (start end &optional pyshellname dedicated switch nostars sepchar split file)
  "Select the handler. "
  (cond (;; enforce proceeding as python-mode.el v5
         python-mode-v5-behavior-p
         (py-execute-python-mode-v5 start end pyshellname))
        (py-execute-no-temp-p
         (py-execute-ge24.3 start end pyshellname dedicated switch nostars sepchar split file))
        ;; No need for a temporary file than
        ((and (not (buffer-modified-p)) file)
         (py-execute-buffer-file start end pyshellname dedicated switch nostars sepchar split file))
        (t (py-execute-buffer-finally start end pyshellname dedicated switch nostars sepchar split))))

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

(defun py-if-needed-insert-shell (&optional name sepchar pyshell--name)
  (let ((erg (or
              name
              (and
               ;; is a shell was specified, don't rely on existing shebang
               (not pyshell--name
                    (save-restriction
                      (widen)
                      (save-excursion
                        (goto-char (point-min))
                        (looking-at py-shebang-regexp))))
               (match-string-no-properties 0))
              (py-choose-shell-by-shebang)
              (py-choose-shell-by-import)
              py-shell-name))
        (sepchar (or sepchar (char-to-string py-separator-char))))
    (when (string-match " " erg) (setq erg (substring erg (1+ (string-match " " erg))))
          ;; closing ">"
                (setq erg (substring erg 0 (1- (length erg)))))
          (goto-char (point-min))
          (while (empty-line-p) (delete-region (point) (1+ (line-end-position))))
          (unless (looking-at py-shebang-regexp)
            (if (string-match (concat "^" erg) "ipython")
                (progn
                  (shell-command "type ipython" t)
                  (switch-to-buffer (current-buffer))
                  (when (looking-at "[^/\n\r]+")
                    (replace-match "#! ")))
              (if (string-match (regexp-quote sepchar) erg)
                  (insert (concat "#! " erg "\n"))
                (insert (concat py-shebang-startstring " " erg "\n")))))))

(defun py-insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-end-of-statement)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py-insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless (empty-line-p) (newline))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

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
  ;; (switch-to-buffer (current-buffer))
  (python-mode)
  (goto-char start)
  (while (empty-line-p)
    (delete-region (line-beginning-position) (1+ (line-end-position))))
  (back-to-indentation)
  (unless (py-beginning-of-statement-p)
    (py-down-statement))
  (while (not (eq (current-indentation) 0))
    (py-shift-left py-indent-offset start end))
  (goto-char (point-max))
  (unless (empty-line-p)
    (newline)))

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
                     (get-buffer-process (py-shell argprompt dedicated (or shell (default-value 'py-shell-name)))))))
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
(defun py-execute-buffer (&optional shell dedicated switch nostars sepchar split file)
  "Send the contents of the buffer to a Python interpreter.

When called with \\[universal-argument], execution through `default-value' of `py-shell-name' is forced.
When called with \\[universal-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch) "
  (interactive "P")
  (let* ((wholebuf t)
         (py-master-file (or py-master-file (py-fetch-py-master-file)))
         (file (or
                file
                (if py-master-file
                    (prog1 (expand-file-name py-master-file)
                      (set-buffer
                       (or (get-file-buffer filename)
                           (get-file-buffer (find-file-noselect filename))))))
                (buffer-file-name)))
         (beg (point-min))
         (end (point-max)))
    (py-execute-region beg end shell dedicated switch nostars sepchar split file)))

(defun py-execute-buffer-base (&optional shell dedicated switch nostars sepchar split file)
  "Honor `py-master-file'. "
  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (file (or
               file
               (if py-master-file
                   (prog1 (expand-file-name py-master-file)
                     (set-buffer
                      (or (get-file-buffer filename)
                          (get-file-buffer (find-file-noselect filename))))))
               (when (buffer-file-name) (buffer-file-name))))
        (beg (point-min))
        (end (point-max)))
    (py-execute-region beg end shell dedicated switch nostars sepchar split file)))

(defun py-execute-buffer-dedicated (&optional shell)
  "Send the contents of the buffer to a unique Python interpreter.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[universal-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell t))

(defun py-execute-buffer-switch (&optional shell dedicated)
  "Send the contents of the buffer to a Python interpreter and switches to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[universal-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell dedicated 'switch))

(defun py-execute-buffer-no-switch (&optional shell dedicated)
  "Send the contents of the buffer to a Python interpreter but don't switch to output.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[universal-argument] user is prompted to specify another then default shell.
See also `\\[py-execute-region]'. "
  (interactive "P")
  (py-execute-buffer-base shell dedicated 'noswitch))

(defalias 'py-execute-buffer-switch-dedicated 'py-execute-buffer-dedicated-switch)
(defun py-execute-buffer-dedicated-switch (&optional shell)
  "Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

With \\[universal-argument] user is prompted to specify another then default shell.
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

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "python")))

(defun py-execute-region-python-no-switch (start end)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "python")))

(defun py-execute-region-python2 (start end)
  "Send the region to a common shell calling the python2 interpreter. "
  (interactive "r")
  (py-execute-base start end "python2"))

(defun py-execute-region-python2-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.
Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "python2")))

(defun py-execute-region-python2-no-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "python2")))

(defun py-execute-region-python2.7 (start end)
  "Send the region to a common shell calling the python2.7 interpreter. "
  (interactive "r")
  (py-execute-base start end "python2.7"))

(defun py-execute-region-python2.7-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "python2.7")))

(defun py-execute-region-python2.7-no-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "python2.7")))

(defun py-execute-region-python3 (start end)
  "Send the region to a common shell calling the python3 interpreter. "
  (interactive "r")
  (py-execute-base start end "python3"))

(defun py-execute-region-python3-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "python3")))

(defun py-execute-region-python3-no-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "python3")))

(defun py-execute-region-python3.2 (start end)
  "Send the region to a common shell calling the python3.2 interpreter. "
  (interactive "r")
  (py-execute-base start end "python3.2"))

(defun py-execute-region-python3.2-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "python3.2")))

(defun py-execute-region-python3.2-no-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "python3.2")))

(defun py-execute-region-ipython (start end)
  "Send the region to a common shell calling the ipython interpreter. "
  (interactive "r")
  (py-execute-base start end "ipython"))

(defun py-execute-region-ipython-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "ipython")))

(defun py-execute-region-ipython-no-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
    (py-execute-base start end "ipython")))

(defun py-execute-region-jython (start end)
  "Send the region to a common shell calling the jython interpreter. "
  (interactive "r")
  (py-execute-base start end "jython"))

(defun py-execute-region-jython-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py-execute-base start end "jython")))

(defun py-execute-region-jython-no-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p))
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
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output")))
        (cmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat cmd " " filename) output-buffer error-buffer)
    (when (interactive-p) (switch-to-buffer output-buffer))))

(defun py-exec-execfile-region (start end &optional shell)
  "Execute the region in a Python interpreter. "
  (interactive "r\nP")
  (let ((shell (if (eq 4 (prefix-numeric-value shell))
                   (read-from-minibuffer "Shell: " (default-value 'py-shell-name))
                 py-shell-name)))
    (let ((strg (buffer-substring-no-properties start end)))
      (py-exec-execfile-base strg shell (interactive-p)))))

(defun py-exec-execfile-base (strg shell iact)
  (let* ((temp (make-temp-name (concat (buffer-name) "-")))
         (file (concat (expand-file-name temp) py-temp-directory ".py"))
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
  (let* ((oldbuf (current-buffer))
         (file (or (expand-file-name filename) (when (ignore-errors (file-readable-p (buffer-file-name))) (buffer-file-name))))
         (shell (or shell (progn (with-temp-buffer (insert-file-contents file)(py-choose-shell)))))
         (name (py-process-name shell dedicated))
         (proc (get-buffer-process (py-shell nil dedicated (or shell (downcase name)))))
         (py-buffer-name (buffer-name (process-buffer proc)))
         (pec (if (string-match "Python3" name)
                  (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file)
                (format "execfile(r'%s') # PYTHON-MODE\n" file)))
         (comint-scroll-to-bottom-on-output t)
         erg)
    (if (file-readable-p file)
        (progn
          (setq erg (py-execute-file-base proc file pec))
          (setq py-exception-buffer (cons file (current-buffer)))
          (py-shell-manage-windows switch split oldbuf py-buffer-name)
          (sit-for 0.1)
          erg)
      (message "File not readable: %s" "Do you have write permissions?"))))

(defun py-execute-file-base (proc filename &optional cmd procbuf origfile)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing.
Returns position where output starts. "
  (let ((procbuf (or procbuf (process-buffer proc)))
        (comint-scroll-to-bottom-on-output t)
        ;; filename might be a temporary, message the orig rather
        (msg (and py-verbose-p (format "## executing %s...\n" (or origfile filename))))
        (cmd (cond (cmd)
                   (py-exec-command)
                   (t (py-which-execute-file-command filename))))
        erg orig)
    (and py-verbose-p
      (unwind-protect
          (save-excursion
            (set-buffer procbuf)
            (funcall (process-filter proc) proc msg))))
    (set-buffer procbuf)
    (goto-char (point-max))
    (setq orig (point))
    (comint-send-string proc cmd)
    (goto-char (point-max))
    (sit-for 0.1)
    ;; (process-send-string proc cmd)
    (setq erg
          (py-output-filter
           (buffer-substring-no-properties orig (point))))
    ;; "Traceback (most recent call last):\n  File \"<stdin>\", line 1, in <module>\n  File \"/tmp/python3-6503yvo.py\", line 135\n    print(44*0e)\n              ^\nSyntaxError: invalid token"
    (and origfile (string-match "\\(.+\\) File \"[^\"]+\", line \\(.+\\)" erg)
         (setq erg (replace-regexp-in-string  "\\(.+\\) File \"[^\"]+\", line \\(.+\\)" (concat (match-string-no-properties 1) " File \"" origfile "\", line " (match-string-no-properties 2)) erg)))
    erg))

;;; Subprocess utilities and filters
(defvar py-last-exeption-buffer nil
  "Internal use only - when `py-up-exception' is called in
  source-buffer, this will deliver the exception-buffer again. ")

(defun py-jump-to-exception-intern (line action exception-buffer)
  (set-buffer exception-buffer)
  ;; (message "%s" (current-buffer) )
  (goto-char (point-min))
  (forward-line (1- line))
  (switch-to-buffer (current-buffer))
  (push-mark)
  (and (search-forward action (line-end-position) t)
       (and py-verbose-p (message "Exception in file %s on line %d" py-exception-buffer (1+ line)))
       (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                    'face 'highlight)
       (sit-for py-error-markup-delay)
       ;; (sit-for 4)
       (remove-overlays (match-beginning 0) (match-end 0) 'face 'highlight)
       ;; (exchange-point-and-mark)
))

;; Result: (nil 5 "print(34ed)" " SyntaxError: invalid token ")
(defun py-jump-to-exception (err-p exception-buffer &optional file)
  "Jump to the Python code in FILE at LINE."
  (let (
        ;; (inhibit-point-motion-hooks t)
        (file (or file (car err-p)))
        (line (cadr err-p))
        (action (nth 2 err-p))
        (errm (nth 3 err-p)))
    (cond ((and exception-buffer
                (buffer-live-p exception-buffer))
           ;; (pop-to-buffer exception-buffer)
           (py-jump-to-exception-intern line action exception-buffer))
          ((file-readable-p file)
           (find-file file)
           (py-jump-to-exception-intern line action (get-buffer (file-name-nondirectory file))))
          ((buffer-live-p (get-buffer file))
           (set-buffer file)
           (switch-to-buffer (current-buffer))
           (py-jump-to-exception-intern line action file))
          (t setq file (find-file (read-file-name "Exception file: "
                                                  nil
                                                  file t)))
          (py-jump-to-exception-intern line action file))))

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
           (py-jump-to-exception (car info) (cdr info)))))
   ;; Emacs -- Please port this!
   ))

(defun py-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at py-traceback-line-re)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (not file)
        (error "Not on a traceback line"))
    (py-jump-to-exception file line)))

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
        (py-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

;; (defun py-find-next-exception (start buffer searchdir errwhere)
;;   "Find the next Python exception and jump to the code that caused it.
;; START is the buffer position in BUFFER from which to begin searching
;; for an exception.  SEARCHDIR is a function, either
;; `re-search-backward' or `re-search-forward' indicating the direction
;; to search.  ERRWHERE is used in an error message if the limit (top or
;; bottom) of the trackback stack is encountered."
;;   (let ((orig (point))
;;         (origline (py-count-lines))
;;         file line pos)
;;     (goto-char (py-point start))
;;     (if (funcall searchdir py-traceback-line-re nil t)
;;         (if (save-match-data (eq (py-count-lines) origline))
;;             (progn
;;               (forward-line (if (string= errwhere "Top") -1 1))
;;               (py-find-next-exception start buffer searchdir errwhere))
;;           (if (not (save-match-data (string-match "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>" (match-string-no-properties 0))))
;;               (progn
;;                 (setq py-last-exeption-buffer (current-buffer))
;;                 (if (save-match-data (string-match "File" (match-string-no-properties 0)))
;;                     (progn
;;                       (setq file (match-string-no-properties 2)
;;                             pos (point)
;;                             line (string-to-number (match-string-no-properties 3))))
;;                   (save-excursion
;;                     ;; file and line-number are in different lines
;;                     (setq line (string-to-number (match-string-no-properties 1))
;;                           pos (point)
;;                           file (progn
;;                                  (when (and (re-search-backward "\\(^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^\t >]+\\)>?[ \t]+in[ \t]+\\([^ \t\n]+\\)" nil t 1)
;;                                             (not (save-match-data (string-match "<\\|^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>" (match-string-no-properties 1)))))
;;                                    (match-string-no-properties 1))))))
;;                 (if file
;;                     (when (string-match ".+\.pyc" file)
;;                       (setq file (substring file 0 -1)))
;;                   (error "%s of traceback" errwhere))
;;                 (if (and file line)
;;                     (if
;;                         (and (string= "<stdin>" file) (eq 1 line))
;;                         (error "%s of traceback" errwhere)
;;                       (py-jump-to-exception file line py-line-number-offset))
;;                   (error "%s of traceback" errwhere)))
;;             (goto-char orig)
;;             (error "%s of traceback" errwhere))))))

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
;;;

(defun py-postprocess-output-buffer (buf exception-buffer &optional line)
  "Highlight exceptions found in BUF.
If an exception occurred return error-string, otherwise return nil.  BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer "
  (let (line file bol err-p estring ecode limit)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (switch-to-buffer (current-buffer))
      (save-excursion
        (when (re-search-backward py-shell-prompt-regexp nil t 1)
          ;; not a useful message, delete it - please tell when thinking otherwise
          (and (re-search-forward "File \"<stdin>\", line 1,.*\n" nil t)
               (replace-match ""))
          (when (and (re-search-forward py-traceback-line-re limit t)
                     ;; (message "%s" (match-string-no-properties 0))
                     (or (match-string 1) (match-string 3)))
            (when (match-string-no-properties 1) (match-string-no-properties 2)
                  (replace-match exception-buffer nil nil nil 1)
                  (setq file exception-buffer)
                  (setq line (string-to-number (match-string-no-properties 2)))
                  (goto-char (match-beginning 0))
                  ;; if no buffer-file exists, signal "Buffer", not "File"
                  (save-match-data
                    (and (not (buffer-file-name (or
                                                 (get-buffer exception-buffer)
                                                 (get-buffer (file-name-nondirectory exception-buffer))))) (string-match "^[ \t]*File" (buffer-substring-no-properties (match-beginning 0) (match-end 0)))

                     (looking-at "[ \t]*File")
                     (replace-match "Buffer"))
                    ))
            (add-to-list 'err-p line)
            (add-to-list 'err-p file)
            (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                         'face 'highlight)
            ;; If not file exists, just a buffer, correct message

            (forward-line 1)
            (when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
              (setq estring (match-string-no-properties 1))
              (add-to-list 'err-p estring t)
              (setq ecode (buffer-substring-no-properties (line-end-position)
                                                          (progn (re-search-forward comint-prompt-regexp nil t 1)(match-beginning 0))))
              ;; (setq ecode (concat (split-string ecode "[ \n\t\f\r^]" t)))
              (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " ecode))
              (add-to-list 'err-p ecode t)))
          ;; (and py-verbose-p (message "%s" (nth 2 err-p)))
          err-p)))))

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

(defalias 'ipython-send-and-indent 'py-execute-line-ipython)
(defalias 'py-execute-region-in-shell 'py-execute-region)
(defalias 'py-ipython-shell-command-on-region 'py-execute-region-ipython)
(defalias 'py-shell-command-on-region 'py-execute-region)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)

(provide 'python-components-execute);
;; python-components-execute.el ends here
