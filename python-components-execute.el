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

(defun py-restore-window-configuration ()
  "Restore py-restore-window-configuration when completion is done resp. abandoned. "
  (let (val)
    (and (setq val (get-register py-windows-config-register))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
	 (jump-to-register py-windows-config-register))))

(defun py-shell-execute-string-now (string &optional shell buffer proc output-buffer)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* (wait
         (procbuf (or buffer (process-buffer proc) (progn (setq wait py-new-shell-delay) (py-shell nil nil shell))))
         (proc (or proc (get-buffer-process procbuf)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
         (outbuf (get-buffer-create (or output-buffer py-output-buffer))))
    ;; wait is used only when a new py-shell buffer was connected
    (and wait (sit-for wait))
    (unwind-protect
        (condition-case nil
            (progn
              (with-current-buffer outbuf
                (delete-region (point-min) (point-max)))
              (with-current-buffer procbuf
                ;; (sit-for 3)
                (comint-redirect-send-command-to-process
                 cmd outbuf proc nil t)
                (accept-process-output proc 5))
              (with-current-buffer outbuf
                (buffer-substring (point-min) (point-max))))
          (quit (with-current-buffer procbuf
                  (interrupt-process proc comint-ptyp)
                  (while (not comint-redirect-completed) ; wait for output
                    (accept-process-output proc 1)))
                (signal 'quit nil))))))

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun py-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name)))
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (py-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      file-name file-name)
     process)))

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
(defalias 'py-toggle-switch-buffers-on-execute 'py-toggle-shell-switch-buffers-on-execute)
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
  (let* ((ptn (or py-shell-name (py-choose-shell) "python"))
         (erg (if py-edit-only-p ptn (executable-find ptn))))
    (when (interactive-p)
      (if erg
          (message "%s" ptn)
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

(defun py-set-ipython-completion-command-string (shell)
  "Set and return `ipython-completion-command-string'. "
  (interactive)
  (let* ((ipython-version (shell-command-to-string (concat shell " -V"))))
    (if (string-match "[0-9]" ipython-version)
        (setq ipython-completion-command-string
              (cond ((string-match "^[^0].+" ipython-version)
		     ipython0.11-completion-command-string)
                    ((string-match "^0.1[1-3]" ipython-version)
                     ipython0.11-completion-command-string)
                    ((string= "^0.10" ipython-version)
                     ipython0.10-completion-command-string)))
      (error ipython-version))))

(defun py-ipython--module-completion-import (proc)
  "Import module-completion "
  (interactive)
  (let ((ipython-version (shell-command-to-string (concat py-shell-name " -V"))))
    (when (and (string-match "^[0-9]" ipython-version)
               (string-match "^[^0].+" ipython-version))
      (process-send-string proc "from IPython.core.completerlib import module_completion")
      (process-send-string proc "\n"))))

(defun py--compose-buffer-name-initials (liste)
  (let (erg)
    (dolist (ele liste)
      (unless (string= "" ele)
	(setq erg (concat erg (char-to-string (aref ele 0))))))
    erg))

(defun py--remove-home-directory-from-list (liste)
  "Prepare for compose-buffer-name-initials. "
  (let ((case-fold-search t)
	(liste liste)
	erg)
    (if (listp (setq erg (split-string (expand-file-name "~") "\/")))
	erg
      (setq erg (split-string (expand-file-name "~") "\\\\")))
     (while erg
      (when (member (car erg) liste)
	(setq liste (cdr (member (car erg) liste))))
      (setq erg (cdr erg)))
    (butlast liste)))

(defun py--buffer-name-prepare (&optional arg dedicated)
  "Return an appropriate name to display in modeline.
SEPCHAR is the file-path separator of your system. "
  (let* ((name-first (or arg py-shell-name))
	 (erg (when name-first (if (stringp name-first) name-first (prin1-to-string name-first))))
	 prefix suffix liste)
    ;; remove suffix
    (when (string-match "[.]" erg)
      (setq erg (substring erg 0 (string-match "[.]" erg))))
    ;; remove prefix
    (when (string-match "^py-" erg)
      (setq erg (nth 1 (split-string erg "-"))))
    ;; remove home-directory from prefix to display
    (unless py-modeline-acronym-display-home-p
      (save-match-data
	(let ((case-fold-search t))
	  (when (string-match (concat ".*" (expand-file-name "~")) erg)
	    (setq erg (replace-regexp-in-string (concat "^" (expand-file-name "~")) "" erg))))))
    (if (or (and (setq prefix (split-string erg "\\\\"))
		 (< 1 (length prefix)))
	    (and (setq prefix (split-string erg "\/"))
		 (< 1 (length prefix))))
	(progn
	  ;; exect something like default py-shell-name
	  (setq erg (car (last prefix)))
	  (unless py-modeline-acronym-display-home-p
	    ;; home-directory may still inside
	    (setq prefix (py--remove-home-directory-from-list prefix))
	    (setq prefix (py--compose-buffer-name-initials prefix))))
      (setq erg (or arg py-shell-name))
      (setq prefix nil))

    ;; (setq name (substring name (1+ (string-match "/[^/]+\\|\\\\[[:alnum:].]+$" name)))))
    (setq erg
          (cond ((string= "ipython" erg)
                 (replace-regexp-in-string "ipython" "IPython" erg))
                ((string= "jython" erg)
                 (replace-regexp-in-string "jython" "Jython" erg))
                ((string= "python" erg)
                 (replace-regexp-in-string "python" "Python" erg))
                ((string-match "python2" erg)
                 (replace-regexp-in-string "python2" "Python2" erg))
                ((string-match "python3" erg)
                 (replace-regexp-in-string "python3" "Python3" erg))
                (t erg)))
    (when (or dedicated py-dedicated-process-p)
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))
          (t (unless (string-match "^\*" erg)(setq erg (concat "*" erg "*")))))
    erg))

(defun py--jump-to-exception-intern (action exception-buffer origline)
  (let (erg)
    (set-buffer exception-buffer)
    (goto-char (point-min))
    (forward-line (1- origline))
    (push-mark)
    (and (search-forward action (line-end-position) t)
         (and py-verbose-p (message "Exception in file %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight)))))

;; Result: (nil 5 "print(34ed)" " SyntaxError: invalid token ")
(defun py--jump-to-exception (py-error origline &optional file)
  "Jump to the Python code in FILE at LINE."
  (let (
        ;; (inhibit-point-motion-hooks t)
        (file (or file (car py-error)))
        (line (cadr py-error))
        (action (nth 2 py-error))
        (errm (nth 3 py-error)))
    (cond ((and py-exception-buffer
                (buffer-live-p py-exception-buffer))
           ;; (pop-to-buffer procbuf)
           (py--jump-to-exception-intern action py-exception-buffer origline))
          ((ignore-errors (file-readable-p file))
           (find-file file)
           (py--jump-to-exception-intern action (get-buffer (file-name-nondirectory file origline))))
          ((buffer-live-p (get-buffer file))
           (set-buffer file)
           (py--jump-to-exception-intern action file origline))
          (t (setq file (find-file (read-file-name "Exception file: "
                                                   nil
                                                   file t)))
             (py--jump-to-exception-intern action file origline)))))

(defalias 'py-toggle-split-windows-on-execute-function 'py-toggle-split-windows-function)
(defun py-toggle-split-windows-function ()
  "If window is splitted vertically or horizontally.

When code is executed and `py-split-windows-on-execute-p' is `t', the result is displays in an output-buffer, \"\*Python\*\" by default.

Customizable variable `py-split-windows-on-execute-function' tells how to split the screen."
  (interactive)
  (if (eq 'split-window-vertically py-split-windows-on-execute-function)
      (setq py-split-windows-on-execute-function'split-window-horizontally)
    (setq py-split-windows-on-execute-function 'split-window-vertically))
  (when (and py-verbose-p (interactive-p))
    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function)))

(defun py--manage-windows-split ()
  "If one window, split according to `py-split-windows-on-execute-function.

Internal use"
  (and
   (one-window-p t)
   (funcall py-split-windows-on-execute-function)))

(defun py--manage-windows-set-and-switch (buffer)
  "Switch to output-buffer, go to point-max.

Internal use"
  (set-buffer buffer)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun py--shell-manage-windows (output-buffer &optional windows-displayed windows-config)
  "Adapt or restore window configuration. Return nil "
  (cond
   (py-keep-windows-configuration
    (py-restore-window-configuration))
   ((and py-split-windows-on-execute-p
	 py-switch-buffers-on-execute-p)
    (py-restore-window-configuration)
    (delete-other-windows)
    (py--manage-windows-split)
    (pop-to-buffer output-buffer)
    (display-buffer py-exception-buffer))
   ;; split, not switch
   ((and
     py-split-windows-on-execute-p
     (not py-switch-buffers-on-execute-p))
    (set-buffer oldbuf)
    (switch-to-buffer (current-buffer))
    (delete-other-windows)
    (py--manage-windows-split)
    (py--manage-windows-set-and-switch output-buffer)
    (display-buffer output-buffer t)
    ;; fast-... fails
    ;; 	 (unless (eq (current-buffer) py-exception-buffer)
    ;; 	   (set-buffer py-exception-buffer)
    ;; 	   (switch-to-buffer (current-buffer)))
    )
   ;; no split, switch
   ((and
     py-switch-buffers-on-execute-p
     (not py-split-windows-on-execute-p))
    (let (pop-up-windows)
      (py--manage-windows-set-and-switch output-buffer)))
   ;; no split, no switch
   ((not py-switch-buffers-on-execute-p)
    (let (pop-up-windows)
      (py-restore-window-configuration)))))

(defun py-kill-shell-unconditional (&optional shell)
  "With optional argument SHELL.

Otherwise kill default (I)Python shell.
Kill buffer and its process.
Receives a buffer-name as argument"
  (interactive)
  (let ((shell (or shell (py-shell))))
    (py-kill-buffer-unconditional shell)))

(defun py-kill-default-shell-unconditional ()
  "Kill buffer \"\*Python\*\" and its process. "
  (interactive)
  (py-kill-buffer-unconditional "*Python*"))

(defun py--report-executable (py-buffer-name)
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

(defun py--shell-make-comint (executable py-buffer-name args)
  (let ((buffer (apply 'make-comint-in-buffer executable py-buffer-name executable nil args)))
    (with-current-buffer buffer
      (py-shell-mode))
    (sit-for 0.1 t)
    buffer))

(defun py--shell-setup (buffer proc)
  (py--shell-send-setup-code proc)
  (and (string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
       (py-ipython--module-completion-import proc))
  (set-process-sentinel proc #'shell-write-history-on-exit))

(defun py--guess-buffer-name (argprompt)
  "Guess the buffer-name core string. "
  (cond
   ((and py-fast-process-p (not py-dedicated-process-p)) py-output-buffer)
   ;; (buffer-name)
   (t (and (not dedicated) argprompt
           (cond
            ((and (eq 2 (prefix-numeric-value argprompt))
                  (fboundp 'split-string))
             (setq args (split-string
                         (read-string "Py-Shell arguments: "
                                      (concat
                                       (mapconcat 'identity py-python-command-args " ") " "))))))))))

(defun py--configured-shell (name)
  "Return the configured PATH/TO/STRING if any. "
  (if (string-match "//\\|\\\\" name)
      name
    (cond ((string-match "^[Ii]" name)
	   py-ipython-command)
	  ((string-match "[Pp]ython3" name)
	   py-python3-command)
	  ((string-match "[Pp]ython2" name)
	   py-python2-command)
	  ((string-match "[Jj]ython" name)
	   py-jython-command)
	  (t py-python-command))))

(defun py--unfontify-banner (buffer)
  "Unfontify the shell banner-text.

Takes a buffer as argument. "
  (interactive)
  (when (ignore-errors (buffer-live-p buffer))
    (with-current-buffer buffer
      ;; (when py-debug-p (message "%s" (concat "py--unfontify-banner: " (buffer-name buffer))))
      ;; (sit-for 0.3 t)
      (let ((erg (and (boundp 'comint-last-prompt)(ignore-errors (car comint-last-prompt)))))
	(if erg
	    (font-lock-unfontify-region (point-min) erg)
	  (progn (and py-debug-p (message "%s" (concat "py--unfontify-banner: Don't see a prompt in buffer " (buffer-name buffer))))))))))

(defun py-shell (&optional argprompt dedicated shell buffer-name)
  "Start an interactive Python interpreter in another window.
  Interactively, \\[universal-argument] prompts for a PATH/TO/EXECUTABLE to use.
  \\[universal-argument] 2 prompts for `py-python-command-args'.
  If `default-directory' is a remote file name, it is also prompted
  to change if called with a prefix arg.

  Returns py-shell's buffer-name.
  Optional string PYSHELLNAME overrides default `py-shell-name'.
  BUFFER allows specifying a name, the Python process is connected to
  "
  (interactive "P")
  ;; done by py-shell-mode
  (let* ((newpath (when (eq 4 (prefix-numeric-value argprompt))
		    (read-shell-command "PATH/TO/EXECUTABLE/[I]python[version]: ")))
	 (oldbuf (current-buffer))
	 (dedicated (or dedicated py-dedicated-process-p))
	 (py-exception-buffer (or py-exception-buffer (and (or (eq 'major-mode 'python-mode)(eq 'major-mode 'py-shell-mode)) (current-buffer))))
	 (path (getenv "PYTHONPATH"))
	 (py-shell-name-raw (or newpath shell py-shell-name (py-choose-shell)))
	 (args
	  (cond (py-fast-process-p nil)
		((string-match "^[Ii]" py-shell-name-raw)
		 py-ipython-command-args)
		((string-match "^[^-]+3" py-shell-name-raw)
		 py-python3-command-args)
		(t py-python-command-args)))
	 ;; unless Path is given with `py-shell-name'
	 ;; call configured command
	 (py-shell-name (py--configured-shell py-shell-name-raw))

	 ;; If we use a pipe, Unicode characters are not printed
	 ;; correctly (Bug#5794) and IPython does not work at
	 ;; all (Bug#5390). python.el
	 ;; (process-connection-type t)
	 ;; already in py-choose-shell
	 (py-use-local-default
	  (if (not (string= "" py-shell-local-path))
	      (expand-file-name py-shell-local-path)
	    (when py-use-local-default
	      (error "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'"))))
	 (py-buffer-name (or buffer-name (py--guess-buffer-name argprompt)))
	 (py-buffer-name (or py-buffer-name (py--buffer-name-prepare newpath dedicated)))
	 (executable (cond (py-shell-name)
			   (py-buffer-name
			    (py--report-executable py-buffer-name))))
	 proc py-smart-indentation)
    ;; lp:1169687, if called from within an existing py-shell, open a new one
    (and (bufferp py-exception-buffer)(string= py-buffer-name (buffer-name py-exception-buffer))
	 (setq py-buffer-name (generate-new-buffer-name py-buffer-name)))
    (if (and py-fast-process-p
	     ;; user may want just to open a interactive shell
	     (not (interactive-p)))
	(unless (get-buffer-process (get-buffer (default-value 'py-buffer-name)))
	  (py-fast-process)
	  (setq py-output-buffer (default-value 'py-buffer-name)))
      (unless (comint-check-proc py-buffer-name)
	;; buffer might exist but not being empty
	(when (buffer-live-p py-buffer-name)
	  (with-current-buffer py-buffer-name
	    (erase-buffer)))
	(py--shell-make-comint executable py-buffer-name args)
	(sit-for 0.1 t)
	(setq py-output-buffer py-buffer-name)
	(if (comint-check-proc py-buffer-name)
	    (with-current-buffer py-buffer-name
	      (py--shell-setup py-buffer-name (get-buffer-process py-buffer-name)))
	  (error (concat "py-shell: No process in " py-buffer-name))))
      ;; (goto-char (point-max))
      (when (or (string-match "[BbIi]*[Pp]ython" (prin1-to-string this-command))(interactive-p)) (py--shell-manage-windows py-buffer-name))
      ;; (when py-shell-mode-hook (run-hooks 'py-shell-mode-hook))
      (when (string-match "[BbIi][Pp]ython" py-buffer-name)
	(sit-for 0.3 t))
      (sit-for 0.1 t)
      ;; (py--unfontify-banner (get-buffer py-buffer-name))
      )
    py-buffer-name))

(defun py-shell-get-process (&optional argprompt py-dedicated-process-p shell switch py-buffer-name)
  "Get appropriate Python process for current buffer and return it."
  (interactive)
  (let ((erg (get-buffer-process (py-shell argprompt py-dedicated-process-p shell py-buffer-name))))
    (when (interactive-p) (message "%S" erg))
    erg))

(defalias 'py-switch-to-python 'py-switch-to-shell)
(defun py-switch-to-shell ()
  "Switch to Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

;;; Code execution commands
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

(defun py--choose-buffer-name (&optional name)
  "Python code might be processed by an
- interactive Python shell (DEFAULT)
- non-interactive Python (py-fast-process-p), select for large
  output

Both processes might run in

- session, i.e. start from previous state (DEFAULT)
- dedicated, open or follow a separate line of execution

Default is interactive, i.e. py-fast-process-p nil, and `py-session'"

  (cond ((and py-fast-process-p py-dedicated-process-p)
	 (py--buffer-name-prepare (default-value 'py-output-buffer)))
        (py-fast-process-p py-output-buffer)
        (t (py--buffer-name-prepare name))))

(defun py--close-execution (tempbuf erg)
  (when py-cleanup-temporary
    (py-kill-buffer-unconditional tempbuf)
    (py-delete-temporary tempfile tempbuf))
  (and (not py-error) erg (or py-debug-p py-store-result-p) (unless (string= (car kill-ring) erg) (kill-new erg)))
  erg)

(defun py--execute-base (&optional start end shell filename proc file wholebuf)
  "Select the handler.

When optional FILE is `t', no temporary file is needed. "
  (let* ((oldbuf (current-buffer))
	 (start (or start (and (use-region-p) (region-beginning)) (point-min)))
         (end (or end (and (use-region-p) (region-end)) (point-max)))
	 (strg (if py-if-name-main-permission-p
		   (buffer-substring-no-properties start end)
		 (py--fix-if-name-main-permission (buffer-substring-no-properties start end))))
	 (strg (py--fix-start strg))
         (wholebuf (unless file (or wholebuf (and (eq (buffer-size) (- end start))))))
         (windows-config (window-configuration-to-register 313465889))
         (origline
          (save-restriction
            (widen)
            (count-lines
             (point-min)
             ;; count-lines doesn't honor current line when at BOL
             end)))
	 ;; argument SHELL might be a string like "python", "IPython" "python3", a symbol holding PATH/TO/EXECUTABLE or just a symbol like 'python3
         (which-shell
	  (if shell (progn
		      (or (stringp shell) shell)
		      (ignore-errors (eval shell))
		      (and (symbolp shell) (prin1-to-string shell)))
	    (py-choose-shell)))
         (py-exception-buffer (current-buffer))
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
         (py-buffer-name
	  (or py-buffer-name
	      (and py-fast-process-p (default-value 'py-output-buffer))
	      (py--choose-buffer-name which-shell)))
         (filename (or (and filename (expand-file-name filename)) (and (not (buffer-modified-p)) (buffer-file-name))))
         (py-orig-buffer-or-file (or filename (current-buffer)))
         (proc (cond (proc)
		     ;; will deal with py-dedicated-process-p also
                     (py-fast-process-p (py-fast-process py-buffer-name))
                     (py-dedicated-process-p
                      (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name)))
                     (t (or (get-buffer-process py-buffer-name)
                            (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name))))))
	 output-buffer erg)
    (setq py-error nil)
    (when py-debug-p
      (with-temp-file "/tmp/py-buffer-name.txt" (insert py-buffer-name)))
    (set-buffer py-exception-buffer)
    (py--update-execute-directory proc py-buffer-name execute-directory)
    (cond (py-fast-process-p
	   (with-current-buffer (setq output-buffer (process-buffer proc))
	     ;; (switch-to-buffer (current-buffer)) 
	     (sit-for 1 t)
	     (erase-buffer)
	     (setq erg
		   (py--fast-send-string-intern strg
						proc
						output-buffer))
	     (sit-for 0.1)
	     (py--shell-manage-windows output-buffer)))
	  ;; enforce proceeding as python-mode.el v5
	  (python-mode-v5-behavior-p
	   (py-execute-python-mode-v5 start end))
	  (py-execute-no-temp-p
	   (py--execute-ge24.3 start end filename execute-directory which-shell py-exception-buffer proc))
	  ((and filename wholebuf)
	   ;; No temporary file than
	   (let (py-cleanup-temporary)
	     (py--execute-file-base proc filename nil py-buffer-name filename execute-directory)
	     (py--close-execution tempbuf erg)
	     (py--shell-manage-windows py-buffer-name)))
	  (t (py--execute-buffer-finally strg execute-directory wholebuf which-shell proc)))))

(defun py--execute-buffer-finally (strg execute-directory wholebuf which-shell proc)
  (let* ((temp (make-temp-name
		;; FixMe: that should be simpler
                (concat (replace-regexp-in-string py-separator-char "-" (replace-regexp-in-string (concat "^" py-separator-char) "" (replace-regexp-in-string ":" "-" (if (stringp which-shell) which-shell (prin1-to-string which-shell))))) "-")))
         (tempfile (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" temp) ".py"))
         (tempbuf (get-buffer-create temp)))
    (with-current-buffer tempbuf
      (insert strg)
      (write-file tempfile))
    (unwind-protect
	(setq erg (py--execute-file-base proc tempfile nil py-buffer-name py-orig-buffer-or-file execute-directory)))
    (sit-for 0.1 t)
    (py--close-execution tempbuf erg)
    (py--shell-manage-windows py-buffer-name)))

(defun py--fetch-comint-result ()
  (save-excursion
    (let (erg)
      (or (and
	   (boundp 'comint-last-prompt)
	   (goto-char (setq erg (car comint-last-prompt))))
	  (goto-char (setq erg (point-max))))
      (re-search-backward py-fast-filter-re nil t 1)
      (goto-char (match-end 0))
      (buffer-substring-no-properties (point) erg))))

(defun py--postprocess (buffer)
  "Provide return values, check result for error, manage windows. "
  ;; py--fast-send-string doesn't set origline
  (with-current-buffer buffer
    (setq py-error (py--postprocess-intern buffer))
    (when py-store-result-p
      (setq erg
	    (if (eq major-mode 'py-shell-mode)
		(py-output-filter (py--fetch-comint-result))
	      (py-output-filter (buffer-substring-no-properties (point) (point-max)))))
      (and erg (not (string= (car kill-ring) erg)) (kill-new erg)))
    ;; (run-with-idle-timer 1 nil 'py--unfontify-banner)
    ;; xpco(py--unfontify-banner (current-buffer))
    )
  erg)

(defun py--execute-file-base (&optional proc filename cmd procbuf origfile execute-directory)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing.
Returns position where output starts. "
  (let* ((cmd (or cmd (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename)))
         (msg (and py-verbose-p (format "## executing %s...\n" (or origfile filename))))
         (buffer (or procbuf (py-shell nil nil nil procbuf)))
         (proc (or proc (get-buffer-process buffer)))
         erg orig)
    (with-current-buffer buffer
      (goto-char (point-max))
      (setq orig (point))
      (comint-send-string proc cmd)
      (setq erg (py--postprocess buffer))
      (message "%s" py-error)
      erg)))

(defun py--execute-ge24.3 (start end filename execute-directory which-shell &optional py-exception-buffer proc)
  "An alternative way to do it.

May we get rid of the temporary file? "
  (and (buffer-file-name) buffer-offer-save (buffer-modified-p) (y-or-n-p "Save buffer before executing? ")
       (write-file (buffer-file-name)))
  (let* ((start (copy-marker start))
         (end (copy-marker end))
         (py-exception-buffer (or py-exception-buffer (current-buffer)))
         (line (count-lines (point-min) (if (eq start (line-beginning-position)) (1+ start) start)))
         (strg (buffer-substring-no-properties start end))
         (tempfile (or (buffer-file-name) (concat (expand-file-name py-temp-directory) py-separator-char (replace-regexp-in-string py-separator-char "-" "temp") ".py")))

         (proc (or proc (if py-dedicated-process-p
                            (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name))
                          (or (get-buffer-process py-buffer-name)
                              (get-buffer-process (py-shell nil py-dedicated-process-p which-shell py-buffer-name))))))
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
      ;; (message "Warning: options `execute-directory' and `py-use-current-dir-when-execute-p' may conflict"))
      (and execute-directory
           (process-send-string proc (concat "import os; os.chdir(\"" execute-directory "\")\n"))
           ))
    (set-buffer filebuf)
    (process-send-string proc
                         (buffer-substring-no-properties
                          (point-min) (point-max)))
    (sit-for 0.1 t)
    (if (and (setq py-error (save-excursion (py--postprocess-intern procbuf origline)))
             (car py-error)
             (not (markerp py-error)))
        (py--jump-to-exception py-error origline)
      (py--shell-manage-windows procbuf py-buffer-name)
      (unless (string= (buffer-name (current-buffer)) (buffer-name procbuf))
        (when py-verbose-p (message "Output buffer: %s" procbuf))))))

(defalias 'py-send-region 'py-execute-region)
;;; execute region
(defun py-execute-region (start end &optional shell dedicated)
  "Send the region to a Python interpreter.

When called with \\[universal-argument], execution through
`default-value' of `py-shell-name' is forced.

When called with \\[universal-argument] followed by a number
different from 4 and 1, user is prompted to specify a shell. This
might be the name of a system-wide shell or include the path to a
virtual environment.

When called from a programm, it accepts a string specifying a
shell which will be forced upon execute as argument.

Optional DEDICATED "
  (interactive "r\nP")
  (save-excursion
    (let ((orig (point))
	  (py-shell-name (cond ((or py-force-py-shell-name-p (eq 4 (prefix-numeric-value shell))) (default-value 'py-shell-name))
			       ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
				(read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
			       (t (or shell py-shell-name))))
	  (py-dedicated-process-p (or dedicated py-dedicated-process-p)))
      (py--execute-base start end))))

(defun py-execute-region-default (start end)
  "Send the region to the systems default Python interpreter. "
  (interactive "r")
  (save-excursion
    (let ((py-dedicated-process-p (default-value 'py-dedicated-process-p))
	  (py-shell-name (default-value 'py-shell-name)))
      (py--execute-base start end))))

(defun py-execute-region-no-switch (start end)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', buffer with region stays current.
 "
  (interactive "r")
  (let (py-switch-buffers-on-execute-p)
    (py--execute-base start end)))

(defun py-execute-region-dedicated (start end &optional shell)
  "Get the region processed by an unique Python interpreter.

When called with \\[universal-argument], execution through
`default-value' of `py-shell-name' is forced.

When called with \\[universal-argument] followed by a number
different from 4 and 1, user is prompted to specify a shell. This
might be the name of a system-wide shell or include the path to a
virtual environment.

When called from a programm, it accepts a string specifying a
shell which will be forced upon execute as argument. "

  (interactive "r\nP")
  (save-excursion
    (let ((py-shell-name (cond ((eq 4 (prefix-numeric-value shell)) (default-value 'py-shell-name))
			       ((and (numberp shell) (not (eq 1 (prefix-numeric-value shell))))
				(read-from-minibuffer "(path-to-)shell-name: " (default-value 'py-shell-name)))
			       (t shell)))
	  (py-dedicated-process-p t))
      (py--execute-base start end))))

(defun py-execute-region-switch (start end)
  "Send the region to a Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to.
"
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defalias 'py-execute-region-dedicated-default 'py-execute-region-default-dedicated)
(defun py-execute-region-default-dedicated (start end)
  "Send the region to an unique shell of systems default Python. "
  (interactive "r")
  (save-excursion
    (let ((py-dedicated-process-p t))
      (py--execute-base start end (default-value 'py-shell-name)))))

(defun py-delete-temporary (&optional file filebuf)
  (when (file-readable-p file)
    (delete-file file))
  (when (buffer-live-p filebuf)
    (set-buffer filebuf)
    (set-buffer-modified-p 'nil)
    (kill-buffer filebuf)))

(defun py-execute-python-mode-v5 (start end)
  (interactive "r")
  (let ((py-exception-buffer (current-buffer))
        (pcmd (concat py-shell-name (if (string-equal py-which-bufname
                                                      "Jython")
                                        " -"
                                      ;; " -c "
                                      ""))))
    (save-excursion
      (shell-command-on-region start end
                               pcmd py-output-buffer))
    (if (not (get-buffer py-output-buffer))
        (message "No output.")
      (setq py-error (py--postprocess-intern py-output-buffer origline))
      (let* ((line (cadr py-error)))
        (if py-error
            (when (and py-jump-on-exception line)
              (pop-to-buffer py-exception-buffer))
          (pop-to-buffer py-output-buffer)
          (goto-char (point-max))
          (copy-marker (point)))))))

(defun py--insert-offset-lines (line)
  "Fix offline amount, make error point at the corect line. "
  (insert (make-string (- line (count-lines (point-min) (point))) 10)))

(defun py-execute-file (filename)
  "When called interactively, user is prompted for filename. "
  (interactive "fFilename: ")
  (let (;; postprocess-output-buffer might want origline
        (origline 1)
        (windows-config (window-configuration-to-register 313465889))
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base nil (expand-file-name filename)))
          (py--execute-file-base nil (expand-file-name filename)))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
    erg))

(defun py--current-working-directory (&optional shell)
  "Return the directory of current `py-shell'."
  (replace-regexp-in-string "\n" "" (shell-command-to-string (concat (or shell py-shell-name) " -c \"import os; print(os.getcwd())\""))))

(defun py--update-execute-directory-intern (dir proc)
  (comint-send-string proc (concat "import os;os.chdir(\"" dir "\")\n")))

(defun py--update-execute-directory (proc procbuf execute-directory)
  (let ((oldbuf (current-buffer))
        orig cwd)
    (set-buffer procbuf)
    (setq cwd (py--current-working-directory))
    (setq orig (point))
    (unless (string= execute-directory (concat cwd "/"))
      (py--update-execute-directory-intern (or py-execute-directory execute-directory) proc)
      (delete-region orig (point-max)))
    (set-buffer oldbuf)))

(defun py-execute-string (&optional string shell)
  "Send the argument STRING to a Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell))))

(defun py-execute-string-dedicated (&optional string shell)
  "Send the argument STRING to an unique Python interpreter.

See also `py-execute-region'. "
  (interactive)
  (let ((string (or string (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name)))
        (py-dedicated-process-p t))
    (with-temp-buffer
      (insert string)
      (py-execute-region (point-min) (point-max) shell))))

(defun py--insert-execute-directory (directory &optional orig done)
  (let ((orig (or orig (point)))
        (done done))
    (if done (goto-char done) (goto-char (point-min)))
    (cond ((re-search-forward "^from __future__ import " nil t 1)
           (py-end-of-statement)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-encoding-string-re nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          ((re-search-forward py-shebang-regexp nil t 1)
           (setq done (point))
           (py--insert-execute-directory directory orig done))
          (t (forward-line 1)
             (unless  ;; (empty-line-p)
                 (eq 9 (char-after)) (newline))
             (insert (concat "import os; os.chdir(\"" directory "\")\n"))))))

(defun py--fix-if-name-main-permission (string)
  "Remove \"if __name__ == '__main__ '\" from code to execute.

See `py-if-name-main-permission-p'"
  (let ((strg (if py-if-name-main-permission-p string
		(replace-regexp-in-string
		 "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
		 ;; space after __main__, i.e. will not be executed
		 "if __name__ == '__main__ ':" string))))
    strg))

(defun py--fix-start-intern (start end)
  (goto-char start)
  (while
      (member (char-after) (list 9 32))
    (delete-char 1))
  (unless (py--beginning-of-statement-p)
    (py-down-statement))
  (while (not (eq (current-indentation) 0))
    (py-shift-left py-indent-offset start end))
  (goto-char (point-max))
  (unless (empty-line-p)
    (newline)))

(defun py--fix-start (string)
  "Internal use by py-execute... functions.

Avoid empty lines at the beginning. "
  (with-temp-buffer
    (insert string)
    (py--fix-start-intern (point-min) (point-max))
    ;; FixMe: Maybe conditial from from some use-tempfile var?
    (and (ignore-errors tempfile)
	 (write-region (point-min) (point-max) tempfile nil t nil 'ask))
    (buffer-substring-no-properties (point-min) (point-max))))

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

(defun py-execute-import-or-reload (&optional argprompt shell)
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
  (when py-master-file
    (let* ((filename (expand-file-name py-master-file))
           (buffer (or (get-file-buffer filename)
                       (find-file-noselect filename))))
      (set-buffer buffer)))
  (let ((py-shell-name (or shell (py-choose-shell argprompt shell)))
        (file (buffer-file-name (current-buffer))))
    (if file
        (let ((proc (or
                     (ignore-errors (get-process (file-name-directory shell)))
                     (get-buffer-process (py-shell argprompt py-dedicated-process-p shell (or shell (default-value 'py-shell-name)))))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
          (py--execute-file-base proc file
                                (if (string-match "\\.py$" file)
                                    (let ((m (py--qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" (file-name-nondirectory shell))
                                          (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n" m m m)
                                        (format "import sys,imp\nif'%s' in sys.modules:\n imp.reload(%s)\nelse:\n import %s\n" m m m)))
                                  ;; (format "execfile(r'%s')\n" file)
                                  (py-which-execute-file-command file))))
      (py-execute-buffer))))

(defun py--qualified-module-name (file)
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
(defun py-execute-buffer ()
  "Send the contents of the buffer to a Python interpreter. "
  (interactive)
  (let ((origline 1))
    (and py-prompt-on-changed-p (buffer-file-name) (interactive-p) (buffer-modified-p)
         (y-or-n-p "Buffer changed, save first? ")
         (write-file (buffer-file-name)))
    (py-execute-region (point-min) (point-max))))

(defun py--execute-buffer-base ()
  "Honor `py-master-file'. "
  (let* ((py-master-file (or py-master-file (py-fetch-py-master-file)))
         (file
          (if py-master-file
              (expand-file-name py-master-file)
            (buffer-file-name))))
    (if file
	(py-execute-file file)
      (py-execute-region (point-min) (point-max)))))

(defun py-execute-buffer-dedicated ()
  "Send the contents of the buffer to a unique Python interpreter. "
  (interactive)
  (let ((py-dedicated-process-p t))
    (py--execute-buffer-base)))

(defun py-execute-buffer-switch ()
  "Send the contents of the buffer to a Python interpreter and switches to output. "
  (interactive)
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-buffer-base)))

(defun py-execute-buffer-no-switch ()
  "Send the contents of the buffer to a Python interpreter but don't switch to output. "
  (interactive)
  (let (py-switch-buffers-on-execute-p)
    (py--execute-buffer-base)))

(defalias 'py-execute-buffer-switch-dedicated 'py-execute-buffer-dedicated-switch)
(defun py-execute-buffer-dedicated-switch ()
  "Send the contents of the buffer to an unique Python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p'. "
  (interactive)
  (let ((py-dedicated-process-p t)
        (py-switch-buffers-on-execute-p t))
    (py--execute-buffer-base)))

;;; Specifying shells start
(defun py-execute-region-python (start end)
  "Send the region to a common shell calling the python interpreter. "
  (interactive "r")
  (let ((py-shell-name "python"))
    (py--execute-base start end)))

(defun py-execute-region-python-switch (start end)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-base start end "python")))

(defun py-execute-region-python-no-switch (beg end)
  "Send region at point to Python interpreter.

Keep current buffer. Ignores `py-switch-buffers-on-execute-p' "
  (interactive "r")
  (let ((py-shell-name "python")
        py-switch-buffers-on-execute-p)
    (py--execute-base beg end)))

(defun py-execute-region-python2 (start end)
  "Send the region to a common shell calling the python2 interpreter. "
  (interactive "r")
  (let ((py-shell-name "python2"))
    (py--execute-base start end)))

(defun py-execute-region-python2-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.
Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "python2")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-python2-no-switch (start end)
  "Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let (py-switch-buffers-on-execute-p)
    (py--execute-base start end "python2")))

(defun py-execute-region-python2.7 (start end)
  "Send the region to a common shell calling the python2.7 interpreter. "
  (interactive "r")
  (let ((py-shell-name "python2.7"))
    (py--execute-base start end)))

(defun py-execute-region-python2.7-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "python2.7")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-python2.7-no-switch (start end)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-name "python2.7")
        (py-switch-buffers-on-execute-p))
    (py--execute-base start end)))

(defun py-execute-region-python3 (start end)
  "Send the region to a common shell calling the python3 interpreter. "
  (interactive "r")
  (let ((py-shell-name "python3"))
    (py--execute-base start end)))

(defun py-execute-region-python3-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "python3")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-python3-no-switch (start end)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-name "python3")
        (py-switch-buffers-on-execute-p))
    (py--execute-base start end)))

(defun py-execute-region-python3.2 (start end)
  "Send the region to a common shell calling the python3.2 interpreter. "
  (interactive "r")
  (let ((py-shell-name "python3.2"))
    (py--execute-base start end)))

(defun py-execute-region-python3.2-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "python3.2")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-python3.2-no-switch (start end)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-name "python3.2")
        (py-switch-buffers-on-execute-p))
    (py--execute-base start end)))

(defun py-execute-region-ipython (start end)
  "Send the region to a common shell calling the ipython interpreter. "
  (interactive "r")
  (let ((py-shell-name "ipython"))
    (py--execute-base start end)))

(defun py-execute-region-ipython-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "ipython")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-ipython-no-switch (start end)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-name "ipython")
        (py-switch-buffers-on-execute-p))
    (py--execute-base start end)))

(defun py-execute-region-jython (start end)
  "Send the region to a common shell calling the jython interpreter. "
  (interactive "r")
  (let ((py-shell-name "jython"))
    (py--execute-base start end)))

(defun py-execute-region-jython-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will being switched to. "
  (interactive "r")
  (let ((py-shell-name "jython")
        (py-switch-buffers-on-execute-p t))
    (py--execute-base start end)))

(defun py-execute-region-jython-no-switch (start end)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-switch-buffers-on-execute-p', output-buffer will not being switched to."
  (interactive "r")
  (let ((py-shell-name "jython")
        (py-switch-buffers-on-execute-p))
    (py--execute-base start end)))

;; Specifying shells end

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the Python process."
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
        (pcmd (py-choose-shell)))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat pcmd " " filename) output-buffer error-buffer)
    (when (interactive-p) (switch-to-buffer output-buffer))))

;;;
(defun py-execute-line ()
  "Send current line from beginning of indent to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (progn (back-to-indentation)
                      (point))))
      (py-execute-region beg (line-end-position)))))

;;; Subprocess utilities and filters
(defvar py-last-exeption-buffer nil
  "Internal use only - when `py-up-exception' is called in
  source-buffer, this will deliver the exception-buffer again. ")

(defun py-remove-overlays-at-point ()
  "Remove overlays as set when `py-highlight-error-source-p' is non-nil. "
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
           (py--jump-to-exception (car info) origline (cdr info)))))))

(defun py-goto-exception (&optional file line)
  "Go to the line indicated by the traceback."
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
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py--point start))
      (if (funcall searchdir py-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (py-goto-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
        (py--find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py--find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if top
        (py--find-next-exception 'bob buffer 're-search-forward "Top")
      (py--find-next-exception 'bol buffer 're-search-backward "Top"))))
;;;

(defun py--postprocess-intern (buf &optional origline)
  "Highlight exceptions found in BUF.
If an exception occurred return error-string, otherwise return nil.  BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer "
  (let* ((pmx (copy-marker (point-max)))
	 file bol estring ecode limit erg)
    (goto-char pmx)
    (sit-for 0.1 t)
    (save-excursion
      (unless (looking-back py-pdbtrack-input-prompt)
        (forward-line -1)
        (end-of-line)
        (when (or (re-search-backward py-shell-prompt-regexp nil t 1)
                  (re-search-backward (concat ipython-de-input-prompt-regexp "\\|" ipython-de-output-prompt-regexp) nil t 1))
          (save-excursion
            (when (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
              (setq erg (copy-marker (point)))
              (delete-region (progn (beginning-of-line)
				    (save-match-data
				      (when (looking-at
					     ;; all prompt-regexp known
					     py-fast-filter-re)
					(goto-char (match-end 0))))

				    (skip-chars-forward " \t\r\n\f")(point)) (line-end-position))
	      (insert (concat "    File " (buffer-name py-exception-buffer) ", line "
			      (prin1-to-string origline)))))
	  ;; Delete links at temporary files created by py--execute-buffer-finally
	  ;; these are let-bound as `tempbuf'
	  (and (boundp 'tempbuf)
	       ;; (message "%s" tempbuf)
	       (search-forward (buffer-name tempbuf) nil t)
	       (delete-region (line-beginning-position) (1+ (line-end-position))))
          ;; if no buffer-file exists, signal "Buffer", not "File(when
          (when erg
            (goto-char erg)
            ;; (forward-char -1)
            ;; (skip-chars-backward "^\t\r\n\f")
            ;; (skip-chars-forward " \t")
            (save-match-data
              (and (not (buffer-file-name
                         (or
                          (get-buffer py-exception-buffer)
                          (get-buffer (file-name-nondirectory py-exception-buffer)))))
		   (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
		   (looking-at "[ \t]*File")
		   (replace-match " Buffer")))
            (add-to-list 'py-error origline)
            (add-to-list 'py-error (buffer-name py-exception-buffer))

	    ;; (put-text-property (line-beginning-position) (line-end-position) 'font-lock-face 'comint-error)
            ;; (put-text-property (line-beginning-position) (line-end-position) 'font-lock-face 'comint-highlight-prompt)
	    ;; (overlay-put (make-overlay (line-beginning-position)
	    ;; (1- (line-end-position)))
	    ;; 'face 'highlight)

            ;; If not file exists, just a buffer, correct message
            (forward-line 1)
            (when (looking-at "[ \t]*\\([^\t\n\r\f]+\\)[ \t]*$")
              (setq estring (match-string-no-properties 1))
              ;; (setq ecode (buffer-substring-no-properties (line-end-position)
              ;; (progn (re-search-forward comint-prompt-regexp nil t 1)(match-beginning 0))))
              (setq ecode (replace-regexp-in-string "[ \n\t\f\r^]+" " " estring))
              (add-to-list 'py-error ecode t))))))
    ;;))
    py-error))

(defun py--find-next-exception-prepare (direction start)
  "Setup exception regexps depending from kind of Python shell. "
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
(defalias 'py-shell-command-on-region 'py-execute-region)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)

(provide 'python-components-execute);
;; python-components-execute.el ends here
