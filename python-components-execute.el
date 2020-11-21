;;; python-components-execute.el --- Part of python-components-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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
(defun py-restore-window-configuration ()
  "Restore ‘py-restore-window-configuration’ when completion is done resp. abandoned."
  (let (val)
    (and (setq val (get-register py-windows-config-register))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
	 (jump-to-register py-windows-config-register))))

(defun py-shell-execute-string-now (strg &optional shell buffer proc)
  "Send STRG to Python interpreter process.

Return collected output

Optional SHELL BUFFER PROC"
  (let* (wait
         (procbuf (or buffer (process-buffer proc) (progn (setq wait py-new-shell-delay) (py-shell nil nil shell))))
         (proc (or proc (get-buffer-process procbuf)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string strg "\n") "\\n")))
	 ;; TBD remove redundant outbuf
         (outbuf procbuf))
    ;; wait is used only when a new py-shell buffer was connected
    (and wait (sit-for wait t))
    (unwind-protect
        (condition-case nil
            (progn
              (with-current-buffer outbuf
                (delete-region (point-min) (point-max)))
              (with-current-buffer procbuf
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

With EOB-P, go to end of buffer."
  (interactive "p")
  (pop-to-buffer (process-buffer (py-proc)) t) ;Runs python if needed.
  (when eob-p
    (goto-char (point-max))))

(defun toggle-force-local-shell (&optional arg)
  "If locally indicated Python shell should be taken.

Enforced upon sessions execute commands.

Toggles boolean ‘py-force-local-shell-p’ along with ‘py-force-py-shell-name-p’
Returns value of ‘toggle-force-local-shell’ switched to.

See also commands
‘py-force-local-shell-on’
‘py-force-local-shell-off’"
  (interactive)
  (let ((arg (or arg (if py-force-local-shell-p -1 1))))
    (if (< 0 arg)
        (progn
          (setq py-shell-name (or py-local-command (py-choose-shell)))
          (setq py-force-local-shell-p t))
      (setq py-shell-name (default-value 'py-shell-name))
      (setq py-force-local-shell-p nil))
    (when (called-interactively-p 'any)
      (if py-force-local-shell-p
          (when py-verbose-p (message "Enforce %s"  py-shell-name))
        (when py-verbose-p (message "py-shell-name default restored to: %s" py-shell-name))))
    py-shell-name))

(defun py-force-local-shell-on ()
  "Make sure, ‘py-force-local-shell-p’ is on.

Returns value of ‘py-force-local-shell-p’.
Optional FAST
Kind of an option 'follow', local shell sets ‘py-shell-name’, enforces its use afterwards"
  (interactive)
  (toggle-force-local-shell 1)
  (when (or py-verbose-p (called-interactively-p 'any))
    (message "Enforce %s" py-shell-name)))

(defun py-force-local-shell-off ()
  "Restore ‘py-shell-name’ default value and ‘behaviour’.

Optional FAST"
  (interactive)
  (toggle-force-local-shell 1)
  (when (or py-verbose-p (called-interactively-p 'any))
    (message "py-shell-name default restored to: %s" py-shell-name)))

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

(defun py-ipython--which-version (shell)
  "Returns IPython version as string"
  (shell-command-to-string (concat (downcase (replace-regexp-in-string  "[[:punct:]+]" "" shell)) " -V")))

(defun py-set-ipython-completion-command-string (shell)
  "Set and return ‘py-ipython-completion-command-string’ according to SHELL."
  (interactive)
  (let* ((ipython-version (py-ipython--which-version shell)))
    (if (string-match "[0-9]" ipython-version)
        (setq py-ipython-completion-command-string
              (cond ((string-match "^[^0].+" ipython-version)
		     py-ipython0.11-completion-command-string)
                    ((string-match "^0.1[1-3]" ipython-version)
                     py-ipython0.11-completion-command-string)
                    ((string= "^0.10" ipython-version)
                     py-ipython0.10-completion-command-string)))
      (error ipython-version))))

(defun py-ipython--module-completion-import (proc)
  "Import module-completion according to PROC."
  (interactive)
  (let ((ipython-version (shell-command-to-string (concat py-shell-name " -V"))))
    (when (and (string-match "^[0-9]" ipython-version)
               (string-match "^[^0].+" ipython-version))
      (process-send-string proc "from IPython.core.completerlib import module_completion"))))

(defun py--compose-buffer-name-initials (liste)
  (let (erg)
    (dolist (ele liste)
      (unless (string= "" ele)
	(setq erg (concat erg (char-to-string (aref ele 0))))))
    erg))

(defun py--remove-home-directory-from-list (liste)
  "Prepare for compose-buffer-name-initials according to LISTE."
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

(defun py--prepare-shell-name (erg)
  "Provide a readable shell name by capitalizing etc."
  (cond ((string-match "^ipython" erg)
	 (replace-regexp-in-string "ipython" "IPython" erg))
	((string-match "^jython" erg)
	 (replace-regexp-in-string "jython" "Jython" erg))
	((string-match "^python" erg)
	 (replace-regexp-in-string "python" "Python" erg))
	((string-match "^python2" erg)
	 (replace-regexp-in-string "python2" "Python2" erg))
	((string-match "^python3" erg)
	 (replace-regexp-in-string "python3" "Python3" erg))
	((string-match "^pypy" erg)
	 (replace-regexp-in-string "pypy" "PyPy" erg))
	(t erg)))

(defun py--choose-buffer-name (&optional name dedicated fast-process)
  "Return an appropriate NAME to display in modeline.

Optional DEDICATED FAST-PROCESS
SEPCHAR is the file-path separator of your system."
  (let* ((name-first (or name py-shell-name))
	 (erg (when name-first (if (stringp name-first) name-first (prin1-to-string name-first))))
	 (fast-process (or fast-process py-fast-process-p))
	 prefix)
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
      (setq erg (or erg py-shell-name))
      (setq prefix nil))
    (when fast-process (setq erg (concat erg " Fast")))
    (setq erg
          (py--prepare-shell-name erg))
    (when (or dedicated py-dedicated-process-p)
      (setq erg (make-temp-name (concat erg "-"))))
    (cond ((and prefix (string-match "^\*" erg))
           (setq erg (replace-regexp-in-string "^\*" (concat "*" prefix " ") erg)))
          (prefix
           (setq erg (concat "*" prefix " " erg "*")))
          (t (unless (string-match "^\*" erg) (setq erg (concat "*" erg "*")))))
    erg))

(defun py--jump-to-exception-intern (act exception-buffer origline)
  (let (erg)
    (set-buffer exception-buffer)
    (goto-char (point-min))
    (forward-line (1- origline))
    (and (search-forward act (line-end-position) t)
         (and py-verbose-p (message "exception-buffer: %s on line %d" py-exception-buffer origline))
         (and py-highlight-error-source-p
              (setq erg (make-overlay (match-beginning 0) (match-end 0)))
              (overlay-put erg
                           'face 'highlight)))))

(defun py--jump-to-exception (perr origline &optional file)
  "Jump to the PERR Python code at ORIGLINE in optional FILE."
  (let (
        (inhibit-point-motion-hooks t)
        (file (or file (car perr)))
        (act (nth 2 perr)))
    (cond ((and py-exception-buffer
                (buffer-live-p py-exception-buffer))
           ;; (pop-to-buffer procbuf)
           (py--jump-to-exception-intern act py-exception-buffer origline))
          ((ignore-errors (file-readable-p file))
           (find-file file)
           (py--jump-to-exception-intern act (get-buffer (file-name-nondirectory file)) origline))
          ((buffer-live-p (get-buffer file))
           (set-buffer file)
           (py--jump-to-exception-intern act file origline))
          (t (setq file (find-file (read-file-name "Exception file: "
                                                   nil
                                                   file t)))
             (py--jump-to-exception-intern act file origline)))))

(defalias 'py-toggle-split-window-on-execute-function 'py-toggle-split-window-function)
(defun py-toggle-split-window-function ()
  "If window is splitted vertically or horizontally.

When code is executed and ‘py-split-window-on-execute’ is t,
the result is displays in an output-buffer, \"\*Python\*\" by default.

Customizable variable ‘py-split-windows-on-execute-function’
tells how to split the screen."
  (interactive)
  (if (eq 'split-window-vertically py-split-windows-on-execute-function)
      (setq py-split-windows-on-execute-function'split-window-horizontally)
    (setq py-split-windows-on-execute-function 'split-window-vertically))
  (when (and py-verbose-p (called-interactively-p 'any))
    (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function)))

(defun py--manage-windows-set-and-switch (buffer)
  "Switch to output BUFFER, go to ‘point-max’.

Internal use"
  (set-buffer buffer)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun py--alternative-split-windows-on-execute-function ()
  "If ‘py--split-windows-on-execute-function’ is ‘split-window-vertically’ return ‘split-window-horizontally’ and vice versa."
  (if (eq py-split-windows-on-execute-function 'split-window-vertically)
      'split-window-horizontally
    'split-window-vertically))

(defun py--get-splittable-window ()
  "If selected window doesn't permit a further split, search ‘window-list’ for a suitable one."
  (or (and (window-left-child)(split-window (window-left-child)))
      (and (window-top-child)(split-window (window-top-child)))
      (and (window-parent)(ignore-errors (split-window (window-parent))))
      (and (window-atom-root)(split-window (window-atom-root)))))

(defun py--manage-windows-split (buffer)
  "If one window, split BUFFER.

according to ‘py-split-windows-on-execute-function’."
  (interactive)
  (set-buffer buffer)
  (or
   ;; (split-window (selected-window) nil ’below)
   (ignore-errors (funcall py-split-windows-on-execute-function))
   ;; If call didn't succeed according to settings of
   ;; ‘split-height-threshold’, ‘split-width-threshold’
   ;; resp. ‘window-min-height’, ‘window-min-width’
   ;; try alternative split
   (unless (ignore-errors (funcall (py--alternative-split-windows-on-execute-function)))
     ;; if alternative split fails, look for larger window
     (py--get-splittable-window)
     (ignore-errors (funcall (py--alternative-split-windows-on-execute-function))))))

;; (defun py--display-windows (output-buffer)
;;     "Otherwise new window appears above"
;;       (display-buffer output-buffer)
;;       (select-window py-exception-window))

(defun py--split-t-not-switch-wm (output-buffer number-of-windows exception-buffer)
  (unless (window-live-p output-buffer)
    (with-current-buffer (get-buffer exception-buffer)

      (when (< number-of-windows py-split-window-on-execute-threshold)
	(unless
	    (member (get-buffer-window output-buffer) (window-list))
	  (py--manage-windows-split exception-buffer)))
      (display-buffer output-buffer t)
      (switch-to-buffer exception-buffer)
      )))

(defun py--shell-manage-windows (output-buffer &optional exception-buffer split switch)
  "Adapt or restore window configuration from OUTPUT-BUFFER.

Optional EXCEPTION-BUFFER SPLIT SWITCH
Return nil."
  (let* ((exception-buffer (or exception-buffer (other-buffer)))
	 (old-window-list (window-list))
	 (number-of-windows (length old-window-list))
	 (split (or split py-split-window-on-execute))
	 (switch
	  (or py-switch-buffers-on-execute-p switch py-pdbtrack-tracked-buffer)))
    ;; (output-buffer-displayed-p)
    (cond
     (py-keep-windows-configuration
      (py-restore-window-configuration)
      (set-buffer output-buffer)
      (goto-char (point-max)))
     ((and (eq split 'always)
	   switch)
      (if (member (get-buffer-window output-buffer) (window-list))
	  ;; (delete-window (get-buffer-window output-buffer))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split exception-buffer)
	;; otherwise new window appears above
	(save-excursion
	  (other-window 1)
	  (switch-to-buffer output-buffer))
	(display-buffer exception-buffer)))
     ((and
       (eq split 'always)
       (not switch))
      (if (member (get-buffer-window output-buffer) (window-list))
	  (select-window (get-buffer-window output-buffer))
	(py--manage-windows-split exception-buffer)
	(display-buffer output-buffer)
	(pop-to-buffer exception-buffer)))
     ((and
       (eq split 'just-two)
       switch)
      (switch-to-buffer (current-buffer))
      (delete-other-windows)
      (py--manage-windows-split exception-buffer)
      ;; otherwise new window appears above
      (other-window 1)
      (set-buffer output-buffer)
      (switch-to-buffer (current-buffer)))
     ((and
       (eq split 'just-two)
       (not switch))
      (switch-to-buffer exception-buffer)
      (delete-other-windows)
      (unless
	  (member (get-buffer-window output-buffer) (window-list))
	(py--manage-windows-split exception-buffer))
      ;; Fixme: otherwise new window appears above
      (save-excursion
	(other-window 1)
	(pop-to-buffer output-buffer)
	(goto-char (point-max))
	(other-window 1)))
     ((and
       split
       (not switch))
      ;; https://bugs.launchpad.net/python-mode/+bug/1478122
      ;; > If the shell is visible in any of the windows it should re-use that window
      ;; > I did double check and py-keep-window-configuration is nil and split is t.
      (py--split-t-not-switch-wm output-buffer number-of-windows exception-buffer))
     ((and split switch)
      (unless
	  (member (get-buffer-window output-buffer) (window-list))
	(py--manage-windows-split exception-buffer))
      ;; Fixme: otherwise new window appears above
      ;; (save-excursion
      ;; (other-window 1)
      ;; (pop-to-buffer output-buffer)
      ;; [Bug 1579309] python buffer window on top when using python3
      (set-buffer output-buffer)
      (switch-to-buffer output-buffer)
      (goto-char (point-max))
      ;; (other-window 1)
      )
     ((not switch)
      (let (pop-up-windows)
	(py-restore-window-configuration))))))

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

(defun py--provide-command-args (shell fast-process)
  "Unbuffered WRT fast-process"
  (let ((erg
	 (delq nil
	       (cond
		;; ((eq 2 (prefix-numeric-value argprompt))
		;; py-python2-command-args)
		((string-match "^[Ii]" shell)
		 (if (string-match "^[0-4]" (py-ipython--which-version shell))
		     (remove "--simple-prompt"  py-ipython-command-args)
		   (if (member "--simple-prompt"  py-ipython-command-args)
		       py-ipython-command-args
		     (cons "--simple-prompt"  py-ipython-command-args))))
		((string-match "^[^-]+3" shell)
		 py-python3-command-args)
		(t
		 py-python-command-args)))))
    (if (and fast-process (not (member "-u" erg)))
	(cons "-u" erg)
      erg)))

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
(defun py-which-execute-file-command (filename)
  "Return the command appropriate to Python version and FILENAME.

Per default it's \"(format \"execfile(r'%s') # PYTHON-MODE\\n\" filename)\" for Python 2 series."
  (format "exec(compile(open(r'%s').read(), r'%s', 'exec')) # PYTHON-MODE\n" filename filename)
  )

(defun py--store-result-maybe (erg)
  "If no error occurred and ‘py-store-result-p’ store ERG for yank."
  (and (not py-error) erg (or py-debug-p py-store-result-p) (kill-new erg)))

(defcustom py-default-working-directory ""
  "If not empty used by ‘py-set-current-working-directory’"
  :type 'string
  :tag "py-default-working-directory"
  :group 'python-mode)

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
    (py-send-string (concat "import os\;os.chdir(\"" dir "\")") proc nil t)
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
  (unless py-debug-p
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

(defun py--fetch-result (buffer limit &optional cmd)
  "CMD: some shells echo the command in output-buffer
Delete it here"
  (when py-verbose-p (message "(current-buffer): %s" (current-buffer))
	(switch-to-buffer (current-buffer)))
  (if python-mode-v5-behavior-p
      (with-current-buffer buffer
	(string-trim (buffer-substring-no-properties (point-min) (point-max)) nil "\n"))
    (when (< limit (point-max))
      (string-trim (replace-regexp-in-string py-shell-prompt-regexp "" (buffer-substring-no-properties limit (point-max)))))))

(defun py--postprocess (output-buffer origline limit &optional cmd filename)
  "Provide return values, check result for error, manage windows.

According to OUTPUT-BUFFER ORIGLINE ORIG"
  ;; py--fast-send-string doesn't set origline
  (when (or py-return-result-p py-store-result-p)
    (with-current-buffer output-buffer
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (sit-for (py--which-delay-process-dependent (prin1-to-string output-buffer)))
      ;; (catch 'py--postprocess
      (setq py-result (py--fetch-result output-buffer limit cmd))
      ;; (throw 'py--postprocess (error "py--postprocess failed"))
      ;;)
      (if (and py-result (not (string= "" py-result)))
	  (if (string-match "^Traceback" py-result)
	      (if filename
		  (setq py-error py-result)
		(progn
		  (with-temp-buffer
		    (insert py-result)
		    (sit-for 0.1 t)
		    (setq py-error (py--fetch-error origline filename)))))
	    (when py-store-result-p
	      (kill-new py-result))
	    (when py-verbose-p (message "py-result: %s" py-result))
	    py-result)
	(when py-verbose-p (message "py--postprocess: %s" "Don't see any result"))))))

(defun py--execute-file-base (&optional proc filename cmd procbuf origline fast)
  "Send to Python interpreter process PROC.

In Python version 2.. \"execfile('FILENAME')\".

Takes also CMD PROCBUF ORIGLINE NO-OUTPUT.

Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
‘kill-output-from-shell’ does The Right Thing.
Returns position where output starts."
  ;; (message "(current-buffer) %s" (current-buffer))
  (let* ((buffer (or procbuf (and proc (process-buffer proc)) (py-shell nil nil nil nil nil fast)))
	 (proc (or proc (get-buffer-process buffer)))
	 (limit (marker-position (process-mark proc)))
	 (cmd (or cmd (py-which-execute-file-command filename)))
	 erg)
    (if fast
	(process-send-string proc cmd)
      (py-send-string cmd proc))
    ;; (message "%s" (current-buffer))
    (with-current-buffer buffer
      (when (or py-return-result-p py-store-result-p)
	(setq erg (py--postprocess buffer origline limit cmd filename))
	(if py-error
	    (setq py-error (prin1-to-string py-error))
	  erg)))))

(defun py--execute-buffer-finally (strg proc procbuf origline filename fast wholebuf)
  (if (and filename wholebuf (not (buffer-modified-p)))
      (unwind-protect
	  (py--execute-file-base proc filename nil procbuf origline fast))
    (let* ((tempfile (concat (expand-file-name py-temp-directory) py-separator-char "temp" (md5 (format "%s" (nth 3 (current-time)))) ".py")))
      (with-temp-buffer
	(insert strg)
	(write-file tempfile))
      (unwind-protect
	  (py--execute-file-base proc tempfile nil procbuf origline fast)
	(and (file-readable-p tempfile) (delete-file tempfile py-debug-p))))))

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

(defun py--execute-base-intern (strg filename proc file wholebuf buffer origline execute-directory start end &optional fast)
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
    (py--execute-ge24.3 start end execute-directory py-shell-name py-exception-buffer proc file origline))
   ((and filename wholebuf)
    (py--execute-file-base proc filename nil buffer origline fast))
   (t
    ;; (message "(current-buffer) %s" (current-buffer))
    (py--execute-buffer-finally strg proc buffer origline filename fast wholebuf)
    ;; (py--delete-temp-file tempfile)
    )))

(defun py--execute-base (&optional start end shell filename proc file wholebuf fast dedicated split switch)
  "Update optional variables START END SHELL FILENAME PROC FILE WHOLEBUF FAST DEDICATED SPLIT SWITCH."
  (setq py-error nil)
  (when py-debug-p (message "py--execute-base: (current-buffer): %s" (current-buffer)))
  (when (or fast py-fast-process-p) (ignore-errors (py-kill-buffer-unconditional py-output-buffer)))
  (let* ((orig (point))
	 (fast (or fast py-fast-process-p))
	 (exception-buffer (current-buffer))
	 (start (or start (and (use-region-p) (region-beginning)) (point-min)))
	 (end (or end (and (use-region-p) (region-end)) (point-max)))
	 (strg-raw (if py-if-name-main-permission-p
		       (buffer-substring-no-properties start end)
		     (py--fix-if-name-main-permission (buffer-substring-no-properties start end))))
	 (strg (py--fix-start strg-raw))
	 (wholebuf (unless file (or wholebuf (and (eq (buffer-size) (- end start))))))
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
		     (sit-for 0.1))))
	 (split (if python-mode-v5-behavior-p 'just-two split)))
    (setq py-output-buffer (or (and python-mode-v5-behavior-p py-output-buffer) (and proc (buffer-name (process-buffer proc)))
			       (py--choose-buffer-name shell dedicated fast)))
    (py--execute-base-intern strg filename proc file wholebuf py-output-buffer origline execute-directory start end fast)
    (when (or split py-split-window-on-execute py-switch-buffers-on-execute-p)
      (py--shell-manage-windows py-output-buffer exception-buffer (or split py-split-window-on-execute) switch))))

;; (defmacro py--execute-prepare (form shell dedicated switch beg end file fast proc wholebuf split)
;;   "Used by python-components-extended-executes ."
;;   (declare  (debug t))
;;   (save-excursion
;;     `(let* ((form ,(prin1-to-string form))
;;            (origline (py-count-lines))
;; 	   (fast
;; 	    (or ,fast ,py-fast-process-p)
;; 	    )
;; 	   (py-exception-buffer (current-buffer))
;;            (beg (unless ,file
;;                   (prog1
;;                       (or ,beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

;;                           (funcall (intern-soft (concat "py-backward-" form)))
;;                           (push-mark)))))
;;            (end (unless ,file
;;                   (or ,end (save-excursion (funcall (intern-soft (concat "py-forward-" form)))))))
;;            filename)
;;       ;; (setq py-buffer-name nil)
;;       (if ,file
;;           (progn
;;             (setq filename (expand-file-name ,file))
;;             (if (file-readable-p filename)
;;                 (py--execute-file-base nil filename nil nil origline)
;;               (message "%s not readable. %s" ,file "Do you have write permissions?")))
;;         (py--execute-base beg end ,shell filename ,proc ,file ,wholebuf ,fast ,dedicated ,split ,switch)))))

(defun py--execute-prepare (form shell &optional dedicated switch beg end file fast proc wholebuf split)
  "Update some vars."
  (save-excursion
    (let* ((form (prin1-to-string form))
           (origline (py-count-lines))
	   (fast
	    (or fast py-fast-process-p))
	   (py-exception-buffer (current-buffer))
           (beg (unless file
                  (prog1
                      (or beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

                          (funcall (intern-soft (concat "py-backward-" form)))
                          (push-mark)))))
           (end (unless file
                  (or end (save-excursion (funcall (intern-soft (concat "py-forward-" form)))))))
           filename)
      ;; (setq py-buffer-name nil)
      (if file
          (progn
            (setq filename (expand-file-name file))
            (if (file-readable-p filename)
                (py--execute-file-base nil filename nil nil origline)
              (message "%s not readable. %s" file "Do you have write permissions?")))
        (py--execute-base beg end shell filename proc file wholebuf fast dedicated split switch)))))

;; (defun py--delete-temp-file (tempfile &optional tempbuf)
;;   "After ‘py--execute-buffer-finally’ returned delete TEMPFILE &optional TEMPBUF."
;;   (sit-for py--delete-temp-file-delay t)
;;   (py--close-execution tempbuf tempfile))

(defun py--fetch-error (output-buffer &optional origline filename)
  "Highlight exceptions found in BUF.

If an exception occurred return error-string, otherwise return nil.
BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember ORIGLINE of source buffer"
  (with-current-buffer output-buffer
    (when py-debug-p (switch-to-buffer (current-buffer)))
    ;; (setq py-error (buffer-substring-no-properties (point) (point-max)))
    (goto-char (point-max))
    (when (re-search-backward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
      (when (and filename (re-search-forward "File \"\\(.+\\)\", line \\([0-9]+\\)\\(.*\\)$" nil t)
		 (replace-match filename nil nil nil 1))
	(when (and origline (re-search-forward "line \\([0-9]+\\)\\(.*\\)$" (line-end-position) t 1))
	  (replace-match origline nil nil nil 2)))
      (setq py-error (buffer-substring-no-properties (point) (point-max))))
    ;; (when erg
    ;;   (goto-char erg)
    ;;   (save-match-data
    ;; 	(and (not (py--buffer-filename-remote-maybe
    ;; 		   (or
    ;; 		    (get-buffer py-exception-buffer)
    ;; 		    (get-buffer (file-name-nondirectory py-exception-buffer)))))
    ;; 	     (string-match "^[ \t]*File" (buffer-substring-no-properties (point) (line-end-position)))
    ;; 	     (looking-at "[ \t]*File")
    ;; 	     (replace-match " Buffer")))
    ;;   (setq py-error (buffer-substring-no-properties (point-min) (point-max)))
    ;; (sit-for 0.1 t)
    py-error))

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

(defun py-execute-file (filename)
  "When called interactively, user is prompted for FILENAME."
  (interactive "fFilename: ")
  (let (;; postprocess-output-buffer might want origline
        (origline 1)
        (py-exception-buffer filename)
        erg)
    (if (file-readable-p filename)
        (if py-store-result-p
            (setq erg (py--execute-file-base nil (expand-file-name filename) nil nil origline))
          (py--execute-file-base nil (expand-file-name filename)))
      (message "%s not readable. %s" filename "Do you have write permissions?"))
    (py--shell-manage-windows py-output-buffer py-exception-buffer nil
                              (or (called-interactively-p 'interactive)))
    erg))

(defun py-execute-string (&optional strg shell dedicated switch fast)
  "Send the optional argument STRG to Python default interpreter.

Optional SHELL DEDICATED SWITCH FAST
See also ‘py-execute-region’."
  (interactive)
  (let ((strg (or strg (read-from-minibuffer "String: ")))
        (shell (or shell (default-value 'py-shell-name))))
    (with-temp-buffer
      (insert strg)
      (py-execute-region (point-min) (point-max) shell dedicated switch fast))))

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

(defun py--fix-if-name-main-permission (strg)
  "Remove \"if __name__ == '__main__ '\" STRG from code to execute.

See ‘py-if-name-main-permission-p’"
  (let ((strg (if py-if-name-main-permission-p strg
		(replace-regexp-in-string
		 "if[( ]*__name__[) ]*==[( ]*['\"]\\{1,3\\}__main__['\"]\\{1,3\\}[) ]*:"
		 ;; space after __main__, i.e. will not be executed
		 "if __name__ == '__main__ ':" strg))))
    strg))

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

(defun py--fix-start (strg)
  "Internal use by py-execute... functions.

Takes STRG
Avoid empty lines at the beginning."
  ;; (when py-debug-p (message "py--fix-start:"))
  (let (py--imenu-create-index-p
	py-guess-py-install-directory-p
	py-autopair-mode
	py-complete-function
	py-load-pymacs-p
	py-load-skeletons-p
	erg)
    (with-temp-buffer
      (python-mode)
      (insert strg)
      (goto-char (point-min))
      (when (< 0 (setq erg (skip-chars-forward " \t\r\n\f" (line-end-position))))
	(dotimes (_ erg)
	  (indent-rigidly-left (point-min) (point-max))))
      (unless (py--beginning-of-statement-p)
	(py-forward-statement))
      (while (not (eq (current-indentation) 0))
	(py-shift-left py-indent-offset))
      (goto-char (point-max))
      (unless (py-empty-line-p)
	(newline 1))
      (buffer-substring-no-properties 1 (point-max)))))

(defun py-fetch-py-master-file ()
  "Lookup if a ‘py-master-file’ is specified.

See also doku of variable ‘py-master-file’"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  (when (called-interactively-p 'any) (message "%s" py-master-file)))

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
          (py--execute-file-base proc file
                                (if (string-match "\\.py$" file)
                                    (let ((m (py--qualified-module-name (expand-file-name file))))
                                      (if (string-match "python2" py-shell-name)
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
(defun py--postprocess-intern (&optional origline exception-buffer output-buffer)
  "Highlight exceptions found in BUF.

Optional ORIGLINE EXCEPTION-BUFFER
If an exception occurred return error-string, otherwise return nil.
BUF must exist.

Indicate LINE if code wasn't run from a file, thus remember line of source buffer"
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
						 1py-shell-prompt-regexp)
					    (goto-char (match-end 0)))))

					(progn (skip-chars-forward " \t\r\n\f"   (line-end-position))(point)))
		  (insert (concat "    File " (buffer-name exception-buffer) ", line "
				  (prin1-to-string origline)))))
	      ;; these are let-bound as ‘tempbuf’
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
		  (push 'py-error ecode))))))
	py-error))))

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
(defalias 'py-shell-command-on-region 'py-execute-region)
(defalias 'py-send-region-ipython 'py-execute-region-ipython)
(defalias 'py-send-region 'py-execute-region)

(provide 'python-components-execute);
;;;  python-components-execute.el ends here
