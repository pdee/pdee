;;; python-components-start3.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.2.3

;; Keywords: languages, processes, python, oop

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2020 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

;; As for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; `py-expression' composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; `py-partial-expression' beginns with a "(", which is
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

(defun py-restore-window-configuration ()
  "Restore ‘py-restore-window-configuration’ when completion is done resp. abandoned."
  (let (val)
    (and (setq val (get-register py--windows-config-register))(and (consp val) (window-configuration-p (car val))(markerp (cadr val)))(marker-buffer (cadr val))
	 (jump-to-register py--windows-config-register))))

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
  "Return `t' if line before contains only whitespace characters. "
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
                (default-value 'py-indent-offset))))
    (setq erg (and (py-guessed-sanity-check erg) erg))
    erg))

(defun py--guess-indent-forward ()
  "Called when moving to end of a form and `py-smart-indentation' is on."
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
  "Called when moving to beginning of a form and `py-smart-indentation' is on."
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
  "Guess `py-indent-offset'.

Set local value of `py-indent-offset', return it

Might change local value of `py-indent-offset' only when called
downwards from beginning of block followed by a statement.
Otherwise ‘default-value’ is returned.
Unless DIRECTION is symbol 'forward, go backward first"
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
              (default-value 'py-indent-offset)))
      (when (called-interactively-p 'any) (message "%s" py-indent-offset))
      py-indent-offset)))

(defun py--leave-backward-string-list-and-comment-maybe (pps)
  (while (or (and (nth 8 pps) (goto-char (nth 8 pps)))
             (and (nth 1 pps) (goto-char (nth 1 pps)))
             (and (nth 4 pps) (goto-char (nth 4 pps))))
    ;; (back-to-indentation)
    (when (or (looking-at comment-start)(member (char-after) (list ?\" ?')))
      (skip-chars-backward " \t\r\n\f"))
    (setq pps (parse-partial-sexp (point-min) (point)))))

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

(defmacro py-shell--add-to-path-with-priority (pathvar paths)
  "Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun py-shell-tramp-refresh-remote-path (vec paths)
  "Update VEC's remote-path giving PATHS priority."
  (let ((remote-path (tramp-get-connection-property vec "remote-path" nil)))
    (when remote-path
      (py-shell--add-to-path-with-priority remote-path paths)
      (tramp-set-connection-property vec "remote-path" remote-path)
      (tramp-set-remote-path vec))))

(defun py-shell-tramp-refresh-process-environment (vec env)
  "Update VEC's process environment with ENV."
  ;; Stolen from `tramp-open-connection-setup-interactive-shell'.
  (let ((env (append (when (fboundp 'tramp-get-remote-locale)
                       ;; Emacs<24.4 compat.
                       (list (tramp-get-remote-locale vec)))
		     (copy-sequence env)))
        (tramp-end-of-heredoc
         (if (boundp 'tramp-end-of-heredoc)
             tramp-end-of-heredoc
           (md5 tramp-end-of-output)))
	unset vars item)
    (while env
      (setq item (split-string (car env) "=" 'omit))
      (setcdr item (mapconcat 'identity (cdr item) "="))
      (if (and (stringp (cdr item)) (not (string-equal (cdr item) "")))
	  (push (format "%s %s" (car item) (cdr item)) vars)
	(push (car item) unset))
      (setq env (cdr env)))
    (when vars
      (tramp-send-command
       vec
       (format "while read var val; do export $var=$val; done <<'%s'\n%s\n%s"
	       tramp-end-of-heredoc
	       (mapconcat 'identity vars "\n")
	       tramp-end-of-heredoc)
       t))
    (when unset
      (tramp-send-command
       vec (format "unset %s" (mapconcat 'identity unset " ")) t))))

(defun py-shell-calculate-pythonpath ()
  "Calculate the PYTHONPATH using `py-shell-extra-pythonpaths'."
  (let ((pythonpath
         (split-string
          (or (getenv "PYTHONPATH") "") path-separator 'omit)))
    (py-shell--add-to-path-with-priority
     pythonpath py-shell-extra-pythonpaths)
    (mapconcat 'identity pythonpath path-separator)))

(defun py-shell-calculate-exec-path ()
  "Calculate `exec-path'.
Prepends `py-shell-exec-path' and adds the binary directory
for virtualenv if `py-shell-virtualenv-root' is set - this
will use the python interpreter from inside the virtualenv when
starting the shell.  If `default-directory' points to a remote host,
the returned value appends `py-shell-remote-exec-path' instead
of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       py-shell-remote-exec-path
                     exec-path)))

        ;; Windows and POSIX systems use different venv directory structures
        (virtualenv-bin-dir (if (eq system-type 'windows-nt) "Scripts" "bin")))
    (py-shell--add-to-path-with-priority
     new-path py-shell-exec-path)
    (if (not py-shell-virtualenv-root)
        new-path
      (py-shell--add-to-path-with-priority
       new-path
       (list (expand-file-name virtualenv-bin-dir py-shell-virtualenv-root)))
      new-path)))

(defun py-shell-calculate-process-environment ()
  "Calculate `process-environment' or `tramp-remote-process-environment'.
Prepends `py-shell-process-environment', sets extra
pythonpaths from `py-shell-extra-pythonpaths' and sets a few
virtualenv related vars.  If `default-directory' points to a
remote host, the returned value is intended for
`tramp-remote-process-environment'."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment))
         (virtualenv (when py-shell-virtualenv-root
                       (directory-file-name py-shell-virtualenv-root))))
    (dolist (env py-shell-process-environment)
      (pcase-let ((`(,key ,value) (split-string env "=")))
        (setenv key value)))
    (when py-shell-unbuffered
      (setenv "PYTHONUNBUFFERED" "1"))
    (when py-shell-extra-pythonpaths
      (setenv "PYTHONPATH" (py-shell-calculate-pythonpath)))
    (if (not virtualenv)
        process-environment
      (setenv "PYTHONHOME" nil)
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defmacro py-shell-with-environment (&rest body)
  "Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`py-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (py-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (py-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (py-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (py-shell-tramp-refresh-remote-path
            ,vec (py-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (py-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))

(defun py-shell (&optional argprompt args dedicated shell buffer fast exception-buffer split switch internal)
  "Connect process to BUFFER.

Start an interpreter according to ‘py-shell-name’ or SHELL.

Optional ARGPROMPT: with \\[universal-argument] start in a new
dedicated shell.

Optional ARGS: Specify other than default command args.

Optional DEDICATED: start in a new dedicated shell.
Optional string SHELL overrides default ‘py-shell-name’.
Optional string BUFFER allows a name, the Python process is connected to
Optional FAST: no fontification in process-buffer.
Optional EXCEPTION-BUFFER: point to error.
Optional SPLIT: see var ‘py-split-window-on-execute’
Optional SWITCH: see var ‘py-switch-buffers-on-execute-p’
Optional INTERNAL shell will be invisible for users

Reusing existing processes: For a given buffer and same values,
if a process is already running for it, it will do nothing.

Runs the hook `py-shell-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive "p")
  (let* ((interactivep (and argprompt (eq 1 (prefix-numeric-value argprompt))))
	 (fast (unless (eq major-mode 'org-mode)
		 (or fast py-fast-process-p)))
	 (dedicated (or (eq 4 (prefix-numeric-value argprompt)) dedicated py-dedicated-process-p))
	 (shell (or shell (py-choose-shell)))
	 (args (or args (py--provide-command-args shell fast)))
	 (buffer-name
	  (or buffer
	      (py--choose-buffer-name shell dedicated fast)))
	 (proc (get-buffer-process buffer-name))
	 (done nil)
	 (delay nil)
	 (buffer
	  (or
	   (and (ignore-errors (process-buffer proc))
		(save-excursion (with-current-buffer (process-buffer proc)
				  ;; point might not be left there
				  (goto-char (point-max))
				  (push-mark)
				  (setq done t)
				  (process-buffer proc))))
	   (save-excursion
	     (py-shell-with-environment
	       (if fast
		   (process-buffer (apply 'start-process shell buffer-name shell args))
		 (apply #'make-comint-in-buffer shell buffer-name
			shell nil args))))))
	 ;; (py-shell-prompt-detect-p (or (string-match "^\*IP" buffer) py-shell-prompt-detect-p))
	 )
    (setq py-output-buffer (buffer-name (if python-mode-v5-behavior-p py-output-buffer buffer)))
    (unless done
      (with-current-buffer buffer
	(setq delay (py--which-delay-process-dependent buffer-name))
	(unless fast
	  (when interactivep
	    (cond ((string-match "^.I" buffer-name)
		   (message "Waiting according to ‘py-ipython-send-delay:’ %s" delay))
		  ((string-match "^.+3" buffer-name)
		   (message "Waiting according to ‘py-python3-send-delay:’ %s" delay))))
	  (setq py-modeline-display (py--update-lighter buffer-name))
	  (sit-for delay t))))
    (if (setq proc (get-buffer-process buffer))
	(progn
	  (with-current-buffer buffer
	    (unless fast (py-shell-mode))
	    (and internal (set-process-query-on-exit-flag proc nil)))
	  (when (or interactivep
		    (or switch py-switch-buffers-on-execute-p py-split-window-on-execute))
	    (py--shell-manage-windows buffer exception-buffer split (or interactivep switch)))
	  buffer)
      (error (concat "py-shell:" (py--fetch-error py-output-buffer))))))

(defun py--point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (let (erg)
    (save-excursion
      (setq erg
            (progn
              (cond
               ((eq position 'bol) (beginning-of-line))
               ((eq position 'eol) (end-of-line))
               ((eq position 'bod) (py-backward-def-or-class))
               ((eq position 'eod) (py-forward-def-or-class))
               ;; Kind of funny, I know, but useful for py-up-exception.
               ((eq position 'bob) (goto-char (point-min)))
               ((eq position 'eob) (goto-char (point-max)))
               ((eq position 'boi) (back-to-indentation))
               ((eq position 'bos) (py-backward-statement))
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))

(defun py--execute-file-base (filename &optional proc cmd procbuf origline fast)
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
      (py-execute-string cmd proc))
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
	  (py--execute-file-base filename proc nil procbuf origline fast))
    (let* ((tempfile (concat (expand-file-name py-temp-directory) py-separator-char "temp" (md5 (format "%s" (nth 3 (current-time)))) ".py")))
      (with-temp-buffer
	(insert strg)
	(write-file tempfile))
      (unwind-protect
	  (py--execute-file-base tempfile proc nil procbuf origline fast)
	(and (file-readable-p tempfile) (delete-file tempfile py--debug-p))))))

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
						 py-shell-prompt-regexp)
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
  "Update optional variables START END SHELL FILENAME PROC FILE WHOLEBUF FAST DEDICATED SPLIT SWITCH."
  (setq py-error nil)
  (when py--debug-p (message "py--execute-base: (current-buffer): %s" (current-buffer)))
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
		     (sit-for 0.1))))
	 (split (if python-mode-v5-behavior-p 'just-two split)))
    (setq py-output-buffer (or (and python-mode-v5-behavior-p py-output-buffer) (and proc (buffer-name (process-buffer proc)))
			       (py--choose-buffer-name shell dedicated fast)))
    (py--execute-base-intern strg filename proc wholebuf py-output-buffer origline execute-directory start end fast)
    (when (or split py-split-window-on-execute py-switch-buffers-on-execute-p)
      (py--shell-manage-windows py-output-buffer exception-buffer (or split py-split-window-on-execute) switch))))

(provide 'python-components-start3)
;;; python-components-start3.el ends here
