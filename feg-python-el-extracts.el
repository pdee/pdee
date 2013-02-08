;;; feg-python-el-extracts.el --- Started from Python's flying circus support for Emacs

;; Copyright (C) 2010, 2011 Free Software Foundation, Inc.

;; Original author: Fabián E. Gallina <fabian@anue.biz>
;; URL: https://github.com/fgallina/python.el
;; Version: 0.23.1
;; Maintainer: FSF
;; Created: Jul 2010
;; Keywords: languages

;; This file is NOT part of GNU Emacs.

;; feg-python-el-extracts.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; python.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with python.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: Build on top of Fabián E. Gallina's excellent work

(defcustom python-shell-buffer-name "Python"
  "Default buffer name for Python interpreter."
  :type 'string
  :group 'python-mode)

(defcustom python-shell-interpreter "python"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python-mode
)

;; Stolen from org-mode
(defun py-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^python-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^python-")
                        (symbol-name (car pair)))
	  (set (make-local-variable (car pair))
	       (cdr pair))))
   (buffer-local-variables from-buffer)))

(defun py-shell-send-setup-code (process)
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (accept-process-output process 1)
  (dolist (code py-setup-codes)
    (py-shell-send-string-no-output
     (symbol-value code) process)
    (sit-for 0.1)))

(defun py-shell-get-process-name (dedicated)
  "Calculate the appropiate process name for inferior Python process.
If DEDICATED is t and the variable `buffer-file-name' is non-nil
returns a string with the form
`python-shell-buffer-name'[variable `buffer-file-name'] else
returns the value of `python-shell-buffer-name'.  After
calculating the process name adds the buffer name for the process
in the `same-window-buffer-names' list."
  (let ((process-name
         (if (and dedicated
                  buffer-file-name)
             (format "%s[%s]" python-shell-buffer-name buffer-file-name)
           (format "%s" python-shell-buffer-name))))
    (add-to-list 'same-window-buffer-names (purecopy
                                            (format "*%s*" process-name)))
    process-name))

(defun py-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When `py-verbose-p' and MSG is non-nil messages the first line of STRING."
  (interactive "sPython command: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (lines (split-string string "\n" t))
         (temp-file-name (concat (with-current-buffer (process-buffer process)
                                   (file-remote-p default-directory))
                                 (py-normalize-directory py-temp-directory)
                                 "psss-temp.py"))
         (file-name (or (buffer-file-name) temp-file-name)))
    (if (> (length lines) 1)
        (progn
          (with-temp-file temp-file-name
            (insert string)
            (delete-trailing-whitespace))
          (py-send-file file-name process temp-file-name))
      (comint-send-string process string)
      (when (or (not (string-match "\n$" string))
                (string-match "\n[ \t].*\n?$" string))
        (comint-send-string process "\n")))))

(defun py-shell-send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* ((output-buffer)
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output-buffer (concat output-buffer string))
                      "")))))
    (py-shell-send-string string process msg)
    (accept-process-output process 1)
    (when output-buffer
      (replace-regexp-in-string
       (if (> (length py-shell-prompt-output-regexp) 0)
           (format "\n*%s$\\|^%s\\|\n$"
                   python-shell-prompt-regexp
                   (or py-shell-prompt-output-regexp ""))
         (format "\n*$\\|^%s\\|\n$"
                 python-shell-prompt-regexp))
       "" output-buffer))))

(defun python-shell-send-region (start end)
  "Send the region delimited by START and END to inferior Python process."
  (interactive "r")
  (let ((deactivate-mark nil))
    (py-shell-send-string (buffer-substring start end) nil t)))

(defun py-send-file (file-name &optional process temp-file-name)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME."
  (interactive "fFile to send: ")
  (let* ((process (or process (get-buffer-process (py-shell))))
         (temp-file-name (when temp-file-name
                           (expand-file-name temp-file-name)))
         (file-name (or (expand-file-name file-name) temp-file-name))
         py-python-command-args)
    (when (not file-name)
      (error "If FILE-NAME is nil then TEMP-FILE-NAME must be non-nil"))
    (py-shell-send-string
     (format
      (concat "__pyfile = open('''%s''');"
              "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
              "__pyfile.close()")
      (or (file-remote-p temp-file-name 'localname) file-name) file-name)
     process)))

(defun py-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer (py-shell) t))

(defcustom python-pdbtrack-stacktrace-info-regexp
  "^> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular Expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :group 'python-mode)

;;; Eldoc
(defcustom python-eldoc-string-code
  "__PYDOC_get_help('''%s''')\n"
  "Python code used to get a string with the documentation of an object."
  :type 'string
  :group 'python-mode)

(defun python-info-current-defun (&optional include-type)
  "Return name of surrounding function with Python compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function is compatible to be used as
`add-log-current-defun-function' since it returns nil if point is
not inside a defun."
  (let ((names '())
        (min-indent)
        (first-run t))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (line-end-position))
        (forward-comment -9999)
        (setq min-indent (current-indentation))
        (while (py-beginning-of-def-or-class)
          (when (or (< (current-indentation) min-indent)
                    first-run)
            (setq first-run nil)
            (setq min-indent (current-indentation))
            (looking-at py-def-or-class-re)
            (setq names (cons
                         (if (not include-type)
                             (match-string-no-properties 1)
                           (mapconcat 'identity
                                      (split-string
                                       (match-string-no-properties 0)) " "))
                         names))))))
    (when names
      (mapconcat (lambda (string) string) names "."))))

(defun py-eldoc--get-doc-at-point (&optional force-input force-process)
  "Internal implementation to get documentation at point.
If not FORCE-INPUT is passed then what `current-word' returns
will be used.  If not FORCE-PROCESS is passed what
`python-shell-get-process' returns is used."
  (let ((process (or force-process (python-shell-get-process))))
    (if (not process)
        "Eldoc needs an inferior Python process running."
      (let* ((current-defun (python-info-current-defun))
             (input (or force-input
                        (with-syntax-table py-dotty-syntax-table
                          (if (not current-defun)
                              (current-word)
                            (concat current-defun "." (current-word))))))
             (ppss (syntax-ppss))
             (help (when (and input
                              (not (string= input (concat current-defun ".")))
                              (not (or (python-info-ppss-context 'string ppss)
                                       (python-info-ppss-context 'comment ppss))))
                     (when (string-match (concat
                                          (regexp-quote (concat current-defun "."))
                                          "self\\.") input)
                       (with-temp-buffer
                         (insert input)
                         (goto-char (point-min))
                         (forward-word)
                         (forward-char)
                         (delete-region (point-marker) (search-forward "self."))
                         (setq input (buffer-substring (point-min) (point-max)))))
                     (py-shell-send-string-no-output
                      (format python-eldoc-string-code input) process))))
        (with-current-buffer (process-buffer process)
          (when comint-last-prompt-overlay
            (delete-region comint-last-input-end
                           (overlay-start comint-last-prompt-overlay))))
        (when (and help
                   (not (string= help "\n")))
          help)))))

(defun py-eldoc-at-point (symbol)
  "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."
  (interactive
   (let ((symbol (with-syntax-table py-dotty-syntax-table
                   (current-word)))
         (enable-recursive-minibuffers t))
     (list (read-string (if symbol
                            (format "Describe symbol (default %s): " symbol)
                          "Describe symbol: ")
                        nil nil symbol))))
  (let ((process (python-shell-get-process)))
    (if (not process)
        (message "Eldoc needs an inferior Python process running.")
      (message (py-eldoc--get-doc-at-point symbol process)))))

;;; Pdb
(defvar python-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")
(make-variable-buffer-local 'python-pdbtrack-buffers-to-kill)



;;;
(defun python-shell-completion--get-completions (input process completion-code)
  "Retrieve available completions for INPUT using PROCESS.
Argument COMPLETION-CODE is the python code used to get
completions on the current context."
  (with-current-buffer (process-buffer process)
    (let ((completions
           (py-shell-send-string-no-output
            (format completion-code input) process)))
      (when (> (length completions) 2)
        (split-string completions "^'\\|^\"\\|;\\|'$\\|\"$" t)))))

(defun python-shell-completion--do-completion-at-point (process imports input)
  "Do completion at point for PROCESS."
  (with-syntax-table py-dotty-syntax-table
    (when imports (py-shell-send-string-no-output imports process))
    (let* ((code python-shell-module-completion-string-code)
           (completions
            (python-shell-completion--get-completions
             input process code))
           (completion (when completions
                         (try-completion input completions))))
      (cond ((eq completion t)
             (if (eq this-command last-command)
                 (when py-completion-last-window-configuration
                   (set-window-configuration
                    py-completion-last-window-configuration)))
             ;; (setq py-completion-last-window-configuration nil)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (ignore-errors (dabbrev-expand nil))(when py-indent-no-completion-p
                                                           (tab-to-tab-stop)))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((null completion)
             (if py-no-completion-calls-dabbrev-expand-p
                 (or (dabbrev-expand nil)(when py-indent-no-completion-p
                                           (tab-to-tab-stop))(message "Can't find completion "))
               (when py-indent-no-completion-p
                 (tab-to-tab-stop)))
             nil)
            ((not (string= input completion))
             (progn (delete-char (- (length input)))
                    (insert completion)
                    ;; minibuffer.el expects a list, a bug IMO
                    nil))
            (t
             (unless py-completion-last-window-configuration
               (setq py-completion-last-window-configuration
                     (current-window-configuration)))
             (with-output-to-temp-buffer "*Python Completions*"
               (display-completion-list
                (all-completions input completions)))
             nil)))))

(defun python-shell-completion-complete-at-point ()
  "Perform completion at point in inferior Python process."
  (interactive)
  (and comint-last-prompt-overlay
       (> (point-marker) (overlay-end comint-last-prompt-overlay))
       (python-shell-completion--do-completion-at-point
	(get-buffer-process (current-buffer)))))

(defun python-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point-marker)))
      (indent-for-tab-command)
    (comint-dynamic-complete)))

(provide 'feg-python-el-extracts)
