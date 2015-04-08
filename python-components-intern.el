;; python-components-intern.el --- Part of python-components-mode

;; Helper functions

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

;;  Keymap



(defvar python-mode-map nil)
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; (define-key map [(control y)] 'py-electric-yank)
        ;; moving point
        (define-key map [(control c)(control p)] 'py-beginning-of-statement)
        (define-key map [(control c)(control n)] 'py-end-of-statement)
        (define-key map [(control c)(control u)] 'py-beginning-of-block)
        (define-key map [(control c)(control q)] 'py-end-of-block)
        (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
        (define-key map [(control meta e)] 'py-end-of-def-or-class)

        ;; (define-key map [(meta i)] 'py-indent-forward-line)
        (define-key map [(control j)] 'py-newline-and-indent)
        ;; Most Pythoneers expect RET `py-newline-and-indent'
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
        (define-key map (kbd "RET") py-return-key)
        ;; (define-key map (kbd "RET") 'newline)
        (define-key map [(super backspace)] 'py-dedent)
        ;; (define-key map [(control return)] 'py-newline-and-dedent)
        ;; indentation level modifiers
        (define-key map [(control c)(control l)] 'py-shift-left)
        (define-key map [(control c)(control r)] 'py-shift-right)
        (define-key map [(control c)(<)] 'py-shift-left)
        (define-key map [(control c)(>)] 'py-shift-right)
        (define-key map [(control c)(tab)] 'py-indent-region)
        (define-key map [(control c)(:)] 'py-guess-indent-offset)
        ;; subprocess commands
        (define-key map [(control c)(control c)] 'py-execute-buffer)
        (define-key map [(control c)(control m)] 'py-execute-import-or-reload)
        (define-key map [(control c)(control s)] 'py-execute-string)
        (define-key map [(control c)(|)] 'py-execute-region)
        (define-key map [(control meta x)] 'py-execute-def-or-class)
        (define-key map [(control c)(!)] 'py-shell)
        (define-key map [(control c)(control t)] 'py-toggle-shell)
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
        (define-key map [(control c)(.)] 'py-expression)
        ;; Miscellaneous
        (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c)(control f)] 'py-sort-imports)
        (define-key map [(control c)(\#)] 'py-comment-region)
        (define-key map [(control c)(\?)] 'py-describe-mode)
        (define-key map [(control c)(control e)] 'py-help-at-point)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
        ;; (define-key map (kbd "TAB") 'py-indent-line)
        (define-key map (kbd "TAB") 'py-indent-or-complete)
	;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] 'py-shell-complete)
        ;;   (define-key map [(esc) (tab)] 'py-shell-complete))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (substitute-key-definition 'backward-up-list 'py-up
                                   map global-map)
        (substitute-key-definition 'down-list 'py-down
                                   map global-map)
        map))

(defvaralias 'py-mode-map 'python-mode-map)

(defvar py-python-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map [(control c)(-)] 'py-up-exception)
    (define-key map [(control c)(=)] 'py-down-exception)
    (define-key map (kbd "TAB") 'py-indent-or-complete)
    (define-key map [(meta tab)] 'py-shell-complete)
    (define-key map [(control c)(!)] 'py-shell)
    (define-key map [(control c)(control t)] 'py-toggle-shell)
    ;; electric keys
    (define-key map [(:)] 'py-electric-colon)
    (define-key map [(\#)] 'py-electric-comment)
    (define-key map [(delete)] 'py-electric-delete)
    (define-key map [(backspace)] 'py-electric-backspace)
    (define-key map [(control backspace)] 'py-hungry-delete-backwards)
    (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
    ;; (define-key map [(control y)] 'py-electric-yank)
    ;; moving point
    (define-key map [(control c)(control p)] 'py-beginning-of-statement)
    (define-key map [(control c)(control n)] 'py-end-of-statement)
    (define-key map [(control c)(control u)] 'py-beginning-of-block)
    (define-key map [(control c)(control q)] 'py-end-of-block)
    (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
    (define-key map [(control meta e)] 'py-end-of-def-or-class)
    (define-key map [(control j)] 'py-newline-and-indent)
    (define-key map [(super backspace)] 'py-dedent)
    ;; (define-key map [(control return)] 'py-newline-and-dedent)
    ;; indentation level modifiers
    (define-key map [(control c)(control l)] 'py-shift-left)
    (define-key map [(control c)(control r)] 'py-shift-right)
    (define-key map [(control c)(<)] 'py-shift-left)
    (define-key map [(control c)(>)] 'py-shift-right)
    (define-key map [(control c)(tab)] 'py-indent-region)
    (define-key map [(control c)(:)] 'py-guess-indent-offset)
    ;; subprocess commands
    (define-key map [(control meta h)] 'py-mark-def-or-class)
    (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
    (define-key map [(control c)(.)] 'py-expression)
    ;; Miscellaneous
    (define-key map [(super q)] 'py-copy-statement)
    (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
    (define-key map [(control c)(\#)] 'py-comment-region)
    (define-key map [(control c)(\?)] 'py-describe-mode)
    (define-key map [(control c)(control e)] 'py-help-at-point)
    (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
    ;; information
    (define-key map [(control c)(control b)] 'py-submit-bug-report)
    (define-key map [(control c)(control v)] 'py-version)
    (define-key map [(control c)(control w)] 'py-pychecker-run)
    (substitute-key-definition 'complete-symbol 'completion-at-point
			       map global-map)
    (substitute-key-definition 'backward-up-list 'py-up
			       map global-map)
    (substitute-key-definition 'down-list 'py-down
			       map global-map)
    map)
  "Used inside a Python-shell")

(defvar py-ipython-shell-mode-map py-python-shell-mode-map
  "Unless setting of ipython-shell-mode needs to be different, let's save some lines of code and copy py-python-shell-mode-map here.")

(defvar py-shell-map py-python-shell-mode-map)

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py-load-skeletons ()
  "Load skeletons from extensions. "
  (interactive)
  (load (concat py-install-directory "/extensions/python-components-skeletons.el")))

(defun py--kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;;  Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

;;  bottle.py
;;  py   = sys.version_info
;;  py3k = py >= (3,0,0)
;;  py25 = py <  (2,6,0)
;;  py31 = (3,1,0) <= py < (3,2,0)

;;  sys.version_info[0]
(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (py--string-strip (shell-command-to-string (concat executable " --version")))))
    (when (interactive-p) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;;  Utility stuff
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

;; dereived from shipped python.el
(defun py-history-input-filter (str)
  "`comint-input-filter' function for Python process.
Don't save anything for STR matching `py-history-filter-regexp'."
  (not (string-match py-history-filter-regexp str)))

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base (get-buffer-process (get-buffer (py-shell))) file-name))

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "P")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
		(get-buffer-process (buffer-name (current-buffer))))
	       (t (py-shell argprompt)))))
    (when (interactive-p) (message "%S" erg))
    erg))

;;  Miscellany.
(defun py--shell-simple-send (proc string)
  (let* ((strg (substring-no-properties string))
         (nln (string-match "\n$" strg)))
    ;; (or nln (setq strg (concat strg "\n")))
    ;; (comint-simple-send proc (substring-no-properties string))
    (process-send-string proc strg)
    (or nln (process-send-string proc "\n"))))

(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

;;  Hooks
;;  arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py--kill-emacs-hook)

(when py--warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py--warn-tmp-files-left))


(defun py-guess-pdb-path ()
  "If py-pdb-path isn't set, find location of pdb.py. "
  (interactive)
  (let ((ele (split-string (shell-command-to-string "whereis python")))
        erg)
    (while (or (not erg)(string= "" erg))
      (when (and (string-match "^/" (car ele)) (not (string-match "/man" (car ele))))
        (setq erg (shell-command-to-string (concat "find " (car ele) " -type f -name \"pdb.py\""))))
      (setq ele (cdr ele)))
    (if erg
        (message "%s" erg)
      (message "%s" "pdb.py not found, please customize `py-pdb-path'"))
    erg))

;;  credits to python.el
;;  (defun py-beg-of-defun-function ()
;;    (set (make-local-variable 'beginning-of-defun-function)
;;         'py-beginning-of-def-or-class))
;;
;;  (defun py-end-of-defun-function ()
;;    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command)))

;;  backward compatibility
(defalias 'py-switch-shells 'py-switch-shell)
(defalias 'py-toggle-shell 'py-switch-shell)
(defun py-switch-shell (&optional arg)
  "Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

\\[universal-argument] `py-toggle-shell' prompts to specify a reachable Python command.
\\[universal-argument] followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
\\[universal-argument] followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     \"MY-PATH-TO-SHELL\")"
  (interactive "P")
  (let ((name (cond ((eq 2 (prefix-numeric-value arg))
                     "python2")
                    ((eq 3 (prefix-numeric-value arg))
                     "python3")
                    ((eq 4 (prefix-numeric-value arg))
                     (py--string-strip
                      (read-from-minibuffer "Python Shell: " py-shell-name) "\" " "\" "
                      ))
                    ((eq 5 (prefix-numeric-value arg))
                     "jython")
                    (t (if (string-match py-shell-name
                                         py-shell-toggle-1)
                           py-shell-toggle-2
                         py-shell-toggle-1))))
        erg msg)
    (cond ((or (string= "ipython" name)
               (string= "IPython" name))
           (setq py-shell-name name
                 py-which-bufname "IPython"
                 msg "IPython"
                 mode-name "IPython"))
          ((string-match "python3" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
                 msg "CPython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "jython" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
                 msg "Jython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "python" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
                 msg "CPython"
                 mode-name py-which-bufname))
          (t
           (setq py-shell-name name
                 py-which-bufname name
                 msg name
                 mode-name name)))
    ;; py-edit-only-p has no interpreter
    ;; (if py-edit-only-p
    ;; (setq erg py-shell-name)
    (setq erg (executable-find py-shell-name))
    ;;)
    (if erg
        (progn
          (force-mode-line-update)
          (when (interactive-p)
            (message "Using the %s shell, %s" msg erg))
          (setq py-output-buffer (format "*%s Output*" py-which-bufname)))
      (error (concat "Could not detect " py-shell-name " on your sys
tem")))))

(defun py-toggle-local-default-use ()
  (interactive)
  "Toggle boolean value of `py-use-local-default'.

Returns `py-use-local-default'

See also `py-install-local-shells'
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (setq py-use-local-default (not py-use-local-default))
  (when (interactive-p) (message "py-use-local-default set to %s" py-use-local-default))
  py-use-local-default)

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

;;  FixMe: for unknown reasons this is not done by mode
(if (file-readable-p abbrev-file-name)
    (add-hook 'python-mode-hook
              (lambda ()
                (setq py-this-abbrevs-changed abbrevs-changed)
                (load abbrev-file-name nil t)
                (setq abbrevs-changed py-this-abbrevs-changed)))
  (message "Warning: %s" "no abbrev-file found, customize `abbrev-file-name' in order to make mode-specific abbrevs work. "))

;; ;
(add-to-list 'hs-special-modes-alist
             (list
              'python-mode
              ;; start regex
              (concat (if py-hide-show-hide-docstrings
                          "^\\s-*\"\"\"\\|" "")
                      (mapconcat 'identity
                                 (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                         py-hide-show-keywords)
                                 "\\|"))
              ;; end regex
              nil
              ;; comment-start regex
              "#"
              ;; forward-sexp function
              (lambda (arg)
                (py-end-of-block-or-clause))
              nil))

;; ;

(defun py--input-filter (str)
  "`comint-input-filter' function for Python.

Don't save anything for STR matching `py-input-filter-re' "
  (not (string-match py-input-filter-re str)))

(make-obsolete 'jpython-mode 'jython-mode nil)

(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))

;;  (add-to-list 'interpreter-mode-alist
;;  (cons (purecopy "[bi]*python[0-9.]*") 'python-mode))
;;
;;  (add-to-list 'interpreter-mode-alist
;;  (cons (purecopy "jython[0-9.]*") 'jython-mode))

(add-to-list 'magic-mode-alist
	     '("!#[ \t]*/.*[jp]ython[0-9.]*" . python-mode))

;;  lp:1355458, what about using `magic-mode-alist'?

(defun py--set-auto-fill-values ()
  "Internal use by `py--run-auto-fill-timer'"
  (let ((pps (syntax-ppss)))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (setq fill-column py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (set (make-local-variable 'fill-column) py-docstring-fill-column))
          (t (setq fill-column py-fill-column-orig)))))

(defun py--run-auto-fill-timer ()
  "Set fill-column to values of `py-docstring-fill-column' resp. to `py-comment-fill-column' according to environment. "
  (when py-auto-fill-mode
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             'py--set-auto-fill-values)))))

(defvar py--timer nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer)

(defvar py--timer-delay nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer-delay)

(defun py--unfontify-banner-intern ()
  (save-excursion
    (goto-char (point-min))
    (let ((erg (or (ignore-errors (car comint-last-prompt))
		   (and
		    (re-search-forward py-fast-filter-re nil t 1)
		    (match-beginning 0))
		   (progn
		     (forward-paragraph)
		     (point)))))
      ;; (sit-for 1 t)
      (if erg
	  (progn
	    (font-lock-unfontify-region (point-min) erg)
	    (goto-char (point-max)))
	(progn (and py-debug-p (message "%s" (concat "py--unfontify-banner: Don't see a prompt in buffer " (buffer-name buffer)))))))))

(defun py--unfontify-banner (&optional buffer)
  "Unfontify the shell banner-text.

Cancels `py--timer'
Expects being called by `py--run-unfontify-timer' "
  (interactive)
    (let ((buffer (or buffer (current-buffer))))
      (if (ignore-errors (buffer-live-p (get-buffer buffer)))
	  (with-current-buffer buffer
	    (py--unfontify-banner-intern)
	    (and (timerp py--timer)(cancel-timer py--timer)))
	(and (timerp py--timer)(cancel-timer py--timer)))))

(defun py--run-unfontify-timer (&optional buffer)
  "Unfontify the shell banner-text "
  (when py--shell-unfontify
    (let ((buffer (or buffer (current-buffer)))
	  done)
      (if (and
	   (buffer-live-p buffer)
	   (or
	    (eq major-mode 'py-python-shell-mode)
	    (eq major-mode 'py-ipython-shell-mode)))
	  (unless py--timer
	    (setq py--timer
		  (run-with-idle-timer
		   (if py--timer-delay (setq py--timer-delay 3)
		     (setq py--timer-delay 0.1))
		   nil
		   #'py--unfontify-banner buffer)))
	(cancel-timer py--timer)))))

;;  unconditional Hooks
;;  (orgstruct-mode 1)
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function py--imenu-create-index-function)
	    (setq indent-tabs-mode py-indent-tabs-mode)))

(remove-hook 'python-mode-hook 'python-setup-brm)
;; ;

(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; don't try completion if buffer wasn't modified
    (unless (eq modified py-complete-last-modified)
      (if py-auto-completion-mode-p
	  (if (string= "*PythonCompletions*" (buffer-name (current-buffer)))
	      (sit-for 0.1 t)
	    (if
		(eq py-auto-completion-buffer (current-buffer))
		;; not after whitespace, TAB or newline
		(unless (member (char-before) (list 32 9 10))
		  (py-complete)
		  (setq py-complete-last-modified (buffer-chars-modified-tick)))
	      (setq py-auto-completion-mode-p nil
		    py-auto-completion-buffer nil)
	      (cancel-timer py--auto-complete-timer)))))))

(defun py-set-command-args (arguments)
  "Set Python arguments on the fly, override defaults in this session.

Use `defcustom' to keep value across sessions "
  (interactive
   (list
    (read-from-minibuffer "Command args: " py-python-command-args)))
    (setq py-python-command-args arguments))

(defun py---emacs-version-greater-23 ()
  "Return `t' if emacs major version is above 23"
  (< 23 (string-to-number (car (split-string emacs-version "\\.")))))

(defun py-beginning-of-commented-section (&optional last)
  "Leave upwards comments and/or empty lines. "
  (interactive)
  (let ((pps (syntax-ppss))
        (last (or last (point))))
    (if (and (or (and (nth 4 pps)(goto-char (nth 8 pps)))(looking-at comment-start))
             (looking-back "^[ \t]*")(not (bobp)))
        (progn
          (skip-chars-backward " \t\r\n\f")
          (py-beginning-of-commented-section last))
      (goto-char last))))

(defun py--empty-arglist-indent (nesting py-indent-offset indent-offset)
  "Internally used by `py-compute-indentation'"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (+ (current-column) (* 2 (or indent-offset py-indent-offset))))
    (+ (current-indentation) py-indent-offset)))

(defun py-symbol-at-point ()
  "Return the current Python symbol."
  (interactive)
  (let ((erg (with-syntax-table
                 py-dotted-expression-syntax-table
               (current-word))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  (interactive
   (list (current-buffer)))
  (let ((buffer (or (and (bufferp buffer) buffer)
		    (get-buffer buffer)))
	proc kill-buffer-query-functions)

    (ignore-errors
      (setq proc (get-buffer-process buffer))
      (and proc (kill-process proc))
      (set-buffer buffer)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun py--line-backward-maybe ()
  (let ((orig (point)))
    (skip-chars-backward " \t\f" (line-beginning-position))
    (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
      (setq line t))))

(defun py--after-empty-line ()
  "Return `t' if line before contains only whitespace characters. "
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line nesting repeat indent-offset liep)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally:
ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING tells repeated executing was started from inside a list
REPEAT counter enables checks against `py-max-specpdl-size'
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      ;; in shell, narrow from previous prompt
      ;; needed by closing
      (unless orig (unless (bobp) (back-to-indentation)))
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines (point-min) (point))))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (liep (or liep (line-end-position)))
             (line line)
             (pps (syntax-ppss))
             (closing
              (or closing
                  (and (nth 1 pps)
                       (looking-at ".*\\(\\s)\\)")(nth 0 pps)
                       ;; char doesn't matter for now, maybe drop
                       (string-to-char (match-string-no-properties 1)))))
             ;; in a recursive call already
             (repeat (if repeat
			 (setq repeat (1+ repeat))
		       0))
             ;; nesting: started nesting a list
             (nesting nesting)
             (indent-offset (or indent-offset py-indent-offset))
             (cubuf (current-buffer))
             erg indent this-line)
        (if (and (< repeat 1)
                 (and (comint-check-proc (current-buffer))
                      (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1)))
            ;; common recursion not suitable because of prompt
            (with-temp-buffer
	      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
              (insert-buffer-substring cubuf (match-end 0) orig)
              (setq indent (py-compute-indentation)))
	  (if (< py-max-specpdl-size repeat)
	      (error "`py-compute-indentation' reached loops max.")
	    (setq nesting (nth 0 pps))
	    (setq indent
		  (cond
		   ((and (bobp) (eq liep (line-end-position)))
		    0)
		   ((and (bobp)(py--statement-opens-block-p py-extended-block-or-clause-re))
		    (+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation)))
		   ((and (bobp)(not (py--statement-opens-block-p py-extended-block-or-clause-re)))
		    (current-indentation))
		   ;; in string
		   ((and (nth 3 pps)(nth 8 pps))
		    (if
			;; still at original line
			(eq origline (line-end-position))
			(progn
			  (forward-line -1)
			  (end-of-line)
			  (skip-chars-backward " \t\r\n\f")
			  (if (ignore-errors (< (nth 8 (syntax-ppss)) (line-beginning-position)))
			      (current-indentation)
			    (ignore-errors (goto-char (nth 8 pps)))
			    (py--line-backward-maybe)
			    (back-to-indentation)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		      (goto-char (nth 8 pps))
		      (current-indentation)))
		   ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ;; comments
		   ((nth 8 pps)
		    (if (eq liep (line-end-position))
			(progn
			  (goto-char (nth 8 pps))
			  (py--line-backward-maybe)
			  (skip-chars-backward " \t")
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (goto-char (nth 8 pps))
		      (if
			  line
			  (if py-indent-honors-inline-comment
			      (current-column)
			    (if py-indent-comments
				(progn
				  (py-beginning-of-commented-section)
				  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			      0))
			(forward-char -1)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not line)
			 (eq liep (line-end-position)))
		    (if py-indent-comments
			(progn
			  (setq line t)
			  (skip-chars-backward " \t\r\n\f")
			  ;; as previous comment-line might
			  ;; be wrongly unindented, travel
			  ;; whole commented section
			  (py-beginning-of-commented-section)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      0))
		   ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not
									 (eq liep (line-end-position))))
		    (current-indentation))
		   ((and (eq ?\# (char-after)) line py-indent-honors-inline-comment)
		    (current-column))
		   ;; lists
		   ((nth 1 pps)
		    (if
			;; ((and nesting (not line))
			nesting
			;; still at original line
			(save-excursion
			  (goto-char (nth 1 pps))
			  (setq this-line (py-count-lines))
			  (cond
			   ((< 0 (- origline this-line))
			    (if (< 1 (- origline this-line))
				(cond
				 (closing
				  (cond
				   (py-closing-list-dedents-bos
				    (goto-char (nth 1 pps))
				    (current-indentation))
				   ((looking-back "^[ \t]*")
				    (current-column))
				   ((and (looking-at "\\s([ \t]*$") py-closing-list-keeps-space)
				    (+ (current-column) py-closing-list-space))
				   ((looking-at "\\s([ \t]*$")
				    (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				   ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
				    (goto-char (match-beginning 1))
				    (if py-indent-paren-spanned-multilines-p
					(+ (current-column) py-indent-offset)
				      (current-column)))
				   (t (py--fetch-previous-indent orig))))
				 ;; already behind a dedented element in list
				 ((<= 2 (- origline this-line))
				  (py--fetch-previous-indent orig))
				 ((< (current-indentation) (current-column))
				  (+ (current-indentation) py-indent-offset))
				 (t (py--fetch-previous-indent orig)))
			      (cond ((looking-at "\\s([ \t]*$")
				     (py--empty-arglist-indent nesting py-indent-offset indent-offset))
				    ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
				     (goto-char (match-beginning 1))
				     (if py-indent-paren-spanned-multilines-p
					 (+ (current-column) py-indent-offset)
				       (current-column)))
				    (t (+ (current-column) (* (nth 0 pps)))))))
			   ((nth 1 (syntax-ppss))
			    (goto-char (nth 1 (syntax-ppss)))
			    (setq line
				  ;; should be faster
				  (< (line-end-position) liep))
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			   ((not (py--beginning-of-statement-p))
			    (py-beginning-of-statement)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			   (t (1+ (current-column)))))
		      (if line
			  (progn
			    (py-beginning-of-statement)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
			(when (looking-at "[ \t]+")
			  (goto-char (match-end 0)))
			(current-column))))
		   ((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
		    (1+ (current-column)))
		   ((py-preceding-line-backslashed-p)
		    (progn
		      (py-beginning-of-statement)
		      (setq this-line (py-count-lines))
		      (if (< 1 (- origline this-line))
			  (py--fetch-previous-indent orig)
			(if (looking-at "from +\\([^ \t\n]+\\) +import")
			    py-backslashed-lines-indent-offset
			  (+ (current-indentation) py-continuation-offset)))))
		   ((and (looking-at py-block-closing-keywords-re)
			 (eq liep (line-end-position)))
		    (skip-chars-backward "[ \t\r\n\f]")
		    (py-beginning-of-statement)
		    (cond ((looking-at py-extended-block-or-clause-re)
			   (+
			    (if py-smart-indentation (py-guess-indent-offset) indent-offset)
			    (current-indentation)))
			  ((looking-at py-block-closing-keywords-re)
			   (- (current-indentation) py-indent-offset))
			  (t (current-column))))
		   ((looking-at py-block-closing-keywords-re)
		    (if (< (line-end-position) orig)
			(- (current-indentation) py-indent-offset)
		      (py-beginning-of-block-or-clause (current-indentation))
		      (current-indentation)))
		   ((looking-at py-no-outdent-re)
		    (if
			(eq liep (line-end-position))
			(progn
			  (back-to-indentation)
			  (py--line-backward-maybe)
			  (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		      (current-indentation)))
		   ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
		    (py--line-backward-maybe)
		    (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
		   ((and (looking-at py-clause-re)(not line)
			 (eq liep (line-end-position)))
		    (cond ((looking-at py-finally-re)
			   (car (py--clause-lookup-keyword py-finally-re -1 nil orig origline)))
			  ((looking-at py-except-re)
			   (car (py--clause-lookup-keyword py-except-re -1 nil orig origline)))
			  ((looking-at py-else-re)
			   (car (py--clause-lookup-keyword py-else-re -1 nil orig origline)))
			  ((looking-at py-elif-re)
			   (car (py--clause-lookup-keyword py-elif-re -1 nil orig origline)))
			  ;; maybe at if, try, with
			  (t (car (py--clause-lookup-keyword py-block-or-clause-re -1 nil orig origline)))))
		   ((looking-at py-extended-block-or-clause-re)
		    (cond ((and (not line)
				(eq liep (line-end-position)))
			   (py--line-backward-maybe)
			   (setq line t)
			   (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			  (t (+
			      (cond (indent-offset)
				    (py-smart-indentation
				     (py-guess-indent-offset))
				    (t py-indent-offset))
			      (current-indentation)))))
		   ((and
		     (< (line-end-position) liep)
		     (eq (current-column) (current-indentation)))
		    (and
		     (looking-at py-assignment-re)
		     (goto-char (match-end 0)))
		    ;; multiline-assignment
		    (if (and nesting (looking-at " *[[{(]")(not (looking-at ".+[]})][ \t]*$")))
			(+ (current-indentation) py-indent-offset)
		      (current-indentation)))
		   ((looking-at py-assignment-re)
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (< (current-indentation) (current-column))(not line))
		    (back-to-indentation)
		    (unless line
		      (setq nesting (nth 0 (syntax-ppss))))
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   ((and (not (py--beginning-of-statement-p)) (not (and line (eq ?\# (char-after)))))
		    (if (bobp)
			(current-column)
		      (if (eq (point) orig)
			  (progn
			    (py--line-backward-maybe)
			    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
			(py-beginning-of-statement)
			(py-compute-indentation orig origline closing line nesting repeat indent-offset liep))))
		   ((or (py--statement-opens-block-p py-extended-block-or-clause-re)(looking-at "@"))
		    (if (< (py-count-lines) origline)
			(+ (if py-smart-indentation (py-guess-indent-offset) indent-offset) (current-indentation))
		      (skip-chars-backward " \t\r\n\f")
		      (setq line t)
		      (back-to-indentation)
		      (py-compute-indentation orig origline closing line nesting repeat indent-offset liep)))
		   ((and py-empty-line-closes-p (py--after-empty-line))
		    (progn (py-beginning-of-statement)
			   (- (current-indentation) py-indent-offset)))
		   ;; still at orignial line
		   ((and (eq liep (line-end-position))
			 (save-excursion
			   (and (setq erg (py--go-to-keyword py-extended-block-or-clause-re))
				(if py-smart-indentation (setq indent-offset (py-guess-indent-offset)) t)
				(ignore-errors (< orig (or (py-end-of-block-or-clause)(point)))))))
		    (+ (car erg) (if py-smart-indentation
				     (or indent (py-guess-indent-offset))
				   indent-offset)))
		   ((and (not line)
			 (eq liep (line-end-position))
			 (py--beginning-of-statement-p))
		    (py-beginning-of-statement)
		    (py-compute-indentation orig origline closing line nesting repeat indent-offset liep))
		   (t (current-indentation))))
	    (when (and py-verbose-p (interactive-p)) (message "%s" indent))
	    indent))))))

(defun py--fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (let ((erg (if (eq 1 arg)
                 py-continuation-offset
               (when (numberp arg)
                 (prog1
                     arg
                   (setq py-continuation-offset arg))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" py-continuation-offset))
    py-continuation-offset))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg (nth 1 (syntax-ppss))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (syntax-ppss))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" end))
    end))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps (syntax-ppss))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (syntax-ppss))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (syntax-ppss))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun py-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive)
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
	   (erg (when (nth 3 pps)
		  (py-in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"\\|'")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (py-in-string-p-intern pps)))))

    ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg)))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (interactive-p) (message "%s" erg))
        erg))))

;;  Beginning-of- p
(defun py-beginning-of-top-level-p ()
  "Returns position, if cursor is at the beginning of a top-level, nil otherwise. "
  (interactive)
  (let (erg)
    (and (py--beginning-of-statement-p)
         (eq 0 (current-column))
         (setq erg (point))
      erg)))

(defun py--beginning-of-line-p ()
  "Returns position, if cursor is at the beginning of a line, nil otherwise. "
  (when (bolp)(point)))

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer, nil otherwise. "
  (when (bobp)(point)))

(defun py--beginning-of-paragraph-p ()
  "Returns position, if cursor is at the beginning of a paragraph, nil otherwise. "
  (let ((orig (point))
        erg)
    (if (and (bolp) (looking-at paragraph-separate))
        (setq erg (point))
      (save-excursion
        (py-end-of-paragraph)
        (py-beginning-of-paragraph)
        (when (eq orig (point))
          (setq erg orig)))
      erg)))

;;  End-of- p
(defun py--end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py--end-of-paragraph-p ()
  "Returns position, if cursor is at the end of a paragraph, nil otherwise. "
  (let ((orig (point))
         erg)
     (if (and (eolp) (looking-at paragraph-separate))
         (setq erg (point))
     (save-excursion
       (py-beginning-of-paragraph)
       (py-end-of-paragraph)
       (when (eq orig (point))
         (setq erg orig)))
       erg)))

;;  Opens
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*")(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py--statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-clause-re))

(defun py--statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-block-or-clause-re))

(defun py--statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-class-re))

(defun py--statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-re))

(defun py--statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (py--statement-opens-base py-def-or-class-re))

(defun py--record-list-error (pps)
  "When encountering a missing parenthesis, store its line, position. `py-verbose-p'  must be t

Unclosed-string errors are not handled here, as made visible by fontification already.
"
  (let ((this-err
         (save-excursion
           (list
            (nth 1 pps)
            (progn
              (goto-char (nth 1 pps))
              (py-count-lines (point-min) (point)))))))
    this-err))

(defun py--message-error (err)
  "Receives a list (position line) "
  (message "Closing paren missed: line %s pos %s" (cadr err) (car err)))

;;  py-look-downward-for-clause
(defun py--end-base (regexp &optional orig decorator)
  "Used internal by functions going to the end forms. "
  (unless (eobp)
    (catch 'exit
      (let* ((orig (or orig (point)))
             (regexp (or regexp 'py-extended-block-or-clause-re))
             (thisregexp
              (cond ((eq regexp 'py-def-or-class-re)
                     py-def-or-class-re)
                    ((eq regexp 'py-def-re)
                     py-def-re)
		    ((eq regexp 'py-class-re)
		     py-class-re)
		    ((eq regexp 'py-minor-block-re)
		     py-minor-block-re)
		    (t py-extended-block-or-clause-re)))
             bofst
             (this (progn (back-to-indentation)
                          (setq bofst (py--beginning-of-statement-p))
                          (cond ((and bofst (eq regexp 'py-clause-re)(looking-at py-extended-block-or-clause-re))
                                 (point))
                                ((and bofst (looking-at thisregexp))
                                 (point))
                                (t
                                 (when
                                     (cdr-safe
                                      (py--go-to-keyword
                                       thisregexp))
                                   (when (py--statement-opens-block-p py-extended-block-or-clause-re)
                                     (point)))))))
             ind erg last pps thisindent done err)
        (cond (this
               (setq thisindent (current-indentation))
               (cond ((and py-close-provides-newline
                           (or (eq regexp 'py-def-re)(eq regexp 'py-class-re)(eq regexp 'py-def-or-class-re)))
                      (while
                          (and
                           ;; lp:1294478 py-mark-def hangs
                           (if last
                               (if (< last (point))
                                   t
                                 (when (nth 1 pps)
                                   (if py-verbose-p
                                       (throw 'exit (py--message-error (py--record-list-error pps)))
                                     (throw 'exit nil))))

                             t)
                           (setq last (point))(re-search-forward "^$" nil t)(skip-chars-forward " \t\r\n\f")(or (nth 8 (setq pps (syntax-ppss))) (nth 1 pps) (< thisindent (current-column)))))
                      ;; (goto-char last)
                      (skip-chars-backward " \t\r\n\f")
                      (setq done t)
                      (and (nth 8 (setq pps (syntax-ppss)))
                           (py-beginning-of-statement)
                           (py-end-of-statement)))
                     (t (while
                            (and (py-down-statement)
                                 (or (< thisindent (current-indentation))
                                     (and (eq thisindent (current-indentation))
                                          (or (eq regexp 'py-minor-block-re)
                                              (eq regexp 'py-block-re))
                                          (looking-at py-clause-re)))
                                 (py-end-of-statement)(setq last (point))))
                        (and last (goto-char last)))))
              (t (goto-char orig)))
        (when (and (<= (point) orig)(not (looking-at thisregexp)))
          ;; found the end above
          ;; py--travel-current-indent will stop of clause at equal indent
          (when (py--look-downward-for-beginning thisregexp)
            (py--end-base regexp orig)))
        (setq pps (syntax-ppss))
        ;; (catch 'exit)
        (and err py-verbose-p (py--message-error err))
        (if (and (< orig (point)) (not (or (looking-at comment-start) (nth 8 pps) (nth 1 pps))))
            (point)
          (goto-char (point-max))
          nil)))))

(defun py--look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let* ((orig (point))
         (erg orig)
         (last orig)
         pps)
    (while (and (setq last (point)) (not (eobp)) (re-search-forward regexp nil t 1)(setq erg (match-beginning 0)) (setq pps (syntax-ppss))
                (or (nth 8 pps) (nth 1 pps))))
    (cond ((not (or (nth 8 pps) (nth 1 pps) (or (looking-at comment-start))))
           (when (ignore-errors (< orig erg))
             erg)))))

(defun py-look-downward-for-clause (&optional ind orig regexp)
  "If beginning of other clause exists downward in current block.

If succesful return position. "
  (interactive)
  (unless (eobp)
    (let ((ind (or ind
                   (save-excursion
                     (py-beginning-of-statement)
                     (if (py--statement-opens-block-p)
                         (current-indentation)
                       (- (current-indentation) py-indent-offset)))))
          (orig (or orig (point)))
          (regexp (or regexp py-extended-block-or-clause-re))
          erg last)
      (end-of-line)
      (when (re-search-forward regexp nil t 1)
        (when (nth 8 (syntax-ppss))
          (while (and (re-search-forward regexp nil t 1)
                      (nth 8 (syntax-ppss)))))
        (setq last (point))
        (back-to-indentation)
        (unless (and (looking-at py-clause-re)
                     (not (nth 8 (syntax-ppss))) (eq (current-indentation) ind))
          (progn (setq ind (current-indentation))
                 (while (and (py-end-of-statement-bol)(not (looking-at py-clause-re))(<= ind (current-indentation)))))
          (if (and (looking-at py-clause-re)
                   (not (nth 8 (syntax-ppss)))
                   (< orig (point)))
              (setq erg (point))
            (goto-char orig))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-current-defun (&optional iact)
  "Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-beginning-of-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
                   (exchange-point-and-mark)
                   (sit-for py-current-defun-delay)))
        (when iact (message (prin1-to-string erg)))
        erg))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py--join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py--in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (syntax-ppss)))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defun py-which-function ()
  "Return the name of the function or class, if curser is in, return nil otherwise. "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((orig (point))
            (erg (if (and (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+")) (not (py-in-string-or-comment-p)))
                     (match-string-no-properties 2)
                   (progn
                     (py-beginning-of-def-or-class)
                     (when (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+"))
                       (match-string-no-properties 2))))))
        (if (and erg (< orig (py-end-of-def-or-class)))
            (when (interactive-p) (message "%s" erg))
          (setq erg nil)
          (when (interactive-p) (message "%s" "Not inside a function or class"))
          erg)))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
http://launchpad.net/python-mode
is preferable for that. ")

;;  Utilities
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
               ((eq position 'bod) (py-beginning-of-def-or-class))
               ((eq position 'eod) (py-end-of-def-or-class))
               ;; Kind of funny, I know, but useful for py-up-exception.
               ((eq position 'bob) (goto-char (point-min)))
               ((eq position 'eob) (goto-char (point-max)))
               ((eq position 'boi) (back-to-indentation))
               ((eq position 'bos) (py-beginning-of-statement))
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))

(defun py-install-search-local ()
  (interactive)
  (let ((erg (split-string (shell-command-to-string (concat "find " default-directory " -maxdepth 9 -type f -name \"*python\"")))))))

;;  (defun py-install-local-epdfree ()
;;    (interactive)
;;    (py-install-local-shells "MY-PATH/epdfree"))

(defun py-install-local-shells (&optional local path-prefix)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. "
  (interactive)
  (let* ((local-dir (if local
                        (expand-file-name local)
                      (read-from-minibuffer "Virtualenv directory: " default-directory)))
         (path-separator (if (string-match "/" local-dir)
                             "/"
                           "\\" t))
         (shells (split-string (shell-command-to-string (concat "find " local-dir " -maxdepth 9 -type f -executable -name \"*python\""))))
         erg newshell prefix akt end orig curexe aktpath)
    (set-buffer (get-buffer-create py-extensions))
    (erase-buffer)
    (dolist (elt shells)
      (setq prefix "")
      (setq curexe (substring elt (1+ (string-match "/[^/]+$" elt))))
      (setq aktpath (substring elt 0 (1+ (string-match "/[^/]+$" elt))))
      (dolist (prf (split-string aktpath (regexp-quote path-separator)))
        (unless (string= "" prf)
          (setq prefix (concat prefix (substring prf 0 1)))))
      (setq orig (point))
      (insert py-shell-template)
      (setq end (point))
      (goto-char orig)
      (when (re-search-forward "\\<NAME\\>" end t 1)
        (replace-match (concat prefix "-" (substring elt (1+ (save-match-data (string-match "/[^/]+$" elt)))))t))
      (goto-char orig)
      (while (search-forward "DOCNAME" end t 1)
        (replace-match (if (string= "ipython" curexe)
                           "IPython"
                         (capitalize curexe)) t))
      (goto-char orig)
      (when (search-forward "FULLNAME" end t 1)
        (replace-match elt t))
      (goto-char (point-max)))
    (emacs-lisp-mode)
    (if (file-readable-p (concat py-install-directory "/" py-extensions))
        (find-file (concat py-install-directory "/" py-extensions)))))

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
  ;; (when py-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when py-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "py-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

;;  (goto-char (match-end 0))
;;  (search-forward (match-string-no-properties 0))))

(defun py--until-found (search-string liste)
  "Search liste for search-string until found. "
  (let ((liste liste) element)
    (while liste
      (if (member search-string (car liste))
          (setq element (car liste) liste nil))
      (setq liste (cdr liste)))
    (when element
      (while (and element (not (numberp element)))
        (if (member search-string (car element))
            (setq element (car element))
          (setq element (cdr element))))
      element)))

;;  (defun py-shell-send-string (string &optional process msg filename)
;;    "Send STRING to Python PROCESS.
;;  When `py-verbose-p' and MSG is non-nil messages the first line of STRING."
;;    (interactive "sPython command: ")
;;    (let* ((process (or process (get-buffer-process (py-shell))))
;;           (lines (split-string string "\n"))
;;           (temp-file-name (concat (with-current-buffer (process-buffer process)
;;                                     (file-remote-p default-directory))
;;                                   (py--normalize-directory py-temp-directory)
;;  				 ;; (md5 (user-login-name))
;;                                   (md5 (concat (user-login-name)(prin1-to-string (current-time))))
;;  				 "-psss-temp.py"))
;;           (file-name (or filename (buffer-file-name) temp-file-name)))
;;      (if (> (length lines) 1)
;;  	(with-temp-file temp-file-name
;;  	  (insert string)
;;  	  (delete-trailing-whitespace)
;;  	  (py-send-file temp-file-name process temp-file-name))
;;        (comint-send-string process string)
;;        (when (or (not (string-match "\n$" string))
;;                  (string-match "\n[ \t].*\n?$" string))
;;          (comint-send-string process "\n")))
;;      (unless py-debug-p (when (file-readable-p temp-file-name)(delete-file temp-file-name)))))

(defun py--delay-process-dependent (process)
  "Call a `py-ipython-send-delay' or `py-python-send-delay' according to process"
  (if (string-match "ipython" (prin1-to-string process))
      (sit-for py-ipython-send-delay t)
    (sit-for py-python-send-delay t)))

(defun py--send-string-no-output (string &optional process msg)
  "Send STRING to PROCESS and inhibit output display.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let* (output
         (process (or process (get-buffer-process (py-shell))))
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq output string)
                      "")))))
    (py-send-string string process)
    (sit-for 0.1 t)
    ;; (py--delay-process-dependent process)
    (when (and output (not (string= "" output)))
	    (py--string-strip
	     (format "[ \n]*%s[ \n]*" py-fast-filter-re)))))

(defun py--send-string-return-output (string &optional process msg)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (with-current-buffer (process-buffer process)
    (let* (erg
	   (process (or process (get-buffer-process (py-shell))))
	   (comint-preoutput-filter-functions
	    (append comint-preoutput-filter-functions
		    '(ansi-color-filter-apply
		      (lambda (string)
			(setq erg (concat erg string))
			"")))))
      (py-send-string string process)
      (accept-process-output process 5)
      (sit-for 0.1 t)
      (when (and erg (not (string= "" erg)))
	(setq erg
	      (replace-regexp-in-string
	       (format "[ \n]*%s[ \n]*" py-fast-filter-re)
	       "" erg)))
      ;; (sit-for 0.1 t)
      erg)))

(defun py-which-def-or-class ()
  "Returns concatenated `def' and `class' names in hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable `which-func-functions' "
  (interactive)
  (let* ((orig (point))
         (first t)
         def-or-class
         done last erg name)
    (and first (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")(not (nth 8 (syntax-ppss)))
         (add-to-list 'def-or-class (match-string-no-properties 2)))
    (while
        (and (not (bobp)) (not done) (or (< 0 (current-indentation)) first))
      (py-beginning-of-def-or-class)
      (looking-at "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]\\([[:alnum:]_]+\\)")
      (setq last (point))
      (setq name (match-string-no-properties 2))
      (if first
          (progn
            (setq first nil)
            (py-end-of-def-or-class)
            (if
                (<= orig (point))
                (goto-char last)
              (setq done t)
              (goto-char orig)))
        t)
      (unless done (add-to-list 'def-or-class name)))
    (unless done (setq def-or-class (mapconcat 'identity def-or-class ".")))
    (goto-char orig)
    (or def-or-class (setq def-or-class "???"))
    (when (interactive-p) (message "%s" def-or-class))
    def-or-class))

(defun py--beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let (erg)
    (unless (bobp)
      (let* ((orig (or orig (point)))
             (indent (or indent (progn
                                  (back-to-indentation)
                                  (or (py--beginning-of-statement-p)
                                      (py-beginning-of-statement))
                                  (current-indentation)))))
        (setq erg (cond ((and (< (point) orig) (looking-at (symbol-value regexp)))
                         (point))
                        ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                         (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                           (py-beginning-of-statement)
                           (unless (looking-at (symbol-value regexp))
                             (cdr (py--go-to-keyword (symbol-value regexp) (current-indentation))))))
                        ((numberp indent)
			 (cdr (py--go-to-keyword (symbol-value regexp) indent)))
                        (t (ignore-errors
                             (cdr (py--go-to-keyword (symbol-value regexp)
                                                    (- (progn (if (py--beginning-of-statement-p) (current-indentation) (save-excursion (py-beginning-of-statement) (current-indentation)))) py-indent-offset)))))))
        (when lc (beginning-of-line) (setq erg (point)))))
    (when (and py-verbose-p iact) (message "%s" erg))
    erg))

(defun py--beginning-of-prepare (indent final-re &optional inter-re iact lc)
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (py--beginning-of-statement-p)
                        (py-beginning-of-statement))
                    (cond ((eq 0 (current-indentation))
                           (current-indentation))
                          ((looking-at (symbol-value inter-re))
                           (current-indentation))
                          (t
                           (if (<= py-indent-offset (current-indentation))
                               (- (current-indentation) (if py-smart-indentation (py-guess-indent-offset) py-indent-offset))
                             py-indent-offset))))))
        erg)
    (if (and (< (point) orig) (looking-at (symbol-value final-re)))
        (progn
          (and lc (beginning-of-line))
          (setq erg (point))
          (when (and py-verbose-p iact) (message "%s" erg))
          erg)
      (py--beginning-of-form-intern final-re iact indent orig lc))))

(defun py--fetch-first-python-buffer ()
  "Returns first (I)Python-buffer found in `buffer-list'"
  (let ((buli (buffer-list))
        erg)
    (while (and buli (not erg))
      (if (string-match "Python" (prin1-to-string (car buli)))
          (setq erg (car buli))
        (setq buli (cdr buli))))
    erg))

(defun py-unload-python-el ()
  "Unloads python-mode delivered by shipped python.el

Removes python-skeleton forms from abbrevs.
These would interfere when inserting forms heading a block"
  (interactive)
  (let (done)
    (when (featurep 'python) (unload-feature 'python t))
    (when (file-readable-p abbrev-file-name)
      (find-file abbrev-file-name)
      (goto-char (point-min))
      (while (re-search-forward "^.+python-skeleton.+$" nil t 1)
	(setq done t)
	(delete-region (match-beginning 0) (1+ (match-end 0))))
      (when done (write-file abbrev-file-name)
	    ;; now reload
	    (read-abbrev-file abbrev-file-name))
      (kill-buffer (file-name-nondirectory abbrev-file-name)))))

(defmacro py--kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  `(let ((proc (get-buffer-process ,buffer))
	 kill-buffer-query-functions)
     (ignore-errors
       (and proc (kill-process proc))
       (set-buffer ,buffer)
       (set-buffer-modified-p 'nil)
       (kill-buffer (current-buffer)))))

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
	 (skip-chars-forward " \t" (line-end-position))
	 (setq done t)
	 (and (< (point) orig) (point)))))

(defun py--eos-in-string (pps)
  "Return stm, i.e. if string is part of a (print)-statement. "
  (let ((orig (point))
        pos stm)
    (goto-char (nth 8 pps))
    (unless (looking-back "^[ \t]*")
      (setq stm t))
    ;; go to end of string
    (and (member (char-after) (list ?' ?\"))
         (ignore-errors (setq pos (scan-sexps (point) 1)))
         (goto-char pos))
    ;; if no closing string delimiter, pos doesn't exist
    (unless (or stm (not pos))
      (setq done t)
      (unless (eq 10 (char-after))
        (and (< 0 (abs (skip-chars-forward "^;#" (line-end-position))))
             (eq ?\; (char-after))
             (skip-chars-forward ";"))))
    stm))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list ?# 10))(forward-line 1)(back-to-indentation))))

(defun py--skip-to-comment-or-semicolon ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point)))
    (cond ((and done (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
		(member (char-after) (list ?# ?\;)))
	   (when (eq ?\; (char-after))
	     (skip-chars-forward ";" (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defalias 'py-beginning-of-top-level-bol 'py-beginning-of-top-level)
(defun py-beginning-of-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg)
    (unless (bobp)
      (while (and (not (bobp)) (setq erg (py-beginning-of-statement))
                  (< 0 (current-indentation))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-end-of-top-level-bol 'py-end-of-top-level)
(defun py-end-of-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-beginning-of-statement))
      (unless (eq 0 (current-column))
        (py-beginning-of-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-end-of-def)))
            ((looking-at py-class-re)
             (setq erg (py-end-of-class)))
            ((looking-at py-block-re)
             (setq erg (py-end-of-block)))
             (t (setq erg (py-end-of-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-end-of-block))
          (setq erg (py-end-of-statement))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defalias 'py-end-of-top-level-lc 'py-end-of-top-level-bol)
(defun py-end-of-top-level-bol ()
  "Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let (erg)
    (py-end-of-top-level)
    (unless (or (eobp) (bolp))
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py-up (&optional indent)
  "Go up or to beginning of form if inside.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to beginning one level above of compound statement or definition at point.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p) t))
          (t (py-beginning-of-statement)))))

(defun py-down (&optional indent)

  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let* ((orig (point))
         erg
         (indent (if
                     (py--beginning-of-statement-p)
                     (current-indentation)
                   (progn
                     (py-beginning-of-statement)
                     (current-indentation))))
         last)
    (while (and (setq last (point)) (py-end-of-statement) (py-end-of-statement) (py-beginning-of-statement) (eq (current-indentation) indent)))
    (if (< indent (current-indentation))
        (setq erg (point))
      (goto-char last))
    (when (< (point) orig)
      (goto-char orig))
    (when (and (eq (point) orig)
               (progn (forward-char 1)
                      (skip-chars-forward "^\"'[({" (line-end-position))
                      (member (char-after) (list ?\( ?\" ?\' ?\[ ?\{)))
               (setq erg (point))))
    (unless erg
      (goto-char orig))
    (when (and py-verbose-p (interactive-p)) (message "%s" erg))
    erg))

(defun py--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form. "
  (when erg
    (unless (eobp)
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))))

(defun py--mark-base (form &optional py-mark-decorators)
  "Returns boundaries of FORM, a cons.

If PY-MARK-DECORATORS, `def'- and `class'-forms include decorators
If BOL is t, mark from beginning-of-line"
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and py-mark-decorators
         (and (setq erg (py-beginning-of-decorator))
              (setq beg erg)))
    (push-mark)
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
	(progn
	  (when (interactive-p) (message "%s %s" beg end))
	  (cons beg end))
      (when (interactive-p) (message "%s" "nil"))
      ;; (goto-char orig)
      nil)))

(defun py--mark-base-bol (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form "-bol")))
         (endform (intern-soft (concat "py-end-of-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator-bol))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

(defun py-mark-base (form &optional py-mark-decorators)
  "Calls py--mark-base, returns bounds of form, a cons. "
  (let* ((bounds (py--mark-base form py-mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

(defun py-beginning (&optional indent)
 "Go to beginning of compound statement or definition at point.

With \\[universal-argument], go to beginning one level above.
Returns position if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (py--beginning-of-form-intern py-extended-block-or-clause-re (interactive-p) indent))

(defun py-end (&optional indent)
 "Go to end of of compound statement or definition at point.

Returns position block if successful, nil otherwise

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
    (let* ((orig (point))
           (erg (py--end-base 'py-extended-block-or-clause-re orig)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg))

;;  Buffer
(defun py-beginning-of-buffer ()
  "Go to beginning-of-buffer, return position. "
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  "Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. "
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

(defun py-backward-same-level ()
  "Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((pps (syntax-ppss)))
    (cond ((nth 8 pps) (goto-char (nth 8 pps)))
          ((nth 1 pps) (goto-char (nth 1 pps)))
          ((py--beginning-of-statement-p) (py--beginning-of-form-intern 'py-extended-block-or-clause-re (interactive-p)))
          (t (py-beginning-of-statement)))))

(defun py--end-of-line-p ()
  "Returns position, if cursor is at the end of a line, nil otherwise. "
  (when (eolp)(point)))

(defun py--end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py--bounds-of-region ()
  "Returns bounds of region at point.

Returns a list, whose car is beg, cdr - end."
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (interactive-p) (message "%s" (list beg end)))
          (list beg end))))))

(defun py--beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
		 (py-beginning-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-paragraph)
		 (point))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--beginning-of-comment-position ()
  "Returns beginning of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-comment)))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

(defun py--end-of-comment-position ()
  "Returns end of comment position. "
  (interactive)
  (save-excursion
    (let ((erg (progn
                 (when (looking-at "[ \\t\\r\\n\\f]*$")
                   (skip-chars-backward " \t\r\n\f")
                   (forward-char -1))
                 (py-end-of-comment))))
      (when (and py-verbose-p (interactive-p)) (message "%s" erg))
      erg)))

;; from gud.el
;; (defun pdb (command-line)
;;   "Run pdb on program FILE in buffer `*gud-FILE*'.
;; The directory containing FILE becomes the initial working directory
;; and source-file directory for your debugger."
;;   (interactive
;;    (list (gud-query-cmdline 'pdb)))

(defun py--pdb-versioned ()
  "Guess existing pdb version from py-shell-name

Return \"pdb[VERSION]\" if executable found, just \"pdb\" otherwise"
  (interactive)
  (let ((erg (when (string-match "[23]" py-shell-name)
	       ;; versions-part
	       (substring py-shell-name (string-match "[23]" py-shell-name)))))
    (if erg
      (cond ((executable-find (concat "pdb" erg))
	     (concat "pdb" erg))
	    ((and (string-match "\\." erg)
		  (executable-find (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
	     (concat "pdb" (substring erg 0 (string-match "\\." erg)))))
      "pdb")))

(defun py-pdb (command-line)
  "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

At GNU Linux systems required pdb version should be detected by `py--pdb-version', at Windows configure `py-python-ms-pdb-command'

lp:963253"
  (interactive
   (list (gud-query-cmdline
	  (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
	      (car (read-from-string py-python-ms-pdb-command))
	    ;; sys.version_info[0]
	    (car (read-from-string (py--pdb-version)))) "asdf")))
  (pdb command-line (buffer-file-name)))

;; /usr/lib/python2.7/pdb.py eyp.py

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-goto-beyond-block 'py-end-of-block-bol)
(defalias 'py-goto-beyond-final-line 'py-end-of-statement-bol)
(defalias 'py-kill-minor-expression 'py-kill-partial-expression)
(defalias 'py-fast-send-string 'py-execute-string-fast)

(defalias 'Python 'python)
(defalias 'pyhotn 'python)
(defalias 'pyhton 'python)
(defalias 'pyt 'python)
(defalias 'Python2 'python2)
(defalias 'Python3 'python3)
(defalias 'IPython 'ipython)
(defalias 'Ipython 'ipython)
(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)

(provide 'python-components-intern)
;;;  python-components-intern.el ends here
