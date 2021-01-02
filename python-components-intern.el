;; python-components-intern.el --- Part of python-components-mode -*- lexical-binding: t; -*-

;; Helper functions

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

;;  Keymap

;;  Utility stuff

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


(defun py--uncomment-intern (beg end)
  (uncomment-region beg end)
  (when py-uncomment-indents-p
    (py-indent-region beg end)))

(defun py-uncomment (&optional beg)
  "Uncomment commented lines at point.

If region is active, restrict uncommenting at region "
  (interactive "*")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (let* (last
             (beg (or beg (save-excursion
                            (while (and (py-backward-comment) (setq last (point))(prog1 (forward-line -1)(end-of-line))))
                            last))))
        (and (py-forward-comment))
        (py--uncomment-intern beg (point))))))

(defun py-load-named-shells ()
  (interactive)
  (dolist (ele py-known-shells)
    (let ((erg (py-install-named-shells-fix-doc ele)))
      (eval (fset (car (read-from-string ele)) (car
						(read-from-string (concat "(lambda (&optional dedicated args) \"Start a ‘" erg "’ interpreter.
Optional DEDICATED: with \\\\[universal-argument] start in a new
dedicated shell.
Optional ARGS overriding ‘py-" ele "-command-args’.

Calls ‘py-shell’
\"
  (interactive \"p\") (py-shell dedicated args nil \""ele"\"))")))))))
  (when (functionp (car (read-from-string (car-safe py-known-shells))))
    (when py-verbose-p (message "py-load-named-shells: %s" "installed named-shells"))))

;; (py-load-named-shells)

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base file-name (get-buffer-process (get-buffer (py-shell)))))

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "P")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
                (get-buffer-process (buffer-name (current-buffer))))
               (t (py-shell argprompt)))))
    ;; (when (called-interactively-p 'any) (message "%S" erg))
    erg))

;;  Hooks
;;  arrange to kill temp files when Emacs exists

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




;;  FixMe: for unknown reasons this is not done by mode
;; (if (file-readable-p abbrev-file-name)
;;     (add-hook 'python-mode-hook
;;               (lambda ()
;;                 (setq py-this-abbrevs-changed abbrevs-changed)
;;                 (load abbrev-file-name nil t)
;;                 (setq abbrevs-changed py-this-abbrevs-changed)))
;;   (message "Warning: %s" "no abbrev-file found, customize `abbrev-file-name' in order to make mode-specific abbrevs work. "))

;; ;
;; (push (list
;;               'python-mode
;;               ;; start regex
;;               (concat (if py-hide-show-hide-docstrings
;;                           "^\\s-*\"\"\"\\|" "")
;;                       (mapconcat 'identity
;;                                  (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
;;                                          py-hide-show-keywords)
;;                                  "\\|"))
;;               ;; end regex
;;               nil
;;               ;; comment-start regex
;;               "#"
;;               ;; forward-sexp function
;;               (lambda ()
;;                 (py-forward-block-or-clause))
;;               nil) hs-special-modes-alist)

;; ;





(defun py--set-auto-fill-values ()
  "Internal use by `py--run-auto-fill-timer'"
  (let ((pps (parse-partial-sexp (point-min) (point))))
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

;;  unconditional Hooks
;;  (orgstruct-mode 1)
(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function py--imenu-create-index-function)
            (setq indent-tabs-mode py-indent-tabs-mode)))

(remove-hook 'python-mode-hook 'python-setup-brm)

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

(defun py--arglist-indent (nesting &optional indent-offset)
  "Internally used by `py-compute-indentation'"
  (if
      (and (eq 1 nesting)
           (save-excursion
             (back-to-indentation)
             (looking-at py-extended-block-or-clause-re)))
      (progn
        (back-to-indentation)
        (1+ (+ (current-column) (* 2 (or indent-offset py-indent-offset)))))
    (+ (current-indentation) (or indent-offset py-indent-offset))))

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

(defun py-kill-buffer-unconditional (buffer)
  "Kill buffer unconditional, kill buffer-process if existing. "
  (interactive
   (list (current-buffer)))
  (ignore-errors (with-current-buffer buffer
    (let (kill-buffer-query-functions)
      (set-buffer-modified-p nil)
      (ignore-errors (kill-process (get-buffer-process buffer)))
      (kill-buffer buffer)))))

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

(defun py--computer-closing-inner-list ()
  "Compute indentation according to py-closing-list-dedents-bos."
  (if py-closing-list-dedents-bos
      (+ (current-indentation) py-indent-offset)
    (1+ (current-column))))

(defun py--compute-closing-outer-list ()
  "Compute indentation according to py-closing-list-dedents-bos."
  (if py-closing-list-dedents-bos
      (current-indentation)
    (+ (current-indentation) py-indent-offset)))

(defun py-compute-indentation--according-to-list-style ()
   "See ‘py-indent-list-style’

Choices are:

'line-up-with-first-element (default)
'one-level-to-beginning-of-statement
'one-level-from-opener"
  (save-excursion
    (pcase py-indent-list-style
      (`line-up-with-first-element
       (1+ (current-column)))
      (`one-level-to-beginning-of-statement
       (py-backward-statement)
       (+ py-indent-offset (current-indentation)))
      (`one-level-from-first-element
       (+ 1 py-indent-offset (current-column))))))

(defun py-compute-indentation-closing-list (pps)
  (cond
   ((< 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    ;; reach the outer list
    (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
    (py--computer-closing-inner-list))
   ;; just close an maybe outer list
   ((eq 1 (nth 0 pps))
    (goto-char (nth 1 pps))
    (py-compute-indentation--according-to-list-style))))

(defun py-compute-list-indent--according-to-circumstance (pps line origline)
  (and (nth 1 pps) (goto-char (nth 1 pps)))
  (if (looking-at "[({\\[][ \t]*$")
      (+ (current-indentation) py-indent-offset)
    (if (or line (< (py-count-lines) origline))
	(py-compute-indentation--according-to-list-style))))

(defun py-compute-indentation-in-list (pps line closing orig origline)
  (if closing
      (py-compute-indentation-closing-list pps)
    (cond ((and (not line) (looking-back py-assignment-re (line-beginning-position)))
	   (py--fetch-indent-statement-above orig))
	  ;; (py-compute-indentation--according-to-list-style pps iact orig origline line nesting repeat indent-offset liep)
	  (t (when (looking-back "[ \t]*\\(\\s(\\)" (line-beginning-position))
	       (goto-char (match-beginning 1))
	       (setq pps (parse-partial-sexp (point-min) (point))))
	     (py-compute-list-indent--according-to-circumstance pps line origline)))))

(defun py-compute-comment-indentation (pps iact orig origline closing line nesting repeat indent-offset liep)
  (cond ((nth 8 pps)
         (goto-char (nth 8 pps))
         (cond ((and line (eq (current-column) (current-indentation)))
                (current-indentation))
               ((and (eq liep (line-end-position))py-indent-honors-inline-comment)
                (current-column))
               ((py--line-backward-maybe)
                (setq line t)
                (skip-chars-backward " \t")
                (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
               (t (if py-indent-comments
                      (progn
                        (py-backward-comment)
                        (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
                    0))))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))(not line)
          (eq liep (line-end-position)))
         (if py-indent-comments
             (progn
               (setq line t)
               (skip-chars-backward " \t\r\n\f")
               ;; as previous comment-line might
               ;; be wrongly unindented, travel
               ;; whole commented section
               (py-backward-comment)
               (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
           0))
        ((and
          (looking-at (concat "[ \t]*" comment-start))
          (looking-back "^[ \t]*" (line-beginning-position))
          (not (eq liep (line-end-position))))
         (current-indentation))
        ((and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment)
         (current-column))))

(defun py-close-according-to-style (pps)
  (goto-char (nth 1 pps))
  (if py-closing-list-dedents-bos
      (current-indentation)
    (pcase py-indent-list-style
      (`line-up-with-first-element
       (1+ (current-column)))
      (`one-level-to-beginning-of-statement
       (+ (current-indentation) py-indent-offset))
      (`one-level-from-first-element
       (+ 1 (current-column) py-indent-offset)))))

(defun py-compute-indentation--at-closer-maybe (pps)
  (save-excursion
    (when (looking-back "^[ \t]*\\(\\s)\\)" (line-beginning-position))
      (forward-char -1)
      (setq pps (parse-partial-sexp (point-min) (point))))
    (when (and (nth 1 pps)
               (looking-at "[ \t]*\\(\\s)\\)")(nth 0 pps))
      (if
          ;; beyond list start?
           (< (progn (unless (bobp) (forward-line -1) (line-beginning-position))) (nth 1 pps))
          (py-close-according-to-style pps)
        (if py-closing-list-dedents-bos
            (- (current-indentation) py-indent-offset)
          (current-indentation))))))

(defun py-compute-indentation (&optional iact orig origline closing line nesting repeat indent-offset liep)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting.

Optional arguments are flags resp. values set and used by `py-compute-indentation' internally:
ORIG keeps original position
ORIGLINE keeps line where compute started
CLOSING is t when started at a char delimiting a list as \"]})\"
LINE indicates being not at origline now
NESTING is currently ignored, if executing from inside a list
REPEAT counter enables checks against `py-max-specpdl-size'
INDENT-OFFSET allows calculation of block-local values
LIEP stores line-end-position at point-of-interest
"
  (interactive "p")
  (save-excursion
    (save-restriction
      (widen)
      ;; in shell, narrow from previous prompt
      ;; needed by closing
      (let* ((orig (or orig (copy-marker (point))))
             (origline (or origline (py-count-lines (point-min) (point))))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             ;; line: moved already a line backward
             (liep (or liep (line-end-position)))
	     (line (or line (not (eq origline (py-count-lines (point-min) (point))))))
             ;; (line line)
             (pps (progn
		    (unless (eq (current-indentation) (current-column))(skip-chars-backward " " (line-beginning-position)))
		    ;; (when (eq 5 (car (syntax-after (1- (point)))))
		    ;;   (forward-char -1))
		    (parse-partial-sexp (point-min) (point))))
             (closing
              (or closing
                  ;; returns update pps
                  (py-compute-indentation--at-closer-maybe pps)))
             ;; in a recursive call already
             (repeat (if repeat
                         (setq repeat (1+ repeat))
                       0))
             ;; nesting: started nesting a list
             (nesting nesting)
             (cubuf (current-buffer))
             erg indent this-line)
        (if (and (< repeat 1)
                 (and (comint-check-proc (current-buffer))
                      (re-search-backward (concat py-shell-prompt-regexp "\\|" py-ipython-output-prompt-re "\\|" py-ipython-input-prompt-re) nil t 1)))
            ;; common recursion not suitable because of prompt
            (with-temp-buffer
              ;; (switch-to-buffer (current-buffer))
              (insert-buffer-substring cubuf (match-end 0) orig)
              (python-mode)
              (setq indent (py-compute-indentation)))
          (if (< py-max-specpdl-size repeat)
              (error "`py-compute-indentation' reached loops max.")
            (setq nesting (nth 0 pps))
            (setq indent
                  (cond (closing)
                        ((bobp)
			 (cond ((eq liep (line-end-position))
                                0)
			       ;; - ((looking-at py-outdent-re)
			       ;; - (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
			       ((and line (looking-at py-block-or-clause-re))
				py-indent-offset)
                               ((looking-at py-outdent-re)
                                (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                               (t
                                (current-indentation))))
                        ;; (cond ((eq liep (line-end-position))
                        ;;        0)
                        ;;       ((looking-at py-outdent-re)
                        ;;        (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation)))
                        ;;       (t
                        ;;        (current-indentation)))
			;; in string
			((and (nth 3 pps) (nth 8 pps))
			 (cond
			  ((py--docstring-p (nth 8 pps))
			   (save-excursion
			     ;; (goto-char (match-beginning 0))
			     (back-to-indentation)
			     (if (looking-at "[uUrR]?\"\"\"\\|[uUrR]?'''")
				 (progn
				   (skip-chars-backward " \t\r\n\f")
				   (back-to-indentation)
				   (if (looking-at py-def-or-class-re)
				       (+ (current-column) py-indent-offset)
				     (current-indentation)))
			       (skip-chars-backward " \t\r\n\f")
			       (back-to-indentation)
			       (current-indentation))))
			  (t 0)))
			((and (looking-at "\"\"\"\\|'''") (not (bobp)))
			 (py-backward-statement)
			 (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			;; comments
			((or
			  (nth 8 pps)
			  (and
			   (looking-at (concat "[ \t]*" comment-start))
			   (looking-back "^[ \t]*" (line-beginning-position))(not line))
			  (and (eq 11 (syntax-after (point))) line py-indent-honors-inline-comment))
			 (py-compute-comment-indentation pps iact orig origline closing line nesting repeat indent-offset liep))
			;; lists
			((nth 1 pps)
			 (if (< (nth 1 pps) (line-beginning-position))
			     (py-compute-indentation-in-list pps line closing orig origline)
			   (back-to-indentation)
			   (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep)))
			((and (eq (char-after) (or ?\( ?\{ ?\[)) line)
			 (1+ (current-column)))
			((py-preceding-line-backslashed-p)
			 (progn
			   (py-backward-statement)
			   (setq this-line (py-count-lines))
			   (if (< 1 (- origline this-line))
                               (py--fetch-indent-line-above orig)
			     (if (looking-at "from +\\([^ \t\n]+\\) +import")
				 py-backslashed-lines-indent-offset
                               (+ (current-indentation) py-continuation-offset)))))
			((and (looking-at py-block-closing-keywords-re)
                              (eq liep (line-end-position)))
			 (skip-chars-backward "[ \t\r\n\f]")
			 (py-backward-statement)
			 (cond ((looking-at py-extended-block-or-clause-re)
				(+
				 ;; (if py-smart-indentation (py-guess-indent-offset) indent-offset)
				 (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset)
				 (current-indentation)))
                               ((looking-at py-block-closing-keywords-re)
				(- (current-indentation) (or indent-offset py-indent-offset)))
                               (t (current-column))))
			((looking-at py-block-closing-keywords-re)
			 (if (< (line-end-position) orig)
			     ;; #80, Lines after return cannot be correctly indented
			     (if (looking-at "return[ \\t]*$")
				 (current-indentation)
			       (- (current-indentation) (or indent-offset py-indent-offset)))
			   (py-backward-block-or-clause)
			   (current-indentation)))
			;; ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
			;; (when (py--line-backward-maybe) (setq line t))
			;; (car (py--clause-lookup-keyword py-elif-re -1 nil origline)))
			((and (looking-at py-clause-re) (not line)
                              (eq liep (line-end-position)))
			 (cond ((looking-at py-outdent-re)
				;; (and (py--backward-regexp 'py-block-or-clause-re) (current-indentation)))
			       	(and (py--go-to-keyword 'py-block-or-clause-re nil nil t) (current-indentation)))
			       ((bobp) 0)
			       (t (save-excursion (skip-chars-backward " \t\r\n\f")
						  (if
						      (py--backward-regexp 'py-block-or-clause-re)
						      (+ py-indent-offset (current-indentation))
						    0)))))
			((looking-at py-extended-block-or-clause-re)
			 (cond ((and (not line)
				     (eq liep (line-end-position)))
				(when (py--line-backward-maybe) (setq line t))
				(py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
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
			 (if (and nesting (looking-at " *[[{(]") (not (looking-at ".+[]})][ \t]*$")))
			     (+ (current-indentation) (or indent-offset py-indent-offset))
			   (current-indentation)))
			((looking-at py-assignment-re)
			 (py-backward-statement)
			 (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			((and (< (current-indentation) (current-column))(not line))
			 (back-to-indentation)
			 (unless line
			   (setq nesting (nth 0 (parse-partial-sexp (point-min) (point)))))
			 (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			((and (not (py--beginning-of-statement-p)) (not (and line (eq 11 (syntax-after (point))))))
			 (if (bobp)
			     (current-column)
			   (if (eq (point) orig)
                               (progn
				 (when (py--line-backward-maybe) (setq line t))
				 (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			     (py-backward-statement)
			     (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))))
			((or (py--statement-opens-block-p py-extended-block-or-clause-re) (looking-at "@"))
			 (if (< (py-count-lines) origline)
			     (+ (or indent-offset (and py-smart-indentation (py-guess-indent-offset)) py-indent-offset) (current-indentation))
			   (skip-chars-backward " \t\r\n\f")
			   (setq line t)
			   (back-to-indentation)
			   (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep)))
			((and py-empty-line-closes-p (py--after-empty-line))
			 (progn (py-backward-statement)
				(- (current-indentation) (or indent-offset py-indent-offset))))
			;; still at orignial line
			((and (eq liep (line-end-position))
                              (save-excursion
				(and (setq erg (py--go-to-keyword 'py-extended-block-or-clause-re (* py-indent-offset 99)))
				     (if (and (not indent-offset) py-smart-indentation) (setq indent-offset (py-guess-indent-offset)) t)
				     (ignore-errors (< orig (or (py-forward-block-or-clause) (point)))))))
			 (+ (car erg) (if py-smart-indentation
					  (or indent-offset (py-guess-indent-offset))
					(or indent-offset py-indent-offset))))
			((and (not line)
                              (eq liep (line-end-position))
                              (py--beginning-of-statement-p))
			 (py-backward-statement)
			 (py-compute-indentation iact orig origline closing line nesting repeat indent-offset liep))
			(t (current-indentation))))
            (when py-verbose-p (message "%s" indent))
            indent))))))



(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-forward-statement))
      (setq beg (py-backward-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (called-interactively-p 'any) (message "%s" erg))
        erg))))

;;  Beginning-of- p
(defun py-backward-top-level-p ()
  "Returns position, if cursor is at the beginning of a top-level, nil otherwise. "
  (interactive)
  (let (erg)
    (and (py--beginning-of-statement-p)
         (eq 0 (current-column))
         (setq erg (point))
      erg)))

(defun py--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer, nil otherwise. "
  (when (bobp)(point)))

;;  End-of- p

;;  Opens
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py--statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-forward-statement)
      (py-backward-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*" (line-beginning-position))(looking-at regexp))
        (setq erg (point))))
    (when (called-interactively-p 'any) (message "%s" erg))
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



(defun py--down-top-level (&optional regexp)
  "Go to the end of a top-level form.

When already at end, go to EOB."
  (end-of-line)
  (while (and (py--forward-regexp (or regexp "^[[:graph:]]"))
	      (save-excursion
		(beginning-of-line)
		(or
		 (looking-at py-clause-re)
		 (looking-at comment-start)))))
  (beginning-of-line)
  (and (looking-at regexp) (point)))

(defun py--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	  (py-backward-comment))))

(defun py--down-according-to-indent (regexp secondvalue &optional indent use-regexp)
  "Return position if moved, nil otherwise.

Optional ENFORCE-REGEXP: search for regexp only."
  (unless (eobp)
    (let* ((orig (point))
	   (indent (or indent 0))
	   done
	   (regexpvalue (if (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
			    (concat (symbol-value regexp) "\\|" (symbol-value 'py-decorator-re))
			    (symbol-value regexp)))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`py-try-re py-finally-re)
			     (`py-if-re py-else-re)))))
      (while
	  (and
	   (not done)
	   (progn (end-of-line)
		  (cond (use-regexp
			 ;; using regexpvalue might stop behind global settings, missing the end of form
			 (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
			(t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_@]+") nil 'move 1))))
	   (or (progn (back-to-indentation) (py--forward-string-maybe (nth 8 (parse-partial-sexp orig (point)))))
	       (and secondvalue (looking-at secondvalue))
	       (and lastvalue (looking-at lastvalue))
	       (and (looking-at regexpvalue) (setq done t))
	       ;; py-forward-def-or-class-test-3JzvVW
	       ;; (setq done t)
	       )))
      (and (< orig (point)) (point)))))

(defun py--end-of-paragraph (regexp)
  (let* ((regexp (if (symbolp regexp) (symbol-value regexp)
                   regexp)))
    (while (and (not (eobp)) (re-search-forward regexp nil 'move 1) (nth 8 (parse-partial-sexp (point-min) (point)))))))

(defun py--leave-backward-string-list-and-comment-maybe (pps)
  (while (or (and (nth 8 pps) (goto-char (nth 8 pps)))
             (and (nth 1 pps) (goto-char (nth 1 pps)))
             (and (nth 4 pps) (goto-char (nth 4 pps))))
    ;; (back-to-indentation)
    (when (or (looking-at comment-start)(member (char-after) (list ?\" ?')))
      (skip-chars-backward " \t\r\n\f"))
    (setq pps (parse-partial-sexp (point-min) (point)))))

(defun py--end-base-determine-secondvalue (regexp)
  "Expects being at block-opener.

REGEXP: a symbol"
  (cond
   ((eq regexp 'py-minor-block-re)
    (cond ((looking-at py-else-re)
	   nil)
	  ((or (looking-at (concat py-try-re)))
	   (concat py-elif-re "\\|" py-else-re "\\|" py-except-re))
	  ((or (looking-at (concat py-except-re "\\|" py-elif-re "\\|" py-if-re)))
	   (concat py-elif-re "\\|" py-else-re))))
   ((member regexp
	    (list
	     'py-block-re
	     'py-block-or-clause-re
	     'py-clause-re
	     'py-if-re
	     ))
    (cond ((looking-at py-if-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-elif-re)
	   (concat py-elif-re "\\|" py-else-re))
	  ((looking-at py-else-re))
	  ((looking-at py-try-re)
	   (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
	  ((looking-at py-except-re)
	   (concat py-else-re "\\|" py-finally-re))
	  ((looking-at py-finally-re)
	   nil)))
   ((eq regexp 'py-for-re) nil)
   ((eq regexp 'py-try-re)
    (cond
     ((looking-at py-try-re)
      (concat py-except-re "\\|" py-else-re "\\|" py-finally-re))
     ((looking-at py-except-re)
      (concat py-else-re "\\|" py-finally-re))
     ((looking-at py-finally-re))))))

(defun py--down-end-form ()
  "Return position."
  (progn (py--backward-empty-lines-or-comment)
	 (point)))

(defun py--refine-regexp-maybe (regexp)
  "Use a more specific regexp if possible. "
  (let ((regexpvalue (symbol-value regexp)))
    (if (looking-at regexpvalue)
	(setq regexp
	      (cond ((looking-at py-if-re)
		     'py-if-re)
		    ((looking-at py-try-re)
		     'py-try-re)
		    ((looking-at py-def-re)
		     'py-def-re)
		    ((looking-at py-class-re)
		     'py-class-re)
		    (t regexp)))
      regexp)))

(defun py--end-base (regexp &optional orig bol repeat)
  "Used internal by functions going to the end FORM.

Returns the indentation of FORM-start
Arg REGEXP, a symbol"
  (unless (eobp)
    (let (;; not looking for an assignment
	  (use-regexp (member regexp (list 'py-def-re 'py-class-re 'py-def-or-class-re)))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-beginning-of-statement))
	(let* (;; when at block-start, be specific
	       (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (symbol-value regexp))
               ;; (regexp (or regexp (symbol-value 'py-extended-block-or-clause-re)))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
			   (if (bolp) 0
			     (abs
			      (- (current-indentation) py-indent-offset)))
			 (current-indentation)))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (py-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list 'py-def-re 'py-def-or-class-re 'py-class-re))
				(looking-at py-decorator-re)
				(py-down-def-or-class (current-indentation)))
			   (and (member regexp (list 'py-minor-block-re 'py-if-re 'py-for-re 'py-try-re))
				(looking-at py-minor-clause-re))))
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) (py--end-base-determine-secondvalue regexp)))
		 ((eq 0 (current-indentation))
		  (py--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (py--go-to-keyword regexp))))
	       (secondvalue (ignore-errors (nth 2 res)))
	       erg)
	  ;; (py-for-block-p (looking-at py-for-re))
	  (setq indent (or (and res (car-safe res)) indent))
	  (cond
	   (res (setq erg
		      (and
		       (py--down-according-to-indent regexp secondvalue (current-indentation))
		       ;; (if (>= indent (current-indentation))
		       (py--down-end-form)
		       ;; (py--end-base regexp orig bol repeat)
		       ;; )
		       )))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (py--forward-regexp (symbol-value regexp))
	      (beginning-of-line)
	      (setq erg (and
			 (py--down-according-to-indent regexp secondvalue (current-indentation) t)
			 (py--down-end-form)))))
	  (cond ((< orig (point))
		 (setq erg (point))
		 (progn
		   (and erg bol (setq erg (py--beginning-of-line-form)))
		   (and erg (cons (current-indentation) erg))))
		((eq (point) orig)
		 (unless (eobp)
		   (cond
		    ((and (< repeat 1)
			  (or
			   ;; looking next indent as part of body
			   (py--down-according-to-indent regexp secondvalue
							 indent
							 ;; if expected indent is 0,
							 ;; search for new start,
							 ;; search for regexp only
							 (eq 0 indent))
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent py-indent-offset)) indent))
			    (py--down-according-to-indent regexp secondvalue
							  indent t))))
		     (py--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (py--down-according-to-indent regexp secondvalue nil t)
		   (py--end-base regexp (point) bol (1+ repeat))))))))))

(defun py--look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let* ((orig (point))
         (erg orig)
         pps)
    (while (and (not (eobp)) (re-search-forward regexp nil t 1) (setq erg (match-beginning 0)) (setq pps (parse-partial-sexp (point-min) (point)))
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
                     (py-backward-statement)
                     (if (py--statement-opens-block-p)
                         (current-indentation)
                       (- (current-indentation) py-indent-offset)))))
          (orig (or orig (point)))
          (regexp (or regexp py-extended-block-or-clause-re))
          erg)
      (end-of-line)
      (when (re-search-forward regexp nil t 1)
        (when (nth 8 (parse-partial-sexp (point-min) (point)))
          (while (and (re-search-forward regexp nil t 1)
                      (nth 8 (parse-partial-sexp (point-min) (point))))))
        ;; (setq last (point))
        (back-to-indentation)
        (unless (and (looking-at py-clause-re)
                     (not (nth 8 (parse-partial-sexp (point-min) (point)))) (eq (current-indentation) ind))
          (progn (setq ind (current-indentation))
                 (while (and (py-forward-statement-bol)(not (looking-at py-clause-re))(<= ind (current-indentation)))))
          (if (and (looking-at py-clause-re)
                   (not (nth 8 (parse-partial-sexp (point-min) (point))))
                   (< orig (point)))
              (setq erg (point))
            (goto-char orig))))
      (when (called-interactively-p 'any) (message "%s" erg))
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
      (let ((erg (when (py-backward-def-or-class)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show)
          (push-mark (point) t t) (skip-chars-forward "^ (")
          (exchange-point-and-mark)
          (sit-for py-current-defun-delay t))
        (when iact (message (prin1-to-string erg)))
        erg))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (ignore-errors (save-excursion (progn (up-list -1) (point)))))
          (close-paren (ignore-errors (save-excursion (progn (up-list 1) (point)))))
          sorted-imports)
      (when (and open-paren close-paren)
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
        (insert ")")))))

(defun py--in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

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
               ((eq position 'bod) (py-backward-def-or-class))
               ((eq position 'eod) (py-forward-def-or-class))
               ;; Kind of funny, I know, but useful for py-up-exception.
               ((eq position 'bob) (goto-char (point-min)))
               ((eq position 'eob) (goto-char (point-max)))
               ((eq position 'boi) (back-to-indentation))
               ((eq position 'bos) (py-backward-statement))
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))

(defun py-install-local-shells (&optional local)
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
         prefix end orig curexe aktpath)
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

(defun py--which-delay-process-dependent (buffer)
  "Call a `py-ipython-send-delay' or `py-python-send-delay' according to process"
  (if (string-match "^.[IJ]" buffer)
      py-ipython-send-delay
    py-python-send-delay))

(defun py-temp-file-name (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py")))

    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py--report-end-marker (process)
  ;; (message "py--report-end-marker in %s" (current-buffer))
  (if (derived-mode-p 'comint-mode)
      (if (bound-and-true-p comint-last-prompt)
	  (car-safe comint-last-prompt)
	(dotimes (_ 3) (when (not (bound-and-true-p comint-last-prompt)))(sit-for 1 t))
	(and (bound-and-true-p comint-last-prompt)
	     (car-safe comint-last-prompt)))
    (if (markerp (process-mark process))
	(process-mark process)
      (progn
	(dotimes (_ 3) (when (not (markerp (process-mark process)))(sit-for 1 t)))
	(process-mark process)))))

(defun py--filter-result (strg)
  "Set ‘py-result’ according to ‘py-fast-filter-re’.

Remove trailing newline"
  (string-trim
   (replace-regexp-in-string
    py-fast-filter-re
    ""
    (ansi-color-filter-apply strg))))

(defun py--cleanup-shell (orig buffer)
  (with-current-buffer buffer
    (with-silent-modifications
      (sit-for py-python3-send-delay)
      (when py--debug-p (switch-to-buffer (current-buffer)))
      (delete-region orig (point-max)))))

(defun py-shell--save-temp-file (strg)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (py-info-encoding)))
    (with-temp-file temp-file-name
      (insert strg)
      (delete-trailing-whitespace))
    temp-file-name))

(defun py-shell-send-string (strg &optional process)
  "Send STRING to Python PROCESS.

Uses ‘comint-send-string’."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (py-shell-get-process))))
    (if (string-match ".\n+." strg)   ;Multiline.
        (let* ((temp-file-name (py-shell--save-temp-file strg))
               (file-name (or (buffer-file-name) temp-file-name)))
          (py-shell-send-file file-name process temp-file-name t))
      (comint-send-string process strg)
      (when (or (not (string-match "\n\\'" strg))
                (string-match "\n[ \t].*\n?\\'" strg))
        (comint-send-string process "\n")))))

(defun py-shell-output-filter (strg)
  "Filter used in `py-shell-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`py-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (let ((py-shell--prompt-calculated-output-regexp
	 (or py-shell--prompt-calculated-output-regexp (py-shell-prompt-set-calculated-regexps))))
    (setq
     strg (ansi-color-filter-apply strg)
     py-shell-output-filter-buffer
     (concat py-shell-output-filter-buffer strg))
    (when (py-shell-comint-end-of-output-p
	   py-shell-output-filter-buffer)
      ;; Output ends when `py-shell-output-filter-buffer' contains
      ;; the prompt attached at the end of it.
      (setq py-shell-output-filter-in-progress nil
	    py-shell-output-filter-buffer
	    (substring py-shell-output-filter-buffer
		       0 (match-beginning 0)))
      (when (string-match
	     py-shell--prompt-calculated-output-regexp
	     py-shell-output-filter-buffer)
	;; Some shells, like IPython might append a prompt before the
	;; output, clean that.
	(setq py-shell-output-filter-buffer
	      (substring py-shell-output-filter-buffer (match-end 0)))))
    ""))

(defun py-send-string-no-output (strg &optional process buffer-name)
  "Send STRING to PROCESS and inhibit output.

Return the output."
  (let* ((proc (or process (py-shell-get-process)))
	 (buffer (or buffer-name (if proc (buffer-name (process-buffer proc)) (py-shell))))
         (comint-preoutput-filter-functions
          '(py-shell-output-filter))
         (py-shell-output-filter-in-progress t)
         (inhibit-quit t)
	 (delay (py--which-delay-process-dependent buffer)))
    (or
     (with-local-quit
       (if (and (string-match ".\n+." strg) (string-match "^\*[Ii]" buffer))  ;; IPython or multiline
           (let* ((temp-file-name (py-temp-file-name strg))
		  (file-name (or (buffer-file-name) temp-file-name)))
	     (py-execute-file file-name proc))
	 (py-shell-send-string strg proc))
       ;; (switch-to-buffer buffer)
       ;; (accept-process-output proc 9)
       (while py-shell-output-filter-in-progress
         ;; `py-shell-output-filter' takes care of setting
         ;; `py-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output proc delay))
       (prog1
           py-shell-output-filter-buffer
         (setq py-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer proc)
       (comint-interrupt-subjob)))))

(defun py-send-string (strg &optional process result no-output orig output-buffer fast argprompt args dedicated shell exception-buffer split switch internal)
  "Evaluate STRG in Python PROCESS.

With optional Arg PROCESS send to process.
With optional Arg RESULT store result in var ‘py-result’, also return it.
With optional Arg NO-OUTPUT don't display any output
With optional Arg ORIG deliver original position.
With optional Arg OUTPUT-BUFFER specify output-buffer"
  (interactive "sPython command: ")
  (save-excursion
    (let* ((buffer (or output-buffer (or (and process (buffer-name (process-buffer process))) (buffer-name (py-shell argprompt args dedicated shell output-buffer fast exception-buffer split switch internal)))))
	   (proc (or process (get-buffer-process buffer)))
	   ;; nil nil nil nil (buffer-name buffer))))
	   (orig (or orig (point)))
   	   (limit (ignore-errors (marker-position (process-mark proc)))))
      (cond ((and no-output fast)
	     (py--fast-send-string-no-output-intern strg proc limit buffer no-output))
	    (no-output
	     (py-send-string-no-output strg proc))
	    ((and (string-match ".\n+." strg) (string-match "^[Ii]"
							    ;; (buffer-name buffer)
							    buffer
							    ))  ;; multiline
	     (let* ((temp-file-name (py-temp-file-name strg))
		    (file-name (or (buffer-file-name) temp-file-name)))
	       (py-execute-file file-name proc)))
	    (t (with-current-buffer buffer
		 (comint-send-string proc strg)
		 (when (or (not (string-match "\n\\'" strg))
			   (string-match "\n[ \t].*\n?\\'" strg))
		   (comint-send-string proc "\n"))
		 (sit-for py-python-send-delay)
		 (cond (result
			(setq py-result
			      (py--fetch-result buffer limit strg)))
		       (no-output
			(and orig (py--cleanup-shell orig buffer))))))))))

;; (defun py-send-file (file-name process)
;;   "Send FILE-NAME to Python PROCESS."
;;   (interactive "fFile to send: ")
;;   (let* ((proc (or
;; 		   process (get-buffer-process (py-shell))))
;; 	 (file-name (expand-file-name file-name)))
;;     (py-send-string
;;      (format
;;       (concat "__pyfile = open('''%s''');"
;; 	      "exec(compile(__pyfile.read(), '''%s''', 'exec'));"
;; 	      "__pyfile.close()")
;;       file-name file-name)
;;      proc)))

(defun py-which-def-or-class (&optional orig)
  "Returns concatenated `def' and `class' names in hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable `which-func-functions' "
  (interactive)
  (let* ((orig (or orig (point)))
         (backindent 99999)
         (re py-def-or-class-re
          ;; (concat py-def-or-class-re "\\([[:alnum:]_]+\\)")
          )
         erg forward indent backward limit)
    (if
        (and (looking-at re)
             (not (nth 8 (parse-partial-sexp (point-min) (point)))))
        (progn
          (setq erg (list (match-string-no-properties 2)))
          (setq backindent (current-indentation)))
      ;; maybe inside a definition's symbol
      (or (eolp) (and (looking-at "[[:alnum:]]")(forward-word 1))))
    (if
        (and (not (and erg (eq 0 (current-indentation))))
             (setq limit (py-backward-top-level))
             (looking-at re))
        (progn
          (push (match-string-no-properties 2)  erg)
          (setq indent (current-indentation)))
      (goto-char orig)
      (while (and
              (re-search-backward py-def-or-class-re limit t 1)
              (< (current-indentation) backindent)
              (setq backindent (current-indentation))
              (setq backward (point))
              (or (< 0 (current-indentation))
                  (nth 8 (parse-partial-sexp (point-min) (point))))))
      (when (and backward
                 (goto-char backward)
                 (looking-at re))
        (push (match-string-no-properties 2)  erg)
        (setq indent (current-indentation))))
    ;; (goto-char orig))
    (if erg
        (progn
          (end-of-line)
          (while (and (re-search-forward py-def-or-class-re nil t 1)
                      (<= (point) orig)
                      (< indent (current-indentation))
                      (or
                       (nth 8 (parse-partial-sexp (point-min) (point)))
                       (setq forward (point)))))
          (if forward
              (progn
                (goto-char forward)
                (save-excursion
                  (back-to-indentation)
                  (and (looking-at re)
                       (setq erg (list (car erg) (match-string-no-properties 2)))
                       ;; (< (py-forward-def-or-class) orig)
                       ;; if match was beyond definition, nil
                       ;; (setq erg nil)
)))
            (goto-char orig))))
    (if erg
        (if (< 1 (length erg))
            (setq erg (mapconcat 'identity erg "."))
          (setq erg (car erg)))
      (setq erg "???"))
    (goto-char orig)
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

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

(defun py-backward-top-level ()
  "Go up to beginning of statments until level of indentation is null.

Returns position if successful, nil otherwise "
  (interactive)
  (let (erg done)
    (unless (bobp)
      (while (and (not done)(not (bobp))
                  (setq erg (re-search-backward "^[[:alpha:]_'\"]" nil t 1)))
        (if
            (nth 8 (parse-partial-sexp (point-min) (point)))
            (setq erg nil)
          (setq done t)))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

;; might be slow due to repeated calls of ‘py-down-statement’
(defun py-forward-top-level ()
  "Go to end of top-level form at point.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (unless (py--beginning-of-statement-p)
        (py-backward-statement))
      (unless (eq 0 (current-column))
        (py-backward-top-level))
      (cond ((looking-at py-def-re)
             (setq erg (py-forward-def)))
            ((looking-at py-class-re)
             (setq erg (py-forward-class)))
            ((looking-at py-block-re)
             (setq erg (py-forward-block)))
            (t (setq erg (py-forward-statement))))
      (unless (< orig (point))
        (while (and (not (eobp)) (py-down-statement)(< 0 (current-indentation))))
        (if (looking-at py-block-re)
            (setq erg (py-forward-block))
          (setq erg (py-forward-statement))))
      (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
      erg)))

(defun py-down-top-level ()
  "Go to beginning of next top-level form downward.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (while (and (not (eobp))
                (progn (end-of-line)
                       (re-search-forward "^[[:alpha:]_'\"]" nil 'move 1))
                (nth 8 (parse-partial-sexp (point-min) (point)))))
    (when (and (not (eobp)) (< orig (point)))
      (goto-char (match-beginning 0))
        (setq erg (point)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-top-level-bol ()
  "Go to end of top-level form at point, stop at next beginning-of-line.

Returns position successful, nil otherwise"
  (interactive)
  (let (erg)
    (py-forward-top-level)
    (unless (or (eobp) (bolp))
      (forward-line 1)
      (beginning-of-line)
      (setq erg (point)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-down (&optional indent)
  "Go to beginning one level below of compound statement or definition at point.

If no statement or block below, but a delimited form --string or list-- go to its beginning. Repeated call from there will behave like down-list.

Returns position if successful, nil otherwise"
  (interactive)
  (let* ((orig (point))
         erg
         (indent (or
                  indent
                  (if
                      (py--beginning-of-statement-p)
                      (current-indentation)
                    (progn
                      (py-backward-statement)
                      (current-indentation))))))
    (while (and (py-forward-statement) (py-forward-statement) (py-backward-statement) (> (current-indentation) indent)))
    (cond ((= indent (current-indentation))
           (setq erg (point)))
          ((< (point) orig)
           (goto-char orig))
          ((and (eq (point) orig)
                (progn (forward-char 1)
                       (skip-chars-forward "^\"'[({" (line-end-position))
                       (member (char-after) (list ?\( ?\" ?\' ?\[ ?\{)))
                (setq erg (point)))))
    (unless erg
      (goto-char orig))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py--thing-at-point (form &optional mark-decorators)
  "Returns buffer-substring of string-argument FORM as cons.

Text properties are stripped.
If PY-MARK-DECORATORS, `def'- and `class'-forms include decorators
If BOL is t, from beginning-of-line"
  (interactive)
  (let* ((begform (intern-soft (concat "py-backward-" form)))
         (endform (intern-soft (concat "py-forward-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and mark-decorators
         (and (setq erg (py-backward-decorator))
              (setq beg erg)))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
        (buffer-substring-no-properties beg end)
      nil)))

(defun py--thing-at-point-bol (form &optional mark-decorators)
  (let* ((begform (intern-soft (concat "py-backward-" form "-bol")))
         (endform (intern-soft (concat "py-forward-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when mark-decorators
      (save-excursion
        (when (setq erg (py-backward-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (cons beg end)))
(defun py--mark-base (form &optional mark-decorators)
  "Returns boundaries of FORM, a cons.

If PY-MARK-DECORATORS, `def'- and `class'-forms include decorators
If BOL is t, mark from beginning-of-line"
  (let* ((begform (intern-soft (concat "py-backward-" form)))
         (endform (intern-soft (concat "py-forward-" form)))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (and mark-decorators
         (and (setq erg (py-backward-decorator))
              (setq beg erg)))
    (push-mark)
    (setq end (funcall endform))
    (unless end (when (< beg (point))
                  (setq end (point))))
    (if (and beg end (<= beg orig) (<= orig end))
        (progn
	  (cons beg end)
	  (exchange-point-and-mark))
      nil)))

(defun py--mark-base-bol (form &optional mark-decorators)
  (let* ((begform (intern-soft (concat "py-backward-" form "-bol")))
         (endform (intern-soft (concat "py-forward-" form "-bol")))
         (begcheckform (intern-soft (concat "py--beginning-of-" form "-bol-p")))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when mark-decorators
      (save-excursion
        (when (setq erg (py-backward-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (cons beg end)))

(defun py-mark-base (form &optional mark-decorators)
  "Calls py--mark-base, returns bounds of form, a cons. "
  (let* ((bounds (py--mark-base form mark-decorators))
         (beg (car bounds)))
    (push-mark beg t t)
    bounds))

(defun py-backward-same-level-intern (indent)
  (while (and
          (py-backward-statement)
          (< indent (current-indentation) ))))

(defun py-backward-same-level ()
  "Go form backward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up."
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (cond ((nth 8 pps) (goto-char (nth 8 pps)))
                    ((nth 1 pps) (goto-char (nth 1 pps)))
                    (t (if (eq (current-column) (current-indentation))
                           (py-backward-same-level-intern (current-indentation))
                         (back-to-indentation)
                         (py-backward-same-level))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py-forward-same-level ()
  "Go form forward keeping indent level if possible.

If inside a delimited form --string or list-- go to its beginning.
If not at beginning of a statement or block, go to its beginning.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point.
If no further element at same level, go one level up."
  (interactive)
  (let (erg)
    (unless (py-beginning-of-statement-p)
      (py-backward-statement))
    (setq erg (py-down (current-indentation)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun py--end-of-buffer-p ()
  "Returns position, if cursor is at the end of buffer, nil otherwise. "
  (when (eobp)(point)))

(defun py-sectionize-region (&optional beg end)
  "Markup code in region as section.

Use current region unless optional args BEG END are delivered."
  (interactive "*")
  (let ((beg (or beg (region-beginning)))
        (end (or (and end (copy-marker end)) (copy-marker (region-end)))))
    (save-excursion
      (goto-char beg)
      (unless (py-empty-line-p) (split-line))
      (beginning-of-line)
      (insert py-section-start)
      (goto-char end)
      (unless (py-empty-line-p) (newline 1))
      (insert py-section-end))))

(defun py-execute-section-prepare (&optional shell)
  "Execute section at point. "
  (save-excursion
    (let ((start (when (or (py--beginning-of-section-p)
                           (py-backward-section))
                   (forward-line 1)
                   (beginning-of-line)
                   (point))))
      (if (and start (py-forward-section))
          (progn
            (beginning-of-line)
            (skip-chars-backward " \t\r\n\f")
            (if shell
                (funcall (car (read-from-string (concat "py-execute-region-" shell))) start (point))
              (py-execute-region start (point))))
        (error "Can't see `py-section-start' resp. `py-section-end'")))))

(defun py--narrow-prepare (name)
  "Used internally. "
  (save-excursion
    (let ((start (cond ((string= name "statement")
                        (if (py--beginning-of-statement-p)
                            (point)
                          (py-backward-statement-bol)))
                       ((funcall (car (read-from-string (concat "py--statement-opens-" name "-p")))))
                       (t (funcall (car (read-from-string (concat "py-backward-" name "-bol"))))))))
      (funcall (car (read-from-string (concat "py-forward-" name))))
      (narrow-to-region (point) start))))

(defun py--forms-report-result (erg &optional iact)
  (let ((res (ignore-errors (buffer-substring-no-properties (car-safe erg) (cdr-safe erg)))))
    (when (and res iact)
      (goto-char (car-safe erg))
      (set-mark (point))
      (goto-char (cdr-safe erg)))
    res))

(defun py-toggle-shell-fontification (msg)
  "Toggles value of ‘py-shell-fontify-p’. "
  (interactive "p")

  (if (setq py-shell-fontify-p (not py-shell-fontify-p))
      (progn
	(py-shell-font-lock-turn-on))
    (py-shell-font-lock-turn-off))
    (when msg (message "py-shell-fontify-p set to: %s" py-shell-fontify-p)))

(defun py-toggle-execute-use-temp-file ()
  (interactive)
  (setq py--execute-use-temp-file-p (not py--execute-use-temp-file-p)))

(defun py--close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (car (py--go-to-keyword regexp))))
    (message "%s" cui)
    (py--end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (py-empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py--backward-regexp (regexp &optional indent condition orig regexpvalue)
  "Search backward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (unless (py-beginning-of-statement-p) (skip-chars-backward " \t\r\n\f")
	    (py-backward-comment (point)))
    (let* (pps
	   (regexpvalue (or regexpvalue (symbol-value regexp)))
	   (indent (or indent (current-indentation)))
	   (condition (or condition '<))
	   (orig (or orig (point))))
      (if (eq (current-indentation) (current-column))
	  (while (and
		  (not (bobp))
		  ;; # class kugel(object) -> a[1:2]:
		  ;; class kugel(object):
		  ;; (re-search-backward regexpvalue nil 'move 1)
		  (re-search-backward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1)
		  ;; re-search-backward not greedy
		  (not (and (looking-back "async *" (line-beginning-position))
			    (goto-char (match-beginning 0))))
		  (or (and
                       (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
                       (goto-char pps))
		      ;; needed by py-backward-clause
                      (and indent
		      	   (funcall condition indent (current-indentation))))))
	(back-to-indentation)
	(and
         (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
         (goto-char pps))
	(unless (and (< (point) orig) (looking-at regexpvalue))
	  (py--backward-regexp regexp (current-indentation) condition orig)))
      (unless (or (eq (point) orig)(bobp)) (back-to-indentation))
      (and (looking-at regexpvalue) (not (nth 8 (parse-partial-sexp (point-min) (point))))(point)))))

(defun py--forward-regexp (regexp)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let (erg)
      (while (and
              (setq erg (re-search-forward regexp nil 'move 1))
              (nth 8 (parse-partial-sexp (point-min) (point)))))
      (unless
	  (nth 8 (parse-partial-sexp (point-min) (point)))
        erg))))

(defun py--backward-regexp-fast (regexp)
  "Search backward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let (last)
      (while (and
              (re-search-backward regexp nil 'move 1)
              (setq last (match-beginning 0))
              (nth 8 (parse-partial-sexp (point-min) (point)))))
      (unless (nth 8 (parse-partial-sexp (point-min) (point)))
        last))))

(defun py--forward-regexp-keep-indent (regexp &optional indent)
  "Search forward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (save-match-data
    (let ((indent (or indent (current-indentation)))
          (regexp (if (stringp regexp)
                      regexp
                    (symbol-value regexp)))
	  (orig (point))
          last done)
      (forward-line 1)
      (beginning-of-line)
      (while (and
	      (not done)
              (re-search-forward regexp nil 'move 1)
              (or (nth 8 (parse-partial-sexp (point-min) (point)))
                  (or (< indent (current-indentation))(setq done t))
		  (setq last (line-end-position)))))
      (unless
          (nth 8 (parse-partial-sexp (point-min) (point)))
	(if last (goto-char last)
	  (back-to-indentation))
        (and (< orig (point)) (point))))))

(defun py-down-base (regexp &optional indent bol)
  (let ((indent (or indent (current-indentation))))
    (and (py--forward-regexp-keep-indent regexp indent)
	 (progn
           (if bol
               (beginning-of-line)
             (back-to-indentation))
           (point)))))

(defun py-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like ‘'py-clause-re’"
  (unless (py-beginning-of-statement-p) (py-backward-statement))
  (unless (looking-at (symbol-value regexp))
        (py--go-to-keyword regexp (or indent (current-indentation)) '<))
  ;; now from beginning-of-block go one indent level upwards
  (py--go-to-keyword regexp (- (or indent (current-indentation)) py-indent-offset) '<))

(defun py-indent-and-forward (&optional indent)
  "Indent current line according to mode, move one line forward.

If optional INDENT is given, use it"
  (interactive "*")
  (beginning-of-line)
  (when (member (char-after) (list 32 9 10 12 13)) (delete-region (point) (progn (skip-chars-forward " \t\r\n\f")(point))))
  (indent-to (or indent (py-compute-indentation)))
  (if (eobp)
      (newline-and-indent)
    (forward-line 1))
  (back-to-indentation))

(defun py--indent-line-by-line (beg end)
  "Indent every line until end to max reasonable extend.

Starts from second line of region specified
BEG END deliver the boundaries of region to work within"
  (goto-char beg)
  (py-indent-and-forward)
  ;; (forward-line 1)
  (while (< (line-end-position) end)
    (if (py-empty-line-p)
	(forward-line 1)
      (py-indent-and-forward)))
  (unless (py-empty-line-p) (py-indent-and-forward)))

(defun py-indent-region (&optional beg end no-check)
  "Reindent a region delimited by BEG END.

In case first line accepts an indent, keep the remaining
lines relative.
Otherwise lines in region get outmost indent,
same with optional argument

In order to shift a chunk of code, where the first line is okay, start with second line.

Optional BEG: used by tests
Optional END: used by tests
Optional NO-CHECK: used by tests
"
  (interactive "*")
  (or no-check (use-region-p) (error "Don't see an active region"))
  (let ((end (copy-marker (or end (region-end)))))
    (goto-char (or beg (region-beginning)))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t\r\n\f")
    (py--indent-line-by-line beg end)))

(provide 'python-components-intern)
 ;;;  python-components-intern.el ends here
