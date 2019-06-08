;; python-components-intern.el --- Part of python-components-mode -*- lexical-binding: t; -*-

;; Helper functions

;; Copyright (C) 2015-2016 Andreas Röhler

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

;;  Keymap

;; (defun py--indent-prepare (inter-re)
;;   (progn (back-to-indentation)
;;       (or (py--beginning-of-statement-p)
;;           (ar-backward-statement))
;;       (cond ((eq 0 (current-indentation))
;;              (current-indentation))
;;             ((looking-at (symbol-value inter-re))
;;              (current-indentation))
;;             (t
;;              (if (<= py-indent-offset (current-indentation))
;;                  (- (current-indentation) (if ar-smart-indentation (ar-guess-indent-offset) py-indent-offset))
;;                py-indent-offset)))))

(defun py-separator-char ()
  "Return the file-path separator char from current machine.

When `py-separator-char' is customized, its taken.
Returns char found. "
  (let ((erg (cond ((characterp py-separator-char)
                    (char-to-string py-separator-char))
                   ;; epd hack
                   ((and
                     (string-match "[Ii][Pp]ython" py-shell-name)
                     (string-match "epd\\|EPD" py-shell-name))
                    (replace-regexp-in-string "\n" ""
                                              (shell-command-to-string (concat py-shell-name " -c \"import os; print(os.sep)\"")))))))
    (if (and erg (string-match "^$" erg))
        (setq erg (substring erg (string-match "^$" erg)))
      (setq erg (replace-regexp-in-string "\n" "" (shell-command-to-string (concat py-shell-name " -W ignore" " -c \"import os; print(os.sep)\"")))))
    erg))

(defun pps-emacs-version ()
  "Include the appropriate `parse-partial-sexp' "
  (if (featurep 'xemacs)
      '(parse-partial-sexp (point-min) (point))
    '(parse-partial-sexp (point-min) (point))))

(defun py--forward-string-maybe (&optional start)
  "Go to the end of string.

Expects START position of string
Return position of moved, nil otherwise."
  (let ((orig (point)))
    (when start (goto-char start)
	  (forward-sexp)
	  (and (< orig (point)) (point)))))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (and (nth 4 pps) (nth 8 pps))))
    erg))
;;
(defun py-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (when (or (looking-at "\"")(looking-at "[ \t]*#[ \t]*"))
        (point))))

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py--buffer-filename-remote-maybe (&optional file-name)
  (let ((file-name (or file-name
                       (and
                        (ignore-errors (file-readable-p (buffer-file-name)))
                        (buffer-file-name)))))
    (if (and (featurep 'tramp) (tramp-tramp-file-p file-name))
        (tramp-file-name-localname
         (tramp-dissect-file-name file-name))
      file-name)))

(defun py-forward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (eobp)
    (goto-char (point-max))))

(defun py-backward-buffer ()
  "A complementary form used by auto-generated commands.

Returns position reached if successful"
  (interactive)
  (unless (bobp)
    (goto-char (point-min))))

;; (defun py--execute-prepare (form &optional shell dedicated switch beg end file fast proc wholebuf split buffer)
;;   "Used by python-components-extended-executes ."
;;   (save-excursion
;;     (let* ((form (prin1-to-string form))
;;            (origline (py-count-lines))
;;            (beg (unless file
;;                   (prog1
;;                       (or beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

;;                           (funcall (intern-soft (concat "py-backward-" form)))
;;                           (push-mark)))))
;;            (end (unless file
;;                   (or end (save-excursion (funcall (intern-soft (concat "py-forward-" form)))))))
;;            filename)
;;       ;; (setq py-buffer-name nil)
;;       (if file
;;           (progn
;;             (setq filename (expand-file-name form))
;;             (if (file-readable-p filename)
;;                 (py--execute-file-base nil filename nil nil origline)
;;               (message "%s not readable. %s" file "Do you have write permissions?")))
;;         (py--execute-base beg end shell filename proc file wholebuf fast dedicated split switch buffer)))))

(defmacro py--execute-prepare (form &optional shell dedicated switch beg end file fast proc wholebuf split buffer)
  "Used by python-components-extended-executes ."
  (save-excursion
    `(let* ((form ,(prin1-to-string form))
           (origline (py-count-lines))
           (beg (unless ,file
                  (prog1
                      (or ,beg (funcall (intern-soft (concat "py--beginning-of-" form "-p")))

                          (funcall (intern-soft (concat "py-backward-" form)))
                          (push-mark)))))
           (end (unless ,file
                  (or ,end (save-excursion (funcall (intern-soft (concat "py-forward-" form)))))))
           filename)
      ;; (setq py-buffer-name nil)
      (if ,file
          (progn
            (setq filename (expand-file-name form))
            (if (file-readable-p filename)
                (py--execute-file-base nil filename nil nil origline)
              (message "%s not readable. %s" ,file "Do you have write permissions?")))
        (py--execute-base beg end ,shell filename ,proc ,file ,wholebuf ,fast ,dedicated ,split ,switch ,buffer)))))

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
    (when (called-interactively-p 'any) (message "%s" erg))
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
    ;; (when (called-interactively-p 'any) (message "%S" erg))
    erg))

;;  Miscellany.
(defun py--shell-simple-send (proc strg)
  (let* ((strg (substring-no-properties strg))
         (nln (string-match "\n$" strg)))
    ;; (or nln (setq strg (concat strg "\n")))
    ;; (comint-simple-send proc (substring-no-properties strg))
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

(defun py-toggle-local-default-use ()
  (interactive)
  "Toggle boolean value of `py-use-local-default'.

Returns `py-use-local-default'

See also `py-install-local-shells'
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (setq py-use-local-default (not py-use-local-default))
  (when (called-interactively-p 'any) (message "py-use-local-default set to %s" py-use-local-default))
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
(push (list
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
              (lambda ()
                (py-forward-block-or-clause))
              nil) hs-special-modes-alist)

;; ;

(defun py--input-filter (str)
  "`comint-input-filter' function for Python.

Don't save anything for STR matching `py-input-filter-re' "
  (not (string-match py-input-filter-re str)))

(make-obsolete 'jpython-mode 'jython-mode nil)

(push (purecopy "*Python*")  same-window-buffer-names)
(push (purecopy "*IPython*")  same-window-buffer-names)

(push (cons (purecopy "\\.py\\'")  'python-mode)  auto-mode-alist)

;; Python Macro File

(unless (member '(".pym'" . python-mode) auto-mode-alist)
  (push (cons (purecopy "\\.pym\\'")  'python-mode)  auto-mode-alist))

(unless (member '(".pyc'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pyc\\'")  'python-mode)  auto-mode-alist))

;; Pyrex Source
(unless (member '(".pyx'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pyx\\'")  'python-mode) auto-mode-alist))

;; Python Optimized Code
(unless (member '(".pyo'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pyo\\'")  'python-mode) auto-mode-alist))

;; Pyrex Definition File
(unless (member '(".pxd'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pxd\\'")  'python-mode) auto-mode-alist))

;; Python Repository
(unless (member '(".pyr'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pyr\\'")  'python-mode)  auto-mode-alist))

;; Python Stub file
;; https://www.python.org/dev/peps/pep-0484/#stub-files
(unless (member '(".pyi'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pyi\\'")  'python-mode)  auto-mode-alist))

;; Python Path Configuration
(unless (member '(".pth'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.pth\\'")  'python-mode)  auto-mode-alist))

;; Python Wheels
(unless (member '(".whl'" . python-mode)  auto-mode-alist)
  (push (cons (purecopy "\\.whl\\'")  'python-mode)  auto-mode-alist))

(unless (member '("!#[          ]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist)
  (push '("!#[ \\t]*/.*[jp]ython[0-9.]*" . python-mode) magic-mode-alist))

;;  lp:1355458, what about using `magic-mode-alist'?

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
                            (while (and (py-beginning-of-comment) (setq last (point))(prog1 (forward-line -1)(end-of-line))))
                            last))))
        (and (py-forward-comment))
        (py--uncomment-intern beg (point))))))

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
    (when (interactive-p) (kill-new erg)
	  (message "%s" erg))
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

(defun py-compute-list-indent--according-to-circumstance (pps line)
  (goto-char (nth 1 pps))
  (if (looking-at "[({][ \t]*$")
      (+ (current-indentation) py-indent-offset)
    ;; (py-compute--dict-indent pps)
    (cond ((and line (eq 0 (current-column)))
           (1+ py-indent-offset))
          (t (py-compute-indentation--according-to-list-style)))))

(defun py-compute-indentation-in-list (pps line closing orig)
  (if closing
      (py-compute-indentation-closing-list pps)
    (cond ((and (not line)(looking-back py-assignment-re (line-beginning-position)))
           (py--fetch-indent-statement-above orig))
          ;; (py-compute-indentation--according-to-list-style pps iact orig origline line nesting repeat indent-offset liep)
          (t (when (looking-back "[ \t]*\\(\\s(\\)" (line-beginning-position))
               (goto-char (match-beginning 1))
               (setq pps (parse-partial-sexp (point-min) (point))))
             (py-compute-list-indent--according-to-circumstance pps line)))))

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
			     (back-to-indentation)
			     (skip-chars-backward " \t\r\n\f")
			     (back-to-indentation)
			     (current-indentation)))
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
			 (py-compute-indentation-in-list pps line closing orig))
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
			     (- (current-indentation) (or indent-offset py-indent-offset))
			   (py-backward-block-or-clause)
			   (current-indentation)))
			;; ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
			;; (when (py--line-backward-maybe) (setq line t))
			;; (car (py--clause-lookup-keyword py-elif-re -1 nil origline)))
			((and (looking-at py-clause-re) (not line)
                              (eq liep (line-end-position)))
			 (cond ((looking-at py-outdent-re)
				;; (and (py--backward-regexp 'py-block-or-clause-re) (current-indentation)))
			       	(and (py--go-to-keyword 'py-block-or-clause-re) (current-indentation)))
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

(defun py-beginning-of-statement-p ()
  (interactive)
  (save-restriction
    (eq (point)
    (save-excursion
      (py-forward-statement)
      (py-backward-statement)))))

(defun py--fetch-indent-statement-above (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (back-to-indentation)
    (if (or (looking-at comment-start)(py-beginning-of-statement-p))
        (current-indentation)
      (py-backward-statement)
      (current-indentation))))

(defun py--fetch-indent-line-above (&optional orig)
  "Report the preceding indent. "
  (save-excursion
    (when orig (goto-char orig))
    (forward-line -1)
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (and (numberp arg) (not (eq 1 arg)) (setq py-continuation-offset arg))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" py-continuation-offset))
  py-continuation-offset)

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py--beginning-of-statement-p)
                   (py-backward-statement))
               (current-indentation))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (nth 1 (parse-partial-sexp (or start (point-min)) (point))))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (parse-partial-sexp ppstart (point)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" end))
    end))

(defun py--in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (ignore-errors (looking-at (concat "[ \t]*" comment-start)))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (parse-partial-sexp (point-min) (point)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
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

(defun py--down-according-to-indent (regexp secondvalue &optional indent enforce-regexp)
  "Return position if moved, nil otherwise.

Optional ENFORCE-REGEXP: search for regexp only."
  (unless (eobp)
    (let* ((orig (point))
	   (indent (or indent 0))
	   done
	   (regexpvalue (symbol-value regexp))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`py-try-re py-finally-re)
			     (`py-if-re py-else-re)))))
      (while
	  (and
	   (not done)
	   (progn (end-of-line)
		  (cond (enforce-regexp
			 ;; using regexpvalue might stop behind global settings, missing the end of form
			 (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
			(t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_]+") nil 'move 1))))
	   (or (progn (back-to-indentation) (py--forward-string-maybe (nth 8 (parse-partial-sexp orig (point)))))
	       (and secondvalue (looking-at secondvalue))
	       (and lastvalue (looking-at lastvalue))
	       (and (looking-at regexpvalue) (setq done t))
	       (setq done t))))
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

Returns the indentation of FORM-start "
  (unless (eobp)
    (let ((orig (or orig (point))))
      (unless (eobp)
	(unless (py-beginning-of-statement-p)
	  (py-beginning-of-statement))
	(let* (;; when at block-start, be specific
	       (regexp (py--refine-regexp-maybe regexp))
               (regexpvalue (symbol-value regexp))
               ;; (regexp (or regexp (symbol-value 'py-extended-block-or-clause-re)))
	       (repeat (or repeat 0))
	       (indent (if
			   (looking-at regexpvalue)
			   (abs
			    (- (current-indentation) py-indent-offset))
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
		  (py--down-according-to-indent regexp nil 0))
		 ;; look upward
		 (t (py--go-to-keyword regexp))))
	       (secondvalue (ignore-errors (nth 2 res))))
	       ;; (py-for-block-p (looking-at py-for-re))
	  (setq indent (or (and res (car-safe res)) indent))
	  (cond
	   (res (setq erg
		      (and
		       (py--down-according-to-indent regexp secondvalue (current-indentation))
		       ;; (py--forward-regexp-keep-indent "^[ \t]*[[:alnum:]_]" (current-indentation))
		       (py--down-end-form))))
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
          (sit-for py-current-defun-delay))
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

(defun py-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position. "
  (interactive)
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
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

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

(defun py--delay-process-dependent (process)
  "Call a `py-ipython-send-delay' or `py-python-send-delay' according to process"
  (if (string-match "ipython" (prin1-to-string process))
      (sit-for py-ipython-send-delay t)
    (sit-for py-python-send-delay t)))

(defun py-send-string (strg &optional process)
  "Evaluate STRG in Python PROCESS."
  (interactive "sPython command: ")
  (let* ((buffer (if process (process-buffer process) (py-shell)))
         (proc (or process (get-buffer-process buffer))))
    (py-fast-send-string strg proc buffer)))

;; (defmacro py--cleanup-shell (orig)
;;   (declare (debug t))
;;   (goto-char (point-max))
;;   `(unless (eq (point) ,orig)
;;      (delete-region ,orig (point))))

(defvar py-known-first-prompts-re (concat py-shell-input-prompt-1-regexp"\\|"py-shell-input-prompt-2-regexp"\\|"py-ipython-input-prompt-re"\\|"py-ipython-output-prompt-re)
  "Match all known Python shell prompts of first level.")

(defun py--remove-extra-prompt ()
  (let ((inhibit-field-text-motion t))
    (and (looking-back (concat "^\\(" py-known-first-prompts-re "\\)\\(" py-known-first-prompts-re "\\)\\(.*\\)") (line-beginning-position)) (delete-region (match-beginning 2) (match-end 2)))))

(defun py--send-string-no-output (strg &optional process orig)
  "Send STRING to PROCESS and inhibit output display.
When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let*       ((inhibit-read-only t)
	       output
	       (orig (or orig (ignore-errors (or comint-last-input-end (and comint-last-prompt (cdr comint-last-prompt)) (point)))))
	       (process (or process (get-buffer-process (py-shell)))))
    (switch-to-buffer (process-buffer process))
    (py-send-string strg process)
    (sit-for 0.1 t)
    (py--remove-extra-prompt)))

(defmacro py--return-and-cleanup-maybe (end orig)
  `(unless (eq ,end ,orig)
     (prog1 (buffer-substring-no-properties ,orig ,end)
       (delete-region ,orig ,end))))

(defun py--send-string-return-output (strg &optional process)
  "Send STRING to PROCESS and return output.

When MSG is non-nil messages the first line of STRING.  Return
the output."
  (let ((process (or process (get-buffer-process (py-shell))))
	(inhibit-read-only t)
        erg)
    (with-current-buffer (process-buffer process)
      (let ((orig (ignore-errors (or comint-last-input-end (and comint-last-prompt (cdr comint-last-prompt)) (point))))
            (end (point)))
        (py-send-string strg process)
        ;; (accept-process-output process)
        (setq end (ignore-errors (and comint-last-prompt (1- (car comint-last-prompt)))))
        (and end orig (setq erg (py--return-and-cleanup-maybe end orig)))
        (if (and erg (stringp erg) (not (or (string= "" erg) (string= "''" erg))))
            (setq erg
                  (replace-regexp-in-string
                   (format "[ \n]*%s[ \n]*" py-fast-filter-re)
                   "" erg))
          (setq erg nil))
        ;; don't insert empty completion string
        ;; (when end
        ;; (when delete (delete-region orig end)))
	))
    erg))

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

(defun py--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon.

Returns `t' if point was moved"
  (prog1
      (< 0 (abs (skip-chars-backward "^;" (or limit (line-beginning-position)))))
    (skip-chars-forward " \t" (line-end-position))))

(defun py-forward-comment ()
  "Go to the end of comment at point."
  (let ((orig (point))
        last)
    (while (and (not (eobp)) (nth 4 (parse-partial-sexp (line-beginning-position) (point))) (setq last (line-end-position)))
      (forward-line 1)
      (end-of-line))
    (when
        (< orig last)
      (goto-char last)(point))))

(defun py--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list  (string-to-char comment-start) 10))(forward-line 1)(back-to-indentation))))

(defun py--skip-to-comment-or-semicolon (done)
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

(defun py--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form.

Return position."
  (if (eobp)
      (point)
    (forward-line 1)
    (beginning-of-line)
    (point)))

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
        (cons beg end)
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
      (unless (empty-line-p) (split-line))
      (beginning-of-line)
      (insert py-section-start)
      (goto-char end)
      (unless (empty-line-p) (newline))
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

(defun py-rotate-shell-fontify-style (msg)
  "Rotates between possible values 'all, 'input and nil. "
  (interactive "p")
  (cond ((eq py-shell-fontify-style 'all)
         (setq py-shell-fontify-style nil))
        ((eq py-shell-fontify-style 'input)
         (setq py-shell-fontify-style 'all))
        (t (setq py-shell-fontify-style 'input)))
  (py--shell-setup-fontification py-shell-fontify-style)
  (when msg (message "py-shell-fontify-style set to: %s" py-shell-fontify-style)))

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
        (unless (empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py-escaped-p (&optional pos)
  "Return t if char at POS is preceded by an odd number of backslashes. "
  (save-excursion
    (when pos (goto-char pos))
    (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

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
	(unless (and (< (point) orig) (looking-at regexpvalue))
	  (py--backward-regexp regexp (current-indentation) condition orig)))
      (unless (eq (point) orig) (back-to-indentation))
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

(provide 'python-components-intern)
 ;;;  python-components-intern.el ends here
