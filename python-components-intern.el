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
                                                (read-from-string (concat "(lambda (&optional dedicated args) \"Start a `" erg "' interpreter.
Optional DEDICATED: with \\\\[universal-argument] start in a new
dedicated shell.
Optional ARGS overriding `py-" ele "-command-args'.

Calls ‘py-shell’
\"
  (interactive \"p\") (py-shell dedicated args nil \""ele"\"))")))))))
  (when (functionp (car (read-from-string (car-safe py-known-shells))))
    (when py-verbose-p (message "py-load-named-shells: %s" "installed named-shells"))))

;; (py-load-named-shells)

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension ‘.py’ import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base file-name (get-buffer-process (get-buffer (py-shell)))))

;;  Hooks
;;  arrange to kill temp files when Emacs exists

(when py--warn-tmp-files-left-p
  (add-hook 'python-mode-hook (quote py--warn-tmp-files-left)))

(defun py-guess-pdb-path ()
  "If py-pdb-path is not set, find location of pdb.py. "
  (interactive)
  (let ((ele (split-string (shell-command-to-string "whereis python")))
        erg)
    (while (or (not erg)(string= "" erg))
      (when (and (string-match "^/" (car ele)) (not (string-match "/man" (car ele))))
        (setq erg (shell-command-to-string (concat "find " (car ele) " -type f -name \"pdb.py\""))))
      (setq ele (cdr ele)))
    (if erg
        (message "%s" erg)
      (message "%s" "pdb.py not found, please customize ‘py-pdb-path’"))
    erg))

(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  (quote py-mouseto-exception))
  (define-key py-mode-output-map "\C-c\C-c" (quote py-goto-exception))
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command)))

(defun py-toggle-comment-auto-fill (&optional arg)
  "Toggles comment-auto-fill mode"
  (interactive "P")
  (if (or (and arg (< 0 (prefix-numeric-value arg)))
          (and (boundp (quote py-comment-auto-fill-p))(not py-comment-auto-fill-p)))
      (progn
        (set (make-local-variable (quote py-comment-auto-fill-p)) t)
        (setq fill-column py-comment-fill-column)
        (auto-fill-mode 1))
    (set (make-local-variable (quote py-comment-auto-fill-p)) nil)
    (auto-fill-mode -1)))

(defun py-comment-auto-fill-on ()
  (interactive)
  (py-toggle-comment-auto-fill 1))

(defun py-comment-auto-fill-off ()
  (interactive)
  (py-toggle-comment-auto-fill -1))

(defun py--set-auto-fill-values ()
  "Internal use by ‘py--run-auto-fill-timer’"
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (setq fill-column py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (setq fill-column py-docstring-fill-column))
          (t (setq fill-column py-fill-column-orig)))))

(defun py--run-auto-fill-timer ()
  "Set fill-column to values according to environment.

‘py-docstring-fill-column’ resp. to ‘py-comment-fill-column’."
  (when py-auto-fill-mode
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             (quote py--set-auto-fill-values))))))

;;  unconditional Hooks
;;  (orgstruct-mode 1)
(declare-function py-complete "pycomplete" ())
(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; do not try completion if buffer was not modified
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

;;  End-of- p

;;  Opens
(defun py--statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (let* ((regexp (or regexp py-block-or-clause-re))
         (erg (py--statement-opens-base regexp)))
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
    erg))

(defun py--statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-clause-re))

(defun py--statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (py--statement-opens-base py-block-or-clause-re))

(defun py--statement-opens-class-p ()
  "If the statement opens a functions or class.

Return ‘t’, nil otherwise. "
  (py--statement-opens-base py-class-re))

(defun py--statement-opens-def-p ()
  "If the statement opens a functions or class.
Return ‘t’, nil otherwise. "
  (py--statement-opens-base py-def-re))

(defun py--statement-opens-def-or-class-p ()
  "If the statement opens a functions or class definition.
Return ‘t’, nil otherwise. "
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

(defun py--end-of-paragraph (regexp)
  (let* ((regexp (if (symbolp regexp) (symbol-value regexp)
                   regexp)))
    (while (and (not (eobp)) (re-search-forward regexp nil 'move 1) (nth 8 (parse-partial-sexp (point-min) (point)))))))

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
      erg)))

(defun py-current-defun ()
  "Go to the outermost method or class definition in current scope.

Python value for ‘add-log-current-defun-function’.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables ‘py-current-defun-show’ and ‘py-current-defun-delay’."
  (interactive)
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
        erg))))

(defun py--join-words-wrapping (words separator prefix line-length)
  (let ((lines ())
        (current-line prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

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
‘https://gitlab.com/python-mode-devs/python-mode/-/issues’
is preferable for that. ")

;;  Utilities

(defun py-install-local-shells (&optional local)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command ‘find’ searches beneath current directory.
Eval resulting buffer to install it, see customizable ‘py-extensions’. "
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

(defun py--report-end-marker (process)
  ;; (message "py--report-end-marker in %s" (current-buffer))
  (if (derived-mode-p 'comint-mode)
      (if (bound-and-true-p comint-last-prompt)
          (car-safe comint-last-prompt)
        (dotimes (_ 3) (when (not (bound-and-true-p comint-last-prompt))(sit-for 1 t)))
        (and (bound-and-true-p comint-last-prompt)
             (car-safe comint-last-prompt)))
    (if (markerp (process-mark process))
        (process-mark process)
      (progn
        (dotimes (_ 3) (when (not (markerp (process-mark process)))(sit-for 1 t)))
        (process-mark process)))))

(defun py-which-def-or-class (&optional orig)
  "Returns concatenated ‘def’ and ‘class’ names.

In hierarchical order, if cursor is inside.

Returns \"???\" otherwise
Used by variable ‘which-func-functions’ "
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
    erg))

(defun py--fetch-first-python-buffer ()
  "Returns first (I)Python-buffer found in ‘buffer-list’"
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

;; (defmacro py-kill-buffer-unconditional (buffer)
;;   "Kill buffer unconditional, kill buffer-process if existing. "
;;   `(let ((proc (get-buffer-process ,buffer))
;;          kill-buffer-query-functions)
;;      (ignore-errors
;;        (and proc (kill-process proc))
;;        (set-buffer ,buffer)
;;        (set-buffer-modified-p 'nil)
;;        (kill-buffer (current-buffer)))))

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
    erg))

(defun py--down-intern ()
  "Go down from current indentation."
  (let ((cui (current-indentation)))
    (while (and (py-down-statement)
                (or (eq (current-indentation) cui)
                    (save-excursion (backward-char) (nth 8 (parse-partial-sexp (point-min) (point)))))))))

(defun py-down ()
  "Move forward down one level of syntactic indentation.

If at start of a list, move down one level of nesting.
From the beginning of a string or comment, jump inside.
Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        (pps (parse-partial-sexp (point-min) (point))))
    (cond ((eq (syntax-class (syntax-after (point))) 4)
           (forward-char))
          ((nth 4 pps)
           (py-forward-comment))
          ((nth 3 pps)
           (goto-char (nth 8 pps))
           (forward-sexp))
          ((nth 1 pps)
           (goto-char (nth 1 pps))
           (forward-sexp))
          ((eq (car (syntax-after orig)) 15)
           (forward-char 1))
          ((not (py--beginning-of-statement-p))
           (py-backward-statement)
           (cond ((py--beginning-of-class-p)
                  (py--down-intern))
                 ((py--beginning-of-def-p)
                  (py--down-intern))
                 ((py--beginning-of-block-p)
                  (py--down-intern))
                 ((py--beginning-of-clause-p)
                  (py--down-intern))))
          ((py--beginning-of-class-p)
           (py--down-intern))
          ((py--beginning-of-def-p)
           (py--down-intern))
          ((py--beginning-of-block-p)
           (py--down-intern))
          ((py--beginning-of-clause-p)
           (py--down-intern))
          (t
           (py-down-statement)))
    (if (< orig (point))
        (point)
      (goto-char orig)
      (end-of-line)
      (skip-chars-forward " \t\r\n\f")
      (if (or (and comment-start (looking-at comment-start)) (and comment-start-skip (looking-at comment-start-skip)))
          (progn
            (goto-char (match-end 0))
            (py-forward-comment)
            (skip-chars-forward " \t\r\n\f"))
        (py-down)))))

(defun py--thing-at-point (form &optional mark-decorators)
  "Returns buffer-substring of string-argument FORM as cons.

Text properties are stripped.
If PY-MARK-DECORATORS, ‘def’- and ‘class’-forms include decorators
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

If PY-MARK-DECORATORS, ‘def’- and ‘class’-forms include decorators
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
    (if (functionp begcheckform)
        (or (setq beg (funcall begcheckform))
            (if (functionp begform)
                (setq beg (funcall begform))
              (error (concat "py--mark-base-bol: " begform " do not exist!" ))))
      (error (concat "py--mark-base-bol: " begcheckform " do not exist!" )))
    (when mark-decorators
      (save-excursion
        (when (setq erg (py-backward-decorator))
          (setq beg erg))))
    (if (functionp endform)
        (setq end (funcall endform))
      (error (concat "py--mark-base-bol: " endform " do not exist!" )))
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
If at beginning of a statement or block,
go to previous beginning of at point.
If no further element at same level, go one level up."
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (erg (cond ((nth 8 pps) (goto-char (nth 8 pps)))
                    ((nth 1 pps) (goto-char (nth 1 pps)))
                    (t (if (eq (current-column) (current-indentation))
                           (py-backward-same-level-intern (current-indentation))
                         (back-to-indentation)
                         (py-backward-same-level))))))
    erg))

;; (defun py-forward-same-level ()
;;   "Go form forward keeping indent level if possible.

;; If inside a delimited form --string or list-- go to its beginning.
;; If not at beginning of a statement or block, go to its beginning.
;; If at beginning of a statement or block, go to previous beginning.
;; If no further element at same level, go one level up."
;;   (interactive)
;;   (let (erg)
;;     (unless (py--beginning-of-statement-p)
;;       (py-backward-statement))
;;     (setq erg (py-down (current-indentation)))
;;     erg))

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
      ;; (beginning-of-line)
      (insert py-section-start)
      (goto-char end)
      (unless (py-empty-line-p) (newline 1))
      (indent-according-to-mode)
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
        (error "Can not see ‘py-section-start’ resp. ‘py-section-end’")))))

(defun py--narrow-prepare (name)
  "Used internally. "
  (save-excursion
    (let ((start (cond ((string= name "statement")
                        (if (py--beginning-of-statement-p)
                            (point)
                          (py-backward-statement-bol)))
                       ((funcall (car (read-from-string (concat "py--statement-opens-" name "-p")))))
                       (t (funcall (car (read-from-string (concat "py-backward-" name))))))))
      (funcall (car (read-from-string (concat "py-forward-" name))))
      ;; (sit-for 1)
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
  (let ((cui (and
              (or (and (looking-at (symbol-value regexp))
                       (not (nth 8 (parse-partial-sexp (point-min) (point)))))
                       (py--go-to-keyword regexp)
                       (current-indentation))
              (current-indentation))))
    (when py-verbose-p (message "%s" cui))
    (py--end-base regexp (point))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (py-empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)))

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

(defun py-indent-region (&optional beg end)
  "Reindent a region delimited by BEG END.

In case first line accepts an indent, keep the remaining
lines relative.
Otherwise lines in region get outmost indent,
same with optional argument

In order to shift a chunk of code, start with second line.

Optional BEG: used by tests
Optional END: used by tests
Optional NO-CHECK: used by tests
"
  (interactive "*")

  (let ((end
         ;; work around a bug in Emacs' ‘end-of-defun’, which fiddles
         ;; after ‘end-of-defun-function’ is called
         ;; See ‘py-ert-borks-all-lp-1294820-sIKMyz’ test
         (if (and (and end (save-excursion (goto-char end) (looking-at py-block-or-clause-re))))
             (copy-marker (- end 1))
           (copy-marker (or end (region-end) (line-end-position)))))
        (beg (or beg (region-beginning) (line-beginning-position))))
    (goto-char beg)
    (py--indent-line-by-line beg end)))

(defun py-find-imports ()
  "Find top-level imports.

Returns imports"
  (interactive)
  (let (imports erg)
    (save-excursion
      (if (eq major-mode 'comint-mode)
          (progn
            (re-search-backward comint-prompt-regexp nil t 1)
            (goto-char (match-end 0))
            (while (re-search-forward
                    "import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
              (setq imports
                    (concat
                     imports
                     (replace-regexp-in-string
                      "[\\]\r?\n?\s*" ""
                      (buffer-substring-no-properties (match-beginning 0) (point))) ";")))
            (when (ignore-errors (string-match ";" imports))
              (setq imports (split-string imports ";" t))
              (dolist (ele imports)
                (and (string-match "import" ele)
                     (if erg
                         (setq erg (concat erg ";" ele))
                       (setq erg ele)))
                (setq imports erg))))
        (goto-char (point-min))
        (while (re-search-forward
                "^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9.]+ +import .*" nil t)
          (unless (py--end-of-statement-p)
            (py-forward-statement))
          (setq imports
                (concat
                 imports
                 (replace-regexp-in-string
                  "[\\]\r*\n*\s*" ""
                  (buffer-substring-no-properties (match-beginning 0) (point))) ";")))))
    ;; (and imports
    ;; (setq imports (replace-regexp-in-string ";$" "" imports)))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" imports))
    imports))

(provide 'python-components-intern)
 ;;;  python-components-intern.el ends here
