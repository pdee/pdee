;;; python-mode-utils.el - generating parts of python-mode.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

;; Keywords: languages, processes, python, oop

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

;;; Some of this forms to generate parts of
;; python-mode.el are just drafts, others outdated.
;; Kept for inspiration.

;;; Code:

(defconst components-directory
  "~/arbeit/emacs-lisp/python-modes/components-python-mode")

(defconst py-shells
  (list
   'ipython
   'ipython2.7
   'ipython3
   'jython
   'python
   'python2
   'python3
   'pypy
   ))

(defvar ar-prefix "py-")

(defconst py-position-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "comment"
   "def"
   "def-or-class"
   "expression"
   "except-block"
   "if-block"
   "indent"
   "line"
   "minor-block"
   "partial-expression"
   "paragraph"
   "section"
   "statement"
   "top-level"
   "try-block"
   ))

;; like positions, but without comment, statement, expression, section,
;; toplevel, : avoid py--end-base
(defconst py-beg-end-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def-or-class"
   "def"
   "if-block"
   "elif-block"
   "else-block"
   "for-block"
   "except-block"
   "try-block"
   "minor-block"
   ))

(defconst py-block-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "minor-block"
   "try-block"
   ))

(defconst py-def-or-class-forms
  (list
   "class"
   "def-or-class"
   "def"
   ))

;; like positions, but without statement and expression, avoid py--end-base
(defconst py-end-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "comment"
   "def"
   "def-or-class"
   "line"
   "minor-block"
   "paragraph"
   "section"
   "statement"
   "top-level"
   ))

;; section has a different end as others
(defconst py-execute-forms
  (list
   "block"
   "block-or-clause"
   "buffer"
   "class"
   "clause"
   "def"
   "def-or-class"
   "expression"
   "indent"
   "line"
   "minor-block"
   "paragraph"
   "partial-expression"
   "region"
   "statement"
   "top-level"
   )
  "Internally used")

(defconst py-bol-forms
  (list
   "assignment"
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "indent"
   "minor-block"
   "statement"
   "try-block"
   ))

(defconst py-non-bol-forms
  (list
   "comment"
   "expression"
   "line"
   "paragraph"
   "partial-expression"
   "section"
   "top-level"
   ))

(defconst py-extra-execute-forms
  (list
   "try-block"
   "if-block"
   "for-block"
   ;; "with-block"
   ))

;; execute + comment
(defconst py-hide-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "comment"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "expression"
   "for-block"
   "if-block"
   "indent"
   "line"
   "minor-block"
   "paragraph"
   "partial-expression"
   "section"
   "statement"
   "top-level"
   ))

;; comment can't use re-forms
(defconst py-navigate-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "expression"
   "for-block"
   "if-block"
   "indent"
   "minor-block"
   "partial-expression"
   "section"
   "statement"
   "top-level"
   "try-block"
   ))

;; like navigate, but not top-level
(defconst py-beginning-of-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "expression"
   "for-block"
   "if-block"
   "indent"
   "minor-block"
   "partial-expression"
   "section"
   "statement"
   "try-block"
   ))

(defconst py-backward-def-or-class-forms
  (list
   "block"
   "class"
   "def"
   "def-or-class"
   ))

(defconst py-backward-minor-block-forms
  (list
   "block-or-clause"
   "clause"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "minor-block"
   "try-block"
   ))

;; comment, section not suitable here
(defconst py-navigate-test-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "expression"
   "for-block"
   "if-block"
   "minor-block"
   "partial-expression"
   "statement"
   "top-level"
   "try-block"
   ))

(defconst py-comment-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "indent"
   "minor-block"
   "section"
   "statement"
   "top-level"
   ))

;; top-level is special
(defconst py-down-forms
  (list
   "block"
   "class"
   "clause"
   "block-or-clause"
   "def"
   "def-or-class"
   "minor-block"
   "statement"
   ))

(defconst py-shift-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "comment"
   "def"
   "def-or-class"
   "indent"
   "minor-block"
   "paragraph"
   "region"
   "statement"
   "top-level"))

;; top-level, paragraph not part of ‘py-shift-bol-forms’
(defconst py-shift-bol-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "indent"
   "minor-block"
   "statement"
   "try-block"
   ))

(defconst py-toggle-form-vars
  (list
   "py-nil-docstring-style"
   "py-onetwo-docstring-style"
   "py-pep-257-docstring-style"
   "py-pep-257-nn-docstring-style"
   "py-symmetric-docstring-style"
   "py-django-docstring-style" ))

(defconst py-options
  (list ""
        "switch"
        "no-switch"
        "dedicated"
        ;; "dedicated-switch"
        ))

(defconst py-full-options
  (list ""
        "switch"
        "no-switch"
        "dedicated"
        "dedicated-switch"))

(defvar py-commands
  (list
   "py-python-command"
   "py-ipython-command"
   "py-python3-command"
   "py-python2-command"
   "py-jython-command")
  "Python-mode will generate commands opening shells mentioned
  here. Edit this list \w resp. to your machine.")

(defconst py-core-command-name
  '("statement"
    "block"
    "def"
    "class"
    "region"
    "file"))

(defconst py-bounds-command-names
  (list
   "block"
   "block-or-clause"
   "buffer"
   "class"
   "clause"
   "def"
   "def-or-class"
   "else-block"
   "except-block"
   "expression"
   "if-block"
   "minor-block"
   "partial-expression"
   "section"
   "statement"
   "top-level"
   "try-block"
   ))

;; statement needed by py-beginning-bol-command-names
(defconst py-beginning-bol-command-names
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "indent"
   "minor-block"
   "statement"
   "try-block"
   ))

;; backward class/def treated with shorter forms internally
(defconst py-backward-bol-command-names
  (list
   "block"
   "block-or-clause"
   "clause"
   "elif-block"
   "else-block"
   "except-block"
   "for-block"
   "if-block"
   "minor-block"
   "try-block"
   ))

(defconst py-checker-command-names
  '("clear-flymake-allowed-file-name-masks"
    "pylint-flymake-mode"
    "pyflakes-flymake-mode"
    "pychecker-flymake-mode"
    "pep8-flymake-mode"
    "pyflakespep8-flymake-mode"
    "py-pylint-doku"
    "py-pyflakes-run"
    "py-pyflakespep8-run"
    "py-pyflakespep8-help"))

(defconst py-fast-execute-forms-names
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "expression"
   "partial-expression"
   "section"
   "statement"
   "top-level"))

(defconst py-navigation-forms
  (list
   "block"
   "block-or-clause"
   "class"
   "clause"
   "def"
   "def-or-class"
   "elif"
   "else"
   "expression"
   "partial-expression"
   "section"
   "try"
   "except"
   ))

;; "top-level" doesn't make sence WRT bol
(defconst docstring-styles
  (list
   "django"
   "onetwo"
   "pep-257"
   "pep-257-nn"
   "symmetric")
  "Customizable variable ‘py-fill-docstring-style’ provides default value
  used by ‘py-fill-string’, ‘py-fill-paragraph’

  DJANGO:

      \"\"\"
      Process foo, return bar.
      \"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  ONETWO:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.

      \"\"\"

  PEP-257:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"Process foo, return bar.

      If processing fails throw ProcessingError.

      \"\"\"

  PEP-257-NN:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  SYMMETRIC:

      \"\"\"Process foo, return bar.\"\"\"

      \"\"\"
      Process foo, return bar.

      If processing fails throw ProcessingError.
      \"\"\"

  Built upon code seen at python.el, thanks Fabian")

(defconst py-fast-core
  (list
   'block
   'block-or-clause
   'class
   'clause
   'def
   'def-or-class
   'expression
   'partial-expression
   'region
   'statement
   'strg
   'top-level))

(defconst py-virtualenv-symbols
  (list
   'activate
   'deactivate
   'p
   'workon))

(defconst py-fast-forms
  (list
   'py--fast-send-string
   'py-process-region-fast
   'py-execute-statement-fast
   'py-execute-block-fast
   'py-execute-block-or-clause-fast
   'py-execute-def-fast
   'py-execute-class-fast
   'py-execute-def-or-class-fast
   'py-execute-expression-fast
   'py-execute-partial-expression-fast
   'py-execute-top-level-fast
   'py-execute-clause-fast))

(defconst py-bol-menu-forms
  (list
   'py-backward-block-bol
   'py-backward-clause-bol
   'py-backward-block-or-clause-bol
   'py-backward-def-bol
   'py-backward-class-bol
   'py-backward-def-or-class-bol
   'py-backward-if-block-bol
   'py-backward-try-block-bol
   'py-backward-minor-block-bol
   'py-backward-statement-bol))

(defconst py-bol-end-forms
  (list
   'py-forward-block-bol
   'py-forward-clause-bol
   'py-forward-block-or-clause-bol
   'py-forward-def-bol
   'py-forward-class-bol
   'py-forward-def-or-class-bol
   'py-forward-if-block-bol
   'py-forward-try-block-bol
   'py-forward-minor-block-bol
   'py-forward-statement-bol))

(defconst py-bol-copy-forms
  (list
   'py-copy-block-bol
   'py-copy-clause-bol
   'py-copy-block-or-clause-bol
   'py-copy-def-bol
   'py-copy-class-bol
   'py-copy-def-or-class-bol
   'py-copy-statement-bol))

(defconst py-other-symbols
  (list
   'boolswitch
   'empty-out-list-backward
   'kill-buffer-unconditional
   'remove-overlays-at-point))

(defconst py-pyflakes-pep8-symbols
  (list
   'py-pyflakes-pep8-run
   'py-pyflakes-pep8-help
   'pyflakes-pep8-flymake-mode))

(defconst py-flake8-symbols
  (list
   'py-flake8-run
   'py-flake8-help))

(defconst py-pyflakes-symbols
  (list
   'py-pyflakes-run
   'py-pyflakes-help
   'pyflakes-flymake-mode))

(defconst py-pep8-symbols
  (list
   'py-pep8-run
   'py-pep8-help
   'pep8-flymake-mode))

(defconst py-pylint-symbols
  (list
   'py-pylint-run
   'py-pylint-help
   'pylint-flymake-mode))

(defconst py-checks-symbols
  (list
   'py-flycheck-mode
   'py-pychecker-run))

(defconst py-debugger-symbols
  (list
   'py-execute-statement-pdb
   'pdb))

(defconst py-help-symbols
  (list
   'py-find-definition
   'py-help-at-point
   'py-info-lookup-symbol
   'py-symbol-at-point))

(defconst py-completion-symbols
  (list
   'py-complete
   'py-indent-or-complete
   'py-shell-complete
   ))

(defconst py-skeletons
  (list
   'else-statement
   'for-statement
   'if-statement
   'py-try/except-statement
   'py-try/finally-statement
   'while-statement
   ))

(defconst py-filling-symbols
  (list
   'py-docstring-style
   'py-fill-comment
   'py-fill-paragraph
   'py-fill-string
   'py-fill-string-django
   'py-fill-string-onetwo
   'py-fill-string-pep-257
   'py-fill-string-pep-257-nn
   'py-fill-string-symmetric
   ))

(defconst py-electric-symbols
  (list
   'complete-electric-comma
   'complete-electric-lparen
   'electric-backspace
   'electric-colon
   'electric-comment
   'electric-delete
   'electric-yank
   'hungry-delete-backwards
   'hungry-delete-forward
   ))

(defconst py-completion-symbols
  (list
   'py-indent-or-complete
   'py-shell-complete
   'py-complete
   ))

(defconst py-skeletons
  (list
   'else-statement
   'for-statement
   'if-statement
   'py-try/except-statement
   'py-try/finally-statement
   'while-statement
   ))

(defconst py-filling-symbols
  (list
   'py-docstring-style
   'py-fill-comment
   'py-fill-paragraph
   'py-fill-string
   'py-fill-string-django
   'py-fill-string-onetwo
   'py-fill-string-pep-257
   'py-fill-string-pep-257-nn
   'py-fill-string-symmetric
   ))

(defconst py-electric-symbols
  (list
   'complete-electric-comma
   'complete-electric-lparen
   'electric-backspace
   'electric-colon
   'electric-comment
   'electric-delete
   'electric-yank
   'hungry-delete-backwards
   'hungry-delete-forward
   ))

(defconst py-other-symbols
  (list
   'boolswitch
   'empty-out-list-backward
   'kill-buffer-unconditional
   'remove-overlays-at-point
   ))

(defconst py-pyflakes-pep8-symbols
  (list
   'py-pyflakes-pep8-run
   'py-pyflakes-pep8-help
   'pyflakes-pep8-flymake-mode
   ))

(defconst py-flake8-symbols
  (list
   'py-flake8-run
   'py-flake8-help
   ))

(defconst py-pyflakes-symbols
  (list
   'py-pyflakes-run
   'py-pyflakes-help
   'pyflakes-flymake-mode
   ))

(defconst py-pep8-symbols
  (list
   'py-pep8-run
   'py-pep8-help
   'pep8-flymake-mode
   ))

(defconst py-pylint-symbols
  (list
   'py-pylint-run
   'py-pylint-help
   'pylint-flymake-mode
   ))

(defconst py-checks-symbols
  (list
   'py-flycheck-mode
   'py-pychecker-run
   ))

(defconst py-debugger-symbols
  (list
   'py-execute-statement-pdb
   'pdb
   ))

(defconst py-help-symbols
  (list
   'py-find-definition
   'py-help-at-point
   'py-info-lookup-symbol
   'py-symbol-at-point
   ))

(defconst arkopf
      "\n;; Copyright (C) 2015-2020 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: languages

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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:

")

(defun py-write-file-forms ()
  "Write ‘py-execute-file-python...’ etc."
  (interactive)
  (set-buffer (get-buffer-create "python-components-execute-file.el"))
  (erase-buffer)
  (insert ";;; python-components-execute-file --- Runs files -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (insert ";; Execute file given\n\n")
  (when (called-interactively-p 'interactive)
    (switch-to-buffer (current-buffer)))
  (goto-char (point-max))
  (dolist (elt py-shells)
    (setq ele (format "%s" elt))
    (insert (concat "(defun py-execute-file-" ele " (filename)"))
    (insert (concat "
  \"Send file to " (py--prepare-shell-name ele) " interpreter\"
  (interactive \"fFile: \")
  (py--execute-base nil nil \"" ele "\" filename))\n\n")))
  (dolist (elt py-shells)
    (setq ele (format "%s" elt))
    (insert (concat "(defun py-execute-file-" ele "-dedicated (filename)"))
    (insert (concat "
  \"Send file to a dedicated" (py--prepare-shell-name ele) " interpreter\"
  (interactive \"fFile: \")
  (py--execute-base nil nil \"" ele "\" filename nil t t nil t))\n\n")))
  (insert "(provide 'python-components-execute-file)
;;; python-components-execute-file.el ends here\n")
  (when (called-interactively-p 'interactive)
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-execute-file.el")))

(defun py--exexutable-name (ele)
  "Return \"IPython\" for \"ipython\" etc."
  (let (erg)
    (if (string-match "ipython" ele)
	(concat "IP" (substring ele 2))
      (capitalize ele))))

;; forms not to be declined with all variants
(defun py-write-execute-forms (&optional command)
  "Write ‘py-execute-block...’ etc."
  (interactive)
    (set-buffer (get-buffer-create "python-components-exec-forms.el"))
    (erase-buffer)
    (insert ";;; python-components-exec-forms.el --- Forms with a reduced range of derived commands -*- lexical-binding: t; -*-\n")
    (insert arkopf)
    (insert ";; Execute forms at point\n\n")
    (dolist (ele py-extra-execute-forms)
      (insert (concat "(defun py-execute-" ele " ()"))
      (insert (concat "
  \"Send " ele " at point to Python default interpreter.\"\n"))
      (insert (concat "  (interactive)
  (let ((beg (prog1
                 (or (py--beginning-of-" ele "-p)
                     (save-excursion
                       (py-backward-" ele ")))))
        (end (save-excursion
               (py-forward-" ele"))))
    (py-execute-region beg end)))\n\n")))
  (insert "(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here\n ")
  (when (called-interactively-p 'interactive)
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode))
    (write-file (concat components-directory "/python-components-exec-forms.el")))

(defun write-options-dokumentation-subform (pyo)
  (cond ((string-match "dedicated" pyo)
         (insert "\n\nUses a dedicated shell.")))
  (cond ((string-match "no-switch" pyo)
         (insert "\nIgnores default of ‘py-switch-buffers-on-execute-p’, uses it with value \\\"nil\\\""))
        ((string-match "switch" pyo)
         (insert "\nIgnores default of ‘py-switch-buffers-on-execute-p’, uses it with value \\\"non-nil\\\""))))

(defun write-execute-ert-tests (&optional command path-to-shell option)
  "Write ‘py-execute-block...’ etc."
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-test-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-executes-test.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-executes-ert-tests.el"))
    (insert " --- executes ert tests")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      (insert (concat "(defun py-execute-" ele "-test ()
  (py-tests-with-temp-buffer \n    "))
      (cond ((or (string-match "block" ele)(string-match "clause" ele))
             (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
            ((string-match "def" ele)
             (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
            ((string= "class" ele)
             (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
            (t (insert (concat "\"print(\\\"I'm the py-execute-" ele))))
      (insert "-test\\\")\"))")
      (insert (concat "
    (py-execute-" ele ")"))
      (insert (concat "    (set-buffer (py--fetch-first-python-buffer))
    (goto-char (point-max))
    (and (should (search-backward \"py-execute-" ele " -test" nil t 1))
         (py-kill-buffer-unconditional (current-buffer))))

  (insert "\n\n(provide 'python-executes-ert-tests)
;;; python-executes-ert-tests.el ends here\n ")
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun write-shell-arg-ert-tests (&optional command path-to-shell option)
  "Write ‘py-shell...’ etc."
  (interactive)
  (set-buffer (get-buffer-create "py-shell-arg-ert-tests.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; py-shell-arg-ert-tests.el --- py-shell ert tests -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (dolist (ele py-shells)
    (setq ele (format "%s" ele))
    (let ((buffer (prin1-to-string (py--choose-buffer-name ele)))
	  ;; (concat "*" ele "*")
	  (arg (concat "py-" ele "-command-args")))
      (insert (concat "(ert-deftest py-ert-" ele "-shell-test ()\n"))
      (insert (make-string 2 ?\ ))
      (insert "(let (("arg"  (list \"-i -c\\\"abc=4\\\"\")))\n")
      (insert (make-string 4 ?\ ))
      (insert (concat "(py-kill-buffer-unconditional " buffer")\n"))
      (insert (make-string 4 ?\ ))
      (insert (concat "(" ele ")\n"))
      (insert (make-string 4 ?\ ))
      (insert (concat "(should (buffer-live-p (get-buffer " buffer ")))\n"))
      (insert (make-string 4 ?\ ))
      ;; comint-last-input-end
      (insert (concat "(set-buffer (get-buffer " buffer "))\n"))
      (insert (make-string 4 ?\ ))
      (insert "(should (string= \"4\" py-result))\n")
      (insert (make-string 4 ?\ ))
      (insert "(should (< 1 comint-last-input-end))))\n\n")))
  (insert "(provide 'py-shell-arg-ert-tests)
;;; py-shell-arg-ert-tests.el ends here\n ")
  (emacs-lisp-mode)
  (write-file (concat components-directory "/test/py-shell-arg-ert-tests.el"))
  (switch-to-buffer (current-buffer)))

(defun write--extended-execute-switches (ele pyo)
  "Internally used by write-extended-execute-forms"

  (if (string-match "dedicated" pyo)
      (insert " t")
    (insert " dedicated"))
  (cond ((or (string= "switch" pyo)
             (string= "dedicated-switch" pyo))
         (insert " 'switch"))
        ((string= "no-switch" pyo)
         (insert " 'no-switch"))
        (t (insert " switch")))
  (cond ((string= "region" ele)
         (insert " (or beg (region-beginning)) (or end (region-end))")
	 (insert " nil fast proc wholebuf split)))\n"))
        ((string= "buffer" ele)
         (insert " (point-min) (point-max)")
	 (insert " nil fast proc wholebuf split)))\n"))
	(t  (insert " nil nil nil fast proc wholebuf split)))\n"))))

(defun write--unified-extended-execute-forms-docu (ele elt pyo)
  (insert (concat "
  \"Send " ele " at point to"))
  (cond ((string-match "ipython" elt)
	 (insert " IPython"))
	((string= "python" elt)
	 (insert " default"))
	(t (insert (concat " " (capitalize elt)))))
  (cond ((string= pyo "dedicated")
	 (insert " unique interpreter."))
	((string= pyo "dedicated-switch")
	 (insert " unique interpreter and switch to result."))
	(t (insert " interpreter.")))
  (cond ((string= pyo "switch")
	 (insert "\n\nSwitch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."))
	((string= pyo "no-switch")
	 (insert "\n\nKeep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ ")))
  (when (string= "python" elt)
    (insert "\n\nFor ‘default’ see value of ‘py-shell-name’"))
  (insert "\"\n"))

(defun write--unified-extended-execute-forms-arglist-intern (ele pyo elt arglist)
  (let ((args (cond ((string= "" pyo)
		     arglist)

		    ((string-match (concat pyo " *") arglist)
		     (replace-regexp-in-string (concat pyo " *") "" arglist))
		    ((string= "dedicated-switch" pyo)
		     (replace-regexp-in-string "dedicated\\|switch" "" arglist))
		    ((string= "no-switch" pyo)
		     (replace-regexp-in-string "switch" "" arglist))

		    (t arglist))))
    ;; (message "%s" args)
    (if (string= "region" ele)
	(if (string= "" elt)
	    (insert (concat " (beg end &optional shell " args ")"))
	  (insert (concat " (beg end &optional " args ")")))
      (if (string= "" elt)
	  (insert (concat " (&optional shell " args ")"))
	(insert (concat " (&optional " args ")"))))))

(defun write--unified-extended-execute-forms-arglist (ele pyo elt)
  (let ((erst (if (string= "" elt)
		  "&optional shell"
		"&optional"))
	(arglist "dedicated fast split switch proc"))
    (write--unified-extended-execute-forms-arglist-intern ele pyo elt arglist)))

(defun write--unified-extended-execute-forms-interactive-spec (ele)
  (cond
   ((string= "region" ele)
    (insert "  (interactive \"r\")\n"))
   (t (insert "  (interactive)\n"))))

(defun write--unified-extended-execute-buffer-form ()
  (insert "  (let ((py-master-file (or py-master-file (py-fetch-py-master-file)))
        (wholebuf t)
        filename buffer)
    (when py-master-file
      (setq filename (expand-file-name py-master-file)
	    buffer (or (get-file-buffer filename)
		       (find-file-noselect filename)))
      (set-buffer buffer))\n"))

(defun write--unified-extended-execute-let-form ()
  (insert "  (let (wholebuf)\n"))

(defun write--unified-extended-execute-shells (elt)
  (if (string= "" elt)
      (insert " shell")
    (insert (concat " '" elt))))

(defun write--unified-extended-execute-forms-intern ()
  (switch-to-buffer (current-buffer))
  (goto-char (point-max))
  (dolist (ele py-execute-forms)
    (dolist (elt py-shells)
      (setq elt (format "%s" elt))
      (dolist (pyo py-full-options)
	(insert (concat "\n(defun py-execute-" ele))
	(unless (string= "" elt)
	  (insert (concat "-" elt)))
	(unless (string= pyo "")(insert (concat "-" pyo)))
	(write--unified-extended-execute-forms-arglist ele pyo elt)
	(write--unified-extended-execute-forms-docu ele elt pyo)
	(write--unified-extended-execute-forms-interactive-spec ele)
	(if (string= "buffer" ele)
	  (write--unified-extended-execute-buffer-form)
	  (write--unified-extended-execute-let-form)
	  )
	(insert (concat "    (py--execute-prepare '"ele))
	(write--unified-extended-execute-shells elt)
	(write--extended-execute-switches ele pyo)))))

;; not ready, don't use
(defun install--unified-extended-execute-forms ()
  ;; (switch-to-buffer (current-buffer))
  ;; (goto-char (point-max))
  (interactive)
  (dolist (ele py-execute-forms)
    (dolist (elt py-shells)
      ;; (setq elt (format "%s" elt))
      (fset  (quote elt
      (dolist (pyo py-full-options)
	(concat "py-execute-" ele)
	(unless (string= "" elt)
	  (concat "-" elt))
	(unless (string= pyo "")(concat "-" pyo))))))))
	(defun write-unified-extended-execute-forms ()
  "Write ‘py-execute-statement, ...’ etc.

Include default forms "
  (interactive)
  (set-buffer (get-buffer-create "python-components-extended-executes.el"))
  (erase-buffer)
  (insert ";; Extended executes --- more execute forms\n")
  (insert arkopf)
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write--unified-extended-execute-forms-intern)
  (insert "\n(provide 'python-components-extended-executes)
;;; python-components-extended-executes.el ends here")
  (write-file (concat components-directory "/python-components-extended-executes.el"))
  )

(defun pmu-fix-ipython (ele)
  (when (string-match "^ipython.*" ele)
	(skip-chars-backward "[a-z][0-9]\\.")
	(delete-char 1)
	(insert "P")
	(end-of-line)))

;; (defun py-install-installed-shells-commands ()
;;   "Reads py-shells, provides commands opening these shell."
;;   (interactive)

;;   (set-buffer (get-buffer-create "py-install-installed-shells-commands"))
;;   (erase-buffer)
;;   ;; (insert ";;; Python named shells -*- lexical-binding: t; -*- \n
;;   ;; This file is generated by python-mode-utils.el")
;;   ;; (goto-char (point-max))
;;   ;; (insert arkopf)
;;   ;; (goto-char (point-max))
;;   (when (called-interactively-p 'interactive)
;;     (switch-to-buffer (current-buffer)))
;;   ;; (goto-char (point-max))
;;   ;; (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
;;   (emacs-lisp-mode)
;;   (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(fset " ele " (lambda (&optional argprompt args buffer fast exception-buffer split)
;;   \"Start an "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")
;;   (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))\n\n"))))
;;   (insert ";; dedicated\n")
;;   (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defun " ele "-dedicated (&optional argprompt args buffer fast exception-buffer split)
;;   \"Start an unique "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")\n"))
;;       (insert (concat "  (py-shell argprompt args t \"" ele "\" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))\n\n"))))
;;   (insert ";; switch\n")
;;   (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defun " ele "-switch (&optional argprompt args buffer fast exception-buffer split)
;;   \"Switch to "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")\n"))
;;       (insert (concat "  (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split t))\n\n"))))
;;   (insert ";; no-switch\n")
;;   (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defun " ele "-no-switch (&optional argprompt args buffer fast exception-buffer split)
;;   \"Open an "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window, but do not switch to it.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))

;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")\n"))
;;       (insert (concat "  (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split))\n\n"))))
;;   (insert ";; dedicated switch\n")
;;   (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defalias '" ele "-dedicated-switch '" ele "-switch-dedicated)\n"))
;;       (insert (concat "(defun " ele "-switch-dedicated (&optional argprompt args buffer fast exception-buffer split)
;;   \"Switch to an unique "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert " interpreter.\"
;;   \(interactive \"p\")\n")
;;       (insert (concat "  (py-shell argprompt args t \"" ele "\" buffer fast exception-buffer split t))\n\n"))))
;;   ;; (py-shell &optional argprompt dedicated shell buffer-name fast exception-buffer)
;;   (insert "(provide 'python-components-named-shells)
;; ;;; python-components-named-shells.el ends here
;; ")

;;   (emacs-lisp-mode)
;;   (write-file (concat components-directory "/python-components-named-shells.el")))

(defun py-provide-installed-shells-commands ()
  "Reads py-shells, provides commands opening these shell."
  (interactive)
  (set-buffer (get-buffer-create "python-components-named-shells.el"))
  (erase-buffer)
  (insert ";;; Python named shells -*- lexical-binding: t; -*- \n
;; This file is generated by python-mode-utils.el")
  (goto-char (point-max))
  (insert arkopf)
  (goto-char (point-max))
  (when (called-interactively-p 'interactive)
    (switch-to-buffer (current-buffer)))
  (goto-char (point-max))
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (dolist (ele py-shells)
    (unless (string= ele "")
      (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
      (insert (concat "(defun " ele " (&optional argprompt args buffer fast exception-buffer split)
  \"Start an "))
      (insert (capitalize ele))
      (pmu-fix-ipython ele)
      (insert (concat " interpreter.

With optional \\\\[universal-argument] get a new dedicated "))
      (insert (concat "shell.\"
  (interactive \"p\")
  (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))\n\n"))))
  ;; (insert ";; dedicated\n")
  ;; (dolist (ele py-shells)
  ;;   (unless (string= ele "")
  ;;     (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
  ;;     (insert (concat "(defun " ele "-dedicated (&optional argprompt args buffer fast exception-buffer split)
  ;; \"Start an unique "))
  ;;     (insert (capitalize ele))
  ;;     (pmu-fix-ipython ele)
  ;;     (insert (concat " interpreter in another window.\"
  ;; (interactive \"p\")\n"))
  ;;     (insert (concat "  (py-shell argprompt args  t \"" ele "\" buffer fast exception-buffer split (unless argprompt (eq 1 (prefix-numeric-value argprompt)))))\n\n"))))
  ;; (insert ";; switch\n")
  ;; (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defun " ele "-switch (&optional argprompt args buffer fast exception-buffer split)
;;   \"Switch to "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")\n"))
;;       (insert (concat "  (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split t))\n\n"))))
  ;; (insert ";; no-switch\n")
  ;; (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defun " ele "-no-switch (&optional argprompt args  buffer fast exception-buffer split)
;;   \"Open an "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window, but do not switch to it.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))

;;       (insert (concat " interpreter.\"
;;   (interactive \"p\")\n"))
;;       (insert (concat "  (py-shell argprompt args nil \"" ele "\" buffer fast exception-buffer split))\n\n"))))
  ;; (insert ";; dedicated switch\n")
  ;; (dolist (ele py-shells)
;;     (unless (string= ele "")
;;       (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
;;       (insert (concat "(defalias '" ele "-dedicated-switch '" ele "-switch-dedicated)\n"))
;;       (insert (concat "(defun " ele "-switch-dedicated (&optional argprompt args buffer fast exception-buffer split)
;;   \"Switch to an unique "))
;;       (insert (capitalize ele))
;;       (pmu-fix-ipython ele)
;;       (insert (concat " interpreter in another window.

;; Optional ARG \\\\[universal-argument] prompts for path to the"))
;;       (insert " interpreter.\"
;;   \(interactive \"p\")\n")
;;       (insert (concat "  (py-shell argprompt args t \"" ele "\" buffer fast exception-buffer split t))\n\n"))))
  ;; (py-shell &optional argprompt dedicated shell buffer-name fast exception-buffer)
  (insert "(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
")

  (emacs-lisp-mode)
  (write-file (concat components-directory "/python-components-named-shells.el")))

(defun py-write-installed-shells-menu ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "python-components-installed-shells-menu.el")
    (erase-buffer)
    (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	  (emacs-lisp-mode))
    (insert " 		  (\"Other\"
		   :help \"Alternative Python Shells\"")
    (newline)
    (dolist (ele py-shells)
      (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
      (unless (string= "python" ele)
	(emen ele)
	(skip-chars-forward "^]")
	(forward-char 1)
	(newline)))
    ;; dedicated
    (insert "\n(\"Dedicated\"
		   :help \"Dedicated Shells\"")
    (dolist (ele py-shells)
      (emen (replace-regexp-in-string "\\\\" "" (concat (prin1-to-string ele) "-dedicated")))
      (skip-chars-forward "^]")
      (forward-char 1)
      (newline))
    (insert ")")
    (newline)
    ;; switch
    (insert "\n(\"Switch\"
		   :help \"Switch to shell\"")
    (dolist (ele py-shells)
      (emen (replace-regexp-in-string "\\\\" "" (concat (prin1-to-string ele) "-switch")))
      (skip-chars-forward "^]")
      (forward-char 1)
      (newline))
    (insert ")")
    (insert ")")
    (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	  (emacs-lisp-mode))
    (write-file (concat components-directory "/devel/python-components-installed-shells-menu.el"))))

(defun py-write-installed-shells-test-intern ()
  (dolist (ele py-shells)
      (setq ele (replace-regexp-in-string "\\\\" "" (prin1-to-string ele)))
      (insert (concat "\n(ert-deftest " ele))
      ;; (pmu-fix-ipython ele)
      ;; (when arg (insert (concat "-" arg)))
      (insert (concat "-shell-test ()
  (py-kill-buffer-unconditional \"*" (capitalize ele)))
      (pmu-fix-ipython ele)
      ;; (when arg (insert (concat "-" arg)))
      (insert (concat "*\")
  (" ele ")
  (should (buffer-live-p (get-buffer \"*" (capitalize ele)))
      (pmu-fix-ipython ele)
      ;; (when arg (insert (concat "-" arg)))
      (insert "*\"))))\n")))

(defun py-write-installed-shells-test ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "python-components-installed-shells-test.el")
    (erase-buffer)
    (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	  (emacs-lisp-mode))
    (py-write-installed-shells-test-intern)
    (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	  (emacs-lisp-mode))
    (write-file (concat components-directory "/test/python-components-installed-shells-test.el"))))

(defun py-write-shift-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "python-components-shift-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-shift-forms.el --- Move forms left or right -*- lexical-binding: t; -*- \n")
  (insert arkopf)

  (insert "
\(defun py-shift-left (&optional count start end)
  \"Dedent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is dedented.
Return indentation reached
Optional COUNT: COUNT times ‘py-indent-offset’
Optional START: region beginning
Optional END: region end\"
  (interactive \"p\")
  (let ((erg (py--shift-intern (- count) start end)))
    (when (and (called-interactively-p 'interactive) py-verbose-p) (message \"%s\" erg))
    erg))

\(defun py-shift-right (&optional count beg end)
  \"Indent region according to ‘py-indent-offset’ by COUNT times.

If no region is active, current line is indented.
Return indentation reached
Optional COUNT: COUNT times ‘py-indent-offset’
Optional BEG: region beginning
Optional END: region end\"
  (interactive \"p\")
  (let ((erg (py--shift-intern count beg end)))
    (when (and (called-interactively-p 'interactive) py-verbose-p) (message \"%s\" erg))
    erg))

\(defun py--shift-intern (count &optional start end)
  (save-excursion
    (let\* ((inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((use-region-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((use-region-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position))))
           (orig end))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (if (< 0 count)
          (indent-rigidly beg end py-indent-offset)
        (indent-rigidly beg end (- py-indent-offset)))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward \" \\t\\r\\n\\f\"))
    (py-indentation-of-statement)))

\(defun py--shift-forms-base (form arg &optional beg end)
  (let\* ((begform (intern-soft (concat \"py-backward-\" form)))
         (endform (intern-soft (concat \"py-forward-\" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((use-region-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((use-region-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py--shift-intern arg beg end)))
    (goto-char orig)
    erg))
")
  (dolist (ele py-shift-forms)
    (insert (concat "
\(defun py-shift-" ele "-right (&optional arg)
  \"Indent " ele " by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \\[universal-argument] to specify a different value.

Return outmost indentation reached.\"
  (interactive \"\*P\")
  (let ((erg (py--shift-forms-base \"" ele "\" (or arg py-indent-offset))))
        (when (called-interactively-p 'interactive) (message \"%s\" erg))
    erg))

\(defun py-shift-" ele "-left (&optional arg)
  \"Dedent " ele " by COUNT spaces.

COUNT defaults to ‘py-indent-offset’,
use \\[universal-argument] to specify a different value.

Return outmost indentation reached.\"
  (interactive \"\*P\")
  (let ((erg (py--shift-forms-base \"" ele "\" (- (or arg py-indent-offset)))))
    (when (called-interactively-p 'interactive) (message \"%s\" erg))
    erg))
")))
      (insert "\n(provide 'python-components-shift-forms)
;;; python-components-shift-forms.el ends here\n ")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (write-file (concat components-directory "/python-components-shift-forms.el"))
    )

;; (defun py-write-down-forms-bol ()
;;   " "
;;   (interactive)
;;   (set-buffer (get-buffer-create "py-end-of-form-bol-commands.txt"))
;;   (erase-buffer)
;;   (dolist (ele py-down-forms)
;;     (insert (concat "py-end-of-" ele "-bol\n")))
;;   (set-buffer (get-buffer-create "py-end-of-form-bol.el"))
;;   (erase-buffer)
;;   (insert ";; Complementary left corner end of form commands")
;;   (dolist (ele py-down-forms)
;;     (insert (concat "
;; \(defalias 'py-down-" ele "-bol 'py-end-of-" ele "-bol)
;; \(defun py-forward-" ele "-bol ()
;;   \"Goto beginning of line following end of " ele ".
;; Return position reached, if successful, nil otherwise.

;; A complementary command travelling at beginning of line, whilst ‘py-forward-" ele "’ stops at right corner.
;; See also ‘py-down-" ele "’: down from current definition to next beginning of " ele " below.\"
;;   (interactive)
;;   (let ((erg (py-forward-" ele ")))
;;     (when erg
;;       (unless (eobp)
;;         (forward-line 1)
;;         (beginning-of-line)
;;         (setq erg (point))))
;;   (when (called-interactively-p 'interactive) (message \"%s\" erg))
;;   erg))
;; "))
;;     (emacs-lisp-mode)
;;     (switch-to-buffer (current-buffer))))

(defun py-write-specifying-shell-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "specifying-shell-forms"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-execute-region-" ele " (start end)
  \"Send the region to a common shell calling the " ele " interpreter.\"
  (interactive \"r\\nP\")
  (py--execute-base start end \"" ele "\"))

\(defun py-execute-region-" ele "-switch (start end)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of ‘py-switch-buffers-on-execute-p’, output-buffer will being switched to.\"
  (interactive \"r\\nP\")
  (let ((py-switch-buffers-on-execute-p t))
    (py--execute-base start end async \"" ele "\")))

\(defun py-execute-region-" ele "-no-switch (start end)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of ‘py-switch-buffers-on-execute-p’, output-buffer will not being switched to.\"
  (interactive \"r\\nP\")
  (let ((py-switch-buffers-on-execute-p))
    (py--execute-base start end async \"" ele "\")))"))))

(defun xemacs-remove-help-strings ()
  "menu :help not supported presently at XEmacs."
  (interactive "*")
  (let (erg)
    (goto-char (point-min))
    (while
        (and (search-forward ":help" nil t 1)(not (ar-in-string-or-comment-p)))
      (save-match-data
        (skip-chars-forward "[[:blank:]\"]+")
        (ar-kill-string-atpt)
        (setq erg (point))
        (push-mark))
      (goto-char (match-beginning 0))
      (delete-region (point) erg)
      (if (py-empty-line-p)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
        (push-mark)
        (setq erg (point))
        (skip-chars-backward " \t\r\n\f")
        (delete-region (point) erg))))
  (message "%s" "fertig"))

(defconst py-noregexp-forms
  (list
   "paragraph"
   "line"
   "statement"
   "expression"
   "partial-expression"))

(defconst py-regexp-forms
  (list
   "block"
   "clause"
   "block-or-clause"
   "def"
   "class"
   "def-or-class"))

(defun write-toggle-forms (&optional arg)
  "Write toggle-forms according to (car kill-ring) "
  (interactive "P")
  (let ((liste (if (eq 4 (prefix-numeric-value arg))
                   (car kill-ring)
                 py-toggle-form-vars))
        done buffer-out first menu-buffer)
    (if (listp liste)
        (dolist (ele liste)
          (write-toggle-forms-intern ele))
      (write-toggle-forms-intern liste))))

(defun write-toggle-forms-intern (ele)
      (if done
          (set-buffer buffer-out)
        (setq buffer-out (capitalize ele))
        (set-buffer (get-buffer-create buffer-out))
        (erase-buffer)
        (insert (concat ";; " ele " forms\n\n"))
        (setq done t))
      (message "Writing for; %s" ele)
      (insert (concat "
\(defun toggle-" ele " (&optional arg)
  \"If ‘" ele "’ should be on or off.

Return value of ‘" ele "’ switched to.\"
  (interactive)
  (let ((arg (or arg (if " ele " -1 1))))
    (if (< 0 arg)
        (setq " ele " t)
      (setq " ele " nil))
    (when (or py-verbose-p (called-interactively-p 'interactive)) (message \"" ele ": %s\" " ele "))
    " ele "))

\(defun " ele "-on (&optional arg)
  \"Make sure, " ele "' is on.

Return value of ‘" ele "’.\"
  (interactive)
  (let ((arg (or arg 1)))
    (toggle-" ele " arg))
  (when (or py-verbose-p (called-interactively-p 'interactive)) (message \"" ele ": %s\" " ele "))
  " ele ")

\(defun " ele "-off ()
  \"Make sure, ‘" ele "’ is off.

Return value of ‘" ele "’.\"
  (interactive)
  (toggle-" ele " -1)
  (when (or py-verbose-p (called-interactively-p 'interactive)) (message \"" ele ": %s\" " ele "))
  " ele ")"))
      (newline)
      (emacs-lisp-mode)
      (eval-buffer)
      (if first
          (set-buffer menu-buffer)
        (setq menu-buffer (concat "Menu " ele))
        (set-buffer (get-buffer-create menu-buffer))
        (erase-buffer)
        (insert (concat ";; " ele " forms\n\n"))
        (setq first t))
      (switch-emen ele)
      ;; (set-buffer buffer-out)
      ;; (switch-to-buffer (current-buffer))
      )

(defun write-commandp-forms ()
  "Write forms according to ‘py-bounds-command-names’ "
  (interactive)
  (let ((erg py-bounds-command-names))

    (set-buffer (get-buffer-create "Commandp tests"))
    (erase-buffer)
    (dolist (ele erg)
      (insert (concat "--funcall " ele "-commandp-test \\\n")))
    (insert ";; Commandp tests")
    ;; (dolist (ele py-bounds-command-names)
    (dolist (ele erg)
      (insert (concat "
\(defun " ele "-commandp-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"\"))
  (py-bug-tests-intern '" ele "-commandp-base arg teststring)))

\(defun " ele "-commandp-base (arg)
    (assert (commandp '" ele ") nil \"" ele "-commandp-test failed\"))"))
      (newline)))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-invoke-py-shell-forms ()
  "Write forms according to ‘py-shells’ "
  (interactive)
  (set-buffer (get-buffer-create "Py-shell interactive calls"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "'py-shell-invoking-" ele "-lp:835151-test\n")))

  (set-buffer (get-buffer-create "Py-shell batch-commands"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "--funcall py-shell-invoking-" ele "-lp:835151-test \\\n")))

  (set-buffer (get-buffer-create "Py-shell tests"))
  (erase-buffer)
  (insert ";; Py-shell tests")
  ;; (dolist (ele py-bounds-command-names)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-shell-invoking-" ele "-lp:835151-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"print(\\\"py-shell-name: " ele "\\\")\"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shell-invoking-" ele "-lp:835151-base arg teststring)))

\(defun py-shell-invoking-" ele "-lp:835151-base (arg)
  (setq py-shell-name \"" ele "\")
  (assert (markerp (py-execute-buffer)) nil \"py-shell-invoking-" ele "-lp:835151-test failed\"))\n")))
  (newline)
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defalias 'fehler-python-tests 'lookup-failing-command)
(defun lookup-failing-command ()
  "From ./python-mode-tests.sh buffer, jump to definition of command.

Needs ‘elisp-find-definition’ from
http://repo.or.cz/w/elbb.git/blob/HEAD:/code/Go-to-Emacs-Lisp-Definition.el
"
  (interactive)
  (let (erg)
    (search-backward " pass")
    (forward-char -1)
    (setq erg (prin1-to-string (symbol-at-point)))
    (find-file "~/arbeit/emacs/python-modes/components-python-mode/test/python-mode-tests.sh")
    (goto-char (point-min))
    (when (search-forward erg)
      (search-forward "--funcall ")
      (setq erg (prin1-to-string (symbol-at-point)))
      (elisp-find-definition erg))
    (when (called-interactively-p 'interactive) (message "%s" erg))
    erg))

(defun py-write-up-down-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-up-down.el"))
  (erase-buffer)
  (when (called-interactively-p 'interactive)
    (switch-to-buffer (current-buffer)))
  (insert ";;; python-components-up-down.el -- Searching up/downwards in buffer -*- lexical-binding: t; -*- \n")
  (insert arkopf)
  (insert "
\(defun py-up-statement ()
  \"go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise.\"
  (interactive)
  (let (erg)
    (if (py--beginning-of-statement-p)
	(setq erg (py-backward-statement))
      (setq erg (and (py-backward-statement) (py-backward-statement))))
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))

(defun py-down-statement ()
  \"Go to the beginning of next statement downwards in buffer.

Corresponds to backward-up-list in Elisp
Return position if statement found, nil otherwise.\"
  (interactive)
  (let\* ((orig (point))
	 erg)
    (cond ((py--end-of-statement-p)
	   (setq erg
		 (and
		  (py-forward-statement)
		  (py-backward-statement)
		  (< orig (point))
		  (point))))
	  ((< orig (ignore-errors (and (py-forward-statement) (py-backward-statement))))
	   (setq erg (point)))
	  (t (setq erg (ignore-errors (< orig (and (py-forward-statement) (py-forward-statement)(py-backward-statement)))))))
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))
")
  (dolist (ele py-down-forms)
    (unless (string= ele "statement")
      (insert (concat "
\(defalias 'py-" ele "-up 'py-up-" ele ")
\(defun py-up-" ele " (&optional indent)
  \"Go to the beginning of next " ele " upwards in buffer according to INDENT.
Optional INDENT
Return position if " ele " found, nil otherwise.\"
  (interactive)
  (py-up-base 'py-"))
      (cond ((string-match "def\\|class\\|section" ele)
	     (insert (concat ele "-re indent))\n")))
	    ;; (t (insert "extended-block-or-clause-re indent))\n"))
            (t (insert (concat ele "-re indent))\n")))
	       ;; (t (insert "extended-block-or-clause-re indent))\n"))
	    )))
  ;; down
  (dolist (ele py-down-forms)
    (unless (string= ele "statement")
      (insert (concat "
\(defalias 'py-" ele "-down 'py-down-" ele ")
\(defun py-down-" ele " (&optional indent)
  \"Go to the beginning of next " ele " below in buffer according to INDENT.

Optional INDENT: honor indentation
Return position if " ele " found, nil otherwise.\"
  (interactive)
  (py-down-base 'py-" ele "-re  indent))\n"))))
  ;; up bol
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
	nil
      (insert (concat "
\(defun py-up-" ele "-bol (&optional indent)
  \"Go to the beginning of next " ele " upwards in buffer according to INDENT.

Go to beginning of line.
Return position if " ele " found, nil otherwise.\"
  (interactive)
  (py-up-base 'py-" ele "-re indent)
  (progn (beginning-of-line)(point)))\n"))))
  ;; down bol
  (dolist (ele py-down-forms)
    (if (string= "statement" ele)
        nil
      (insert (concat "
\(defun py-down-" ele "-bol (&optional indent)
  \"Go to the beginning of next " ele " below in buffer according to INDENT.

Go to beginning of line
Optional INDENT: honor indentation
Return position if " ele " found, nil otherwise \"
  (interactive)
  (py-down-base 'py-" ele "-re indent t)
  (progn (beginning-of-line)(point)))\n"))))
  (insert "\n;; python-components-up-down.el ends here
\(provide 'python-components-up-down)")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-up-down.el")))

(defun temen (&optional symbol)
  "Provide menu for toggle-commands using checkbox."
  (interactive "*")
  (let* ((erg (or symbol (car kill-ring)))
         (name (intern-soft erg))
         (doku (if (functionp name)
                   (documentation name)
                 (documentation-property name 'variable-documentation)))
         (banner1 (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)))
         (banner2 (replace-regexp-in-string " p$" " " (replace-regexp-in-string "py-" "" banner1))))
    ;; (goto-char (point-max))
    (switch-to-buffer (current-buffer))
    (save-excursion
      (insert (concat "\n\[\"" banner2 "\"
  (setq " erg "
     (not " erg "))
 :help \""))
      (when doku (insert (regexp-quote doku)))
      (insert (concat "Use ‘M-x customize-variable’ to set it permanently\"
 :style toggle :selected " erg "]\n")))
    (skip-chars-forward "[[:punct:]]")
    (capitalize-word 1)))

(defun switch-emen (&optional symbol)
  "Provide menu draft for switches."
  (interactive "*")
  (let* ((erg (or symbol (car kill-ring)))
         (name (intern-soft erg))
         (doku (if (functionp name)
                   (documentation name)
                 (documentation-property name 'variable-documentation))))
    (switch-to-buffer (current-buffer))
    (save-excursion
      ;; ("py-switch-buffers-on-execute-p"
      ;; :help "Toggle ‘py-switch-buffers-on-execute-p’"
      (insert (concat "(\"" (replace-regexp-in-string "-" " " (replace-regexp-in-string "py-" "" erg)) "\"
 :help \"Toggle ‘" erg "’\"
")))
    (capitalize-word 1)
    (goto-char (point-max))
    (emen (concat "toggle-" symbol))
    (goto-char (point-max))
    (emen (concat symbol "-on"))
    (goto-char (point-max))
    (emen (concat symbol "-off"))
    (goto-char (point-max))
    (insert "\n)\n"))
  (emacs-lisp-mode))

(defun write-py-comment-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-comment.el"))
  (erase-buffer)
  (insert ";;; python-components-comment.el -- Comment/uncomment python constructs at point -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (insert "
\(defun py-comment-region (beg end &optional arg)
  \"Like ‘comment-region’ but uses double hash (‘#’) comment starter.\"
  (interactive \"r\\nP\")
  (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start)))
    (comment-region beg end arg)))\n
")
  (dolist (ele py-comment-forms)
    (insert (concat "(defun py-comment-" ele " (&optional beg end arg)
  \"Comments " ele " at point.

Uses double hash (‘#’) comment starter when ‘py-block-comment-prefix-p’ is  t,
the default\"
  (interactive \"\*\")
  (save-excursion
    (let ((comment-start (if py-block-comment-prefix-p
                             py-block-comment-prefix
                           comment-start))
          (beg (or beg (py--beginning-of-" ele "-position)))
          (end (or end (py-end-of-" ele "-position))))
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (comment-region beg end arg))))\n\n")))
  (insert "\n;; python-components-comment ends here
\(provide 'python-components-comment)")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-comment.el")))

  ;; (set-buffer (get-buffer-create "Menu-Python-Components-Comments"))
  ;; (erase-buffer)
  ;; (insert "(\"Comment ...\"
  ;;           :help \"Comment forms\"\n\n")

  ;; (switch-to-buffer (current-buffer))
  ;; (dolist (ele py-comment-forms)
  ;;   (setq name (concat "py-comment-" ele))
  ;;   (write-menu-entry name))
  ;; (insert "      ))")
  ;; (emacs-lisp-mode)
  ;; (switch-to-buffer (current-buffer)))

(defun py-write-mark-bol ()
  (interactive)
    (set-buffer (get-buffer-create "mark-bol.el"))
    (erase-buffer)
    (dolist (ele py-navigate-forms)
      (insert (concat "
\(defun py-mark-" ele "-bol ()
  \"Mark " ele ", take beginning of line positions.

Return beginning and end positions of region, a cons.\"
  (interactive)
  (let (erg)
    (setq erg (py--mark-base-bol \"" ele "\"))
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))
")))
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode))

(defun py--insert-backward-def-or-class-forms ()
  (dolist (ele py-backward-def-or-class-forms)
    (insert (concat "
\(defun py-backward-" ele " ()"))
    ;; (if (string-match "def\\|class" ele)
    ;; 	(insert "&optional decorator)")
    ;; (insert ")"))
    (insert (concat "\n \"Go to beginning of ‘" ele "’.

If already at beginning, go one ‘" ele "’ backward."))
    ;; (when (string-match "def\\|class" ele)
    ;;   (insert "\nOptional DECORATOR\n"))
    (insert (concat "
Return beginning of form if successful, nil otherwise\"\n"))
    (insert "  (interactive)
  (let (erg)")
    ;; (cond
    ;; 	  ((string-match "def\\|class" ele)
    ;; 	   (insert (concat "
    ;; (cdr-safe (py--go-to-keyword 'py-" ele "-re decorator)))\n")))
    ;; 	  (t (insert (concat "
    ;; (cdr-safe (py--go-to-keyword 'py-" (ar-block-regexp-name-richten ele) "-re)))\n")))
    ;;)
    (insert (concat "
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-" (ar-block-regexp-name-richten ele) "-re))))"))
    ;; (setq erg (py--backward-regexp 'py-" (ar-block-regexp-name-richten ele) "-re (current-indentation)))"))
    (when (string-match  "def\\|class$\\|block$" ele)
    (insert "\n    (when py-mark-decorators (and (py-backward-decorator)
                                                 (setq erg (point))))"))
    (insert "\n    erg))\n")))

(defun py--insert-backward-def-or-class-bol-forms ()
  ;; bol forms
  (dolist (ele py-backward-def-or-class-forms)
    (when (or (string-match "def" ele) (string-match "class" ele))
      (insert "\n;;;###autoload"))
    (insert (concat "
\(defun py-backward-" ele "-bol ()"))
    ;; (if (string-match "def\\|class" ele)
    ;; 	(insert "&optional decorator)")
    ;;   (insert ")"))
    (insert (concat "
  \"Go to beginning of ‘" ele "’, go to BOL."))
    ;; (when (string-match "def\\|class" ele)
    ;;   (insert  "\nOptional DECORATOR\n"))

(insert (concat "
If already at beginning, go one ‘" ele "’ backward.
Return beginning of ‘" ele "’ if successful, nil otherwise"))
    (insert "\"\n")
    (insert "  (interactive)")
	   (insert (concat "
  (and (py-backward-" ele ")
       (progn (beginning-of-line)(point))))\n"))))

(defun py--insert-backward-minor-block-forms ()
  (dolist (ele py-backward-minor-block-forms)
    (insert (concat "
\(defun py-backward-" ele " ()"))
    ;; (if (string-match "def\\|class" ele)
    ;; 	(insert "&optional decorator)")
    ;; (insert ")"))
    (insert (concat "\n \"Go to beginning of ‘" ele "’.

If already at beginning, go one ‘" ele "’ backward."))
    ;; (when (string-match "def\\|class" ele)
    ;;   (insert "\nOptional DECORATOR\n"))
    (insert (concat "
Return beginning of form if successful, nil otherwise\"\n"))
    (insert "  (interactive)
  (let (erg)")
    ;; (cond
    ;; 	  ((string-match "def\\|class" ele)
    ;; 	   (insert (concat "
    ;; (cdr-safe (py--go-to-keyword 'py-" ele "-re decorator)))\n")))
    ;; 	  (t (insert (concat "
    ;; (cdr-safe (py--go-to-keyword 'py-" (ar-block-regexp-name-richten ele) "-re)))\n")))
    ;;)
    (insert (concat "
    (setq erg (car-safe (cdr-safe (py--go-to-keyword 'py-" (ar-block-regexp-name-richten ele) "-re))))"))
    ;; (setq erg (py--backward-regexp 'py-" (ar-block-regexp-name-richten ele) "-re (current-indentation)))"))
    (insert "\n    erg))\n")))

(defun py--insert-backward-minor-block-bol-forms ()
  ;; bol forms
  (dolist (ele py-backward-minor-block-forms)
    (insert (concat "
\(defun py-backward-" ele "-bol ()"))
    ;; (if (string-match "def\\|class" ele)
    ;; 	(insert "&optional decorator)")
    ;;   (insert ")"))
    (insert (concat "
  \"Go to beginning of ‘" ele "’, go to BOL."))
    ;; (when (string-match "def\\|class" ele)
    ;;   (insert  "\nOptional DECORATOR\n"))

(insert (concat "
If already at beginning, go one ‘" ele "’ backward.
Return beginning of ‘" ele "’ if successful, nil otherwise"))
    (insert "\"\n")
    (insert "  (interactive)")
	   (insert (concat "
  (and (py-backward-" ele ")
       (progn (beginning-of-line)(point))))\n"))))

(defun py-write-backward-forms ()
  "Uses py-backward-forms, not ‘py-navigate-forms’.

Use backward-statement for ‘top-level’, also bol-forms don't make sense here"
  (interactive)
  (set-buffer (get-buffer-create "python-components-backward-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-backward-forms.el --- Go to beginning of form or further backward -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (insert "(defun py-backward-region ()
  \"Go to the beginning of current region.\"
  (interactive)
  (let ((beg (region-beginning)))
    (when beg (goto-char beg))))
")

  ;; don't handle (partial)-expression forms here
  (py--insert-backward-def-or-class-forms)
  (py--insert-backward-def-or-class-bol-forms)
  (py--insert-backward-minor-block-forms)
  (py--insert-backward-minor-block-bol-forms)
  (insert "\n(provide 'python-components-backward-forms)
;;; python-components-backward-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-backward-forms.el")))

(defun py-write-forms-code ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-forms-code.el"))
  (erase-buffer)
  (insert ";;; python-components-forms-code.el --- Return Python forms' code -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (dolist (ele py-execute-forms)
    (insert (concat "
\(defun py-" ele))
    (if (member ele (list "block" "block-or-clause" "def" "def-or-class" "class" "top-level"))
	(insert " (&optional decorators)")
      (insert " ()"))
    (insert (concat "
  \"When called interactively, mark " (capitalize ele) " at point.

When called from a programm, return source-code of " (capitalize ele) " at point, a string."))
        (if (member ele (list "block" "block-or-clause" "def" "def-or-class" "class" "top-level"))
	    (insert "\n\nOptional arg DECORATORS: include decorators when called at def or class.
Also honors setting of ‘py-mark-decorators’\"")
	  (insert "\""))
	(insert (concat "
  (interactive)
  (if (called-interactively-p 'interactive)
      (py--mark-base \"" ele "\""))
	(when (member ele (list "block" "block-or-clause" "def" "def-or-class" "class" "top-level"))
	  (insert " (or decorators py-mark-decorators)"))
	(insert (concat  ")
    (py--thing-at-point \""ele"\""))
	(when (member ele (list "block" "block-or-clause" "def" "def-or-class" "class" "top-level"))
	  (insert " (or decorators py-mark-decorators)"))
     (insert ")))\n"))
  (insert "\n;; python-components-forms-code.el ends here
\(provide 'python-components-forms-code)")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  (write-file (concat components-directory "/python-components-forms-code.el")))

(defun py-write-hide-forms ()
  (interactive "*")
  (set-buffer (get-buffer-create "python-components-hide-show.el"))
  (erase-buffer)
  (insert ";;; python-components-hide-show.el --- Provide hs-minor-mode forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (insert"
\;; (setq hs-block-start-regexp 'py-extended-block-or-clause-re)
\;; (setq hs-forward-sexp-func 'py-forward-block)

\(defun py-hide-base (form &optional beg end)
  \"Hide visibility of existing form at point.\"
  (hs-minor-mode 1)
  (save-excursion
    (let\* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat \"py--beginning-of-\" form \"-p\")))
                            (funcall (intern-soft (concat \"py-backward-\" form))))))
           (end (or end (funcall (intern-soft (concat \"py-forward-\" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (progn
            (hs-make-overlay beg end 'code)
            (set-buffer-modified-p modified))
        (error (concat \"No \" (format \"%s\" form) \" at point\"))))))

\(defun py-hide-show (&optional form beg end)
  \"Toggle visibility of existing forms at point.\"
  (interactive)
  (save-excursion
    (let\* ((form (prin1-to-string form))
           (beg (or beg (or (funcall (intern-soft (concat \"py--beginning-of-\" form \"-p\")))
                            (funcall (intern-soft (concat \"py-backward-\" form))))))
           (end (or end (funcall (intern-soft (concat \"py-forward-\" form)))))
           (modified (buffer-modified-p))
           (inhibit-read-only t))
      (if (and beg end)
          (if (overlays-in beg end)
              (hs-discard-overlays beg end)
            (hs-make-overlay beg end 'code))
        (error (concat \"No \" (format \"%s\" form) \" at point\")))
      (set-buffer-modified-p modified))))

\(defun py-show ()
  \"Remove invisibility of existing form at point\.\"
  (interactive)
  (with-silent-modifications
    (save-excursion
      (back-to-indentation)
      (let ((end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)))))

\(defun py-show-all ()
  \"Remove invisibility of hidden forms in buffer\.\"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (end)
      (while (and (not (eobp))  (setq end (next-overlay-change (point))))
	(hs-discard-overlays (point) end)
	(goto-char end)))))

\(defun py-hide-region (beg end)
  \"Hide active region.\"
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-hide-base 'region beg end))

\(defun py-show-region (beg end)
  \"Un-hide active region.\"
  (interactive
   (list
    (and (use-region-p) (region-beginning))(and (use-region-p) (region-end))))
  (py-show-base 'region beg end))
")
  (dolist (ele py-hide-forms)
    (insert (concat "
\(defun py-hide-" ele " ()
  \"Hide " ele " at point.\"
  (interactive)
  (py-hide-base '" ele "))
")))

;; \(defun py-show-" ele " ()
;;   \"Show " ele " at point.\"
;;   (interactive)
;;   (py-show-base '" ele "))

  (insert "\n;; python-components-hide-show.el ends here
\(provide 'python-components-hide-show)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
    (write-file (concat components-directory "/python-components-hide-show.el")))

(defun write-py-ert-always-split-lp-1361531-tests (&optional pyshellname-list)
  (interactive)
  (let* ((liste py-shells))
    (set-buffer (get-buffer-create "py-ert-always-split-lp-1361531-tests.el"))
    (erase-buffer)
    (insert ";;; py-ert-always-split-lp-1361531-tests.el --- Test splitting -*- lexical-binding: t; -*-\n")
    (insert arkopf)
    (when py--debug-p (switch-to-buffer (current-buffer)))
    (dolist (ele liste)
      (setq elt (prin1-to-string ele))
    (insert (concat "
\(ert-deftest py-ert-always-split-dedicated-lp-1361531-" elt "-test ()
  (py-test-with-temp-buffer
      \"#! /usr/bin/env " elt "
# -*- coding: utf-8 -*-
print(\\\"I'm the py-always-split-dedicated-lp-1361531-" elt "-test\\\")\""))
    (when py--debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    (insert (concat "
    (delete-other-windows)
    (let* ((py-split-window-on-execute 'always)
           (erg1 (progn (py-execute-statement-" elt "-dedicated) py-buffer-name))
           (erg2 (progn (py-execute-statement-" elt "-dedicated) py-buffer-name)))
      (sit-for 1 t)
      (when py--debug-p (message \"(count-windows) %s\" (count-windows)))
      (should (< 2 (count-windows)))
      (py-kill-buffer-unconditional erg1)
      (py-kill-buffer-unconditional erg2)
      (py-restore-window-configuration))))
"))))
    (insert "\n;; py-ert-always-split-lp-1361531-tests.el ends here
\(provide 'py-ert-always-split-lp-1361531-tests)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-py-ert-just-two-split-lp-1361531-tests (&optional pyshellname-list)
  (interactive)
  (let* ((liste py-shells))
    (set-buffer (get-buffer-create "py-ert-just-two-split-lp-1361531-tests.el"))
    (erase-buffer)
    (insert ";;; py-ert-just-two-split-lp-1361531-tests.el --- Test splitting -*- lexical-binding: t; -*-\n")
    (insert arkopf)
    (when py--debug-p (switch-to-buffer (current-buffer)))
    (dolist (ele liste)
      (setq elt (prin1-to-string ele))
    (insert (concat "
\(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-" elt "-test ()
  (py-test-with-temp-buffer
      \"#! /usr/bin/env " elt "
# -*- coding: utf-8 -*-
print(\\\"I'm the py-just-two-split-dedicated-lp-1361531-" elt "-test\\\")\""))
    (when py--debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
    (insert (concat "
    (delete-other-windows)
    (let* ((py-split-window-on-execute 'just-two)
           (erg1 (progn (py-execute-statement-" elt "-dedicated) py-buffer-name))
           (erg2 (progn (py-execute-statement-" elt "-dedicated) py-buffer-name)))
      (sit-for 1 t)
      (when py--debug-p (message \"(count-windows) %s\" (count-windows)))
      (should (eq 2 (count-windows)))
      (py-kill-buffer-unconditional erg1)
      (py-kill-buffer-unconditional erg2)
      (py-restore-window-configuration))))
"))))
    (insert "\n;; py-ert-just-two-split-lp-1361531-tests.el ends here
\(provide 'py-ert-just-two-split-lp-1361531-tests)")
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-ert-beginning-tests ()
  (interactive)
  (set-buffer (get-buffer-create "py-ert-beginning-tests.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; py-ert-beginning-tests.el --- Just some more tests \n -*- lexical-binding: t; -*-")
  (insert arkopf)
  (dolist (ele py-navigate-forms)
    (insert (concat "
\(ert-deftest py-ert-beginning-of-" ele "-test ()
  (py-test-with-temp-buffer
      \"
# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in range(anzahl):
                    pass
        except:
            block2
\"
    (forward-line -3)
    (when py--debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (py-beginning-of-" ele ")
    (should (eq (char-after) "))
    (cond ((string= "block" ele)
           (insert "?f"))
          ((string= "clause" ele)
           (insert "?f"))
	  ((string= "for-block" ele)
           (insert "?f"))
	  ((string= "block-or-clause" ele)
           (insert "?f"))
          ((string= "def" ele)
           (insert "?d"))
          ((string= "class" ele)
           (insert "?c"))
          ((string= "def-or-class" ele)
           (insert "?d"))
          ((string= "if-block" ele)
           (insert "?i"))
          ((string= "try-block" ele)
           (insert "?t"))
          ((string= "minor-block" ele)
           (insert "?f"))
          ((string= "top-level" ele)
           (insert "?c"))
          ((string= "statement" ele)
           (insert "?f"))
          ((string= "expression" ele)
           (insert "?r"))
          ((string= "partial-expression" ele)
           (insert "?r")))
    (insert "))))\n"))

  (dolist (ele py-bol-forms)
    (insert (concat "
\(ert-deftest py-ert-beginning-of-" ele "-bol-test ()
  (py-test-with-temp-buffer
      \"
\# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in range(anzahl):
                    pass
        except:
            block2
\"
    (when py--debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (forward-line -3)
    (py-beginning-of-" ele "-bol)
    (should (eq (char-after) "))

    (cond ((string-match "block" ele)
           (insert "?\\ "))
          ((string-match "clause" ele)
           (insert "?\\ "))
	  ((string-match "for-block" ele)
           (insert "?\\ "))
          ((string-match "block-or-clause" ele)
           (insert "?\\ "))
          ((string-match "def" ele)
           (insert "?\\ "))
          ((string-match "class" ele)
           (insert "?c"))
          ((string-match "def-or-class" ele)
           (insert "?\\ "))
          ((string-match "if-block" ele)
           (insert "?\\ "))
          ((string-match "try-block" ele)
           (insert "?\\ "))
          ((string-match "minor-block" ele)
           (insert "?\\ "))
	  ((string= "statement" ele)
           (insert "?\\ ")))

    (insert "))))\n\n"))

  (insert "(provide 'py-ert-beginning-tests)
;;; py-ert-beginning-tests.el ends here")

  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode)
  (write-file (concat components-directory "/test/py-ert-beginning-tests.el")))

(defun py-write-beginning-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-beginning-position-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-beginning-position-forms.el --- -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (dolist (ele py-position-forms)
    (insert (concat "
\(defun py--beginning-of-" ele "-position ()
  \"Return beginning of " ele " position.\"
  (save-excursion
    (or (py--beginning-of-" ele "-p)
        (py-backward-" ele "))))\n")))

  (dolist (ele py-beginning-bol-command-names)
    (insert (concat "
\(defun py--beginning-of-" ele "-position-bol ()
  \"Return beginning of " ele " position at ‘beginning-of-line’.\"
  (save-excursion
    (or (py--beginning-of-" ele "-bol-p)
        (py-backward-" ele "-bol))))
")))
  (insert "\n(provide 'python-components-beginning-position-forms)
;;; python-components-beginning-position-forms.el ends here")

  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-beginning-position-forms.el")))

(defun py-write-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-end-position-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-end-position-forms.el --- -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (dolist (ele py-position-forms)
    (insert (concat "
\(defun py--end-of-" ele "-position ()
  \"Return end of " ele " position.\"
  (save-excursion (py-forward-" ele ")))\n")))

  (dolist (ele py-shift-bol-forms)
    (insert (concat "
\(defun py--end-of-" ele "-position-bol ()
  \"Return end of " ele " position at ‘beginning-of-line’.\"
  (save-excursion (py-forward-" ele "-bol)))\n")))

 (insert "\n(provide 'python-components-end-position-forms)
;;; python-components-end-position-forms.el ends here")

  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer)))
  (emacs-lisp-mode)
  (write-file (concat components-directory "/python-components-end-position-forms.el")))

(defun py-write-forward-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-forward-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))

  (insert ";;; python-components-forward-forms.el -- Go to the end of forms -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (goto-char (point-max))
  (insert "(defun py-forward-region ()
  \"Go to the end of current region.\"
  (interactive)
  (let ((end (region-end)))
    (when end (goto-char end))))
")
  (dolist (ele py-block-forms)
    (when (or (string-match "def" ele) (string-match "class" ele))
      (insert "\n;;;###autoload"))
    ;; beg-end check forms
    (insert (concat "
\(defun py-forward-" ele " (&optional orig bol)
  \"Go to end of " ele ".

Return end of ‘" ele "’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position\"
  (interactive)
  (cdr-safe (py--end-base 'py-" (ar-block-regexp-name-richten ele)
			       "-re orig bol)))

\(defun py-forward-" ele "-bol ()
  \"Goto beginning of line following end of ‘" ele "’.

Return position reached, if successful, nil otherwise.
See also ‘py-down-" ele "’: down from current definition to next beginning of ‘" ele "’ below.\"
  (interactive)
  (py-forward-" ele " nil t))\n")))

  (insert "\n;; python-components-forward-forms.el ends here
\(provide 'python-components-forward-forms)")
  (write-file (concat components-directory "/python-components-forward-forms.el"))
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode)))

(defun py--beginning-of-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-block-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-block-or-clause-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘block-or-clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-block-or-clause-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-class-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-class-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-clause-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘clause’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-clause-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-def-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-def-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-def-or-class-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘def-or-class’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-def-or-class-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-elif-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘elif-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-elif-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-else-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘else-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-else-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-except-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘except-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-except-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-for-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘for-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-for-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-if-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘if-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-if-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-indent-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘indent’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-indent-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-minor-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘minor-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-minor-block-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-statement-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-statement-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py--beginning-of-try-block-bol-p (&optional pps)
  "Return position, if cursor is at the beginning of a ‘try-block’, nil otherwise."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
       (looking-at py-try-re)
       (looking-back "[^ \t]*" (line-beginning-position))
       (not (nth 8 (parse-partial-sexp (point-min) (point))))
       (point))))

(defun py-write-booleans-beginning-forms ()
  "Uses ‘py-booleans-beginning-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-booleans-beginning-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-booleans-beginning-forms.el --- booleans-beginning forms -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (switch-to-buffer (current-buffer))
  (goto-char (point-max))
  (dolist (ele py-non-bol-forms)
    ;; expression needs "\b"
    (unless (equal ele "expression")
      (insert (concat "\(defun py--beginning-of-" ele "-p (&optional pps)
  \"Return position, if cursor is at the beginning of a ‘" ele "’, nil otherwise.\"\n"))
      (insert (concat "  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-" (ar-block-regexp-name-richten ele) "-re)
         (point))))\n\n"))))
  (dolist (ele py-bol-forms)
    (insert (concat "\(defun py--beginning-of-" ele "-p (&optional pps)
  \"Return position, if cursor is at the beginning of a ‘" ele "’, nil otherwise.\""))
    (insert (concat "
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-" (ar-block-regexp-name-richten ele) "-re)
         (looking-back \"[^ \\t]*\" (line-beginning-position))
         (eq (current-column)(current-indentation))
         (point))))\n\n")))
  (dolist (ele py-bol-forms)
    (insert (concat "\(defun py--beginning-of-" ele "-bol-p (&optional pps)
  \"Return position, if cursor is at the beginning of a ‘" ele "’, nil otherwise.\""))
    (insert (concat "
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (and (bolp)
         (not (or (nth 8 pps)(nth 1 pps)))
         (looking-at py-" (ar-block-regexp-name-richten ele) "-re)
         (looking-back \"[^ \\t]*\" (line-beginning-position))
         (point))))\n\n")))
  (insert "(provide 'python-components-booleans-beginning-forms)
;; python-components-booleans-beginning-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-booleans-beginning-forms.el")))

(defun py-write-booleans-end-forms (&optional filename dirname)
  "Write elisp code defining end-of-syntactic element predicate functions.

The source code is stored in a Emacs Lisp source file with the name
specified by FILENAME if specified. Otherwise it is stored inside
the default name: python-components-booleans-end-forms.el
The FILENAME must include the .el extension.

The file is stored inside the directory identified by DIRNAME if specified.
If nil, the default, specified by the global `components-directory' is used.

The generated source code defines functions that return the position
of point when it is located at the end of a specific Python syntactic element,
or return nil otherwise.

The generated functions with names that look like:
- py--end-of-XX-p
- py--end-of-XX-bol-p

where 'XX' is the Python syntactic element."
  (interactive)
  (let* ((filename (or filename "python-components-booleans-end-forms.el"))
         (dirname  (or dirname  components-directory))
         (file-path (expand-file-name filename dirname))
         (feature-name (file-name-sans-extension filename)))
    (set-buffer (get-buffer-create filename))
    (erase-buffer)
    (insert (format "\
;;; %s --- booleans-end forms -*- lexical-binding: t; -*-\n"
                    filename))
    (insert arkopf)
    ;;
    (dolist (ele py-non-bol-forms)
      (insert (concat "
\(defun py--end-of-" ele "-p ()
  \"Return position, if cursor is at the end of a " ele ", nil otherwise.\"
  (let ((orig (point)))
    (save-excursion
      (py-backward-" ele ")
      (py-forward-" ele ")
      (when (eq orig (point))
        orig))))\n")))
    ;;
    (dolist (ele py-bol-forms)
      (insert (concat "
\(defun py--end-of-" ele "-bol-p ()
  \"Return position, if cursor is at ‘beginning-of-line’ at the end of a " ele ", nil otherwise.\"
  (let ((orig (point)))
    (save-excursion
      (py-backward-" ele "-bol)
      (py-forward-" ele "-bol)
      (when (eq orig (point))
        orig))))\n")))
    ;;
    (dolist (ele py-bol-forms)
      (insert (concat "
\(defun py--end-of-" ele "-p ()
  \"Return position, if cursor is at the end of a " ele ", nil otherwise.\"
  (let ((orig (point)))
    (save-excursion
      (py-backward-" ele ")
      (py-forward-" ele ")
      (when (eq orig (point))
        orig))))\n")))
    ;;
    (insert (format "\n(provide '%s)
;; python-components-booleans-end-forms.el ends here\n"
                    feature-name))
    (when (called-interactively-p 'interactive)
      (switch-to-buffer (current-buffer))
      (emacs-lisp-mode))
    (write-file file-path)))

(defun py-write-kill-forms (&optional forms)
  "Useseb ‘py-kill-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-kill-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-kill-forms.el --- kill forms -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (let ((forms (or forms py-non-bol-forms)))
    (dolist (ele forms)
      (insert (concat "
\(defun py-kill-"ele" ()
  \"Delete " ele " at point.

Stores data in kill ring\"
  (interactive \"*\")
  (let ((erg (py--mark-base \"" ele "\")))
    (kill-region (car erg) (cdr erg))))\n")))
    ;; expression-forms don't make sense WRT bol
    (dolist (ele py-bol-forms)
      (insert (concat "
\(defun py-kill-" ele " ()
  \"Delete " ele " at point.

Stores data in kill ring. Might be yanked back using ‘C-y’.\"
  (interactive \"\*\")
  (let ((erg (py--mark-base-bol \"" ele "\")))
    (kill-region (car erg) (cdr erg))))
"))))
  (insert "\n(provide 'python-components-kill-forms)
;;; python-components-kill-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-kill-forms.el")))

(defun py-write-close-forms (&optional forms)
  "Useseb ‘py-close-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-close-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-close-forms.el --- close forms -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
  (let ((forms (or forms py-down-forms)))
    (dolist (ele forms)
      (insert (concat "
\(defun py-close-"ele" ()
  \"Close " ele " at point.

Set indent level to that of beginning of function definition.

If final line isn't empty and ‘py-close-block-provides-newline’ non-nil, insert a newline.\"
  (interactive \"*\")
  (let ((erg (py--close-intern 'py-" ele "-re)))
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))
"))))
  (insert "\n(provide 'python-components-close-forms)
;;; python-components-close-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-close-forms.el")))

(defun py-write-mark-forms ()
  "Uses ‘py-mark-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-mark-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-mark-forms.el --- mark forms -*- lexical-binding: t; -*-\n")
  (insert "\n;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.\n")
  (insert arkopf)
    (dolist (ele py-non-bol-forms)
      (if (string-match "def\\|class" ele)
	  (insert (concat "
\(defun py-mark-" ele " (&optional arg)"))
	(insert (concat "
\(defun py-mark-" ele " ()")))
      (insert (concat "
  \"Mark " ele " at point.\n\n"))
      (when (string-match "def\\|class" ele)
	(insert "With ARG \\\\[universal-argument] or ‘py-mark-decorators’ set to t, decorators are marked too.
"))

      (insert (concat "Return beginning and end positions of marked area, a cons.\""))
      (if (string-match "def\\|class" ele)
	  (insert "\n  (interactive \"P\")")
	(insert "\n  (interactive)"))
      (if (string-match "def\\|class" ele)
	  (insert (concat "\n  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base \"" ele "\" py-mark-decorators)"))
	(insert "\n  (let (erg)
    (setq erg (py--mark-base \"" ele "\"))"))
      (insert "
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))")
      (newline))
    (dolist (ele py-bol-forms)
      (unless (string= ele "section")
    ;; Mark bol-forms
      (if (string-match "def\\|class" ele)
	  (insert (concat "
\(defun py-mark-" ele " (&optional arg)"))
	(insert (concat "
\(defun py-mark-" ele " ()")))
      (insert (concat "
  \"Mark " ele ", take beginning of line positions. \n\n"))
      (when (string-match "def\\|class" ele)
	(insert "With ARG \\\\[universal-argument] or ‘py-mark-decorators’ set to t, decorators are marked too.
"))

      (insert (concat "Return beginning and end positions of region, a cons.\""))
      (if (string-match "def\\|class" ele)
	  (insert "\n  (interactive \"P\")")
	(insert "\n  (interactive)"))
      (if (string-match "def\\|class" ele)
	  (insert (concat "\n  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py--mark-base-bol \"" ele "\" py-mark-decorators)"))
	(insert "\n  (let (erg)
    (setq erg (py--mark-base-bol \"" ele "\"))"))
      (insert "
    (exchange-point-and-mark)
    (when (and py-verbose-p (called-interactively-p 'interactive)) (message \"%s\" erg))
    erg))\n")))

  (insert "\n(provide 'python-components-mark-forms)
;;; python-components-mark-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-mark-forms.el")))

(defun py-write-copy-forms (&optional forms)
  "Uses ‘py-execute-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-copy-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-copy-forms.el --- copy forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (let ((forms (or forms py-execute-forms)))
    (dolist (ele forms)
      (insert (concat "
\(defun py-copy-" ele " ()
  \"Copy " ele " at point.

Store data in kill ring, so it might yanked back.\"
  (interactive \"\*\")
  (save-excursion
    (let ((erg (py--mark-base-bol \"" ele "\")))
      (copy-region-as-kill (car erg) (cdr erg)))))\n")))

    (dolist (ele forms)
      (unless (string= "section" ele)
	(insert (concat "
\(defun py-copy-" ele "-bol ()
  \"Delete " ele " bol at point.

Stores data in kill ring. Might be yanked back using ‘C-y’.\"
  (interactive \"\*\")
  (save-excursion
    (let ((erg (py--mark-base-bol \"" ele "\")))
      (copy-region-as-kill (car erg) (cdr erg)))))
")))))
  (insert "\n(provide 'python-components-copy-forms)
;; python-components-copy-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-copy-forms.el")))

(defun py--write-delete-forms (forms)
  (dolist (ele forms)
    (insert (concat "
\(defun py-delete-" ele " ()"))
    (insert (concat "
  \"Delete " (upcase ele) " at point.

\Don't store data in kill ring."))
    (insert "\"")

    (if (string-match "def\\|class" ele)
	(insert "\n  (interactive \"P\")")
      (insert "\n  (interactive)"))
    (if (string-match "def\\|class" ele)
	(insert (concat "\n (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base \"" ele "\" py-mark-decorators)))"))
      (insert (concat
	       "\n  (let ((erg (py--mark-base \"" ele "\")))")))
    (insert "
    (delete-region (car erg) (cdr erg))))\n")))

(defun py--write-delete-forms-bol (forms)
  (dolist (ele forms)
    (if (string-match "def\\|class" ele)
	(insert (concat "
\(defun py-delete-" ele " (&optional arg)"))
      (insert (concat "
\(defun py-delete-" ele " ()")))
    (insert (concat "
  \"Delete " (upcase ele) " at point until ‘beginning-of-line’.

\Don't store data in kill ring."))
    (if (string-match "def\\|class" ele)
	(insert "\nWith ARG \\\\[universal-argument] or ‘py-mark-decorators’ set to t, ‘decorators’ are included.\"")
      (insert "\""))
    (if (string-match "def\\|class" ele)
	(insert "\n  (interactive \"P\")")
      (insert "\n  (interactive)"))
    (if (string-match "def\\|class" ele)
	(insert (concat "\n (let* ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py--mark-base \"" ele "\" py-mark-decorators)))"))
      (insert (concat
	       "\n  (let ((erg (py--mark-base-bol \"" ele "\")))")))
    (insert "
    (delete-region (car erg) (cdr erg))))\n")))

(defun py-write-delete-forms ()
  "Uses ‘py-execute-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-delete-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-delete-forms.el --- delete forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (py--write-delete-forms-bol py-bol-forms)
  (py--write-delete-forms py-non-bol-forms)
  (insert "\n(provide 'python-components-delete-forms)
;; python-components-delete-forms.el ends here\n")
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write-file (concat components-directory "/python-components-delete-forms.el")))

(defun write--section-forms ()
  (dolist (ele py-shells)
    (setq ele (format "%s" ele))
    (insert "(defun py-execute-section")
    (unless (string= "" ele) (insert (concat "-" ele)))
    (insert " ()
  \"Execute section at point")
    (unless (string= "" ele) (insert (concat " using " ele " interpreter")))
    (insert ".\"
  (interactive)
  (py-execute-section-prepare")
    (unless (string= "" ele) (insert (concat " \"" ele "\"")))
    (insert "))\n\n")))

(defun py-write-section-forms ()
  "Uses ‘py-section-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-section-forms.el"))
  (erase-buffer)
  (insert ";;; python-components-section-forms.el --- section forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write--section-forms)
  (insert "\n(provide 'python-components-section-forms)
;;; python-components-section-forms.el ends here\n")
  (write-file (concat components-directory "/python-components-section-forms.el")))

;; py-comment-forms
(defun write--narrow-forms ()
  (dolist (ele py-comment-forms)
    ;; (setq ele (format "%s" ele))
    (insert "(defun py-narrow-to")
    (unless (string= "" ele) (insert (concat "-" ele)))
    (insert (concat " ()
  \"Narrow to " ele " at point"))
	    ;; (unless (string= "" ele) (insert (concat " using " ele " interpreter")))
    (insert ".\"
  (interactive)
  (py--narrow-prepare")
    (unless (string= "" ele) (insert (concat " \"" ele "\"")))
    (insert "))\n\n")))

(defun py-write-narrow-forms ()
  "Uses ‘py-narrow-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-narrow.el"))
  (erase-buffer)
  (insert ";;; python-components-narrow.el --- narrow forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write--narrow-forms)
  (insert "(provide 'python-components-narrow)
;;; python-components-narrow.el ends here\n")
  (write-file (concat components-directory "/python-components-narrow.el")))

;;  python-components-execute-region
(defun write--execute-region ()
  (dolist (ele py-shells)
    (setq ele (format "%s" ele))
    (dolist (pyo py-full-options)
      (insert "(defun py-execute-region")
      (unless (string= "" ele) (insert (concat "-" ele)))
      (unless (string= "" pyo) (insert (concat "-" pyo)))
      (insert (concat " (beg end &optional shell filename proc)
  \"Execute region " ele))
      (insert ".\"
  (interactive \"r\")\n")
      (unless (string= "" pyo)
	(insert "  (let (")
	(py--insert-split-switch-forms pyo)
	(insert ")\n  "))
      (insert "  (py--execute-base beg end ")
      (if (string= "" ele)
	  (insert "shell")
	(insert (concat "'" ele)))
      (insert " filename proc))")
      (unless (string= "" pyo)(insert ")"))
      (insert "\n\n")
      )))

(defun py-write-execute-region ()
  "Uses ‘py-execute-region-forms’."
  (interactive)
  (set-buffer (get-buffer-create "python-components-execute-region.el"))
  (switch-to-buffer (current-buffer))
  (erase-buffer)
  (insert ";;; python-components-execute-region.el --- execute-region forms -*- lexical-binding: t; -*-\n")
  (insert arkopf)
  (when (called-interactively-p 'interactive) (switch-to-buffer (current-buffer))
	(emacs-lisp-mode))
  (write--execute-region)
  (insert "(provide 'python-components-execute-region)
;;; python-components-execute-region.el ends here\n")
  (write-file (concat components-directory "/python-components-execute-region.el")))

(defun py--insert-split-switch-doku (pyo)
    (cond ((string= pyo "switch")
                 (insert "Switch to output buffer. Ignores ‘py-switch-buffers-on-execute-p’."))
                ((string= pyo "no-switch")
                 (insert "\n\nKeep current buffer. Ignores ‘py-switch-buffers-on-execute-p’ "))))

(defun py--insert-split-switch-forms (pyo)
  (cond ((string= pyo "switch")
	 ;; (insert (make-string 2 ?\ ))
	 (insert "(py-switch-buffers-on-execute-p t)"))
	((string= pyo "dedicated-switch")
	 ;; (insert (make-string 2 ?\ ))
	 (insert "(py-switch-buffers-on-execute-p t)\n")
	 (insert (make-string 8 ?\ ))
	 (insert "(py-split-window-on-execute t)"))
	((string= pyo "no-switch")
	 ;; (insert (make-string 2 ?\ ))
	 (insert "(py-switch-buffers-on-execute-p nil)"))
	((string= pyo "dedicated")
	 ;; (insert (make-string 2 ?\ ))
	 (insert "(py-dedicated-process-p t)"))))

(defun ar-block-regexp-name-richten (ele)
  (cond ((string-match "^if-" ele)
	 "if")
	((string-match "for-" ele)
	 "for")
	((string-match "clause-" ele)
	 "clause")
	((string-match "elif-" ele)
	 "elif")
	((string-match "try-" ele)
	 "try")
	((string-match "else-" ele)
	 "else")
	((string-match "except-" ele)
	 "except")
	(t ele)))

(defun write-most-of-forms ()
  "Let's see if we can write/update forms at once."
  (interactive)
  (py-provide-installed-shells-commands)
  (py-write-backward-forms)
  (py-write-beginning-position-forms)
  (py-write-end-position-forms)
  (py-write-booleans-beginning-forms)
  (py-write-booleans-end-forms)
  (py-write-copy-forms)
  (py-write-delete-forms)
  (py-write-forms-code)
  (py-write-forward-forms)
  (py-write-kill-forms)
  (py-write-close-forms)
  (py-write-mark-forms)
  (write-py-comment-forms)
  (py-write-up-down-forms)
  (py-write-execute-forms)
  (write-unified-extended-execute-forms)
  (py-write-hide-forms)
  ;; (py-write-execute-region)
  ;; (py-write-edit-forms)
  ;; (write-execute-region-forms)
  )
