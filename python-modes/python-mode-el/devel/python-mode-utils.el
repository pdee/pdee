;;; python-mode-shell-install.el --- Installing python, python3, ipython and other python shells

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

;; Python-components-mode started from python-mode.el
;; and python.el, where Tim Peters, Barry A. Warsaw,
;; Skip Montanaro, Ken Manheimer, Dave Love and many
;; others wrote major parts. Author of ipython.el's
;; stuff merged is Alexander Schmolck.

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

;;; Code:

(defvar arkopf)

(setq arkopf
      "\n;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Commentary: Edit `py-test-pyshellname-list' before
;; running this test-builder or give a list of shells as
;; arguments

;;; Code:

")

(setq py-options (list "" "switch" "noswitch" "dedicated" "dedicated-switch"))

(defcustom py-shells
  '("python" "ipython" "python3" "python2" "python2.7" "jython" "python3.2")
  "Python-mode will generate commands opening shells mentioned here. Edit this list \w resp. to your machine. "
  :type '(repeat string)
  :group 'python)

(setq py-test-shells
      (list "python" "ipython" "python3" "python2" "python2.7"))

(setq py-shift-forms (list "paragraph" "block" "clause" "block-or-clause" "def" "class" "def-or-class" "line" "statement" "expression" "partial-expression"))

(setq py-core-command-name '("statement" "block" "def" "class" "region" "file"))

;; (setq py-bounds-command-names '("statement" "block" "clause" "block-or-clause" "def" "class" "region" "buffer" "expression" "partial-expression" "line"))

(setq py-bounds-command-names '("clear-flymake-allowed-file-name-masks" "pylint-flymake-mode" "pyflakes-flymake-mode" "pychecker-flymake-mode" "pep8-flymake-mode" "pyflakespep8-flymake-mode" "py-pylint-doku" "py-pyflakes-run" "py-pyflakespep8-run" "py-pyflakespep8-help"))

(setq py-execute-forms-names '("statement" "block" "block-or-clause" "def" "class" "def-or-class" "expression" "partial-expression"))

(defvar py-re-forms-names '("block" "clause" "block-or-clause" "def" "class" "def-or-class" "if-block" "try-block" "minor-block")
  "Forms whose start is described by a regexp in python-mode." )

(defun write-execute-forms (&optional command)
  "Write `py-execute-block...' etc. "
  (interactive)
  (let ((py-bounds-command-names (if command (list command) py-execute-forms-names)))
    (set-buffer (get-buffer-create "python-components-exec-forms.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; python-components-exec-forms.el --- Execute forms at point\n")
    (insert arkopf)
    (insert ";;; Execute forms at point\n\n")
    (dolist (ele py-bounds-command-names)
      (insert (concat "(defun py-execute-" ele " (&optional shell dedicated switch)"))
      (insert (concat "
  \"Send " ele " at point to a Python interpreter.\n\n"))
      (insert "When called with \\\\[univeral-argument], execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with \\\\[univeral-argument] followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

When called from a programm, it accepts a string specifying a shell which will be forced upon execute as argument.

Optional arguments DEDICATED (boolean) and SWITCH (symbols 'noswitch/'switch)\"\n")
      (insert (concat "  (interactive \"P\")
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-" ele "-p)
                       (py-beginning-of-" ele "))))
          (end (py-end-of-" ele")))
      (py-execute-region beg end shell dedicated switch))))\n\n"))))
  (insert "(provide 'python-components-exec-forms)
;;; python-components-exec-forms.el ends here\n ")
  (emacs-lisp-mode))

(defun write-execute-forms-test (&optional command path-to-shell option)
  "Write `py-execute-block...' etc. "
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
    (insert ";;; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-executes-test.el"))
    (insert " --- executes test")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      ;; (dolist (elt py-shells)
      ;; (dolist (pyo py-options)
      (insert (concat "(defun py-execute-" ele "-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \""))
      (cond ((or (string-match "block" ele)(string-match "clause" ele))
             (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
            ((string-match "def" ele)
             (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
            ((string= "class" ele)
             (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
            (t (insert (concat "print(\\\"I'm the py-execute-" ele))))
      (insert "-test\\\")\"))")
      (insert (concat "
    (py-bug-tests-intern 'py-execute-" ele))
      (insert (concat "-base arg teststring)))\n"))
      (insert (concat "\n(defun py-execute-" ele))
      (insert "-base ()\n")
      (insert (concat "  (assert (markerp (py-execute-" ele))
      (cond ((string= "region" ele)
             (insert " (line-beginning-position) (line-end-position)"))
            ;; ((string= "buffer" ele)
            ;; (insert " (point-min)(point-max)"))
            )
      (insert "))")
      (insert (concat " nil \"py-execute-" ele))
      (insert "-test failed\"))\n\n")))
  (insert "\n\n(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here\n ")
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun write-extended-execute-forms (&optional path-to-shell command option)
  "Write `py-execute-block...' etc. "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-extended-executes.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; Extended executes")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-extended-executes.el"))
    (insert " --- more execute forms")
    (insert arkopf)

    (insert "
;;; Extended executes
;; created by `write-extended-execute-forms'
\(defun py-execute-prepare (form &optional shell dedicated switch)
  \"Used by python-extended-executes .\"
  (save-excursion
    (let ((beg (prog1
                   (or (funcall (intern-soft (concat \"py-beginning-of-\" form \"-p\")))

                       (funcall (intern-soft (concat \"py-beginning-of-\" form)))
                       (push-mark))))
          (end (funcall (intern-soft (concat \"py-end-of-\" form)))))
      (py-execute-base beg end shell dedicated switch))))\n\n")

    (dolist (ele py-bounds-command-names)
      (dolist (elt py-shells)
        (dolist (pyo py-options)
          (if (string= "" elt)
              (insert (concat "(defun py-execute-" ele))
            (insert (concat "(defun py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (if (string= "region" ele)
              (insert " (beg end)")
            (insert " ()"))
          (insert (concat "
  \"Send " ele " at point to "))
          (if (string= "ipython" elt)
              (insert "IPython")
            (insert (capitalize elt)))
          (cond ((string= pyo "dedicated")
                 (insert " unique interpreter. "))
                ((string= pyo "dedicated-switch")
                 (insert " unique interpreter and switch to result. "))
                ((string= "" elt)
                 (insert "default interpreter. "))
                (t (insert " interpreter. ")))
          (cond ((string= pyo "switch")
                 (insert "\n\nSwitch to output buffer. Ignores `py-shell-switch-buffers-on-execute-p'. "))
                ((string= pyo "noswitch")
                 (insert "\n\nKeep current buffer. Ignores `py-shell-switch-buffers-on-execute-p' ")))
          (insert "\"\n")
          (cond ((string= "region" ele)
                 (insert (concat "  (interactive \"r\")
  (py-execute-base beg end \"" elt "\"")))
                ((string= "buffer" ele)
                 (insert "  (interactive)
  \(save-excursion
    (let ((wholebuf t)
          (py-master-file (or py-master-file (py-fetch-py-master-file)))
          beg end)
      (when py-master-file
        (let* ((filename (expand-file-name py-master-file))
               (buffer (or (get-file-buffer filename)
                           (find-file-noselect filename))))
          (set-buffer buffer)))
      (setq beg (point-min))
      (setq end (point-max))
      (py-execute-region beg end \"" elt "\""))
                (t (insert (concat "  (interactive)
  (py-execute-prepare \"" ele "\" \"" elt "\""))))

          (if (string-match "dedicated" pyo)
              (insert " t")
            (insert " nil"))
          (cond ((or (string= "switch" pyo)
                     (string= "dedicated-switch" pyo))
                 (insert " 'switch"))
                ((string= "noswitch" pyo)
                 (insert " 'noswitch"))
                (t (insert " nil")))
          (if (string= "buffer" ele)
              (insert "))))\n\n")
            (insert "))\n\n"))))))
  (if path-to-shell
      (insert (concat "(provide '" path-to-shell) ")
;;; " path-to-shell ".el ends here\n")
    (insert "(provide 'python-extended-executes)
;;; python-extended-executes.el ends here\n "))
  (emacs-lisp-mode))

(defun write-extended-execute-forms-test (&optional command path-to-shell option)
  "Write `py-execute-block...' etc. "
  (interactive)
  ;; (load-shells)
  (let ((py-bounds-command-names (if command (list command) py-bounds-command-names))
        (py-test-shells (if path-to-shell (list path-to-shell) py-shells))
        (py-options (if option (list option) py-options)))
    (if path-to-shell
        (set-buffer (get-buffer-create (concat path-to-shell ".el")))
      (set-buffer (get-buffer-create "python-extended-executes-test.el")))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; ")
    (if path-to-shell
        (insert (concat path-to-shell ".el"))
      (insert "python-extended-executes-test.el"))
    (insert " --- extended-executes test")
    (insert arkopf)
    (dolist (ele py-bounds-command-names)
      (dolist (elt py-shells)
        (dolist (pyo py-options)
          (if (string= "" elt)
              (insert (concat "\n\n(defun py-execute-" ele))
            (insert (concat "\n\n(defun py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test")
          (insert " (&optional arg load-branch-function)")
          (insert (concat "
  (interactive \"p\")
  (let ((teststring \""))
          (cond ((or (string-match "block" ele)(string-match "clause" ele))
                 (insert (concat "if True: print(\\\"I'm the py-execute-" ele)))
                ((string-match "def" ele)
                 (insert (concat "def foo (): print(\\\"I'm the py-execute-" ele)))
                ((string= "class" ele)
                 (insert (concat "class foo (): print(\\\"I'm the py-execute-" ele)))
                (t (insert (concat "print(\\\"I'm the py-execute-" ele))))
          (unless (string= "" elt) (insert (concat "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test\\\")\"))")
          (if (string= "" elt)
              (insert (concat "
  (py-bug-tests-intern 'py-execute-" ele))
            (insert (concat "
  (py-bug-tests-intern 'py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert (concat "-base arg teststring)))\n"))
          (if (string= "" elt)
              (insert (concat "\n(defun py-execute-" ele))
            (insert (concat "\n\(defun py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-base ()\n")
          (if (string= "" elt)
              (insert (concat "  (assert (markerp (py-execute-" ele))
            (insert (concat "  (assert (markerp (py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))                  (cond ((string= "region" ele)
                                                                                     (insert " (line-beginning-position) (line-end-position)")))
          (insert "))")
          (if (string= "" elt)
              (insert (concat "
           nil \"py-execute-" ele))
            (insert (concat " nil \"py-execute-" ele "-" elt)))
          (unless (string= pyo "")(insert (concat "-" pyo)))
          (insert "-test failed\"))"))))
    (insert "\n\n(provide 'python-extended-executes-test)
;;; python-extended-executes-test.el ends here\n "))
  (emacs-lisp-mode))

(defun write-all-bounds-forms ()
  (interactive)
  (write-bounds-forms py-bounds-command-names))

(defun write-bounds-forms (&optional commands)
  "Write `py-bounds-of-block' etc. "
  (interactive)
  (set-buffer (get-buffer-create "Bounds-forms"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; Bounds\n")
  (dolist (ele (or commands py-bounds-command-names))
    (if (string= ele "region")
        (insert (concat "(defun py-bounds-of-" ele " ()
  \"Returns bounds of " ele " at point.

Returns a list, whose car is beg, cdr - end.\"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((beg (region-beginning))
            (end (region-end)))
        (if (and beg end)
            (when (interactive-p) (message \"%s\" (list beg end)))
          (list beg end))))))\n\n"))
      (insert (concat "(defun py-bounds-of-" ele " (&optional position)
  \"Returns bounds of " ele " at point.

\With optional POSITION, a number, report bounds of " ele " at POSITION.
\Returns a list, whose car is beg, cdr - end.\"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (when position (goto-char position))
      (let ((beg (py-beginning-of-" ele "-position))
            (end (py-end-of-" ele "-position)))
        (if (and beg end)
            (when (interactive-p) (message \"%s\" (list beg end)))
          (list beg end))))))\n\n
\n"))))
  (emacs-lisp-mode))

(defun write-all-py-menu ()
  (interactive)
  (write-py-menu py-bounds-command-names))

(defun write-py-menu (&optional commands)
  "Reads py-shells. "
  (interactive)
  (let ((menu-buffer "Python Executes Menu Buffer"))
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; Menu py-execute forms
    \(easy-menu-define py-menu map \"Execute Python\"
          `(\"PyExec\"
            :help \"Python-specific features\"\n")
    (dolist (ccc (or commands py-core-command-name))
      ;; ["Execute statement" py-execute-statement
      ;;              :help "`py-execute-statement'
      ;; Send statement at point to Python interpreter. "]
      (insert (concat "
            [\"Execute " ccc "\" py-execute-" ccc "
             :help \"`py-execute-" ccc "'
       Send " ccc " at point to Python interpreter. \"]\n")))
    (dolist (ccc (or commands py-core-command-name))
      (insert (concat "            ;; " ccc "\n
            (\"Execute " ccc " ... \"
            :help \"Execute " ccc " functions\"\n"))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "\" py-execute-" ccc "-" ele "
            :help \"Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
        With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))

      (insert "            ;; dedicated\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-dedicated\" py-execute-" ccc "-" ele "-dedicated
:help \"Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert " IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))

      (insert "            ;; switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "            \[\"py-execute-" ccc "-" ele "-switch\" py-execute-" ccc "-" ele "-switch
:help \"Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))
      (insert "            ;; dedicated-switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "            \[\"py-execute-" ccc "-" ele "-dedicated-switch\" py-execute-" ccc "-" ele "-dedicated-switch
:help \"Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))
      (insert ")"))
    (insert "))")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun write-py-menu-doppel ()
  "Reads py-shells. "
  (interactive)
  (let ((menu-buffer "*Python Executes Menu Buffer*"))
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert "(easy-menu-define py-menu map \"Execute Python\"
          `(\"PyExec\"
            :help \"Python-specific features\"\n")
    (dolist (ccc py-core-command-name)
      (insert (concat ";; " ccc "\n"))
      ;; ["Execute statement" py-execute-statement
      ;;              :help "`py-execute-statement'
      ;; Send statement at point to Python interpreter. "]
      (insert (concat "[\"Execute " ccc "\" py-execute-" ccc "
             :help \"`py-execute-" ccc "'
Send statement at point to Python interpreter. \"]\n
             (\"Execute " ccc " ... \"
             :help \"Execute " ccc " functions\"\n"))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "\" py-execute-" ccc "-" ele "\n
:help \"  Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))

      (insert ";; dedicated\n")
      (switch-to-buffer (current-buffer))
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-dedicated\" py-execute-" ccc "-" ele "-dedicated
:help \"  Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert " IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))

      (insert ";; switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-switch\" py-execute-" ccc "-" ele "-switch
:help \"  Execute " ccc " through a"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
        (if (string= "ipython" ele)
            (insert "IPython")
          (insert (capitalize ele)))
        (insert (concat " interpreter. \"]\n")))
      (insert ";; dedicated-switch\n")
      (dolist (ele py-shells)
        ;; ["if" py-if
        ;; :help "Inserts if-statement"]
        (insert (concat "\[\"py-execute-" ccc "-" ele "-dedicated-switch\" py-execute-" ccc "-" ele "-dedicated-switch
:help \"  Execute " ccc " through a unique"))
        (if (string= "ipython" ele)
            (insert "n IPython")
          (insert (concat " " (capitalize ele))))
        (insert (concat " interpreter.

Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))
      (insert ")"))
    (insert "))")

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-make-shell-menu ()
  "Reads py-shells, menu entries for these shells. "
  (interactive)
  (let ((temp-buffer "*Python Shell Menu Buffer*"))
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";; Python shell menu")
    (newline)
    (switch-to-buffer (current-buffer))
    (insert "(easy-menu-define py-menu map \"Python Shells\"
'(\"Py-Shell\"
  :help \"Python Shells\"\n
  \[\"Switch to interpreter\" py-shell
   :help \"Switch to `inferior' Python in separate buffer\"]\n")
    (let ((liste py-shells)
          erg)
      (while liste
        (setq ele (car liste))
        (setq erg (documentation (intern-soft (car liste))))
        (when (string-match "Optional DEDICATED SWITCH are provided for use from programs. " erg)
          (setq erg (replace-regexp-in-string "\n *Optional DEDICATED SWITCH are provided for use from programs. " "" erg)))
        ;; '("Python"
        ;;       :help "Python-specific features"
        ;;       ["Execute statement" py-execute-statement
        ;;        :help "Send statement at point to Python interpreter. "]
        (insert (concat " \[\"" ele "\" " ele "
  :help \"" erg "\"]\n"))
        (setq liste (cdr liste))))
    (insert "\"-\"")
    ;; dedicated
    (let ((liste py-shells)
          erg)
      (while liste
        (setq ele (concat (car liste) "-dedicated"))
        (setq erg (documentation (intern-soft ele)))
        ;; '("Python"
        ;;       :help "Python-specific features"
        ;;       ["Execute statement" py-execute-statement
        ;;        :help "Send statement at point to Python interpreter. "]
        (insert (concat " \[\"" ele "\" " ele "
  :help \"" erg "\"]\n"))
        (setq liste (cdr liste))))
    (insert "))")))

(defun py-provide-executes-with-resp-to-installed-python ()
  "Reads py-shells. "
  (interactive)
  (let ((temp-buffer "*Python Executes Install Buffer*")
        (menu-buffer "*Python Executes Menu Buffer*"))
    (set-buffer (get-buffer-create menu-buffer))
    (erase-buffer)
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";;; Python execute with")
    (newline)
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele " (&optional dedicated switch)
  \"Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py-execute-buffer-base \"" ele "\" dedicated switch)))\n\n"))
      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "\" py-execute-buffer-" ele "
:help \"  Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"]\n")))

    (set-buffer temp-buffer)
    (goto-char (point-max))
    (insert ";; dedicated\n")
    (switch-to-buffer (current-buffer))
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-dedicated (&optional switch)
  \"Execute buffer through an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py-execute-buffer-base \"" ele "\" t switch)))\n\n"))

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-dedicated\" py-execute-buffer-" ele "-dedicated
:help \"  Execute buffer through a unique"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))
    (set-buffer temp-buffer)
    (goto-char (point-max))

    (insert ";; switch\n")
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-switch (&optional dedicated)
  \"Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter and switch to output buffer.

Ignores `py-shell-switch-buffers-on-execute-p'.
Optional \\\\[universal-argument] makes"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert " run as an unique process. \"
  (interactive \"P\")
  (let ((wholebuf t))
  (py-execute-buffer-base \"" ele "\" dedicated 'switch)))\n\n")

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-switch\" py-execute-buffer-" ele "-switch
:help \"  Execute buffer through a"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

With \\\\[universal-argument] use an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"]\n")))

    (set-buffer temp-buffer)
    (goto-char (point-max))
    (insert ";; dedicated-switch\n")
    (dolist (ele py-shells)
      (set-buffer temp-buffer)
      (goto-char (point-max))
      (insert (concat "(defun py-execute-buffer-" ele "-dedicated-switch (&optional dedicated)
  \"Execute buffer through an unique"))
      (if (string= "ipython" ele)
          (insert " IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p'. \"
  (interactive)
  (let ((wholebuf t))
  (py-execute-buffer-base \"" ele "\" t 'switch)))\n\n"))

      (set-buffer menu-buffer)
      (goto-char (point-max))
      ;; ["if" py-if
      ;; :help "Inserts if-statement"]
      (insert (concat "\[\"py-execute-buffer-" ele "-dedicated-switch\" py-execute-buffer-" ele "-dedicated-switch
:help \"  Execute buffer through a unique"))
      (if (string= "ipython" ele)
          (insert "n IPython")
        (insert (concat " " (capitalize ele))))
      (insert (concat " interpreter.

Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p'. \"]\n")))
    (set-buffer temp-buffer)
    (goto-char (point-max))

    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-provide-installed-shells-commands ()
  "Reads py-shells, provides commands opening these shell. "
  (interactive)
  (let ((temp-buffer "Python-Shell-Install-Buffer"))
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer)
    (insert ";;; Python named shells")
    (newline)
    (dolist (ele py-shells)
      (insert (concat "(defun " ele " (&optional argprompt dedicated switch)
  \"Start an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter.

Optional \\\\[universal-argument] prompts for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. See `py-python-command-args'.
   Optional DEDICATED SWITCH are provided for use from programs. \"
  (interactive \"P\")
  (py-shell argprompt dedicated \"" ele "\" switch))\n\n")))
    (insert ";; dedicated\n")
    (dolist (ele py-shells)
      (insert (concat "(defun " ele "-dedicated (&optional argprompt switch)
  \"Start an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. See `py-python-command-args'.\"
  (interactive \"P\")
  (py-set-shell-completion-environment)
  (py-shell argprompt t \"" ele "\" switch))\n\n")))
    (insert ";; switch\n")
    (dolist (ele py-shells)
      (insert (concat "(defun " ele "-switch (&optional argprompt dedicated)
  \"Switch to "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. See `py-python-command-args'.\"
  (interactive \"P\")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated \"" ele "\" 'switch))\n\n")))
    ;; noswitch
    (dolist (ele py-shells)
      (insert (concat "(defun " ele "-no-switch (&optional argprompt dedicated)
  \"Open an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window, but do not switch to it.

Optional \\\\[universal-argument] prompts for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. See `py-python-command-args'.\"
  (interactive \"P\")
  (py-set-shell-completion-environment)
  (py-shell argprompt dedicated \"" ele "\" 'noswitch))\n\n")))
    ;; dedicated switch
    (dolist (ele py-shells)
      (insert (concat "(defalias '" ele "-dedicated-switch '" ele "-switch-dedicated)\n"))
      (insert (concat "(defun " ele "-switch-dedicated (&optional argprompt)
  \"Switch to an unique "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.

Optional \\\\[universal-argument] prompts for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. See `py-python-command-args'.\"
  (interactive \"P\")
  (py-set-shell-completion-environment)
  (py-shell argprompt t \"" ele "\" 'switch))\n\n"))))
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun py-write-re-beg-end-forms ()
  (interactive)
  (set-buffer (get-buffer-create "python-components-re-forms.el"))
  (erase-buffer)
  (switch-to-buffer (current-buffer))
  (insert ";;; python-components-re-forms.el --- Forms start described by a regular-expression \n")
  (insert arkopf)
  (insert ";;; Beg-end forms")
  (dolist (ele py-re-forms-names)
    (insert (concat "
\(defun py-beginning-of-" ele " (&optional indent)
  \"Returns beginning of " ele " if successful, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-" ele "-re indent)))))
    erg))

\(defun py-end-of-"ele" ()
  \"Go to the end of " ele".

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html\"
  (interactive)
  (let* ((orig (point))
        (erg (py-end-base py-" ele "-re orig)))
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))\n")))
  (insert "

;; Buffer
(defun py-beginning-of-buffer ()
  \"Go to beginning-of-buffer, return position. \"
  (let ((erg (unless (bobp)
               (goto-char (point-min)))))
    erg))

(defun py-end-of-buffer ()
  \"Go to end-of-buffer, return position.

  If already at end-of-buffer and not at EOB, go to end of next line. \"
  (let ((erg (unless (eobp)
               (goto-char (point-max)))))
    erg))

\(defalias 'py-forward-block 'py-end-of-block)
\(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
\(defalias 'py-forward-class 'py-end-of-class)
\(defalias 'py-forward-clause 'py-end-of-clause)
\(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
\(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
\
\(defalias 'py-previous-block 'py-beginning-of-block)
\(defalias 'py-goto-block-up 'py-beginning-of-block)
\(defalias 'py-backward-block 'py-beginning-of-block)
\(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
\(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
\(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
\(defalias 'beginning-of-class 'py-beginning-of-class)
\(defalias 'py-backward-class 'py-beginning-of-class)
\(defalias 'py-previous-class 'py-beginning-of-class)
\(defalias 'py-previous-clause 'py-beginning-of-clause)
\(defalias 'py-goto-clause-up 'py-beginning-of-clause)
\(defalias 'py-backward-clause 'py-beginning-of-clause)
\(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
\(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
")
    (insert "\n(provide 'python-components-re-forms)
;;; python-components-re-forms.el ends here\n ")
(emacs-lisp-mode))

(defun py-write-beg-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Positions"))
  (erase-buffer)
  (insert ";;; Positions")
  (dolist (ele py-shift-forms)
    (insert (concat "
\(defun py-beginning-of-" ele "-position ()
  \"Returns beginning of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))

\(defun py-end-of-" ele "-position ()
  \"Returns end of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))
"))
    (switch-to-buffer (current-buffer))
    (emacs-lisp-mode)))

(defun py-write-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "py-write-end-position-forms"))
  (erase-buffer)
  (dolist (ele py-shift-forms)
    (insert "
(defun py-end-of-" ele "-position ()
  \"Returns end of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))
")))

(defun py-write-shift-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-shift-forms"))
  (erase-buffer)
  (dolist (ele py-shift-forms)
    (insert (concat "
\(defun py-shift-" ele "-right (&optional arg)
  \"Indent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py-shift-forms-base \"" ele "\" (or arg py-indent-offset))))
        (when (interactive-p) (message \"%s\" erg))
    erg))

\(defun py-shift-" ele "-left (&optional arg)
  \"Dedent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py-shift-forms-base \"" ele "\" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(setq py-down-forms (list "block" "clause" "block-or-clause" "def" "class" "def-or-class"))

(defun py-write-down-forms-lc ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-down-forms-lc.el"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-down-" ele "-lc ()
  \"Goto beginning of line following end of " ele ".
  Returns position reached, if successful, nil otherwise.

\\\"-lc\\\" stands for \\\"left-corner\\\" - a complementary command travelling left, whilst `py-end-of-" ele "' stops at right corner.
See also `py-down-" ele "': down from current definition to next beginning of " ele " below. \"
  (interactive)
  (let ((erg (py-end-of-" ele ")))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-down-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-down-forms.el"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-down-" ele " ()
  \"Go to the beginning of next " ele " below in buffer.

Returns indentation if " ele " found, nil otherwise. \"
  (interactive)
  (let\* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(or (py-in-string-or-comment-p)(not (looking-at py-" ele "-re))))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-up-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-up-forms"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-up-" ele " ()
  \"Goto end of line preceding beginning of " ele ".
  Returns position reached, if successful, nil otherwise.

A complementary command travelling right, whilst `py-beginning-of-" ele "' stops at left corner. \"
  (interactive)
  (let ((erg (py-beginning-of-" ele ")))
    (when erg
      (unless (bobp)
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward \" \\t\\r\\n\\f\")
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-specifying-shell-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "specifying-shell-forms"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-execute-region-" ele " (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter. \"
  (interactive \"r\\nP\")
  (py-execute-base start end async \"" ele "\"))

\(defun py-execute-region-" ele "-switch (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will being switched to. \"
  (interactive \"r\\nP\")
  (let ((py-shell-switch-buffers-on-execute-p t))
    (py-execute-base start end async \"" ele "\")))

\(defun py-execute-region-" ele "-no-switch (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter.
  Ignores setting of `py-shell-switch-buffers-on-execute-p', output-buffer will not being switched to.\"
  (interactive \"r\\nP\")
  (let ((py-shell-switch-buffers-on-execute-p))
    (py-execute-base start end async \"" ele "\")))"))))

(defun xemacs-remove-help-strings ()
  "menu :help not supported presently at XEmacs. "
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
      (if (empty-line-p)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
        (push-mark)
        (setq erg (point))
        (skip-chars-backward " \t\r\n\f")
        (delete-region (point) erg))))
  (message "%s" "fertig"))

(setq py-noregexp-forms (list "paragraph" "line" "statement" "expression" "partial-expression"))

(setq py-regexp-forms (list "block" "clause" "block-or-clause" "def" "class" "def-or-class"))

(defun py-write-beginning-of-p-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Beginning-of"))
  (erase-buffer)
  (insert ";;; Beginning-of- p
\(defun py-beginning-of-line-p ()
  \"Returns position, if cursor is at the beginning of a line, nil otherwise. \"
  (when (bolp)(point)))

\(defun py-beginning-of-buffer-p ()
  \"Returns position, if cursor is at the beginning of buffer, nil otherwise. \"
  (when (bobp)(point)))\n")

  (dolist (ele py-noregexp-forms)
    (unless (string= "line" ele)
      (insert (concat "
\(defun py-beginning-of-" ele "-p ()
  \"Returns position, if cursor is at the beginning of a " ele ", nil otherwise. \"
  (let ((orig (point))
         erg)"))
      (when (string= "paragraph" ele)
        (insert "
     (if (and (bolp) (looking-at paragraph-separate))
         (setq erg (point))"))
      (insert (concat "
     (save-excursion
       (py-end-of-" ele ")
       (py-beginning-of-" ele ")
       (when (eq orig (point))
         (setq erg orig))"))
      (when (string= "paragraph" ele)
        (insert ")"))
      (insert (concat "
       erg)))
"))))
  (dolist (ele py-regexp-forms)
    (insert (concat "
\(defun py-beginning-of-" ele "-p ()
  \"Returns position, if cursor is at the beginning of a " ele ", nil otherwise. \"
    (when (and (looking-at py-" ele "-re)
               (not (py-in-string-or-comment-p)))
      (point)))
")))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun py-write-mark-forms ()
  (interactive)
  (set-buffer (get-buffer-create "Mark-forms"))
  (erase-buffer)
  (insert ";;; Mark forms
\(defun py-mark-base (form &optional py-mark-decorators)
  (let\* ((begform (intern-soft (concat \"py-beginning-of-\" form)))
         (endform (intern-soft (concat \"py-end-of-\" form)))
         (begcheckform (intern-soft (concat \"py-beginning-of-\" form \"-p\")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
          (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message \"%s %s\" beg end))
    (cons beg end)))\n")
  (dolist (ele py-shift-forms)
    (if (string-match "def\\|class" ele)
        (insert (concat "
\(defun py-mark-" ele " (&optional arg)"))
      (insert (concat "
\(defun py-mark-" ele " ()")))
    (insert (concat "
  \"Mark " ele " at point.\n\n"))
    (when (string-match "def\\|class" ele)
      (insert "With \\\\[universal argument] or `py-mark-decorators' set to `t', decorators are marked too.
"))

    (insert (concat "Returns beginning and end positions of marked area, a cons. \""))
    (if (string-match "def\\|class" ele)
        (insert "\n  (interactive \"P\")")
      (insert "\n  (interactive)"))
    (if (string-match "def\\|class" ele)
        (insert (concat "\n  (let ((py-mark-decorators (or arg py-mark-decorators))
        erg)
    (py-mark-base \"" ele "\" py-mark-decorators)"))
      (insert "\n  (let (erg)
    (setq erg (py-mark-base \"" ele "\"))"))
    (insert "
    (exchange-point-and-mark)
    (when (and py-verbose-p (interactive-p)) (message \"%s\" erg))
    erg))")
    (newline))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-toggle-forms ()
  "Write toggle-forms according to (car kill-ring) "
  (interactive)
  (let ((ele (car kill-ring)))
    (message "Writing for; %s" (car kill-ring))
    (set-buffer (get-buffer-create (capitalize ele)))
    (erase-buffer)
    (insert (concat ";;; " (capitalize ele) " forms"))
    (insert (concat "
\(defalias 'toggle-" ele " 'py-toggle-" ele ")
\(defun py-toggle-" ele " (&optional arg)
  \"If `" ele "' should be on or off.

  Returns value of `" ele "' switched to. \"
  (interactive)
  (let ((arg (or arg (if " ele " -1 1))))
    (if (< 0 arg)
        (setq " ele " t)
      (setq " ele " nil))
    (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
    " ele "))

\(defun " ele "-on (&optional arg)
  \"Make sure, `py-" ele "' is on.

Returns value of `" ele "'. \"
  (interactive \"p\")
  (let ((arg (or arg 1)))
    (toggle-" ele " arg))
  (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
  " ele ")

\(defun " ele "-off ()
  \"Make sure, `" ele "' is off.

Returns value of `" ele "'. \"
  (interactive \"p\")
  (toggle-" ele " -1)
  (when (or py-verbose-p (interactive-p)) (message \"" ele ": %s\" " ele "))
  " ele ")"))
    (newline))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-commandp-forms ()
  "Write forms according to `py-bounds-command-names' "
  (interactive)
  (let ((erg py-bounds-command-names))

    (set-buffer (get-buffer-create "Commandp tests"))
    (erase-buffer)
    (dolist (ele erg)
      (insert (concat "--funcall " ele "-commandp-test \\\n")))
    (insert ";;; Commandp tests")
    ;; (dolist (ele py-bounds-command-names)
    (dolist (ele erg)
      (insert (concat "
\(defun " ele "-commandp-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"\"))
  (py-bug-tests-intern '" ele "-commandp-base arg teststring)))

\(defun " ele "-commandp-base ()
    (assert (commandp '" ele ") nil \"" ele "-commandp-test failed\"))"))
      (newline)))
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defun write-invoke-py-shell-forms ()
  "Write forms according to `py-shells' "
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
  (insert ";;; Py-shell tests")
  ;; (dolist (ele py-bounds-command-names)
  (dolist (ele py-shells)
    (insert (concat "
\(defun py-shell-invoking-" ele "-lp:835151-test (&optional arg load-branch-function)
  (interactive \"p\")
  (let ((teststring \"print(\\\"py-shell-name: " ele "\\\")\"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shell-invoking-" ele "-lp:835151-base arg teststring)))

\(defun py-shell-invoking-" ele "-lp:835151-base ()
  (setq py-shell-name \"" ele "\")
  (assert (markerp (py-execute-buffer)) nil \"py-shell-invoking-" ele "-lp:835151-test failed\"))\n")))
  (newline)
  (switch-to-buffer (current-buffer))
  (emacs-lisp-mode))

(defalias 'fehler-python-tests 'lookup-failing-command)
(defun lookup-failing-command ()
  "From ./python-mode-tests.sh buffer, jump to definition of command.

Needs `elisp-find-definition' from
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
    (when (interactive-p) (message "%s" erg))
    erg))

(defun list-python-mode-test-forms ()
  (interactive)
  (let* ((oldbuf (current-buffer))
         (batchbuffer (concat (capitalize (buffer-name oldbuf)) "-test-batch-calls"))
         (symbolbuffer (concat (capitalize (buffer-name oldbuf)) "-test-symbols"))
         tests)
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (set-buffer (get-buffer-create batchbuffer))
    (erase-buffer)
    (set-buffer oldbuf)
    (goto-char (point-min))
    (while (and (not (eobp))(re-search-forward "^(defun [[:alpha:]]" nil t 1))
      (let* ((name  (prin1-to-string (symbol-at-point))))
        (unless (string-match "-base" name) (add-to-list 'tests name))
        (forward-line 1)))
    (setq tests (nreverse tests))
    (set-buffer batchbuffer)
    (dolist (ele tests)
      (insert (concat "'" ele "\n")))
    (set-buffer symbolbuffer)
    (dolist (ele tests)
      (insert (concat "--funcall " ele " \\\n")))))

(defun write-script-completion-tests (&optional pyshellname-list)
  (interactive)
  (let ((pyshellname-list (or pyshellname-list py-test-pyshellname-list))
        (sepchar (py-separator-char))
        (symbolbuffer "script-completion-test-symbols")
        (batchbuffer "script-completion-test-funcalls"))
    (set-buffer (get-buffer-create batchbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^[/~]+" "" ele)))
      (insert (concat "--funcall " ele "-complete-test \\\n")))
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^[/~]+" "" ele)))
      (insert (concat "'" ele "-complete-test\n")))
    (set-buffer (get-buffer-create "py-script-completion-tests.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; py-script-completion-tests.el --- Test completion for available Python shell\n")
    (insert arkopf)
    ;; (insert ";;; \n\n")
    (insert "(setq python-mode-script-complete-tests
        (list \n")
    (insert-buffer symbolbuffer)
    (goto-char (point-max))
    (insert "))\n
\(defun py-run-script-complete-tests ()
  (interactive)
  (dolist (ele python-mode-script-complete-tests)
    (funcall ele)))\n\n")

    (dolist (ele pyshellname-list)
      (setq elt ele)
      (when (string-match "^~" ele)
        (setq ele (replace-regexp-in-string "^~" (concat "/home/" user-login-name) ele)))
      (setq ele (replace-regexp-in-string "/+" "-" (replace-regexp-in-string "^/" "" ele)))
      (insert (concat "\(defun " ele "-complete-test (&optional arg)
  (interactive \"p\")
  (let ((teststring \""))
      (if (string-match sepchar elt)
          (progn
            (when (string-match "^~" elt)
              (setq elt (replace-regexp-in-string "^~" (concat "/home/" user-login-name) elt)))
            (insert (concat "#! " elt "\n")))
        (insert (concat "#! /usr/bin/env " elt "\n")))
      (insert (concat "pri\"))
    (py-bug-tests-intern '" ele "-complete-base arg teststring)))

\(defun " ele "-complete-base ()
  (save-excursion (completion-at-point))
  ;; (sit-for 0.1)
  (assert (looking-at \"print\") nil \"" ele "complete-test failed\"))\n\n"))))
  (insert "\n(provide 'py-script-completion-tests)
;;; py-script-completion-tests ends here\n ")

  (emacs-lisp-mode))

(defun write-shell-completion-tests (&optional pyshellname-list)
  (interactive)
  (let ((pyshellname-list (or pyshellname-list py-test-pyshellname-list))
        (sepchar (py-separator-char))
        (symbolbuffer "completion-test-symbols")
        (batchbuffer "completion-test-funcalls"))
    (set-buffer (get-buffer-create  batchbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "--funcall " ele "-shell-complete-test \\\n")))
    (set-buffer (get-buffer-create symbolbuffer))
    (erase-buffer)
    (dolist (ele pyshellname-list)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "'" ele "-shell-complete-test\n")))
    (set-buffer (get-buffer-create "py-shell-completion-tests.el"))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (insert ";;; py-shell-completion-tests.el --- Test completion for available Python shell\n")
    (insert arkopf)
    (insert "(setq python-mode-shell-complete-tests
        (list\n\n")
    (insert-buffer symbolbuffer)
    (goto-char (point-max))
    (insert "))\n\n")
    (forward-line -4)
    (richten)
    (goto-char (point-max))
    (insert "(defun py-run-shell-complete-tests ()
  (interactive)
  (dolist (ele python-mode-shell-complete-tests)
    (funcall ele)))\n\n")

    ;; (insert ";;; \n\n")
    (dolist (ele pyshellname-list)
      (setq elt ele)
      (setq ele (replace-regexp-in-string  "/+" "-" (replace-regexp-in-string  "^[/~]+" "" ele)))
      (insert (concat "\(defun " ele "-shell-complete-test ()
  (interactive)
  (let (py-shell-switch-buffers-on-execute-p
        py-split-windows-on-execute-p)
    (set-buffer (py-shell nil t \"" elt "\" nil \"" sepchar "\"))
    (sit-for 0.2 t)
    (goto-char (point-max))
    (insert \"pri\")
    (completion-at-point)
    (beginning-of-line)
    (assert (looking-at \"print\") nil \"" ele "-shell-complete-test failed\")
    (when py-verbose-p (message \"%s\" \"" ele "-shell-complete-test passed\"))))
\n\n")))
    (insert "\n(provide 'py-shell-completion-tests)
;;; py-shell-completion-tests ends here\n ")
    (emacs-lisp-mode)))
