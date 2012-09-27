;;; python-mode-test.el --- tests for Emacs python-mode.el

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: lisp, languages

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

;; A couple of test cases for python-mode.el

;;; Code:
(require 'py-bug-numbered-tests)

(setq python-mode-tests
      (list
       'py-guess-indent-offset-test
       'py-moves-test
       'augmented-assigment-test
       'py-smart-operator-test
       'key-binding-tests
       'py-end-of-statement-test
       'py-compute-indentation-test
       'py-end-of-def-inline-comment-test
       'before-inline-comment-test
       'toggle-force-py-shell-name-p-test
       'py-execute-statement-python-test
       'py-execute-statement-python-switch-test
       'py-execute-statement-python-noswitch-test
       'py-execute-statement-python-dedicated-test
       'py-execute-statement-python-dedicated-switch-test
       'py-execute-statement-ipython-test
       'py-execute-statement-ipython-switch-test
       'py-execute-statement-ipython-noswitch-test
       'py-execute-statement-ipython-dedicated-test
       'py-execute-statement-ipython-dedicated-switch-test
       'py-execute-statement-python3-test
       'py-execute-statement-python3-switch-test
       'py-execute-statement-python3-noswitch-test
       'py-execute-statement-python3-dedicated-test
       'py-execute-statement-python3-dedicated-switch-test
       'py-execute-statement-python2-test
       'py-execute-statement-python2-switch-test
       'py-execute-statement-python2-noswitch-test
       'py-execute-statement-python2-dedicated-test
       'py-execute-statement-python2-dedicated-switch-test
       'py-execute-statement-python2.7-test
       'py-execute-statement-python2.7-switch-test
       'py-execute-statement-python2.7-noswitch-test
       'py-execute-statement-python2.7-dedicated-test
       'py-execute-statement-python2.7-dedicated-switch-test
       'py-execute-statement-jython-test
       'py-execute-statement-jython-switch-test
       'py-execute-statement-jython-noswitch-test
       'py-execute-statement-jython-dedicated-test
       'py-execute-statement-jython-dedicated-switch-test
       'py-separator-char-test
       'py-execute-block-python-test
       'py-execute-block-python-switch-test
       'py-execute-block-python-noswitch-test
       'py-execute-block-python-dedicated-test
       'py-execute-block-python-dedicated-switch-test
       'py-execute-block-ipython-test
       'py-execute-block-ipython-switch-test
       'py-execute-block-ipython-noswitch-test
       'py-execute-block-ipython-dedicated-test
       'py-execute-block-ipython-dedicated-switch-test
       'py-execute-block-python3-test
       'py-execute-block-python3-switch-test
       'py-execute-block-python3-noswitch-test
       'py-execute-block-python3-dedicated-test
       'py-execute-block-python3-dedicated-switch-test
       'py-execute-block-python2-test
       'py-execute-block-python2-switch-test
       'py-execute-block-python2-noswitch-test
       'py-execute-block-python2-dedicated-test
       'py-execute-block-python2-dedicated-switch-test
       'py-execute-block-python2.7-test
       'py-execute-block-python2.7-switch-test
       'py-execute-block-python2.7-noswitch-test
       'py-execute-block-python2.7-dedicated-test
       'py-execute-block-python2.7-dedicated-switch-test
       'py-execute-block-jython-test
       'py-execute-block-jython-switch-test
       'py-execute-block-jython-noswitch-test
       'py-execute-block-jython-dedicated-test
       'py-execute-block-jython-dedicated-switch-test
       'py-execute-block-or-clause-python-test
       'py-execute-block-or-clause-python-switch-test
       'py-execute-block-or-clause-python-noswitch-test
       'py-execute-block-or-clause-python-dedicated-test
       'py-execute-block-or-clause-python-dedicated-switch-test
       'py-execute-block-or-clause-ipython-test
       'py-execute-block-or-clause-ipython-switch-test
       'py-execute-block-or-clause-ipython-noswitch-test
       'py-execute-block-or-clause-ipython-dedicated-test
       'py-execute-block-or-clause-ipython-dedicated-switch-test
       'py-execute-block-or-clause-python3-test
       'py-execute-block-or-clause-python3-switch-test
       'py-execute-block-or-clause-python3-noswitch-test
       'py-execute-block-or-clause-python3-dedicated-test
       'py-execute-block-or-clause-python3-dedicated-switch-test
       'py-execute-block-or-clause-python2-test
       'py-execute-block-or-clause-python2-switch-test
       'py-execute-block-or-clause-python2-noswitch-test
       'py-execute-block-or-clause-python2-dedicated-test
       'py-execute-block-or-clause-python2-dedicated-switch-test
       'py-execute-block-or-clause-python2.7-test
       'py-execute-block-or-clause-python2.7-switch-test
       'py-execute-block-or-clause-python2.7-noswitch-test
       'py-execute-block-or-clause-python2.7-dedicated-test
       'py-execute-block-or-clause-python2.7-dedicated-switch-test
       'py-execute-block-or-clause-jython-test
       'py-execute-block-or-clause-jython-switch-test
       'py-execute-block-or-clause-jython-noswitch-test
       'py-execute-block-or-clause-jython-dedicated-test
       'py-execute-block-or-clause-jython-dedicated-switch-test
       'py-execute-def-python-test
       'py-execute-def-python-switch-test
       'py-execute-def-python-noswitch-test
       'py-execute-def-python-dedicated-test
       'py-execute-def-python-dedicated-switch-test
       'py-execute-def-ipython-test
       'py-execute-def-ipython-switch-test
       'py-execute-def-ipython-noswitch-test
       'py-execute-def-ipython-dedicated-test
       'py-execute-def-ipython-dedicated-switch-test
       'py-execute-def-python3-test
       'py-execute-def-python3-switch-test
       'py-execute-def-python3-noswitch-test
       'py-execute-def-python3-dedicated-test
       'py-execute-def-python3-dedicated-switch-test
       'py-execute-def-python2-test
       'py-execute-def-python2-switch-test
       'py-execute-def-python2-noswitch-test
       'py-execute-def-python2-dedicated-test
       'py-execute-def-python2-dedicated-switch-test
       'py-execute-def-python2.7-test
       'py-execute-def-python2.7-switch-test
       'py-execute-def-python2.7-noswitch-test
       'py-execute-def-python2.7-dedicated-test
       'py-execute-def-python2.7-dedicated-switch-test
       'py-execute-def-jython-test
       'py-execute-def-jython-switch-test
       'py-execute-def-jython-noswitch-test
       'py-execute-def-jython-dedicated-test
       'py-execute-def-jython-dedicated-switch-test
       'py-execute-class-python-test
       'py-execute-class-python-switch-test
       'py-execute-class-python-noswitch-test
       'py-execute-class-python-dedicated-test
       'py-execute-class-python-dedicated-switch-test
       'py-execute-class-ipython-test
       'py-execute-class-ipython-switch-test
       'py-execute-class-ipython-noswitch-test
       'py-execute-class-ipython-dedicated-test
       'py-execute-class-ipython-dedicated-switch-test
       'py-execute-class-python3-test
       'py-execute-class-python3-switch-test
       'py-execute-class-python3-noswitch-test
       'py-execute-class-python3-dedicated-test
       'py-execute-class-python3-dedicated-switch-test
       'py-execute-class-python2-test
       'py-execute-class-python2-switch-test
       'py-execute-class-python2-noswitch-test
       'py-execute-class-python2-dedicated-test
       'py-execute-class-python2-dedicated-switch-test
       'py-execute-class-python2.7-test
       'py-execute-class-python2.7-switch-test
       'py-execute-class-python2.7-noswitch-test
       'py-execute-class-python2.7-dedicated-test
       'py-execute-class-python2.7-dedicated-switch-test
       'py-execute-class-jython-test
       'py-execute-class-jython-switch-test
       'py-execute-class-jython-noswitch-test
       'py-execute-class-jython-dedicated-test
       'py-execute-class-jython-dedicated-switch-test
       'py-execute-region-python-test
       'py-execute-region-python-switch-test
       'py-execute-region-python-noswitch-test
       'py-execute-region-python-dedicated-test
       'py-execute-region-python-dedicated-switch-test
       'py-execute-region-ipython-test
       'py-execute-region-ipython-switch-test
       'py-execute-region-ipython-noswitch-test
       'py-execute-region-ipython-dedicated-test
       'py-execute-region-ipython-dedicated-switch-test
       'py-execute-region-python3-test
       'py-execute-region-python3-switch-test
       'py-execute-region-python3-noswitch-test
       'py-execute-region-python3-dedicated-test
       'py-execute-region-python3-dedicated-switch-test
       'py-execute-region-python2-test
       'py-execute-region-python2-switch-test
       'py-execute-region-python2-noswitch-test
       'py-execute-region-python2-dedicated-test
       'py-execute-region-python2-dedicated-switch-test
       'py-execute-region-python2.7-test
       'py-execute-region-python2.7-switch-test
       'py-execute-region-python2.7-noswitch-test
       'py-execute-region-python2.7-dedicated-test
       'py-execute-region-python2.7-dedicated-switch-test
       'py-execute-region-jython-test
       'py-execute-region-jython-switch-test
       'py-execute-region-jython-noswitch-test
       'py-execute-region-jython-dedicated-test
       'py-execute-region-jython-dedicated-switch-test
       'py-execute-buffer-python-test
       'py-execute-buffer-python-switch-test
       'py-execute-buffer-python-noswitch-test
       'py-execute-buffer-python-dedicated-test
       'py-execute-buffer-python-dedicated-switch-test
       'py-execute-buffer-ipython-test
       'py-execute-buffer-ipython-switch-test
       'py-execute-buffer-ipython-noswitch-test
       'py-execute-buffer-ipython-dedicated-test
       'py-execute-buffer-ipython-dedicated-switch-test
       'py-execute-buffer-python3-test
       'py-execute-buffer-python3-switch-test
       'py-execute-buffer-python3-noswitch-test
       'py-execute-buffer-python3-dedicated-test
       'py-execute-buffer-python3-dedicated-switch-test
       'py-execute-buffer-python2-test
       'py-execute-buffer-python2-switch-test
       'py-execute-buffer-python2-noswitch-test
       'py-execute-buffer-python2-dedicated-test
       'py-execute-buffer-python2-dedicated-switch-test
       'py-execute-buffer-python2.7-test
       'py-execute-buffer-python2.7-switch-test
       'py-execute-buffer-python2.7-noswitch-test
       'py-execute-buffer-python2.7-dedicated-test
       'py-execute-buffer-python2.7-dedicated-switch-test
       'py-execute-buffer-jython-test
       'py-execute-buffer-jython-switch-test
       'py-execute-buffer-jython-noswitch-test
       'py-execute-buffer-jython-dedicated-test
       'py-execute-buffer-jython-dedicated-switch-test
       'py-execute-expression-python-test
       'py-execute-expression-python-switch-test
       'py-execute-expression-python-noswitch-test
       'py-execute-expression-python-dedicated-test
       'py-execute-expression-python-dedicated-switch-test
       'py-execute-expression-ipython-test
       'py-execute-expression-ipython-switch-test
       'py-execute-expression-ipython-noswitch-test
       'py-execute-expression-ipython-dedicated-test
       'py-execute-expression-ipython-dedicated-switch-test
       'py-execute-expression-python3-test
       'py-execute-expression-python3-switch-test
       'py-execute-expression-python3-noswitch-test
       'py-execute-expression-python3-dedicated-test
       'py-execute-expression-python3-dedicated-switch-test
       'py-execute-expression-python2-test
       'py-execute-expression-python2-switch-test
       'py-execute-expression-python2-noswitch-test
       'py-execute-expression-python2-dedicated-test
       'py-execute-expression-python2-dedicated-switch-test
       'py-execute-expression-python2.7-test
       'py-execute-expression-python2.7-switch-test
       'py-execute-expression-python2.7-noswitch-test
       'py-execute-expression-python2.7-dedicated-test
       'py-execute-expression-python2.7-dedicated-switch-test
       'py-execute-expression-jython-test
       'py-execute-expression-jython-switch-test
       'py-execute-expression-jython-noswitch-test
       'py-execute-expression-jython-dedicated-test
       'py-execute-expression-jython-dedicated-switch-test
       'py-execute-partial-expression-python-test
       'py-execute-partial-expression-python-switch-test
       'py-execute-partial-expression-python-noswitch-test
       'py-execute-partial-expression-python-dedicated-test
       'py-execute-partial-expression-python-dedicated-switch-test
       'py-execute-partial-expression-ipython-test
       'py-execute-partial-expression-ipython-switch-test
       'py-execute-partial-expression-ipython-noswitch-test
       'py-execute-partial-expression-ipython-dedicated-test
       'py-execute-partial-expression-ipython-dedicated-switch-test
       'py-execute-partial-expression-python3-test
       'py-execute-partial-expression-python3-switch-test
       'py-execute-partial-expression-python3-noswitch-test
       'py-execute-partial-expression-python3-dedicated-test
       'py-execute-partial-expression-python3-dedicated-switch-test
       'py-execute-partial-expression-python2-test
       'py-execute-partial-expression-python2-switch-test
       'py-execute-partial-expression-python2-noswitch-test
       'py-execute-partial-expression-python2-dedicated-test
       'py-execute-partial-expression-python2-dedicated-switch-test
       'py-execute-partial-expression-python2.7-test
       'py-execute-partial-expression-python2.7-switch-test
       'py-execute-partial-expression-python2.7-noswitch-test
       'py-execute-partial-expression-python2.7-dedicated-test
       'py-execute-partial-expression-python2.7-dedicated-switch-test
       'py-execute-partial-expression-jython-test
       'py-execute-partial-expression-jython-switch-test
       'py-execute-partial-expression-jython-noswitch-test
       'py-execute-partial-expression-jython-dedicated-test
       'py-execute-partial-expression-jython-dedicated-switch-test
       'py-execute-line-python-test
       'py-execute-line-python-switch-test
       'py-execute-line-python-noswitch-test
       'py-execute-line-python-dedicated-test
       'py-execute-line-python-dedicated-switch-test
       'py-execute-line-ipython-test
       'py-execute-line-ipython-switch-test
       'py-execute-line-ipython-noswitch-test
       'py-execute-line-ipython-dedicated-test
       'py-execute-line-ipython-dedicated-switch-test
       'py-execute-line-python3-test
       'py-execute-line-python3-switch-test
       'py-execute-line-python3-noswitch-test
       'py-execute-line-python3-dedicated-test
       'py-execute-line-python3-dedicated-switch-test
       'py-execute-line-python2-test
       'py-execute-line-python2-switch-test
       'py-execute-line-python2-noswitch-test
       'py-execute-line-python2-dedicated-test
       'py-execute-line-python2-dedicated-switch-test
       'py-execute-line-python2.7-test
       'py-execute-line-python2.7-switch-test
       'py-execute-line-python2.7-noswitch-test
       'py-execute-line-python2.7-dedicated-test
       'py-execute-line-python2.7-dedicated-switch-test
       'py-execute-line-jython-test
       'py-execute-line-jython-switch-test
       'py-execute-line-jython-noswitch-test
       'py-execute-line-jython-dedicated-test
       'py-execute-line-jython-dedicated-switch-test

       'py-beginning-of-block-test
       'py-end-of-block-test
       'py-beginning-of-block-or-clause-test
       'py-end-of-block-or-clause-test
       'py-beginning-of-def-test
       'py-end-of-def-test
       'py-beginning-of-def-or-class-test
       'py-end-of-def-or-class-test
       'py-electric-backspace-test
       'py-electric-delete-test
       'dict-error-test
       ;;         'py-expand-abbrev-pst-pdb.set_trace-test
       'near-bob-beginning-of-statement-test
       'bob-beginning-of-statement-test
       'honor-comments-indent-test
       'assignment-indent-test
       'if-elif-test
       'if-elif-bob-test
       'try-else-clause-test
       'try-except-test
       'assignment-after-block-test
       'py-beginning-of-clause-test
       'py-end-of-clause-test
       'py-beginning-of-expression-test
       'py-end-of-expression-test
       'py-expression-index-test
       'py-indent-after-assigment-test
       'leave-dict-test
       'eofs-attribut-test
       'py-insert-super-python2-test
       'py-insert-super-python3-test
       'args-list-first-line-indent-test
       'py-partial-expression-test
       'py-execute-block-test
       'multiline-list-indent-test
       'close-block-test
       'py-shift-block-test
       'nesting-if-test
       'py-end-of-print-statement-test
       'nested-try-test
       'nested-if-test
       'nested-try-finally-test
       'py-shell-complete-test
       'py-completion-at-point-test
       'python-dedicated-test
       'tqs-list-error-test
       'py-mark-def-commandp-test
       'split-windows-on-execute-p-test
       'switch-windows-on-execute-p-test
       'py-install-directory-path-test
       'UnicodeEncodeError-python3-test

       ))

(defun py-run-tests (&optional arg)
  (interactive "p")
  (dolist (ele python-mode-tests)
    (funcall ele arg)))

(defvar python-mode-teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
        elif b:
            pass
        else b:
            pass

            ])

"
  "String used for tests by python-mode-test.el")

(setq python-mode-teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
        elif b:
            pass
        else b:
            pass
")
"class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if a:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
        elif b:
           pass
        else b:
           pass
"
(defun py-beginning-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-block-base arg teststring)))

(defun py-beginning-of-block-base ()
  (goto-char (point-max))
  (py-beginning-of-block)
  (assert (looking-at "if") nil "py-beginning-of-block-test failed"))

(defun py-end-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if foo:

            ar_atpt_python_list_roh = ([
                'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])

        elif bar:
            pass
        else:
            pass
 "))
    (py-bug-tests-intern 'py-end-of-block-base arg teststring)))

(defun py-end-of-block-base ()
  (goto-char 326)
  (assert (eq 562 (py-end-of-clause)) nil "py-end-of-block-test #1 failed")
  (assert (eq 598 (py-end-of-clause)) nil "py-end-of-block-test #2 failed")
  (assert (eq 629 (py-end-of-block)) nil "py-end-of-block-test #3 failed"))

(defun py-beginning-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-base arg teststring)))

(defun py-beginning-of-block-or-clause-base ()
  (goto-char (point-max))
  (py-beginning-of-block-or-clause)
  (assert (looking-at "else") nil "py-beginning-of-block-or-clause-test failed")
  (py-beginning-of-block-or-clause)
  (assert (looking-at "elif") nil "py-beginning-of-block-or-clause-test failed")
  (py-beginning-of-block-or-clause)
  (assert (looking-at "if") nil "py-beginning-of-block-or-clause-test failed")

  )

(defun py-end-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-block-or-clause-base arg teststring)))

(defun py-end-of-block-or-clause-base ()
  (py-beginning-of-block-or-clause)
  (py-end-of-block-or-clause)
  (assert (eq (point) 626) nil "py-end-of-block-or-clause-test failed"))

(defun py-beginning-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-def-base arg teststring)))

(defun py-beginning-of-def-base ()
  (py-beginning-of-def)
  (assert (eq (point) 238) nil "py-beginning-of-def-test failed")
  )

(defun py-end-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-def-base arg teststring)))

(defun py-end-of-def-base ()
  (py-beginning-of-def)
  (py-end-of-def)
  (assert (eq (point) 626) nil "py-end-of-def-test failed")
  )

(defun py-beginning-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-base arg teststring)))

(defun py-beginning-of-def-or-class-base ()
  (py-beginning-of-def-or-class 4)
  (assert (eq (point) 238) nil "py-beginning-of-def-or-class-test failed"))

(defun py-end-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-end-of-def-or-class-base arg teststring)))

(defun py-end-of-def-or-class-base ()
  (assert (eq 238 (py-beginning-of-def-or-class)) nil "py-end-of-def-or-class-test #1 failed")
  (assert (eq 146 (py-beginning-of-def-or-class)) nil "py-end-of-def-or-class-test #2 failed")
  (goto-char 201)
  (assert (eq 232 (py-end-of-def-or-class)) nil "py-end-of-def-or-class-test #3 failed")
  (assert (eq 626 (py-end-of-def-or-class '(4))) nil "py-end-of-def-or-class-test #4 failed"))

(defun py-electric-backspace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-electric-backspace-base arg teststring)))

(defun py-electric-backspace-base ()
  (goto-char 232)
  (py-newline-and-indent)
  (sit-for 0.1)
  (assert (eq 241 (point)) nil "py-electric-backspace-test #1 failed")
  (py-electric-backspace)
  (assert (eq 4 (current-column)) nil "py-electric-backspace-test #2 failed")
  (py-electric-backspace)
  (assert (eq 0 (current-column)) nil "py-electric-backspace-test #3 failed")
  (py-electric-backspace)
  (assert (eq 232 (point)) nil "py-electric-backspace-test #4 failed"))

(defun py-electric-delete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-electric-delete-base arg teststring)))

(defun py-electric-delete-base ()
  (goto-char 202)
  (py-electric-delete)
  (assert (eq 4 (length (progn (looking-at "[ \t]+")(match-string-no-properties 0)))) nil "py-electric-delete-test #1 failed")
  (py-electric-delete)
  (assert (not (looking-at "[ \t]+")) nil "py-electric-delete-test #2 failed")
  (py-electric-delete)
  (assert (looking-at "ict") nil "py-electric-delete-test #2 failed")
  )

(defun UnicodeEncodeError-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -\*- coding: utf-8 -\*-\n
print(\'\\xA9\')
")))
    (py-bug-tests-intern 'UnicodeEncodeError-python3-base 2 teststring)))

(defun UnicodeEncodeError-python3-base ()
  (delete-other-windows)
  (let ((py-split-windows-on-execute-p t)
        (py-shell-switch-buffers-on-execute-p t)
        erg pos)
    (py-execute-region 49 62)
    (setq erg (goto-char (point-max)))
    (sit-for 1.0)
    (assert (and (setq pos (search-backward "©"))(< (- erg pos) 9)) nil "UnicodeEncodeError-python3-test failed")))

(defun dict-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -*- coding: utf-8 -*-

class foo(bar):
	\"\"\"baz\"\"\"
       	_some_extensions = {

		'38': 'asd', #  whatever
		'43': 'ddd',
		'45': 'ddd',
	}
")))
    (py-bug-tests-intern 'dict-error-base arg teststring)))

(defun dict-error-base ()
  (goto-char 78)
  (assert (eq 166 (progn (py-end-of-statement) (sit-for 0.1) (point) )) nil "dict-error-test failed"))

(defun py-expand-abbrev-pst-pdb.set_trace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print('\xA9')
pst
")))
    (py-bug-tests-intern 'py-expand-abbrev-pst-pdb.set_trace-base arg teststring)))

(defun py-expand-abbrev-pst-pdb.set_trace-base ()
  (forward-char -1)
  (expand-abbrev)
  (sit-for 1)
  ;;  (assert (string= (expand-abbrev) "pst") nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  ;; (assert (expand-abbrev) nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  (progn (looking-back "pdb.set_trace()")
         ;; (message "Looking back: %s" (match-string-no-properties 0))
         )
  (assert (looking-back "pdb.set_trace()")
          ;;          (message "%s" (match-string-no-properties 1))
          nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))

(defun near-bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -*- coding: utf-8 -*-

print u'\xA9'
")))
    (py-bug-tests-intern 'near-bob-beginning-of-statement-base arg teststring)))

(defun near-bob-beginning-of-statement-base ()
  (goto-char 50)
  (assert (eq 0 (py-compute-indentation)) nil "near-bob-beginning-of-statement-test failed"))

(defun bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Foo.py
"))
    (py-bug-tests-intern 'bob-beginning-of-statement-base arg teststring)))

(defun bob-beginning-of-statement-base ()
  (py-beginning-of-statement)
  (assert (eq 1 (point))  "bob-beginning-of-statement-test failed"))

(defun honor-comments-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (py-bug-tests-intern 'honor-comments-indent-base arg teststring)))

(defun honor-comments-indent-base ()
  (goto-char 19)
  (assert (eq 4 (py-compute-indentation)) nil "honor-comments-indent-test failed"))

(defun first-line-offset-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (py-bug-tests-intern 'first-line-offset-base arg teststring)))

(defun first-line-offset-base ()
  (goto-char 18)
  (assert (eq 4 (py-compute-indentation)) nil "first-line-offset-test failed"))

(defun assignment-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
sammlung = []
"))
    (py-bug-tests-intern 'assignment-indent-base arg teststring)))

(defun assignment-indent-base ()
  (goto-char 12)
  (assert (eq 4 (py-compute-indentation)) nil "assignment-indent-test failed"))

(defun if-elif-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if bar in baz:
    print \"0, baz\"
    abc[1] = \"x\"

elif barr in bazz:
    print \"\"
"))
    (py-bug-tests-intern 'if-elif-base arg teststring)))

(defun if-elif-base ()
  (goto-char 76)
  (assert (eq 4 (py-compute-indentation)) nil "if-elif.py-test failed"))

(defun if-elif-bob-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if bar in baz:
    print \"0, baz\"
"))
    (py-bug-tests-intern 'if-elif-bob-base arg teststring)))

(defun if-elif-bob-base ()
  (goto-char (point-min))
  (assert (eq 0 (py-compute-indentation)) nil "if-elif-bob.py-test failed"))

(defun try-else-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
# an example from http://www.thomas-guettler.de
# © 2002-2008 Thomas Güttler. Der Text darf nach belieben kopiert und modifiziert werden, solange dieser Hinweis zum Copyright und ein Links zu dem Original unter www.thomas-guettler.de erhalten bleibt. Es wäre nett, wenn Sie mir Verbesserungsvorschläge mitteilen: guettli@thomas-guettler.de

def _commit_on_success(*args, **kw):
    begin()
    try:
        res = func(*args, **kw)
    except Exception, e:
        rollback()
        raise # Re-raise (aufgefangene Exception erneut werfen)
    else:
        commit()
    return res
"))
    (py-bug-tests-intern 'try-else-clause-base arg teststring)))

(defun try-else-clause-base ()
  (goto-char 541)
  (assert (eq 4 (py-compute-indentation)) nil "try-else-clause-test failed"))

(defun try-except-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
# an example from http://www.thomas-guettler.de
# © 2002-2008 Thomas Güttler. Der Text darf nach belieben kopiert und modifiziert werden, solange dieser Hinweis zum Copyright und ein Links zu dem Original unter www.thomas-guettler.de erhalten bleibt. Es wäre nett, wenn Sie mir Verbesserungsvorschläge mitteilen: guettli@thomas-guettler.de

def _commit_on_success(*args, **kw):
    begin()
    try:
        res = func(*args, **kw)
    except Exception, e:
        rollback()
        raise # Re-raise (aufgefangene Exception erneut werfen)
    else:
        commit()
    return res
"))
    (py-bug-tests-intern 'try-except-base arg teststring)))

(defun try-except-base ()
  (goto-char 434)
  (assert (eq 4 (py-compute-indentation)) nil "try-except-test failed"))

(defun assignment-after-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"

a = \"asdf\"
b = \"asdf\"
"))
    (py-bug-tests-intern 'assignment-after-block-base arg teststring)))

(defun assignment-after-block-base ()
  (forward-line -1)
  (assert (eq 0 (py-compute-indentation)) nil "assignment-after-block-test failed"))

(defun py-beginning-of-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (py-bug-tests-intern 'py-beginning-of-clause-base arg teststring)))

(defun py-beginning-of-clause-base ()
  (goto-char 364)
  (assert (eq 346 (py-beginning-of-clause)) "py-beginning-of-clause-test failed"))

(defun py-end-of-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (py-bug-tests-intern 'py-end-of-clause-base arg teststring)))

(defun py-end-of-clause-base ()
  (goto-char 364)
  (assert (eq 412 (py-end-of-clause)) "py-end-of-clause-test failed"))

(defun py-beginning-of-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
"))
    (py-bug-tests-intern 'py-beginning-of-expression-base arg teststring)))

(defun py-beginning-of-expression-base ()
  (goto-char 227)
  (assert (eq 221 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #1 failed")
  (assert (eq 205 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #2 failed")
  (assert (eq 177 (py-beginning-of-expression)) nil "py-beginning-of-expression-test #3 failed"))

(defun py-end-of-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
"))
    (py-bug-tests-intern 'py-end-of-expression-base arg teststring)))

(defun py-end-of-expression-base ()
  (goto-char 49)
  (assert (eq 60 (py-end-of-expression)) nil "py-end-of-expression-test failed")
  (goto-char 69)
  (assert (eq 72 (py-end-of-expression)) nil "py-end-of-expression-test failed")
  (assert (eq 85 (py-end-of-expression)) nil "py-end-of-expression-test failed")
  (assert (eq 94 (py-end-of-expression)) nil "py-end-of-expression-test failed")
  (assert (eq 113 (py-end-of-expression)) nil "py-end-of-expression-test failed")
  (goto-char 225)
  (assert (eq 232 (py-end-of-expression)) nil "py-end-of-expression-test failed"))

(defun py-expression-index-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
 # -\*- coding: utf-8 -\*-
b = a[0].split(':')[1]
")))
    (py-bug-tests-intern 'py-expression-index-base arg teststring)))

(defun py-expression-index-base ()
  (goto-char 58)
  (assert (eq 71 (py-end-of-expression)) nil "py-expression-index-test failed")
)

(defun py-insert-super-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -\*- coding: utf-8 -\*-
# As example given in Python v3.1 documentation » The Python Standard Library »
#
# class C(B):
#     def method(self, arg):
#         super().method(arg) # This does the same thing as:
#                                # super(C, self).method(arg)\"

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        ")))
    (py-bug-tests-intern 'py-insert-super-python2-base arg teststring)))

(defun py-insert-super-python2-base ()
  (ignore-errors (py-insert-super))
  (sit-for 0.1)
  (assert (looking-back "super(OrderedDict1, self).__init__(d={})") nil "py-insert-super-python2-test failed"))

(defun py-insert-super-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let* ((py-test-shebang "#! /usr/bin/env python3")
         (teststring (concat py-test-shebang "
# -\*- coding: utf-8 -\*-
# As example given in Python v3.1 documentation » The Python Standard Library »
#
# class C(B):
#     def method(self, arg):
#         super().method(arg) # This does the same thing as:
#                                # super(C, self).method(arg)\"

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        ")))
    (py-bug-tests-intern 'py-insert-super-python3-base arg teststring)))

(defun py-insert-super-python3-base ()
  (save-excursion
    (py-insert-super))
  (sit-for 0.2)
  (assert (looking-at "super().__init__(d={})") nil "py-insert-super-python3-test failed"))

(defun py-indent-after-assigment-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

#####################################
def foo( self, bar=False ):  # version 12345
    title = self.barz.attrs['file'].split('.')[ -1 ]
    if asdf:
")))
    (py-bug-tests-intern 'indent-after-assigment-base arg teststring)))

(defun indent-after-assigment-base ()
  (goto-char 185)
  (assert (eq 4 (py-compute-indentation)) nil "py-indent-after-assigment-test failed"))

(defun leave-dict-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
foo = {
    b\"yyyyt\": \"bxxk\",
    \"bxxk\": { \"yyer\": [\"wxrddef\", \"yytem\", \"hym\",],
              \"wfter\": [], \"xbject\": BxxkTwg, },
    \"yytem\": { \"yyer\": [], \"wfter\": [\"yytem\"], \"xbject\": ItemTwg, },
    \"hym\": { \"yyer\": [], \"wfter\": [\"hym\"], \"xbject\": ItemTwg, },
    \"yyfx\": { \"yyer\": [], \"wfter\": [\"yytem\", \"hym\"], \"xbject\": IfxTwg, },
    \"wxrddef\": { \"yyer\": [], \"wfter\": [\"yyfx\", \"yytem\", \"hym\"], \"xbject\": WxrddefTwg, },
}
"))
    (py-bug-tests-intern 'leave-dict-base arg teststring)))

(defun leave-dict-base ()
  (goto-char (point-min))
  (py-end-of-statement)
  (assert (eq 431 (point)) nil "leave-dict-test failed"))

(defun eofs-attribut-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo( baz ):  # version
    return baz.replace(\"\+\",\"§\").replace(\"_\", \" \").replace(\"ﬁ\",\"fr\").replace(
        \"ﬂ\", \"fg\").replace(\"--\", \"ü\")
"))
    (py-bug-tests-intern 'eofs-attribut-base arg teststring)))

(defun eofs-attribut-base ()
  (forward-line -2)
  (assert (eq 142 (py-end-of-statement))  nil "eofs-attribut-test failed"))

(defun args-list-first-line-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
            )
    )
")))
    (py-bug-tests-intern 'args-list-first-line-indent-base arg teststring)))

(defun args-list-first-line-indent-base ()
  (goto-char 72)
  (assert (eq 4 (py-compute-indentation)) nil "args-list-first-line-indent-test failed"))

(defun py-partial-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
            )
        )
")))
    (py-bug-tests-intern 'py-partial-expression-base arg teststring)))

(defun py-partial-expression-base ()
  (goto-char 102)
  (assert (eq 100 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #1 failed")
  (assert (eq 108 (py-end-of-partial-expression)) nil "py-partial-expression-test #2 failed")
  (goto-char 178)
  (assert (eq 177 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  (assert (eq 181 (py-end-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  )

(defun py-execute-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True:
    print \"asdf\""))
    (py-bug-tests-intern 'py-execute-block-base 2 teststring)))

(defun py-execute-block-base ()
  (beginning-of-line)
  (let ((py-shell-switch-buffers-on-execute-p nil)
        (py-cleanup-temporary nil))
    (assert (py-execute-block) nil "py-execute-block-test failed")))

(defun multiline-list-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print [1, 2,
    3, 4]"))
    (py-bug-tests-intern 'multiline-list-indent-base arg teststring)))

(defun multiline-list-indent-base ()
  (assert (eq 7 (py-compute-indentation)) nil "multiline-list-indent-test failed"))

(defun no-switch-no-split-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

print u'\\xA9'
")))
    (py-bug-tests-intern 'no-switch-no-split-base arg teststring)))

(defun no-switch-no-split-base ()
  (let ((oldbuf (current-buffer))
        py-split-windows-on-execute py-shell-switch-buffers-on-execute-p)
    (goto-char 49)
    (push-mark)
    (end-of-line)
    (py-execute-region (line-beginning-position) (point))
    (assert (window-full-height-p) "no-switch-no-split-test failed")
    (assert (eq (current-buffer) oldbuf))))

(defun close-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
")))
    (py-bug-tests-intern 'close-block-base arg teststring)))

(defun close-block-base ()
  (goto-char 102)
  (assert (eq 4 (py-close-block)) nil "close-block-test failed"))

(defun py-shift-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
         ")))
    (py-bug-tests-intern 'py-shift-block-base arg teststring)))

(defun py-shift-block-base ()
  (goto-char 237)
  (assert (eq 12 (py-shift-block-right)) nil "py-shift-block-test #1 failed")
  (assert (eq 8 (py-shift-block-left)) nil "py-shift-block-test #1 failed"))

(defun nesting-if-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if foo:
    if bar:
        pass
    else:
        pass
else:
    pass

")))
    (py-bug-tests-intern 'nesting-if-test-base arg teststring)))

(defun nesting-if-test-base ()
  (goto-char 105)
  (assert (eq 0 (py-compute-indentation)) nil "nesting-if-test failed"))

(defun py-end-of-print-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def usage():
    print \"\"\"Error: %s
somme errors
\"\"\" % (
          os.path.basename(sys.argv[0]))
")))
    (py-bug-tests-intern 'py-end-of-print-statement-base arg teststring)))

(defun py-end-of-print-statement-base ()
  (goto-char 66)
  (sit-for 0.2)
  (assert (eq 146 (py-end-of-statement)) nil "py-end-of-print-statement-test failed"))

(defun nested-try-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

def main(argv):
    grammar = \"foo.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        try:
            bla
        except getopt.GetoptError:
            asdf()
        finally:
            return \"blub\"
    finally:
        print \"asdf\"

")))
    (py-bug-tests-intern 'nested-try-base arg teststring)))

(defun nested-try-base ()
  (goto-char 306)
  (assert (eq 8 (py-compute-indentation)) nil "nested-try-test failed"))

(defun nested-if-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

if abr:
    if x > 0:
        if foo:
            print \"foo\"
        elif bar:
            print \"abasdf\"
        elif baz:
            for i in range(100):
                print i
            else:
                print \\\"All done\\\"
    elif x < 0:
        print \\\"x is negative\\\"
else:
    print \"asbd\"

")))
    (py-bug-tests-intern 'nested-if-base arg teststring)))

(defun nested-if-base ()
  (goto-char 299)
  (assert (eq 8 (py-compute-indentation)) nil "nested-if-test failed"))

(defun nested-try-finally-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-

# Example from:
# To: python-ideas@python.org
# From: Nikolaus Rath <Nikolaus@rath.org>
# Date: Tue, 18 Oct 2011 22:14:56 -0400
# Message-ID: <87pqhtafrz.fsf@vostro.rath.org>

def my_fun():
    allocate_res1()
    try:
        # do stuff
        allocate_res2()
        try:
            # do stuff
            allocate_res3()
            try:
                do stuff
            finally:
                cleanup_res3()
        finally:
            cleanup_res2()
    finally:
        cleanup_res1()

    return

")))
    (py-bug-tests-intern 'nested-try-finally-base arg teststring)))

(defun nested-try-finally-base ()
  (goto-char 431)
  (assert (eq 12 (py-compute-indentation)) nil "nested-try-finally-test failed"))

(defun tqs-list-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
class foo(bar, baz):
    \"\"\"
    foo is an ABC for matrix containers; i.e.,
    \\\"\\\"\\\"containers of a finite number of orig
\"\"\"
    pass
")))
    (py-bug-tests-intern 'tqs-list-error-base 2 teststring)))

(defun tqs-list-error-base ()
  (goto-char 90)
  (sit-for 0.2)
  (assert (eq 175 (py-end-of-statement)) nil "tqs-list-error-test failed"))

(defun py-smart-indent-eight-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((py-smart-indentation t)
        (teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
for x in y:
    for z in l:
        for r in t:
                pass # <--- indents here. Pressing <backspace> dedents eight spaces (i.e. you can go to column 0 in two presess)
")))
    (py-bug-tests-intern 'py-smart-indent-eight-base arg teststring)))

(defun py-smart-indent-eight-base ()
  (goto-char 112)
  (assert (eq 12 (py-compute-indentation)) nil "py-smart-indent-eight-test failed"))

(defun py-install-directory-path-test (&optional arg)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
")))
    (py-bug-tests-intern 'py-install-directory-path-base arg teststring)))

(defun py-install-directory-path-base ()
  "See if `py-install-directory' is set when required. "
  (assert (py-install-directory-check) nil "`py-install-directory' not valid. See INSTALL. "))

;;; Commandp tests
(defun clear-flymake-allowed-file-name-masks-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'clear-flymake-allowed-file-name-masks-commandp-base arg teststring)))

(defun clear-flymake-allowed-file-name-masks-commandp-base ()
  (assert (commandp 'clear-flymake-allowed-file-name-masks) nil "clear-flymake-allowed-file-name-masks-commandp-test failed"))

(defun pylint-flymake-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pylint-flymake-mode-commandp-base arg teststring)))

(defun pylint-flymake-mode-commandp-base ()
  (assert (commandp 'pylint-flymake-mode) nil "pylint-flymake-mode-commandp-test failed"))

(defun pyflakes-flymake-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pyflakes-flymake-mode-commandp-base arg teststring)))

(defun pyflakes-flymake-mode-commandp-base ()
  (assert (commandp 'pyflakes-flymake-mode) nil "pyflakes-flymake-mode-commandp-test failed"))

(defun pychecker-flymake-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pychecker-flymake-mode-commandp-base arg teststring)))

(defun pychecker-flymake-mode-commandp-base ()
  (assert (commandp 'pychecker-flymake-mode) nil "pychecker-flymake-mode-commandp-test failed"))

(defun pep8-flymake-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pep8-flymake-mode-commandp-base arg teststring)))

(defun pep8-flymake-mode-commandp-base ()
  (assert (commandp 'pep8-flymake-mode) nil "pep8-flymake-mode-commandp-test failed"))

(defun pyflakespep8-flymake-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pyflakespep8-flymake-mode-commandp-base arg teststring)))

(defun pyflakespep8-flymake-mode-commandp-base ()
  (assert (commandp 'pyflakespep8-flymake-mode) nil "pyflakespep8-flymake-mode-commandp-test failed"))

(defun py-pylint-doku-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pylint-doku-commandp-base arg teststring)))

(defun py-pylint-doku-commandp-base ()
  (assert (commandp 'py-pylint-doku) nil "py-pylint-doku-commandp-test failed"))

(defun py-pyflakes-run-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pyflakes-run-commandp-base arg teststring)))

(defun py-pyflakes-run-commandp-base ()
  (assert (commandp 'py-pyflakes-run) nil "py-pyflakes-run-commandp-test failed"))

(defun py-pyflakespep8-run-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pyflakespep8-run-commandp-base arg teststring)))

(defun py-pyflakespep8-run-commandp-base ()
  (assert (commandp 'py-pyflakespep8-run) nil "py-pyflakespep8-run-commandp-test failed"))

(defun py-pyflakespep8-help-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pyflakespep8-help-commandp-base arg teststring)))

(defun py-pyflakespep8-help-commandp-base ()
  (assert (commandp 'py-pyflakespep8-help) nil "py-pyflakespep8-help-commandp-test failed"))

(defun py-guess-pdb-path-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-guess-pdb-path-commandp-base arg teststring)))

(defun py-guess-pdb-path-commandp-base ()
  (assert (commandp 'py-guess-pdb-path) nil "py-guess-pdb-path-commandp-test failed"))

(defun highlight-indentation-on-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'highlight-indentation-on-commandp-base arg teststring)))

(defun highlight-indentation-on-commandp-base ()
  (assert (commandp 'highlight-indentation-on) nil "highlight-indentation-on-commandp-test failed"))

(defun highlight-indentation-off-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'highlight-indentation-off-commandp-base arg teststring)))

(defun highlight-indentation-off-commandp-base ()
  (assert (commandp 'highlight-indentation-off) nil "highlight-indentation-off-commandp-test failed"))

(defun highlight-indentation-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'highlight-indentation-commandp-base arg teststring)))

(defun highlight-indentation-commandp-base ()
  (assert (commandp 'highlight-indentation) nil "highlight-indentation-commandp-test failed"))

(defun py-insert-default-shebang-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-insert-default-shebang-commandp-base arg teststring)))

(defun py-insert-default-shebang-commandp-base ()
  (assert (commandp 'py-insert-default-shebang) nil "py-insert-default-shebang-commandp-test failed"))

(defun py-electric-comment-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-electric-comment-commandp-base arg teststring)))

(defun py-electric-comment-commandp-base ()
  (assert (commandp 'py-electric-comment) nil "py-electric-comment-commandp-test failed"))

(defun py-electric-colon-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-electric-colon-commandp-base arg teststring)))

(defun py-electric-colon-commandp-base ()
  (assert (commandp 'py-electric-colon) nil "py-electric-colon-commandp-test failed"))

(defun py-electric-backspace-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-electric-backspace-commandp-base arg teststring)))

(defun py-electric-backspace-commandp-base ()
  (assert (commandp 'py-electric-backspace) nil "py-electric-backspace-commandp-test failed"))

(defun py-electric-delete-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-electric-delete-commandp-base arg teststring)))

(defun py-electric-delete-commandp-base ()
  (assert (commandp 'py-electric-delete) nil "py-electric-delete-commandp-test failed"))

(defun py-indent-line-outmost-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-indent-line-outmost-commandp-base arg teststring)))

(defun py-indent-line-outmost-commandp-base ()
  (assert (commandp 'py-indent-line-outmost) nil "py-indent-line-outmost-commandp-test failed"))

(defun py-indent-line-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-indent-line-commandp-base arg teststring)))

(defun py-indent-line-commandp-base ()
  (assert (commandp 'py-indent-line) nil "py-indent-line-commandp-test failed"))

(defun py-newline-and-indent-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-newline-and-indent-commandp-base arg teststring)))

(defun py-newline-and-indent-commandp-base ()
  (assert (commandp 'py-newline-and-indent) nil "py-newline-and-indent-commandp-test failed"))

(defun py-newline-and-dedent-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-newline-and-dedent-commandp-base arg teststring)))

(defun py-newline-and-dedent-commandp-base ()
  (assert (commandp 'py-newline-and-dedent) nil "py-newline-and-dedent-commandp-test failed"))

(defun toggle-indent-tabs-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'toggle-indent-tabs-mode-commandp-base arg teststring)))

(defun toggle-indent-tabs-mode-commandp-base ()
  (assert (commandp 'py-toggle-indent-tabs-mode) nil "toggle-indent-tabs-mode-commandp-test failed"))

(defun indent-tabs-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'indent-tabs-mode-commandp-base arg teststring)))

(defun indent-tabs-mode-commandp-base ()
  (assert (commandp 'py-indent-tabs-mode) nil "indent-tabs-mode-commandp-test failed"))

(defun indent-tabs-mode-on-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'indent-tabs-mode-on-commandp-base arg teststring)))

(defun indent-tabs-mode-on-commandp-base ()
  (assert (commandp 'py-indent-tabs-mode-on) nil "indent-tabs-mode-on-commandp-test failed"))

(defun indent-tabs-mode-off-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'indent-tabs-mode-off-commandp-base arg teststring)))

(defun indent-tabs-mode-off-commandp-base ()
  (assert (commandp 'py-indent-tabs-mode-off) nil "indent-tabs-mode-off-commandp-test failed"))

(defun py-guess-indent-offset-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-guess-indent-offset-commandp-base arg teststring)))

(defun py-guess-indent-offset-commandp-base ()
  (assert (commandp 'py-guess-indent-offset) nil "py-guess-indent-offset-commandp-test failed"))

(defun py-narrow-to-defun-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-narrow-to-defun-commandp-base arg teststring)))

(defun py-narrow-to-defun-commandp-base ()
  (assert (commandp 'py-narrow-to-defun) nil "py-narrow-to-defun-commandp-test failed"))

(defun py-shift-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-left-commandp-base arg teststring)))

(defun py-shift-left-commandp-base ()
  (assert (commandp 'py-shift-left) nil "py-shift-left-commandp-test failed"))

(defun py-shift-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-right-commandp-base arg teststring)))

(defun py-shift-right-commandp-base ()
  (assert (commandp 'py-shift-right) nil "py-shift-right-commandp-test failed"))

(defun py-shift-paragraph-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-paragraph-right-commandp-base arg teststring)))

(defun py-shift-paragraph-right-commandp-base ()
  (assert (commandp 'py-shift-paragraph-right) nil "py-shift-paragraph-right-commandp-test failed"))

(defun py-shift-paragraph-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-paragraph-left-commandp-base arg teststring)))

(defun py-shift-paragraph-left-commandp-base ()
  (assert (commandp 'py-shift-paragraph-left) nil "py-shift-paragraph-left-commandp-test failed"))

(defun py-shift-block-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-block-right-commandp-base arg teststring)))

(defun py-shift-block-right-commandp-base ()
  (assert (commandp 'py-shift-block-right) nil "py-shift-block-right-commandp-test failed"))

(defun py-shift-block-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-block-left-commandp-base arg teststring)))

(defun py-shift-block-left-commandp-base ()
  (assert (commandp 'py-shift-block-left) nil "py-shift-block-left-commandp-test failed"))

(defun py-shift-clause-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-clause-right-commandp-base arg teststring)))

(defun py-shift-clause-right-commandp-base ()
  (assert (commandp 'py-shift-clause-right) nil "py-shift-clause-right-commandp-test failed"))

(defun py-shift-clause-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-clause-left-commandp-base arg teststring)))

(defun py-shift-clause-left-commandp-base ()
  (assert (commandp 'py-shift-clause-left) nil "py-shift-clause-left-commandp-test failed"))

(defun py-shift-def-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-def-right-commandp-base arg teststring)))

(defun py-shift-def-right-commandp-base ()
  (assert (commandp 'py-shift-def-right) nil "py-shift-def-right-commandp-test failed"))

(defun py-shift-def-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-def-left-commandp-base arg teststring)))

(defun py-shift-def-left-commandp-base ()
  (assert (commandp 'py-shift-def-left) nil "py-shift-def-left-commandp-test failed"))

(defun py-shift-class-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-class-right-commandp-base arg teststring)))

(defun py-shift-class-right-commandp-base ()
  (assert (commandp 'py-shift-class-right) nil "py-shift-class-right-commandp-test failed"))

(defun py-shift-class-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-class-left-commandp-base arg teststring)))

(defun py-shift-class-left-commandp-base ()
  (assert (commandp 'py-shift-class-left) nil "py-shift-class-left-commandp-test failed"))

(defun py-shift-line-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-line-right-commandp-base arg teststring)))

(defun py-shift-line-right-commandp-base ()
  (assert (commandp 'py-shift-line-right) nil "py-shift-line-right-commandp-test failed"))

(defun py-shift-line-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-line-left-commandp-base arg teststring)))

(defun py-shift-line-left-commandp-base ()
  (assert (commandp 'py-shift-line-left) nil "py-shift-line-left-commandp-test failed"))

(defun py-shift-statement-right-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-statement-right-commandp-base arg teststring)))

(defun py-shift-statement-right-commandp-base ()
  (assert (commandp 'py-shift-statement-right) nil "py-shift-statement-right-commandp-test failed"))

(defun py-shift-statement-left-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shift-statement-left-commandp-base arg teststring)))

(defun py-shift-statement-left-commandp-base ()
  (assert (commandp 'py-shift-statement-left) nil "py-shift-statement-left-commandp-test failed"))

(defun py-indent-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-indent-region-commandp-base arg teststring)))

(defun py-indent-region-commandp-base ()
  (assert (commandp 'py-indent-region) nil "py-indent-region-commandp-test failed"))

(defun py-beginning-of-paragraph-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-paragraph-position-commandp-base arg teststring)))

(defun py-beginning-of-paragraph-position-commandp-base ()
  (assert (commandp 'py-beginning-of-paragraph-position) nil "py-beginning-of-paragraph-position-commandp-test failed"))

(defun py-end-of-paragraph-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-paragraph-position-commandp-base arg teststring)))

(defun py-end-of-paragraph-position-commandp-base ()
  (assert (commandp 'py-end-of-paragraph-position) nil "py-end-of-paragraph-position-commandp-test failed"))

(defun py-beginning-of-block-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-position-commandp-base arg teststring)))

(defun py-beginning-of-block-position-commandp-base ()
  (assert (commandp 'py-beginning-of-block-position) nil "py-beginning-of-block-position-commandp-test failed"))

(defun py-end-of-block-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-block-position-commandp-base arg teststring)))

(defun py-end-of-block-position-commandp-base ()
  (assert (commandp 'py-end-of-block-position) nil "py-end-of-block-position-commandp-test failed"))

(defun py-beginning-of-clause-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-clause-position-commandp-base arg teststring)))

(defun py-beginning-of-clause-position-commandp-base ()
  (assert (commandp 'py-beginning-of-clause-position) nil "py-beginning-of-clause-position-commandp-test failed"))

(defun py-end-of-clause-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-clause-position-commandp-base arg teststring)))

(defun py-end-of-clause-position-commandp-base ()
  (assert (commandp 'py-end-of-clause-position) nil "py-end-of-clause-position-commandp-test failed"))

(defun py-beginning-of-block-or-clause-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-position-commandp-base arg teststring)))

(defun py-beginning-of-block-or-clause-position-commandp-base ()
  (assert (commandp 'py-beginning-of-block-or-clause-position) nil "py-beginning-of-block-or-clause-position-commandp-test failed"))

(defun py-end-of-block-or-clause-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-block-or-clause-position-commandp-base arg teststring)))

(defun py-end-of-block-or-clause-position-commandp-base ()
  (assert (commandp 'py-end-of-block-or-clause-position) nil "py-end-of-block-or-clause-position-commandp-test failed"))

(defun py-beginning-of-def-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-position-commandp-base arg teststring)))

(defun py-beginning-of-def-position-commandp-base ()
  (assert (commandp 'py-beginning-of-def-position) nil "py-beginning-of-def-position-commandp-test failed"))

(defun py-end-of-def-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-def-position-commandp-base arg teststring)))

(defun py-end-of-def-position-commandp-base ()
  (assert (commandp 'py-end-of-def-position) nil "py-end-of-def-position-commandp-test failed"))

(defun py-beginning-of-class-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-class-position-commandp-base arg teststring)))

(defun py-beginning-of-class-position-commandp-base ()
  (assert (commandp 'py-beginning-of-class-position) nil "py-beginning-of-class-position-commandp-test failed"))

(defun py-end-of-class-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-class-position-commandp-base arg teststring)))

(defun py-end-of-class-position-commandp-base ()
  (assert (commandp 'py-end-of-class-position) nil "py-end-of-class-position-commandp-test failed"))

(defun py-beginning-of-def-or-class-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-position-commandp-base arg teststring)))

(defun py-beginning-of-def-or-class-position-commandp-base ()
  (assert (commandp 'py-beginning-of-def-or-class-position) nil "py-beginning-of-def-or-class-position-commandp-test failed"))

(defun py-end-of-def-or-class-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-def-or-class-position-commandp-base arg teststring)))

(defun py-end-of-def-or-class-position-commandp-base ()
  (assert (commandp 'py-end-of-def-or-class-position) nil "py-end-of-def-or-class-position-commandp-test failed"))

(defun py-beginning-of-line-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-line-position-commandp-base arg teststring)))

(defun py-beginning-of-line-position-commandp-base ()
  (assert (commandp 'py-beginning-of-line-position) nil "py-beginning-of-line-position-commandp-test failed"))

(defun py-end-of-line-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-line-position-commandp-base arg teststring)))

(defun py-end-of-line-position-commandp-base ()
  (assert (commandp 'py-end-of-line-position) nil "py-end-of-line-position-commandp-test failed"))

(defun py-beginning-of-statement-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-statement-position-commandp-base arg teststring)))

(defun py-beginning-of-statement-position-commandp-base ()
  (assert (commandp 'py-beginning-of-statement-position) nil "py-beginning-of-statement-position-commandp-test failed"))

(defun py-end-of-statement-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-statement-position-commandp-base arg teststring)))

(defun py-end-of-statement-position-commandp-base ()
  (assert (commandp 'py-end-of-statement-position) nil "py-end-of-statement-position-commandp-test failed"))

(defun py-beginning-of-expression-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-expression-position-commandp-base arg teststring)))

(defun py-beginning-of-expression-position-commandp-base ()
  (assert (commandp 'py-beginning-of-expression-position) nil "py-beginning-of-expression-position-commandp-test failed"))

(defun py-end-of-expression-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-expression-position-commandp-base arg teststring)))

(defun py-end-of-expression-position-commandp-base ()
  (assert (commandp 'py-end-of-expression-position) nil "py-end-of-expression-position-commandp-test failed"))

(defun py-beginning-of-partial-expression-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-partial-expression-position-commandp-base arg teststring)))

(defun py-beginning-of-partial-expression-position-commandp-base ()
  (assert (commandp 'py-beginning-of-partial-expression-position) nil "py-beginning-of-partial-expression-position-commandp-test failed"))

(defun py-end-of-partial-expression-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-partial-expression-position-commandp-base arg teststring)))

(defun py-end-of-partial-expression-position-commandp-base ()
  (assert (commandp 'py-end-of-partial-expression-position) nil "py-end-of-partial-expression-position-commandp-test failed"))

(defun py-bounds-of-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-statement-commandp-base arg teststring)))

(defun py-bounds-of-statement-commandp-base ()
  (assert (commandp 'py-bounds-of-statement) nil "py-bounds-of-statement-commandp-test failed"))

(defun py-bounds-of-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-block-commandp-base arg teststring)))

(defun py-bounds-of-block-commandp-base ()
  (assert (commandp 'py-bounds-of-block) nil "py-bounds-of-block-commandp-test failed"))

(defun py-bounds-of-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-clause-commandp-base arg teststring)))

(defun py-bounds-of-clause-commandp-base ()
  (assert (commandp 'py-bounds-of-clause) nil "py-bounds-of-clause-commandp-test failed"))

(defun py-bounds-of-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-block-or-clause-commandp-base arg teststring)))

(defun py-bounds-of-block-or-clause-commandp-base ()
  (assert (commandp 'py-bounds-of-block-or-clause) nil "py-bounds-of-block-or-clause-commandp-test failed"))

(defun py-bounds-of-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-def-commandp-base arg teststring)))

(defun py-bounds-of-def-commandp-base ()
  (assert (commandp 'py-bounds-of-def) nil "py-bounds-of-def-commandp-test failed"))

(defun py-bounds-of-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-class-commandp-base arg teststring)))

(defun py-bounds-of-class-commandp-base ()
  (assert (commandp 'py-bounds-of-class) nil "py-bounds-of-class-commandp-test failed"))

(defun py-bounds-of-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-region-commandp-base arg teststring)))

(defun py-bounds-of-region-commandp-base ()
  (assert (commandp 'py-bounds-of-region) nil "py-bounds-of-region-commandp-test failed"))

(defun py-bounds-of-buffer-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-buffer-commandp-base arg teststring)))

(defun py-bounds-of-buffer-commandp-base ()
  (assert (commandp 'py-bounds-of-buffer) nil "py-bounds-of-buffer-commandp-test failed"))

(defun py-bounds-of-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-expression-commandp-base arg teststring)))

(defun py-bounds-of-expression-commandp-base ()
  (assert (commandp 'py-bounds-of-expression) nil "py-bounds-of-expression-commandp-test failed"))

(defun py-bounds-of-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-partial-expression-commandp-base arg teststring)))

(defun py-bounds-of-partial-expression-commandp-base ()
  (assert (commandp 'py-bounds-of-partial-expression) nil "py-bounds-of-partial-expression-commandp-test failed"))

(defun py-bounds-of-declarations-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-declarations-commandp-base arg teststring)))

(defun py-bounds-of-declarations-commandp-base ()
  (assert (commandp 'py-bounds-of-declarations) nil "py-bounds-of-declarations-commandp-test failed"))

(defun py-beginning-of-declarations-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-declarations-commandp-base arg teststring)))

(defun py-beginning-of-declarations-commandp-base ()
  (assert (commandp 'py-beginning-of-declarations) nil "py-beginning-of-declarations-commandp-test failed"))

(defun py-end-of-declarations-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-declarations-commandp-base arg teststring)))

(defun py-end-of-declarations-commandp-base ()
  (assert (commandp 'py-end-of-declarations) nil "py-end-of-declarations-commandp-test failed"))

(defun py-declarations-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-declarations-commandp-base arg teststring)))

(defun py-declarations-commandp-base ()
  (assert (commandp 'py-declarations) nil "py-declarations-commandp-test failed"))

(defun py-kill-declarations-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-declarations-commandp-base arg teststring)))

(defun py-kill-declarations-commandp-base ()
  (assert (commandp 'py-kill-declarations) nil "py-kill-declarations-commandp-test failed"))

(defun py-bounds-of-statements-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-bounds-of-statements-commandp-base arg teststring)))

(defun py-bounds-of-statements-commandp-base ()
  (assert (commandp 'py-bounds-of-statements) nil "py-bounds-of-statements-commandp-test failed"))

(defun py-beginning-of-statements-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-statements-commandp-base arg teststring)))

(defun py-beginning-of-statements-commandp-base ()
  (assert (commandp 'py-beginning-of-statements) nil "py-beginning-of-statements-commandp-test failed"))

(defun py-end-of-statements-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-statements-commandp-base arg teststring)))

(defun py-end-of-statements-commandp-base ()
  (assert (commandp 'py-end-of-statements) nil "py-end-of-statements-commandp-test failed"))

(defun py-statements-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statements-commandp-base arg teststring)))

(defun py-statements-commandp-base ()
  (assert (commandp 'py-statements) nil "py-statements-commandp-test failed"))

(defun py-kill-statements-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-statements-commandp-base arg teststring)))

(defun py-kill-statements-commandp-base ()
  (assert (commandp 'py-kill-statements) nil "py-kill-statements-commandp-test failed"))

(defun py-comment-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-comment-region-commandp-base arg teststring)))

(defun py-comment-region-commandp-base ()
  (assert (commandp 'py-comment-region) nil "py-comment-region-commandp-test failed"))

(defun py-fill-paragraph-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-fill-paragraph-commandp-base arg teststring)))

(defun py-fill-paragraph-commandp-base ()
  (assert (commandp 'py-fill-paragraph) nil "py-fill-paragraph-commandp-test failed"))

(defun py-insert-super-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-insert-super-commandp-base arg teststring)))

(defun py-insert-super-commandp-base ()
  (assert (commandp 'py-insert-super) nil "py-insert-super-commandp-test failed"))

(defun py-compute-indentation-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-compute-indentation-commandp-base arg teststring)))

(defun py-compute-indentation-commandp-base ()
  (assert (commandp 'py-compute-indentation) nil "py-compute-indentation-commandp-test failed"))

(defun py-continuation-offset-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-continuation-offset-commandp-base arg teststring)))

(defun py-continuation-offset-commandp-base ()
  (assert (commandp 'py-continuation-offset) nil "py-continuation-offset-commandp-test failed"))

(defun py-indentation-of-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-indentation-of-statement-commandp-base arg teststring)))

(defun py-indentation-of-statement-commandp-base ()
  (assert (commandp 'py-indentation-of-statement) nil "py-indentation-of-statement-commandp-test failed"))

(defun py-list-beginning-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-list-beginning-position-commandp-base arg teststring)))

(defun py-list-beginning-position-commandp-base ()
  (assert (commandp 'py-list-beginning-position) nil "py-list-beginning-position-commandp-test failed"))

(defun py-end-of-list-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-list-position-commandp-base arg teststring)))

(defun py-end-of-list-position-commandp-base ()
  (assert (commandp 'py-end-of-list-position) nil "py-end-of-list-position-commandp-test failed"))

(defun py-in-triplequoted-string-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-in-triplequoted-string-p-commandp-base arg teststring)))

(defun py-in-triplequoted-string-p-commandp-base ()
  (assert (commandp 'py-in-triplequoted-string-p) nil "py-in-triplequoted-string-p-commandp-test failed"))

(defun py-in-string-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-in-string-p-commandp-base arg teststring)))

(defun py-in-string-p-commandp-base ()
  (assert (commandp 'py-in-string-p) nil "py-in-string-p-commandp-test failed"))

(defun py-in-statement-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-in-statement-p-commandp-base arg teststring)))

(defun py-in-statement-p-commandp-base ()
  (assert (commandp 'py-in-statement-p) nil "py-in-statement-p-commandp-test failed"))

(defun py-beginning-of-paragraph-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-paragraph-p-commandp-base arg teststring)))

(defun py-beginning-of-paragraph-p-commandp-base ()
  (assert (commandp 'py-beginning-of-paragraph-p) nil "py-beginning-of-paragraph-p-commandp-test failed"))

(defun py-beginning-of-line-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-line-p-commandp-base arg teststring)))

(defun py-beginning-of-line-p-commandp-base ()
  (assert (commandp 'py-beginning-of-line-p) nil "py-beginning-of-line-p-commandp-test failed"))

(defun py-beginning-of-statement-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-statement-p-commandp-base arg teststring)))

(defun py-beginning-of-statement-p-commandp-base ()
  (assert (commandp 'py-beginning-of-statement-p) nil "py-beginning-of-statement-p-commandp-test failed"))

(defun py-beginning-of-expression-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-expression-p-commandp-base arg teststring)))

(defun py-beginning-of-expression-p-commandp-base ()
  (assert (commandp 'py-beginning-of-expression-p) nil "py-beginning-of-expression-p-commandp-test failed"))

(defun py-beginning-of-partial-expression-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-partial-expression-p-commandp-base arg teststring)))

(defun py-beginning-of-partial-expression-p-commandp-base ()
  (assert (commandp 'py-beginning-of-partial-expression-p) nil "py-beginning-of-partial-expression-p-commandp-test failed"))

(defun py-beginning-of-block-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-p-commandp-base arg teststring)))

(defun py-beginning-of-block-p-commandp-base ()
  (assert (commandp 'py-beginning-of-block-p) nil "py-beginning-of-block-p-commandp-test failed"))

(defun py-beginning-of-clause-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-clause-p-commandp-base arg teststring)))

(defun py-beginning-of-clause-p-commandp-base ()
  (assert (commandp 'py-beginning-of-clause-p) nil "py-beginning-of-clause-p-commandp-test failed"))

(defun py-beginning-of-block-or-clause-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-p-commandp-base arg teststring)))

(defun py-beginning-of-block-or-clause-p-commandp-base ()
  (assert (commandp 'py-beginning-of-block-or-clause-p) nil "py-beginning-of-block-or-clause-p-commandp-test failed"))

(defun py-beginning-of-def-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-p-commandp-base arg teststring)))

(defun py-beginning-of-def-p-commandp-base ()
  (assert (commandp 'py-beginning-of-def-p) nil "py-beginning-of-def-p-commandp-test failed"))

(defun py-beginning-of-class-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-class-p-commandp-base arg teststring)))

(defun py-beginning-of-class-p-commandp-base ()
  (assert (commandp 'py-beginning-of-class-p) nil "py-beginning-of-class-p-commandp-test failed"))

(defun py-beginning-of-def-or-class-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-p-commandp-base arg teststring)))

(defun py-beginning-of-def-or-class-p-commandp-base ()
  (assert (commandp 'py-beginning-of-def-or-class-p) nil "py-beginning-of-def-or-class-p-commandp-test failed"))

(defun py-statement-opens-block-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-block-p-commandp-base arg teststring)))

(defun py-statement-opens-block-p-commandp-base ()
  (assert (commandp 'py-statement-opens-block-p) nil "py-statement-opens-block-p-commandp-test failed"))

(defun py-statement-opens-clause-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-clause-p-commandp-base arg teststring)))

(defun py-statement-opens-clause-p-commandp-base ()
  (assert (commandp 'py-statement-opens-clause-p) nil "py-statement-opens-clause-p-commandp-test failed"))

(defun py-statement-opens-block-or-clause-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-block-or-clause-p-commandp-base arg teststring)))

(defun py-statement-opens-block-or-clause-p-commandp-base ()
  (assert (commandp 'py-statement-opens-block-or-clause-p) nil "py-statement-opens-block-or-clause-p-commandp-test failed"))

(defun py-statement-opens-class-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-class-p-commandp-base arg teststring)))

(defun py-statement-opens-class-p-commandp-base ()
  (assert (commandp 'py-statement-opens-class-p) nil "py-statement-opens-class-p-commandp-test failed"))

(defun py-statement-opens-def-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-def-p-commandp-base arg teststring)))

(defun py-statement-opens-def-p-commandp-base ()
  (assert (commandp 'py-statement-opens-def-p) nil "py-statement-opens-def-p-commandp-test failed"))

(defun py-statement-opens-def-or-class-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-opens-def-or-class-p-commandp-base arg teststring)))

(defun py-statement-opens-def-or-class-p-commandp-base ()
  (assert (commandp 'py-statement-opens-def-or-class-p) nil "py-statement-opens-def-or-class-p-commandp-test failed"))

(defun py-current-defun-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-current-defun-commandp-base arg teststring)))

(defun py-current-defun-commandp-base ()
  (assert (commandp 'py-current-defun) nil "py-current-defun-commandp-test failed"))

(defun py-sort-imports-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-sort-imports-commandp-base arg teststring)))

(defun py-sort-imports-commandp-base ()
  (assert (commandp 'py-sort-imports) nil "py-sort-imports-commandp-test failed"))

(defun empty-line-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'empty-line-p-commandp-base arg teststring)))

(defun empty-line-p-commandp-base ()
  (assert (commandp 'empty-line-p) nil "empty-line-p-commandp-test failed"))

(defun py-which-function-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-which-function-commandp-base arg teststring)))

(defun py-which-function-commandp-base ()
  (assert (commandp 'py-which-function) nil "py-which-function-commandp-test failed"))

(defun py-beginning-of-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-commandp-base arg teststring)))

(defun py-beginning-of-block-commandp-base ()
  (assert (commandp 'py-beginning-of-block) nil "py-beginning-of-block-commandp-test failed"))

(defun py-beginning-of-if-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-if-block-commandp-base arg teststring)))

(defun py-beginning-of-if-block-commandp-base ()
  (assert (commandp 'py-beginning-of-if-block) nil "py-beginning-of-if-block-commandp-test failed"))

(defun py-beginning-of-try-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-try-block-commandp-base arg teststring)))

(defun py-beginning-of-try-block-commandp-base ()
  (assert (commandp 'py-beginning-of-try-block) nil "py-beginning-of-try-block-commandp-test failed"))

(defun py-end-of-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-block-commandp-base arg teststring)))

(defun py-end-of-block-commandp-base ()
  (assert (commandp 'py-end-of-block) nil "py-end-of-block-commandp-test failed"))

(defun py-beginning-of-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-commandp-base arg teststring)))

(defun py-beginning-of-block-or-clause-commandp-base ()
  (assert (commandp 'py-beginning-of-block-or-clause) nil "py-beginning-of-block-or-clause-commandp-test failed"))

(defun py-end-of-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-block-or-clause-commandp-base arg teststring)))

(defun py-end-of-block-or-clause-commandp-base ()
  (assert (commandp 'py-end-of-block-or-clause) nil "py-end-of-block-or-clause-commandp-test failed"))

(defun py-beginning-of-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-class-commandp-base arg teststring)))

(defun py-beginning-of-class-commandp-base ()
  (assert (commandp 'py-beginning-of-class) nil "py-beginning-of-class-commandp-test failed"))

(defun py-end-of-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-class-commandp-base arg teststring)))

(defun py-end-of-class-commandp-base ()
  (assert (commandp 'py-end-of-class) nil "py-end-of-class-commandp-test failed"))

(defun py-beginning-of-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-clause-commandp-base arg teststring)))

(defun py-beginning-of-clause-commandp-base ()
  (assert (commandp 'py-beginning-of-clause) nil "py-beginning-of-clause-commandp-test failed"))

(defun py-end-of-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-clause-commandp-base arg teststring)))

(defun py-end-of-clause-commandp-base ()
  (assert (commandp 'py-end-of-clause) nil "py-end-of-clause-commandp-test failed"))

(defun py-beginning-of-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-commandp-base arg teststring)))

(defun py-beginning-of-def-commandp-base ()
  (assert (commandp 'py-beginning-of-def) nil "py-beginning-of-def-commandp-test failed"))

(defun py-end-of-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-def-commandp-base arg teststring)))

(defun py-end-of-def-commandp-base ()
  (assert (commandp 'py-end-of-def) nil "py-end-of-def-commandp-test failed"))

(defun py-beginning-of-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-commandp-base arg teststring)))

(defun py-beginning-of-def-or-class-commandp-base ()
  (assert (commandp 'py-beginning-of-def-or-class) nil "py-beginning-of-def-or-class-commandp-test failed"))

(defun py-end-of-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-def-or-class-commandp-base arg teststring)))

(defun py-end-of-def-or-class-commandp-base ()
  (assert (commandp 'py-end-of-def-or-class) nil "py-end-of-def-or-class-commandp-test failed"))

(defun py-beginning-of-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-expression-commandp-base arg teststring)))

(defun py-beginning-of-expression-commandp-base ()
  (assert (commandp 'py-beginning-of-expression) nil "py-beginning-of-expression-commandp-test failed"))

(defun py-end-of-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-expression-commandp-base arg teststring)))

(defun py-end-of-expression-commandp-base ()
  (assert (commandp 'py-end-of-expression) nil "py-end-of-expression-commandp-test failed"))

(defun py-beginning-of-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-partial-expression-commandp-base arg teststring)))

(defun py-beginning-of-partial-expression-commandp-base ()
  (assert (commandp 'py-beginning-of-partial-expression) nil "py-beginning-of-partial-expression-commandp-test failed"))

(defun py-end-of-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-partial-expression-commandp-base arg teststring)))

(defun py-end-of-partial-expression-commandp-base ()
  (assert (commandp 'py-end-of-partial-expression) nil "py-end-of-partial-expression-commandp-test failed"))

(defun py-beginning-of-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-statement-commandp-base arg teststring)))

(defun py-beginning-of-statement-commandp-base ()
  (assert (commandp 'py-beginning-of-statement) nil "py-beginning-of-statement-commandp-test failed"))

(defun py-end-of-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-statement-commandp-base arg teststring)))

(defun py-end-of-statement-commandp-base ()
  (assert (commandp 'py-end-of-statement) nil "py-end-of-statement-commandp-test failed"))

(defun py-goto-statement-below-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-goto-statement-below-commandp-base arg teststring)))

(defun py-goto-statement-below-commandp-base ()
  (assert (commandp 'py-goto-statement-below) nil "py-goto-statement-below-commandp-test failed"))

(defun py-mark-paragraph-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-paragraph-commandp-base arg teststring)))

(defun py-mark-paragraph-commandp-base ()
  (assert (commandp 'py-mark-paragraph) nil "py-mark-paragraph-commandp-test failed"))

(defun py-mark-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-block-commandp-base arg teststring)))

(defun py-mark-block-commandp-base ()
  (assert (commandp 'py-mark-block) nil "py-mark-block-commandp-test failed"))

(defun py-mark-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-clause-commandp-base arg teststring)))

(defun py-mark-clause-commandp-base ()
  (assert (commandp 'py-mark-clause) nil "py-mark-clause-commandp-test failed"))

(defun py-mark-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-block-or-clause-commandp-base arg teststring)))

(defun py-mark-block-or-clause-commandp-base ()
  (assert (commandp 'py-mark-block-or-clause) nil "py-mark-block-or-clause-commandp-test failed"))

(defun py-mark-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-def-commandp-base arg teststring)))

(defun py-mark-def-commandp-base ()
  (assert (commandp 'py-mark-def) nil "py-mark-def-commandp-test failed"))

(defun py-mark-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-class-commandp-base arg teststring)))

(defun py-mark-class-commandp-base ()
  (assert (commandp 'py-mark-class) nil "py-mark-class-commandp-test failed"))

(defun py-mark-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-def-or-class-commandp-base arg teststring)))

(defun py-mark-def-or-class-commandp-base ()
  (assert (commandp 'py-mark-def-or-class) nil "py-mark-def-or-class-commandp-test failed"))

(defun py-mark-line-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-line-commandp-base arg teststring)))

(defun py-mark-line-commandp-base ()
  (assert (commandp 'py-mark-line) nil "py-mark-line-commandp-test failed"))

(defun py-mark-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-statement-commandp-base arg teststring)))

(defun py-mark-statement-commandp-base ()
  (assert (commandp 'py-mark-statement) nil "py-mark-statement-commandp-test failed"))

(defun py-mark-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-expression-commandp-base arg teststring)))

(defun py-mark-expression-commandp-base ()
  (assert (commandp 'py-mark-expression) nil "py-mark-expression-commandp-test failed"))

(defun py-mark-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-mark-partial-expression-commandp-base arg teststring)))

(defun py-mark-partial-expression-commandp-base ()
  (assert (commandp 'py-mark-partial-expression) nil "py-mark-partial-expression-commandp-test failed"))

(defun py-beginning-of-decorator-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-decorator-commandp-base arg teststring)))

(defun py-beginning-of-decorator-commandp-base ()
  (assert (commandp 'py-beginning-of-decorator) nil "py-beginning-of-decorator-commandp-test failed"))

(defun py-end-of-decorator-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-end-of-decorator-commandp-base arg teststring)))

(defun py-end-of-decorator-commandp-base ()
  (assert (commandp 'py-end-of-decorator) nil "py-end-of-decorator-commandp-test failed"))

(defun py-copy-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-expression-commandp-base arg teststring)))

(defun py-copy-expression-commandp-base ()
  (assert (commandp 'py-copy-expression) nil "py-copy-expression-commandp-test failed"))

(defun py-copy-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-partial-expression-commandp-base arg teststring)))

(defun py-copy-partial-expression-commandp-base ()
  (assert (commandp 'py-copy-partial-expression) nil "py-copy-partial-expression-commandp-test failed"))

(defun py-copy-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-statement-commandp-base arg teststring)))

(defun py-copy-statement-commandp-base ()
  (assert (commandp 'py-copy-statement) nil "py-copy-statement-commandp-test failed"))

(defun py-copy-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-block-commandp-base arg teststring)))

(defun py-copy-block-commandp-base ()
  (assert (commandp 'py-copy-block) nil "py-copy-block-commandp-test failed"))

(defun py-copy-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-block-or-clause-commandp-base arg teststring)))

(defun py-copy-block-or-clause-commandp-base ()
  (assert (commandp 'py-copy-block-or-clause) nil "py-copy-block-or-clause-commandp-test failed"))

(defun py-copy-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-def-commandp-base arg teststring)))

(defun py-copy-def-commandp-base ()
  (assert (commandp 'py-copy-def) nil "py-copy-def-commandp-test failed"))

(defun py-copy-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-def-or-class-commandp-base arg teststring)))

(defun py-copy-def-or-class-commandp-base ()
  (assert (commandp 'py-copy-def-or-class) nil "py-copy-def-or-class-commandp-test failed"))

(defun py-copy-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-class-commandp-base arg teststring)))

(defun py-copy-class-commandp-base ()
  (assert (commandp 'py-copy-class) nil "py-copy-class-commandp-test failed"))

(defun py-copy-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-copy-clause-commandp-base arg teststring)))

(defun py-copy-clause-commandp-base ()
  (assert (commandp 'py-copy-clause) nil "py-copy-clause-commandp-test failed"))

(defun py-kill-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-expression-commandp-base arg teststring)))

(defun py-kill-expression-commandp-base ()
  (assert (commandp 'py-kill-expression) nil "py-kill-expression-commandp-test failed"))

(defun py-kill-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-partial-expression-commandp-base arg teststring)))

(defun py-kill-partial-expression-commandp-base ()
  (assert (commandp 'py-kill-partial-expression) nil "py-kill-partial-expression-commandp-test failed"))

(defun py-kill-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-statement-commandp-base arg teststring)))

(defun py-kill-statement-commandp-base ()
  (assert (commandp 'py-kill-statement) nil "py-kill-statement-commandp-test failed"))

(defun py-kill-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-block-commandp-base arg teststring)))

(defun py-kill-block-commandp-base ()
  (assert (commandp 'py-kill-block) nil "py-kill-block-commandp-test failed"))

(defun py-kill-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-block-or-clause-commandp-base arg teststring)))

(defun py-kill-block-or-clause-commandp-base ()
  (assert (commandp 'py-kill-block-or-clause) nil "py-kill-block-or-clause-commandp-test failed"))

(defun py-kill-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-def-or-class-commandp-base arg teststring)))

(defun py-kill-def-or-class-commandp-base ()
  (assert (commandp 'py-kill-def-or-class) nil "py-kill-def-or-class-commandp-test failed"))

(defun py-kill-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-class-commandp-base arg teststring)))

(defun py-kill-class-commandp-base ()
  (assert (commandp 'py-kill-class) nil "py-kill-class-commandp-test failed"))

(defun py-kill-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-def-commandp-base arg teststring)))

(defun py-kill-def-commandp-base ()
  (assert (commandp 'py-kill-def) nil "py-kill-def-commandp-test failed"))

(defun py-kill-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-kill-clause-commandp-base arg teststring)))

(defun py-kill-clause-commandp-base ()
  (assert (commandp 'py-kill-clause) nil "py-kill-clause-commandp-test failed"))

(defun py-forward-line-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-forward-line-commandp-base arg teststring)))

(defun py-forward-line-commandp-base ()
  (assert (commandp 'py-forward-line) nil "py-forward-line-commandp-test failed"))

(defun py-beginning-of-comment-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-comment-commandp-base arg teststring)))

(defun py-beginning-of-comment-commandp-base ()
  (assert (commandp 'py-beginning-of-comment) nil "py-beginning-of-comment-commandp-test failed"))

(defun py-leave-comment-or-string-backward-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-leave-comment-or-string-backward-commandp-base arg teststring)))

(defun py-leave-comment-or-string-backward-commandp-base ()
  (assert (commandp 'py-leave-comment-or-string-backward) nil "py-leave-comment-or-string-backward-commandp-test failed"))

(defun py-beginning-of-list-pps-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-beginning-of-list-pps-commandp-base arg teststring)))

(defun py-beginning-of-list-pps-commandp-base ()
  (assert (commandp 'py-beginning-of-list-pps) nil "py-beginning-of-list-pps-commandp-test failed"))

(defun py-down-block-bol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-block-bol-commandp-base arg teststring)))

(defun py-down-block-bol-commandp-base ()
  (assert (commandp 'py-down-block-bol) nil "py-down-block-bol-commandp-test failed"))

(defun py-down-clause-bol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-clause-bol-commandp-base arg teststring)))

(defun py-down-clause-bol-commandp-base ()
  (assert (commandp 'py-down-clause-bol) nil "py-down-clause-bol-commandp-test failed"))

(defun py-down-def-bol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-def-bol-commandp-base arg teststring)))

(defun py-down-def-bol-commandp-base ()
  (assert (commandp 'py-down-def-bol) nil "py-down-def-bol-commandp-test failed"))

(defun py-down-class-bol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-class-bol-commandp-base arg teststring)))

(defun py-down-class-bol-commandp-base ()
  (assert (commandp 'py-down-class-bol) nil "py-down-class-bol-commandp-test failed"))

(defun py-down-statement-bol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-statement-bol-commandp-base arg teststring)))

(defun py-down-statement-bol-commandp-base ()
  (assert (commandp 'py-down-statement-bol) nil "py-down-statement-bol-commandp-test failed"))

(defun py-down-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-statement-commandp-base arg teststring)))

(defun py-down-statement-commandp-base ()
  (assert (commandp 'py-down-statement) nil "py-down-statement-commandp-test failed"))

(defun py-down-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-block-commandp-base arg teststring)))

(defun py-down-block-commandp-base ()
  (assert (commandp 'py-down-block) nil "py-down-block-commandp-test failed"))

(defun py-down-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-clause-commandp-base arg teststring)))

(defun py-down-clause-commandp-base ()
  (assert (commandp 'py-down-clause) nil "py-down-clause-commandp-test failed"))

(defun py-down-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-block-or-clause-commandp-base arg teststring)))

(defun py-down-block-or-clause-commandp-base ()
  (assert (commandp 'py-down-block-or-clause) nil "py-down-block-or-clause-commandp-test failed"))

(defun py-down-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-def-commandp-base arg teststring)))

(defun py-down-def-commandp-base ()
  (assert (commandp 'py-down-def) nil "py-down-def-commandp-test failed"))

(defun py-down-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-class-commandp-base arg teststring)))

(defun py-down-class-commandp-base ()
  (assert (commandp 'py-down-class) nil "py-down-class-commandp-test failed"))

(defun py-down-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-def-or-class-commandp-base arg teststring)))

(defun py-down-def-or-class-commandp-base ()
  (assert (commandp 'py-down-def-or-class) nil "py-down-def-or-class-commandp-test failed"))

(defun py-forward-into-nomenclature-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-forward-into-nomenclature-commandp-base arg teststring)))

(defun py-forward-into-nomenclature-commandp-base ()
  (assert (commandp 'py-forward-into-nomenclature) nil "py-forward-into-nomenclature-commandp-test failed"))

(defun py-backward-into-nomenclature-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-backward-into-nomenclature-commandp-base arg teststring)))

(defun py-backward-into-nomenclature-commandp-base ()
  (assert (commandp 'py-backward-into-nomenclature) nil "py-backward-into-nomenclature-commandp-test failed"))

(defun match-paren-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'match-paren-commandp-base arg teststring)))

(defun match-paren-commandp-base ()
  (assert (commandp 'match-paren) nil "match-paren-commandp-test failed"))

(defun py-toggle-execute-keep-temporary-file-p-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-toggle-execute-keep-temporary-file-p-commandp-base arg teststring)))

(defun py-toggle-execute-keep-temporary-file-p-commandp-base ()
  (assert (commandp 'py-toggle-execute-keep-temporary-file-p) nil "py-toggle-execute-keep-temporary-file-p-commandp-test failed"))

(defun py-guess-default-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-guess-default-python-commandp-base arg teststring)))

(defun py-guess-default-python-commandp-base ()
  (assert (commandp 'py-guess-default-python) nil "py-guess-default-python-commandp-test failed"))

(defun py-set-shell-completion-environment-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-set-shell-completion-environment-commandp-base arg teststring)))

(defun py-set-shell-completion-environment-commandp-base ()
  (assert (commandp 'py-set-shell-completion-environment) nil "py-set-shell-completion-environment-commandp-test failed"))

(defun py-set-ipython-completion-command-string-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-set-ipython-completion-command-string-commandp-base arg teststring)))

(defun py-set-ipython-completion-command-string-commandp-base ()
  (assert (commandp 'py-set-ipython-completion-command-string) nil "py-set-ipython-completion-command-string-commandp-test failed"))

(defun py-shell-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-dedicated-commandp-base arg teststring)))

(defun py-shell-dedicated-commandp-base ()
  (assert (commandp 'py-shell-dedicated) nil "py-shell-dedicated-commandp-test failed"))

(defun py-shell-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-commandp-base arg teststring)))

(defun py-shell-commandp-base ()
  (assert (commandp 'py-shell) nil "py-shell-commandp-test failed"))

(defun python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-commandp-base arg teststring)))

(defun python-commandp-base ()
  (assert (commandp 'python) nil "python-commandp-test failed"))

(defun ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-commandp-base arg teststring)))

(defun ipython-commandp-base ()
  (assert (commandp 'ipython) nil "ipython-commandp-test failed"))

(defun python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3-commandp-base arg teststring)))

(defun python3-commandp-base ()
  (assert (commandp 'python3) nil "python3-commandp-test failed"))

(defun python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2-commandp-base arg teststring)))

(defun python2-commandp-base ()
  (assert (commandp 'python2) nil "python2-commandp-test failed"))

(defun python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2.7-commandp-base arg teststring)))

(defun python2.7-commandp-base ()
  (assert (commandp 'python2.7) nil "python2.7-commandp-test failed"))

(defun jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'jython-commandp-base arg teststring)))

(defun jython-commandp-base ()
  (assert (commandp 'jython) nil "jython-commandp-test failed"))

(defun python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3.2-commandp-base arg teststring)))

(defun python3.2-commandp-base ()
  (assert (commandp 'python3.2) nil "python3.2-commandp-test failed"))

(defun python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-dedicated-commandp-base arg teststring)))

(defun python-dedicated-commandp-base ()
  (assert (commandp 'python-dedicated) nil "python-dedicated-commandp-test failed"))

(defun ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-dedicated-commandp-base arg teststring)))

(defun ipython-dedicated-commandp-base ()
  (assert (commandp 'ipython-dedicated) nil "ipython-dedicated-commandp-test failed"))

(defun python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3-dedicated-commandp-base arg teststring)))

(defun python3-dedicated-commandp-base ()
  (assert (commandp 'python3-dedicated) nil "python3-dedicated-commandp-test failed"))

(defun python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2-dedicated-commandp-base arg teststring)))

(defun python2-dedicated-commandp-base ()
  (assert (commandp 'python2-dedicated) nil "python2-dedicated-commandp-test failed"))

(defun python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2.7-dedicated-commandp-base arg teststring)))

(defun python2.7-dedicated-commandp-base ()
  (assert (commandp 'python2.7-dedicated) nil "python2.7-dedicated-commandp-test failed"))

(defun jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'jython-dedicated-commandp-base arg teststring)))

(defun jython-dedicated-commandp-base ()
  (assert (commandp 'jython-dedicated) nil "jython-dedicated-commandp-test failed"))

(defun python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3.2-dedicated-commandp-base arg teststring)))

(defun python3.2-dedicated-commandp-base ()
  (assert (commandp 'python3.2-dedicated) nil "python3.2-dedicated-commandp-test failed"))

(defun python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-switch-commandp-base arg teststring)))

(defun python-switch-commandp-base ()
  (assert (commandp 'python-switch) nil "python-switch-commandp-test failed"))

(defun ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-switch-commandp-base arg teststring)))

(defun ipython-switch-commandp-base ()
  (assert (commandp 'ipython-switch) nil "ipython-switch-commandp-test failed"))

(defun python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3-switch-commandp-base arg teststring)))

(defun python3-switch-commandp-base ()
  (assert (commandp 'python3-switch) nil "python3-switch-commandp-test failed"))

(defun python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2-switch-commandp-base arg teststring)))

(defun python2-switch-commandp-base ()
  (assert (commandp 'python2-switch) nil "python2-switch-commandp-test failed"))

(defun python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2.7-switch-commandp-base arg teststring)))

(defun python2.7-switch-commandp-base ()
  (assert (commandp 'python2.7-switch) nil "python2.7-switch-commandp-test failed"))

(defun jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'jython-switch-commandp-base arg teststring)))

(defun jython-switch-commandp-base ()
  (assert (commandp 'jython-switch) nil "jython-switch-commandp-test failed"))

(defun python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3.2-switch-commandp-base arg teststring)))

(defun python3.2-switch-commandp-base ()
  (assert (commandp 'python3.2-switch) nil "python3.2-switch-commandp-test failed"))

(defun python-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-no-switch-commandp-base arg teststring)))

(defun python-no-switch-commandp-base ()
  (assert (commandp 'python-no-switch) nil "python-no-switch-commandp-test failed"))

(defun ipython-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-no-switch-commandp-base arg teststring)))

(defun ipython-no-switch-commandp-base ()
  (assert (commandp 'ipython-no-switch) nil "ipython-no-switch-commandp-test failed"))

(defun python3-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3-no-switch-commandp-base arg teststring)))

(defun python3-no-switch-commandp-base ()
  (assert (commandp 'python3-no-switch) nil "python3-no-switch-commandp-test failed"))

(defun python2-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2-no-switch-commandp-base arg teststring)))

(defun python2-no-switch-commandp-base ()
  (assert (commandp 'python2-no-switch) nil "python2-no-switch-commandp-test failed"))

(defun python2.7-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2.7-no-switch-commandp-base arg teststring)))

(defun python2.7-no-switch-commandp-base ()
  (assert (commandp 'python2.7-no-switch) nil "python2.7-no-switch-commandp-test failed"))

(defun jython-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'jython-no-switch-commandp-base arg teststring)))

(defun jython-no-switch-commandp-base ()
  (assert (commandp 'jython-no-switch) nil "jython-no-switch-commandp-test failed"))

(defun python3.2-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3.2-no-switch-commandp-base arg teststring)))

(defun python3.2-no-switch-commandp-base ()
  (assert (commandp 'python3.2-no-switch) nil "python3.2-no-switch-commandp-test failed"))

(defun python-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-switch-dedicated-commandp-base arg teststring)))

(defun python-switch-dedicated-commandp-base ()
  (assert (commandp 'python-switch-dedicated) nil "python-switch-dedicated-commandp-test failed"))

(defun ipython-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-switch-dedicated-commandp-base arg teststring)))

(defun ipython-switch-dedicated-commandp-base ()
  (assert (commandp 'ipython-switch-dedicated) nil "ipython-switch-dedicated-commandp-test failed"))

(defun python3-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3-switch-dedicated-commandp-base arg teststring)))

(defun python3-switch-dedicated-commandp-base ()
  (assert (commandp 'python3-switch-dedicated) nil "python3-switch-dedicated-commandp-test failed"))

(defun python2-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2-switch-dedicated-commandp-base arg teststring)))

(defun python2-switch-dedicated-commandp-base ()
  (assert (commandp 'python2-switch-dedicated) nil "python2-switch-dedicated-commandp-test failed"))

(defun python2.7-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python2.7-switch-dedicated-commandp-base arg teststring)))

(defun python2.7-switch-dedicated-commandp-base ()
  (assert (commandp 'python2.7-switch-dedicated) nil "python2.7-switch-dedicated-commandp-test failed"))

(defun jython-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'jython-switch-dedicated-commandp-base arg teststring)))

(defun jython-switch-dedicated-commandp-base ()
  (assert (commandp 'jython-switch-dedicated) nil "jython-switch-dedicated-commandp-test failed"))

(defun python3.2-switch-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python3.2-switch-dedicated-commandp-base arg teststring)))

(defun python3.2-switch-dedicated-commandp-base ()
  (assert (commandp 'python3.2-switch-dedicated) nil "python3.2-switch-dedicated-commandp-test failed"))

(defun py-which-execute-file-command-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-which-execute-file-command-commandp-base arg teststring)))

(defun py-which-execute-file-command-commandp-base ()
  (assert (commandp 'py-which-execute-file-command) nil "py-which-execute-file-command-commandp-test failed"))

(defun py-execute-region-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-no-switch-commandp-base arg teststring)))

(defun py-execute-region-no-switch-commandp-base ()
  (assert (commandp 'py-execute-region-no-switch) nil "py-execute-region-no-switch-commandp-test failed"))

(defun py-execute-region-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-switch-commandp-base arg teststring)))

(defun py-execute-region-switch-commandp-base ()
  (assert (commandp 'py-execute-region-switch) nil "py-execute-region-switch-commandp-test failed"))

(defun py-execute-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-commandp-base arg teststring)))

(defun py-execute-region-commandp-base ()
  (assert (commandp 'py-execute-region) nil "py-execute-region-commandp-test failed"))

(defun py-execute-region-default-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-default-commandp-base arg teststring)))

(defun py-execute-region-default-commandp-base ()
  (assert (commandp 'py-execute-region-default) nil "py-execute-region-default-commandp-test failed"))

(defun py-execute-region-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-dedicated-commandp-base arg teststring)))

(defun py-execute-region-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-dedicated) nil "py-execute-region-dedicated-commandp-test failed"))

(defun py-execute-region-default-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-default-dedicated-commandp-base arg teststring)))

(defun py-execute-region-default-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-default-dedicated) nil "py-execute-region-default-dedicated-commandp-test failed"))

(defun py-execute-string-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-string-commandp-base arg teststring)))

(defun py-execute-string-commandp-base ()
  (assert (commandp 'py-execute-string) nil "py-execute-string-commandp-test failed"))

(defun py-execute-string-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-string-dedicated-commandp-base arg teststring)))

(defun py-execute-string-dedicated-commandp-base ()
  (assert (commandp 'py-execute-string-dedicated) nil "py-execute-string-dedicated-commandp-test failed"))

(defun py-shell-command-on-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-command-on-region-commandp-base arg teststring)))

(defun py-shell-command-on-region-commandp-base ()
  (assert (commandp 'py-shell-command-on-region) nil "py-shell-command-on-region-commandp-test failed"))

(defun py-ipython-shell-command-on-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-ipython-shell-command-on-region-commandp-base arg teststring)))

(defun py-ipython-shell-command-on-region-commandp-base ()
  (assert (commandp 'py-ipython-shell-command-on-region) nil "py-ipython-shell-command-on-region-commandp-test failed"))

(defun py-send-region-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-send-region-ipython-commandp-base arg teststring)))

(defun py-send-region-ipython-commandp-base ()
  (assert (commandp 'py-send-region-ipython) nil "py-send-region-ipython-commandp-test failed"))

(defun ipython-send-and-indent-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-send-and-indent-commandp-base arg teststring)))

(defun ipython-send-and-indent-commandp-base ()
  (assert (commandp 'ipython-send-and-indent) nil "ipython-send-and-indent-commandp-test failed"))

(defun py-execute-region-in-shell-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-in-shell-commandp-base arg teststring)))

(defun py-execute-region-in-shell-commandp-base ()
  (assert (commandp 'py-execute-region-in-shell) nil "py-execute-region-in-shell-commandp-test failed"))

(defun py-fetch-py-master-file-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-fetch-py-master-file-commandp-base arg teststring)))

(defun py-fetch-py-master-file-commandp-base ()
  (assert (commandp 'py-fetch-py-master-file) nil "py-fetch-py-master-file-commandp-test failed"))

(defun py-execute-import-or-reload-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-import-or-reload-commandp-base arg teststring)))

(defun py-execute-import-or-reload-commandp-base ()
  (assert (commandp 'py-execute-import-or-reload) nil "py-execute-import-or-reload-commandp-test failed"))

(defun py-execute-buffer-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-dedicated) nil "py-execute-buffer-dedicated-commandp-test failed"))

(defun py-execute-buffer-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-switch-commandp-base arg teststring)))

(defun py-execute-buffer-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-switch) nil "py-execute-buffer-switch-commandp-test failed"))

(defun py-execute-buffer-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-dedicated-switch) nil "py-execute-buffer-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-commandp-base arg teststring)))

(defun py-execute-buffer-commandp-base ()
  (assert (commandp 'py-execute-buffer) nil "py-execute-buffer-commandp-test failed"))

(defun py-execute-buffer-no-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-no-switch-commandp-base arg teststring)))

(defun py-execute-buffer-no-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-no-switch) nil "py-execute-buffer-no-switch-commandp-test failed"))

(defun py-execute-defun-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-defun-commandp-base arg teststring)))

(defun py-execute-defun-commandp-base ()
  (assert (commandp 'py-execute-defun) nil "py-execute-defun-commandp-test failed"))

(defun py-process-file-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-process-file-commandp-base arg teststring)))

(defun py-process-file-commandp-base ()
  (assert (commandp 'py-process-file) nil "py-process-file-commandp-test failed"))

(defun py-exec-execfile-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-exec-execfile-region-commandp-base arg teststring)))

(defun py-exec-execfile-region-commandp-base ()
  (assert (commandp 'py-exec-execfile-region) nil "py-exec-execfile-region-commandp-test failed"))

(defun py-exec-execfile-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-exec-execfile-commandp-base arg teststring)))

(defun py-exec-execfile-commandp-base ()
  (assert (commandp 'py-exec-execfile) nil "py-exec-execfile-commandp-test failed"))

(defun py-execute-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-commandp-base arg teststring)))

(defun py-execute-block-commandp-base ()
  (assert (commandp 'py-execute-block) nil "py-execute-block-commandp-test failed"))

(defun py-execute-block-or-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-commandp-base arg teststring)))

(defun py-execute-block-or-clause-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause) nil "py-execute-block-or-clause-commandp-test failed"))

(defun py-execute-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-commandp-base arg teststring)))

(defun py-execute-class-commandp-base ()
  (assert (commandp 'py-execute-class) nil "py-execute-class-commandp-test failed"))

(defun py-execute-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-commandp-base arg teststring)))

(defun py-execute-def-commandp-base ()
  (assert (commandp 'py-execute-def) nil "py-execute-def-commandp-test failed"))

(defun py-execute-def-or-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-or-class-commandp-base arg teststring)))

(defun py-execute-def-or-class-commandp-base ()
  (assert (commandp 'py-execute-def-or-class) nil "py-execute-def-or-class-commandp-test failed"))

(defun py-execute-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-commandp-base arg teststring)))

(defun py-execute-expression-commandp-base ()
  (assert (commandp 'py-execute-expression) nil "py-execute-expression-commandp-test failed"))

(defun py-execute-partial-expression-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-commandp-base arg teststring)))

(defun py-execute-partial-expression-commandp-base ()
  (assert (commandp 'py-execute-partial-expression) nil "py-execute-partial-expression-commandp-test failed"))

(defun py-execute-statement-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-commandp-base arg teststring)))

(defun py-execute-statement-commandp-base ()
  (assert (commandp 'py-execute-statement) nil "py-execute-statement-commandp-test failed"))

(defun py-execute-file-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-file-commandp-base arg teststring)))

(defun py-execute-file-commandp-base ()
  (assert (commandp 'py-execute-file) nil "py-execute-file-commandp-test failed"))

(defun py-down-exception-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-down-exception-commandp-base arg teststring)))

(defun py-down-exception-commandp-base ()
  (assert (commandp 'py-down-exception) nil "py-down-exception-commandp-test failed"))

(defun py-up-exception-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-up-exception-commandp-base arg teststring)))

(defun py-up-exception-commandp-base ()
  (assert (commandp 'py-up-exception) nil "py-up-exception-commandp-test failed"))

(defun py-output-buffer-filter-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-output-buffer-filter-commandp-base arg teststring)))

(defun py-output-buffer-filter-commandp-base ()
  (assert (commandp 'py-output-buffer-filter) nil "py-output-buffer-filter-commandp-test failed"))

(defun py-send-string-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-send-string-commandp-base arg teststring)))

(defun py-send-string-commandp-base ()
  (assert (commandp 'py-send-string) nil "py-send-string-commandp-test failed"))

(defun py-pdbtrack-toggle-stack-tracking-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pdbtrack-toggle-stack-tracking-commandp-base arg teststring)))

(defun py-pdbtrack-toggle-stack-tracking-commandp-base ()
  (assert (commandp 'py-pdbtrack-toggle-stack-tracking) nil "py-pdbtrack-toggle-stack-tracking-commandp-test failed"))

(defun turn-on-pdbtrack-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'turn-on-pdbtrack-commandp-base arg teststring)))

(defun turn-on-pdbtrack-commandp-base ()
  (assert (commandp 'turn-on-pdbtrack) nil "turn-on-pdbtrack-commandp-test failed"))

(defun turn-off-pdbtrack-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'turn-off-pdbtrack-commandp-base arg teststring)))

(defun turn-off-pdbtrack-commandp-base ()
  (assert (commandp 'turn-off-pdbtrack) nil "turn-off-pdbtrack-commandp-test failed"))

(defun py-fetch-docu-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-fetch-docu-commandp-base arg teststring)))

(defun py-fetch-docu-commandp-base ()
  (assert (commandp 'py-fetch-docu) nil "py-fetch-docu-commandp-test failed"))

(defun py-find-imports-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-find-imports-commandp-base arg teststring)))

(defun py-find-imports-commandp-base ()
  (assert (commandp 'py-find-imports) nil "py-find-imports-commandp-test failed"))

(defun python-find-imports-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-find-imports-commandp-base arg teststring)))

(defun python-find-imports-commandp-base ()
  (assert (commandp 'python-find-imports) nil "python-find-imports-commandp-test failed"))

(defun py-describe-symbol-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-describe-symbol-commandp-base arg teststring)))

(defun py-describe-symbol-commandp-base ()
  (assert (commandp 'py-describe-symbol) nil "py-describe-symbol-commandp-test failed"))

(defun py-describe-mode-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-describe-mode-commandp-base arg teststring)))

(defun py-describe-mode-commandp-base ()
  (assert (commandp 'py-describe-mode) nil "py-describe-mode-commandp-test failed"))

(defun py-find-function-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-find-function-commandp-base arg teststring)))

(defun py-find-function-commandp-base ()
  (assert (commandp 'py-find-function) nil "py-find-function-commandp-test failed"))

(defun py-update-imports-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-update-imports-commandp-base arg teststring)))

(defun py-update-imports-commandp-base ()
  (assert (commandp 'py-update-imports) nil "py-update-imports-commandp-test failed"))

(defun py-indent-forward-line-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-indent-forward-line-commandp-base arg teststring)))

(defun py-indent-forward-line-commandp-base ()
  (assert (commandp 'py-indent-forward-line) nil "py-indent-forward-line-commandp-test failed"))

(defun py-dedent-forward-line-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-dedent-forward-line-commandp-base arg teststring)))

(defun py-dedent-forward-line-commandp-base ()
  (assert (commandp 'py-dedent-forward-line) nil "py-dedent-forward-line-commandp-test failed"))

(defun py-dedent-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-dedent-commandp-base arg teststring)))

(defun py-dedent-commandp-base ()
  (assert (commandp 'py-dedent) nil "py-dedent-commandp-test failed"))

(defun py-close-def-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-close-def-commandp-base arg teststring)))

(defun py-close-def-commandp-base ()
  (assert (commandp 'py-close-def) nil "py-close-def-commandp-test failed"))

(defun py-close-class-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-close-class-commandp-base arg teststring)))

(defun py-close-class-commandp-base ()
  (assert (commandp 'py-close-class) nil "py-close-class-commandp-test failed"))

(defun py-close-clause-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-close-clause-commandp-base arg teststring)))

(defun py-close-clause-commandp-base ()
  (assert (commandp 'py-close-clause) nil "py-close-clause-commandp-test failed"))

(defun py-close-block-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-close-block-commandp-base arg teststring)))

(defun py-close-block-commandp-base ()
  (assert (commandp 'py-close-block) nil "py-close-block-commandp-test failed"))

(defun py-class-at-point-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-class-at-point-commandp-base arg teststring)))

(defun py-class-at-point-commandp-base ()
  (assert (commandp 'py-class-at-point) nil "py-class-at-point-commandp-test failed"))

(defun py-match-paren-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-match-paren-commandp-base arg teststring)))

(defun py-match-paren-commandp-base ()
  (assert (commandp 'py-match-paren) nil "py-match-paren-commandp-test failed"))

(defun eva-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'eva-commandp-base arg teststring)))

(defun eva-commandp-base ()
  (assert (commandp 'eva) nil "eva-commandp-test failed"))

(defun pst-here-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'pst-here-commandp-base arg teststring)))

(defun pst-here-commandp-base ()
  (assert (commandp 'pst-here) nil "pst-here-commandp-test failed"))

(defun py-printform-insert-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-printform-insert-commandp-base arg teststring)))

(defun py-printform-insert-commandp-base ()
  (assert (commandp 'py-printform-insert) nil "py-printform-insert-commandp-test failed"))

(defun py-line-to-printform-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-line-to-printform-python2-commandp-base arg teststring)))

(defun py-line-to-printform-python2-commandp-base ()
  (assert (commandp 'py-line-to-printform-python2) nil "py-line-to-printform-python2-commandp-test failed"))

(defun py-switch-imenu-index-function-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-switch-imenu-index-function-commandp-base arg teststring)))

(defun py-switch-imenu-index-function-commandp-base ()
  (assert (commandp 'py-switch-imenu-index-function) nil "py-switch-imenu-index-function-commandp-test failed"))

(defun py-completion-at-point-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-completion-at-point-commandp-base arg teststring)))

(defun py-completion-at-point-commandp-base ()
  (assert (commandp 'py-completion-at-point) nil "py-completion-at-point-commandp-test failed"))

(defun py-choose-shell-by-shebang-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-choose-shell-by-shebang-commandp-base arg teststring)))

(defun py-choose-shell-by-shebang-commandp-base ()
  (assert (commandp 'py-choose-shell-by-shebang) nil "py-choose-shell-by-shebang-commandp-test failed"))

(defun py-which-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-which-python-commandp-base arg teststring)))

(defun py-which-python-commandp-base ()
  (assert (commandp 'py-which-python) nil "py-which-python-commandp-test failed"))

(defun py-python-current-environment-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-python-current-environment-commandp-base arg teststring)))

(defun py-python-current-environment-commandp-base ()
  (assert (commandp 'py-python-current-environment) nil "py-python-current-environment-commandp-test failed"))

(defun py-switch-shell-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-switch-shell-commandp-base arg teststring)))

(defun py-switch-shell-commandp-base ()
  (assert (commandp 'py-switch-shell) nil "py-switch-shell-commandp-test failed"))

(defun py-choose-shell-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-choose-shell-commandp-base arg teststring)))

(defun py-choose-shell-commandp-base ()
  (assert (commandp 'py-choose-shell) nil "py-choose-shell-commandp-test failed"))

(defun py-toggle-smart-indentation-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-toggle-smart-indentation-commandp-base arg teststring)))

(defun py-toggle-smart-indentation-commandp-base ()
  (assert (commandp 'py-toggle-smart-indentation) nil "py-toggle-smart-indentation-commandp-test failed"))

(defun py-smart-indentation-on-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-smart-indentation-on-commandp-base arg teststring)))

(defun py-smart-indentation-on-commandp-base ()
  (assert (commandp 'py-smart-indentation-on) nil "py-smart-indentation-on-commandp-test failed"))

(defun py-smart-indentation-off-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-smart-indentation-off-commandp-base arg teststring)))

(defun py-smart-indentation-off-commandp-base ()
  (assert (commandp 'py-smart-indentation-off) nil "py-smart-indentation-off-commandp-test failed"))

(defun py-toggle-split-windows-on-execute-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-toggle-split-windows-on-execute-commandp-base arg teststring)))

(defun py-toggle-split-windows-on-execute-commandp-base ()
  (assert (commandp 'py-toggle-split-windows-on-execute) nil "py-toggle-split-windows-on-execute-commandp-test failed"))

(defun py-split-windows-on-execute-on-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-split-windows-on-execute-on-commandp-base arg teststring)))

(defun py-split-windows-on-execute-on-commandp-base ()
  (assert (commandp 'py-split-windows-on-execute-on) nil "py-split-windows-on-execute-on-commandp-test failed"))

(defun py-split-windows-on-execute-off-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-split-windows-on-execute-off-commandp-base arg teststring)))

(defun py-split-windows-on-execute-off-commandp-base ()
  (assert (commandp 'py-split-windows-on-execute-off) nil "py-split-windows-on-execute-off-commandp-test failed"))

(defun py-toggle-shell-switch-buffers-on-execute-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-toggle-shell-switch-buffers-on-execute-commandp-base arg teststring)))

(defun py-toggle-shell-switch-buffers-on-execute-commandp-base ()
  (assert (commandp 'py-toggle-shell-switch-buffers-on-execute) nil "py-toggle-shell-switch-buffers-on-execute-commandp-test failed"))

(defun py-shell-switch-buffers-on-execute-on-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-switch-buffers-on-execute-on-commandp-base arg teststring)))

(defun py-shell-switch-buffers-on-execute-on-commandp-base ()
  (assert (commandp 'py-shell-switch-buffers-on-execute-on) nil "py-shell-switch-buffers-on-execute-on-commandp-test failed"))

(defun py-shell-switch-buffers-on-execute-off-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-switch-buffers-on-execute-off-commandp-base arg teststring)))

(defun py-shell-switch-buffers-on-execute-off-commandp-base ()
  (assert (commandp 'py-shell-switch-buffers-on-execute-off) nil "py-shell-switch-buffers-on-execute-off-commandp-test failed"))

(defun py-install-directory-check-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-install-directory-check-commandp-base arg teststring)))

(defun py-install-directory-check-commandp-base ()
  (assert (commandp 'py-install-directory-check) nil "py-install-directory-check-commandp-test failed"))

(defun py-load-pymacs-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-load-pymacs-commandp-base arg teststring)))

(defun py-load-pymacs-commandp-base ()
  (assert (commandp 'py-load-pymacs) nil "py-load-pymacs-commandp-test failed"))

(defun py-guess-py-install-directory-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-guess-py-install-directory-commandp-base arg teststring)))

(defun py-guess-py-install-directory-commandp-base ()
  (assert (commandp 'py-guess-py-install-directory) nil "py-guess-py-install-directory-commandp-test failed"))

(defun py-set-load-path-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-set-load-path-commandp-base arg teststring)))

(defun py-set-load-path-commandp-base ()
  (assert (commandp 'py-set-load-path) nil "py-set-load-path-commandp-test failed"))

(defun py-def-or-class-beginning-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-def-or-class-beginning-position-commandp-base arg teststring)))

(defun py-def-or-class-beginning-position-commandp-base ()
  (assert (commandp 'py-def-or-class-beginning-position) nil "py-def-or-class-beginning-position-commandp-test failed"))

(defun py-def-or-class-end-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-def-or-class-end-position-commandp-base arg teststring)))

(defun py-def-or-class-end-position-commandp-base ()
  (assert (commandp 'py-def-or-class-end-position) nil "py-def-or-class-end-position-commandp-test failed"))

(defun py-statement-beginning-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-beginning-position-commandp-base arg teststring)))

(defun py-statement-beginning-position-commandp-base ()
  (assert (commandp 'py-statement-beginning-position) nil "py-statement-beginning-position-commandp-test failed"))

(defun py-statement-end-position-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-statement-end-position-commandp-base arg teststring)))

(defun py-statement-end-position-commandp-base ()
  (assert (commandp 'py-statement-end-position) nil "py-statement-end-position-commandp-test failed"))

(defun py-current-indentation-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-current-indentation-commandp-base arg teststring)))

(defun py-current-indentation-commandp-base ()
  (assert (commandp 'py-current-indentation) nil "py-current-indentation-commandp-test failed"))

(defun py-version-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-version-commandp-base arg teststring)))

(defun py-version-commandp-base ()
  (assert (commandp 'py-version) nil "py-version-commandp-test failed"))

(defun run-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'run-python-commandp-base arg teststring)))

(defun run-python-commandp-base ()
  (assert (commandp 'run-python) nil "run-python-commandp-test failed"))

(defun py-send-region-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-send-region-commandp-base arg teststring)))

(defun py-send-region-commandp-base ()
  (assert (commandp 'py-send-region) nil "py-send-region-commandp-test failed"))

(defun py-send-buffer-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-send-buffer-commandp-base arg teststring)))

(defun py-send-buffer-commandp-base ()
  (assert (commandp 'py-send-buffer) nil "py-send-buffer-commandp-test failed"))

(defun py-switch-to-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-switch-to-python-commandp-base arg teststring)))

(defun py-switch-to-python-commandp-base ()
  (assert (commandp 'py-switch-to-python) nil "py-switch-to-python-commandp-test failed"))

(defun py-send-region-and-go-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-send-region-and-go-commandp-base arg teststring)))

(defun py-send-region-and-go-commandp-base ()
  (assert (commandp 'py-send-region-and-go) nil "py-send-region-and-go-commandp-test failed"))

(defun py-load-file-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-load-file-commandp-base arg teststring)))

(defun py-load-file-commandp-base ()
  (assert (commandp 'py-load-file) nil "py-load-file-commandp-test failed"))

(defun py-set-proc-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-set-proc-commandp-base arg teststring)))

(defun py-set-proc-commandp-base ()
  (assert (commandp 'py-set-proc) nil "py-set-proc-commandp-test failed"))

(defun python-send-string-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-send-string-commandp-base arg teststring)))

(defun python-send-string-commandp-base ()
  (assert (commandp 'python-send-string) nil "python-send-string-commandp-test failed"))

(defun py-shell-complete-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-shell-complete-commandp-base arg teststring)))

(defun py-shell-complete-commandp-base ()
  (assert (commandp 'py-shell-complete) nil "py-shell-complete-commandp-test failed"))

(defun ipython-complete-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'ipython-complete-commandp-base arg teststring)))

(defun ipython-complete-commandp-base ()
  (assert (commandp 'ipython-complete) nil "ipython-complete-commandp-test failed"))

(defun py-pychecker-run-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-pychecker-run-commandp-base arg teststring)))

(defun py-pychecker-run-commandp-base ()
  (assert (commandp 'py-pychecker-run) nil "py-pychecker-run-commandp-test failed"))

(defun virtualenv-current-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'virtualenv-current-commandp-base arg teststring)))

(defun virtualenv-current-commandp-base ()
  (assert (commandp 'virtualenv-current) nil "virtualenv-current-commandp-test failed"))

(defun virtualenv-activate-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'virtualenv-activate-commandp-base arg teststring)))

(defun virtualenv-activate-commandp-base ()
  (assert (commandp 'virtualenv-activate) nil "virtualenv-activate-commandp-test failed"))

(defun virtualenv-deactivate-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'virtualenv-deactivate-commandp-base arg teststring)))

(defun virtualenv-deactivate-commandp-base ()
  (assert (commandp 'virtualenv-deactivate) nil "virtualenv-deactivate-commandp-test failed"))

(defun virtualenv-workon-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'virtualenv-workon-commandp-base arg teststring)))

(defun virtualenv-workon-commandp-base ()
  (assert (commandp 'virtualenv-workon) nil "virtualenv-workon-commandp-test failed"))

(defun py-toggle-local-default-use-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-toggle-local-default-use-commandp-base arg teststring)))

(defun py-toggle-local-default-use-commandp-base ()
  (assert (commandp 'py-toggle-local-default-use) nil "py-toggle-local-default-use-commandp-test failed"))

(defun py-execute-statement-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python-commandp-base arg teststring)))

(defun py-execute-statement-python-commandp-base ()
  (assert (commandp 'py-execute-statement-python) nil "py-execute-statement-python-commandp-test failed"))

(defun py-execute-statement-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python-switch-commandp-base arg teststring)))

(defun py-execute-statement-python-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python-switch) nil "py-execute-statement-python-switch-commandp-test failed"))

(defun py-execute-statement-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-python-noswitch) nil "py-execute-statement-python-noswitch-commandp-test failed"))

(defun py-execute-statement-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-python-dedicated) nil "py-execute-statement-python-dedicated-commandp-test failed"))

(defun py-execute-statement-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python-dedicated-switch) nil "py-execute-statement-python-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-ipython-commandp-base arg teststring)))

(defun py-execute-statement-ipython-commandp-base ()
  (assert (commandp 'py-execute-statement-ipython) nil "py-execute-statement-ipython-commandp-test failed"))

(defun py-execute-statement-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-ipython-switch-commandp-base arg teststring)))

(defun py-execute-statement-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-ipython-switch) nil "py-execute-statement-ipython-switch-commandp-test failed"))

(defun py-execute-statement-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-ipython-noswitch) nil "py-execute-statement-ipython-noswitch-commandp-test failed"))

(defun py-execute-statement-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-ipython-dedicated) nil "py-execute-statement-ipython-dedicated-commandp-test failed"))

(defun py-execute-statement-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-ipython-dedicated-switch) nil "py-execute-statement-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3-commandp-base arg teststring)))

(defun py-execute-statement-python3-commandp-base ()
  (assert (commandp 'py-execute-statement-python3) nil "py-execute-statement-python3-commandp-test failed"))

(defun py-execute-statement-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3-switch-commandp-base arg teststring)))

(defun py-execute-statement-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3-switch) nil "py-execute-statement-python3-switch-commandp-test failed"))

(defun py-execute-statement-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3-noswitch) nil "py-execute-statement-python3-noswitch-commandp-test failed"))

(defun py-execute-statement-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-python3-dedicated) nil "py-execute-statement-python3-dedicated-commandp-test failed"))

(defun py-execute-statement-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3-dedicated-switch) nil "py-execute-statement-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2-commandp-base arg teststring)))

(defun py-execute-statement-python2-commandp-base ()
  (assert (commandp 'py-execute-statement-python2) nil "py-execute-statement-python2-commandp-test failed"))

(defun py-execute-statement-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2-switch-commandp-base arg teststring)))

(defun py-execute-statement-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2-switch) nil "py-execute-statement-python2-switch-commandp-test failed"))

(defun py-execute-statement-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2-noswitch) nil "py-execute-statement-python2-noswitch-commandp-test failed"))

(defun py-execute-statement-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-python2-dedicated) nil "py-execute-statement-python2-dedicated-commandp-test failed"))

(defun py-execute-statement-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2-dedicated-switch) nil "py-execute-statement-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2.7-commandp-base arg teststring)))

(defun py-execute-statement-python2.7-commandp-base ()
  (assert (commandp 'py-execute-statement-python2.7) nil "py-execute-statement-python2.7-commandp-test failed"))

(defun py-execute-statement-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-statement-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2.7-switch) nil "py-execute-statement-python2.7-switch-commandp-test failed"))

(defun py-execute-statement-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2.7-noswitch) nil "py-execute-statement-python2.7-noswitch-commandp-test failed"))

(defun py-execute-statement-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-python2.7-dedicated) nil "py-execute-statement-python2.7-dedicated-commandp-test failed"))

(defun py-execute-statement-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python2.7-dedicated-switch) nil "py-execute-statement-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-jython-commandp-base arg teststring)))

(defun py-execute-statement-jython-commandp-base ()
  (assert (commandp 'py-execute-statement-jython) nil "py-execute-statement-jython-commandp-test failed"))

(defun py-execute-statement-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-jython-switch-commandp-base arg teststring)))

(defun py-execute-statement-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-jython-switch) nil "py-execute-statement-jython-switch-commandp-test failed"))

(defun py-execute-statement-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-jython-noswitch) nil "py-execute-statement-jython-noswitch-commandp-test failed"))

(defun py-execute-statement-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-jython-dedicated) nil "py-execute-statement-jython-dedicated-commandp-test failed"))

(defun py-execute-statement-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-jython-dedicated-switch) nil "py-execute-statement-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-statement-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3.2-commandp-base arg teststring)))

(defun py-execute-statement-python3.2-commandp-base ()
  (assert (commandp 'py-execute-statement-python3.2) nil "py-execute-statement-python3.2-commandp-test failed"))

(defun py-execute-statement-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-statement-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3.2-switch) nil "py-execute-statement-python3.2-switch-commandp-test failed"))

(defun py-execute-statement-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-statement-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3.2-noswitch) nil "py-execute-statement-python3.2-noswitch-commandp-test failed"))

(defun py-execute-statement-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-statement-python3.2-dedicated) nil "py-execute-statement-python3.2-dedicated-commandp-test failed"))

(defun py-execute-statement-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-statement-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-statement-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-statement-python3.2-dedicated-switch) nil "py-execute-statement-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-block-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python-commandp-base arg teststring)))

(defun py-execute-block-python-commandp-base ()
  (assert (commandp 'py-execute-block-python) nil "py-execute-block-python-commandp-test failed"))

(defun py-execute-block-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python-switch-commandp-base arg teststring)))

(defun py-execute-block-python-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python-switch) nil "py-execute-block-python-switch-commandp-test failed"))

(defun py-execute-block-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python-noswitch-commandp-base arg teststring)))

(defun py-execute-block-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-python-noswitch) nil "py-execute-block-python-noswitch-commandp-test failed"))

(defun py-execute-block-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python-dedicated-commandp-base arg teststring)))

(defun py-execute-block-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-python-dedicated) nil "py-execute-block-python-dedicated-commandp-test failed"))

(defun py-execute-block-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python-dedicated-switch) nil "py-execute-block-python-dedicated-switch-commandp-test failed"))

(defun py-execute-block-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-ipython-commandp-base arg teststring)))

(defun py-execute-block-ipython-commandp-base ()
  (assert (commandp 'py-execute-block-ipython) nil "py-execute-block-ipython-commandp-test failed"))

(defun py-execute-block-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-ipython-switch-commandp-base arg teststring)))

(defun py-execute-block-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-block-ipython-switch) nil "py-execute-block-ipython-switch-commandp-test failed"))

(defun py-execute-block-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-block-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-ipython-noswitch) nil "py-execute-block-ipython-noswitch-commandp-test failed"))

(defun py-execute-block-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-block-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-ipython-dedicated) nil "py-execute-block-ipython-dedicated-commandp-test failed"))

(defun py-execute-block-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-ipython-dedicated-switch) nil "py-execute-block-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-block-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3-commandp-base arg teststring)))

(defun py-execute-block-python3-commandp-base ()
  (assert (commandp 'py-execute-block-python3) nil "py-execute-block-python3-commandp-test failed"))

(defun py-execute-block-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3-switch-commandp-base arg teststring)))

(defun py-execute-block-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python3-switch) nil "py-execute-block-python3-switch-commandp-test failed"))

(defun py-execute-block-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-block-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-python3-noswitch) nil "py-execute-block-python3-noswitch-commandp-test failed"))

(defun py-execute-block-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-block-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-python3-dedicated) nil "py-execute-block-python3-dedicated-commandp-test failed"))

(defun py-execute-block-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python3-dedicated-switch) nil "py-execute-block-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-block-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2-commandp-base arg teststring)))

(defun py-execute-block-python2-commandp-base ()
  (assert (commandp 'py-execute-block-python2) nil "py-execute-block-python2-commandp-test failed"))

(defun py-execute-block-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2-switch-commandp-base arg teststring)))

(defun py-execute-block-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python2-switch) nil "py-execute-block-python2-switch-commandp-test failed"))

(defun py-execute-block-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-block-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-python2-noswitch) nil "py-execute-block-python2-noswitch-commandp-test failed"))

(defun py-execute-block-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-block-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-python2-dedicated) nil "py-execute-block-python2-dedicated-commandp-test failed"))

(defun py-execute-block-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python2-dedicated-switch) nil "py-execute-block-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-block-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2.7-commandp-base arg teststring)))

(defun py-execute-block-python2.7-commandp-base ()
  (assert (commandp 'py-execute-block-python2.7) nil "py-execute-block-python2.7-commandp-test failed"))

(defun py-execute-block-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-block-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python2.7-switch) nil "py-execute-block-python2.7-switch-commandp-test failed"))

(defun py-execute-block-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-block-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-python2.7-noswitch) nil "py-execute-block-python2.7-noswitch-commandp-test failed"))

(defun py-execute-block-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-python2.7-dedicated) nil "py-execute-block-python2.7-dedicated-commandp-test failed"))

(defun py-execute-block-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python2.7-dedicated-switch) nil "py-execute-block-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-block-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-jython-commandp-base arg teststring)))

(defun py-execute-block-jython-commandp-base ()
  (assert (commandp 'py-execute-block-jython) nil "py-execute-block-jython-commandp-test failed"))

(defun py-execute-block-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-jython-switch-commandp-base arg teststring)))

(defun py-execute-block-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-block-jython-switch) nil "py-execute-block-jython-switch-commandp-test failed"))

(defun py-execute-block-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-block-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-jython-noswitch) nil "py-execute-block-jython-noswitch-commandp-test failed"))

(defun py-execute-block-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-block-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-jython-dedicated) nil "py-execute-block-jython-dedicated-commandp-test failed"))

(defun py-execute-block-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-jython-dedicated-switch) nil "py-execute-block-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-block-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3.2-commandp-base arg teststring)))

(defun py-execute-block-python3.2-commandp-base ()
  (assert (commandp 'py-execute-block-python3.2) nil "py-execute-block-python3.2-commandp-test failed"))

(defun py-execute-block-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-block-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python3.2-switch) nil "py-execute-block-python3.2-switch-commandp-test failed"))

(defun py-execute-block-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-block-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-python3.2-noswitch) nil "py-execute-block-python3.2-noswitch-commandp-test failed"))

(defun py-execute-block-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-python3.2-dedicated) nil "py-execute-block-python3.2-dedicated-commandp-test failed"))

(defun py-execute-block-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-python3.2-dedicated-switch) nil "py-execute-block-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python) nil "py-execute-block-or-clause-python-commandp-test failed"))

(defun py-execute-block-or-clause-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python-switch) nil "py-execute-block-or-clause-python-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python-noswitch) nil "py-execute-block-or-clause-python-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python-dedicated) nil "py-execute-block-or-clause-python-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python-dedicated-switch) nil "py-execute-block-or-clause-python-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-commandp-base arg teststring)))

(defun py-execute-block-or-clause-ipython-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-ipython) nil "py-execute-block-or-clause-ipython-commandp-test failed"))

(defun py-execute-block-or-clause-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-ipython-switch) nil "py-execute-block-or-clause-ipython-switch-commandp-test failed"))

(defun py-execute-block-or-clause-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-ipython-noswitch) nil "py-execute-block-or-clause-ipython-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-ipython-dedicated) nil "py-execute-block-or-clause-ipython-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-ipython-dedicated-switch) nil "py-execute-block-or-clause-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3) nil "py-execute-block-or-clause-python3-commandp-test failed"))

(defun py-execute-block-or-clause-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3-switch) nil "py-execute-block-or-clause-python3-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3-noswitch) nil "py-execute-block-or-clause-python3-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3-dedicated) nil "py-execute-block-or-clause-python3-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3-dedicated-switch) nil "py-execute-block-or-clause-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2) nil "py-execute-block-or-clause-python2-commandp-test failed"))

(defun py-execute-block-or-clause-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2-switch) nil "py-execute-block-or-clause-python2-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2-noswitch) nil "py-execute-block-or-clause-python2-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2-dedicated) nil "py-execute-block-or-clause-python2-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2-dedicated-switch) nil "py-execute-block-or-clause-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2.7) nil "py-execute-block-or-clause-python2.7-commandp-test failed"))

(defun py-execute-block-or-clause-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2.7-switch) nil "py-execute-block-or-clause-python2.7-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2.7-noswitch) nil "py-execute-block-or-clause-python2.7-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2.7-dedicated) nil "py-execute-block-or-clause-python2.7-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python2.7-dedicated-switch) nil "py-execute-block-or-clause-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-commandp-base arg teststring)))

(defun py-execute-block-or-clause-jython-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-jython) nil "py-execute-block-or-clause-jython-commandp-test failed"))

(defun py-execute-block-or-clause-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-jython-switch) nil "py-execute-block-or-clause-jython-switch-commandp-test failed"))

(defun py-execute-block-or-clause-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-jython-noswitch) nil "py-execute-block-or-clause-jython-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-jython-dedicated) nil "py-execute-block-or-clause-jython-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-jython-dedicated-switch) nil "py-execute-block-or-clause-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3.2) nil "py-execute-block-or-clause-python3.2-commandp-test failed"))

(defun py-execute-block-or-clause-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3.2-switch) nil "py-execute-block-or-clause-python3.2-switch-commandp-test failed"))

(defun py-execute-block-or-clause-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3.2-noswitch) nil "py-execute-block-or-clause-python3.2-noswitch-commandp-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3.2-dedicated) nil "py-execute-block-or-clause-python3.2-dedicated-commandp-test failed"))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-block-or-clause-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-block-or-clause-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-block-or-clause-python3.2-dedicated-switch) nil "py-execute-block-or-clause-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-def-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python-commandp-base arg teststring)))

(defun py-execute-def-python-commandp-base ()
  (assert (commandp 'py-execute-def-python) nil "py-execute-def-python-commandp-test failed"))

(defun py-execute-def-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python-switch-commandp-base arg teststring)))

(defun py-execute-def-python-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python-switch) nil "py-execute-def-python-switch-commandp-test failed"))

(defun py-execute-def-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python-noswitch-commandp-base arg teststring)))

(defun py-execute-def-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-python-noswitch) nil "py-execute-def-python-noswitch-commandp-test failed"))

(defun py-execute-def-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python-dedicated-commandp-base arg teststring)))

(defun py-execute-def-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-python-dedicated) nil "py-execute-def-python-dedicated-commandp-test failed"))

(defun py-execute-def-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python-dedicated-switch) nil "py-execute-def-python-dedicated-switch-commandp-test failed"))

(defun py-execute-def-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-ipython-commandp-base arg teststring)))

(defun py-execute-def-ipython-commandp-base ()
  (assert (commandp 'py-execute-def-ipython) nil "py-execute-def-ipython-commandp-test failed"))

(defun py-execute-def-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-ipython-switch-commandp-base arg teststring)))

(defun py-execute-def-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-def-ipython-switch) nil "py-execute-def-ipython-switch-commandp-test failed"))

(defun py-execute-def-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-def-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-ipython-noswitch) nil "py-execute-def-ipython-noswitch-commandp-test failed"))

(defun py-execute-def-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-def-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-ipython-dedicated) nil "py-execute-def-ipython-dedicated-commandp-test failed"))

(defun py-execute-def-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-ipython-dedicated-switch) nil "py-execute-def-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-def-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3-commandp-base arg teststring)))

(defun py-execute-def-python3-commandp-base ()
  (assert (commandp 'py-execute-def-python3) nil "py-execute-def-python3-commandp-test failed"))

(defun py-execute-def-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3-switch-commandp-base arg teststring)))

(defun py-execute-def-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python3-switch) nil "py-execute-def-python3-switch-commandp-test failed"))

(defun py-execute-def-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-def-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-python3-noswitch) nil "py-execute-def-python3-noswitch-commandp-test failed"))

(defun py-execute-def-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-def-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-python3-dedicated) nil "py-execute-def-python3-dedicated-commandp-test failed"))

(defun py-execute-def-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python3-dedicated-switch) nil "py-execute-def-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-def-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2-commandp-base arg teststring)))

(defun py-execute-def-python2-commandp-base ()
  (assert (commandp 'py-execute-def-python2) nil "py-execute-def-python2-commandp-test failed"))

(defun py-execute-def-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2-switch-commandp-base arg teststring)))

(defun py-execute-def-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python2-switch) nil "py-execute-def-python2-switch-commandp-test failed"))

(defun py-execute-def-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-def-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-python2-noswitch) nil "py-execute-def-python2-noswitch-commandp-test failed"))

(defun py-execute-def-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-def-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-python2-dedicated) nil "py-execute-def-python2-dedicated-commandp-test failed"))

(defun py-execute-def-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python2-dedicated-switch) nil "py-execute-def-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-def-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2.7-commandp-base arg teststring)))

(defun py-execute-def-python2.7-commandp-base ()
  (assert (commandp 'py-execute-def-python2.7) nil "py-execute-def-python2.7-commandp-test failed"))

(defun py-execute-def-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-def-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python2.7-switch) nil "py-execute-def-python2.7-switch-commandp-test failed"))

(defun py-execute-def-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-def-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-python2.7-noswitch) nil "py-execute-def-python2.7-noswitch-commandp-test failed"))

(defun py-execute-def-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-python2.7-dedicated) nil "py-execute-def-python2.7-dedicated-commandp-test failed"))

(defun py-execute-def-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python2.7-dedicated-switch) nil "py-execute-def-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-def-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-jython-commandp-base arg teststring)))

(defun py-execute-def-jython-commandp-base ()
  (assert (commandp 'py-execute-def-jython) nil "py-execute-def-jython-commandp-test failed"))

(defun py-execute-def-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-jython-switch-commandp-base arg teststring)))

(defun py-execute-def-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-def-jython-switch) nil "py-execute-def-jython-switch-commandp-test failed"))

(defun py-execute-def-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-def-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-jython-noswitch) nil "py-execute-def-jython-noswitch-commandp-test failed"))

(defun py-execute-def-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-def-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-jython-dedicated) nil "py-execute-def-jython-dedicated-commandp-test failed"))

(defun py-execute-def-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-jython-dedicated-switch) nil "py-execute-def-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-def-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3.2-commandp-base arg teststring)))

(defun py-execute-def-python3.2-commandp-base ()
  (assert (commandp 'py-execute-def-python3.2) nil "py-execute-def-python3.2-commandp-test failed"))

(defun py-execute-def-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-def-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python3.2-switch) nil "py-execute-def-python3.2-switch-commandp-test failed"))

(defun py-execute-def-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-def-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-def-python3.2-noswitch) nil "py-execute-def-python3.2-noswitch-commandp-test failed"))

(defun py-execute-def-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-def-python3.2-dedicated) nil "py-execute-def-python3.2-dedicated-commandp-test failed"))

(defun py-execute-def-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-def-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-def-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-def-python3.2-dedicated-switch) nil "py-execute-def-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-class-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python-commandp-base arg teststring)))

(defun py-execute-class-python-commandp-base ()
  (assert (commandp 'py-execute-class-python) nil "py-execute-class-python-commandp-test failed"))

(defun py-execute-class-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python-switch-commandp-base arg teststring)))

(defun py-execute-class-python-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python-switch) nil "py-execute-class-python-switch-commandp-test failed"))

(defun py-execute-class-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python-noswitch-commandp-base arg teststring)))

(defun py-execute-class-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-python-noswitch) nil "py-execute-class-python-noswitch-commandp-test failed"))

(defun py-execute-class-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python-dedicated-commandp-base arg teststring)))

(defun py-execute-class-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-python-dedicated) nil "py-execute-class-python-dedicated-commandp-test failed"))

(defun py-execute-class-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python-dedicated-switch) nil "py-execute-class-python-dedicated-switch-commandp-test failed"))

(defun py-execute-class-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-ipython-commandp-base arg teststring)))

(defun py-execute-class-ipython-commandp-base ()
  (assert (commandp 'py-execute-class-ipython) nil "py-execute-class-ipython-commandp-test failed"))

(defun py-execute-class-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-ipython-switch-commandp-base arg teststring)))

(defun py-execute-class-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-class-ipython-switch) nil "py-execute-class-ipython-switch-commandp-test failed"))

(defun py-execute-class-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-class-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-ipython-noswitch) nil "py-execute-class-ipython-noswitch-commandp-test failed"))

(defun py-execute-class-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-class-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-ipython-dedicated) nil "py-execute-class-ipython-dedicated-commandp-test failed"))

(defun py-execute-class-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-ipython-dedicated-switch) nil "py-execute-class-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-class-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3-commandp-base arg teststring)))

(defun py-execute-class-python3-commandp-base ()
  (assert (commandp 'py-execute-class-python3) nil "py-execute-class-python3-commandp-test failed"))

(defun py-execute-class-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3-switch-commandp-base arg teststring)))

(defun py-execute-class-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python3-switch) nil "py-execute-class-python3-switch-commandp-test failed"))

(defun py-execute-class-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-class-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-python3-noswitch) nil "py-execute-class-python3-noswitch-commandp-test failed"))

(defun py-execute-class-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-class-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-python3-dedicated) nil "py-execute-class-python3-dedicated-commandp-test failed"))

(defun py-execute-class-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python3-dedicated-switch) nil "py-execute-class-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-class-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2-commandp-base arg teststring)))

(defun py-execute-class-python2-commandp-base ()
  (assert (commandp 'py-execute-class-python2) nil "py-execute-class-python2-commandp-test failed"))

(defun py-execute-class-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2-switch-commandp-base arg teststring)))

(defun py-execute-class-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python2-switch) nil "py-execute-class-python2-switch-commandp-test failed"))

(defun py-execute-class-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-class-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-python2-noswitch) nil "py-execute-class-python2-noswitch-commandp-test failed"))

(defun py-execute-class-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-class-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-python2-dedicated) nil "py-execute-class-python2-dedicated-commandp-test failed"))

(defun py-execute-class-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python2-dedicated-switch) nil "py-execute-class-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-class-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2.7-commandp-base arg teststring)))

(defun py-execute-class-python2.7-commandp-base ()
  (assert (commandp 'py-execute-class-python2.7) nil "py-execute-class-python2.7-commandp-test failed"))

(defun py-execute-class-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-class-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python2.7-switch) nil "py-execute-class-python2.7-switch-commandp-test failed"))

(defun py-execute-class-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-class-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-python2.7-noswitch) nil "py-execute-class-python2.7-noswitch-commandp-test failed"))

(defun py-execute-class-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-python2.7-dedicated) nil "py-execute-class-python2.7-dedicated-commandp-test failed"))

(defun py-execute-class-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python2.7-dedicated-switch) nil "py-execute-class-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-class-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-jython-commandp-base arg teststring)))

(defun py-execute-class-jython-commandp-base ()
  (assert (commandp 'py-execute-class-jython) nil "py-execute-class-jython-commandp-test failed"))

(defun py-execute-class-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-jython-switch-commandp-base arg teststring)))

(defun py-execute-class-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-class-jython-switch) nil "py-execute-class-jython-switch-commandp-test failed"))

(defun py-execute-class-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-class-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-jython-noswitch) nil "py-execute-class-jython-noswitch-commandp-test failed"))

(defun py-execute-class-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-class-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-jython-dedicated) nil "py-execute-class-jython-dedicated-commandp-test failed"))

(defun py-execute-class-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-jython-dedicated-switch) nil "py-execute-class-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-class-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3.2-commandp-base arg teststring)))

(defun py-execute-class-python3.2-commandp-base ()
  (assert (commandp 'py-execute-class-python3.2) nil "py-execute-class-python3.2-commandp-test failed"))

(defun py-execute-class-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-class-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python3.2-switch) nil "py-execute-class-python3.2-switch-commandp-test failed"))

(defun py-execute-class-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-class-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-class-python3.2-noswitch) nil "py-execute-class-python3.2-noswitch-commandp-test failed"))

(defun py-execute-class-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-class-python3.2-dedicated) nil "py-execute-class-python3.2-dedicated-commandp-test failed"))

(defun py-execute-class-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-class-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-class-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-class-python3.2-dedicated-switch) nil "py-execute-class-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-region-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python-commandp-base arg teststring)))

(defun py-execute-region-python-commandp-base ()
  (assert (commandp 'py-execute-region-python) nil "py-execute-region-python-commandp-test failed"))

(defun py-execute-region-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python-switch-commandp-base arg teststring)))

(defun py-execute-region-python-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python-switch) nil "py-execute-region-python-switch-commandp-test failed"))

(defun py-execute-region-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python-noswitch-commandp-base arg teststring)))

(defun py-execute-region-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-python-noswitch) nil "py-execute-region-python-noswitch-commandp-test failed"))

(defun py-execute-region-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python-dedicated-commandp-base arg teststring)))

(defun py-execute-region-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-python-dedicated) nil "py-execute-region-python-dedicated-commandp-test failed"))

(defun py-execute-region-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python-dedicated-switch) nil "py-execute-region-python-dedicated-switch-commandp-test failed"))

(defun py-execute-region-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-ipython-commandp-base arg teststring)))

(defun py-execute-region-ipython-commandp-base ()
  (assert (commandp 'py-execute-region-ipython) nil "py-execute-region-ipython-commandp-test failed"))

(defun py-execute-region-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-ipython-switch-commandp-base arg teststring)))

(defun py-execute-region-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-region-ipython-switch) nil "py-execute-region-ipython-switch-commandp-test failed"))

(defun py-execute-region-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-region-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-ipython-noswitch) nil "py-execute-region-ipython-noswitch-commandp-test failed"))

(defun py-execute-region-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-region-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-ipython-dedicated) nil "py-execute-region-ipython-dedicated-commandp-test failed"))

(defun py-execute-region-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-ipython-dedicated-switch) nil "py-execute-region-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-region-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3-commandp-base arg teststring)))

(defun py-execute-region-python3-commandp-base ()
  (assert (commandp 'py-execute-region-python3) nil "py-execute-region-python3-commandp-test failed"))

(defun py-execute-region-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3-switch-commandp-base arg teststring)))

(defun py-execute-region-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python3-switch) nil "py-execute-region-python3-switch-commandp-test failed"))

(defun py-execute-region-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-region-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-python3-noswitch) nil "py-execute-region-python3-noswitch-commandp-test failed"))

(defun py-execute-region-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-region-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-python3-dedicated) nil "py-execute-region-python3-dedicated-commandp-test failed"))

(defun py-execute-region-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python3-dedicated-switch) nil "py-execute-region-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-region-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2-commandp-base arg teststring)))

(defun py-execute-region-python2-commandp-base ()
  (assert (commandp 'py-execute-region-python2) nil "py-execute-region-python2-commandp-test failed"))

(defun py-execute-region-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2-switch-commandp-base arg teststring)))

(defun py-execute-region-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python2-switch) nil "py-execute-region-python2-switch-commandp-test failed"))

(defun py-execute-region-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-region-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-python2-noswitch) nil "py-execute-region-python2-noswitch-commandp-test failed"))

(defun py-execute-region-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-region-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-python2-dedicated) nil "py-execute-region-python2-dedicated-commandp-test failed"))

(defun py-execute-region-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python2-dedicated-switch) nil "py-execute-region-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-region-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2.7-commandp-base arg teststring)))

(defun py-execute-region-python2.7-commandp-base ()
  (assert (commandp 'py-execute-region-python2.7) nil "py-execute-region-python2.7-commandp-test failed"))

(defun py-execute-region-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-region-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python2.7-switch) nil "py-execute-region-python2.7-switch-commandp-test failed"))

(defun py-execute-region-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-region-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-python2.7-noswitch) nil "py-execute-region-python2.7-noswitch-commandp-test failed"))

(defun py-execute-region-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-python2.7-dedicated) nil "py-execute-region-python2.7-dedicated-commandp-test failed"))

(defun py-execute-region-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python2.7-dedicated-switch) nil "py-execute-region-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-region-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-jython-commandp-base arg teststring)))

(defun py-execute-region-jython-commandp-base ()
  (assert (commandp 'py-execute-region-jython) nil "py-execute-region-jython-commandp-test failed"))

(defun py-execute-region-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-jython-switch-commandp-base arg teststring)))

(defun py-execute-region-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-region-jython-switch) nil "py-execute-region-jython-switch-commandp-test failed"))

(defun py-execute-region-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-region-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-jython-noswitch) nil "py-execute-region-jython-noswitch-commandp-test failed"))

(defun py-execute-region-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-region-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-jython-dedicated) nil "py-execute-region-jython-dedicated-commandp-test failed"))

(defun py-execute-region-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-jython-dedicated-switch) nil "py-execute-region-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-region-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3.2-commandp-base arg teststring)))

(defun py-execute-region-python3.2-commandp-base ()
  (assert (commandp 'py-execute-region-python3.2) nil "py-execute-region-python3.2-commandp-test failed"))

(defun py-execute-region-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-region-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python3.2-switch) nil "py-execute-region-python3.2-switch-commandp-test failed"))

(defun py-execute-region-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-region-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-region-python3.2-noswitch) nil "py-execute-region-python3.2-noswitch-commandp-test failed"))

(defun py-execute-region-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-region-python3.2-dedicated) nil "py-execute-region-python3.2-dedicated-commandp-test failed"))

(defun py-execute-region-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-region-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-region-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-region-python3.2-dedicated-switch) nil "py-execute-region-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python-commandp-base arg teststring)))

(defun py-execute-buffer-python-commandp-base ()
  (assert (commandp 'py-execute-buffer-python) nil "py-execute-buffer-python-commandp-test failed"))

(defun py-execute-buffer-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python-switch) nil "py-execute-buffer-python-switch-commandp-test failed"))

(defun py-execute-buffer-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python-noswitch) nil "py-execute-buffer-python-noswitch-commandp-test failed"))

(defun py-execute-buffer-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-python-dedicated) nil "py-execute-buffer-python-dedicated-commandp-test failed"))

(defun py-execute-buffer-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python-dedicated-switch) nil "py-execute-buffer-python-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-ipython-commandp-base arg teststring)))

(defun py-execute-buffer-ipython-commandp-base ()
  (assert (commandp 'py-execute-buffer-ipython) nil "py-execute-buffer-ipython-commandp-test failed"))

(defun py-execute-buffer-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-ipython-switch-commandp-base arg teststring)))

(defun py-execute-buffer-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-ipython-switch) nil "py-execute-buffer-ipython-switch-commandp-test failed"))

(defun py-execute-buffer-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-ipython-noswitch) nil "py-execute-buffer-ipython-noswitch-commandp-test failed"))

(defun py-execute-buffer-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-ipython-dedicated) nil "py-execute-buffer-ipython-dedicated-commandp-test failed"))

(defun py-execute-buffer-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-ipython-dedicated-switch) nil "py-execute-buffer-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3-commandp-base arg teststring)))

(defun py-execute-buffer-python3-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3) nil "py-execute-buffer-python3-commandp-test failed"))

(defun py-execute-buffer-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3-switch) nil "py-execute-buffer-python3-switch-commandp-test failed"))

(defun py-execute-buffer-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3-noswitch) nil "py-execute-buffer-python3-noswitch-commandp-test failed"))

(defun py-execute-buffer-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3-dedicated) nil "py-execute-buffer-python3-dedicated-commandp-test failed"))

(defun py-execute-buffer-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3-dedicated-switch) nil "py-execute-buffer-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2-commandp-base arg teststring)))

(defun py-execute-buffer-python2-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2) nil "py-execute-buffer-python2-commandp-test failed"))

(defun py-execute-buffer-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2-switch) nil "py-execute-buffer-python2-switch-commandp-test failed"))

(defun py-execute-buffer-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2-noswitch) nil "py-execute-buffer-python2-noswitch-commandp-test failed"))

(defun py-execute-buffer-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2-dedicated) nil "py-execute-buffer-python2-dedicated-commandp-test failed"))

(defun py-execute-buffer-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2-dedicated-switch) nil "py-execute-buffer-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-commandp-base arg teststring)))

(defun py-execute-buffer-python2.7-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2.7) nil "py-execute-buffer-python2.7-commandp-test failed"))

(defun py-execute-buffer-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2.7-switch) nil "py-execute-buffer-python2.7-switch-commandp-test failed"))

(defun py-execute-buffer-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2.7-noswitch) nil "py-execute-buffer-python2.7-noswitch-commandp-test failed"))

(defun py-execute-buffer-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2.7-dedicated) nil "py-execute-buffer-python2.7-dedicated-commandp-test failed"))

(defun py-execute-buffer-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python2.7-dedicated-switch) nil "py-execute-buffer-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-jython-commandp-base arg teststring)))

(defun py-execute-buffer-jython-commandp-base ()
  (assert (commandp 'py-execute-buffer-jython) nil "py-execute-buffer-jython-commandp-test failed"))

(defun py-execute-buffer-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-jython-switch-commandp-base arg teststring)))

(defun py-execute-buffer-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-jython-switch) nil "py-execute-buffer-jython-switch-commandp-test failed"))

(defun py-execute-buffer-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-jython-noswitch) nil "py-execute-buffer-jython-noswitch-commandp-test failed"))

(defun py-execute-buffer-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-jython-dedicated) nil "py-execute-buffer-jython-dedicated-commandp-test failed"))

(defun py-execute-buffer-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-jython-dedicated-switch) nil "py-execute-buffer-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-buffer-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-commandp-base arg teststring)))

(defun py-execute-buffer-python3.2-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3.2) nil "py-execute-buffer-python3.2-commandp-test failed"))

(defun py-execute-buffer-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3.2-switch) nil "py-execute-buffer-python3.2-switch-commandp-test failed"))

(defun py-execute-buffer-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-buffer-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3.2-noswitch) nil "py-execute-buffer-python3.2-noswitch-commandp-test failed"))

(defun py-execute-buffer-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3.2-dedicated) nil "py-execute-buffer-python3.2-dedicated-commandp-test failed"))

(defun py-execute-buffer-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-buffer-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-buffer-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-buffer-python3.2-dedicated-switch) nil "py-execute-buffer-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python-commandp-base arg teststring)))

(defun py-execute-expression-python-commandp-base ()
  (assert (commandp 'py-execute-expression-python) nil "py-execute-expression-python-commandp-test failed"))

(defun py-execute-expression-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python-switch-commandp-base arg teststring)))

(defun py-execute-expression-python-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python-switch) nil "py-execute-expression-python-switch-commandp-test failed"))

(defun py-execute-expression-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-python-noswitch) nil "py-execute-expression-python-noswitch-commandp-test failed"))

(defun py-execute-expression-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-python-dedicated) nil "py-execute-expression-python-dedicated-commandp-test failed"))

(defun py-execute-expression-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python-dedicated-switch) nil "py-execute-expression-python-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-ipython-commandp-base arg teststring)))

(defun py-execute-expression-ipython-commandp-base ()
  (assert (commandp 'py-execute-expression-ipython) nil "py-execute-expression-ipython-commandp-test failed"))

(defun py-execute-expression-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-ipython-switch-commandp-base arg teststring)))

(defun py-execute-expression-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-ipython-switch) nil "py-execute-expression-ipython-switch-commandp-test failed"))

(defun py-execute-expression-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-ipython-noswitch) nil "py-execute-expression-ipython-noswitch-commandp-test failed"))

(defun py-execute-expression-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-ipython-dedicated) nil "py-execute-expression-ipython-dedicated-commandp-test failed"))

(defun py-execute-expression-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-ipython-dedicated-switch) nil "py-execute-expression-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3-commandp-base arg teststring)))

(defun py-execute-expression-python3-commandp-base ()
  (assert (commandp 'py-execute-expression-python3) nil "py-execute-expression-python3-commandp-test failed"))

(defun py-execute-expression-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3-switch-commandp-base arg teststring)))

(defun py-execute-expression-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3-switch) nil "py-execute-expression-python3-switch-commandp-test failed"))

(defun py-execute-expression-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3-noswitch) nil "py-execute-expression-python3-noswitch-commandp-test failed"))

(defun py-execute-expression-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-python3-dedicated) nil "py-execute-expression-python3-dedicated-commandp-test failed"))

(defun py-execute-expression-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3-dedicated-switch) nil "py-execute-expression-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2-commandp-base arg teststring)))

(defun py-execute-expression-python2-commandp-base ()
  (assert (commandp 'py-execute-expression-python2) nil "py-execute-expression-python2-commandp-test failed"))

(defun py-execute-expression-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2-switch-commandp-base arg teststring)))

(defun py-execute-expression-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2-switch) nil "py-execute-expression-python2-switch-commandp-test failed"))

(defun py-execute-expression-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2-noswitch) nil "py-execute-expression-python2-noswitch-commandp-test failed"))

(defun py-execute-expression-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-python2-dedicated) nil "py-execute-expression-python2-dedicated-commandp-test failed"))

(defun py-execute-expression-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2-dedicated-switch) nil "py-execute-expression-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2.7-commandp-base arg teststring)))

(defun py-execute-expression-python2.7-commandp-base ()
  (assert (commandp 'py-execute-expression-python2.7) nil "py-execute-expression-python2.7-commandp-test failed"))

(defun py-execute-expression-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-expression-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2.7-switch) nil "py-execute-expression-python2.7-switch-commandp-test failed"))

(defun py-execute-expression-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2.7-noswitch) nil "py-execute-expression-python2.7-noswitch-commandp-test failed"))

(defun py-execute-expression-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-python2.7-dedicated) nil "py-execute-expression-python2.7-dedicated-commandp-test failed"))

(defun py-execute-expression-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python2.7-dedicated-switch) nil "py-execute-expression-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-jython-commandp-base arg teststring)))

(defun py-execute-expression-jython-commandp-base ()
  (assert (commandp 'py-execute-expression-jython) nil "py-execute-expression-jython-commandp-test failed"))

(defun py-execute-expression-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-jython-switch-commandp-base arg teststring)))

(defun py-execute-expression-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-jython-switch) nil "py-execute-expression-jython-switch-commandp-test failed"))

(defun py-execute-expression-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-jython-noswitch) nil "py-execute-expression-jython-noswitch-commandp-test failed"))

(defun py-execute-expression-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-jython-dedicated) nil "py-execute-expression-jython-dedicated-commandp-test failed"))

(defun py-execute-expression-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-jython-dedicated-switch) nil "py-execute-expression-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-expression-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3.2-commandp-base arg teststring)))

(defun py-execute-expression-python3.2-commandp-base ()
  (assert (commandp 'py-execute-expression-python3.2) nil "py-execute-expression-python3.2-commandp-test failed"))

(defun py-execute-expression-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-expression-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3.2-switch) nil "py-execute-expression-python3.2-switch-commandp-test failed"))

(defun py-execute-expression-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-expression-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3.2-noswitch) nil "py-execute-expression-python3.2-noswitch-commandp-test failed"))

(defun py-execute-expression-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-expression-python3.2-dedicated) nil "py-execute-expression-python3.2-dedicated-commandp-test failed"))

(defun py-execute-expression-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-expression-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-expression-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-expression-python3.2-dedicated-switch) nil "py-execute-expression-python3.2-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-python-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python-commandp-base arg teststring)))

(defun py-execute-partial-expression-python-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python) nil "py-execute-partial-expression-python-commandp-test failed"))

(defun py-execute-partial-expression-python-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python-switch) nil "py-execute-partial-expression-python-switch-commandp-test failed"))

(defun py-execute-partial-expression-python-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python-noswitch) nil "py-execute-partial-expression-python-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-python-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python-dedicated) nil "py-execute-partial-expression-python-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-python-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python-dedicated-switch) nil "py-execute-partial-expression-python-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-ipython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-commandp-base arg teststring)))

(defun py-execute-partial-expression-ipython-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-ipython) nil "py-execute-partial-expression-ipython-commandp-test failed"))

(defun py-execute-partial-expression-ipython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-ipython-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-ipython-switch) nil "py-execute-partial-expression-ipython-switch-commandp-test failed"))

(defun py-execute-partial-expression-ipython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-ipython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-ipython-noswitch) nil "py-execute-partial-expression-ipython-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-ipython-dedicated) nil "py-execute-partial-expression-ipython-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-ipython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-ipython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-ipython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-ipython-dedicated-switch) nil "py-execute-partial-expression-ipython-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-python3-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3) nil "py-execute-partial-expression-python3-commandp-test failed"))

(defun py-execute-partial-expression-python3-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3-switch) nil "py-execute-partial-expression-python3-switch-commandp-test failed"))

(defun py-execute-partial-expression-python3-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3-noswitch) nil "py-execute-partial-expression-python3-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-python3-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3-dedicated) nil "py-execute-partial-expression-python3-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-python3-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3-dedicated-switch) nil "py-execute-partial-expression-python3-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-python2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2) nil "py-execute-partial-expression-python2-commandp-test failed"))

(defun py-execute-partial-expression-python2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2-switch) nil "py-execute-partial-expression-python2-switch-commandp-test failed"))

(defun py-execute-partial-expression-python2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2-noswitch) nil "py-execute-partial-expression-python2-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-python2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2-dedicated) nil "py-execute-partial-expression-python2-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-python2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2-dedicated-switch) nil "py-execute-partial-expression-python2-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-python2.7-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2.7-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2.7) nil "py-execute-partial-expression-python2.7-commandp-test failed"))

(defun py-execute-partial-expression-python2.7-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2.7-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2.7-switch) nil "py-execute-partial-expression-python2.7-switch-commandp-test failed"))

(defun py-execute-partial-expression-python2.7-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2.7-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2.7-noswitch) nil "py-execute-partial-expression-python2.7-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2.7-dedicated) nil "py-execute-partial-expression-python2.7-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-python2.7-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python2.7-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python2.7-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python2.7-dedicated-switch) nil "py-execute-partial-expression-python2.7-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-jython-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-commandp-base arg teststring)))

(defun py-execute-partial-expression-jython-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-jython) nil "py-execute-partial-expression-jython-commandp-test failed"))

(defun py-execute-partial-expression-jython-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-jython-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-jython-switch) nil "py-execute-partial-expression-jython-switch-commandp-test failed"))

(defun py-execute-partial-expression-jython-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-jython-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-jython-noswitch) nil "py-execute-partial-expression-jython-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-jython-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-jython-dedicated) nil "py-execute-partial-expression-jython-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-jython-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-jython-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-jython-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-jython-dedicated-switch) nil "py-execute-partial-expression-jython-dedicated-switch-commandp-test failed"))

(defun py-execute-partial-expression-python3.2-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3.2-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3.2) nil "py-execute-partial-expression-python3.2-commandp-test failed"))

(defun py-execute-partial-expression-python3.2-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3.2-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3.2-switch) nil "py-execute-partial-expression-python3.2-switch-commandp-test failed"))

(defun py-execute-partial-expression-python3.2-noswitch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-noswitch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3.2-noswitch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3.2-noswitch) nil "py-execute-partial-expression-python3.2-noswitch-commandp-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3.2-dedicated) nil "py-execute-partial-expression-python3.2-dedicated-commandp-test failed"))

(defun py-execute-partial-expression-python3.2-dedicated-switch-commandp-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-execute-partial-expression-python3.2-dedicated-switch-commandp-base arg teststring)))

(defun py-execute-partial-expression-python3.2-dedicated-switch-commandp-base ()
  (assert (commandp 'py-execute-partial-expression-python3.2-dedicated-switch) nil "py-execute-partial-expression-python3.2-dedicated-switch-commandp-test failed"))

(defun switch-windows-on-execute-p-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the switch-windows-on-execute-p-test\")
")))
    (py-bug-tests-intern 'switch-windows-on-execute-p-base arg teststring)))

(defun switch-windows-on-execute-p-base ()
  (let ((py-shell-switch-buffers-on-execute-p t)
        (erg (buffer-name)))
    (assert (py-execute-buffer) nil "switch-windows-on-execute-p-test failed")))

(defun split-windows-on-execute-p-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the `split-windows-on-execute-p-test'\")
")))
    (py-bug-tests-intern 'split-windows-on-execute-p-base arg teststring)))

(defun split-windows-on-execute-p-base ()
  (delete-other-windows)
  (let ((py-split-windows-on-execute-p t)
        (py-split-windows-on-execute-function 'split-window-vertically)
        (py-shell-switch-buffers-on-execute-p t)
        (erg (current-window-configuration)))
    (py-execute-buffer)
    (assert (not (window-full-height-p)) nil "split-windows-on-execute-p-test failed")))

;; this test is not valable, as python-mode-map often changes 
(defun py-menu-pyshell-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
print(\"I'm the `py-menu-pyshell-test'\")
")))
    (py-bug-tests-intern 'py-menu-pyshell-base arg teststring)))

(defun py-menu-pyshell-base ()
  (assert (string= "PyShell" (prin1-to-string
                              (car (nth 1 (cdr (nth 17 python-mode-map))))
                              ;; (car (nth 2 (nth 1 (cdr python-mode-map))))
                              )) nil "py-menu-pyshell-test failed"))

(defun python-dedicated-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'python-dedicated-base arg teststring)))

(defun python-dedicated-base ()
  (set-buffer (python-dedicated))
  (sit-for 0.1)
  (assert (string-match "^\*Python-[:alnum:]+*" (buffer-name)) nil "python-dedicated-test failed"))

(defun py-separator-char-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'py-separator-char-base arg teststring)))

(defun py-separator-char-base ()
  (assert (stringp (py-separator-char)) nil "py-separator-char-test failed"))

(defun py-completion-at-point-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
pri")))
    (py-bug-tests-intern 'py-completion-at-point-base 2 teststring)))

(defun py-completion-at-point-base ()
  (delete-other-windows)
  (py-completion-at-point)
  (sit-for 0.1)
  (assert (looking-back "print") nil "py-completion-at-point-test failed"))

(defun py-shell-complete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
impo")))
    (py-bug-tests-intern 'py-shell-complete-base 2 teststring)))

(defun py-shell-complete-base ()
  (py-shell-complete)
  (sit-for 0.1)
  (assert (looking-back "import") nil "py-completion-at-point-test failed"))

(defun toggle-force-py-shell-name-p-test (&optional arg)
  (interactive "p")
  (let ((teststring ""))
    (py-bug-tests-intern 'toggle-force-py-shell-name-p-base arg teststring)))

(defun toggle-force-py-shell-name-p-base ()
  (let ((old py-force-py-shell-name-p))
    (assert (not (eq old (toggle-force-py-shell-name-p))) nil "toggle-force-py-shell-name-p-test failed")
    (setq py-force-py-shell-name-p old)))

(defun before-inline-comment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
self._blah_blup = 0xe000 # aka: foo bar baz
self.nult = {}
self.nult['_foobar'] = []
"))
    (py-bug-tests-intern 'before-inline-comment-base arg teststring)))

(defun before-inline-comment-base ()
  (goto-char 72)
  (py-end-of-statement)
  (sit-for 0.1)
  (assert (eq 106 (point)) nil "before-inline-comment-test failed"))

(defun py-end-of-def-inline-comment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

    #####################################
#####################################
def fooBaz( bar ):  # version 2003/9/7
  if \"Foo:\" == bar \\
          or  \"[Foo:]\" == bar \\
          or \"Foo:]\" == bar \\
          or \"Baz:\" == bar:
      return False
  return True
"))
    (py-bug-tests-intern 'py-end-of-def-inline-comment-base arg teststring)))

(defun py-end-of-def-inline-comment-base ()
  (goto-char 49)
  (py-end-of-def-or-class)
  (assert (eq 311 (point)) nil "py-end-of-def-inline-comment-test failed"))

(defun py-compute-indentation-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-
with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")

def foo()
"))
    (py-bug-tests-intern 'py-compute-indentation-base arg teststring)))

(defun py-compute-indentation-base ()
  (goto-char 99)
  (assert (eq 4 (py-compute-indentation)) nil "py-compute-indentation-test #1 failed")
  (goto-char 127)
  (assert (eq 8 (py-compute-indentation)) nil "py-compute-indentation-test #2 failed"))

(defun py-end-of-statement-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/python
# -*- coding: utf-8 -*-
print dir()
c = Cat()
c.hello() #causes error, but emacs tracking fails
import sys, os; os.remove('do/something/nasty') # lp:1025000
"))
    (py-bug-tests-intern 'py-end-of-statement-base arg teststring)))

(defun py-end-of-statement-base ()
  (goto-char (point-min))
  (py-end-of-statement)
  (assert (eq 55 (point)) nil "py-end-of-statement-test #1 failed")
  (goto-char 65)
  (py-end-of-statement)
  (assert (eq 75 (point)) nil "py-end-of-statement-test #2 failed")
  (goto-char 99)
  (py-end-of-statement)
  (assert (eq 163 (point)) nil "py-end-of-statement-test #3 failed")
  )

(defun key-binding-tests (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
"))
    (py-bug-tests-intern 'key-binding-base arg teststring)))

(defun key-binding-base ()
  (assert (eq (key-binding [(:)]) 'py-electric-colon) nil "py-electric-colon key-binding test failed")
  (assert (eq (key-binding [(\#)]) 'py-electric-comment) nil "py-electric-comment key-binding test failed")
  (assert (eq (key-binding [(delete)]) 'py-electric-delete) nil "py-electric-delete key-binding test failed")
  (assert (eq (key-binding [(backspace)]) 'py-electric-backspace) nil "py-electric-backspace key-binding test failed")
  (assert (eq (key-binding [(control backspace)]) 'py-hungry-delete-backwards) nil "py-hungry-delete-backwards key-binding test failed")
  (assert (eq (key-binding [(control c) (delete)]) 'py-hungry-delete-forward) nil "py-hungry-delete-forward key-binding test failed")
  (assert (eq (key-binding [(control meta a)]) 'py-beginning-of-def-or-class) nil "py-beginning-of-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control meta e)]) 'py-end-of-def-or-class) nil "py-end-of-def-or-class key-binding test failed")
  (assert (eq (key-binding [(super backspace)]) 'py-dedent) nil "py-dedent key-binding test failed")
  (assert (eq (key-binding [(control c)(control l)]) 'py-shift-left) nil "py-shift-left key-binding test failed")
  (assert (eq (key-binding [(control c)(control r)]) 'py-shift-right) nil "py-shift-right key-binding test failed")
  (assert (eq (key-binding [(control c)(<)]) 'py-shift-left) nil "py-shift-left key-binding test failed")
  (assert (eq (key-binding [(control c)(>)]) 'py-shift-right) nil "py-shift-right key-binding test failed")
  (assert (eq (key-binding [(control c)(tab)]) 'py-indent-region) nil "py-indent-region key-binding test failed")
  (assert (eq (key-binding [(control c)(:)]) 'py-guess-indent-offset) nil "py-guess-indent-offset key-binding test failed")
  (assert (eq (key-binding [(control c)(control c)]) 'py-execute-buffer) nil "py-execute-buffer key-binding test failed")
  (assert (eq (key-binding [(control c)(control m)]) 'py-execute-import-or-reload) nil "py-execute-import-or-reload key-binding test failed")
  (assert (eq (key-binding [(control c)(control s)]) 'py-execute-string) nil "py-execute-string key-binding test failed")
  (assert (eq (key-binding [(control c)(|)]) 'py-execute-region) nil "py-execute-region key-binding test failed")
  (assert (eq (key-binding [(control meta x)]) 'py-execute-def-or-class) nil "py-execute-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control c)(!)]) 'py-shell) nil "py-shell key-binding test failed")
  (assert (eq (key-binding [(control c)(control t)]) 'py-toggle-shell) nil "py-toggle-shell key-binding test failed")
  (assert (eq (key-binding [(control meta h)]) 'py-mark-def-or-class) nil "py-mark-def-or-class key-binding test failed")
  (assert (eq (key-binding [(control c)(control k)]) 'py-mark-block-or-clause) nil "py-mark-block-or-clause key-binding test failed")
  (assert (eq (key-binding [(control c)(.)]) 'py-expression) nil "py-expression key-binding test failed")
  (assert (eq (key-binding [(control c)(control d)]) 'py-pdbtrack-toggle-stack-tracking) nil "py-pdbtrack-toggle-stack-tracking key-binding test failed")
  (assert (eq (key-binding [(control c)(control f)]) 'py-sort-imports) nil "py-sort-imports key-binding test failed")
  (assert (eq (key-binding [(control c)(\#)]) 'py-comment-region) nil "py-comment-region key-binding test failed")
  (assert (eq (key-binding [(control c)(\?)]) 'py-describe-mode) nil "py-describe-mode key-binding test failed")
  (assert (eq (key-binding [(control c)(control e)]) 'py-describe-symbol) nil "py-describe-symbol key-binding test failed")
  (assert (eq (key-binding [(control c)(-)]) 'py-up-exception) nil "py-up-exception key-binding test failed")
  (assert (eq (key-binding [(control c)(=)]) 'py-down-exception) nil "py-down-exception key-binding test failed")
  (assert (eq (key-binding [(control x) (n) (d)]) 'py-narrow-to-defun) nil "py-narrow-to-defun key-binding test failed")

  (assert (eq (key-binding [(control c)(control b)]) 'py-submit-bug-report) nil "py-submit-bug-report key-binding test failed")
  (assert (eq (key-binding [(control c)(control v)]) 'py-version) nil "py-version key-binding test failed")
  (assert (eq (key-binding [(control c)(control w)]) 'py-pychecker-run) nil "py-pychecker-run key-binding test failed")
  (assert (eq (key-binding (kbd "TAB")) 'py-indent-line) nil "py-indent-line key-binding test failed")
  (assert (eq (key-binding [(control c)(control p)]) 'py-beginning-of-statement) nil "py-beginning-of-statement key-binding test failed")
  (assert (eq (key-binding [(control c)(control n)]) 'py-end-of-statement) nil "py-end-of-statement key-binding test failed")
  (assert (eq (key-binding [(control j)]) 'py-newline-and-indent) nil "py-newline-and-indent key-binding test failed")
  (assert (eq (key-binding (kbd "RET")) 'py-newline-and-indent) nil "py-newline-and-indent key-binding test failed")
  )

(defun py-smart-operator-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo "))
    (py-bug-tests-intern 'py-smart-operator-base arg teststring)))

(defun py-smart-operator-base ()
  (let ((py-smart-operator-mode-p t))
    (py-smart-operator-mode-on)
    (goto-char 52)
    (save-excursion
      (py-smart-operator-<))
    (assert (looking-at " < ") nil "py-smart-operator-test \"py-smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion
      (py-smart-operator->))
    (assert (looking-at " > ") nil "py-smart-operator-test \"py-smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-%))
    (assert (looking-at " % ") nil "py-smart-operator-test \"py-smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-+))
    (assert (looking-at " \\+ ") nil "py-smart-operator-test \"py-smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator--))
    (assert (looking-at " - ") nil "py-smart-operator-test \"py-smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-*))
    (assert (looking-at " * ") nil "py-smart-operator-test \"py-smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-&))
    (assert (looking-at " & ") nil "py-smart-operator-test \"py-smart-operator-&\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-!))
    (assert (looking-at "! ") nil "py-smart-operator-test \"py-smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-?))
    (assert (looking-at "? ") nil "py-smart-operator-test \"py-smart-operator-?\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-,))
    (assert (looking-at ", ") nil "py-smart-operator-test \"py-smart-operator-,\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-.))
    (assert (looking-at ".") nil "py-smart-operator-test \"py-smart-operator-.\" failed")
    (when py-verbose-p (message "%s" "py-smart-operator-test passed"))))

(defun augmented-assigment-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo "))
    (py-bug-tests-intern 'augmented-assigment-base arg teststring)))

(defun augmented-assigment-base ()
  (let ((py-smart-operator-mode-p t))
    (py-smart-operator-mode-on)
    (goto-char 52)
    (save-excursion
      (py-smart-operator-< 4))
    (assert (looking-at " <= ") nil "augmented-assigment-test \"py-smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion
      (py-smart-operator-> 4))
    (assert (looking-at " >= ") nil "augmented-assigment-test \"py-smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-% 4))
    (assert (looking-at " %= ") nil "augmented-assigment-test \"py-smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-+ 4))
    (assert (looking-at " \\+= ") nil "augmented-assigment-test \"py-smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-- 4))
    (assert (looking-at " -= ") nil "augmented-assigment-test \"py-smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-* 4))
    (assert (looking-at " \\*= ") nil "augmented-assigment-test \"py-smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-& 4))
    (assert (looking-at " &= ") nil "augmented-assigment-test \"py-smart-operator-&\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-! 4))
    (assert (looking-at " != ") nil "augmented-assigment-test \"py-smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (save-excursion (py-smart-operator-? 4))
    (assert (looking-at " \\?= ") nil "augmented-assigment-test \"py-smart-operator-?\" failed")
    ;; (delete-region (point) (line-end-position))
    ;; (save-excursion (py-smart-operator-, 4))
    ;; (assert (looking-at " ,= ") nil "augmented-assigment-test \"py-smart-operator-,\" failed")
    ;; (delete-region (point) (line-end-position))
    ;; (save-excursion (py-smart-operator-. 4))
    ;; (assert (looking-at " .= ") nil "augmented-assigment-test \"py-smart-operator-.\" failed")
    ;; (assert nil "py-smart-operator-test failed")
    (when py-verbose-p (message "%s" "augmented-assigment-test passed"))))

(defun py-smart-operator-repeat-test (&optional arg)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
foo "))
    (py-bug-tests-intern 'py-smart-operator-repeat-base arg teststring)))

(defun py-smart-operator-repeat-base ()
  (let ((py-smart-operator-mode-p t))
    (py-smart-operator-mode-on)
    (goto-char 52)
    (setq last-command nil)
    (save-excursion
      (call-interactively 'py-smart-operator-> t)
      (setq last-command 'py-smart-operator->)
      (setq this-command 'py-smart-operator->)
      ;; (message "%s" this-command-keys-vector)
      (call-interactively 'py-smart-operator->))
    (assert (looking-at " >> ") nil "py-smart-operator-test \"py-smart-operator->\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion
      (call-interactively 'py-smart-operator-<)
      (setq last-command 'py-smart-operator-<)
      (setq this-command 'py-smart-operator-<)
      (call-interactively 'py-smart-operator-<))
    (assert (looking-at " << ") nil "py-smart-operator-test \"py-smart-operator-<\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-%)(setq this-command 'py-smart-operator-%)(setq last-command 'py-smart-operator-%)(py-smart-operator-%))
    (assert (looking-at " %% ") nil "py-smart-operator-test \"py-smart-operator-%\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-+)(setq this-command 'py-smart-operator-+)(setq last-command 'py-smart-operator-+)(py-smart-operator-+))
    (assert (looking-at " \\+\\+ ") nil "py-smart-operator-test \"py-smart-operator-+\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator--)(setq this-command 'py-smart-operator--)(setq last-command 'py-smart-operator--)(py-smart-operator--))
    (assert (looking-at " -- ") nil "py-smart-operator-test \"py-smart-operator--\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-*)(setq this-command 'py-smart-operator-*)(setq last-command 'py-smart-operator-*)(py-smart-operator-*))
    (assert (looking-at " ** ") nil "py-smart-operator-test \"py-smart-operator-*\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-&)(setq this-command 'py-smart-operator-&)(setq last-command 'py-smart-operator-&)(py-smart-operator-&))
    (assert (looking-at " && ") nil "py-smart-operator-test \"py-smart-operator-&\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-!)(setq this-command 'py-smart-operator-!)(setq last-command 'py-smart-operator-!)(py-smart-operator-!))
    (assert (looking-at "!! ") nil "py-smart-operator-test \"py-smart-operator-!\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-?)(setq this-command 'py-smart-operator-?)(setq last-command 'py-smart-operator-?)(py-smart-operator-?))
    (assert (looking-at "\\?\\? ") nil "py-smart-operator-test \"py-smart-operator-?\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-,)(setq this-command 'py-smart-operator-,)(setq last-command 'py-smart-operator-,)(py-smart-operator-,))
    (assert (looking-at ",, ") nil "py-smart-operator-test \"py-smart-operator-,\" failed")
    (delete-region (point) (line-end-position))
    (setq last-command nil)
    (save-excursion (py-smart-operator-.)(setq this-command 'py-smart-operator-.)(setq last-command 'py-smart-operator-.)(py-smart-operator-.))
    (assert (looking-at "..") nil "py-smart-operator-test \"py-smart-operator-.\" failed")
    (when py-verbose-p (message "%s" "py-smart-operator-test passed"))))


(defun py-switch-imenu-index-function-test (&optional arg)
  (interactive "p")
  (let ((teststring python-mode-teststring))
  (py-bug-tests-intern 'py-switch-imenu-index-function-base arg teststring)))

(defun py-switch-imenu-index-function-base ()
  (assert (listp imenu--index-alist) nil "py-switch-imenu-index-function-test failed")
  (assert (py-switch-imenu-index-function) nil "py-switch-imenu-index-function-test failed")
  (assert (listp imenu--index-alist) nil "py-switch-imenu-index-function-test failed"))

(defun py-moves-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (py-bug-tests-intern 'py-bol-moves-base arg teststring)))

(defun py-bol-moves-base ()
  (goto-char 592)
  (assert (eq 561 (py-up-clause-bol)) nil "py-up-clause-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 317 (py-up-block-or-clause-bol)) nil "py-up-block-or-clause-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 234 (py-up-def-bol)) nil "py-up-def-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 1 (py-up-class-bol)) nil "py-up-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 234 (py-up-def-or-class-bol)) nil "py-up-def-or-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 317 (py-up-block-bol)) nil "py-up-block-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 317 (py-up-minor-block-bol)) nil "py-up-minor-block-bol-test of `py-moves-test' failed")
  (goto-char 592)
  (assert (eq 325 (py-up-block)) nil "py-up-block-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 325 (py-up-minor-block)) nil "py-up-minor-block-test of `py-moves-test' failed")
  (goto-char 592)
  (assert (eq 569 (py-up-clause)) nil "py-up-clause-test of `py-moves-test' failed")
  (goto-char 592)
  (assert (eq 569 (py-up-block-or-clause)) nil "py-up-block-or-clause-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 238 (py-up-def)) nil "py-up-def-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 1 (py-up-class)) nil "py-up-class-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 238 (py-up-def-or-class)) nil "py-up-def-or-class-test of `py-moves-test' failed")
  (goto-char 264)
  (assert (eq 317 (py-down-block-bol)) nil "py-down-block-bol-test of `py-moves-test' failed")
  (goto-char 561)
  (assert (eq 594 (py-down-clause-bol)) nil "py-down-clause-bol-test of `py-moves-test' failed")
  (goto-char 264)
  (assert (eq 317 (py-down-block-or-clause-bol)) nil "py-down-block-or-clause-bol-test of `py-moves-test' failed")
  (goto-char (point-min))
  (assert (eq 142 (py-down-def-bol)) nil "py-down-def-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (not (py-down-class-bol)) nil "py-down-class-bol-test of `py-moves-test' failed")
  (goto-char (point-min))
  (assert (eq 142 (py-down-def-or-class-bol)) nil "py-down-def-or-class-bol-test of `py-moves-test' failed")
  (goto-char 264)
  (assert (eq 325 (py-down-block)) nil "py-down-block-test of `py-moves-test' failed")
  (goto-char 264)
  (assert (eq 317 (py-down-block-bol)) nil "py-down-block-bol-test of `py-moves-test' failed")

  (goto-char 264)
  (assert (eq 325 (py-down-minor-block)) nil "py-down-minor-block-test of `py-moves-test' failed")
  (goto-char 264)
  (assert (eq 317 (py-down-minor-block-bol)) nil "py-down-minor-block-bol-test of `py-moves-test' failed")


  (goto-char 569)
  (assert (eq 602 (py-down-clause)) nil "py-down-clause-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 569 (py-down-block-or-clause)) nil "py-down-block-or-clause-test of `py-moves-test' failed")
  (goto-char (point-min))
  (assert (eq 146 (py-down-def)) nil "py-down-def-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (not (py-down-class)) nil "py-down-class-test of `py-moves-test' failed")
  (goto-char (point-min))
  (assert (eq 146 (py-down-def-or-class)) nil "py-down-def-or-class-test of `py-moves-test' failed")

  (goto-char 410)
  (assert (eq 332 (py-beginning-of-statement-bol)) nil "py-beginning-of-statement-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 317 (py-beginning-of-block-bol)) nil "py-beginning-of-block-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq nil (py-beginning-of-clause-bol)) nil "py-beginning-of-clause-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 317 (py-beginning-of-block-or-clause-bol)) nil "py-beginning-of-block-or-clause-bol-test of `py-moves-test' failed")
  (assert (eq 1 (py-beginning-of-class-bol)) nil "py-beginning-of-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 234 (py-beginning-of-def-or-class-bol)) nil "py-beginning-of-def-or-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 627 (py-end-of-block-bol)) nil "py-end-of-block-bol-test of `py-moves-test' failed")
  (goto-char 576)
  (assert (eq 594 (py-end-of-clause-bol)) nil "py-end-of-clause-bol-test of `py-moves-test' failed")
  (goto-char 576)
  (assert (eq 594 (py-end-of-block-or-clause-bol)) nil "py-end-of-block-or-clause-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 627 (py-end-of-def-bol)) nil "py-end-of-def-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 627 (py-end-of-class-bol)) nil "py-end-of-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 627 (py-end-of-def-or-class-bol)) nil "py-end-of-def-or-class-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 561 (py-end-of-statement-bol)) nil "py-end-of-statement-bol-test of `py-moves-test' failed")
  (goto-char 410)
  (assert (eq 234 (py-beginning-of-def-bol)) nil "py-beginning-of-def-bol-test of `py-moves-test' failed")
  )

(defun py-guess-indent-offset-test (&optional arg)
  (interactive "p")
  (let (py-smart-indentation
        (teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

    #####################################
#####################################
def fooBaz( bar ):  # version 2003/9/7
  if \"Foo:\" == bar \\
          or  \"[Foo:]\" == bar \\
          or \"Foo:]\" == bar \\
          or \"Baz:\" == bar:
      return False
  return True
"))
    (py-bug-tests-intern 'py-guess-indent-offset-base arg teststring)))

(defun py-guess-indent-offset-base ()
  (goto-char 49)
  (assert (eq (default-value 'py-indent-offset) (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")
  (goto-char 168)
  (assert (eq 2 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")
  (goto-char 251)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")
  (goto-char 279)
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")
  (goto-char 298)
  ;; indent might be eithe 4 or 2
  (assert (eq 4 (py-guess-indent-offset)) nil "py-guess-indent-offset-test #1 failed")

)
;; imenu--subalist-p
(provide 'python-mode-test)
;;; python-mode-test.el ends here
