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
      (if (featurep 'xemacs)
          (list
           'py-beginning-of-block-or-clause-test)
        (list

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
         'tqs-list-error-test
         'UnicodeEncodeError-python3-test

         )))

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

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"
  "String used for tests by python-mode-test.el")

(defun py-beginning-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-block-base arg teststring)))

(defun py-beginning-of-block-base ()
  (goto-char (point-max))
  (py-beginning-of-block)
  (assert (looking-at "if") nil "py-beginning-of-block-test failed"))

(defun py-end-of-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-block-base arg teststring)))

(defun py-end-of-block-base ()
  (py-beginning-of-block)
  (py-end-of-block)
  (assert (eq (point) 556) nil "py-end-of-block-test failed"))

(defun py-beginning-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-block-or-clause-base arg teststring)))

(defun py-beginning-of-block-or-clause-base ()
  (goto-char (point-max))
  (py-beginning-of-block-or-clause)
  (assert (looking-at "if") nil "py-beginning-of-block-or-clause-test failed"))

(defun py-end-of-block-or-clause-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-block-or-clause-base arg teststring)))

(defun py-end-of-block-or-clause-base ()
  (py-beginning-of-block-or-clause)
  (py-end-of-block-or-clause)
  (assert (eq (point) 556) nil "py-end-of-block-or-clause-test failed"))

(defun py-beginning-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-def-base arg teststring)))

(defun py-beginning-of-def-base ()
  (py-beginning-of-def)
  (assert (eq (point) 238) nil "py-beginning-of-def-test failed")
  )

(defun py-end-of-def-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-def-base arg teststring)))

(defun py-end-of-def-base ()
  (py-beginning-of-def)
  (py-end-of-def)
  (assert (eq (point) 556) nil "py-end-of-def-test failed")
  )

(defun py-beginning-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-def-or-class-base arg teststring)))

(defun py-beginning-of-def-or-class-base ()
  (py-beginning-of-def-or-class 4)
  (assert (eq (point) 1) nil "py-beginning-of-def-or-class-test failed"))

(defun py-end-of-def-or-class-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-def-or-class-base arg teststring)))

(defun py-end-of-def-or-class-base ()
  (py-beginning-of-def-or-class t)
  (py-end-of-def-or-class t)
  (assert (eq (point) 556) nil "py-end-of-def-or-class-test failed"))

(defun py-electric-backspace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
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
  (let ((teststring "#! /usr/bin/env python3
# -\*- coding: utf-8 -\*-\n
print('\\xA9')"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'UnicodeEncodeError-python3-base 2 teststring)))

(defun UnicodeEncodeError-python3-base ()
  (let ((comint-use-prompt-regexp t)
        (comint-prompt-regexp py-shell-input-prompt-1-regexp))
    (goto-char 50)
    (push-mark)
    (end-of-line)
    (py-choose-shell)
    (message "%s" py-shell-name)
    (py-execute-region (line-beginning-position) (point))
    ;; (switch-to-buffer (concat "*" (capitalize py-shell-name) "*"))
    (set-buffer (concat "*" (capitalize py-shell-name) "*"))
    (unless (eq (point) (point-max)) (goto-char (point-max)))
    (sit-for 0.1)
    (or (looking-at "©")
        (when (looking-back comint-prompt-regexp)
          (goto-char (1- (match-beginning 0))))
        (sit-for 0.1)))
  (message "%s %s" (point) (buffer-name))
  (assert (looking-back "©") nil "UnicodeEncodeError-python3-test failed"))

(defun dict-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python3
 # -*- coding: utf-8 -*-

class foo(bar):
	\"\"\"baz\"\"\"
       	_some_extensions = {

		'38': 'asd', #  whatever
		'43': 'ddd',
		'45': 'ddd',
	}
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'dict-error-base arg teststring)))

(defun dict-error-base ()
  (goto-char 78)
  (assert (eq 167 (py-end-of-statement)) nil "dict-error-test failed"))

(defun py-expand-abbrev-pst-pdb.set_trace-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
print('\xA9')
pst
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-expand-abbrev-pst-pdb.set_trace-base arg teststring)))

(defun py-expand-abbrev-pst-pdb.set_trace-base ()
  (forward-char -1)
  (expand-abbrev)
  (sit-for 1)
  ;;  (assert (string= (expand-abbrev) "pst") nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  ;; (assert (expand-abbrev) nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))
  (progn (looking-back "pdb.set_trace()")
         (message "Looking back: %s" (match-string-no-properties 0)))
  (assert (looking-back "pdb.set_trace()")
          ;;          (message "%s" (match-string-no-properties 1))
          nil "py-expand-abbrev-pst-pdb.set_trace-test failed"))

(defun near-bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

print u'\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'near-bob-beginning-of-statement-base arg teststring)))

(defun near-bob-beginning-of-statement-base ()
  (goto-char 50)
  (assert (eq 0 (py-compute-indentation)) nil "near-bob-beginning-of-statement-test failed"))

(defun bob-beginning-of-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Foo.py
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'bob-beginning-of-statement-base arg teststring)))

(defun bob-beginning-of-statement-base ()
  (py-beginning-of-statement)
  (assert (eq 1 (point))  "bob-beginning-of-statement-test failed"))

(defun honor-comments-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'honor-comments-indent-base arg teststring)))

(defun honor-comments-indent-base ()
  (goto-char 19)
  (assert (eq 4 (py-compute-indentation)) nil "honor-comments-indent-test failed"))

(defun first-line-offset-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "    #Something.py
    # The purpose of this program is uncertain.
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'first-line-offset-base arg teststring)))

(defun first-line-offset-base ()
  (goto-char 18)
  (assert (eq 4 (py-compute-indentation)) nil "first-line-offset-test failed"))

(defun assignment-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
sammlung = []
"))
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'if-elif-base arg teststring)))

(defun if-elif-base ()
  (goto-char 76)
  (assert (eq 4 (py-compute-indentation)) nil "if-elif.py-test failed"))

(defun if-elif-bob-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if bar in baz:
    print \"0, baz\"
"))
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-clause-base arg teststring)))

(defun py-beginning-of-clause-base ()
  (goto-char 295)
  (assert (eq 267 (py-beginning-of-clause)) "py-beginning-of-clause-test failed"))

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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-clause-base arg teststring)))

(defun py-end-of-clause-base ()
  (goto-char 295)
  (assert (eq 337 (py-end-of-clause)) "py-end-of-clause-test failed"))

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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-beginning-of-expression-base arg teststring)))

(defun py-beginning-of-expression-base ()
  (goto-char 227)
  (assert (eq 221 (py-beginning-of-expression)) nil "py-beginning-of-expression-test failed"))

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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-expression-base arg teststring)))

(defun py-end-of-expression-base ()
  (goto-char 225)
  (assert (eq 232 (py-end-of-expression)) nil "py-beginning-of-expression-test failed"))

(defun py-expression-index-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -\*- coding: utf-8 -\*-
b = a[0].split(':')[1]
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-expression-index-base arg teststring)))

(defun py-expression-index-base ()
  (goto-char 58)
  (assert (eq 71 (py-end-of-expression)) nil "py-expression-index-test failed"))

(defun py-insert-super-python2-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python2
# -*- coding: utf-8 -*-

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        "))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-insert-super-python2-base arg teststring)))

(defun py-insert-super-python2-base ()
  (ignore-errors (py-insert-super))
  (sit-for 0.1)
  (assert (looking-back "super(OrderedDict1, self).__init__(d={})") nil "py-insert-super-python2-test failed"))

(defun py-insert-super-python3-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python3
# -*- coding: utf-8 -*-

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
        "))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-insert-super-python3-base arg teststring)))

(defun py-insert-super-python3-base ()
  (py-insert-super)
  (back-to-indentation)
  (assert (looking-at "super(OrderedDict1, self).__init__(d={})") "py-insert-super-python3-test failed"))

(defun py-indent-after-assigment-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

#####################################
def foo( self, bar=False ):  # version 12345
    title = self.barz.attrs['file'].split('.')[ -1 ]
    if asdf:
"))
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
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
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'eofs-attribut-base arg teststring)))

(defun eofs-attribut-base ()
  (forward-line -2)
  (assert (eq 142 (py-end-of-statement))  nil "eofs-attribut-test failed"))

(defun args-list-first-line-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
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
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'args-list-first-line-indent-base arg teststring)))

(defun args-list-first-line-indent-base ()
  (goto-char 72)
  (assert (eq 4 (py-compute-indentation)) nil "args-list-first-line-indent-test failed"))

(defun py-partial-expression-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
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
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-partial-expression-base arg teststring)))

(defun py-partial-expression-base ()
  (goto-char 99)
  (assert (eq 99 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #1 failed")
  (assert (eq 130 (py-end-of-partial-expression)) nil "py-partial-expression-test #2 failed")
  (goto-char 178)
  (assert (eq 177 (py-beginning-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  (assert (eq 181 (py-end-of-partial-expression)) nil "py-partial-expression-test #3 failed")
  )

(defun py-execute-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True:
    print \"asdf\""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-execute-block-base 2 teststring)))

(defun py-execute-block-base ()
  (beginning-of-line)
  (let ((py-shell-switch-buffers-on-execute nil)
        (py-cleanup-temporary nil))
    (assert (py-execute-block) nil "py-execute-block-test failed")))

(defun multiline-list-indent-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "print [1, 2,
    3, 4]"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'multiline-list-indent-base arg teststring)))

(defun multiline-list-indent-base ()
  (assert (eq 7 (py-compute-indentation)) nil "multiline-list-indent-test failed"))

(defun no-switch-no-split-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

print u'\\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'no-switch-no-split-base arg teststring)))

(defun no-switch-no-split-base ()
  (let ((oldbuf (current-buffer))
        py-split-windows-on-execute py-shell-switch-buffers-on-execute)
    (goto-char 49)
    (push-mark)
    (end-of-line)
    (py-execute-region (line-beginning-position) (point))
    (assert (window-full-height-p) "no-switch-no-split-test failed")
    (assert (eq (current-buffer) oldbuf))))

(defun close-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'close-block-base arg teststring)))

(defun close-block-base ()
  (goto-char 102)
  (assert (eq 4 (py-close-block)) nil "close-block-test failed"))

(defun py-shift-block-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python2
# -*- coding: utf-8 -*-

class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)
         "))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shift-block-base arg teststring)))

(defun py-shift-block-base ()
  (goto-char 237)
  (assert (eq 24 (py-shift-block-right)) nil "py-shift-block-test #1 failed")
  (assert (eq 8 (py-shift-block-left)) nil "py-shift-block-test #1 failed"))

(defun nesting-if-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if foo:
    if bar:
        pass
    else:
        pass
else:
    pass

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nesting-if-test-base arg teststring)))

(defun nesting-if-test-base ()
  (goto-char 105)
  (assert (eq 0 (py-compute-indentation)) nil "nesting-if-test failed"))

(defun py-end-of-print-statement-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def usage():
    print \"\"\"Error: %s
somme errors
\"\"\" % (
          os.path.basename(sys.argv[0]))
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-end-of-print-statement-base arg teststring)))

(defun py-end-of-print-statement-base ()
  (goto-char 66)
  (sit-for 0.1)
  (assert (eq 146 (py-end-of-statement)) nil "py-end-of-print-statement-test failed"))

(defun nested-try-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
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

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-try-base arg teststring)))

(defun nested-try-base ()
  (goto-char 306)
  (assert (eq 8 (py-compute-indentation)) nil "nested-try-test failed"))

(defun nested-if-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
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

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-if-base arg teststring)))

(defun nested-if-base ()
  (goto-char 299)
  (assert (eq 8 (py-compute-indentation)) nil "nested-if-test failed"))

(defun nested-try-finally-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
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

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-try-finally-base arg teststring)))

(defun nested-try-finally-base ()
  (goto-char 431)
  (assert (eq 12 (py-compute-indentation)) nil "nested-try-finally-test failed"))

(defun py-completion-at-point-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
pri"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-completion-at-point-base 2 teststring)))

(defun py-completion-at-point-base ()
  (py-completion-at-point)
  (sit-for 0.1)
  (assert (looking-back "print") nil "py-completion-at-point-test failed"))

(defun py-shell-complete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
impo"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shell-complete-base 2 teststring)))

(defun py-shell-complete-base ()
  (py-shell-complete)
  (sit-for 0.1)
  (assert (looking-back "import") nil "py-completion-at-point-test failed"))

(defun tqs-list-error-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
class foo(bar, baz):
    \"\"\"
    foo is an ABC for matrix containers; i.e.,
    \\\"\\\"\\\"containers of a finite number of orig
\"\"\"
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'tqs-list-error-base 2 teststring)))

(defun tqs-list-error-base ()
  (goto-char 90)
  (sit-for 0.1)
  (assert (eq 175 (py-end-of-statement)) nil "tqs-list-error-test failed"))

;; (defun index-menu-test (&optional arg load-branch-function)
;;   (interactive "p")
;;   (let ((teststring "#! /usr/bin/env python
;; # -*- coding: utf-8 -*-
;;
;; class OrderedDict1(dict):
;;     \"\"\"
;;     This implementation of a dictionary keeps track of the order
;;     in which keys were inserted.
;;     \"\"\"
;;
;;     def __init__(self, d={}):
;;         self._keys = d.keys()
;;         dict.__init__(self, d)
;; "))
;;   (when load-branch-function (funcall load-branch-function))
;;   (py-bug-tests-intern 'index-menu-base arg teststring)))

;; (defun index-menu-base ()
;;   (assert (string= "IM-Python" (nth 2 (nth 2 (cadr (current-local-map))))) nil "index-menu-test failed"))

(provide 'python-mode-test)
;;; python-mode-test.el ends here
