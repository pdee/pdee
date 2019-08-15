;; py-ert-tests.el --- Tests, some adapted from python.el -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2015 Andreas Röhler, <andreas.roehler@online.de>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; (require 'ert)

;; tests are expected to run from directory test

;; (add-to-list 'load-path default-directory)

(ert-deftest py-ert-indent-list-style-test ()
  (should py-indent-list-style))

(ert-deftest py-ert-electric-kill-backward-bracket-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring[0:1]"
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\] (char-after))))))

(ert-deftest py-ert-electric-kill-backward-region-test ()
  (py-test-with-temp-buffer
      "mystring[0:1]     "
    (goto-char (point-max))
    (let ((py-electric-kill-backward-p t)
	  (delete-active-region t)
	  (transient-mark-mode t))
      (skip-chars-backward " \t\r\n\f")
      (set-mark (point))
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\] (char-before))))))

(ert-deftest py-ert-electric-delete-eob-test ()
  (py-test-with-temp-buffer
      "mystring[0:1]     "
    (goto-char (point-max))
    (let ((py-electric-kill-backward-p t)
	  (delete-active-region t)
	  (transient-mark-mode t))
      (skip-chars-backward " \t")
      (set-mark (point))
      (skip-chars-forward " \t")
      (py-electric-delete)
      (should (eobp)))))

(ert-deftest py-ert-electric-delete-test ()
  (let ((py-electric-kill-backward-p t)
	(delete-active-region t)
	(transient-mark-mode t))
    (py-test-with-temp-buffer
	"mystring[0:1]     "
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (set-mark (point))
      (skip-chars-backward " \t\r\n\f")
      (py-electric-delete)
      (should (eobp)))))

(ert-deftest py-ert-electric-kill-backward-paren-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring(\"asdf\")"
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\) (char-after)))
      )))

(ert-deftest py-ert-electric-kill-backward-brace-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring{0 . 1}"
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\} (char-after))))))

(ert-deftest py-ert-indent-dedenters-WoWM6j ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-4qujpk ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-OmirYx ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-P9MB72 ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-SFnpJ4 ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-m0FUAw ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())"
    (goto-char (point-max))
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-nK9iWx ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())
        except (IndexError, AttributeError):"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-WRXYEM ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())
        except (IndexError, AttributeError):
            print(c)
        finally:
            print('nor a, nor b are true')"
    (goto-char (point-max))
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-ztz4Yn ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\"
    (goto-char (point-max))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-LlOrDK ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\"
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-nvVJgu ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5"
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-1 ()
  "The most common case."
  (py-test-with-temp-buffer-point-min
      "
from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5
"
    (goto-char (point-min))
    (search-forward "something_2 something_3,")
    (should (= (py-compute-indentation) 5))
    (search-forward "something_4, something_5")
    (should (= (py-compute-indentation) 5))))

(ert-deftest py-ert-bracket-closing-1 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-2 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-3 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-4 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-5 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-6 ()
  ""
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-tx8E5Q ()
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-closing-GOD79x ()
  (py-test-with-temp-buffer
      "result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f')"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-closing-06uwom ()
  (py-test-with-temp-buffer
      "result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f')"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-closing-GOD79x ()
  (py-test-with-temp-buffer
      "result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f')"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-moves-up-class-bol-uzF3su ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-up-class-bol)
    (should (bobp))))

(ert-deftest py-ert-moves-up-def-or-class-bol-iPn4ge ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-up-def-or-class)
    (should (looking-at "class"))))

(ert-deftest py-ert-moves-up-minor-block-bol-sqyjbT ()
  (py-test-with-temp-buffer
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
                'python-expression'])

            if b:
                ar_atpt_python_list_roh = ([
                    'python-expression'])"
    (goto-char (point-max))
    (py-up-minor-block-bol)
    (should (looking-at " +if a:"))))

(ert-deftest py-ert-moves-up-block-bol-u0LDDH ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-up-block-bol)
    (should (looking-at " +def f():"))))

(ert-deftest py-ert-moves-up-block-w7eExs ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-up-block)
    (should  (eq (char-after) ?d))))

(ert-deftest py-ert-moves-backward-block-w7eExs ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-backward-block)
    (should (looking-at "if a:"))))

(ert-deftest py-ert-moves-up-minor-block-bol-2 ()
  (py-test-with-temp-buffer
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
        # if c
            if b:
                pass"
    (goto-char (point-max))
    (py-up-minor-block)
    (should (looking-at "if a:"))))

(ert-deftest py-ert-moves-up-def-bol-wTMxJq ()
  (py-test-with-temp-buffer
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
	    \"\"\""
    (goto-char (point-max))
    (py-up-def-bol)
    (should (looking-at " +def __init__"))))

(ert-deftest py-ert-moves-up-class-bol-hObRZJ ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (py-up-class)
    (should (bolp))))

(ert-deftest py-ert-moves-up-def-or-class-bol-2 ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-up-def-or-class)
    (should (looking-at "class"))))

(ert-deftest py-ert-moves-down-block-bol-1 ()
  (py-test-with-temp-buffer-point-min
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-min))
    (goto-char 264)
    (py-down-block-bol)
    (should (bolp))))

(ert-deftest py-ert-moves-down-def-bol-1 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-down-class-bol-ubMUm6 ()
  (py-test-with-temp-buffer-point-min
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
"
    (goto-char (point-min))
    (py-down-class-bol)
    (should (eobp))))

(ert-deftest py-ert-moves-down-def-or-class-bol-1 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-or-class-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-moves-down-block-1 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-block)
    (should (looking-at "def"))))

(ert-deftest py-ert-moves-down-block-bol-2 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-block-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-moves-down-minor-block-1 ()
  (py-test-with-temp-buffer
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
        if b:
            pass"
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))
    (search-backward "a:")
    (py-down-minor-block)
    (should (eq (char-after) ?i))))

(ert-deftest py-ert-moves-down-minor-block-bol-1 ()
  (py-test-with-temp-buffer
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
        if b:
            pass"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-minor-block-bol)
    (should (bolp))
    (should (looking-at " +if"))))

(ert-deftest py-ert-moves-down-def-1 ()
  (py-test-with-temp-buffer-point-min
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-min))
    (search-forward "def __init__")
    (py-down-def)
    (should (looking-at "def f()"))))

(ert-deftest py-ert-moves-down-def-2 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-moves-down-class-5kkTKq ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (should (not (py-down-class)))))

(ert-deftest py-ert-moves-down-def-or-class-1 ()
  (py-test-with-temp-buffer
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

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-or-class)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-moves-backward-statement-bol-Njn9my ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (should (eq 332 (py-backward-statement-bol)))))

(ert-deftest py-ert-moves-backward-block-bol-x9If7W ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (indent-to 12)
    (should (eq 317 (py-backward-block-bol)))))

(ert-deftest py-ert-moves-backward-clause-bol-RpODhD ()
  (py-test-with-temp-buffer
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
                "
    (goto-char (point-max))
    (should (eq 317 (py-backward-clause-bol)))))

(ert-deftest py-ert-moves-backward-block-or-clause-bol-ZDM7aD ()
  (py-test-with-temp-buffer
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
                                  "
    (goto-char (point-max))
    (should (eq 317 (py-backward-block-or-clause-bol)))))

(ert-deftest py-ert-moves-backward-class-bol-PFB3qC ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (py-backward-class-bol)
    (should (bobp))))

(ert-deftest py-ert-moves-backward-def-or-class-bol-xTvIPv ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-backward-def-or-class-bol)
    (should (looking-at "^ +def"))))

(ert-deftest py-ert-moves-forward-clause-bol ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (goto-char 576)
    (should (eq 594 (py-forward-clause-bol)))))

(ert-deftest py-ert-moves-forward-block-or-clause-bol ()
  (py-test-with-temp-buffer-point-min
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
    (search-forward "elif b:")
    (py-forward-block-or-clause-bol)
    (should (looking-at " +else b:"))))

(ert-deftest py-ert-moves-up-position-tests-12 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]
        if treffer in gruen:
            # print \"0, Gruen\"
        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer
        else:
            # print \"%i, manque\" % (treffer)
            ausgabe[7] = treffer
"
    (goto-char (point-max))
    (search-backward "self")
    (end-of-line)
    (py-forward-statement)
    (should (eq (char-before) ?\]))))

(ert-deftest py-ert-moves-up-copy-statement-test ()
  (py-test-with-temp-buffer-point-min
   "from foo.bar.baz import something
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (py-copy-statement)
   (should (string-match "from foo.bar.baz import something" (car kill-ring)))))

(ert-deftest py-ert-moves-up-honor-dedent-lp-1280982 ()
  (py-test-with-temp-buffer
      "def foo():
    def bar():
        asdf
    "
    (goto-char (point-max))
    (py-newline-and-indent)
    (py-electric-backspace)
    (py-newline-and-indent)
    (should (eq 42 (point)))))

(ert-deftest py-ert-moves-up-fill-paragraph-lp-1286318 ()
  (py-test-with-temp-buffer-point-min
      "# r1416

def baz():
    \"\"\"Hello there.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7

# The last line of the docstring is longer than fill-column (set to
# 78 = for me). Put point on the 'T' in 'This' and hit M-q= . Nothing
# happens.
#
# Another example:
#
def baz():
    \"\"\"Hello there.

    This is a multiline
    function definition.
    Don't worry, be happy.
    Be very very happy.
    Very. happy.
    \"\"\"
    return 7

# All of those lines are shorter than fill-column. Put point anywhere
# = in that paragraph and hit M-q. Nothing happens.
#
# In both cases I would expect to end up with:
#
def baz():
    \"\"\"Hello there.

    This is a multiline function definition. Don= 't worry, be happy. Be very
    very happy. Very. happy.
    \"\"\"
    return 7
"
    (goto-char 49)
    ;; (sit-for 0.1 t)
    (fill-paragraph)
    (end-of-line)
    (should (<= (current-column) 72))
    (goto-char 409)
    (fill-paragraph)
    (end-of-line)
    (should (<= (current-column) 72))
    (goto-char 731)
    (fill-paragraph)
    (end-of-line)
    (should (<= (current-column) 72))
    (search-forward "\"\"\"")
    (forward-line -1)
    ;; (sit-for 0.1 t)
    (should (not (empty-line-p)))

    ))

(ert-deftest py-ert-moves-up-fill-paragraph-pep-257-nn-1 ()
  (let ((py-docstring-style 'pep-257-nn))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      'python-mode
      py-debug-p
      (goto-char 49)
      (py-fill-string)
      (end-of-line)
      ;; (sit-for 0.1 t)
      (should (<= (current-column) 72))
      (forward-line 2)
      (end-of-line)
      (should (<= (current-column) 72))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (fill-paragraph)
      (end-of-line)
      ;; (sit-for 0.1 t)
      (should (<= (current-column) 72))
      )))

(ert-deftest py-ert-moves-up-fill-paragraph-pep-257 ()
  (let ((py-docstring-style 'pep-257))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      'python-mode
      py-debug-p
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
      (goto-char 49)
      (fill-paragraph)
      (end-of-line)
      (should (<= (current-column) 72))
      (forward-line 2)
      (end-of-line)
      (should (<= (current-column) 72))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (should (empty-line-p))
      )))

(ert-deftest py-ert-moves-up-fill-paragraph-onetwo ()
  (let ((py-docstring-style 'onetwo))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      'python-mode
      py-debug-p
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
      (goto-char 49)
      (fill-paragraph)
      (search-backward "\"\"\"")
      (goto-char (match-end 0))
      (eolp)
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (should (empty-line-p)))))

(ert-deftest py-ert-moves-up-fill-paragraph-django-2 ()
  (let ((py-docstring-style 'django))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      'python-mode
      py-debug-p
      (goto-char 49)
      (fill-paragraph)
      (search-forward "\"\"\"")
      (forward-line -2)
      (should (empty-line-p)))))

(ert-deftest py-ert-moves-up-fill-paragraph-symmetric ()
  (let ((py-docstring-style 'symmetric))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      'python-mode
      py-debug-p
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
      (goto-char 49)
      (fill-paragraph)
      (search-backward "\"\"\"")
      (goto-char (match-end 0))
      (eolp)
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (should (not (empty-line-p))))))

(ert-deftest py-partial-expression-test-1 ()
  (py-test-with-temp-buffer-point-min
      "foo=1"
    (goto-char (point-min))
    (and (should (string= "foo" (py-partial-expression)))
	 (py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-partial-expression-test-2 ()
  (py-test-with-temp-buffer-point-min
      "print(root.getchildren()[0])"
    (goto-char (point-min))
    (search-forward "getchildren")
    (and (should (string= "getchildren()[0]" (py-partial-expression)))
	 (py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-ert-moves-up-execute-statement-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-test\")"
    (goto-char (point-min))
    (let ((py-shell-name "python2"))
      (py-execute-statement)
      (set-buffer "*Python2*")
      (goto-char (point-max))
      (and (should (search-backward "py-execute-statement-test" nil t 1))
	   (py-kill-buffer-unconditional (current-buffer))))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-1 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
if True:
    print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (search-forward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max)))
   (should (eq 4 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-2 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
if True:
    print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (search-forward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max)))
   (search-forward "with file")
   (should (eq 8 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-3 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
if True:
    print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (search-forward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max)))
   (search-forward "for i ")
   (should (eq 12 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-4 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
if True:
    print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (search-forward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max)))
   (search-forward "bar.")
   (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-5 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
if True:
    print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        datei.write(str(baz[i]) + \"\\n\")
"
   'python-mode
   py-debug-p
   (goto-char (point-min))
   (search-forward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max)))
   (search-forward "datei.write")
   (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-arg-2-test ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        # called from correct first line
        # wrong indent should to be fixed
            datei.write(str(baz[i]) + \"\\n\")
"
    (goto-char (point-min))
    (search-forward "with file")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max)))
    (should (eq 0 (current-indentation)))
    (search-forward "for i ")
    (should (eq 4 (current-indentation)))
    (search-forward "bar.")
    (should (eq 8 (current-indentation)))
    (search-forward "datei.write")
    (should (eq 8 (current-indentation)))))

(ert-deftest py--pdb-versioned-test-Ft0557 ()
  (let ((py-shell-name "python3"))
    (py-test-with-temp-buffer
	""
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (should (string= "pdb3" (py--pdb-versioned))))))

(ert-deftest py--pdb-versioned-test-QoHSpJ ()
  (let ((py-shell-name "python"))
    (py-test-with-temp-buffer
	""
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (should (string= "pdb" (py--pdb-versioned))))))

(ert-deftest py-ert-moves-up-forward-expression-test ()
    (py-test-with-temp-buffer-point-min
	"class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
      'python-mode
      py-debug-p
      (goto-char (point-min))
      (py-forward-expression)
      (should (eq (char-before) ?s))
      (py-forward-expression)
      (should (eq (char-before) ?:))
      (py-forward-expression)
      (should (eq (char-before) ?t))
      (py-forward-expression)
      (should (eq (char-before) ?\)))
      (py-forward-expression)
      (should (eq (char-before) ?l))
      (py-forward-expression)
      (should (eq (char-before) ?\]))
      (py-forward-expression)
      (should (eq (char-before) ?n))
      (py-forward-expression)
      (should (eq (char-before) ?\]))
      (py-forward-expression)
      (should (eq (char-before) ?t))
      (py-forward-expression)
      (should (eq (char-before) ?\]))
      (py-forward-expression)
      (should (eq (char-before) ?f))
      (py-forward-expression)
      (should (eq (char-before) ?:))
      (py-forward-expression)
      (should (eq (char-before) ?\"))
      (search-forward "fertig")
      (py-forward-expression)
      (should (eq (char-before) ?'))
      (py-forward-expression)
      (should (eq (char-before) ?f))
      (search-forward "__name__")
      (py-forward-expression)
      (should (eq (char-before) ?:))
      (py-forward-expression)
      (should (eq (char-before) ?\)))
      ))

(ert-deftest py-ert-moves-up-backward-expression-test ()
    (py-test-with-temp-buffer
	"class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (py-backward-expression)
      (should (eq (char-after) ?m))
      (py-backward-expression)
      (should (eq (char-after) ?\"))
      (py-backward-expression)
      (should (eq (char-after) ?_))
      (py-backward-expression)
      (should (eq (char-after) ?i))
      (py-backward-expression)
      (should (eq (char-after) ?t))
      (py-backward-expression)
      (should (eq (char-after) ?a))
      (py-backward-expression)
      (should (eq (char-after) ?s))
      (py-backward-expression)
      (should (eq (char-after) ?i))
      (beginning-of-line)
      (search-backward "if")
      (py-backward-expression)
      (should (eq (char-after) ?'))
      (search-backward "ausgabe")
      (py-backward-expression)
      (should (eq (char-after) ?\[))))

(ert-deftest py-ert-which-def-or-class-test-1 ()
  (py-test-with-temp-buffer-point-min
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-min))
    (search-forward "kugel")
    (should (string-match "kugel" (py-which-def-or-class)))
    (search-forward "pylauf")
    (should (string-match "kugel.pylauf" (py-which-def-or-class)))))

(ert-deftest py-ert-which-def-or-class-test-2 ()
  (py-test-with-temp-buffer
      "except AttributeError:

    # To fix reloading, force it to create a new foo
    if hasattr(threading.currentThread(), '__decimal_foo__'):
        del threading.currentThread().__decimal_foo__

    def setfoo(foo):
        \"\"\"Set this thread's foo to foo.\"\"\"
        if foo in (DefaultContext, BasicContext, ExtendedContext):
            foo = foo.copy()
            foo.clear_flags()
        threading.currentThread().__decimal_foo__ = foo

    def getfoo():
        \"\"\"Returns this thread's foo.

        If this thread does not yet have a foo, returns
        \"\"\"
        try:
            return threading.currentThread().__decimal_foo__
        except AttributeError:
            foo = Context()
            threading.currentThread().__decimal_foo__ = foo
            return foo

else:
"
    (goto-char (point-max))
    (should (string= "???" (py-which-def-or-class)))
    (forward-line -3)
    (should (string= "getfoo" (py-which-def-or-class)))))

(ert-deftest py-ert-which-def-or-class-test-3 ()
  (py-test-with-temp-buffer

      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]
    schwarz = [2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35]
    ausgabe = []
    treffer = None
    fertig = ''
    treffer = random.randint(0, 36)

    def foo():
        bar

    def pylauf(self):
"
    (goto-char (point-max))
    (forward-line -2)
    (should (string= "kugel.foo" (py-which-def-or-class)))))

(ert-deftest py-ert-match-paren-test-1 ()
  (py-test-with-temp-buffer
      "if __name__ == \"__main__\":
    main()"
    (goto-char (point-max))
    (forward-char -1)
    (py-match-paren)
    (should (eq (char-after) ?\())))

(ert-deftest py-ert-match-paren-test-2 ()
    (py-test-with-temp-buffer
	"if __name__ == \"__main__\":
    main()"
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (forward-char -2)
      (py-match-paren)
      (should (eq (char-after) ?\)))))

(ert-deftest py-ert-match-paren-test-4 ()
    (py-test-with-temp-buffer
	"if __name__ == \"__main__\":
    main()
    "
      'python-mode
      py-debug-p
      (goto-char (point-max))
      (py-match-paren)
      (should (eq (char-after) ?m))))

(ert-deftest py-ert-match-paren-test-5 ()
    (py-test-with-temp-buffer-point-min
	"if __name__ == \"__main__\":
    main()
    "
      'python-mode
      py-debug-p
      (goto-char (point-min))
      (py-match-paren)
      (should (empty-line-p))
      (py-match-paren)
      (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-7 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
    (skip-chars-backward "^\]")
    (forward-char -1)
    (py-match-paren)
    (should (eq (char-after) ?\[))
    (py-match-paren)
    (should (eq (char-after) ?\]))))

(ert-deftest py-ert-match-paren-test-8 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
      (skip-chars-backward "^:")
      (py-match-paren)
      (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-9 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
      (search-backward "pylauf")
      (py-match-paren)
      (should (eq (char-after) ?\"))
      (py-match-paren)
      (should (eq (char-after) ?\"))
      ))

(ert-deftest py-ert-match-paren-test-faMqA3 ()
  (py-test-with-temp-buffer
      "def main():
    if len(sys.argv) == 1:
        usage()
        sys.exit()
              "
    (goto-char (point-max))
    (search-backward "if")
    (py-match-paren)
    (should (empty-line-p))
    (py-match-paren)
    (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-AOADGb ()
  (py-test-with-temp-buffer-point-min
      "import re
import sys
import os
"
    (goto-char (point-min))
    (py-match-paren)
    (should (looking-at "import sys"))
    (setq last-command 'py-match-paren)
    (py-match-paren)
    (should (looking-at "import re"))))

(ert-deftest py-ert-match-paren-nonempty-test-6 ()
  (py-test-with-temp-buffer
      "def main():
    if len(sys.argv) == 1:
        usage()
        sys.exit()

    class asdf(object):
        zeit = time.strftime('%Y%m%d--%H-%M-%S')

        def Utf8_Exists(filename):
            return os.path.exists(filename.encode('utf-8'))
"
    (goto-char (point-max))
    (search-backward "class")
    (py-match-paren)
    (should (empty-line-p))
    (should (eq 4 (current-column)))
    ))

(ert-deftest py-ert-match-paren-nonempty-test-7 ()
  (py-test-with-temp-buffer
      "try:
    anzahl = int(args[1])
except:
    print \"Setze anzahl auf 1\"
"
    (goto-char (point-max))
    (search-backward "arg")
    (py-match-paren)
    (should (eq (char-after) ?\())))

(ert-deftest py-ert-match-paren-nonempty-test-8 ()
  (py-test-with-temp-buffer
      "try:
    anzahl = int(args[1])
except:
    print \"Setze anzahl auf 1\"
"
    (goto-char (point-max))
    (search-backward " int")
    (py-match-paren)
    (should (eq (char-after) ?a))
    (py-match-paren)
    (should (eq (char-before) 32))
    (should (empty-line-p))
    (should (eq 4 (current-column)))))

(ert-deftest py-ert-match-paren-test-9 ()
  (py-test-with-temp-buffer
      "if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
    (py-match-paren)
    (should (eq (char-after) ?i))))

(ert-deftest py-ert-moves-up-match-paren-test-2 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
    (forward-line -3)
    (indent-to 12)
    (py-match-paren)
    (should (eq (char-after) ?a))))

(ert-deftest py-ert-moves-up-match-paren-test-10 ()
  (py-test-with-temp-buffer
      "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        fertig = ''
#        print \"treffer, schwarz, gruen, rot, pair, impair, passe, manque, spiel\"
        if treffer in gruen:
            # print \"0, Gruen\"
            ausgabe[1] = treffer
            ausgabe[2] = treffer

        elif treffer in schwarz:
            # print \"%i, Schwarz\" % (treffer)
            ausgabe[1] = treffer

if __name__ == \"__main__\":
    main()
"
    (goto-char (point-max))
    (forward-line -3)
    (indent-to 8)
    (py-match-paren)
    (should (eq (char-after) ?e))
    (forward-line 3)
    (should (eolp))))

(ert-deftest py-ert-backward-def-or-class-1 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (py-backward-def-or-class)
    (should (char-equal ?d (char-after)))))

(ert-deftest py-ert-backward-def-or-class-2 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def-or-class)
    (should (char-equal ?d (char-after)))))

(ert-deftest py-ert-backward-def-or-class-3 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (search-backward "def" nil t 2)
    (py-backward-def-or-class)
    (should (char-equal ?c (char-after)))))

(provide 'py-ert-tests-1)
;;; py-ert-tests-1.el ends here
