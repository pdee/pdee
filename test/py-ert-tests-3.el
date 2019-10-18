;; py-ert-tests-3.el --- Some more Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Andreas Röhler, <andreas.roehler@online.de>

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

;; tests are expected to run from directory test

(add-to-list 'load-path default-directory)
(load "py-ert-tests-1.el" nil t)

;; py-if-name-main-permission-p
(ert-deftest py-ert-if-name-main-permission-lp-326620-test ()
  (py-test-with-temp-buffer-point-min
   "#! /usr/bin/env python2
# -*- coding: utf-8 -*-
def py_if_name_main_permission_test():
    if __name__ == \"__main__\" :
        print(\"__name__ == '__main__' run\")
        return True

    else:
        print(\"__name__ == '__main__' supressed\")
        return False

py_if_name_main_permission_test()
"
   (goto-char (point-min))
   (let ((py-if-name-main-permission-p t))
     (py-execute-buffer-python2)
     (set-buffer "*Python2*")
     (goto-char (point-max))
     (should (search-backward "run" nil t)))))

(ert-deftest py-ert-indent-try-test ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

        try:"
    (goto-char (point-min) )
    (search-forward "try")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-multiple-decorators-test-1 ()
  (py-test-with-temp-buffer
      "@blah
@blub
def foo():
    pass
"
    (goto-char (point-max))
    (let ((py-mark-decorators t))
      (py-beginning-of-def-or-class)
      (should (bobp)))))

(ert-deftest py-ert-multiple-decorators-test-2 ()
  (py-test-with-temp-buffer
      "@blah
@blub
def foo():
    pass
"
    (goto-char (point-max))
    (let* (py-mark-decorators
           (erg (py-beginning-of-def-or-class)))
      (should (eq 13 erg)))))

(ert-deftest py-ert-async-backward-block-test ()
  (py-test-with-temp-buffer
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    (goto-char (point-max))
    (py-backward-block)
    (should (looking-at "async with"))))

(ert-deftest py-ert-async-backward-def-test ()
  (py-test-with-temp-buffer
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    (goto-char (point-max))
    (py-backward-def)
    (should (looking-at "async def"))))

(ert-deftest py-ert-async-indent-test ()
  (py-test-with-temp-buffer-point-min
      "async def coro(name, lock):

    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    (goto-char (point-min) )
    (forward-line 1)
    (should (eq 4 (py-compute-indentation)))
    (forward-line 3)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-fill-comment-test ()
  (py-test-with-temp-buffer-point-min
      "class Foo(Bar):
    def baz(self):
        # Given a winning upgrade path, we can ceiling the maximum image number from that path to be applied.  This is useful for image testing purposes.  XXX
        self.assertEqual([str(image.version) for image in state.winner],
                             [])"
    (goto-char (point-min))
    (search-forward "XXX")
    (fill-paragraph)
    (search-forward "self")
    (back-to-indentation)
    (should (eq 8 (current-column)))
    (should (eq 6 (count-lines (point-min) (point))))))

(ert-deftest py-ert-parens-span-multiple-lines-lp-1191225-test ()
  (py-test-with-temp-buffer-point-min
      "# -*- coding: utf-8 -*-
def foo():
    if (foo &&
        baz):
        bar()
# >> This example raises a pep8 warning[0],
# >> I've been dealing with it and manually
# >> adding another indentation level to not leave 'baz' aligned with 'baz
# ()'
# >>
def foo():
    if (foo &&
            baz):
        bar()
"
    (goto-char (point-min) )
    (let ((py-indent-list-style 'one-level-from-first-element))
      (search-forward "b")
      (should (eq 12 (py-compute-indentation))))))

(ert-deftest py-raw-docstring-test-pep-257-nn ()
  (py-test-with-temp-buffer-point-min
      "def f():
    r\"\"\" This is the docstring for my function.It's a raw docstring because I want to type \\t here, and maybe \\n,for example in LaTeX code like \\tau or \\nu.

More docstring here.
\"\"\"
 pass"
    (goto-char (point-min) )
    (let ((py-docstring-style 'pep-257-nn))
      (search-forward "docstring")
      (fill-paragraph)
      (forward-line 1)
      (skip-chars-forward " \t\r\n\f")
      (should (eq 4 (current-indentation))))))

(ert-deftest py-ert-backward-indent-test ()
  (py-test-with-temp-buffer
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (goto-char (point-max))
    (py-backward-indent)
    (should (eq (char-after) ?a))
    (py-backward-indent)
    (should (eq (char-after) ?d))
    (py-backward-indent)
    (should (eq (char-after) ?s))))

(ert-deftest py-ert-forward-indent-test-1 ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (goto-char (point-min) )
    (search-forward "sdf")
    (py-forward-indent)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-beginning-of-indent-p-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (should (not (py--beginning-of-indent-p)))
    (py-backward-indent)
    (should (py--beginning-of-indent-p))))

(ert-deftest py-ert-beginning-of-indent-bol-p-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (should (not (py--beginning-of-indent-bol-p)))
    (beginning-of-line)
    (should (py--beginning-of-indent-bol-p))))

(ert-deftest py-ert-copy-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-copy-indent)
    (should (string-match "sdfasde" (car kill-ring)))
    (should (not (py--beginning-of-indent-p)))
    (py-backward-statement)
    (should (py--beginning-of-indent-p))))

(ert-deftest py-ert-delete-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-delete-indent)
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-kill-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-kill-indent)
    (should (string= (concat (make-string 8 ?\ ) "sdfasde\n" (make-string 8 ?\ ) "pass") (car kill-ring)))
    (should (eobp))
    (should (bolp))))

(ert-deftest py-ert-mark-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-mark-indent)
    ;; (message "%s" (buffer-substring-no-properties (region-beginning) (region-end)))
    (should (eq 28 (length (buffer-substring-no-properties (region-beginning) (region-end)))))))

(ert-deftest py-ert-backward-comment-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        # sdfasde
        # sdfasde
        # sdfasde
        print(123)"
    (goto-char (point-min) )
    (search-forward "sdfasde" nil t 3)
    (py-backward-comment)
    (should (eq 43 (point)))))

(ert-deftest py-ert-forward-comment-test-ibueq9 ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        # sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdf")
    (py-forward-comment)
    (should (eq (char-before) ?e))))

(ert-deftest py-ert-else-clause-test ()
  (py-test-with-temp-buffer
      "def foo()
    if aaa:
        if bbb:
            x = 1
        y = 1
    else
"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-shift-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        sdfasde
        sdfasde
        print(123)"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (py-shift-indent-right)
    (should (eq 12 (current-indentation)))
    (py-shift-indent-left)
    (should (eq 8 (current-indentation)))))

(ert-deftest py-ert-list-indent-test-1 ()
  (py-test-with-temp-buffer
      "print('test'
          'string'
          'here')"
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 6 (py-compute-indentation)))))

(ert-deftest py-ert-list-indent-test-2 ()
  (py-test-with-temp-buffer
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):
    # Yes, so break the lock.
    self._break()
    log.error('lifetime has expired, breaking')"
    (goto-char (point-max))
    (let ((py-indent-list-style 'line-up-with-first-element))
      (search-backward "datetime.datetime.now")
      (indent-line-to (py-compute-indentation))
      (should (eq 4 (current-indentation))))))

(ert-deftest py-ert-list-indent-test-3 ()
  (py-test-with-temp-buffer
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):
    # Yes, so break the lock.
    self._break()
    log.error('lifetime has expired, breaking')"
    (goto-char (point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (search-backward "datetime.datetime.now")
      (indent-line-to (py-compute-indentation))
      (should (eq 8 (current-indentation))))))

(ert-deftest py-ert-list-indent-test-4 ()
  (py-test-with-temp-buffer
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-embedded-def-or-class-test-RrkIDD ()
  (py-test-with-temp-buffer
      " class Foo:
    def bar(self):
        print(\"\"\"
This is
a nested
string.
\"\"\")
        return True"
    (goto-char (point-max))
    (py-backward-def-or-class)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-wrong-indent-inside-string-lp-1574731-test ()
  (py-test-with-temp-buffer
      "def foo():
    print(\"\"\"

Bar
\"\"\")
"
    (goto-char (point-max))
    (forward-line -3)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-edit-docstring-write-content-back-test ()
  (py-test-with-temp-buffer-point-min
      "def foo():
    \"\"\"def bar():
    pass\"\"\"
    pass
"
    (goto-char (point-min) )
    (let ((py-edit-buffer "Edit docstring"))
      (search-forward "pass" nil t 1)
      (py-edit-docstring)
      (set-buffer py-edit-buffer)
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (end-of-line)
      (newline)
      (insert "'''My edit-docstring ert-test'''")
      (beginning-of-line)
      (indent-according-to-mode)
      (py--write-edit)
      ;; back in orginial test buffer
      (forward-line -1)
      (should (and (nth 3 (parse-partial-sexp (point-min) (point)))
                   (nth 8 (parse-partial-sexp (point-min) (point)))))
      )))

(ert-deftest py-ert-nested-def-lp-1594263-test ()
  (py-test-with-temp-buffer
      "def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
   pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(\*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(\*args)
            print 'After f(\*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (forward-line -1)
    (back-to-indentation)
    (py-backward-def-or-class)
    (should (looking-at "def wwrap"))))

(ert-deftest py--indent-line-by-line-lp-1621672 ()
  (py-test-with-temp-buffer
      "def asdf()
     pass"
    (goto-char (point-max))
    (py-indent-region (point-min) (point-max))
    (should (eq 4 (current-indentation)))))

(ert-deftest py--indent-line-by-line-lp-1621672-b ()
  (py-test-with-temp-buffer
      "    print(\"asdf\")"
    (goto-char (point-max))
    (py-indent-region (point-min) (point-max))
    (should (eq 0 (current-indentation)))))

(ert-deftest py-forward-def-or-class-1 ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (search-backward "args)'")
    (py-forward-def-or-class)
    (should (eq (char-before) ?'))
    (py-forward-def-or-class)
    (should (eq (char-before) ?f))))

(ert-deftest py-forward-def-or-class-9oeKIr ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (goto-char (point-max))
    (search-backward "'")
    (forward-char 1)
    (py-forward-def-or-class)
    (should (eq (char-before) ?f))))

(ert-deftest py-forward-block-1 ()
  (py-test-with-temp-buffer-point-min
      "if True:
    def bar():
        pass
elif False:
    def baz():
        pass
else:
    try:
        1 == 1
    except True:
        def foo1():
            if True:
                def bar1():
                    pass
            elif False:
                def baz1():
                    pass
            else:
                try:
                    1 == 1
                except True:
                    pass
                else True:
                    pass
                finally:
                    pass
    else True:
        pass
    finally:
        pass"
    (goto-char (point-min))
    (py-forward-block)
    (should (eobp))))

(ert-deftest py-forward-clause-lp-1630952-1 ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (search-backward "args)'")
    (py-forward-clause)
    (should (eq (char-before) ?'))))

(ert-deftest py-up-block-test-Ek86Xk ()
  (py-test-with-temp-buffer
      "
def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass
    else:
        try:
            1 == 1
        except True:
        "
    (goto-char (point-max))
    (py-up-block)
    (should (looking-at "if True:"))))

(ert-deftest py-execute-region-no-transmm-test-1 ()
  (py-test-with-temp-buffer
      "print(u'\\xA9')"
    (goto-char (point-max))
    (let (transient-mark-mode)
      (push-mark)
      (beginning-of-line)
      (py-shift-region-right)
      (should (eq 4 (current-indentation))))))

(ert-deftest py-forward-statement-test-3 ()
  (py-test-with-temp-buffer-point-min
      "print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-min) )
    (py-forward-statement)
    (py-forward-statement)
    (should (eobp))))

(ert-deftest py-execute-import-or-reload-test ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os"
    (goto-char (point-max))
    (py-execute-import-or-reload)
    (should t)))

(ert-deftest py-fill-docstring-pep-257-nn-test ()
  (py-test-with-temp-buffer
      "def usage():
    \'\'\' asdf\' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
\'\'\'
        pass"
    (goto-char (point-max))
    (font-lock-fontify-region (point-min)(point-max))
    (goto-char (point-min))
    (search-forward "'''")
    (py-fill-string nil 'pep-257-nn)
    (search-forward "'''")
    (should (eq 4 (current-indentation)))))

;; https://bugs.launchpad.net/python-mode/+bug/1321266
(ert-deftest py-fill-string-lp-1321266-test ()
  (py-test-with-temp-buffer
      "print(\"%(language)s has %(number)03d quote types. asdf asdf asdf asdfa sasdf asdfasdfasdfasdfasdfasda asd asdfa a asdf asdfa asdf \" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-max))
    (search-backward "asdf")
    (py-fill-string)
    (goto-char (point-min))
    (end-of-line)
    (should (eq (char-before) 92))))

(ert-deftest py-syntax-highlighting-for-builtin-functions-55-test ()
  (py-test-with-temp-buffer
      "range(len(list((1, 2, 3))))"
    (goto-char (point-max))
    ;; (goto-char (point-max))
    (font-lock-fontify-region (point-min) (point-max))
    (sit-for 0.1)
    (search-backward "le")
    (should (face-equal (face-at-point) 'py-builtins-face))))

(ert-deftest py-backward-clause-test-p52Dcj ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (search-backward "elif")
    (skip-chars-backward " \t\r\n\f")
    (py-backward-clause)
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py-indentation-after-an-explicit-dedent-61-test-lpYaIp ()
  (py-test-with-temp-buffer
      "mport sys

def main():
    if len(sys.argv) == 2:
        print('hey')

    x = 7
"
    (goto-char (point-max))
    (goto-char (point-max))
    (should (eq 4  (py-compute-indentation)))))

(ert-deftest py-backward-clause-test-p52Dcj ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (search-backward "elif")
    (skip-chars-backward " \t\r\n\f")
    (py-backward-clause)
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py-backward-clause-test-hPywHz ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (sit-for 0.1)
    (search-backward "elif")
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py-shell-dedicated-buffer-test-t3Sizn ()
  (let ((buffer (py-shell nil nil t)))
  (should (buffer-live-p buffer))))

(ert-deftest py-shell-fontify-test-t3Sizn ()
  (let ((buffer (py-shell nil nil t)))
    (with-current-buffer buffer
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (insert "def")
      (backward-char)
      (should (eq (char-after) ?f)))))

(ert-deftest py--beginning-of-assignment-p-test-yJqNFd ()
  (py-test-with-temp-buffer-point-min
      "afd = \"asdf\""
    (goto-char (point-min))
    (search-forward "f")
    (should-not (py--beginning-of-assignment-p))))

(ert-deftest py--beginning-of-assignment-p-test-nnyBdy ()
  (py-test-with-temp-buffer-point-min
      "afd = \"asdf\""
    (goto-char (point-min))
    (should (py--beginning-of-assignment-p))))

(ert-deftest py-beginning-of-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "a, b, c = (1, 2, 3)"
    (goto-char (point-max))
    (should (py-beginning-of-assignment))))

(ert-deftest py-end-of-assignment-test-nnyBdy()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
      (py-end-of-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-end-of-assignment-test-wQIiGk()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
	(search-forward "b") 
      (py-end-of-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-end-of-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
	(search-backward "2") 
      (py-end-of-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-backward-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "print('%(language)s has %(number)03d quote types\.' %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-max))
    (should-not (py-backward-assignment))))

(ert-deftest py-backward-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "a, b, c = (1, 2, 3)"
    (goto-char (point-max))
    (should (py-backward-assignment))))

(ert-deftest py-forward-assignment-test-nnyBdy()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-wQIiGk()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
	(search-forward "b") 
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
	(search-backward "2") 
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
      (goto-char (point-max)) 
      (py-forward-assignment)
      (should (eq (char-before) ?\]))))
    
(provide 'py-ert-tests-3)
;;; py-ert-tests-3.el ends here
