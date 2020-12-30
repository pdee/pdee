;; py-ert-fill-tests-1.el --- testing python-mode.el -*- lexical-binding: t; -*-

;; Keywords: languages

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

(require 'setup-ert-tests)

(ert-deftest py-ert-fill-comment-test-Byd1i0 ()
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

(ert-deftest py-ert-parens-span-multiple-lines-lp-1191225-test-AkoTP3 ()
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

(ert-deftest py-raw-docstring-test-pep-257-nn-pbqel7 ()
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

(ert-deftest py-ert-backward-indent-test-hJv5Sb-qSF0Qa ()
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
    (should (eq (char-after) ?a))))

(ert-deftest py-ert-forward-indent-test-1-D3Bcke ()
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

(ert-deftest py-ert-beginning-of-indent-p-test-AAX9Kh ()
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

(ert-deftest py-ert-beginning-of-indent-bol-p-test-TqZeal ()
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

(ert-deftest py-ert-copy-indent-test-UbzMto ()
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

(ert-deftest py-ert-delete-indent-test-HhZNOr ()
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

(ert-deftest py-ert-kill-indent-test-ECwA5u ()
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

(ert-deftest py-ert-mark-indent-test-lJ6Hny ()
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

(ert-deftest py-ert-backward-comment-test-OGNbDB ()
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

(ert-deftest py-ert-forward-comment-test-ibueq9-1vqzQE ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        # sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdf")
    (py-forward-comment)
    (should (eq (char-before) ?e))))

(ert-deftest py-ert-else-clause-test-gIyr2H ()
  (py-test-with-temp-buffer
      "def foo()
    if aaa:
        if bbb:
            x = 1
        y = 1
    else:
"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-shift-indent-test-NZCkbL ()
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

(ert-deftest py-ert-list-indent-test-1-48C7hO ()
  (py-test-with-temp-buffer
      "print('test'
          'string'
          'here')"
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 6 (py-compute-indentation)))))

(ert-deftest py-ert-list-indent-test-2-THFplR ()
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

(ert-deftest py-ert-list-indent-test-3-GXE2bT ()
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

(ert-deftest py-ert-list-indent-test-4-hvCk3U ()
  (py-test-with-temp-buffer
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-embedded-def-or-class-test-RrkIDD-Cu8cTW ()
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

(ert-deftest py-ert-wrong-indent-inside-string-lp-1574731-test-P19RGY ()
  (py-test-with-temp-buffer
      "def foo():
    print(\"\"\"

Bar
\"\"\")
"
    (goto-char (point-max))
    (forward-line -3)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-edit-docstring-write-content-back-test-mh1es0 ()
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

(ert-deftest py-ert-nested-def-lp-1594263-test-3Tbta2 ()
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

(ert-deftest py--indent-line-by-line-lp-1621672-GmsSN3 ()
  (py-test-with-temp-buffer
      "def asdf()
     pass"
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min))
    (py-indent-region (point-min) (point-max) t)
    (should (eq 4 (current-indentation)))))

(ert-deftest py--indent-line-by-line-lp-1621672-b-tACrr5 ()
  (py-test-with-temp-buffer
      "    print(\"asdf\")"
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min))
    (py-indent-region (point-min) (point-max) t)
    (should (eq 0 (current-indentation)))))

(ert-deftest py-forward-def-or-class-1-cmHY16 ()
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

(ert-deftest py-forward-def-or-class-9oeKIr-jqD5z8 ()
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

(ert-deftest py-forward-block-1-Ex3K59 ()
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

(ert-deftest py-forward-clause-lp-1630952-1-HFqYAb ()
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

(ert-deftest py-up-block-test-Ek86Xk-GRei3c ()
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

(ert-deftest py-execute-region-no-transmm-test-1-7nmEse ()
  (py-test-with-temp-buffer
      "print(u'\\xA9')"
    (goto-char (point-max))
    (let (transient-mark-mode)
      (push-mark)
      (beginning-of-line)
      (py-shift-region-right)
      (should (eq 4 (current-indentation))))))

(ert-deftest py-forward-statement-test-3-ealLPf ()
  (py-test-with-temp-buffer-point-min
      "print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-min) )
    (py-forward-statement)
    (py-forward-statement)
    (should (eobp))))

(ert-deftest py-execute-import-or-reload-test-ZYUvdh ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os"
    (goto-char (point-max))
    (py-execute-import-or-reload)
    (should t)))

(ert-deftest py-fill-docstring-pep-257-nn-test-ylBRzi ()
  (py-test-with-temp-buffer-point-min
      "def usage():
    ''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''
        pass"
    (font-lock-fontify-region (point-min)(point-max))
    (goto-char (point-min))
    (search-forward "'''")
    (py-fill-string nil 'pep-257-nn)
    (sit-for 0.1) 
    (search-forward "'''")
    (should (eq 4 (current-indentation)))))

;; https://bugs.launchpad.net/python-mode/+bug/1321266
(ert-deftest py-fill-string-lp-1321266-test-f8sTTj ()
  (py-test-with-temp-buffer
      "print(\"%(language)s has %(number)03d quote types. asdf asdf asdf asdfa sasdf asdfasdfasdfasdfasdfasda asd asdfa a asdf asdfa asdf \" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-max))
    (search-backward "asdf")
    (py-fill-string)
    (goto-char (point-min))
    (end-of-line)
    (should (eq (char-before) 92))))

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

(ert-deftest py-ert-fill-comment-test-Byd1i0 ()
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

(ert-deftest py-raw-docstring-test-pep-257-nn-pbqel7 ()
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

(ert-deftest py-fill-docstring-pep-257-nn-test-ylBRzi ()
  (py-test-with-temp-buffer
      "def usage():
    \'\'\' asdf\' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
\'\'\'
        pass"
    (goto-char (point-max))
    ;; (font-lock-fontify-region (point-min)(point-max))
    (goto-char (point-min))
    (search-forward "'''")
    (py-fill-string nil 'pep-257-nn)
    (search-forward "'''")
    (should (eq 4 (current-indentation)))))

;; https://bugs.launchpad.net/python-mode/+bug/1321266
(ert-deftest py-fill-string-lp-1321266-test-f8sTTj ()
  (py-test-with-temp-buffer
      "print(\"%(language)s has %(number)03d quote types. asdf asdf asdf asdfa sasdf asdfasdfasdfasdfasdfasda asd asdfa a asdf asdfa asdf \" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-max))
    (search-backward "asdf")
    (py-fill-string)
    (goto-char (point-min))
    (end-of-line)
    (should (eq (char-before) 92))))

(ert-deftest py-fill-singlequoted-string-test-zeKa2U ()
  (py-test-with-temp-buffer
      "asd = 'asdf asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf asdf asdf asdf '"
    (goto-char (point-max))
    (backward-char 2)
    (py-fill-string)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?'))
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\\))))

(ert-deftest py-fill-doublequoted-string-test-Xi6FaW ()
  (py-test-with-temp-buffer
      "asd = \"asdf asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf asdf asdf asdf \""
    (goto-char (point-max))
    (backward-char 2)
    (py-fill-string)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\"))
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\\))))

(ert-deftest py-fill-paragraph-LEON2Q ()
  (py-test-with-temp-buffer
      "r\'\'\'aaa

this is a test this is a test this is a test this is a test this is a test this k
is a test

\'\'\'"
    (goto-char (point-max))
    (search-backward "k")
    (end-of-line)
    (py-fill-paragraph)
    (search-backward "'''")
    (forward-line 1)
    (should-not (eq (char-after) ?\\))))

(ert-deftest py-fill-comment-test-MQfKpX ()
  (py-test-with-temp-buffer
      "def foo():
    # asdf asdf adf adf adsf adsf adsf adf adf adf ad adf adf adf adf"
      (goto-char (point-max))
      (turn-on-auto-fill)
      (insert " ")
      (insert "asd")
      (py-fill-string-or-comment)
      (should (eq 9 (current-column)))))

(ert-deftest py-fill-comment-test-64-kGN9tr ()
  (py-test-with-temp-buffer
      "def foo():
    #r# asdf asdf adf adf adsf adsf adsf adf adf adf ad adf adf adf adf"
    (goto-char (point-max))
    (turn-on-auto-fill)
    (let ((comment-start-skip "^[ 	]*#r#+ *")
	  (comment-start "#r#"))
      (insert " ")
      (insert "asd")
      (py-fill-string-or-comment)
      (should (eq 15 (current-column))))))

(ert-deftest py-fill-string-test-75-kGN9tr ()
  (py-test-with-temp-buffer
      "def foo():
    try:
        run()
    except Timeout:
        print('foo
    # Things are no good.
    for line in proc.stdout.splitlines():
        mo = CRE.match(line)
        version = mo['version']"
    (goto-char (point-max))
    (search-backward "foo")
    (end-of-line)
    (insert " ")
    (py-fill-string-or-comment)
    (should (eolp))))

(ert-deftest py-fill-string-test-75-QkyOzd ()
  (py-test-with-temp-buffer
      "def test():
    \"a b"
    (auto-fill-mode 1)
    (goto-char (point-max))
    (insert " ")
    (py-fill-string-or-comment)
    (should (eq 9 (current-column)))))

(ert-deftest py-fill-string-test-75-f0mU3i ()
  (py-test-with-temp-buffer
      "https://github\.com/swig/swig/issues/889
def foo(rho, x):
    r\"\"\"Calculate :math:\`D^\\nu \\rho(x)"
    (auto-fill-mode 1)
    (goto-char (point-max))
    (insert " ")
    (py-fill-string-or-comment)
    (should (equal 39 (current-column)))))

(provide 'py-ert-fill-tests-1)
;;; py-ert-fill-tests-1.el ends here
