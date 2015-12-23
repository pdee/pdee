;; py-ert-tests-3.el --- Some more Tests

;; Copyright (C) 2014 Andreas Roehler, <andreas.roehler@online.de>

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
      "#! /usr/bin/env python
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
    (let ((py-if-name-main-permission-p t))
      (py-execute-buffer)
      (set-buffer "*Python*")
      (goto-char (point-max))
      (forward-line -1)
      (end-of-line)
      (sit-for 0.2)
      (assert (looking-back "run") nil "py-if-name-main-permission-lp-326620-test #1 failed"))))


(ert-deftest py-ert-intend-try-test ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

        try:"
    (search-forward "try")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-find-definition-test-1 ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

a = sys.argv"
    (search-forward "a = sy")
    (py-find-definition)))

(ert-deftest py-ert-multiple-decorators-test-1 ()
  (py-test-with-temp-buffer
      "@blah
@blub
def foo():
    pass
"
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
    (let* (py-mark-decorators
	   (erg (py-beginning-of-def-or-class)))
      (should (eq 13 erg)))))

;; (ert-deftest py-ert-find-definition-test-2 ()
;;   (py-test-with-temp-buffer
;;       "#! /usr/bin/env python

;; import sys
;; import os

;; def foo ()
;;     pass

;; foo()"
;;     (beginning-of-line)
;;     (sit-for 0.1)
;;     (should (string= "def foo" (py-find-definition)))))

(ert-deftest py-ert-async-backward-block-test ()
  (py-test-with-temp-buffer
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
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
    (forward-line 1)
    (should (eq 4 (py-compute-indentation)))
    (forward-line 3)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-execute-statement-fast-test ()
  (py-test-with-temp-buffer-point-min
      "print(123234)"
    (py-execute-statement-fast)))

(ert-deftest py-ert-fill-comment-test ()
  (py-test-with-temp-buffer-point-min

"class Foo(Bar):
    def baz(self):
        # Given a winning upgrade path, we can ceiling the maximum image number from that path to be applied.  This is useful for image testing purposes.  XXX
        self.assertEqual([str(image.version) for image in state.winner],
                             [])"
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
  (let (py-indent-paren-spanned-multilines-p)
    (search-forward "b")
    (should (eq 8 (py-compute-indentation)))
    (search-forward "def foo():")
    (search-forward "b")
    (setq py-indent-paren-spanned-multilines-p t)
    (should (eq 12 (py-compute-indentation))))))


;; (ert-deftest py-raw-docstring-test-1 ()
;;   (py-test-with-temp-buffer-point-min
;;       "def f():
;;     r\"\"\" This is the docstring for my function.It's a raw docstring because I want to type \\t here, and maybe \\n,for example in LaTeX code like \\tau or \\nu.

;; More docstring here.
;; \"\"\"
;;  pass"
;;     (search-forward "docstring")
;;     (py-backward-statement)
;;     (sit-for 0.1)
;;     (should (eq (char-after) ?r))))

(ert-deftest py-raw-docstring-test-2 ()
  (py-test-with-temp-buffer-point-min
      "def f():
    r\"\"\" This is the docstring for my function.It's a raw docstring because I want to type \\t here, and maybe \\n,for example in LaTeX code like \\tau or \\nu.

More docstring here.
\"\"\"
 pass"
    (let ((py-docstring-style 'pep-257-nn))
      (search-forward "docstring")
      (fill-paragraph)
      (forward-line 1)
      (skip-chars-forward " \t\r\n\f")
      (should (eq 4 (current-indentation))))))

(ert-deftest py-backward-indent-test ()
  (py-test-with-temp-buffer
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (py-backward-indent)
    (should (eq (char-after) ?a))
    (py-backward-indent)
    (should (eq (char-after) ?d))
    (py-backward-indent)
    (should (eq (char-after) ?s))))


(ert-deftest py-forward-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (py-forward-indent)
    (should (eq (char-before) ?:))
    (py-forward-indent)
    (should (eq (char-before) ?:))
    (py-forward-indent)
    (should (eq (char-before) ?s))
    (py-forward-indent)
    (should (eq (char-before) ?:))))


(ert-deftest py-beginning-of-indent-test ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (search-forward "sdfasde")
    (should (not (py--beginning-of-indent-p)))
    (back-to-indentation) 
    (should (py--beginning-of-indent-p))))


(provide 'py-ert-tests-3)
;;; py-ert-tests-3.el ends here
