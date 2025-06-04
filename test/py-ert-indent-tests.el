;; py-ert-indent-tests.el --- testing python-mode.el -*- lexical-binding: t; -*-

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

;; (setq py-verbose-p t)

(require 'py-setup-ert-tests)

(ert-deftest py-ert-indent-list-style-test-UVqzej ()
  (should py-indent-list-style))

(ert-deftest py-ert-indent-dedenters-WoWM6j-YhnDUf ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-4qujpk-8S5Su6 ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-OmirYx ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-P9MB72-6D8DxN ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-SFnpJ4-GyGW0D ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-m0FUAw-ohKXqu ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-0sEVRk ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())
        except (IndexError, AttributeError):"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-F9didu ()
  "Check all dedenters."
  (py-test
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)
    else:
        try:
            print(c.pop())
        except (IndexError, AttributeError):
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-WRXYEM-aKmUgb ()
  "Check all dedenters."
  (py-test
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
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-ztz4Yn-cTO9B1 ()
  (py-test
      "from foo.bar.baz import something, something_1 \\"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-LlOrDK-sHqVVR ()
  (py-test
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-nvVJgu-QTa6cI ()
  (py-test
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-orr7uy ()
  "The most common case."
  (py-test-point-min
      "
from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "something_2 something_3,")
    (should (= (py-compute-indentation) 5))
    (search-forward "something_4, something_5")
    (should (= (py-compute-indentation) 5))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-wF0CYZ ()
  "Indent line-by-line as first line is okay "
  (py-test-point-min
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
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (search-backward "True")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max) t))
    (search-forward "with file")
    (should (eq 8 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-yAzv0R ()
  "Indent line-by-line as first line is okay "
  (py-test
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
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (search-backward "True")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max) t))
    (search-forward "for i ")
    (should (eq 12 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-MvlWZJ ()
  "Indent line-by-line as first line is okay "
  (py-test
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
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (search-backward "True")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max) t))
    (search-forward "bar.")
    (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-cXQ3WB ()
  "Indent line-by-line as first line is okay "
  (py-test
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
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (search-backward "True")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max) t))
    (search-forward "datei.write")
    (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-arg-test-gDZcSt ()
  "Indent line-by-line as first line is okay "
  (py-test
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        # called from correct first line
        # wrong indent should to be fixed
            datei.write(str(baz[i]) + \"\\n\")
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (set-mark (point))
    (search-backward "with file")
    (save-excursion
      (py-indent-region (point) (point-max) t))
    (should (eq 0 (current-indentation)))
    (search-forward "for i ")
    (should (eq 4 (current-indentation)))
    (search-forward "bar.")
    (should (eq 8 (current-indentation)))
    (search-forward "datei.write")
    (should (eq 8 (current-indentation)))))

(ert-deftest py-compute-indentation-after-import-test-XvL29H ()
  (py-test
      "import pdb
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-compute-indentation-bob-test-HcSwMS ()
  (py-test-point-min
      "def foo():
    if True:
        pass
    else:
        pass
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-ld9am7 ()
  (py-test
      "
asdf = {
    'a':{"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-Pg4yts ()
  (py-test
      "
asdf = {
    'a':{"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (forward-char -2)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-2qSrrt ()
  (py-test
      "
asdf = {
    'a':{
         'b':3,
         'c':4"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (beginning-of-line)
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-CNMePW ()
  (py-test
      "
asdf = {
    'a':{
         'b':3,
         'c':4"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-I73t1i ()
  (py-test
      "
asdf = {
    'a':{
         'b':4,
         'c':5"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-9UP0Kr ()
  (py-test
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
         }"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 8 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-ABORqFr ()
  (py-test
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
         }"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 8 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-NzoaiZ ()
  (py-test
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
        }
"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-10-uGdWJg ()
  (py-test
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-11-3JVKy7 ()
  (py-test
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-12-ygoJUM4 ()
  (py-test
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-8L9T1k ()
  (py-test
      "(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    'python-mode
    'py-verbose-p
    (let ((py-indent-list-style 'line-up-with-first-element))
      (goto-char (point-max))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-list-indent-test-THFplR ()
  (py-test
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):
    # Yes, so break the lock.
    self._break()
    log.error('lifetime has expired, breaking')"
    'python-mode
    'py-verbose-p
    (let ((py-indent-list-style 'line-up-with-first-element))
      (goto-char (point-max))
      (search-backward "datetime.datetime.now")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-akkTLs ()
  (py-test
      "packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (search-backward "d")
    (let ((py-indent-list-style 'line-up-with-first-element))
      (should (eq 16 (py-compute-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-pGszAP ()
  (py-test
      "def foo (a,\n):"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (search-backward ")")
    (let ((py-indent-list-style 'line-up-with-first-element))
      (should (eq 9 (py-compute-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-3kW4RJ ()
  (py-test
      "def foo (
a):"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (search-backward ")")
    (let ((py-indent-list-style 'line-up-with-first-element))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-2TmrDT ()
  (py-test
      "def foo (a,\n):"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-to-beginning-of-statement))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-kYc1wJ ()
  (py-test
      "def foo (\na,"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-to-beginning-of-statement))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-close-block-test-8LPQD3 ()
  (py-test-point-min
      "# -*- coding: utf-8 -*-
def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 4 (py-close-block)))))

(ert-deftest py-ert-indent-in-arglist-test-euyfAZ ()
  (py-test
      "def foo (a,\n):"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element)
          py-closing-list-dedents-bos)
      (should (eq 13 (py-compute-indentation))))))

(ert-deftest py-ert-close-def-or-class-test-7Kc5SQ ()
  (py-test-point-min
      "# -*- coding: utf-8 -*-
def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-def-or-class)))))

(ert-deftest py-ert-close-def-test-nKOJaZ ()
  (py-test-point-min
      "# -*- coding: utf-8 -*-
def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-def)))))

(ert-deftest py-ert-close-class-test-FPi2i3 ()
  (py-test-point-min
      "# -*- coding: utf-8 -*-
class asdf:
    def main():
        if len(sys.argv)==1:
            usage()
            sys.exit()
    if __name__==\"__main__\":
        main()
"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-class)))))

(ert-deftest py-ert-dedent-forward-test-61dWA6 ()
  (py-test
      "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
        datei.write(str(spiel[i]) + \"\\n\")"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (skip-chars-backward " \t\r\n\f")
    (py-dedent-forward-line)
    (should (py-empty-line-p))
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-async-backward-block-test-aAFOA0 () ()
             (py-test
                 "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
               'python-mode
               'py-verbose-p
               (goto-char (point-max))
               (py-backward-block)
               (should (looking-at "async with"))))

(ert-deftest py-ert-indent-try-test-zg6QYI ()
  (py-test-point-min
      "#! /usr/bin/env python
import sys
import os
        try:"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (search-forward "try")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-indent-closing-tx8E5Q ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6
    ]
"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-1vO0ER ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6
    7, 8, 9]
"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (skip-chars-backward " \t\r\n\f")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-OUClAx ()
  (py-test
      "
my_list = [1, 2, 3,
    4, 5, 6
    ]"
    'python-mode
    'py-verbose-p
    (let (py-closing-list-dedents-bos)
      (goto-char (point-max))
      (search-backward "4")
      ;; line-up-with-first-element (default)
      (should (eq 11 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-EcP0BS ()
  (py-test
      "
my_list = [1, 2, 3,
    4, 5, 6
    ]"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (search-backward "4")
      ;; line-up-with-first-element (default)
      (should (eq 11 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-vmqBXD ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6
    ]"
    'python-mode
    'py-verbose-p
    (let ((py-indent-list-style 'one-level-to-beginning-of-statement))
      (goto-char (point-max))
      (search-backward "]")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-0yQS1n ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6
    ]"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (search-backward "]")
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-Juf3lq ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6]"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t)
          (py-indent-list-style 'one-level-to-beginning-of-statement))
      (goto-char (point-max))
      (search-backward "]")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-6IYou0 ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6]"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t)
          (py-indent-list-style 'line-up-with-first-element))
      (goto-char (point-max))
      ;; previous line matters
      (search-backward "]")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-indent-closing-TGnvyS ()
  (py-test
      "
my_list = [
    1, 2, 3,
    4, 5, 6]"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (search-backward "3")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-async-indent-test-MFS8IW ()
  (py-test-point-min
      "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (forward-line 1)
    (should (eq 4 (py-compute-indentation)))
    (forward-line 3)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-parens-span-multiple-lines-lp-1191225-test-AkoTP3 ()
  (py-test-point-min
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
    'python-mode
    'py-verbose-p
    (goto-char (point-min) )
    (let ((py-indent-list-style 'one-level-from-first-element))
      (search-forward "b")
      (should (eq 12 (py-compute-indentation))))))

(ert-deftest py-ert-indent-else-clause-test-gIyr2H ()
  (py-test
      "def foo()
    if aaa:
        if bbb:
            x = 1
        y = 1
    else:
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-list-indent-test-48C7hO ()
  (py-test
      "print('test'
          'string'
          'here')"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "s")
    (should (eq 6 (py-compute-indentation)))))

(ert-deftest py-ert-list-indent-test-GXE2bT ()
  (py-test
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):
    # Yes, so break the lock.
    self._break()
    log.error('lifetime has expired, breaking')"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (search-backward "datetime.datetime.now")
      (should (eq 8 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-70Eccx ()
  (py-test
      "(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (skip-chars-backward " \t\r\n\f")
    (let ((py-indent-list-style 'one-level-from-first-element))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-umQ6Tk ()
  (py-test
      "( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-list-indent-test-hvCk3U ()
  (py-test
      "if (release_time != -1 and
    datetime.datetime.now() > release_time + CLOCK_SLOP):"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py--indent-line-by-line-lp-1621672-GmsSN3 ()
  (py-test
      "def asdf()
     pass"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min))
    (py-indent-region (point-min) (point-max) t)
    (should (eq 4 (current-indentation)))))

(ert-deftest py--indent-line-by-line-lp-1621672-b-tACrr5 ()
  (py-test
      "    print(\"asdf\")"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (goto-char (point-min))
    (py-indent-region (point-min) (point-max) t)
    (should (eq 0 (current-indentation)))))

(ert-deftest py-indentation-after-an-explicit-dedent-61-test-lpYaIp ()
  (py-test
      "mport sys
def main():
    if len(sys.argv) == 2:
        print('hey')
    x = 7
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-clause-indent-test-UXZsX9 ()
  (py-test
      "def ziffernraten ()
    ziffer = random.randint(1,20)
    guess = 0
    tries = 0
    print('Try to guess a number between 1 and 20, using the four clues if you need them, You have 5 guesses')
    while (guess!=ziffer) and (tries<5):
        print(\"Falsch\")
        tries += 1
        guess = int(input ('What is your guess? '))
    if guess == ziffer:
        print(\"Erfolg\")
else: "
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

;; (ert-deftest py-multline-arguments-with-literal-lists-79-test-7NWa5T ()
;;   (py-test
;;       ;; Improper indentation for multline arguments with literal lists (#79)
;;       "def foo():
;;     bar = dosomething([
;;                        x <- point"
;;     (goto-char (point-max))
;;     (search-backward "x")
;;     (should (eq 8 (py-compute-indentation)))))

(ert-deftest lines-after-return-80-Ahdpe8 ()
  (py-test
      "def empty():
    return
    yield"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))
    (search-backward "return")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-electric-indent-test-KaDCGx ()
  (py-test
      "def main():
if len(sys.argv) == 1"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (let (py-electric-colon-greedy-p
	  (py-electric-colon-active-p t))
      (py-electric-colon 1))
    (should (eq 4 (current-indentation)))))

(ert-deftest py-98-indent-test-KaDCGx ()
  (py-test
      "def some_fct():
    \"\"\" Test for py-indent-or-complete.
    To test place point on the first statement.
    If this line is here, py-compute-indentation returns 0.
    \"\"\"
    if the_cursor_is_here:"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (should (eq 4 (current-indentation)))))

(ert-deftest py-indent-test-W5dPqP ()
  (py-test
      "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
import os"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (backward-char 6)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-indent-gnu-bug34268-i1nySM ()
  (py-test
      "def long_function_name(var_one, var_two, var_three,
                       var_four):
    print(var_one)
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "print")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-incorrect-indentation-for-functions-bug113-i1nySM ()
  (py-test
      "def draw(
    handlecolor=\"blue\",
    handleline=\"blue\"
):
foo"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-2H3ET7 ()
  "Indent line-by-line as first line is okay "
  (py-test
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
    'py-verbose-p
    (goto-char (point-max))
    (push-mark)
    (search-backward "True")
    (save-excursion
      (py-indent-region (line-beginning-position) (point-max) t))
    (should (eq 4 (current-indentation)))))

(ert-deftest py-match-case-test-i1nySM ()
  (py-test
      "match (100, 200):
    case (100, 300):  # Mismatch: 200 != 300
        print('Case 1')
    case (100, 200) if flag:  # Successful match, but guard fails
        print('Case 2')
    case (100, y):  # Matches and binds y to 200
        print(f'Case 3, y: {y}')
    case _:  # Pattern not attempted
        print('Case 4, I match anything!')"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "print")
    ;; (beginning-of-line)
    (should (eq 8 (py-compute-indentation)))
    (search-backward "case")
    (should (eq 4 (py-compute-indentation)))
    (search-backward "print")
    (should (eq 8 (py-compute-indentation)))
    (search-backward "case")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-on-first-line-i1nySM ()
  (py-test
      "test()"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward ")")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-EH1WJl ()
  (py-test
      "
 asdf = {
     'a':{"
    'python-mode
    'py-verbose-p
    (goto-char(point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-compute-indent-crasher-136-i1nySM ()
  (py-test
      "def my_func(self):
    this_line() # is bad
    if condition:
        pass
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "#")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-bug-56742-indendation-problem-after-comment-hOliT8 ()
  (py-test
      "def test(n):
    if n < 0:
        return -1
    # test
    else:
        return 0"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "else")
    (should (eq 4 (current-indentation)))))

(ert-deftest py-match-case-indent-137-XVkUIJ ()
  (py-test
      "def http_error(status):
    match status:
    case 400:
        return \"Bad request\"
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "case")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-match-case-indent-137-b1Kuye ()
  (py-test
      "def http_error(status):
    match status:
        case 400:
        return \"Bad request\"
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "return")
    (should (eq 12 (py-compute-indentation)))))

(ert-deftest py-emacs-bug-57262-indent-test-8iPIh0 ()
  (py-test
      "for long_variable_name \\
        in (1, 2):
    print(long_variable_name)
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "print")
    (should (eq 4 (py-compute-indentation)))
    (search-backward "in")
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-comment-shouldnt-outdent-109-test-vkJNom ()
  (py-test
      "def addProductToShopify(sample)
    try:
        a = sample
    except NoResultFound as e:
        return False
    # comment 1
    # comment 2
    # comment 3
    return True
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "comment" nil t 2)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-test-NobJ29 ()
  (py-test
      "for i in b:
    c = len(foo)
    print(\"asdf\")
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "print" nil t 1)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-Lc2wzd ()
  (py-test
      "for infix in [ # some description
              \"_cdata\", \"_cmeta\", \"_corig\", \"_cpool\", \"_cvol\", \"_wcorig\",
              \"indentation is broken here\", \"bar\"]:
    print(infix)
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "is" nil t 1)
    (should (eq 14 (py-compute-indentation)))
    (search-backward "ata" nil t 1)
    (should (eq 14 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-SCEA4j ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "name" nil t 1)
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-Nhdprq ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "name" nil t 1)
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-vn8oBL ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "pk" nil t 1)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-MFw6EK ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "name" nil t 2)
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-Zy6iEE ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "pk" nil t 2)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-indent-in-tuple-test-8y1ZyT ()
  (py-test
      "_PN34 = (-1475, -1438, -1427, -1401, -1398, -1380, -1376, -1363, -1354,
        -1277)"
    'python-mode
    'py-verbose-p
    (let ((py-closing-list-dedents-bos t))
      (goto-char (point-max))
      (should (eq (py-compute-indentation) 9)))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-tZmIx7 ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test "def fun(arg):
    if(
args.suppliername == \"Messingschlager\" or
args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "args" nil t 1)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-KaDCGx ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test "def fun(arg):
    if(
args.suppliername == \"Messingschlager\" or
args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "args" nil t 2)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-3qcJU7 ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test "def fun(arg):
    if(
args.suppliername == \"Messingschlager\" or
args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (let (py-closing-list-dedents-bos)
      (goto-char (point-max))
      (search-backward "else:")
      (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-4RSTQF ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test "def fun(arg):
    if(args.suppliername == \"Messingschlager\" or
       args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (let (py-closing-list-dedents-bos)
      (goto-char (point-max))
      (search-backward ")")
      (should (eq 7 (py-compute-indentation))))))

(ert-deftest py-indent-bug63959-test-6sVElZ ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "obj" nil t 1)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-dgPK5O ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-WQJF2a ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "}")
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-AsmMX4 ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "}" nil t 2)
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-ZdzpFK ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "]")
    (forward-char 1)
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-nSKy69 ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "]")
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-4HtEoV ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "]")
    (beginning-of-line)
    (should (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-xhNrCD ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "name")
    (forward-char -1)
    (should-not (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-HnA9Ko ()
  (py-test
      "data = {'key': {
    'objlist': [
        {'pk': 1,
         'name': 'first'},
        {'pk': 2,
         'name': 'second'}
    ]
}}
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "}" nil t 3)
    (should-not (py-compute-indentation--at-closer-p))))

(ert-deftest py-indent-bug63959-test-VCIfFY ()
  (py-test
      "var5: Sequence[Mapping[str, Sequence[str]]] = [
    {
     'red': ['scarlet', 'vermilion', 'ruby'],
     'green': ['emerald', 'aqua']
    },
    {
                'sword': ['cutlass', 'rapier']
    }
]
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "sword" nil t)
    (beginning-of-line)
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-7JiI5a ()
  (py-test
      "def f():
    \"\"\"
    Return nothing.
    .. NOTE::
        First note line
    second note line\"\"\"
    pass
"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "pass")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-7YHplS ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test
      "def fun(arg):
    if(args.suppliername == \"Messingschlager\" or
args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "args.")
    (should (eq 7 (py-compute-indentation)))))

(ert-deftest py-gnu-bug42513-indent-multi-line-if-test-eE5Kpl ()
  ""
  (py-test
      "def fun(arg):
    if(args.suppliername == \"Messingschlager\" or
args.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    'python-mode
    'py-verbose-p
    (goto-char (point-max))
    (search-backward "else" nil t 2)
    (beginning-of-line) 
    (should (eq 4 (py-compute-indentation)))))


(provide 'py-ert-indent-tests)
;;; py-ert-indent-tests.el ends here
