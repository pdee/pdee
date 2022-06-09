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

(require 'py-setup-ert-tests)

(ert-deftest py-ert-indent-list-style-test-UVqzej ()
  (should py-indent-list-style))

(ert-deftest py-ert-indent-dedenters-WoWM6j-YhnDUf ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-4qujpk-8S5Su6 ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-OmirYx-iako2W ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-P9MB72-6D8DxN ()
  "Check all dedenters."

  (py-test-with-temp-buffer
      "def foo(a, b, c):
    if a:
        print(a)
    elif b:
        print(b)"
    (goto-char (point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-indent-dedenters-SFnpJ4-GyGW0D ()
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

(ert-deftest py-ert-indent-dedenters-m0FUAw-ohKXqu ()
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

(ert-deftest py-ert-indent-dedenters-nK9iWx-0sEVRk ()
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

(ert-deftest py-ert-indent-dedenters-WRXYEM-aKmUgb ()
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

(ert-deftest py-ert-indent-after-backslash-lp-852052-ztz4Yn-cTO9B1 ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\"
    (goto-char (point-max))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-LlOrDK-sHqVVR ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\"
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-nvVJgu-QTa6cI ()
  (py-test-with-temp-buffer
      "from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5"
    (goto-char (point-max))
    (should (eq 5 (py-compute-indentation)))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-1-orr7uy ()
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

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-2-wF0CYZ ()
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
   (goto-char (point-max))
   (push-mark)
   (search-backward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max) t))
   (search-forward "with file")
   (should (eq 8 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-3-yAzv0R ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer
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
   (goto-char (point-max))
   (push-mark)
   (search-backward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max) t))
   (search-forward "for i ")
   (should (eq 12 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-4-MvlWZJ ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer
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
   (goto-char (point-max))
   (push-mark)
   (search-backward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max) t))
   (search-forward "bar.")
   (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-5-cXQ3WB ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer
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
   (goto-char (point-max))
   (push-mark)
   (search-backward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max) t))
   (search-forward "datei.write")
   (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-arg-2-test-gDZcSt ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
with file(\"foo\" + zeit + \".ending\", 'w') as datei:
    for i in range(anzahl):
        bar.dosomething()
        # called from correct first line
        # wrong indent should to be fixed
            datei.write(str(baz[i]) + \"\\n\")
"
    (goto-char (point-max))
    (push-mark)
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
  (py-test-with-temp-buffer
      "import pdb
"
    (goto-char (point-max))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-compute-indentation-bob-test-HcSwMS ()
  (py-test-with-temp-buffer-point-min
      " def foo():
    if True:
        pass
    else:
        pass
"
    (goto-char (point-min))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-1-ld9am7 ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{"
    (goto-char(point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-2-Pg4yts ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{"
    (goto-char(point-max))
    (forward-char -2)
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-3-EH1WJl ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{"
    (goto-char(point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-4-2qSrrt ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{
         'b':3,
         'c':4"
    (goto-char(point-max))
    (beginning-of-line)
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-5-CNMePW ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{
         'b':3,
         'c':4"
    (goto-char(point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-6-I73t1i ()
  (py-test-with-temp-buffer
      "
asdf = {
    'a':{
         'b':3,
         'c':4"
    (goto-char(point-max))
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-7-9UP0Kr ()
  (py-test-with-temp-buffer
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
         }"
    (goto-char(point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 9 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-8-ABORqFr ()
  (py-test-with-temp-buffer
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
         }"
    (goto-char(point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 9 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-9-NzoaiZ ()
  (py-test-with-temp-buffer
      "# hanging, py-closing-list-dedents-bos nil
asdf = {
    'a':{
         'b':3,
         'c':4
         }"
    (goto-char(point-max))
    (let ((py-closing-list-dedents-bos nil))
      (should (eq 9 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-10-uGdWJg ()
  (py-test-with-temp-buffer
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    (goto-char(point-max))
    (beginning-of-line)
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-11-3JVKy7 ()
  (py-test-with-temp-buffer
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    (goto-char(point-max))
    (forward-char -1)
    (let ((py-closing-list-dedents-bos t))
      (indent-line-to (py-compute-indentation))
      (should (eq 0 (current-column))))))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test-12-ygoJUM4 ()
  (py-test-with-temp-buffer
      "# closing, py-closing-list-dedents-bos t
asdf = {
    'a':{
         'b':3,
         'c':4
    }
    }"
    (goto-char(point-max))
    (let ((py-closing-list-dedents-bos t))
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-1-eBQAKK ()
  (py-test-with-temp-buffer-point-min
      "\(along, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    (goto-char (point-min))
    (forward-char 1)
    (should (eq nil (get-char-property (point) 'face)))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-2-70Eccx ()
  (py-test-with-temp-buffer
      "\(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (should (eq 5 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-3-8L9T1k ()
  (py-test-with-temp-buffer
      "\(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    (goto-char(point-max))
    (let ((py-indent-list-style 'line-up-with-first-element))
      (goto-char (point-max))
      (indent-line-to (py-compute-indentation))
      (should (eq 1 (current-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-4-akkTLs ()
  (py-test-with-temp-buffer
      "packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)"
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (indent-line-to (py-compute-indentation))
      (should  (eq 20 (current-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test-5-umQ6Tk ()
  (py-test-with-temp-buffer
      "\( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list"
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (indent-line-to (py-compute-indentation))
      (should (eq 5 (py-compute-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-1-pGszAP ()
  (py-test-with-temp-buffer
      "def foo (a,\n):"
    (goto-char(point-max))
    (let ((py-indent-list-style 'line-up-with-first-element))
      (indent-line-to  (py-compute-indentation))
      (should (eq 9 (current-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-2-2TmrDT ()
  (py-test-with-temp-buffer
      "def foo (a,\n):"
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-to-beginning-of-statement))
      (indent-line-to (py-compute-indentation))
      (should (eq 4 (current-indentation))))))

(ert-deftest py-ert-indent-in-arglist-test-3-euyfAZ ()
  (py-test-with-temp-buffer
      "def foo (a,\n):"
    (goto-char(point-max))
    (let ((py-indent-list-style 'one-level-from-first-element))
      (indent-line-to (py-compute-indentation))
      (should (eq 13 (current-indentation))))))

(ert-deftest py-ert-close-block-test-8LPQD3 ()
  (py-test-with-temp-buffer-point-min
      "# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 4 (py-close-block)))))

(ert-deftest py-ert-close-def-or-class-test-7Kc5SQ ()
  (py-test-with-temp-buffer-point-min
      "# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-def-or-class)))))

(ert-deftest py-ert-close-def-test-nKOJaZ ()
  (py-test-with-temp-buffer-point-min
      "# -*- coding: utf-8 -*-

def main():
    if len(sys.argv)==1:
        usage()
        sys.exit()
if __name__==\"__main__\":
    main()
"
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-def)))))

(ert-deftest py-ert-close-class-test-FPi2i3 ()
  (py-test-with-temp-buffer-point-min
      "# -*- coding: utf-8 -*-
class asdf:
    def main():
        if len(sys.argv)==1:
            usage()
            sys.exit()
    if __name__==\"__main__\":
        main()
"
    (goto-char (point-min))
    (search-forward "exit()")
    (should (eq 0 (py-close-class)))))

(ert-deftest py-ert-dedent-forward-test-61dWA6 ()
  (py-test-with-temp-buffer
      "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
        datei.write(str(spiel[i]) + \"\\n\")"
    (goto-char(point-max))
    (skip-chars-backward " \t\r\n\f")
    (py-dedent-forward-line)
    (should (py-empty-line-p))
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-async-backward-block-test-aAFOA0 () ()
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

(ert-deftest py-ert-indent-try-test-zg6QYI ()
  (py-test-with-temp-buffer-point-min
      "#! /usr/bin/env python

import sys
import os

        try:"
    (goto-char (point-min) )
    (search-forward "try")
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-ert-indent-closing-tx8E5Q-m4T4xF ()
  (py-test-with-temp-buffer
      "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
    (goto-char (point-max))
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-async-indent-test-MFS8IW ()
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

(ert-deftest py-ert-indent-else-clause-test-gIyr2H ()
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

(ert-deftest py-clause-indent-test-UXZsX9 ()
  (py-test-with-temp-buffer
      "def ziffernraten ()
    ziffer = random\.randint(1,20)

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
    (goto-char (point-max))
    (should (eq 4  (py-compute-indentation)))))

(ert-deftest py-multline-arguments-with-literal-lists-79-test-7NWa5T ()
  (py-test-with-temp-buffer
      ;; Improper indentation for multline arguments with liiteral lists (#79)
      "def foo():
    bar = dosomething([
                       x <- point"
    (goto-char (point-max))
    (search-backward "x")
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest lines-after-return-80-Ahdpe8 ()
  (py-test-with-temp-buffer
      "def empty():
    return
    yield"
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 4 (py-compute-indentation)))
    (search-backward "return")
    (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-electric-indent-test-KaDCGx ()
  (py-test-with-temp-buffer
      "def main():
if len(sys.argv) == 1"
    (goto-char (point-max))
    (let (py-electric-colon-greedy-p
	  (py-electric-colon-active-p t))
      (py-electric-colon 1))
    (should (eq 4 (current-indentation)))))

(ert-deftest py-98-indent-test-KaDCGx ()
  (py-test-with-temp-buffer
      "def some_fct():
    \"\"\" Test for py-indent-or-complete\.

    To test place point on the first statement\.

    If this line is here, py-compute-indentation returns 0\.

    \"\"\"
    if the_cursor_is_here:"
    (goto-char (point-max))
    (should (eq 4 (current-indentation)))))

(ert-deftest py-bug42513-indent-multi-line-if-test-KaDCGx ()
  ";; bug#42513: Python indentation bug when using multi-line on an if-conditition"
  (py-test-with-temp-buffer "def fun(arg):
    if(
args\.suppliername == \"Messingschlager\" or
args\.suppliercodename == \"MS\"
	): #<- culprit
		#do something
else: #<- this else is not possible to indent 1 tab
		#do something"
    (goto-char (point-max))
    (search-backward "else:")
    (should (eq 4  (py-compute-indentation)))
    (search-backward "args" nil t 2)
    (should (eq 7  (py-compute-indentation)))))

(ert-deftest py-indent-test-W5dPqP ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-

import os"
    (goto-char (point-max))
    (backward-char 6)
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-indent-gnu-bug34268-i1nySM ()
  (py-test-with-temp-buffer
      "def long_function_name(var_one, var_two, var_three,
                       var_four):
    print(var_one)
"
    (goto-char (point-max))
    (search-backward "print")
    (should (eq 4  (py-compute-indentation)))))

(ert-deftest py-incorrect-indentation-for-functions-bug113-i1nySM ()
  (py-test-with-temp-buffer
      "def draw(
    handlecolor=\"blue\",
    handleline=\"blue\"
):
foo"
    (goto-char (point-max))
    (beginning-of-line)
    (should (eq 4  (py-compute-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-test-1-2H3ET7 ()
  "Indent line-by-line as first line is okay "
  (py-test-with-temp-buffer
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
   (goto-char (point-max))
   (push-mark)
   (search-backward "True")
   (save-excursion
     (py-indent-region (line-beginning-position) (point-max) t))
   (should (eq 4 (current-indentation)))))

(ert-deftest py-match-case-test-i1nySM ()
  (py-test-with-temp-buffer
      "match (100, 200):
    case (100, 300):  # Mismatch: 200 != 300
        print('Case 1')
    case (100, 200) if flag:  # Successful match, but guard fails
        print('Case 2')
    case (100, y):  # Matches and binds y to 200
        print(f'Case 3, y: {y}')
    case _:  # Pattern not attempted
        print('Case 4, I match anything!')"
    (goto-char (point-max))
    (search-backward "print")
    ;; (beginning-of-line)
    (should (eq 8  (py-compute-indentation)))
    (search-backward "case")
    (should (eq 4  (py-compute-indentation)))
    (search-backward "print")
    (should (eq 8  (py-compute-indentation)))
    (search-backward "case")
    (should (eq 4  (py-compute-indentation)))
    ))

(provide 'py-ert-indent-tests)
;;; py-ert-indent-tests.el ends here
