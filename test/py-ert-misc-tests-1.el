;; py-ert-tests.el --- Tests, some adapted from python.el -*- lexical-binding: t; -*-

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

(require 'setup-ert-tests)

(add-to-list 'load-path default-directory)
(load "py-ert-tests-1.el" nil t)

;; (require 'python-mode-test)

;;;
(ert-deftest py-ert-keyword-face-lp-1294742-N1cjiI ()
  (py-test-with-temp-buffer-point-min
      " and as assert break continue del elif else except exec finally for global if in is lambda not or pass raise return while with yield"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'font-lock-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-builtins-face-lp-1294742-zvZYG5 ()
  (py-test-with-temp-buffer-point-min
      "_ __doc__ __import__ __name__ __package__ abs all any apply basestring bin bool buffer bytearray bytes callable chr classmethod cmp coerce compile complex delattr dict dir divmod enumerate eval execfile file filter float format frozenset getattr globals hasattr hash help hex id input int intern isinstance issubclass iter len list locals long map max min next object oct open ord pow print property range raw_input reduce reload repr reversed round set setattr slice sorted staticmethod str sum super tuple type unichr unicode vars xrange zip"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    ;; (when py-debug-p (switch-to-buffer (current-buffer)))
    (should (eq 'py-builtins-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-pseudo-keyword-face-lp-1294742-KgocNc ()
  (py-test-with-temp-buffer-point-min
      "  Ellipsis True False None  __debug__ NotImplemented"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-pseudo-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-object-reference-face-lp-1294742-HCkKIc ()
  (py-test-with-temp-buffer-point-min
      "self cls"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min) (point-max))
    (should (eq 'py-object-reference-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-borks-all-lp-1294820-sIKMyz ()
  (py-test-with-temp-buffer-point-min
      "# M-q within some code (not in= a docstring) completely borks all previous
# code in the file:
#
# E.g. here, if I M-q within the last function:

def foo(self):
    some_actual_code()

def bar(self):
    some_actual_code()

def baz(self):
    some_actual_code()

# def foo(self): some_actual_code() def bar(self): some_actual_code() def
# baz(self):
#     some_actual_code()
"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (search-forward "def baz(self):")
    (fill-paragraph)
    (forward-line -1)
    (should (eq (char-after) ?\n))))

(ert-deftest py-ert-backward-same-level-test-7ZoU20 ()
  (py-test-with-temp-buffer-point-min
      "def foo():
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
                    else:
                        pass
                    finally:
                        pass
        else True:
            pass
        finally:
            pass
"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (goto-char 632)
    (py-backward-same-level)
    (should (looking-at "except"))
    (py-backward-same-level)
    (should (looking-at "try"))))

(ert-deftest py-ert-up-level-test-2-a8UchN ()
  (py-test-with-temp-buffer
      "def foo():
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
                        pass"
    (goto-char (point-max))
    (py-up-block)
    (should (looking-at "if"))))

(ert-deftest py-ert-up-level-test-2-taTfXm ()
  (py-test-with-temp-buffer
      "def foo():
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
                        pass"
    (goto-char (point-max))
    (py-up-clause)
    (should (looking-at "else:"))))

(ert-deftest py-ert-deletes-too-much-lp:1300270-dMegYd ()
  (py-test-with-temp-buffer "
x = {'abc':'def',
         'ghi':'jkl'}
"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (goto-char 24)
    (py-electric-delete)
    (should (eq 5 (current-indentation)))))

(ert-deftest py-ert-mark-expression-test-29MdXT ()
  (py-test-with-temp-buffer
      "assert pycompletions('TestClass.test' , name) == \
          ['testclassmeth', 'testmeth', 'testprop', 'teststaticmeth']"
    (goto-char (point-max))
    (forward-char -1)
    (py-mark-expression)
    (should (eq 119 (mark)))
    (goto-char 44)
    (py-mark-expression)
    (should (eq 46 (mark)))))

(ert-deftest py-dedicated-shell-test-7tw0PH ()
  (let ((erg (buffer-name (py-shell nil nil t "python"))))
    (should (< 8 (length erg)))
    (should (eq 0 (string-match "^*Python" erg)))))

(ert-deftest py-python-shell-test-Ms1Z0k ()
  ""
  (let ((erg (python)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-python2-shell-test-8Ostfe ()
  ""
  (let ((erg (python2)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-python3-shell-test-YW7ToN ()
  ""
  (let ((erg (python3)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-keep-windows-configuration-test-Hh2GD6 ()
  (py-test-with-temp-buffer
      "print('py-keep-windows-configuration-test-string')"
    (delete-other-windows)
    (let ((py-keep-windows-configuration t)
          (py-split-window-on-execute t)
          (full-height (window-height)))
      (py-execute-statement)
      (should (eq (window-height) full-height)))))

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

(ert-deftest py-indentation-lp-1375122-test-yx67am ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
pass
"
    (goto-char (point-max))
    (forward-line -1)
    (call-interactively 'py-indent-or-complete)
    (sit-for 0.1 t)
    (should (eq 8 (current-column)))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to 4)
    (call-interactively 'py-indent-or-complete)
    (sit-for 0.1 t)
    (should (eq 8 (current-column)))))

(ert-deftest py-shell-python-lp-1398530-test-Haizw1 ()
  (when (buffer-live-p (get-buffer "*Python*"))(py-kill-buffer-unconditional "*Python*"))
  (py-test-with-temp-buffer
      ""
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python*"))))))

(ert-deftest py-shell-python3-lp-1398530-test-gm7LwH ()
  (when (buffer-live-p (get-buffer "*Python3*"))(py-kill-buffer-unconditional "*Python3*"))
  (py-test-with-temp-buffer
      ""
    (goto-char (point-max))
    (let ((py-shell-name "python3"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python3*"))))))

(ert-deftest py-shell-python2-lp-1398530-test-TaiABe ()
  (when (buffer-live-p (get-buffer "*Python2*"))(py-kill-buffer-unconditional "*Python2*"))
  (py-test-with-temp-buffer
      ""
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python2"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python2*"))))))

(ert-deftest py-backward-statement-test-1-QcNOgE ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
print dir()
c = Cat()
c.hello() #causes error, but emacs tracking fails
import sys, os; os.remove('do/something/nasty') # lp:1025000

def foo(*args):2
    \"\"\"
    ASDF
    \"\"\"
    # ABD
    args = \"asdf\"
"
    (goto-char (point-max))
    (let ((py-return-result-p t)
          py-result py-store-result-p)
      (py-backward-statement)
      (should (eq (char-after) ?a))
      (py-backward-statement)
      (should (eq (char-after) ?d))
      (py-backward-statement)
      (should (eq (char-after) ?o))
      (py-backward-statement)
      (should (eq (char-after) ?i))
      (py-backward-statement)
      (should (eq (char-after) ?c))
      (py-backward-statement)
      (should (eq (char-after) ?c))
      (py-backward-statement)
      (should (eq (char-after) ?p))
      (py-backward-statement)
      (should (bobp)))))

(ert-deftest py-ert-backward-except-block-test-TC7jEj ()
  (py-test-with-temp-buffer
      "
# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in range(anzahl):
                    pass
        except:
            block2
             "
    (goto-char (point-max))
    (py-backward-except-block)
    (should (eq (char-after) ?e))))

(ert-deftest py-ert-backward-except-block-bol-test-Tp9NOw ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in range(anzahl):
                    pass
        except:
            block2
             "
    (goto-char(point-max))
    (py-backward-except-block-bol)
    (sit-for 0.1)
    (should (eq (char-after) 32))))

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

(ert-deftest py-face-lp-1454858-python2-1-test-cBoEWe ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python2
file.close()"
    (goto-char(point-max))
    (let ((py-python-edit-version ""))
      (font-lock-fontify-region (point-min) (point-max))
      (forward-line 1)
      (should (eq (face-at-point) 'py-builtins-face)))))

;; Setting of py-python-edit-version should precede
(ert-deftest py-face-lp-1454858-python2-2-test-VGqacZ ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python3
file.close()"
    (goto-char(point-max))
    (let ((py-python-edit-version "python2"))
      (font-lock-fontify-region (point-min) (point-max))
      (forward-line 1)
      (should (eq (face-at-point) 'py-builtins-face)))))

(ert-deftest py-face-lp-1454858-python2-3-test-X7oyjk ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python2
print()"
    (goto-char(point-max))
    (let ((py-python-edit-version ""))
      (font-lock-fontify-region (point-min) (point-max))
      (forward-line 1)
      (should (eq (face-at-point) 'font-lock-keyword-face)))))

(ert-deftest py-ert-in-comment-p-test-G6FUaB ()
  (py-test-with-temp-buffer
      "# "
    (should (py--in-comment-p))))

(ert-deftest py-ert-in-sq-string-p-test-nwha1D ()
  (py-test-with-temp-buffer
      "' "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-string-p-test-lYrt9b ()
  (py-test-with-temp-buffer
      "\" "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-sq-tqs-string-p-test-EwMSzz ()
  (py-test-with-temp-buffer
      "''' "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-in-dq-tqs-string-p-test-jkHHQH ()
  (py-test-with-temp-buffer
      "\"\"\" "
    (goto-char(point-max))
    (should (py-in-string-p))))

(ert-deftest py-ert-electric-delete-test-HecKiw ()
  (py-test-with-temp-buffer-point-min
      "  {}"
    (goto-char (point-min))
    (py-electric-delete)
    (should (eq (char-after) ?{))))

(ert-deftest py-ert-end-of-def-or-class-test-1-ibbr7d ()
  (py-test-with-temp-buffer
      "class MyTest(unittest.TestCase):
    def test(self):
        self.assertEqual(fun(3), 4)

"
    'python-mode
    py-debug-p
    (goto-char (point-max))
    (skip-chars-backward " \n")
    (forward-char 1)
    (py-end-of-def-or-class)
    (should (eobp))))

(ert-deftest py-ert-end-of-def-or-class-test-2-ctMmXA ()
  (py-test-with-temp-buffer-point-min
      "class MyTest(unittest.TestCase):
    def test(self):
        pass
    def test(self):
        pass"
    (goto-char (point-min))
    (search-forward "pass")
    (py-end-of-def-or-class)
    (should (eobp))))

(ert-deftest py-ert-narrow-to-block-test-uDQtR1-tqncYG ()
  (py-test-with-temp-buffer
      "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
                    "
    (goto-char(point-max))
    (py-narrow-to-block)
    (should (eq 50 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-block-test-xnEs46-GPBOHw ()
  (py-test-with-temp-buffer
      "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
        "
    (goto-char(point-max))
    (py-narrow-to-block)
    (should (eq 50 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-block-or-clause-test-43VsYV ()
  (py-test-with-temp-buffer
      "if treffer in gruen:
    # print \"0, Gruen\"
    ausgabe[1] = treffer
    ausgabe[2] = treffer

elif treffer in schwarz:
    # print \"%i, Schwarz\" % (treffer)
    ausgabe[1] = treffer
"
    (goto-char(point-max))
    (skip-chars-backward " \t\r\n\f")
    (py-narrow-to-block-or-clause)
    (should (eq 87 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-clause-test-rHLyyW ()
  (py-test-with-temp-buffer
      "if treffer in gruen:
    # print \"0, Gruen\"
    ausgabe[1] = treffer
    ausgabe[2] = treffer

elif treffer in schwarz:
    # print \"%i, Schwarz\" % (treffer)
    ausgabe[1] = treffer
"
    (goto-char(point-max))
    (py-narrow-to-clause)
    (should (eq 87 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-class-test-MNaZDI ()
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
    (goto-char(point-max))
    (search-backward "treffer")
    (py-narrow-to-class)
    (should (eq 710 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-def-test-wGwY45 ()
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
    (goto-char(point-max))
    (search-backward "treffer")
    (py-narrow-to-def)
    (should (< 480 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-def-or-class-test-46QGK4 ()
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
    (goto-char(point-max))
    (search-backward "treffer")
    (py-narrow-to-def-or-class)
    (should (< 480 (length (buffer-substring-no-properties (point-min)(point-max)))))
    (should (> 490 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-statement-test-7WyEtz ()
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
    (goto-char(point-max))
    (search-backward "treffer")
    (py-narrow-to-statement)
    (should (eq 32 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-section-backward-test-f5QRWB ()
  (py-test-with-temp-buffer
      "# {{
print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})
# }}
# {{
print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})
# }}
"
    (goto-char(point-max))
    (py-backward-section)
    (should (eq (char-after) ?#))
    (py-backward-section)
    (should (eq (char-after) ?#))))

(ert-deftest py-ert-section-forward-test-ZW09DD ()
  (py-test-with-temp-buffer-point-min
      "# {{
print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})
# }}
# {{
print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})
# }}
"
    (goto-char (point-min))
    (py-forward-section)
    (should (eq (char-before) ?}))
    (py-forward-section)
    (should (eq (char-before) ?}))))

(ert-deftest py-ert-sectionize-test-g5EFUz ()
  (py-test-with-temp-buffer-point-min
      "print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})
"
    (goto-char (point-min))
    (end-of-line)
    (py-sectionize-region (point-min) (point-max))
    (goto-char (point-min))
    (should (eq (char-after) ?#))
    (py-forward-section)
    (should (eq (char-before) ?}))))

(ert-deftest py-ert-jump-matching-indent-test-sQ8keh ()
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
    (goto-char(point-max))
    (search-backward "if ")
    (forward-line -1)
    (indent-to 12)
    (py-backward-block)
    (should (eq (current-column) 8))))

(ert-deftest py-ert-fill-plain-string-test-OEykwr ()
  (py-test-with-temp-buffer-point-min
      "'''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''"
    (goto-char (point-min))
    (forward-char 4)
    (fill-paragraph)
    (forward-line 1)
    (should (not (py-empty-line-p)))))

(ert-deftest py-ert-nil-docstring-style-lp-1477422-test-6wpqLB ()
  (py-test-with-temp-buffer-point-min
      "def foo():
    '''asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdfasdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'''"
    (goto-char (point-min))
    (let (py-docstring-style)
      (search-forward "'''")
      (save-excursion
        (fill-paragraph))
      (forward-line 1)
      (should (not (py-empty-line-p))))))

(ert-deftest py-markup-region-as-section-test-KetMYL ()
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
    (search-forward "fertig")
    (py-sectionize-region (match-beginning 0) (line-end-position))
    (py-mark-section)
    (should (eq 371 (region-beginning)))
    (should (eq 408 (region-end)))))

(ert-deftest py-test-embedded-51-test-sgaO9V ()
  (py-test-with-temp-buffer
      "from PrintEngine import *

GeomSim."
    (goto-char(point-max))
    ;; (switch-to-buffer (current-buffer))
    (ignore-errors (py-indent-or-complete))
    (sit-for 0.1)
    (should (eq (char-before) ?.))))

(ert-deftest py-beginning-of-block-test-GccKh6 ()
  (py-test-with-temp-buffer
      "if False:
    print(\"Nein\")
else:
    print(\"Ja\")"
    (goto-char(point-max))
    (py-beginning-of-block)
    (should (eq (char-after) ?i))))

(ert-deftest py-forward-def-or-class-test-C3L8jg ()
  (py-test-with-temp-buffer-point-min
      "def find_function(funcname, filename):
    cre = re.compile(r'def\\s+%s\\s*[(]' % re.escape(funcname))
    try:
        fp = open(filename)
    except IOError:
        return None
    # consumer of this info expects the first line to be 1
    lineno = 1
    answer = None
    while 1:
        line = fp.readline()
        if line == '':
            break
        if cre.match(line):
            answer = funcname, filename, lineno
            break
        lineno = lineno + 1
    fp.close()
    return answer

# Interaction prompt line will separate file and call info from code
# text using value of line_prefix string.  A newline and arrow may
# be to your liking.  You can set it once pdb is imported using the
# command \"pdb.line_prefix = '\\n% '\".
# line_prefix = ': '    # Use this to get the old situation back
line_prefix = '\\n-> '   # Probably a better default

class Pdb(bdb.Bdb, cmd.Cmd):

    def __init__(self, completekey='tab', stdin=None, stdout=None, skip=None):
        bdb.Bdb.__init__(self, skip=skip)
        cmd.Cmd.__init__(self, completekey, stdin, stdout)
        if stdout:
            self.use_rawinput = 0
        self.prompt = '(Pdb) '
        self.aliases = {}
        self.mainpyfile = ''
        self._wait_for_mainpyfile = 0
        # Try to load readline if it exists
        try:
            import readline
        except ImportError:
            pass

        # Read $HOME/.pdbrc and ./.pdbrc
        self.rcLines = []
        if 'HOME' in os.environ:
            envHome = os.environ['HOME']
            try:
                rcFile = open(os.path.join(envHome, \".pdbrc\"))
            except IOError:
                pass
            else:
                for line in rcFile.readlines():
                    self.rcLines.append(line)
                rcFile.close()
        try:
            rcFile = open(\".pdbrc\")
        except IOError:
            pass
        else:
            for line in rcFile.readlines():
                self.rcLines.append(line)
            rcFile.close()

        self.commands = {} # associates a command list to breakpoint numbers
        self.commands_doprompt = {} # for each bp num, tells if the prompt
                                    # must be disp. after execing the cmd list
        self.commands_silent = {} # for each bp num, tells if the stack trace
                                  # must be disp. after execing the cmd list
        self.commands_defining = False # True while in the process of defining
                                       # a command list
        self.commands_bnum = None # The breakpoint number for which we are
                                  # defining a list

    def reset(self):
        bdb.Bdb.reset(self)
        self.forget()

    def forget(self):
        self.lineno = None
        self.stack = []
        self.curindex = 0
        self.curframe = None

    def setup(self, f, t):
        self.forget()
        self.stack, self.curindex = self.get_stack(f, t)
        self.curframe = self.stack[self.curindex][0]
        # The f_locals dictionary is updated from the actual frame
        # locals whenever the .f_locals accessor is called, so we
        # cache it here to ensure that modifications are not overwritten.
        self.curframe_locals = self.curframe.f_locals
        self.execRcLines()

    # Can be executed earlier than 'setup' if desired
    def execRcLines(self):
        if self.rcLines:
            # Make local copy because of recursion
            rcLines = self.rcLines
            # executed only once
            self.rcLines = []
            for line in rcLines:
                line = line[:-1]
                if len(line) > 0 and line[0] != '#':
                    self.onecmd(line)

    # Override Bdb methods

    def user_call(self, frame, argument_list):
        \"\"\"This method is called when there is the remote possibility
        that we ever need to stop in this function.\"\"\"
        if self._wait_for_mainpyfile:
            return
        if self.stop_here(frame):
            print >>self.stdout, '--Call--'
            self.interaction(frame, None)

    def user_line(self, frame):
        \"\"\"This function is called when we stop or break at this line.\"\"\"
        if self._wait_for_mainpyfile:
            if (self.mainpyfile != self.canonic(frame.f_code.co_filename)
                or frame.f_lineno<= 0):
                return
            self._wait_for_mainpyfile = 0
        if self.bp_commands(frame):
            self.interaction(frame, None)

    def bp_commands(self,frame):
        \"\"\"Call every command that was set for the current active breakpoint
        (if there is one).

        Returns True if the normal interaction function must be called,
        False otherwise.\"\"\"
        # self.currentbp is set in bdb in Bdb.break_here if a breakpoint was hit
        if getattr(self, \"currentbp\", False) and \\
               self.currentbp in self.commands:
            currentbp = self.currentbp
            self.currentbp = 0
            lastcmd_back = self.lastcmd
            self.setup(frame, None)
            for line in self.commands[currentbp]:
                self.onecmd(line)
            self.lastcmd = lastcmd_back
            if not self.commands_silent[currentbp]:
                self.print_stack_entry(self.stack[self.curindex])
            if self.commands_doprompt[currentbp]:
                self.cmdloop()
            self.forget()
            return
        return 1

    def user_return(self, frame, return_value):
        \"\"\"This function is called when a return trap is set here.\"\"\"
        if self._wait_for_mainpyfile:
            return
        frame.f_locals['__return__'] = return_value
        print >>self.stdout, '--Return--'
        self.interaction(frame, None)

    def user_exception(self, frame, exc_info):
        \"\"\"This function is called if an exception occurs,
        but only if we are to stop at or just below this level.\"\"\"
        if self._wait_for_mainpyfile:
            return
        exc_type, exc_value, exc_traceback = exc_info
        frame.f_locals['__exception__'] = exc_type, exc_value
        if type(exc_type) == type(''):
            exc_type_name = exc_type
        else: exc_type_name = exc_type.__name__
        print >>self.stdout, exc_type_name + ':', _saferepr(exc_value)
        self.interaction(frame, exc_traceback)

    # General interaction function

    def interaction(self, frame, traceback):
        self.setup(frame, traceback)
        self.print_stack_entry(self.stack[self.curindex])
        self.cmdloop()
        self.forget()

    def displayhook(self, obj):
        \"\"\"Custom displayhook for the exec in default(), which prevents
        assignment of the _ variable in the builtins.
        \"\"\"
        # reproduce the behavior of the standard displayhook, not printing None
        if obj is not None:
            print repr(obj)

    def default(self, line):
        if line[:1] == '!': line = line[1:]
        locals = self.curframe_locals
        globals = self.curframe.f_globals
        try:
            code = compile(line + '\\n', '<stdin>', 'single')
            save_stdout = sys.stdout
            save_stdin = sys.stdin
            save_displayhook = sys.displayhook
            try:
                sys.stdin = self.stdin
                sys.stdout = self.stdout
                sys.displayhook = self.displayhook
                exec code in globals, locals
            finally:
                sys.stdout = save_stdout
                sys.stdin = save_stdin
                sys.displayhook = save_displayhook
        except:
            t, v = sys.exc_info()[:2]
            if type(t) == type(''):
                exc_type_name = t
            else: exc_type_name = t.__name__
            print >>self.stdout, '***', exc_type_name + ':', v

    def precmd(self, line):
        \"\"\"Handle alias expansion and '\;\;' separator.\"\"\"
        if not line.strip():
            return line
        args = line.split()
        while args[0] in self.aliases:
            line = self.aliases[args[0]]
            ii = 1
            for tmpArg in args[1:]:
                line = line.replace(\"%\" + str(ii),
                                      tmpArg)
                ii = ii + 1
            line = line.replace(\"%*\", ' '.join(args[1:]))
            args = line.split()
        # split into '\;\;' separated commands
        # unless it's an alias command
        if args[0] != 'alias':
            marker = line.find('\;\;')
            if marker >= 0:
                # queue up everything after marker
                next = line[marker+2:].lstrip()
                self.cmdqueue.append(next)
                line = line[:marker].rstrip()
        return line

    def onecmd(self, line):
        \"\"\"Interpret the argument as though it had been typed in response
        to the prompt.

        Checks whether this line is typed at the normal prompt or in
        a breakpoint command list definition.
        \"\"\"
        if not self.commands_defining:
            return cmd.Cmd.onecmd(self, line)
        else:
            return self.handle_command_def(line)

    def handle_command_def(self,line):
        \"\"\"Handles one command line during command list definition.\"\"\"
        cmd, arg, line = self.parseline(line)
        if not cmd:
            return
        if cmd == 'silent':
            self.commands_silent[self.commands_bnum] = True
            return # continue to handle other cmd def in the cmd list
        elif cmd == 'end':
            self.cmdqueue = []
            return 1 # end of cmd list
        cmdlist = self.commands[self.commands_bnum]
        if arg:
            cmdlist.append(cmd+' '+arg)
        else:
            cmdlist.append(cmd)
        # Determine if we must stop
        try:
            func = getattr(self, 'do_' + cmd)
        except AttributeError:
            func = self.default
        # one of the resuming commands
        if func.func_name in self.commands_resuming:
            self.commands_doprompt[self.commands_bnum] = False
            self.cmdqueue = []
            return 1
        return

    # Command definitions, called by cmdloop()
    # The argument is the remaining string on the command line
    # Return true to exit from the command loop

    do_h = cmd.Cmd.do_help

    def do_commands(self, arg):
        \"\"\"Defines a list of commands associated to a breakpoint.

        Those commands will be executed whenever the breakpoint causes
        the program to stop execution.\"\"\"
        if not arg:
            bnum = len(bdb.Breakpoint.bpbynumber)-1
        else:
            try:
                bnum = int(arg)
            except:
                print >>self.stdout, \"Usage : commands [bnum]\\n ...\" \\
                                     \"\\n end\"
                return
        self.commands_bnum = bnum
        self.commands[bnum] = []
        self.commands_doprompt[bnum] = True
        self.commands_silent[bnum] = False
        prompt_back = self.prompt
        self.prompt = '(com) '
        self.commands_defining = True
        try:
            self.cmdloop()
        finally:
            self.commands_defining = False
            self.prompt = prompt_back

    def do_break(self, arg, temporary = 0):
        # break [ ([filename:]lineno | function) [, \"condition\"] ]
        if not arg:
            if self.breaks:  # There's at least one
                print >>self.stdout, \"Num Type Disp Enb Where\"
                for bp in bdb.Breakpoint.bpbynumber:
                    if bp:
                        bp.bpprint(self.stdout)
            return
        # parse arguments\; comma has lowest precedence
        # and cannot occur in filename
        filename = None
        lineno = None
        cond = None
        comma = arg.find(',')
        if comma > 0:
            # parse stuff after comma: \"condition\"
            cond = arg[comma+1:].lstrip()
            arg = arg[:comma].rstrip()
        # parse stuff before comma: [filename:]lineno | function
        colon = arg.rfind(':')
        funcname = None
        if colon >= 0:
            filename = arg[:colon].rstrip()
            f = self.lookupmodule(filename)
            if not f:
                print >>self.stdout, '*** ', repr(filename),
                print >>self.stdout, 'not found from sys.path'
                return
            else:
                filename = f
            arg = arg[colon+1:].lstrip()
            try:
                lineno = int(arg)
            except ValueError, msg:
                print >>self.stdout, '*** Bad lineno:', arg
                return
        else:
            # no colon\; can be lineno or function
            try:
                lineno = int(arg)
            except ValueError:
                try:
                    func = eval(arg,
                                self.curframe.f_globals,
                                self.curframe_locals)
                except:
                    func = arg
                try:
                    if hasattr(func, 'im_func'):
                        func = func.im_func
                    code = func.func_code
                    #use co_name to identify the bkpt (function names
                    #could be aliased, but co_name is invariant)
                    funcname = code.co_name
                    lineno = code.co_firstlineno
                    filename = code.co_filename
                except:
                    # last thing to try
                    (ok, filename, ln) = self.lineinfo(arg)
                    if not ok:
                        print >>self.stdout, '*** The specified object',
                        print >>self.stdout, repr(arg),
                        print >>self.stdout, 'is not a function'
                        print >>self.stdout, 'or was not found along sys.path.'
                        return
                    funcname = ok # ok contains a function name
                    lineno = int(ln)
        if not filename:
            filename = self.defaultFile()
        # Check for reasonable breakpoint
        line = self.checkline(filename, lineno)
        if line:
            # now set the break point
            err = self.set_break(filename, line, temporary, cond, funcname)
            if err: print >>self.stdout, '***', err
            else:
                bp = self.get_breaks(filename, line)[-1]
                print >>self.stdout, \"Breakpoint %d at %s:%d\" % (bp.number,
                                                                 bp.file,
                                                                 bp.line)

    # To be overridden in derived debuggers
    def defaultFile(self):
        \"\"\"Produce a reasonable default.\"\"\"
        filename = self.curframe.f_code.co_filename
        if filename == '<string>' and self.mainpyfile:
            filename = self.mainpyfile
        return filename

    do_b = do_break

    def do_tbreak(self, arg):
        self.do_break(arg, 1)

    def lineinfo(self, identifier):
        failed = (None, None, None)
        # Input is identifier, may be in single quotes
        idstring = identifier.split(\"'\")
        if len(idstring) == 1:
            # not in single quotes
            id = idstring[0].strip()
        elif len(idstring) == 3:
            # quoted
            id = idstring[1].strip()
        else:
            return failed
        if id == '': return failed
        parts = id.split('.')
        # Protection for derived debuggers
        if parts[0] == 'self':
            del parts[0]
            if len(parts) == 0:
                return failed
        # Best first guess at file to look at
        fname = self.defaultFile()
        if len(parts) == 1:
            item = parts[0]
        else:
            # More than one part.
            # First is module, second is method/class
            f = self.lookupmodule(parts[0])
            if f:
                fname = f
            item = parts[1]
        answer = find_function(item, fname)
        return answer or failed

    def checkline(self, filename, lineno):
        \"\"\"Check whether specified line seems to be executable.

        Return `lineno` if it is, 0 if not (e.g. a docstring, comment, blank
        line or EOF). Warning: testing is not comprehensive.
        \"\"\"
        # this method should be callable before starting debugging, so default
        # to \"no globals\" if there is no current frame
        globs = self.curframe.f_globals if hasattr(self, 'curframe') else None
        line = linecache.getline(filename, lineno, globs)
        if not line:
            print >>self.stdout, 'End of file'
            return 0
        line = line.strip()
        # Don't allow setting breakpoint at a blank line
        if (not line or (line[0] == '#') or
             (line[:3] == '\"\"\"') or line[:3] == \"'''\"):
            print >>self.stdout, '*** Blank or comment'
            return 0
        return lineno

    def do_enable(self, arg):
        args = arg.split()
        for i in args:
            try:
                i = int(i)
            except ValueError:
                print >>self.stdout, 'Breakpoint index %r is not a number' % i
                continue

            if not (0 <= i < len(bdb.Breakpoint.bpbynumber)):
                print >>self.stdout, 'No breakpoint numbered', i
                continue

            bp = bdb.Breakpoint.bpbynumber[i]
            if bp:
                bp.enable()

    def do_disable(self, arg):
        args = arg.split()
        for i in args:
            try:
                i = int(i)
            except ValueError:
                print >>self.stdout, 'Breakpoint index %r is not a number' % i
                continue

            if not (0 <= i < len(bdb.Breakpoint.bpbynumber)):
                print >>self.stdout, 'No breakpoint numbered', i
                continue

            bp = bdb.Breakpoint.bpbynumber[i]
            if bp:
                bp.disable()

    def do_condition(self, arg):
        # arg is breakpoint number and condition
        args = arg.split(' ', 1)
        try:
            bpnum = int(args[0].strip())
        except ValueError:
            # something went wrong
            print >>self.stdout, \\
                'Breakpoint index %r is not a number' % args[0]
            return
        try:
            cond = args[1]
        except:
            cond = None
        try:
            bp = bdb.Breakpoint.bpbynumber[bpnum]
        except IndexError:
            print >>self.stdout, 'Breakpoint index %r is not valid' % args[0]
            return
        if bp:
            bp.cond = cond
            if not cond:
                print >>self.stdout, 'Breakpoint', bpnum,
                print >>self.stdout, 'is now unconditional.'

    def do_ignore(self,arg):
        \"\"\"arg is bp number followed by ignore count.\"\"\"
        args = arg.split()
        try:
            bpnum = int(args[0].strip())
        except ValueError:
            # something went wrong
            print >>self.stdout, \\
                'Breakpoint index %r is not a number' % args[0]
            return
        try:
            count = int(args[1].strip())
        except:
            count = 0
        try:
            bp = bdb.Breakpoint.bpbynumber[bpnum]
        except IndexError:
            print >>self.stdout, 'Breakpoint index %r is not valid' % args[0]
            return
        if bp:
            bp.ignore = count
            if count > 0:
                reply = 'Will ignore next '
                if count > 1:
                    reply = reply + '%d crossings' % count
                else:
                    reply = reply + '1 crossing'
                print >>self.stdout, reply + ' of breakpoint %d.' % bpnum
            else:
                print >>self.stdout, 'Will stop next time breakpoint',
                print >>self.stdout, bpnum, 'is reached.'

    def do_clear(self, arg):
        \"\"\"Three possibilities, tried in this order:
        clear -> clear all breaks, ask for confirmation
        clear file:lineno -> clear all breaks at file:lineno
        clear bpno bpno ... -> clear breakpoints by number\"\"\"
        if not arg:
            try:
                reply = raw_input('Clear all breaks? ')
            except EOFError:
                reply = 'no'
            reply = reply.strip().lower()
            if reply in ('y', 'yes'):
                self.clear_all_breaks()
            return
        if ':' in arg:
            # Make sure it works for \"clear C:\\foo\\bar.py:12\"
            i = arg.rfind(':')
            filename = arg[:i]
            arg = arg[i+1:]
            try:
                lineno = int(arg)
            except ValueError:
                err = \"Invalid line number (%s)\" % arg
            else:
                err = self.clear_break(filename, lineno)
            if err: print >>self.stdout, '***', err
            return
        numberlist = arg.split()
        for i in numberlist:
            try:
                i = int(i)
            except ValueError:
                print >>self.stdout, 'Breakpoint index %r is not a number' % i
                continue

            if not (0 <= i < len(bdb.Breakpoint.bpbynumber)):
                print >>self.stdout, 'No breakpoint numbered', i
                continue
            err = self.clear_bpbynumber(i)
            if err:
                print >>self.stdout, '***', err
            else:
                print >>self.stdout, 'Deleted breakpoint', i
    do_cl = do_clear # 'c' is already an abbreviation for 'continue'

    def do_where(self, arg):
        self.print_stack_trace()
    do_w = do_where
    do_bt = do_where

    def do_up(self, arg):
        if self.curindex == 0:
            print >>self.stdout, '*** Oldest frame'
        else:
            self.curindex = self.curindex - 1
            self.curframe = self.stack[self.curindex][0]
            self.curframe_locals = self.curframe.f_locals
            self.print_stack_entry(self.stack[self.curindex])
            self.lineno = None
    do_u = do_up

    def do_down(self, arg):
        if self.curindex + 1 == len(self.stack):
            print >>self.stdout, '*** Newest frame'
        else:
            self.curindex = self.curindex + 1
            self.curframe = self.stack[self.curindex][0]
            self.curframe_locals = self.curframe.f_locals
            self.print_stack_entry(self.stack[self.curindex])
            self.lineno = None
    do_d = do_down

    def do_until(self, arg):
        self.set_until(self.curframe)
        return 1
    do_unt = do_until

    def do_step(self, arg):
        self.set_step()
        return 1
    do_s = do_step

    def do_next(self, arg):
        self.set_next(self.curframe)
        return 1
    do_n = do_next

    def do_run(self, arg):
        \"\"\"Restart program by raising an exception to be caught in the main
        debugger loop.  If arguments were given, set them in sys.argv.\"\"\"
        if arg:
            import shlex
            argv0 = sys.argv[0:1]
            sys.argv = shlex.split(arg)
            sys.argv[:0] = argv0
        raise Restart

    do_restart = do_run

    def do_return(self, arg):
        self.set_return(self.curframe)
        return 1
    do_r = do_return

    def do_continue(self, arg):
        self.set_continue()
        return 1
    do_c = do_cont = do_continue

    def do_jump(self, arg):
        if self.curindex + 1 != len(self.stack):
            print >>self.stdout, \"*** You can only jump within the bottom frame\"
            return
        try:
            arg = int(arg)
        except ValueError:
            print >>self.stdout, \"*** The 'jump' command requires a line number.\"
        else:
            try:
                # Do the jump, fix up our copy of the stack, and display the
                # new position
                self.curframe.f_lineno = arg
                self.stack[self.curindex] = self.stack[self.curindex][0], arg
                self.print_stack_entry(self.stack[self.curindex])
            except ValueError, e:
                print >>self.stdout, '*** Jump failed:', e
    do_j = do_jump

    def do_debug(self, arg):
        sys.settrace(None)
        globals = self.curframe.f_globals
        locals = self.curframe_locals
        p = Pdb(self.completekey, self.stdin, self.stdout)
        p.prompt = \"(%s) \" % self.prompt.strip()
        print >>self.stdout, \"ENTERING RECURSIVE DEBUGGER\"
        sys.call_tracing(p.run, (arg, globals, locals))
        print >>self.stdout, \"LEAVING RECURSIVE DEBUGGER\"
        sys.settrace(self.trace_dispatch)
        self.lastcmd = p.lastcmd

    def do_quit(self, arg):
        self._user_requested_quit = 1
        self.set_quit()
        return 1

    do_q = do_quit
    do_exit = do_quit

    def do_EOF(self, arg):
        print >>self.stdout
        self._user_requested_quit = 1
        self.set_quit()
        return 1

    def do_args(self, arg):
        co = self.curframe.f_code
        dict = self.curframe_locals
        n = co.co_argcount
        if co.co_flags & 4: n = n+1
        if co.co_flags & 8: n = n+1
        for i in range(n):
            name = co.co_varnames[i]
            print >>self.stdout, name, '=',
            if name in dict: print >>self.stdout, dict[name]
            else: print >>self.stdout, \"*** undefined ***\"
    do_a = do_args

    def do_retval(self, arg):
        if '__return__' in self.curframe_locals:
            print >>self.stdout, self.curframe_locals['__return__']
        else:
            print >>self.stdout, '*** Not yet returned!'
    do_rv = do_retval

    def _getval(self, arg):
        try:
            return eval(arg, self.curframe.f_globals,
                        self.curframe_locals)
        except:
            t, v = sys.exc_info()[:2]
            if isinstance(t, str):
                exc_type_name = t
            else: exc_type_name = t.__name__
            print >>self.stdout, '***', exc_type_name + ':', repr(v)
            raise

    def do_p(self, arg):
        try:
            print >>self.stdout, repr(self._getval(arg))
        except:
            pass

    def do_pp(self, arg):
        try:
            pprint.pprint(self._getval(arg), self.stdout)
        except:
            pass

    def do_list(self, arg):
        self.lastcmd = 'list'
        last = None
        if arg:
            try:
                x = eval(arg, {}, {})
                if type(x) == type(()):
                    first, last = x
                    first = int(first)
                    last = int(last)
                    if last < first:
                        # Assume it's a count
                        last = first + last
                else:
                    first = max(1, int(x) - 5)
            except:
                print >>self.stdout, '*** Error in argument:', repr(arg)
                return
        elif self.lineno is None:
            first = max(1, self.curframe.f_lineno - 5)
        else:
            first = self.lineno + 1
        if last is None:
            last = first + 10
        filename = self.curframe.f_code.co_filename
        breaklist = self.get_file_breaks(filename)
        try:
            for lineno in range(first, last+1):
                line = linecache.getline(filename, lineno,
                                         self.curframe.f_globals)
                if not line:
                    print >>self.stdout, '[EOF]'
                    break
                else:
                    s = repr(lineno).rjust(3)
                    if len(s) < 4: s = s + ' '
                    if lineno in breaklist: s = s + 'B'
                    else: s = s + ' '
                    if lineno == self.curframe.f_lineno:
                        s = s + '->'
                    print >>self.stdout, s + '\\t' + line,
                    self.lineno = lineno
        except KeyboardInterrupt:
            pass
    do_l = do_list

    def do_whatis(self, arg):
        try:
            value = eval(arg, self.curframe.f_globals,
                            self.curframe_locals)
        except:
            t, v = sys.exc_info()[:2]
            if type(t) == type(''):
                exc_type_name = t
            else: exc_type_name = t.__name__
            print >>self.stdout, '***', exc_type_name + ':', repr(v)
            return
        code = None
        # Is it a function?
        try: code = value.func_code
        except: pass
        if code:
            print >>self.stdout, 'Function', code.co_name
            return
        # Is it an instance method?
        try: code = value.im_func.func_code
        except: pass
        if code:
            print >>self.stdout, 'Method', code.co_name
            return
        # None of the above...
        print >>self.stdout, type(value)

    def do_alias(self, arg):
        args = arg.split()
        if len(args) == 0:
            keys = self.aliases.keys()
            keys.sort()
            for alias in keys:
                print >>self.stdout, \"%s = %s\" % (alias, self.aliases[alias])
            return
        if args[0] in self.aliases and len(args) == 1:
            print >>self.stdout, \"%s = %s\" % (args[0], self.aliases[args[0]])
        else:
            self.aliases[args[0]] = ' '.join(args[1:])

    def do_unalias(self, arg):
        args = arg.split()
        if len(args) == 0: return
        if args[0] in self.aliases:
            del self.aliases[args[0]]

    #list of all the commands making the program resume execution.
    commands_resuming = ['do_continue', 'do_step', 'do_next', 'do_return',
                         'do_quit', 'do_jump']

    # Print a traceback starting at the top stack frame.
    # The most recently entered frame is printed last\;
    # this is different from dbx and gdb, but consistent with
    # the Python interpreter's stack trace.
    # It is also consistent with the up/down commands (which are
    # compatible with dbx and gdb: up moves towards 'main()'
    # and down moves towards the most recent stack frame).

    def print_stack_trace(self):
        try:
            for frame_lineno in self.stack:
                self.print_stack_entry(frame_lineno)
        except KeyboardInterrupt:
            pass

    def print_stack_entry(self, frame_lineno, prompt_prefix=line_prefix):
        frame, lineno = frame_lineno
        if frame is self.curframe:
            print >>self.stdout, '>',
        else:
            print >>self.stdout, ' ',
        print >>self.stdout, self.format_stack_entry(frame_lineno,
                                                     prompt_prefix)

    # Help methods (derived from pdb.doc)

    def help_help(self):
        self.help_h()

    def help_h(self):
        print >>self.stdout, \"\"\"h(elp)
Without argument, print the list of available commands.
With a command name as argument, print help about that command
\"help pdb\" pipes the full documentation file to the $PAGER
\"help exec\" gives help on the ! command\"\"\"

    def help_where(self):
        self.help_w()

    def help_w(self):
        print >>self.stdout, \"\"\"w(here)
Print a stack trace, with the most recent frame at the bottom.
An arrow indicates the \"current frame\", which determines the
context of most commands.  'bt' is an alias for this command.\"\"\"

    help_bt = help_w

    def help_down(self):
        self.help_d()

    def help_d(self):
        print >>self.stdout, \"\"\"d(own)
Move the current frame one level down in the stack trace
\(to a newer frame).\"\"\"

    def help_up(self):
        self.help_u()

    def help_u(self):
        print >>self.stdout, \"\"\"u(p)
Move the current frame one level up in the stack trace
\(to an older frame).\"\"\"

    def help_break(self):
        self.help_b()

    def help_b(self):
        print >>self.stdout, \"\"\"b(reak) ([file:]lineno | function) [, condition]
With a line number argument, set a break there in the current
file.  With a function name, set a break at first executable line
of that function.  Without argument, list all breaks.  If a second
argument is present, it is a string specifying an expression
which must evaluate to true before the breakpoint is honored.

The line number may be prefixed with a filename and a colon,
to specify a breakpoint in another file (probably one that
hasn't been loaded yet).  The file is searched for on sys.path\;
the .py suffix may be omitted.\"\"\"

    def help_clear(self):
        self.help_cl()

    def help_cl(self):
        print >>self.stdout, \"cl(ear) filename:lineno\"
        print >>self.stdout, \"\"\"cl(ear) [bpnumber [bpnumber...]]
With a space separated list of breakpoint numbers, clear
those breakpoints.  Without argument, clear all breaks (but
first ask confirmation).  With a filename:lineno argument,
clear all breaks at that line in that file.

Note that the argument is different from previous versions of
the debugger (in python distributions 1.5.1 and before) where
a linenumber was used instead of either filename:lineno or
breakpoint numbers.\"\"\"

    def help_tbreak(self):
        print >>self.stdout, \"\"\"tbreak same arguments as break, but breakpoint
is removed when first hit.\"\"\"

    def help_enable(self):
        print >>self.stdout, \"\"\"enable bpnumber [bpnumber ...]
Enables the breakpoints given as a space separated list of
bp numbers.\"\"\"

    def help_disable(self):
        print >>self.stdout, \"\"\"disable bpnumber [bpnumber ...]
Disables the breakpoints given as a space separated list of
bp numbers.\"\"\"

    def help_ignore(self):
        print >>self.stdout, \"\"\"ignore bpnumber count
Sets the ignore count for the given breakpoint number.  A breakpoint
becomes active when the ignore count is zero.  When non-zero, the
count is decremented each time the breakpoint is reached and the
breakpoint is not disabled and any associated condition evaluates
to true.\"\"\"

    def help_condition(self):
        print >>self.stdout, \"\"\"condition bpnumber str_condition
str_condition is a string specifying an expression which
must evaluate to true before the breakpoint is honored.
If str_condition is absent, any existing condition is removed\;
i.e., the breakpoint is made unconditional.\"\"\"

    def help_step(self):
        self.help_s()

    def help_s(self):
        print >>self.stdout, \"\"\"s(tep)
Execute the current line, stop at the first possible occasion
\(either in a function that is called or in the current function).\"\"\"

    def help_until(self):
        self.help_unt()

    def help_unt(self):
        print \"\"\"unt(il)
Continue execution until the line with a number greater than the current
one is reached or until the current frame returns\"\"\"

    def help_next(self):
        self.help_n()

    def help_n(self):
        print >>self.stdout, \"\"\"n(ext)
Continue execution until the next line in the current function
is reached or it returns.\"\"\"

    def help_return(self):
        self.help_r()

    def help_r(self):
        print >>self.stdout, \"\"\"r(eturn)
Continue execution until the current function returns.\"\"\"

    def help_continue(self):
        self.help_c()

    def help_cont(self):
        self.help_c()

    def help_c(self):
        print >>self.stdout, \"\"\"c(ont(inue))
Continue execution, only stop when a breakpoint is encountered.\"\"\"

    def help_jump(self):
        self.help_j()

    def help_j(self):
        print >>self.stdout, \"\"\"j(ump) lineno
Set the next line that will be executed.\"\"\"

    def help_debug(self):
        print >>self.stdout, \"\"\"debug code
Enter a recursive debugger that steps through the code argument
\(which is an arbitrary expression or statement to be executed
in the current environment).\"\"\"

    def help_list(self):
        self.help_l()

    def help_l(self):
        print >>self.stdout, \"\"\"l(ist) [first [,last]]
List source code for the current file.
Without arguments, list 11 lines around the current line
or continue the previous listing.
With one argument, list 11 lines starting at that line.
With two arguments, list the given range\;
if the second argument is less than the first, it is a count.\"\"\"

    def help_args(self):
        self.help_a()

    def help_a(self):
        print >>self.stdout, \"\"\"a(rgs)
Print the arguments of the current function.\"\"\"

    def help_p(self):
        print >>self.stdout, \"\"\"p expression
Print the value of the expression.\"\"\"

    def help_pp(self):
        print >>self.stdout, \"\"\"pp expression
Pretty-print the value of the expression.\"\"\"

    def help_exec(self):
        print >>self.stdout, \"\"\"(!) statement
Execute the (one-line) statement in the context of
the current stack frame.
The exclamation point can be omitted unless the first word
of the statement resembles a debugger command.
To assign to a global variable you must always prefix the
command with a 'global' command, e.g.:
^\(Pdb) global list_options\; list_options = ['-l']
^\(Pdb)\"\"\"

    def help_run(self):
        print \"\"\"run [args...]
Restart the debugged python program. If a string is supplied, it is
split with \"shlex\" and the result is used as the new sys.argv.
History, breakpoints, actions and debugger options are preserved.
\"restart\" is an alias for \"run\".\"\"\"

    help_restart = help_run

    def help_quit(self):
        self.help_q()

    def help_q(self):
        print >>self.stdout, \"\"\"q(uit) or exit - Quit from the debugger.
The program being executed is aborted.\"\"\"

    help_exit = help_q

    def help_whatis(self):
        print >>self.stdout, \"\"\"whatis arg
Prints the type of the argument.\"\"\"

    def help_EOF(self):
        print >>self.stdout, \"\"\"EOF
Handles the receipt of EOF as a command.\"\"\"

    def help_alias(self):
        print >>self.stdout, \"\"\"alias [name [command [parameter parameter ...]]]
Creates an alias called 'name' the executes 'command'.  The command
must *not* be enclosed in quotes.  Replaceable parameters are
indicated by %1, %2, and so on, while %* is replaced by all the
parameters.  If no command is given, the current alias for name
is shown. If no name is given, all aliases are listed.

Aliases may be nested and can contain anything that can be
legally typed at the pdb prompt.  Note!  You *can* override
internal pdb commands with aliases!  Those internal commands
are then hidden until the alias is removed.  Aliasing is recursively
applied to the first word of the command line\; all other words
in the line are left alone.

Some useful aliases (especially when placed in the .pdbrc file) are:

#Print instance variables (usage \"pi classInst\")
alias pi for k in %1.__dict__.keys(): print \"%1.\",k,\"=\",%1.__dict__[k]

#Print instance variables in self
alias ps pi self
\"\"\"

    def help_unalias(self):
        print >>self.stdout, \"\"\"unalias name
Deletes the specified alias.\"\"\"

    def help_commands(self):
        print >>self.stdout, \"\"\"commands [bpnumber]
\(com) ...
\(com) end
\(Pdb)

Specify a list of commands for breakpoint number bpnumber.  The
commands themselves appear on the following lines.  Type a line
containing just 'end' to terminate the commands.

To remove all commands from a breakpoint, type commands and
follow it immediately with end\; that is, give no commands.

With no bpnumber argument, commands refers to the last
breakpoint set.

You can use breakpoint commands to start your program up again.
Simply use the continue command, or step, or any other
command that resumes execution.

Specifying any command resuming execution (currently continue,
step, next, return, jump, quit and their abbreviations) terminates
the command list (as if that command was immediately followed by end).
This is because any time you resume execution
\(even with a simple next or step), you may encounter
another breakpoint--which could have its own command list, leading to
ambiguities about which list to execute.

   If you use the 'silent' command in the command list, the
usual message about stopping at a breakpoint is not printed.  This may
be desirable for breakpoints that are to print a specific message and
then continue.  If none of the other commands print anything, you
see no sign that the breakpoint was reached.
\"\"\"

    def help_pdb(self):
        help()

    def lookupmodule(self, filename):
        \"\"\"Helper function for break/clear parsing -- may be overridden.

        lookupmodule() translates (possibly incomplete) file or module name
        into an absolute file name.
        \"\"\"
        if os.path.isabs(filename) and os.path.exists(filename):
            return filename
        f = os.path.join(sys.path[0], filename)
        if os.path.exists(f) and self.canonic(f) == self.mainpyfile:
            return f
        root, ext = os.path.splitext(filename)
        if ext == '':
            filename = filename + '.py'
        if os.path.isabs(filename):
            return filename
        for dirname in sys.path:
            while os.path.islink(dirname):
                dirname = os.readlink(dirname)
            fullname = os.path.join(dirname, filename)
            if os.path.exists(fullname):
                return fullname
        return None

    def _runscript(self, filename):
        # The script has to run in __main__ namespace (or imports from
        # __main__ will break).
        #
        # So we clear up the __main__ and set several special variables
        # (this gets rid of pdb's globals and cleans old variables on restarts).
        import __main__
        __main__.__dict__.clear()
        __main__.__dict__.update({\"__name__\"    : \"__main__\",
                                  \"__file__\"    : filename,
                                  \"__builtins__\": __builtins__,
                                 })

        # When bdb sets tracing, a number of call and line events happens
        # BEFORE debugger even reaches user's code (and the exact sequence of
        # events depends on python version). So we take special measures to
        # avoid stopping before we reach the main script (see user_line and
        # user_call for details).
        self._wait_for_mainpyfile = 1
        self.mainpyfile = self.canonic(filename)
        self._user_requested_quit = 0
        statement = 'execfile(%r)' % filename
        self.run(statement)

# Simplified interface

def run(statement, globals=None, locals=None):
    Pdb().run(statement, globals, locals)
"
    (let (py-font-lock-defaults-p)
      (goto-char (point-min))
      (search-forward "return answer")
      (py-end-of-def-or-class)
      (should (looking-back "self.run(statement)" (line-beginning-position))))))

(ert-deftest py-forward-def-or-class-test-pB8W4q ()
  (py-test-with-temp-buffer
      "class Pdb(bdb.Bdb, cmd.Cmd):

    def __init__(self, completekey='tab', stdin=None, stdout=None, skip=None):
        bdb.Bdb.__init__(self, skip=skip)
        cmd.Cmd.__init__(self, completekey, stdin, stdout)
        if stdout:
            self.use_rawinput = 0
        self.prompt = '(Pdb) '
        self.aliases = {}
        self.mainpyfile = ''
        self._wait_for_mainpyfile = 0
        # Try to load readline if it exists
        try:
            import readline
        except ImportError:
            pass

    def print_stack_entry(self, frame_lineno, prompt_prefix=line_prefix):
        frame, lineno = frame_lineno
        if frame is self.curframe:
            print >>self.stdout, '>',
        else:
            print >>self.stdout, ' ',
        print >>self.stdout, self.format_stack_entry(frame_lineno,
                                                     prompt_prefix)

    # Help methods (derived from pdb.doc)

    def help_help(self):
        self.help_h()

    def help_h(self):
        print >>self.stdout, \"\"\"h(elp)
Without argument, print the list of available commands.
With a command name as argument, print help about that command
\"help pdb\" pipes the full documentation file to the $PAGER
\"help exec\" gives help on the ! command\"\"\"
"
    (goto-char (point-max))
    (search-backward "prompt_prefix)" nil t)
    (end-of-line)
    (py-forward-def-or-class)
    (should (looking-back "()" (line-beginning-position)))
    (py-forward-def-or-class)
    (should (looking-back "command\"\"\"" (line-beginning-position)))))

(provide 'py-ert-tests-2)
;;; py-ert-tests-2.el ends here
