;; py-ert-tests.el --- Tests, some adapted from python.el

;; Copyright (C) 2013 Free Software Foundation, Inc.
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

;; (require 'ert)

;; tests are expected to run from directory test

(setq ert-test-default-buffer "*Python*")

(add-to-list 'load-path default-directory)
(require 'python-mode-test)

(defmacro py-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (python-mode)
       (insert ,contents)
       (message "ERT %s" (point))
       (goto-char (point-min))
       ,@body)))

(defmacro py-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
;;     (and (featurep 'python) (unload-feature 'python))
     (let (hs-minor-mode)
       (python-mode)
       (insert ,contents)
       (message "ERT %s" (point))

       ,@body)))

(defun py-tests-go-to (string)
  "Move point at beginning of STRING in the current test. "
  (and (eq (point) (point-max))(goto-char (point-min)))
  (search-forward string nil t 1))

(ert-deftest py-ert-electric-kill-backward-test1 ()
  (let ((py-electric-kill-backward-p t))
    (with-temp-buffer
      (insert "mystring[0:1]")
      (py-electric-backspace 1)
      (should (eq ?\] (char-after))))))

(ert-deftest py-ert-electric-kill-backward-test2 ()
  (let ((py-electric-kill-backward-p t))
    (with-temp-buffer
      (insert "mystring(\"asdf\")")
      (py-electric-backspace 1)
      (should (eq ?\) (char-after)))
      )))

(ert-deftest py-ert-electric-kill-backward-test3 ()
  (let ((py-electric-kill-backward-p t))
    (with-temp-buffer
      (insert "mystring{0 . 1}")
      (py-electric-backspace 1)
      (should (eq ?\} (char-after))))))

(ert-deftest py-ert-indent-dedenters-1 ()
  "Check all dedenters."
  (py-test-with-temp-buffer-point-min
   "
def foo(a, b, c):
    if a:
        print (a)
    elif b:
        print (b)
    else:
        try:
            print (c.pop())
        except (IndexError, AttributeError):
            print (c)
        finally:
            print ('nor a, nor b are true')
"
   (py-tests-go-to "if a:")
   (should (= (py-compute-indentation) 4))
   (py-tests-go-to "print (a)")
   (should (= (py-compute-indentation) 8))
   (py-tests-go-to "elif b:")
   (should (= (py-compute-indentation) 4))
   (py-tests-go-to "print (b)")
   (should (= (py-compute-indentation) 8))
   (py-tests-go-to "else:")
   (should (= (py-compute-indentation) 4))
   (py-tests-go-to "try:")
   (should (= (py-compute-indentation) 8))
   (py-tests-go-to "print (c.pop())")
   (should (= (py-compute-indentation) 12))
   (py-tests-go-to "except (IndexError, AttributeError):")
   (should (= (py-compute-indentation) 8))
   (py-tests-go-to "print (c)")
   (should (= (py-compute-indentation) 12))
   (py-tests-go-to "finally:")
   (should (= (py-compute-indentation) 8))
   (py-tests-go-to "print ('nor a, nor b are true')")
   (should (= (py-compute-indentation) 12))))

(ert-deftest py-ert-indent-after-backslash-lp-852052-1 ()
  "The most common case."
  (py-test-with-temp-buffer-point-min
      "
from foo.bar.baz import something, something_1 \\
     something_2 something_3, \\
     something_4, something_5
"
    (py-tests-go-to "from foo.bar.baz import something, something_1")
    (should (= (py-compute-indentation) 0))
    (py-tests-go-to "something_2 something_3,")
    (should (= (py-compute-indentation) 5))
    (py-tests-go-to "something_4, something_5")
    (should (= (py-compute-indentation) 5))))

(ert-deftest py-ert-indent-closing ()
  ""
  (py-test-with-temp-buffer-point-min
   "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]
result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',
    )
"
   (goto-char 40)
   (should (eq 4 (py-compute-indentation)))
   (goto-char 129)
   (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-ert-moves ()
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
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (font-lock-fontify-buffer)
    (message "comment-start: %s" comment-start)
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 561 (py-up-clause-bol)))
    (message "%s" "py-up-clause-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-block-or-clause-bol)))
    (message "%s" "py-up-block-or-clause-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-up-def-bol)))
    (message "%s" "py-up-def-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 1 (py-up-class-bol)))
    (message "%s" "py-up-class-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-up-def-or-class-bol)))
    (message "%s" "py-up-def-or-class-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-block-bol)))
    (message "%s" "py-up-block-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-minor-block-bol)))
    (message "%s" "py-up-minor-block-bol-test of `py-moves-test'  done")
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 325 (py-up-block)))
    (message "%s" "py-up-block-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 325 (py-up-minor-block)))
    (message "%s" "py-up-minor-block-test of `py-moves-test'  done")
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 569 (py-up-clause)))
    (message "%s" "py-up-clause-test of `py-moves-test'  done")
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 569 (py-up-block-or-clause)))
    (message "%s" "py-up-block-or-clause-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 238 (py-up-def)))
    (message "%s" "py-up-def-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 1 (py-up-class)))
    (message "%s" "py-up-class-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 238 (py-up-def-or-class)))
    (message "%s" "py-up-def-or-class-test of `py-moves-test'  done")
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-bol)))
    (message "%s" "py-down-block-bol-test of `py-moves-test'  done")
    (goto-char 561)
    ;; (sit-for 1)
    (should (eq 594 (py-down-clause-bol)))
    (message "%s" "py-down-clause-bol-test of `py-moves-test'  done")
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-or-clause-bol)))
    (message "%s" "py-down-block-or-clause-bol-test of `py-moves-test'  done")
    (goto-char (point-min))
    (should (eq 142 (py-down-def-bol)))
    (message "%s" "py-down-def-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (not (py-down-class-bol)))
    (message "%s" "py-down-class-bol-test of `py-moves-test'  done")
    (goto-char (point-min))
    (should (eq 142 (py-down-def-or-class-bol)))
    (message "%s" "py-down-def-or-class-bol-test of `py-moves-test'  done")
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 325 (py-down-block)))
    (message "%s" "py-down-block-test of `py-moves-test'  done")
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-bol)))
    (message "%s" "py-down-block-bol-test of `py-moves-test'  done")

    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 325 (py-down-minor-block)))
    (message "%s" "py-down-minor-block-test of `py-moves-test'  done")
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-minor-block-bol)))
    (message "%s" "py-down-minor-block-bol-test of `py-moves-test'  done")

    (goto-char 569)
    ;; (sit-for 1)
    (should (eq 602 (py-down-clause)))
    (message "%s" "py-down-clause-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 569 (py-down-block-or-clause)))
    (message "%s" "py-down-block-or-clause-test of `py-moves-test'  done")
    (goto-char (point-min))
    (should (eq 146 (py-down-def)))
    (message "%s" "py-down-def-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (not (py-down-class)))
    (message "%s" "py-down-class-test of `py-moves-test'  done")
    (goto-char (point-min))
    (should (eq 146 (py-down-def-or-class)))
    (message "%s" "py-down-def-or-class-test of `py-moves-test'  done")

    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 332 (py-beginning-of-statement-bol)))
    (message "%s" "py-beginning-of-statement-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-beginning-of-block-bol)))
    (message "%s" "py-beginning-of-block-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-beginning-of-clause-bol)))
    (message "%s" "py-beginning-of-clause-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-beginning-of-block-or-clause-bol)))
    (message "%s" "py-beginning-of-block-or-clause-bol-test of `py-moves-test'  done")
    (should (eq 1 (py-beginning-of-class-bol)))
    (message "%s" "py-beginning-of-class-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-beginning-of-def-or-class-bol)))
    (message "%s" "py-beginning-of-def-or-class-bol-test of `py-moves-test'  done")
    (message "%s" "py-end-of-block-bol-test of `py-moves-test'  done")
    (goto-char 576)
    ;; (sit-for 1)
    (should (eq 594 (py-end-of-clause-bol)))
    (message "%s" "py-end-of-clause-bol-test of `py-moves-test'  done")
    (goto-char 576)
    ;; (sit-for 1)
    (should (eq 594 (py-end-of-block-or-clause-bol)))
    (message "%s" "py-end-of-block-or-clause-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-end-of-def-bol)))
    (message "%s" "py-end-of-def-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-end-of-class-bol)))
    (message "%s" "py-end-of-class-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-end-of-def-or-class-bol)))
    (message "%s" "py-end-of-def-or-class-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 561 (py-end-of-statement-bol)))
    (message "%s" "py-end-of-statement-bol-test of `py-moves-test'  done")
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-beginning-of-def-bol)))
    (message "%s" "py-beginning-of-def-bol-test of `py-moves-test'  done")))

(ert-deftest py-ert-indent-tabs-mode-test ()
  (py-test-with-temp-buffer-point-min
      "class OrderedDict1(dict):"
    (end-of-line)
    (let ((indent-tabs-mode t))
      (py-newline-and-indent)
      (should (looking-back "^\t")))))

(ert-deftest py-ert-no-indent-tabs-mode-test ()
  (py-test-with-temp-buffer-point-min
      "class OrderedDict1(dict):"
    (end-of-line)
    (let ((indent-tabs-mode))
      (py-newline-and-indent)
      (should (looking-back "^    ")))))

(ert-deftest py-ert-pyflakespep-command-test ()
  (py-test-with-temp-buffer-point-min
      ""
      (file-readable-p py-pyflakespep8-command)))

(ert-deftest py-ert-bogus-dedent-when-typing-colon-in-dictionary-literal-lp-1197171 ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Put point at the end of the last line and hit colon, as you would to
# separate the key from the value. The last line will incorrectly dedent
# to column 4. Indentation should not change.

def foo():
    bar('thing',
        {'another'"
    (should (eq 8 (py-compute-indentation)))))

(ert-deftest py-ert-pep-arglist-indent ()
  (py-test-with-temp-buffer-point-min
      "# Aligned with opening delimiter
foo = long_function_name(var_one, var_two,
                         var_three, var_four)

# More indentation included to distinguish this from the rest.
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    print(var_one)
"
    (search-forward "var_three")
    (should (eq 25 (py-compute-indentation)))
    (search-forward "var_three")
    (should (eq 8 (py-compute-indentation)))

    ))

(ert-deftest py-ert-close-at-start-column ()
  (py-test-with-temp-buffer-point-min
      "# boolean `py-closing-list-dedents-bos',

# Current behavior matches default nil

my_list = [
    1, 2, 3,
    4, 5, 6,
    ]

result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',)

# When non-nil, it will be lined up under the first character of the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6,
]

result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',)

# Examples see PEP8

asdf = {
    'a':{
        'b':3,
        'c':4
    }
}

data = {
    'key':
    {
        'objlist': [
            {
                'pk': 1,
                'name': 'first',
            },
            {
                'pk': 2,
                'name': 'second',
            }
        ]
    }
}

"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (let (py-closing-list-dedents-bos)
      (search-forward "]")
      (should (eq 4 (py-compute-indentation)))
      (search-forward ")")
      (should (eq 4 (py-compute-indentation)))
      (setq py-closing-list-dedents-bos t)
      (search-forward "]")
      (should (eq 0 (py-compute-indentation)))
      (search-forward ")")
      (should (eq 0 (py-compute-indentation)))
      ;; dicts
      (search-forward "}")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 0 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 12 (py-compute-indentation)))
      (search-forward "]")
      (should (eq 8 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-top-level ()
  (py-test-with-temp-buffer-point-min
      "klauf = kugel()

with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
            datei.write(str(spiel[i]) + \"\\n\")
"
    (message "%s" (point))
    (should (eq 16 (py-end-of-top-level)))
    (should (eq 168 (py-end-of-top-level)))

    (should (eq 18 (py-beginning-of-top-level)))
    (should (eq 1 (py-beginning-of-top-level)))
    (should (eq 1 (py-beginning-of-top-level-p)))))

(ert-deftest py-ert-position-tests ()
  (interactive)
  (py-test-with-temp-buffer-point-min
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
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (font-lock-fontify-buffer)
    (search-forward "else:")
    (forward-char -1)
    (should (eq 1 (py-beginning-of-top-level-position)))
    (should (eq 445 (py-end-of-top-level-position)))
    (should (eq 362 (py--beginning-of-statement-position)))
    (should (eq 367 (py--end-of-statement-position)))
    (should (eq 367 (py--end-of-line-position)))
    (should (eq 354 (py--beginning-of-line-position)))
    (should (eq 1 (py--beginning-of-paragraph-position)))
    (should (eq 446 (py--end-of-paragraph-position)))
    (should (eq 190 (py--beginning-of-block-position)))
    (should (eq 445 (py--end-of-block-position)))
    (should (eq 190 (py-beginning-of-minor-block-position)))
    (should (eq 445 (py-end-of-minor-block-position)))
    (should (eq 362 (py--beginning-of-clause-position)))
    (should (eq 445 (py--end-of-clause-position)))
    (should (eq 362 (py--beginning-of-block-or-clause-position)))
    (should (eq 445 (py--end-of-block-or-clause-position)))
    (should (eq 71 (py--beginning-of-def-position)))
    (should (eq 445 (py--end-of-def-position)))
    (should (eq 1 (py--beginning-of-class-position)))
    (should (eq 445 (py--end-of-class-position)))
    (should (eq 71 (py--beginning-of-def-or-class-position)))
    (should (eq 445 (py--end-of-def-or-class-position)))
    (search-forward "#")
    (should (eq 380 (py-beginning-of-comment-position)))
    (should (eq 412 (py-end-of-comment-position)))))

(ert-deftest py-ert-copy-statement-test ()
  (interactive)
  (py-test-with-temp-buffer-point-min
   "from foo.bar.baz import something
"
   (should (and (not (py-copy-statement))(string-match "from foo.bar.baz import something" (car kill-ring))))))

;; (ert-deftest py-ert-abbrevs-changed-lp-1270631 ()
;;   (interactive)
;;   (with-temp-buffer
;;     (insert "foo")
;;     (emacs-lisp-mode)
;;     (define-abbrev lisp-mode-abbrev-table "fooa" "fooa")
;;     (should abbrevs-changed)
;;     (python-mode)
;;     (should abbrevs-changed)))

(ert-deftest py-ert-honor-dedent-lp-1280982 ()
  (py-test-with-temp-buffer
      "def foo():
    def bar():
        asdf
    "
    (py-newline-and-indent)
    (py-electric-backspace)
    (py-newline-and-indent)
    (should (eq 42 (point)))))

(ert-deftest py-ert-socket-modul-completion-lp-1284141 ()
  (py-test-with-temp-buffer-point-min
      "import socket"
    (let ((py-debug-p t)
	  oldbuf)
      (py-execute-buffer-dedicated)
      (sit-for 0.1 t)
      (set-buffer py-output-buffer)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (insert "socket.")
      (sit-for 0.1)
      ;; (switch-to-buffer (current-buffer))
      (py-indent-or-complete)
      (sit-for 0.1)
      (set-buffer "*Python Completions*")
      ;; (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (sit-for 0.2)
      (prog1 (should (search-forward "socket."))
	(py-kill-buffer-unconditional oldbuf)
	(py-kill-buffer-unconditional "py-buffer-name.txt")))))

(ert-deftest py-ert-fill-paragraph-lp-1286318 ()
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
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (font-lock-fontify-buffer)
    (goto-char 49)
    (sit-for 0.1 t)
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
    (sit-for 0.1 t)
    (should (not (empty-line-p)))

    ))

(ert-deftest py-ert-fill-paragraph-pep-257-nn ()
  (let ((py-docstring-style 'pep-257-nn))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (font-lock-fontify-buffer)
      (goto-char 49)
      (py-fill-string)
      (end-of-line)
      (sit-for 0.1 t)
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
      (sit-for 0.1 t)
      (should (<= (current-column) 72))
      )))

(ert-deftest py-ert-fill-paragraph-pep-257 ()
  (let ((py-docstring-style 'pep-257))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (font-lock-fontify-buffer)
;;      (switch-to-buffer (current-buffer))
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

(ert-deftest py-ert-fill-paragraph-onetwo ()
  (let ((py-docstring-style 'onetwo))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (font-lock-fontify-buffer)
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

(ert-deftest py-ert-fill-paragraph-django ()
  (let ((py-docstring-style 'django))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (python-mode)
      (font-lock-fontify-buffer)
      (goto-char 49)
      (fill-paragraph)
      (search-backward "\"\"\"")
      (goto-char (match-end 0))
      (eolp)
      (forward-line 1)
      (end-of-line)
      (when py-debug-p (message "fill-column: %s" fill-column))
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -2)
      (should (empty-line-p)))))

(ert-deftest py-ert-fill-paragraph-symmetric ()
  (let ((py-docstring-style 'symmetric))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (font-lock-fontify-buffer)
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

(ert-deftest py-partial-expression-test ()
  (py-test-with-temp-buffer-point-min
      "foo=1"
    (and (should (string= "foo" (py-partial-expression)))
	 (py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-ert-execute-statement-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-test\")"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python"))
      (py-execute-statement)
      (set-buffer ert-test-default-buffer)
      (when py-debug-p (switch-to-buffer (current-buffer))) 
      (goto-char (point-max)) 
      (sit-for 0.3 t)
      (and (should (search-backward "py-execute-statement-test" nil t 1))
	   (py-kill-buffer-unconditional (current-buffer))))))

(ert-deftest py-ert-execute-statement-python2-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-python2-test\")"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (py-execute-statement-python2)
    (set-buffer "*Python2*")
    (goto-char (point-max))
    (sit-for 0.2 t)
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (and (should (search-backward "py-execute-statement-python2-test" nil t 1))
	 (py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-ert-execute-statement-python3-dedicated-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-python3-dedicated-test\")"
    (let ((py-debug-p t)
	  py-store-result-p
	  erg)
      (call-interactively 'py-execute-statement-python3-dedicated)
      (sit-for 0.1 t)
      ;; (when py-debug-p (message "py-ert-execute-statement-python3-dedicated-test: %s" py-buffer-name))
      (set-buffer py-buffer-name)
      (goto-char (point-min))
      (should (search-forward "py-execute-statement-python3-dedicated-test" nil t 1)))))

(ert-deftest py-ert-execute-statement-split ()
  (py-test-with-temp-buffer-point-min
      "print(123)"
    (let ((py-split-window-on-execute t))
      (py-execute-statement)
      (sit-for 0.1 t) 
      (should (not (one-window-p))))))

;;;

(provide 'py-ert-tests-1)
