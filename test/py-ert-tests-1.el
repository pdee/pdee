;; py-ert-tests.el --- Tests, some adapted from python.el

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2015 Andreas Roehler, <andreas.roehler@online.de>

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

(defvar py-def-and-class-test-string "class kugel(object):
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
")

(setq ert-test-default-buffer "*Python*")

(add-to-list 'load-path default-directory)
(require 'python-mode-test)


(defun py-tests-go-to (string)
  "Move point at beginning of STRING in the current test. "
  (and (eq (point) (point-max))(goto-char (point-min)))
  (search-forward string nil t 1))

(ert-deftest py-ert-electric-kill-backward-bracket-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring[0:1]"
      (py-electric-backspace 1)
      (should (eq ?\] (char-after))))))

(ert-deftest py-ert-electric-kill-backward-region-test ()
  (let ((py-electric-kill-backward-p t)
	(delete-active-region t)
	(transient-mark-mode t))
    (py-test-with-temp-buffer
	"mystring[0:1]     "
      (skip-chars-backward " \t\r\n\f")
      (set-mark (point))
      (goto-char (point-max))
      (py-electric-backspace 1)
      (should (eq ?\] (char-before))))))

(ert-deftest py-ert-electric-delete-eob-test ()
  (let ((py-electric-kill-backward-p t)
	(delete-active-region t)
	(transient-mark-mode t))
    (py-test-with-temp-buffer
	"mystring[0:1]     "
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
      (set-mark (point))
      (skip-chars-backward " \t\r\n\f")
      (py-electric-delete)
      (should (eobp)))))

(ert-deftest py-ert-electric-kill-backward-paren-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring(\"asdf\")"
      (py-electric-backspace 1)
      (should (eq ?\) (char-after)))
      )))

(ert-deftest py-ert-electric-kill-backward-brace-test ()
  (let ((py-electric-kill-backward-p t))
    (py-test-with-temp-buffer
      "mystring{0 . 1}"
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
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 561 (py-up-clause-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-block-or-clause-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-up-def-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 1 (py-up-class-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-up-def-or-class-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-block-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-up-minor-block-bol)))
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 325 (py-up-block)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 325 (py-up-minor-block)))
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 569 (py-up-clause)))
    (goto-char 592)
    ;; (sit-for 1)
    (should (eq 569 (py-up-block-or-clause)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 238 (py-up-def)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 1 (py-up-class)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 238 (py-up-def-or-class)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-bol)))
    (goto-char 561)
    ;; (sit-for 1)
    (should (eq 594 (py-down-clause-bol)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-or-clause-bol)))
    (goto-char (point-min))
    (should (eq 142 (py-down-def-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (not (py-down-class-bol)))
    (goto-char (point-min))
    (should (eq 142 (py-down-def-or-class-bol)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 325 (py-down-block)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-block-bol)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 325 (py-down-minor-block)))
    (goto-char 264)
    ;; (sit-for 1)
    (should (eq 317 (py-down-minor-block-bol)))
    (goto-char 569)
    ;; (sit-for 1)
    (should (eq 602 (py-down-clause)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 569 (py-down-block-or-clause)))
    (goto-char (point-min))
    (should (eq 146 (py-down-def)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (not (py-down-class)))
    (goto-char (point-min))
    (should (eq 146 (py-down-def-or-class)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 332 (py-backward-statement-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-backward-block-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-backward-clause-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 317 (py-backward-block-or-clause-bol)))
    (should (eq 1 (py-backward-class-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-backward-def-or-class-bol)))
    (goto-char 576)
    ;; (sit-for 1)
    (should (eq 594 (py-forward-clause-bol)))
    (goto-char 576)
    ;; (sit-for 1)
    (should (eq 594 (py-forward-block-or-clause-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-forward-def-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-forward-class-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 627 (py-forward-def-or-class-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 561 (py-forward-statement-bol)))
    (goto-char 410)
    ;; (sit-for 1)
    (should (eq 234 (py-backward-def-bol)))
    ;; (message "%s" "py-backward-def-bol-test of `py-moves-test'  done")
    ))

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
    (should (eq 16 (py-forward-top-level)))
    (should (eq 168 (py-forward-top-level)))

    (should (eq 18 (py-backward-top-level)))
    (should (eq 1 (py-backward-top-level)))
    (should (eq 1 (py--beginning-of-top-level-p)))))

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
    (search-forward "else:")
    (forward-char -1)
    (should (eq 1 (py--beginning-of-top-level-position)))
    (should (eq 445 (py--end-of-top-level-position)))
    (should (eq 362 (py--beginning-of-statement-position)))
    (should (eq 367 (py--end-of-statement-position)))
    (should (eq 1 (py--beginning-of-paragraph-position)))
    (should (eq 446 (py--end-of-paragraph-position)))
    (should (eq 190 (py--beginning-of-block-position)))
    (should (eq 445 (py--end-of-block-position)))
    (should (eq 190 (py--beginning-of-minor-block-position)))
    (should (eq 445 (py--end-of-minor-block-position)))
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
    (should (eq 380 (py--beginning-of-comment-position)))
    (should (eq 412 (py--end-of-comment-position)))))

(ert-deftest py-ert-copy-statement-test ()
  (interactive)
  (py-test-with-temp-buffer-point-min
   "from foo.bar.baz import something
"
   (when py-debug-p (switch-to-buffer (current-buffer))
	 (font-lock-fontify-buffer))
   (py-copy-statement)
   (should (string-match "from foo.bar.baz import something" (car kill-ring)))))

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
  (dolist (ele py-ert-test-default-executables)
    (when (buffer-live-p (get-buffer "*Python Completions*"))
      (py-kill-buffer-unconditional (get-buffer "*Python Completions*")))
    (py-test-with-temp-buffer
	"import socket\nsocket."
      (let ((py-debug-p t)
	    (py-shell-name ele)
	    oldbuf)
	(when py-debug-p (switch-to-buffer (current-buffer))
	      (font-lock-fontify-buffer))
	(py-indent-or-complete)
	(if (string-match "ipython" ele)
	    (sit-for 0.5)
	  (sit-for 0.1))
	(should (buffer-live-p (get-buffer "*Python Completions*")))
	(set-buffer "*Python Completions*")
	(switch-to-buffer (current-buffer))
	(goto-char (point-min))
	(sit-for 0.1)
	(prog1 (should (search-forward "socket."))
	  (py-kill-buffer-unconditional (current-buffer)))))))

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
      (should (eolp))
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
     (message "%s" (py-partial-expression))
    (and (should (string= "foo" (py-partial-expression)))
	 (py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-ert-execute-statement-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-test\")"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python"))
      (py-execute-statement)
      (set-buffer ert-test-default-buffer)
      (when py-debug-p (switch-to-buffer (current-buffer))
	    (font-lock-fontify-buffer))
      (goto-char (point-max))
      (sit-for 0.3 t)
      (and (should (search-backward "py-execute-statement-test" nil t 1))
	   (sit-for 0.1 t)
	   (py-kill-buffer-unconditional (current-buffer))))))

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

(ert-deftest py-ert-script-buffer-appears-instead-of-python-shell-buffer-lp-957561-test ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python
 # -*- coding: utf-8 -*-
print(\"I'm the script-buffer-appears-instead-of-python-shell-buffer-lp-957561-test\")
"
     (let (py-switch-buffers-on-execute-p
	  (py-split-window-on-execute t))
      (delete-other-windows)
      (ipython)
      (sit-for 0.1)
      (py-execute-buffer-ipython)
      ;; (should (window-live-p (other-buffer)))
      (should (not (window-full-height-p))))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-1-test ()
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
   (search-forward "True")
   (py-indent-region (line-beginning-position) (point-max))
   (should (eq 4 (current-indentation)))
   (search-forward "with file")
   (should (eq 4 (current-indentation)))
   (search-forward "for i ")
   (should (eq 8 (current-indentation)))
   (search-forward "bar.")
   (should (eq 12 (current-indentation)))
   (search-forward "datei.write")
   (should (eq 12 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-2-test ()
  "Keep indent of remaining block as first line was fixed. "
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
   (search-forward "def foo")
   (py-indent-region (line-beginning-position) (point-max))
   (search-forward "True")
   (should (eq 4 (current-indentation)))
   (search-forward "with file")
   (should (eq 0 (current-indentation)))
   (search-forward "for i ")
   (should (eq 4 (current-indentation)))
   (search-forward "bar.")
   (should (eq 8 (current-indentation)))
   (search-forward "datei.write")
   (should (eq 8 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-no-arg-3-test ()
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
        # also wrong indent needs to be preserved here
            datei.write(str(baz[i]) + \"\\n\")
"
   (search-forward "True")
   (py-indent-region (line-beginning-position) (point-max))
   (should (eq 4 (current-indentation)))
   (search-forward "with file")
   (should (eq 4 (current-indentation)))
   (search-forward "for i ")
   (should (eq 8 (current-indentation)))
   (search-forward "bar.")
   (should (eq 12 (current-indentation)))
   (search-forward "datei.write")
   (should (eq 16 (current-indentation)))))

(ert-deftest indent-region-lp-997958-lp-1426903-arg-1-test ()
  (py-test-with-temp-buffer
   "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo ():
print(123)

with file(\"foo\" + zeit + \".ending\", 'w') as datei:
for i in range(anzahl):
bar.dosomething()
datei.write(str(baz[i]) + \"\\n\")
"
   (py-indent-region 48 (point-max) '(4))
   (goto-char (point-min))
   (search-forward "print(123)")
   (should (eq 4 (current-indentation)))
   (search-forward "with file")
   (should (eq 4 (current-indentation)))
   (search-forward "for i ")
   (should (eq 8 (current-indentation)))
   (search-forward "bar.")
   (should (eq 12 (current-indentation)))
   (search-forward "datei.write")
   (should (eq 12 (current-indentation)))))

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
   (search-forward "with file")
   (py-indent-region (line-beginning-position) (point-max))
   (should (eq 0 (current-indentation)))
   (search-forward "for i ")
   (should (eq 4 (current-indentation)))
   (search-forward "bar.")
   (should (eq 8 (current-indentation)))
   (search-forward "datei.write")
   (should (eq 8 (current-indentation)))))

(ert-deftest py--pdb-versioned-test ()
  (py-test-with-temp-buffer
      ""
    (let ((py-shell-name "python3"))
      (should (string= "pdb3" (py--pdb-versioned))))
    (let ((py-shell-name "python"))
      (should (string= "pdb" (py--pdb-versioned))))))

(ert-deftest py-ert-forward-expression-test ()
    (py-test-with-temp-buffer-point-min
	py-def-and-class-test-string
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

(ert-deftest py-ert-backward-expression-test ()
    (py-test-with-temp-buffer
	py-def-and-class-test-string
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
      (should (eq (char-after) ?\[))
      
      ))

(provide 'py-ert-tests-1)
;;; py-ert-tests-1.el ends here
