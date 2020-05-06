;;; py-interactive-tests.el --- Tests expected to succeed interactively -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: lisp

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

;;

;;; Code:

(defun py-shell-complete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
impo")))
    (py-bug-tests-intern 'py-shell-complete-base arg teststring)))

(defun py-shell-complete-base ()
  (when (and (interactive-p) py-debug-p) (switch-to-buffer (current-buffer))
	(jit-lock-fontify-now))
  ;; (sit-for 0.1 t)
  (py-shell-complete)
  ;; (sit-for 0.1)
  (assert (looking-back "import") nil "py-shell-complete-test failed"))

(ert-deftest py-ert-shift-right-test-1-n8l82j ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-2-CF12wC ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (py-shift-right 1)
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-3-fxBOYU ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (beginning-of-line)
    (py-shift-right 1)
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-4-Ub4nod ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (forward-word 1)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))
    (beginning-of-buffer)
    (should (eq 0 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-5-rmZsLv ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (forward-char -2)
    (deactivate-mark)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))
    (beginning-of-buffer)
    (should (eq 0 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-6-iJd75N ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (forward-char -2)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))
    (beginning-of-buffer)
    (should (eq 0 (current-indentation)))))

(ert-deftest py-execute-region-test-1-R45Ll6 ()
  (py-test-with-temp-buffer
      "def foo(x):
    if x == 1:
        return 0"
    (let ((py-output-buffer (py-shell)))
      (push-mark)
      (goto-char (point-min))
      (py-execute-region (point) (mark))
      (message "%s" py-output-buffer)
      (set-buffer py-output-buffer)
      ;; (set-buffer "*Python3*")
      (switch-to-buffer (current-buffer))
      (should-not (search-backward "FileNotFoundError" nil t 1)))))

(ert-deftest py-end-of-def-or-class-test-1-YpTSCo ()
  (py-test-with-temp-buffer
      "class foo:
    \"\"\"asdf\"\"\"
    def bar():
        \"\"\"\"\"\"
        return True

    @asdf
    def baz():
        \"\"\"\"\"\"
        pass"
    (goto-char (point-max))
    (search-backward "@asdf")
    (end-of-line)
    (py-end-of-def-or-class)
    (should (looking-back "pass"))))

(ert-deftest py-down-statement-test-1-zsvwPG ()
  (py-test-with-temp-buffer
      "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def __init__(self, d={}):
        self._keys = d.keys()
        dict.__init__(self, d)"
    (search-backward "\"")
    (forward-char 1)
    (py-down-statement)
    (should (eq (char-after) ?d))))

(ert-deftest py-backward-minor-block-test-1-OLNs0Y ()
  (py-test-with-temp-buffer
      "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\"

    def f():
        \"\"\"
        class for in 'for in while with blah'
        \"\"\"
        if c:
             pass
        if a:"
    (py-backward-minor-block)
    (should (eq (char-after) ?i))))

(ert-deftest py-forward-block-or-clause-test-1-FCZrch ()
  (py-test-with-temp-buffer
      "def main():
    if len(sys.argv) == 1:
        usage()
        sys.exit()

    class asdf(object):
        zeit = time.strftime('%Y%m%d--%H-%M-%S')

        def Utf8_Exists(filename):
            return os.path.exists(filename.encode('utf-8'))"
    (search-backward "()")
    (end-of-line)
    (py-forward-block-or-clause)
    (should (eq (char-before) ?\)))))

(ert-deftest py-forward-statement-38-test-ghDkby ()
  (py-test-with-temp-buffer
      "def gen_segments(x, y):
    for i in range(0, len(x)-1):
        yield (x[i], x[i+1]), (y[i], y[i+1])"
    (back-to-indentation)
    (py-forward-statement)
    (should (eobp))))

(ert-deftest py-wrong-on-quotes-31-test-hVRL4O ()
  (py-test-with-temp-buffer
      "print(\"don't\")"
    (py-execute-statement)
    (should (not (nth 3 (parse-partial-sexp (point-min) (point)))))))

(ert-deftest py-support-PEP-553-built-in-breakpoint-42-test-CPvkW5 ()
  (py-test-with-temp-buffer
  "# point is at the end, now hit return
# point ends up here when it should line up under the 'b'
def foo(x):
    if x == 7:
        breakpoint()"
  (should (eq 8 (py-compute-indentation)))))

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

(ert-deftest py-in-list-indent-test-LEON2Q ()
  (py-test-with-temp-buffer
      "def foo():
print(rest)"
    (goto-char (point-max))
    (search-backward "rest")
    (py-indent-or-complete)
    (sit-for 0.1)
    (should (eq 4 (current-indentation)))))

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

(ert-deftest py-forward-indent-jcMT2k ()
  (py-test-with-temp-buffer
      "{
  \"a\": 1
  \"b\": {
    \"c\": {
      \"d\": 2
    }
  }
}"
      (goto-char (point-max))
      (search-backward "\"b")
      (should (eq (py-forward-indent) 31))))

(ert-deftest py-forward-indent-AvmF3n ()
  (py-test-with-temp-buffer-point-min
      "class kugel(object):
    zeit = time\.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time\.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = \[]
    gruen = \[0]
"
    (goto-char (point-min))
    (search-forward "zeit")
    (end-of-line)
    (py-forward-indent)
    (should (eq (char-before) ?\]))))

(ert-deftest py-forward-indent-tWBEjf ()
  (py-test-with-temp-buffer-point-min
    "def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = \[\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe\[0] = treffer
        fertig = ''
"
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (py-forward-indent)
    (should (eq (char-before) ?'))))

(ert-deftest py-TQS-tWBEjf ()
  (py-test-with-temp-buffer-point-min
      "#!/usr/bin/env python

#
# Licensed to theasdf adsf adf  under one or more
# contributor license agreements\.  See the NOTICE file distributed with

\"\"\"
Utility for creating release candidates and promoting release candidates to a final relase\.
asdf asfsd afd
\"\"\"

from __future__ import foo
"
    (goto-char (point-min))
    (py-end-of-def-or-class)
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eq (char-before) ?o))))

(ert-deftest py-backward-indent-tWBEjf ()
  (py-test-with-temp-buffer
      "except:
    print(\"Setze anzahl auf 1\")
    anzahl = 1"
    (goto-char (point-max))
    (py-backward-indent)
    (should (eq (char-after) ?p))))

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

(ert-deftest py-highlight-vars-inside-fstring-76-kGN9tr ()
  (py-test-with-temp-buffer
      "name = 'Fred'
age = 42
doo = f'He said his name is {name} and he is {age} years old'"
    (goto-char (point-max))
    (search-backward "age")
    (font-lock-fontify-buffer)
    (should (eq (face-at-point) 'font-lock-variable-name-face))))

(ert-deftest py-typing-replaces-buffer-with-scratch-buffer-77-test-Nldz2m ()
  (py-test-with-temp-buffer
  ""
  (insert "\t")
  (should (eq major-mode 'python-mode))))

(ert-deftest py-backward-def-or-class-text-IZvvZ5 ()
  (py-test-with-temp-buffer
      "  d
  "
    (goto-char (point-max))
    (should (eq nil (py-backward-def-or-class)))))

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
    
(provide 'py-interactive-tests)
;;; py-interactive-tests.el ends here
