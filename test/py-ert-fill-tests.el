;; py-ert-fill-tests.el --- testing python-mode.el -*- lexical-binding: t; -*-

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
      (should (eq 4 (current-indentation)))
      (search-forward "\"\"\"")
      (should (eq 4 (current-indentation)))
 )))

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

(ert-deftest fill-paragraph-causes-wrong-indent-lp-1397936-test-DqgSN7 ()
  (py-test-with-temp-buffer
      "def foo():
    \"\"\"abc\"\"\"
"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (jit-lock-fontify-now))
    (goto-char 20)
    (call-interactively 'fill-paragraph)
    (should (eq 4 (current-indentation)))))

(provide 'py-ert-fill-tests)
;;; py-ert-fill-tests.el ends here
