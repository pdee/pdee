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

;; tests are expected to run from directory test

(add-to-list 'load-path default-directory)
(load "py-ert-tests-1.el" nil t)

;; (require 'python-mode-test)

;;; fast-process
(ert-deftest py-shell-complete-in-dedicated-shell ()
  (let (erg
	;; py-split-window-on-execute
	py-switch-buffers-on-execute-p)
    (with-temp-buffer
      (python-mode)
      (setq erg (python-dedicated))
      (with-current-buffer erg
	(goto-char (point-max))
	(when py-debug-p (switch-to-buffer (current-buffer)))
	(switch-to-buffer (current-buffer))
	(insert "pri")
	(sit-for 1 t)
	(call-interactively 'py-indent-or-complete)
	(sit-for 0.1 t)
	(should (or (eq 40 (char-before))
		    ;; python may just offer print(
		    (buffer-live-p (get-buffer  "*Python Completions*"))))
      (py-kill-buffer-unconditional erg)))))

(ert-deftest py-ert-fast-complete-1 ()
  (py-test-with-temp-buffer
      "pri"
    (let ((py-return-result-p t)
	  py-result py-store-result-p)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (py-fast-complete)
      (should (eq (char-before) 40)))))

(ert-deftest py-ert-execute-statement-fast-1 ()
  (py-test-with-temp-buffer-point-min
      "print(1)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-execute-statement-fast-2 ()
  (py-test-with-temp-buffer-point-min
      "print(2)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement-fast)
      (should (string= "2" py-result)))))

(ert-deftest py-ert-execute-block-fast ()
  (py-test-with-temp-buffer-point-min
      "if True:
    a = 1
    print(a)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (sit-for 0.1 t)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-execute-block-fast-2 ()
  (py-test-with-temp-buffer-point-min
      "try:
    a
except NameError:
    a=1
finally:
    a+=1
    print(a)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (should (numberp (string-to-number (car (split-string py-result))))))))

;;;
(ert-deftest py-ert-keyword-face-lp-1294742 ()
  (py-test-with-temp-buffer-point-min
      " and as assert break continue del elif else except exec finally for global if in is lambda not or pass raise return while with yield"
    (font-lock-fontify-buffer)
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'font-lock-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-exception-name-face-lp-1294742 ()
  (py-test-with-temp-buffer-point-min
      " ArithmeticError AssertionError AttributeError BaseException BufferError BytesWarning DeprecationWarning EOFError EnvironmentError Exception FloatingPointError FutureWarning GeneratorExit IOError ImportError ImportWarning IndentationError IndexError KeyError KeyboardInterrupt LookupError MemoryError NameError NoResultFound NotImplementedError OSError OverflowError PendingDeprecationWarning ReferenceError RuntimeError RuntimeWarning StandardError StopIteration SyntaxError SyntaxWarning SystemError SystemExit TabError TypeError UnboundLocalError UnicodeDecodeError UnicodeEncodeError UnicodeError UnicodeTranslateError UnicodeWarning UserWarning ValueError Warning ZeroDivisionError"
    (font-lock-fontify-buffer)
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-exception-name-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-builtins-face-lp-1294742 ()
  (let ((py-shell-name "python3"))
    (py-test-with-temp-buffer-point-min
	"_ __doc__ __import__ __name__ __package__ abs all any apply basestring bin bool buffer bytearray bytes callable chr classmethod cmp coerce compile complex delattr dict dir divmod enumerate eval execfile file filter float format frozenset getattr globals hasattr hash help hex id input int intern isinstance issubclass iter len list locals long map max min next object oct open ord pow print property range raw_input reduce reload repr reversed round set setattr slice sorted staticmethod str sum super tuple type unichr unicode vars xrange zip"
      (font-lock-fontify-buffer)
      (when py-debug-p (switch-to-buffer (current-buffer)))

      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
      (should (eq 'py-builtins-face (get-char-property (point) 'face))))))

(ert-deftest py-ert-py-pseudo-keyword-face-lp-1294742 ()
  (py-test-with-temp-buffer-point-min
      "  Ellipsis True False None  __debug__ NotImplemented"
    (font-lock-fontify-buffer)
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-pseudo-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-py-object-reference-face-lp-1294742 ()
  (py-test-with-temp-buffer-point-min
      " self cls"
    (font-lock-fontify-buffer)
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-object-reference-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-borks-all-lp-1294820 ()
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
    (font-lock-fontify-buffer)
    (search-forward "def baz(self):")
    (fill-paragraph)
    (forward-line -1)
    (should (eq (char-after) ?\n))))

(ert-deftest py-ert-respect-paragraph-1294829 ()

  (py-test-with-temp-buffer-point-min
      "# py-fill-paragraph doesn';t respect existing paragraph breaks when
# reflowing the docstring, e.g.

def foo(self)
    \"\"\"First one-line summary.

    Some other stuff which I don't want a paragraph break inserted into
    the middle of.

    And another para hjkdfgh fdjkg hfdjkg hdfjk ghdfk ghjkdf
    ghjkdf ghjdf ghjdkf k
    \"\"\"

def foo(self)
    \"\"\"Second one-line summary. Some other stuff which I don't want a
paragraph

    break inserted into the middle of. And another para hjkdfgh
fdjkg
    hfdjkg hdfjk ghdfk ghjkdf ghjkdf ghjdf ghjdkf k \"\"\"

# I feel it would be better if it didn't attempt to
# reflow the whole docstring, rather just reflow the
# particular paragraph within it which the point is
# positioned in.

# It would also be good if it could avoid mangling parameter
# descriptions like this:

def foo(self):
    \"\"\"Summary line.

    Foo bar fhgdjkfd hgjfd hgjkfd ghjkdf ghjkdf hgjdf ghjkdf
hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkg hjfdkg

    Parameters
    ----------
    endog : array-like
        1-d endogenous response variable. The dependent variable.
    exog : array-like
        A nobs x k array where `nobs` is the number of
observations and `k`
        is the number of regressors. An interecept is not
included by default
        and should be added by the user. See
        `statsmodels.tools.add_constant`.\"\"\"

def foo(self):
    \"\"\"Summary line. Foo bar fhgdjkfdhgjfd hgjkfd ghjkdf ghjkdf
hgjdf

    ghjkdf hgjdf hjgk dfhjkg dfhjkg dfhjkg fdhjkghjfdkg
Parameters
    ---------- endog : array-like 1-d endogenous response
variable. The
    dependent variable. exog : array-like A nobs x karray where
`nobs`
    is the number of observations and `k` is the number of
regressors.
    An interecept is not included by default and should be added
by the
    user. See `statsmodels.tools.add_constant`.
    \"\"\"

# Failing that though, if I can at least choose to
# reflow individual paragraphs in the docstring and
# leave others intact, I can format these things
# manually while still being able to flow other
# paragraphs using M-q.
"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (font-lock-fontify-buffer)
    (search-forward "Some other" nil t 1)
    (sit-for 0.1 t)
    (fill-paragraph)
    (forward-line -2)
    (should (not (empty-line-p)))
    (forward-line 1)
    (should (eq (char-after) ?\n))
    (search-forward "one-line summary." nil t 1)
    (when py-debug-p (message "fill-column: %s" fill-column))
    (fill-paragraph)
    (forward-line 1)
    (sit-for 0.1 t)
    (should (empty-line-p))
    (search-forward "Foo bar" nil t 1)
    (fill-paragraph)
    (forward-line 2)
    (should (eq (char-after) ?\n))))

(ert-deftest py-ert-backward-same-level-test ()
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
    (font-lock-fontify-buffer)
    (goto-char 632)
    (py-backward-same-level)
    (should (eq (char-after) ?p))
    (py-backward-same-level)
    (should (eq (char-after) ?f))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?t))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?i))
    (py-backward-same-level)
    (should (eq (char-after) ?d))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?t))
    (py-backward-same-level)
    (should (eq (char-after) ?e))
    (py-backward-same-level)
    (should (eq (char-after) ?e))))

(ert-deftest py-ert-py-up-level-test ()
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
                    else True:
                        pass
                    finally:
                        pass
        else True:
            pass
        finally:
            pass
"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (goto-char 632)
    (py-up)
    (should (eq (char-after) ?p))
    (py-up)
    (should (eq (char-after) ?f))
    (py-up)
    (should (eq (char-after) ?e))
    (py-up)
    (should (eq (char-after) ?d))
    (py-up)
    (should (eq (char-after) ?e))
    (py-up)
    (should (eq (char-after) ?e))
    (py-up)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-hide-test ()

  (py-test-with-temp-buffer-point-min "
class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')

    def pylauf(self):
        \"\"\"Eine Doku fuer pylauf\"\"\"
        ausgabe = [\" \",\" \",\" \",\" \",\" \",\" \",\" \",\" \", \" \"]

        ausgabe[0] = treffer
        if treffer in gruen:
            # print \"0, Gruen\"
            datei.write(str(spiel[i]) + \"\\n\")
"
    (font-lock-fontify-buffer)
    (search-forward "+ \"")

    (py-hide-partial-expression)
    (should (string-match "overlay from 315 to 317" (prin1-to-string (car (overlays-at (point))))))
    (py-show-partial-expression)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))
    (py-hide-expression)
    (should (string-match "overlay from 286 to 319" (prin1-to-string (car (overlays-at (point))))))
    (py-show-expression)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))

    (py-hide-clause)
    (should (string-match "overlay from 222 to 319" (prin1-to-string (car (overlays-at (point))))))
    (py-show-clause)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))

    (py-hide-block)
    (should (string-match "overlay from 222 to 319" (prin1-to-string (car (overlays-at (point))))))
    (py-show-block)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))

    (py-hide-def)
    (should (string-match "overlay from 73 to 319" (prin1-to-string (car (overlays-at (point))))))
    (py-show-def)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))

    (py-hide-class)
    (should (string-match "overlay from 2 to 319" (prin1-to-string (car (overlays-at (point))))))
    (py-show-class)
    (should (not (string-match "overlay" (prin1-to-string (car (overlays-at (point)))))))))

(ert-deftest py-ert-deletes-too-much-lp:1300270 ()
  (py-test-with-temp-buffer "
x = {'abc':'def',
         'ghi':'jkl'}
"
    ;; (when py-debug-p (switch-to-buffer (current-buffer)))
    (goto-char 24)
    (py-electric-delete)
    (should (eq 5 (current-indentation)))))

(ert-deftest py-ert-mark-expression-test ()
    "Avoid infinite loop"
  (py-test-with-temp-buffer
      "assert pycompletions('TestClass.test' , name) == \
          ['testclassmeth', 'testmeth', 'testprop', 'teststaticmeth']"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (forward-char -1)
    (py-mark-expression)
    (should (eq 119 (mark)))
    (goto-char 44)
    (py-mark-expression)
    (should (eq 46 (mark)))))

(ert-deftest py-dedicated-shell-test ()
  ""
  (let ((erg (py-shell nil t "python")))
    (should (< 8 (length erg)))
    (should (eq 0 (string-match "^*Python" erg)))))

(ert-deftest py-python-shell-test ()
  ""
  (let ((erg (python)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-python2-shell-test ()
  ""
  (let ((erg (python2)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-python3-shell-test ()
  ""
  (let ((erg (python3)))
    (should (bufferp (get-buffer erg)))
    (should (get-buffer-process erg))))

(ert-deftest py-keep-windows-configuration-test ()
  "print('py-keep-windows-configuration-test-string')"
  (delete-other-windows)
  (let ((py-keep-windows-configuration t)
	(py-split-window-on-execute t)
	(full-height (window-height)))
    (py-execute-statement)
    (should (eq (window-height) full-height))))

(ert-deftest py-compute-indentation-bob-test ()
    (py-test-with-temp-buffer-point-min
    " def foo():
    if True:
        pass
    else:
        pass
"
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (should (eq 0 (py-compute-indentation)))))

(ert-deftest py-indentation-lp-1375122-test ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
pass
"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (forward-line -1)
    (call-interactively 'py-indent-or-complete)
    (sit-for 0.1 t)
    (should (eq 8 (current-column)))
    ;; FixMe, last-command seems not set
    ;; (call-interactively 'py-indent-or-complete)
    ;; (py-indent-or-complete)
    ;; (sit-for 0.1 t)
    ;; (should (eq 4 (current-column)))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to 4)
    (py-indent-or-complete)
    (sit-for 0.1 t)
    (should (eq 0 (current-column)))
    ;; (py-indent-or-complete)
    ;; (sit-for 0.1 t)
    ;; (should (eq 8 (current-column)))
    ))

(ert-deftest py-shell-python-lp-1398530-test ()
  (when (buffer-live-p (get-buffer "*Python*"))(py-kill-buffer-unconditional "*Python*"))
  (py-test-with-temp-buffer
      ""
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python*"))))))

(ert-deftest py-shell-python3-lp-1398530-test ()
  (when (buffer-live-p (get-buffer "*Python3*"))(py-kill-buffer-unconditional "*Python3*"))
  (py-test-with-temp-buffer
      ""
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python3"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python3*"))))))

(ert-deftest py-shell-python2-lp-1398530-test ()
  (when (buffer-live-p (get-buffer "*Python2*"))(py-kill-buffer-unconditional "*Python2*"))
  (py-test-with-temp-buffer
      ""
    (when py-debug-p (switch-to-buffer (current-buffer)))
    (let ((py-shell-name "python2"))
      (py-shell)
      (sit-for 0.1 t)
      (should (buffer-live-p (get-buffer "*Python2*"))))))

(ert-deftest py-beginning-of-statement-test-1 ()
  (py-test-with-temp-buffer
      (let ((py-return-result-p t)
	    py-result py-store-result-p)
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
")
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (py-beginning-of-statement)
    (should (eq (char-after) ?a))
    (py-beginning-of-statement)
    (should (eq (char-after) ?\"))
    (py-beginning-of-statement)
    (should (eq (char-after) ?d))
    (py-beginning-of-statement)
    (should (eq (char-after) ?o))
    (py-beginning-of-statement)
    (should (eq (char-after) ?i))
    (py-beginning-of-statement)
    (should (eq (char-after) ?c))
    (py-beginning-of-statement)
    (should (eq (char-after) ?c))
    (py-beginning-of-statement)
    (should (eq (char-after) ?p))
    (py-beginning-of-statement)
    (should (bobp))))


(ert-deftest py-ert-beginning-of-except-block-test ()
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
    (when py-debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (py-beginning-of-except-block)
    (should (eq (char-after) ?e))))

(ert-deftest py-ert-beginning-of-except-block-bol-test ()
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
    (when py-debug-p (switch-to-buffer (current-buffer))
          (font-lock-fontify-buffer))
    (py-beginning-of-except-block-bol)
    (should (eq (char-after) ?\ ))))

  ;; (and (bufferp (get-buffer "*Python*"))(buffer-live-p (get-buffer "*Python*"))(py-kill-buffer-unconditional "*Python*"))
  ;; (and (bufferp (get-buffer "*IPython*"))(buffer-live-p (get-buffer "*IPython*"))(py-kill-buffer-unconditional "*IPython*")))

(defun nested-dictionaries-indent-lp:328791-test (&optional arg)
  "With ARG greater 1 keep test buffer open.

If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (interactive "p")
  (let ((teststring "

# hanging
asdf = {
    'a':{
         'b':3,
         'c':4
        }
    }

# closing
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

"))
    (py-bug-tests-intern 'nested-dictionaries-indent-lp:328791-base arg teststring)))

(ert-deftest py-ert-nested-dictionaries-indent-lp:328791-test ()
  (py-test-with-temp-buffer-point-min
      "

# hanging
asdf = {
    'a':{
         'b':3,
         'c':4
        }
    }

# closing
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
    (let ((py-indent-honors-multiline-listing t)
	  py-closing-list-dedents-bos)
      (search-forward "'a':{")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 8 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 4 (py-compute-indentation)))
      ;; py-closing-list-dedents-bos
      (setq py-closing-list-dedents-bos t)
      (search-forward "'a':{")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 0 (py-compute-indentation)))
      (search-forward "}" nil nil 2)
      (should (eq 12 (py-compute-indentation)))
      (search-forward "]")
      (should (eq 8 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 4 (py-compute-indentation)))
      (search-forward "}")
      (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-flexible-indentation-lp-328842-test ()
  (py-test-with-temp-buffer-point-min
      "\(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list

packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)

\( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list
"
    (let ((py-indent-honors-multiline-listing t))
      (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
      (search-forward "(long")
      (forward-char -1)
      ;; (goto-char 6)
      (should (eq nil (get-char-property (point) 'face)))
      (goto-char 33)
      (assert (eq 1 (py-compute-indentation)) nil "flexible-indentation-lp-328842-test failed")
      (goto-char 115)
      (assert (eq 16 (py-compute-indentation)) nil "flexible-indentation-lp-328842-test failed")
      (goto-char 202)
      (assert (eq 2 (py-compute-indentation)) nil "flexible-indentation-lp-328842-test failed"))))

(ert-deftest py-ert-indent-in-arglist-test ()
  (py-test-with-temp-buffer
      "def foo (a,

):"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (beginning-of-line)
    (should (eq 9 (py-compute-indentation)))))

(ert-deftest py-ert-variablen-tests ()
  (py-test-with-temp-buffer ""
    (sit-for 0.1 t)
    (should (boundp 'virtualenv-workon-home))
    (should (boundp 'virtualenv-name))
    (should (boundp 'virtualenv-old-path))
    (should (boundp 'virtualenv-old-exec-path))
    (should (boundp 'virtualenv-name))
    (should (boundp 'py-last-exeption-buffer))
    (should (boundp 'py-keywords))
    (should (boundp 'py-pdbtrack-is-tracking-p))
    (should (boundp 'py-underscore-word-syntax-p))
    (should (boundp 'auto-fill-mode-orig))
    (should (boundp 'fill-column-orig))
    (should (boundp 'py-match-paren-mode))
    (should (boundp 'py-match-paren-key))
    (should (boundp 'py-kill-empty-line))
    (should (boundp 'python-mode-map))
    (should (boundp 'py-python-shell-mode-map))
    (should (boundp 'py-ipython-shell-mode-map))
    (should (boundp 'py-shell-map))
    (should (boundp 'py--timer))
    (should (boundp 'py--timer-delay))
    (should (boundp 'py-eldoc-string-code))
    (should (boundp 'py-install-directory))
    (should (boundp 'python-mode-modeline-display))
    (should (boundp 'py-extensions))
    (should (boundp 'info-lookup-mode))
    (should (boundp 'py-fast-process-p))
    (should (boundp 'py-shell-unfontify-p))
    (should (boundp 'py-session-p))
    (should (boundp 'py-max-help-buffer-p))
    (should (boundp 'py-highlight-error-source-p))
    (should (boundp 'py-set-pager-cat-p))
    (should (boundp 'py-empty-line-closes-p))
    (should (boundp 'py-prompt-on-changed-p))
    (should (boundp 'py-dedicated-process-p))
    (should (boundp 'py-store-result-p))
    (should (boundp 'py-return-result-p))
    (should (boundp 'py-new-session-p))
    (should (boundp 'py-hide-show-minor-mode-p))
    (should (boundp 'py-load-skeletons-p))
    (should (boundp 'py-if-name-main-permission-p))
    (should (boundp 'py-use-font-lock-doc-face-p))
    (should (boundp 'empty-comment-line-separates-paragraph-p))
    (should (boundp 'py-indent-honors-inline-comment))
    (should (boundp 'py-auto-fill-mode))
    (should (boundp 'py-error-markup-delay))
    (should (boundp 'py-fast-completion-delay))
    (should (boundp 'py-new-shell-delay))
    (should (boundp 'py-autofill-timer-delay))
    (should (boundp 'py-docstring-fill-column))
    (should (boundp 'py-comment-fill-column))
    (should (boundp 'py-fontify-shell-buffer-p))
    (should (boundp 'py-modeline-display-full-path-p))
    (should (boundp 'py-modeline-acronym-display-home-p))
    (should (boundp 'smart-operator-mode))
    (should (boundp 'highlight-indent-active))
    (should (boundp 'autopair-mode))
    (should (boundp 'py-result))
    (should (boundp 'py-error))
    (should (boundp 'py-python-completions))
    (should (boundp 'py-ipython-completions))
    (should (boundp 'py-timer-close-completions-p))
    (should (boundp 'py-smart-operator-mode-p))
    (should (boundp 'py-autopair-mode))
    (should (boundp 'py-no-completion-calls-dabbrev-expand-p))
    (should (boundp 'py-indent-no-completion-p))
    (should (boundp 'py-company-pycomplete-p))
    (should (boundp 'py-auto-completion-mode-p))
    (should (boundp 'py-complete-last-modified))
    (should (boundp 'py--auto-complete-timer))
    (should (boundp 'py-auto-completion-buffer))
    (should (boundp 'py--auto-complete-timer))
    (should (boundp 'py--auto-complete-timer-delay))
    (should (boundp 'py-auto-complete-p))
    (should (boundp 'py-tab-shifts-region-p))
    (should (boundp 'py-tab-indents-region-p))
    (should (boundp 'py-block-comment-prefix-p))
    (should (boundp 'py-org-cycle-p))
    (should (boundp 'py-set-complete-keymap-p))
    (should (boundp 'py-outline-minor-mode-p))
    (should (boundp 'py-guess-py-install-directory-p))
    (should (boundp 'py-load-pymacs-p))
    (should (boundp 'py-verbose-p))
    (should (boundp 'py-sexp-function))
    (should (boundp 'py-close-provides-newline))
    (should (boundp 'py-dedent-keep-relative-column))
    (should (boundp 'py-indent-honors-multiline-listing))
    (should (boundp 'py-indent-paren-spanned-multilines-p))
    (should (boundp 'py-closing-list-dedents-bos))
    (should (boundp 'py-closing-list-space))
    (should (boundp 'py-max-specpdl-size))
    (should (boundp 'py-closing-list-keeps-space))
    (should (boundp 'py-electric-kill-backward-p))
    (should (boundp 'py-electric-colon-active-p))
    (should (boundp 'py-electric-colon-bobl-only))
    (should (boundp 'py-electric-yank-active-p))
    (should (boundp 'py-electric-colon-greedy-p))
    (should (boundp 'py-electric-colon-newline-and-indent-p))
    (should (boundp 'py-electric-comment-p))
    (should (boundp 'py-electric-comment-add-space-p))
    (should (boundp 'py-mark-decorators))
    (should (boundp 'py-defun-use-top-level-p))
    (should (boundp 'py-tab-indent))
    (should (boundp 'py-return-key))
    (should (boundp 'py-complete-function))
    (should (boundp 'ipython-complete-function))
    (should (boundp 'py-encoding-string))
    (should (boundp 'py-shebang-startstring))
    (should (boundp 'py-flake8-command))
    (should (boundp 'py-flake8-command-args))
    (should (boundp 'py-flake8-history))
    (should (boundp 'py-message-executing-temporary-file))
    (should (boundp 'py-execute-no-temp-p))
    (should (boundp 'py-lhs-inbound-indent))
    (should (boundp 'py-continuation-offset))
    (should (boundp 'py-indent-tabs-mode))
    (should (boundp 'py-smart-indentation))
    (should (boundp 'py-block-comment-prefix))
    (should (boundp 'py-indent-offset))
    (should (boundp 'py-backslashed-lines-indent-offset))
    (should (boundp 'pdb-path))
    (should (boundp 'py-indent-comments))
    (should (boundp 'py-uncomment-indents-p))
    (should (boundp 'py-separator-char))
    (should (boundp 'py-custom-temp-directory))
    (should (boundp 'py-beep-if-tab-change))
    (should (boundp 'py-jump-on-exception))
    (should (boundp 'py-ask-about-save))
    (should (boundp 'py-delete-function))
    (should (boundp 'py-pdbtrack-do-tracking-p))
    (should (boundp 'py-pdbtrack-filename-mapping))
    (should (boundp 'py-pdbtrack-minor-mode-string))
    (should (boundp 'py-import-check-point-max))
    (should (boundp 'py-jython-packages))
    (should (boundp 'py-current-defun-show))
    (should (boundp 'py-current-defun-delay))
    (should (boundp 'py-python-send-delay))
    (should (boundp 'py-ipython-send-delay))
    (should (boundp 'py-master-file))
    (should (boundp 'py-pychecker-command))
    (should (boundp 'py-pychecker-command-args))
    (should (boundp 'py-pyflakes-command))
    (should (boundp 'py-pyflakes-command-args))
    (should (boundp 'py-pep8-command))
    (should (boundp 'py-pep8-command-args))
    (should (boundp 'py-pyflakespep8-command))
    (should (boundp 'py-pyflakespep8-command-args))
    (should (boundp 'py-pylint-command))
    (should (boundp 'py-pylint-command-args))
    (should (boundp 'py-shell-input-prompt-1-regexp))
    (should (boundp 'py-shell-input-prompt-2-regexp))
    (should (boundp 'py-shell-prompt-read-only))
    (should (boundp 'py-honor-IPYTHONDIR-p))
    (should (boundp 'py-ipython-history))
    (should (boundp 'py-honor-PYTHONHISTORY-p))
    (should (boundp 'py-python-history))
    (should (boundp 'py-switch-buffers-on-execute-p))
    (should (boundp 'py-split-window-on-execute))
    (should (boundp 'py-split-windows-on-execute-function))
    (should (boundp 'py-hide-show-keywords))
    (should (boundp 'py-hide-show-hide-docstrings))
    (should (boundp 'py-hide-comments-when-hiding-all))
    (should (boundp 'py-outline-mode-keywords))
    (should (boundp 'python-mode-hook))
    (should (boundp 'py-shell-name))
    (should (boundp 'py-default-interpreter))
    (should (boundp 'py-python-command))
    (should (boundp 'py-python-command-args))
    (should (boundp 'py-python2-command))
    (should (boundp 'py-python2-command-args))
    (should (boundp 'py-python3-command))
    (should (boundp 'py-python3-command-args))
    (should (boundp 'py-ipython-command))
    (should (boundp 'py-ipython-command-args))
    (should (boundp 'py-jython-command))
    (should (boundp 'py-jython-command-args))
    (should (boundp 'py-bpython-command))
    (should (boundp 'py-bpython-command-args))
    (should (boundp 'py-shell-toggle-1))
    (should (boundp 'py-shell-toggle-2))
    (should (boundp 'py--imenu-create-index-p))
    (should (boundp 'py-history-filter-regexp))
    (should (boundp 'py-match-paren-mode))
    (should (boundp 'py-match-paren-key))
    (should (boundp 'py-kill-empty-line))
    (should (boundp 'py-imenu-show-method-args-p))
    (should (boundp 'py-use-local-default))
    (should (boundp 'py-edit-only-p))
    (should (boundp 'py-force-py-shell-name-p))
    (should (boundp 'python-mode-v5-behavior-p))
    (should (boundp 'py-trailing-whitespace-smart-delete-p))
    (should (boundp 'py-newline-delete-trailing-whitespace-p))
    (should (boundp 'py--warn-tmp-files-left-p))
    (should (boundp 'py-complete-ac-sources))
    (should (boundp 'py-remove-cwd-from-path))
    (should (boundp 'py-ignore-result-p))
    (should (boundp 'py-shell-local-path))
    (should (boundp 'py-ipython-execute-delay))
    (should (boundp 'py-shell-completion-setup-code))
    (should (boundp 'py-shell-module-completion-code))
    (should (boundp 'py-ipython-module-completion-code))
    (should (boundp 'py-ipython-module-completion-string))
    (should (boundp 'py--imenu-create-index-function))
    (should (boundp 'python-source-modes))
    (should (boundp 'py-input-filter-re))
    (should (boundp 'strip-chars-before))
    (should (boundp 'strip-chars-after))
    (should (boundp 'py-docstring-style))
    (should (boundp 'py-execute-directory))
    (should (boundp 'py-use-current-dir-when-execute-p))
    (should (boundp 'py-keep-shell-dir-when-execute-p))
    (should (boundp 'py-fileless-buffer-use-default-directory-p))
    (should (boundp 'py-check-command))
    (should (boundp 'py-this-abbrevs-changed))
    (should (boundp 'py-ffap-p))
    (should (boundp 'py-ffap))
    (should (boundp 'ffap-alist))
    (should (boundp 'py-buffer-name))
    (should (boundp 'py-orig-buffer-or-file))
    (should (boundp 'py-ffap-p))
    (should (boundp 'py-keep-windows-configuration))
    (should (boundp 'py-output-buffer))
    (should (boundp 'py-ffap-string-code))
    (should (boundp 'py-shell-prompt-regexp))
    (should (boundp 'py-ffap-setup-code))
    (should (boundp 'py-eldoc-setup-code))
    (should (boundp 'py-shell-prompt-output-regexp))
    (should (boundp 'py-underscore-word-syntax-p))
    (should (boundp 'py-autofill-timer))
    (should (boundp 'py-fill-column-orig))
    (should (boundp 'python-mode-message-string))
    (should (boundp 'python-mode-syntax-table))
    (should (boundp 'py-local-command))
    (should (boundp 'py-local-versioned-command))
    (should (boundp 'ipython-completion-command-string))
    (should (boundp 'ipython0.10-completion-command-string))
    (should (boundp 'ipython0.11-completion-command-string))
    (should (boundp 'py-encoding-string-re))
    (should (boundp 'py-shebang-regexp))
    (should (boundp 'py-separator-char))
    (should (boundp 'py-temp-directory))
    (should (boundp 'py-pdbtrack-input-prompt))
    (should (boundp 'py-pydbtrack-input-prompt))
    (should (boundp 'ipython-de-input-prompt-regexp))
    (should (boundp 'py-exec-command))
    (should (boundp 'py-which-bufname))
    (should (boundp 'py-pychecker-history))
    (should (boundp 'py-pyflakes-history))
    (should (boundp 'py-pep8-history))
    (should (boundp 'py-pyflakespep8-history))
    (should (boundp 'py-pylint-history))
    (should (boundp 'ipython-de-input-prompt-regexp))
    (should (boundp 'ipython-de-output-prompt-regexp))
    (should (boundp 'py-mode-output-map))
    (should (boundp 'hs-hide-comments-when-hiding-all))
    (should (boundp 'py-force-local-shell-p))
    (should (boundp 'py-shell-complete-debug))
    (should (boundp 'py-debug-p))
    (should (boundp 'py-completion-last-window-configuration))
    (should (boundp 'py-exception-buffer))
    (should (boundp 'py-string-delim-re))
    (should (boundp 'py-labelled-re))
    (should (boundp 'py-expression-skip-regexp))
    (should (boundp 'py-expression-skip-chars))
    (should (boundp 'py-expression-re))
    (should (boundp 'py-not-expression-regexp))
    (should (boundp 'py-not-expression-chars))
    (should (boundp 'py-partial-expression-backward-chars))
    (should (boundp 'py-partial-expression-forward-chars))
    (should (boundp 'py-operator-regexp))
    (should (boundp 'py-assignment-regexp))
    (should (boundp 'py-delimiter-regexp))
    (should (boundp 'py-line-number-offset))
    (should (boundp 'match-paren-no-use-syntax-pps))
    (should (boundp 'py-traceback-line-re))
    (should (boundp 'py-bol-forms-last-indent))
    (should (boundp 'py-XXX-tag-face))
    (should (boundp 'py-pseudo-keyword-face))
    (should (boundp 'py-variable-name-face))
    (should (boundp 'py-number-face))
    (should (boundp 'py-decorators-face))
    (should (boundp 'py-object-reference-face))
    (should (boundp 'py-builtins-face))
    (should (boundp 'py-class-name-face))
    (should (boundp 'py-exception-name-face))
    (should (boundp 'py-import-from-face))
    (should (boundp 'py-def-class-face))
    (should (boundp 'py-try-if-face))
    (should (boundp 'py-file-queue))
    (should (boundp 'jython-mode-hook))
    (should (boundp 'py-shell-hook))
    (should (boundp 'python-font-lock-keywords))
    (should (boundp 'py-dotted-expression-syntax-table))
    (should (boundp 'python-default-template))
    (should (boundp 'py-already-guessed-indent-offset))
    (should (boundp 'py-shell-template))
    (should (boundp 'py-fast-filter-re))
    (should (boundp 'py-block-closing-keywords-re))
    (should (boundp 'py-block-or-clause-re))
    (should (boundp 'py-compilation-regexp-alist))
    (should (boundp 'py-windows-config))
    (should (boundp 'symbol-definition-start-re))
    (should (boundp 'python-font-lock-keywords))
    (should (boundp 'py-imenu-class-regexp))
    (should (boundp 'py-imenu-method-regexp))
    (should (boundp 'py-imenu-method-no-arg-parens))
    (should (boundp 'py-imenu-method-arg-parens))
    (should (boundp 'py-imenu-generic-expression))
    (should (boundp 'py-imenu-generic-regexp))
    (should (boundp 'py-imenu-generic-parens))
    ))
