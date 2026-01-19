;;; py-ert-extra-tests.el --- extra tests                -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; (setq py-verbose-p t)

;; (require 'org)
;; (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      '((emacs-lisp . t)
;;        (python . t)
;;        ))

(require 'py-setup-ert-tests)

(ert-deftest py-ert-moves-up-execute-statement-test-RdqUKX ()
  ""
  (py-test-point-min
   "print(\"I'm the py-execute-statement-test\")"
   'python-mode
   'py-verbose-p
   (if (executable-find "python2")
       (progn
         (goto-char (point-min))
         (let ((py-shell-name "python2"))
           (py-execute-statement)
           (sit-for 0.1)
           (set-buffer (get-buffer "*Python2*"))
           (goto-char (point-max))
           (and (should (search-backward "py-execute-statement-test" nil t 1))
	        (py-kill-buffer-unconditional (current-buffer)))))
     (when py-debug-p (message "py-ert-moves-up-execute-statement-test-RdqUKX: %s" "Can't see python2")))))

(ert-deftest py-ert-moves-up-execute-statement-test-itkuh0 ()
  ""
  (py-test-point-min
      "print(\"I'm the py-execute-statement-test\")"
      'python-mode
      'py-verbose-p
      (when (executable-find "python3")
        (goto-char (point-min))
        (let ((py-shell-name "python3"))
          (py-execute-statement)
          (sit-for 0.1)
          (set-buffer (get-buffer "*Python3*"))
          (goto-char (point-max))
          (should (search-backward "py-execute-statement-test" nil t 1))
          (py-kill-buffer-unconditional (current-buffer))))))

(ert-deftest UnicodeEncodeError-lp-550661-test-1oxvP0 ()
  ""
  (py-test
      "print(u'\\xA9')"
    'python-mode
    'py-verbose-p
    (let ((py-return-result-p t)
	  (py-store-result-p t)
          (py-verbose-p t))
      (goto-char (point-max))
      (py-execute-buffer)
      ;; (setq erg (car (read-from-string py-result)))
      ;; (message "UnicodeEncodeError-lp-550661-test-1 erg: %s" erg)
      (sit-for 0.1)
      (when py-verbose-p (message "py-result: %s" py-result))
      (should (string= "©" py-result))
      (py-kill-buffer-unconditional (get-buffer  "*Python3*")))))

(ert-deftest py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2 ()
  ""
  (py-test
      "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
os.write"
    'python-mode
    'py-verbose-p
    (if (executable-find "python")
        (progn
          (goto-char (point-max))
          (forward-char -1)
          (py-help-at-point)
          (sit-for 0.1)
          (set-buffer py-output-buffer)
          (goto-char (point-max))
          (when py-debug-p (switch-to-buffer (current-buffer)))
          (when py-debug-p (message "%s" (current-buffer)))
          (goto-char comint-last-output-start)
          (sit-for 0.1)
          (should (string-match "write" (buffer-substring-no-properties (point) (point-max)))))
      (when py-verbose-p (message "py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2: %s" "No executable python found")))))

(ert-deftest py-describe-symbol-fails-on-modules-lp-919719-test-MppJiJ ()
  ""
  (py-test
      "# -*- coding: utf-8 -*-
import os
os.write"
    'python-mode
    'py-verbose-p
    (if (executable-find "python3")
        (progn
          (goto-char (point-max))
          (forward-char -1)
          (py-help-at-point)
          ;; (sit-for 0.1)
          (set-buffer py-output-buffer)
          (switch-to-buffer (current-buffer))
          (goto-char (point-max))
          (when py-debug-p (switch-to-buffer (current-buffer)))
          (when py-debug-p (message "%s" (current-buffer)))
          (goto-char (point-min))
          (should (string-match "write" (buffer-substring-no-properties (point) (point-max)))))
      (when py-verbose-p (message "py-describe-symbol-fails-on-modules-lp-919719-test-9UErj2: %s" "No executable python found")))))

(ert-deftest py-ert-execute-block-fast-9Ui5ja-zo3sa5 ()
  ""
  (py-test-point-min
   "try:
    a
except:
    NameError
    a=1
finally:
    a+=1
    print(a)"
   'python-mode
   'py-verbose-p
   (let ((py-fast-process-p t)
	 (py-return-result-p t)
	 ;; (py-debug-p t)
	 py-result py-split-window-on-execute)
     (py-execute-block)
     (sit-for 0.3)
     (when py-debug-p (message "py-ert-execute-block-fast-9Ui5ja, py-result: %s" py-result))
     (should (string-match "[0-9]" py-result))
     (py-kill-buffer-unconditional (get-buffer  "*Python3 Fast*")))))

(ert-deftest  py-ert-execute-block-9Ui5ja-DUvXA6 ()
  ""
  (py-test-point-min
   "try:
    a
except:
    NameError
    a=1
finally:
    a+=1
    print(a)"
   'python-mode
   'py-verbose-p
   (setq py-result "")
   (let ((py-fast-process-p nil)
	 (py-return-result-p t)
	 ;; (py-debug-p t)
	 py-split-window-on-execute)
     (py-execute-block)
     (sit-for 0.1)
     (when py-debug-p (message "py-ert-execute-block-fast-9Ui5ja, py-result: %s" py-result))
     (should (string-match "[0-9]+" py-result))
     (py-kill-buffer-unconditional (get-buffer  "*Python3 Fast*")))))

(ert-deftest py-ert-moves-up-execute-statement-python3-dedicated-test-zI51W7 ()
  ""
  (py-test-point-min
   "print(\"I'm the py-execute-statement-python3-dedicated-test\")"
   'python-mode
   'py-verbose-p
   (let (;; (py-debug-p t)
	 py-store-result-p
	 erg)
     (call-interactively 'py-execute-statement-python3-dedicated)
     ;; (sit-for 0.1 t)
     (set-buffer py-output-buffer)
     (when (called-interactively-p 'interactive) (switch-to-buffer py-output-buffer))
     ;; (switch-to-buffer (current-buffer))
     (goto-char (point-min))
     (should (search-forward "py-execute-statement-python3-dedicated-test" nil t 1))
     (py-kill-buffer-unconditional (current-buffer))

     )))

(ert-deftest py-ert-execute-statement-fast-7XrRee ()
  ""
  (py-test-point-min
   "print(2)"
   'python-mode
   'py-verbose-p
   (let ((py-fast-process-p t)
	 (py-return-result-p t)
	 py-result py-store-result-p)
     (py-execute-statement-fast)
     (sit-for 0.1)
     (should (string= "2" py-result))
     (py-kill-buffer-unconditional (get-buffer  "*Python3 Fast*")))))

;; adapted from python.el
(ert-deftest py-syntax-after-backspace-TwyMwn-xjlPqf ()
  ""
  (py-test
   "\"\""
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (should (string= (buffer-string) "\"\""))
   (should (null (nth 3 (parse-partial-sexp (point-min) (point)))))
   ;; (py-kill-buffer-unconditional (get-buffer  "*Python3*"))
   ))

(ert-deftest py-ert-execute-statement-fast-test-noYr4j ()
  ""
  (py-test-point-min
   "print(123234)"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (let ((py-split-window-on-execute py-switch-buffers-on-execute-p)
         (shell (py-choose-shell)))
     (py-execute-statement-fast)
     (save-excursion
       (set-buffer (concat "*" (capitalize shell) " Fast*"))
       (goto-char (point-max))
       ;; (sit-for 0.1)
       (when py-verbose-p (message "py-ert-execute-statement-fast-test: current-buffer: %s" (current-buffer)))
       (should (search-backward "123234"))
       (py-kill-buffer-unconditional (get-buffer  "*Python3 Fast*"))))))

(ert-deftest py-ert-fast-complete-vS8fnm ()
  ""
  (py-test
   "obj"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-fast-complete)
   ;; (sit-for 0.1)
   (goto-char (point-max))
   (when py-debug-p (message "py-ert-fast-complete-1, current-buffer: %s" (current-buffer)))
   (should (search-backward "ect"))
   (py-kill-buffer-unconditional (get-buffer  "*Python3 Fast*"))
   ))

(ert-deftest py-execute-string-text-dtOWbA1 ()
  ""
  (py-test
   ""
   'python-mode
   'py-verbose-p
   (let ((py-store-result-p t))
     (py-execute-string "print(\"foo\")" nil t)
     (sit-for 0.1)
     (should (string= py-result "foo"))
     (py-kill-buffer-unconditional (get-buffer  "*Python3*")))))

(ert-deftest py-ert-class-definitions-lp-1018164-test-3pDuRq ()
  ""
  (py-test-point-min
   "class EmacsFrameThemes(object, metaclass=cldef.metaClass):
    __cldef = cldef.metaClass.wrapclass()
    __cldef.slots(\"__themes\")
    __cldef.field('cacheFile', 'ri', sethook=cacheFilePath)
    __cldef.field(\"saveEnabled\", 'rw', initval=True, sethook=bool)
    __cldef.field(\"current\", 'rw',
                  fset=lambda obj,theme: obj.setCurrentTheme(theme))
    ###########################################################################
    def __new__(cls, themeMgr=None):
        if themeMgr is None:
            # pickle internals called __new__ SURPRISE!
            return object.__new__(cls)
        if themeMgr.cacheFile.isfile():
            # load themes from cache file
            return cls.loadCache_i(themeMgr.cacheFile)
        themes = object.__new__(cls)
        elispFiles = themeMgr.archive.files('\*.el')
        if elispFiles:
            # load themes from a collection (archive) of elisp files created
            # this module and emacs can use to initialize themes
            themes.loadArchive_i(elispFiles, themeMgr.cacheFile)
        else:
            # create themes using a few 'canned' themes
            themes.bootstrap_i(themeMgr.cacheFile)
        return themes
    ###########################################################################
"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (py-forward-def-or-class)
   (should (looking-back "return themes" (line-beginning-position)))
   (py-kill-buffer-unconditional (get-buffer "*Python3*"))))

(ert-deftest py-execute-region-ipython-test-1gyFLs ()
  ""
  (py-test
   "print(u'\\xA9')"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (push-mark)
   (beginning-of-line)
   (when (executable-find "ipython")
       ;; (message "py-execute-region-ipython-test-1gyFLs: %s" "No executable found")
     (py-execute-region-ipython (region-beginning) (region-end))
     (set-buffer "*IPython*")
     (string-match "@" (buffer-substring-no-properties (point-min) (point-max)))
     (py-kill-buffer-unconditional (get-buffer "*IPython*")))))

(ert-deftest py-ert-respect-paragraph-1294829-test-dpmi5s ()
  ""
  (py-test-point-min
      "# py-fill-paragraph doesn\';t respect existing paragraph breaks when
# reflowing the docstring, e.g.

def foo(self)
    \"\"\"First one-line summary.

    Some other stuff which I don\'t want a paragraph break inserted into
    the middle of.

    And another para hjkdfgh fdjkg hfdjkg hdfjk ghdfk ghjkdf
    ghjkdf ghjdf ghjdkf k
    \"\"\"

def foo(self)
    \"\"\"Second one-line summary. Some other stuff which I don\'t want a
paragraph

    break inserted into the middle of. And another para hjkdfgh
fdjkg
    hfdjkg hdfjk ghdfk ghjkdf ghjkdf ghjdf ghjdkf k \"\"\"

# I feel it would be better if it didn\'t attempt to
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
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    ;; (font-lock-fontify-region (point-min)(point-max))
    (search-forward "Some other" nil t 1)
    (sit-for 0.1 t)
    (fill-paragraph)
    (forward-line -2)
    (should (not (py-empty-line-p)))
    (forward-line 1)
    (should (eq (char-after) ?\n))
    (py-kill-buffer-unconditional (get-buffer "*Python3*"))
    ))

(ert-deftest py-ert-respect-paragraph-1294829-test-s7lFth ()
  ""
  (py-test-point-min
      "# py-fill-paragraph doesn\';t respect existing paragraph breaks when
# reflowing the docstring, e.g.

def foo(self)
    \"\"\"First one-line summary.

    Some other stuff which I don\'t want a paragraph break inserted into
    the middle of.

    And another para hjkdfgh fdjkg hfdjkg hdfjk ghdfk ghjkdf
    ghjkdf ghjdf ghjdkf k
    \"\"\"

def foo(self)
    \"\"\"Second one-line summary. Some other stuff which I don\'t want a
paragraph

    break inserted into the middle of. And another para hjkdfgh
fdjkg
    hfdjkg hdfjk ghdfk ghjkdf ghjkdf ghjdf ghjdkf k \"\"\"

# I feel it would be better if it didn\'t attempt to
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
    'python-mode
    py-debug-p
    (goto-char (point-min))
    (search-forward "one-line summary." nil t 1)
    (when py-debug-p (message "fill-column: %s" fill-column))
    (fill-paragraph)
    (forward-line 1)
    (sit-for 0.1 t)
    (should (py-empty-line-p))
    (search-forward "Foo bar" nil t 1)
    (fill-paragraph)
    (forward-line 2)
    (should (eq (char-after) ?\n))
    (py-kill-buffer-unconditional (get-buffer "*Python3*"))
    ))

(ert-deftest py-indent-bug63959-test-6ZlhPF ()
  ""
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
   (search-backward "First")
   (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-bug63959-test-Bfr7rA ()
  ""
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
   (search-backward "Return")
   (should (eq 4 (py-compute-indentation)))))

(ert-deftest py-indent-or-complete-7NWa5T ()
  ""
  (py-test
   "def foo:
    pass\n\npri"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (py-indent-or-complete)
   (should (looking-back "print.?" (line-beginning-position)))))

(ert-deftest py-ert-moves-up-fill-paragraph-pep-257-nn-BBJoDt ()
  ""
  (let ((py-docstring-style 'pep-257-nn))
    (py-test-point-min
     "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. Not unhappy.

    Now this is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very glad.
    \"\"\"
    return 7
"
     'python-mode
     'py-verbose-p
     (search-forward "\"\"\"")
     (fill-paragraph)
     (goto-char (point-min))
     (search-forward "unhappy")
     (forward-line 1)
     (should (py-empty-line-p))
     (search-forward "Now")
     (should (eq 4 (current-indentation)))
     (search-forward "glad")
     (forward-line 1)
     ;; (sit-for 1)
     (back-to-indentation)
     ;; (sit-for 1)
     ;; (should (eq (char-after) 34))
     )))

(ert-deftest py-ert-moves-up-fill-paragraph-django-BVA4Jt ()
  ""
  (let ((py-docstring-style 'django))
    (py-test-point-min
     "# r1416
def baz():
    \"\"\"Hello there. This is a multiline function definition. Don't wor ry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    This is a multiline function definition. Don't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
     'python-mode
     'py-verbose-p
     ;; (font-lock-ensure)
     (goto-char 49)
     (fill-paragraph)
     (search-forward "multiline" nil t 3)
     (forward-line 2)
     (should (py-empty-line-p)))))

;; (ert-deftest py-fast-send-string-no-output-VxbcvH ()
;;   ""
;;   (py-test
;;    "print(234)"
;;    'python-mode
;;    'py-verbose-p
;;    (py--fast-send-string-no-output (buffer-substring-no-properties (point-min) (point-max)))
;;    (set-buffer "*Python3 Fast*")
;;    (when py-debug-p (switch-to-buffer (current-buffer)))
;;    (when py-debug-p (switch-to-buffer "*Python3 Fast*"))
;;    (goto-char (point-max))
;;    (should (search-backward "234"))))

(ert-deftest py-send-string-no-output-VxbcvH ()
  ""
  (py-test
   "print(234)"
   'python-mode
   'py-verbose-p
   (py-send-string-no-output (buffer-substring-no-properties (point-min) (point-max)))
   (set-buffer "*Python3*")
   (goto-char (point-max))
   (when py-debug-p (switch-to-buffer "*Python3*"))
   (should-not (looking-back "123" (line-beginning-position)))))

(ert-deftest py-pdbtrack-test-H6CpKY ()
  ""
  (py-test
   "import pdb
import sys
import os
pdb.set_trace()
args = sys.argv
def usage():
    print(\"\"\"Fehler: %s
Es muß die aufzurufende Ziehungszahl als Argument angegeben werden:
'python roulette.py 1, 'python roulette.py 2', ... 'python roulette.py n'.
\"\"\" % (
          os.path.basename(sys.argv\[0])))
def main():
    if len(sys.argv) == 1:
        usage()
        # sys.exit()
"
   'python-mode
   'py-verbose-p
   (save-excursion
     (let ((inhibit-field-text-motion t)
	   py-split-window-on-execute
	   py-switch-buffers-on-execute-p)
       (py-execute-buffer)
       (should (buffer-live-p (get-buffer "*Python3*")))
       (set-buffer (get-buffer "*Python3*"))
       ;; (switch-to-buffer (current-buffer))
       (goto-char (point-max))
       (skip-chars-backward " \t\r\n\f")
       (sit-for 0.1)
       (should (string-match "Pdb" (buffer-substring-no-properties (line-beginning-position) (point-max))))))))

(ert-deftest highlight-typed-variables-in-python-41684-test-ZFhHGT ()
  ""
  (py-test
   ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2020-06/msg00128.html
   "foo: int = 1"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "foo")
   (let ((erg (face-at-point)))
     (sit-for 0.1)
     (should (equal erg 'py-variable-name-face)))))

(provide 'py-ert-extra-tests)
;;; py-ert-extra-tests.el ends here
