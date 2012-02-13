;;; py-bug-numbered-tests.el --- run single tests according to bug number

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages
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

(defvar bug-numbered-tests nil
  "Tests following reports at https://bugs.launchpad.net/python-mode")

(defun py-run-bug-numbered-tests (&optional arg)
  "With ARG greater 1 keep test buffers open. "
  (interactive "p")
  (dolist (ele bug-numbered-tests)
    (funcall ele arg)))

(setq bug-numbered-tests
      (if (featurep 'xemacs)
          (list
           'bullet-lists-in-comments-lp:328782-test
           'fill-paragraph-problems-lp:710373-test
           'nested-indents-lp:328775-test
           'previous-statement-lp:637955-test)
        (list
         'mark-block-region-lp:328806-test
         'mark-decorators-lp:328851-test
         'nested-dictionaries-indent-lp:328791-test
         'triple-quoted-string-dq-lp:302834-test
         'fore-00007F-breaks-indentation-lp:328788-test
         'dq-in-tqs-string-lp:328813-test
         'flexible-indentation-lp:328842-test
         'py-current-defun-lp:328846-test
         'cls-pseudo-keyword-lp:328849-test
         'hungry-delete-backwards-lp:328853-test
         'hungry-delete-forward-lp:328853-test
         'beg-end-of-defun-lp:303622-test
         'bullet-lists-in-comments-lp:328782-test
         'imenu-newline-arglist-lp:328783-test
         'imenu-matches-in-docstring-lp:436285-test
         'exceptions-not-highlighted-lp:473525-test
         'fill-paragraph-problems-lp:710373-test
         'nested-indents-lp:328775-test
         'previous-statement-lp:637955-test
         'inbound-indentation-multiline-assignement-lp:629916-test
         'indentation-of-continuation-lines-lp:691185-test
         ;; test passes only when run from edebug
         ;; assistance appreciated
         ;; 'syntaxerror-on-py-execute-region-lp:691542-test
         'goto-beginning-of-tqs-lp:735328-test
         'class-treated-as-keyword-lp:709478-test
         'py-decorators-face-lp:744335-test
         'indent-after-return-lp:745208-test
         'keep-assignements-column-lp:748198-test
         'indent-triplequoted-to-itself-lp:752252-test
         'multiline-listings-indent-lp:761946-test
         'new-page-char-causes-loop-lp:762498-test
         'nested-dicts-indent-lp:763756-test
         'bad-indent-after-except-lp:771289-test
         'indent-open-paren-not-last-lp:771291-test
         'wrong-indent-after-else-lp:772610-test
         'except-indents-wrong-lp:784432-test
         'indent-explicitly-set-in-multiline-tqs-lp:784225-test
         'unbalanced-parentheses-lp:784645-test
         'explicitly-indent-in-list-lp:785018-test
         'explicit-backslashed-continuation-line-indent-lp:785091-test
         'indentation-error-lp:795773-test
         'indent-function-arglist-lp:800088-test
         'python-mode-hangs-lp:801780-test
         'stops-backslashed-line-lp:802504-test
         'stops-backslashed-line-lp:802504-test2
         'python-mode-slow-lp:803275-test
         'master-file-not-honored-lp:794850-test
         'py-variable-name-face-lp:798538-test
         'colon-causes-error-lp:818665-test
         'if-indentation-lp:818720-test
         'closing-parentesis-indent-lp:821820-test
         'py-indent-line-lp:822532-test
         'indent-honor-arglist-whitespaces-lp:822540-test
         'comments-indent-honor-setting-lp:824427-test
         'infinite-loop-after-tqs-lp:826044-test
         'closing-list-lp:826144-test
         'py-electric-comment-add-space-lp:828398-test
         'py-electric-comment-add-space-t-lp:828398-test
         'execute-indented-code-lp:828314-test
         'py-hungry-delete-backwards-needs-cc-lp-850595-test
         'wrong-guess-for-py-indent-offset-lp-852052-test
         'indent-match-import-pkg-lp-852500-test
         'py-shift-line-when-no-region-lp-855565-test
         'indentation-of-from-import-continuation-lines-lp-858041-test
         'indentation-after-one-line-suites-lp:858044-test
         'py-compute-indentation-wrong-at-eol-lp-858043-test
         'comment-indentation-level-lp-869854-test
         'indentation-wrong-after-multi-line-parameter-list-lp-871698-test
         'no-indent-after-continue-lp-872676-test
         'indent-after-inline-comment-lp-873372-test
         'else-clause-indentation-lp-874470-test
         'py-complete-lp:858621-test
         'incorrect-use-of-region-in-py-shift-left-lp:875951-test
         'indent-after-multiple-except-statements-lp:883815-test
         'wrongly-highlighted-as-keywords-lp-885144-test
         'glitch-when-indenting-lists-lp-886473-test
         'indentation-keyword-lp-885143-test
         'indentation-bug-inside-docstrings-lp-899455-test
         'another-indentation-bug-inside-docstrings-lp:900684-test
         'py-shebang-consider-ipython-lp-849293-test
         'py-shebang-ipython-env-lp-849293-test
         'indent-offset-not-guessed-when-loading-lp:902890-test
         'from-__future__-import-absolute_import-mishighlighted-lp-907084-test
         'automatic-indentation-is-broken-lp-889643-test
         'chars-uU-preceding-triple-quoted-get-string-face-lp-909517-test
         'wrong-type-argument-lp-901541-test
         'py-pychecker-run-missing-lp-910783-test
         'py-forward-into-nomenclature-lp-916818-test
         'py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test
         'py-backward-into-nomenclature-caps-names-lp:919541-test
         'execute-buffer-ipython-fails-lp:928087-test
         'UnicodeEncodeError-lp:550661-test
         'py-shell-complete-lp-328836-test)))

(defun py-bug-tests-intern (testname &optional arg teststring)
  (let (py-load-pymacs-p)
    (if arg
        (progn
          (set-buffer (get-buffer-create (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname))))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (fundamental-mode)
          (insert teststring)
          (python-mode)
          (funcall testname)
          (message "%s" (concat (replace-regexp-in-string "-base$" "-test" (prin1-to-string testname)) " passed"))
          (unless (< 1 arg)
            (set-buffer-modified-p 'nil)
            ;; (cond ((processp (get-process "Python3")) (kill-process "Python3"))
            ;; ((processp (get-process "Python2")) (kill-process "Python2"))
            ;; ((processp (get-process "Python")) (ignore-errors (kill-process "Python"))))
            (kill-buffer (current-buffer))))
      (with-temp-buffer
        (let ((font-lock-verbose nil))
          (insert teststring)
          (funcall testname))))))

(defvar python-mode-teststring "class OrderedDict1(dict):
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

    #     def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
            ])
"
  "String used for tests by python-mode-test.el")

(defun sexp-commands-lp:328778-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked.

Reported by Montanaro on 2003-08-05
\[ ... ]
 You can kill balanced expressions on a
 particular line but it's not possible to remove the
 whole of an 'if' or 'while' block."
  (interactive "p")
  (let ((teststring "# Examples from http://diveintopython.org/

def main(argv):
    grammar = \"kant.xml\"
    try:
        opts, args = getopt.getopt(argv, \"hg:d\", [\"help\", \"grammar=\"])
    except getopt.GetoptError:
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt in (\"-h\", \"--help\"):
            usage()
            sys.exit()
        elif opt == '-d':
            global _debug
            _debug = 1
        elif opt in (\"-g\", \"--grammar\"):
            grammar = arg
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'sexp-commands-lp:328778 arg teststring)))

(defun sexp-commands-lp:328778 ()
  (let ((size (buffer-size)))
    (goto-char (point-min))
    (forward-line 15)
    (py-kill-clause)
    (assert (< (buffer-size) size) nil "sexp-commands-lp:328778-test failed")
    (assert (eq (buffer-size) 526) nil "sexp-commands-lp:328778-test failed")
    (kill-line 1)
    (indent-according-to-mode)
    (forward-line -4)
    (py-kill-block)
    (assert (eq (buffer-size) 324) nil "sexp-commands-lp:328778-test failed")
    ))

(defun nested-dictionaries-indent-lp:328791-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.

If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked. "
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    d = {'a':{'b':3,
              'c':4}}
"))
    (py-bug-tests-intern 'nested-dictionaries-indent-lp:328791 arg teststring)))

(defun nested-dictionaries-indent-lp:328791 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char (point-min))
    (forward-line 2)
    (assert (eq 14 (py-compute-indentation)) nil "nested-dictionaries-indent-lp:328791-test failed")))

(defun mark-block-region-lp:328806-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "def f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
                'python-expression',

    #     def ar_thingatpt_write_lists (&optional datei):
                'python-partial-expression',
                'python-statement',
                ])
"))
    (py-bug-tests-intern 'mark-block-region-lp:328806-base arg teststring)))

(defun mark-block-region-lp:328806-base ()
  (forward-line -2)
  (py-mark-block)
  (assert (< (region-beginning) (region-end)) nil "mark-block-region-lp:328806-test failed!"))

(defun flexible-indentation-lp:328842-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
\(long, sequence, of_items,
 that, needs, to_be, wrapped) = input_list

packed_entry = (long, sequence, of_items,
that, needs, to_be, wrapped)

\( whitespaced, long, sequence, of_items,
    that, needs, to_be, wrapped) = input_list
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'flexible-indentation-lp:328842-base arg teststring)))

(defun flexible-indentation-lp:328842-base ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char 33)
    (assert (eq 1 (py-compute-indentation)) nil "flexible-indentation-lp:328842-test failed")
    (goto-char 115)
    (assert (eq 16 (py-compute-indentation)) nil "flexible-indentation-lp:328842-test failed")
    (goto-char 202)
    (assert (eq 2 (py-compute-indentation)) nil "flexible-indentation-lp:328842-test failed")))

(defun py-current-defun-lp:328846-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-current-defun-lp:328846-base arg teststring)))

(defun py-current-defun-lp:328846-base ()
  (goto-char 331)
  (assert (string= "f" (py-current-defun)) nil "py-current-defun-lp:328846-test failed"))

(defun cls-pseudo-keyword-lp:328849-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo(object):
    def summat(cls, x):
          .....
    summat = classmethod(summat)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'cls-pseudo-keyword-lp:328849-base arg teststring)))

(defun cls-pseudo-keyword-lp:328849-base ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 36)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-pseudo-keyword-face) nil "cls-pseudo-keyword-lp:328849-test failed ")))

(defun mark-decorators-lp:328851-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'mark-decorators-lp:328851-base arg teststring)))

(defun mark-decorators-lp:328851-base ()
  (goto-char 10)
  (py-mark-def t)
  (assert (eq 28 (- (region-end)(region-beginning))) nil "mark-decorators-lp:328851-test failed"))

(defun beg-end-of-defun-lp:303622-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
class f():
    \"\"\"
    class blah blah
    \"\"\"
    if a:
        ar_atpt_python_list_roh = ([
            'python-expression',

            # def ar_thingatpt_write_lists (&optional datei):
            'python-partial-expression',
            'python-statement',
        ])
"))
    (py-bug-tests-intern 'beg-end-of-defun-lp:303622 arg teststring)))

(defun beg-end-of-defun-lp:303622 ()
  (goto-char 13)
  (py-end-of-def-or-class)
  (sit-for 0.1)
  (assert (eq 275 (point)) nil "beg-end-of-defun-lp:303622-test #1 failed!")
  (beginning-of-defun)
  (sit-for 0.1)
  (assert (eq 2 (point)) nil "beg-end-of-defun-lp:303622-test #2 failed!"))

(defun dq-in-tqs-string-lp:328813-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
# Bug #328813 (sf1775975)
print \"\"\" \"Hi!\" I'm a doc string\"\"\"
print ''' 'Hi!' I'm a doc string'''
print \"\"\" ''' \"Hi!\" I'm a doc string ''' \"\"\"
print ''' \"\"\" \"Hi!\" I'm a doc string \"\"\" '''
"))
    (py-bug-tests-intern 'dq-in-tqs-string-lp:328813 arg teststring)))

(defun dq-in-tqs-string-lp:328813 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (insert "\"")
      (font-lock-fontify-buffer)
      (assert (eq erg (get-char-property (point) 'face)) nil "dq-in-tqs-string-lp:328813-test failed ")
      (goto-char 122))))

(defun imenu-matches-in-docstring-lp:436285-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
class foo():
    \"\"\"
    class hello(object):
        def __init__(self):
        ...
    \"\"\"
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-matches-in-docstring-lp:436285-base arg teststring)))

(defun imenu-matches-in-docstring-lp:436285-base ()
  (goto-char 40)
  (assert (eq (py-beginning-of-def-or-class) 2) nil "imenu-matches-in-docstring-lp:436285-test failed"))

(defun fill-paragraph-problems-lp:710373-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
    \"\"\"
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    triple-quoted string containing \"quotation\" marks.
    \"\"\"
"))
    (fill-paragraph-problems-lp:710373-test-intern arg teststring)))

(defun fill-paragraph-problems-lp:710373-test-intern (arg teststring)
  (let ((tmp-dir "/tmp/")
        (fpp-exec-buffer "fill-paragraph-problems-lp:710373")
        (diff-buffer "fpp-lp:710373-old"))
    (set-buffer (get-buffer-create diff-buffer))
    (erase-buffer)
    (fundamental-mode)
    (insert teststring)
    (write-file (concat tmp-dir diff-buffer))
    (if arg
        (progn
          (set-buffer (get-buffer-create fpp-exec-buffer))
          (switch-to-buffer (current-buffer))
          (erase-buffer)
          (insert teststring)
          (fundamental-mode)
          (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer))
      (with-temp-buffer
        (insert teststring)
        (fill-paragraph-problems-lp:710373-test-base arg tmp-dir fpp-exec-buffer diff-buffer)))))

(defun fill-paragraph-problems-lp:710373-test-base (arg tmp-dir fpp-exec-buffer diff-buffer)
  (goto-char 48)
  (py-fill-paragraph)
  (write-file (concat tmp-dir fpp-exec-buffer))
  (diff (concat tmp-dir fpp-exec-buffer) (concat tmp-dir diff-buffer) "-u")
  (if (featurep 'xemacs)
      (progn
        (set-buffer "*Diff Output*")
        (switch-to-buffer (current-buffer)))
    (set-buffer "*Diff*")
    (sit-for 1)
    (assert (numberp (progn (goto-char (point-min))(search-forward "no differences" nil t 1))) t)
    (message "%s" "fill-paragraph-problems-lp:710373 passed"))
  (set-buffer "fill-paragraph-problems-lp:710373")
  (unless (< 1 arg)
    (set-buffer-modified-p 'nil)
    (kill-buffer (current-buffer))))

(defun triple-quoted-string-dq-lp:302834-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'triple-quoted-string-dq-lp:302834 arg teststring)))

(defun triple-quoted-string-dq-lp:302834 ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 78)
    (let ((erg (get-char-property (point) 'face)))
      (insert "\"")
      (font-lock-fontify-buffer)
      (assert (eq erg (get-char-property (point) 'face)) "Being stuck inside triple-quoted-string. Did not reach beginning of class."))))

(defun inbound-indentation-multiline-assignement-lp:629916-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo_long_long_long_long = (
    bar_long_long_long_long[
        (x_long_long_long_long == X) &
        (y_long_long_long_long == Y)])
"))
    (py-bug-tests-intern 'inbound-indentation-multiline-assignement-lp:629916 arg teststring)))

(defun inbound-indentation-multiline-assignement-lp:629916 ()
  (let ((py-indent-honors-multiline-listing t))
    (goto-char 33)
    (assert (eq 4 (py-compute-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916-test #1 failed")
    (goto-char 62)
    (assert (eq 8 (current-indentation)) nil "inbound-indentation-multiline-assignement-lp:629916-test #2 failed")))

(defun previous-statement-lp:637955-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class OrderedDict1(dict):
    \"\"\"
    This implementation of a dictionary keeps track of the order
    in which keys were inserted.
    \"\"\""))
    (py-bug-tests-intern 'previous-statement-lp:637955 arg teststring)))

(defun previous-statement-lp:637955 ()
  (beginning-of-line)
  (py-previous-statement)
  (assert (eq 31 (point)) nil "previous-statement-lp:637955-test failed."))

(defun nested-indents-lp:328775-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"
elif x < 0:
    print \"x is negative\"
"))
    (py-bug-tests-intern 'nested-indents-lp:328775 arg teststring)))

(defun nested-indents-lp:328775 ()
  (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775-test #1 failed!")
  (goto-char 41)
  (assert (eq 8 (py-compute-indentation)) nil "nested-indents-lp:328775-test #2 failed!")
  (goto-char 53)
  (assert (eq 4 (py-compute-indentation)) nil "nested-indents-lp:328775-test #3 failed!"))

(defun bullet-lists-in-comments-lp:328782-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring))
    (bullet-lists-in-comments-lp:328782-test-intern arg teststring)))

(defun bullet-lists-in-comments-lp:328782-test-intern (&optional arg teststring)
  (let ((font-lock-verbose nil))
    (set-buffer (get-buffer-create "bullet-lists-in-comments-lp:328782-test"))
    (erase-buffer)
    (insert "
## * If the filename is a directory and not a Maildir nor
##   an MH Mailbox, it will be processed as a Mailbox --this bug named here: bullet-lists-in-comments-lp:328782.htm--
##   directory consisting of just .txt and .lorien files.
")
    (when arg (switch-to-buffer (current-buffer)))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 100)
    (py-fill-paragraph)
    (set-buffer "bullet-lists-in-comments-lp:328782-test")
    (unless (< 1 arg)
      (set-buffer-modified-p 'nil)
      (kill-buffer (current-buffer)))))

(defun imenu-newline-arglist-lp:328783-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def editor(db, db_name, table_name,
    #api
    dbapi,dbapi_exceptions):
        pass"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'imenu-newline-arglist-lp:328783-base arg teststring)))

(defun imenu-newline-arglist-lp:328783-base ()
  (goto-char 60)
  (py-beginning-of-def-or-class)
  (assert (eq (point) 1) nil "imenu-newline-arglist-lp:328783-test failed"))

(defun hungry-delete-backwards-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-backwards-lp:328853 arg teststring)))

(defun hungry-delete-backwards-lp:328853 ()
  (goto-char 421)
  (py-hungry-delete-backwards)
  (assert (eq 416 (point)) nil "hungry-delete-backwards test failed"))

(defun hungry-delete-forward-lp:328853-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring python-mode-teststring))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'hungry-delete-forward-lp:328853 arg teststring)))

(defun hungry-delete-forward-lp:328853 ()
  (goto-char 409)
  (py-hungry-delete-forward)
  (assert (looking-at "#") nil "hungry-delete-backwards test failed"))

(defun UnicodeEncodeError-lp:550661-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-
print u'\\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'UnicodeEncodeError-lp:550661-base 2 teststring)))

(defun UnicodeEncodeError-lp:550661-base ()
  (goto-char 48)
  (push-mark)
  (end-of-line)
  (py-execute-region-switch (line-beginning-position) (point))
  (sit-for 0.2)
  (when (looking-back comint-prompt-regexp)
    (goto-char (1- (match-beginning 0))))
  (sit-for 0.1)
  (assert (or (looking-back "©")(looking-at "©")) nil "UnicodeEncodeError-lp:550661-test failed"))

(defun indentation-of-continuation-lines-lp:691185-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "    def f(val):
        # current behavior - indent to just after the first space
        a_verry_loonng_variable_nammmee = \\
                                        val
"))
    (py-bug-tests-intern 'indentation-of-continuation-lines-lp:691185 arg teststring)))

(defun indentation-of-continuation-lines-lp:691185 ()
  (let ((py-continuation-offset 2))
    (goto-char 127)
    (delete-horizontal-space)
    (indent-to (py-compute-indentation))
    (assert (eq 10 (current-indentation)) nil "indentation-of-continuation-lines-lp:691185-test failed!")))

(defun goto-beginning-of-tqs-lp:735328-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "class Foo(object):
\"\"\"
This docstring isn't indented, test should pass anyway.
\"\"\"

"))
    (py-bug-tests-intern 'goto-beginning-of-tqs-lp:735328 arg teststring)))

(defun goto-beginning-of-tqs-lp:735328 ()
  (goto-char 84)
  (assert (eq 0 (py-compute-indentation)) nil "goto-beginning-of-tqs-lp:735328-test failed")
  )

(defun class-treated-as-keyword-lp:709478-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (when load-branch-function (funcall load-branch-function))
  (let ((teststring "foo = [
    T.div(
        T.tabl(*trows),

        CLASS='blok',)
]
"))
    (py-bug-tests-intern 'class-treated-as-keyword-lp:709478 arg teststring)))

(defun class-treated-as-keyword-lp:709478 ()
  (let ((font-lock-verbose nil))
    (font-lock-fontify-buffer)
    (goto-char 63)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'font-lock-string-face) nil "class-treated-as-keyword-lp:709478d 1th test failed")
    (goto-char 57)
    ;; (assert (if (get-char-property (point) 'face)(eq (get-char-property (point) 'face) 'py-variable-name-face)t) nil "class-treated-as-keyword-lp:709478-test 2th failed")))
    (assert (eq (get-char-property (point) 'face) 'py-variable-name-face) nil "class-treated-as-keyword-lp:709478-test 2th failed")))

(defun fore-00007F-breaks-indentation-lp:328788-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class a:
    def __init__(self):
        self.StyleSetSpec(self.STYLE_FIELD, \"fore:#00007F\" )
            self.StyleSetSpec(self.STYLE_FIELD, \"fore:#00007F\" )
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'fore-00007F-breaks-indentation-lp:328788 arg teststring)))

(defun fore-00007F-breaks-indentation-lp:328788 ()
  (switch-to-buffer (current-buffer))
  (goto-char 34)
  (sit-for 0.1)
  ;; (debug-on-entry 'py-compute-indentation)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation-test #1 failed")
  (goto-char 121)
  (assert (eq 8 (py-compute-indentation)) nil "fore-00007F-breaks-indentation-test #2 failed"))

(defun exceptions-not-highlighted-lp:473525-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "excs = (SystemExit, Exception, KeyboardInterrupt)"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'exceptions-not-highlighted-lp:473525 arg teststring)))

(defun exceptions-not-highlighted-lp:473525 ()
  (let ((font-lock-verbose nil))
    (goto-char 39)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-exception-name-face) nil "exceptions-not-highlighted-lp:473525-test failed")))

(defun syntaxerror-on-py-execute-region-lp:691542-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# -*- coding: utf-8 -*-
print \"Poet Friedrich Hölderlin\""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'syntaxerror-on-py-execute-region-lp:691542-base arg teststring)))

(defun syntaxerror-on-py-execute-region-lp:691542-base ()
  (let ((oldbuf (current-buffer))
        erg kill-buffer-query-functions py-switch-to-python)
    (when (buffer-live-p (get-buffer (concat "*" py-which-bufname "*")))
      (when
          (processp (get-process py-which-bufname))

        (set-process-query-on-exit-flag (get-process py-which-bufname) nil))
      (kill-buffer (concat "*" py-which-bufname "*")))
    (py-execute-region (line-beginning-position) (line-end-position))
    (when (interactive-p) (switch-to-buffer (current-buffer)))
    (set-buffer (get-buffer (concat "*" py-which-bufname "*")))
    (assert (or (search-forward "Hölderlin" nil t 1)
                (search-backward "Hölderlin" nil t 1)) nil "syntaxerror-on-py-execute-region-lp:691542-test failed")))

(defun backslashed-continuation-line-indent-lp:742993-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "
self.last_abc_attr = \
self.last_xyz_attr = \
self.last_abc_other = \
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None

self.last_abc_attr = \\
self.last_xyz_attr = \\
self.last_abc_other = \\
self.last_xyz_other = None
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'backslashed-continuation-line-indent-lp:742993 arg teststring)))

(defun backslashed-continuation-line-indent-lp:742993 ()
  (let ((py-continuation-offset 2))
    (goto-char 93)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (goto-char 145)
    (indent-to (py-compute-indentation))
    (assert (eq 2 (current-indentation)) nil "backslashed-continuation-line-indent-lp:742993-test #1 failed")
    (goto-char 170)
    (assert (eq (py-compute-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993-test #2 failed")

    (setq py-continuation-offset 4)
    (forward-line 2)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))
    (goto-char 277)
    (assert (eq (py-compute-indentation) py-continuation-offset) nil "backslashed-continuation-line-indent-lp:742993-test #4 failed")

    (setq py-continuation-offset 6)
    (forward-line 3)
    (insert (concat "\n# py-continuation-offset: " (number-to-string py-continuation-offset)))))

(defun py-decorators-face-lp:744335-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "@foo.bar
def baz():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-decorators-face-lp:744335 arg teststring)))

(defun py-decorators-face-lp:744335 ()
  (let ((font-lock-verbose nil))
    (goto-char 7)
    (font-lock-fontify-buffer)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-decorators-face) nil "py-decorators-face-lp:744335-test failed")))

(defun indent-after-return-lp:745208-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "class FOO\():
    if len(sys.argv)==1:
        usage\()
        sys.exit\()

    def build_extension\(self, ext):

        if ext.name == '_ctypes':
            if not self.configure_ctypes\(ext):
                return

        try:
            build_ext.build_extension\(self, ext)
        except \(CCompilerError, DistutilsError) as why:
            self.announce\('WARNING: building of extension \"%s\"
failed: %s' %
                          \(ext.name, sys.exc_info()\[1]))
            self.failed.append(ext.name)
            return
        # Workaround for Mac OS X: The Carbon-based modules cannot be
        # reliably imported into a command-line Python
        if 'Carbon' in ext.extra_link_args:
            self.announce\(
                'WARNING: skipping import check for Carbon-based
\"%s\"' %
                ext.name)
            return
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-after-return-lp:745208 arg teststring)))

(defun indent-after-return-lp:745208 ()
  (goto-char (point-max))
  (assert (eq 8 (py-compute-indentation)) nil "indent-after-return-lp:745208-test failed"))

(defun keep-assignements-column-lp:748198-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "bar = foo(a=1,
          b=2,
          c=3)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'keep-assignements-column-lp:748198 arg teststring)))

(defun keep-assignements-column-lp:748198 ()
  (goto-char 45)
  (py-newline-and-indent)
  (assert (eq 0 (current-column)) nil "py-vor test failed"))

(defun indent-triplequoted-to-itself-lp:752252-test (&optional arg load-branch-function)
  "With ARG greater 1 keep test buffer open.
If no `load-branch-function' is specified, make sure the appropriate branch is loaded. Otherwise default python-mode will be checked."
  (interactive "p")
  (let ((teststring "def foo():
    \"\"\"The real foo thing.\n"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-triplequoted-to-itself-lp:752252-base arg teststring)))

(defun indent-triplequoted-to-itself-lp:752252-base ()
  (sit-for 0.1)
  (assert (eq 4 (py-compute-indentation)) nil "indent-triplequoted-to-itself-lp:752252-test failed"))

(defun multiline-listings-indent-lp:761946-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    do_something_first(
        a=1,
                       b=2,
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'multiline-listings-indent-lp:761946-base arg teststring)))

(defun multiline-listings-indent-lp:761946-base ()
  (goto-char 49)
  (assert (eq 8 (py-compute-indentation)) nil "multiline-listings-indent-lp:761946-test failed"))

(defun new-page-char-causes-loop-lp:762498-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo:
    def baz(self):


"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'new-page-char-causes-loop-lp:762498-base arg teststring)))

(defun new-page-char-causes-loop-lp:762498-base ()
  (goto-char 31)
  (assert (eq 8 (py-compute-indentation)) "new-page-char-causes-loop-lp:762498-test failed"))

(defun nested-dicts-indent-lp:763756-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }
))
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'nested-dicts-indent-lp:763756-base arg teststring)))

(defun nested-dicts-indent-lp:763756-base ()
  (let ((py-indent-honors-multiline-listing nil))
    (goto-char 30)
    (assert (eq 4 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756-test failed")
    (goto-char 49)
    (assert (eq 8 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756-test failed")
    (forward-line 1)
    (assert (eq 12 (py-compute-indentation)) nil "nested-dicts-indent-lp:763756-test failed")))

(defun bad-indent-after-except-lp:771289-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    try:
        baz()
    except ValueError:
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'bad-indent-after-except-lp:771289-base arg teststring)))

(defun bad-indent-after-except-lp:771289-base ()
  (assert (eq 8 (py-compute-indentation)) nil "bad-indent-after-except-lp:771289-test failed"))

(defun indent-open-paren-not-last-lp:771291-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "

# Put point after the comma on the last line and hit return. You
# end up in column 8 (i.e. under the 'g' in 'thing') when you
# should end up in column 20 (under the 'w' in 'with_something').
# Note that this is a different case than previously reported,
# where the open paren was the last thing on the line. When the
# open paren is *not* the last thing on the line, the next line's
# indentation should line up under the first non-whitespace
# character following the open paren.

def foo():
    thing = call_it(with_something,"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-open-paren-not-last-lp:771291-base arg teststring)))

(defun indent-open-paren-not-last-lp:771291-base ()
  (newline-and-indent)
  (assert (eq 20 (py-compute-indentation)) nil "indent-open-paren-not-last-lp:771291-test failed"))

(defun wrong-indent-after-else-lp:772610-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if True:
    pass
else:
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'wrong-indent-after-else-lp:772610-base arg teststring)))

(defun wrong-indent-after-else-lp:772610-base ()
  (assert (eq 4 (py-compute-indentation)) nil "wrong-indent-after-else-lp:772610-test failed"))

(defun except-indents-wrong-lp:784432-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "try:
    block1
except:
    block2"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'except-indents-wrong-lp:784432-base arg teststring)))

(defun except-indents-wrong-lp:784432-base ()
  (goto-char 17)
  (assert (eq 0 (py-compute-indentation)) nil "except-indents-wrong-lp:784432-test #1 failed")
  (goto-char 25)
  (assert (eq 4 (py-compute-indentation)) nil "except-indents-wrong-lp:784432-test #2 failed"))

(defun indent-explicitly-set-in-multiline-tqs-lp:784225-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    with bar('x', \"\"\"
        [hello]
"
                    ))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-explicitly-set-in-multiline-tqs-lp:784225-base arg teststring)))

(defun indent-explicitly-set-in-multiline-tqs-lp:784225-base ()
  (assert (eq 8 (py-compute-indentation)) nil "indent-explicitly-set-in-multiline-tqs-lp:784225-test failed"))

(defun unbalanced-parentheses-lp:784645-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    something()
    another(
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'unbalanced-parentheses-lp:784645-base arg teststring)))

(defun unbalanced-parentheses-lp:784645-base ()
  (goto-char 27)
  (newline-and-indent)
  (assert (eq 4 (py-compute-indentation)) nil "unbalanced-parentheses-lp:784645-test failed"))

(defun explicitly-indent-in-list-lp:785018-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def foo():
    with bar('x',
        [hello]
"
                    ))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'explicitly-indent-in-list-lp:785018-base arg teststring)))

(defun explicitly-indent-in-list-lp:785018-base ()
  (assert (eq 8 (py-compute-indentation)) nil "explicitly-dedented-in-list-lp:784225-test failed"))

(defun explicit-backslashed-continuation-line-indent-lp:785091-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "        a_verry_loonng_variable_nammmee = \\
                                        val \\
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'explicit-backslashed-continuation-line-indent-lp:785091-base arg teststring)))

(defun explicit-backslashed-continuation-line-indent-lp:785091-base ()
  (assert (eq 40 (py-compute-indentation)) nil "explicit-backslashed-continuation-line-indent-lp:785091  test failed"))

(defun indentation-error-lp:795773-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class MailTransportAgentAliases:
    \"\"\"Utility for generating all the aliases of a mailing
list.\"\"\"

    implements(IMailTransportAgentAliases)

    def aliases(self, mlist):
        \"\"\"See `IMailTransportAgentAliases`.\"\"\"
        # Always return
        yield mlist.posting_address
        for destination in SUBDESTINATIONS:
            yield '{0}-{1}@{2}'.format(mlist.list_name,
                                             destination,
                                             mlist.host_name)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-error-lp:795773-base arg teststring)))

(defun indentation-error-lp:795773-base ()
  (goto-char 385)
  (assert (eq 39 (py-compute-indentation)) nil "indentation-error-lp:795773-test failed"))

(defun class-highlighted-as-keywords-lp:798287-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class X:
    pass

# Everything is highlighted as a keyword.
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'class-highlighted-as-keywords-lp:798287-base arg teststring)))

(defun class-highlighted-as-keywords-lp:798287-base ()
  (let ((font-lock-verbose nil))
    (goto-char 7)
    (font-lock-fontify-buffer)
    ;; (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-class-name-face) nil "class-highlighted-as-keywords-lp:798287-test failed")))

(defun indent-function-arglist-lp:800088-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "def long_function_name(
        var_one, var_two, var_three,
        var_four):
     print(var_one)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-function-arglist-lp:800088-base arg teststring)))

(defun indent-function-arglist-lp:800088-base ()
  (goto-char 25)
  (let ((py-indent-offset 4))
    (assert (eq 8 (py-compute-indentation)) nil "indent-function-arglist-lp:800088-test failed")))

(defun python-mode-hangs-lp:801780-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "@jit.unroll_safe
def pushrevvalues(self, n, values_w): # n should be len(values_w)
    make_sure_not_resized(values_w)
    while True:
        n -= 1
        if n < 0:
            break
        self.pushvalue(values_w[n])
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'python-mode-hangs-lp:801780-base arg teststring)))

(defun python-mode-hangs-lp:801780-base ()
  (assert (eq 18 (py-beginning-of-def-or-class)) nil "python-mode-hangs-lp:801780-test failed"))

(defun stops-backslashed-line-lp:802504-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if bar == 1 or bar == 2 or bar == 3 or bar == 4 or bar == 5 or bar == 6 or bar == 7 \\
  or bar == 8 or bar == 9 or bar == 10 or bar == 11 or bar == 12 or bar == 13 \\
  or bar == 14 or bar == 15 or bar == 16 or bar == 17 or bar == 18:
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'stops-backslashed-line-lp:802504-base arg teststring)))

(defun stops-backslashed-line-lp:802504-base ()
  (goto-char 49)
  (assert (eq 282 (py-end-of-statement)) nil "stops-backslashed-line-lp:802504-test failed"))

(defun stops-backslashed-line-lp:802504-test2 (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if x>1 and x<100 and y>1 and y<200:
  if bar == 1 or bar == 2 or bar == 3 or bar == 4 or bar == 5 or bar == 6 or bar == 7 \\
  or bar == 8 or bar == 9 or bar == 10 or bar == 11 or bar == 12 or bar == 13 or \\
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'stops-backslashed-line2-lp:802504-base arg teststring)))

(defun stops-backslashed-line2-lp:802504-base ()
  (assert (eq 87 (py-beginning-of-statement)) nil "stops-backslashed-line-lp:802504-test failed"))

(defun python-mode-slow-lp:803275-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# commands.py - command processing for mercurial
#
# Copyright 2005-2007 Matt Mackall <mpm@selenic.com>
#
# This software may be used and distributed according to the terms of the
# GNU General Public License version 2 or any later version.

from node import hex, bin, nullid, nullrev, short
from lock import release
from i18n import _, gettext
import os, re, difflib, time, tempfile, errno
import hg, scmutil, util, revlog, extensions, copies, error, bookmarks
import patch, help, url, encoding, templatekw, discovery
import archival, changegroup, cmdutil, hbisect
import sshserver, hgweb, hgweb.server, commandserver
import merge as mergemod
import minirst, revset, fileset
import dagparser, context, simplemerge
import random, setdiscovery, treediscovery, dagutil

table = {}

command = cmdutil.command(table)

# common command options

globalopts = [
    ('R', 'repository', '',
     _('repository root directory or name of overlay bundle file'),
     _('REPO')),
    ('', 'cwd', '',
     _('change working directory'), _('DIR')),
    ('y', 'noninteractive', None,
     _('do not prompt, assume \\'yes\\' for any required answers')),
    ('q', 'quiet', None, _('suppress output')),
    ('v', 'verbose', None, _('enable additional output')),
    ('', 'config', [],
     _('set/override config option (use \\'section.name=value\\')'),
     _('CONFIG')),
    ('', 'debug', None, _('enable debugging output')),
    ('', 'debugger', None, _('start debugger')),
    ('', 'encoding', encoding.encoding, _('set the charset encoding'),
     _('ENCODE')),
    ('', 'encodingmode', encoding.encodingmode,
     _('set the charset encoding mode'), _('MODE')),
    ('', 'traceback', None, _('always print a traceback on exception')),
    ('', 'time', None, _('time how long the command takes')),
    ('', 'profile', None, _('print command execution profile')),
    ('', 'version', None, _('output version information and exit')),
    ('h', 'help', None, _('display help and exit')),
]

dryrunopts = [('n', 'dry-run', None,
               _('do not perform actions, just print output'))]

remoteopts = [
    ('e', 'ssh', '',
     _('specify ssh command to use'), _('CMD')),
    ('', 'remotecmd', '',
     _('specify hg command to run on the remote side'), _('CMD')),
    ('', 'insecure', None,
     _('do not verify server certificate (ignoring web.cacerts config)')),
]

walkopts = [
    ('I', 'include', [],
     _('include names matching the given patterns'), _('PATTERN')),
    ('X', 'exclude', [],
     _('exclude names matching the given patterns'), _('PATTERN')),
]

commitopts = [
    ('m', 'message', '',
     _('use text as commit message'), _('TEXT')),
    ('l', 'logfile', '',
     _('read commit message from file'), _('FILE')),
]

commitopts2 = [
    ('d', 'date', '',
     _('record the specified date as commit date'), _('DATE')),
    ('u', 'user', '',
     _('record the specified user as committer'), _('USER')),
]

templateopts = [
    ('', 'style', '',
     _('display using template map file'), _('STYLE')),
    ('', 'template', '',
     _('display with template'), _('TEMPLATE')),
]

logopts = [
    ('p', 'patch', None, _('show patch')),
    ('g', 'git', None, _('use git extended diff format')),
    ('l', 'limit', '',
     _('limit number of changes displayed'), _('NUM')),
    ('M', 'no-merges', None, _('do not show merges')),
    ('', 'stat', None, _('output diffstat-style summary of changes')),
] + templateopts

diffopts = [
    ('a', 'text', None, _('treat all files as text')),
    ('g', 'git', None, _('use git extended diff format')),
    ('', 'nodates', None, _('omit dates from diff headers'))
]

diffopts2 = [
    ('p', 'show-function', None, _('show which function each change is in')),
    ('', 'reverse', None, _('produce a diff that undoes the changes')),
    ('w', 'ignore-all-space', None,
     _('ignore white space when comparing lines')),
    ('b', 'ignore-space-change', None,
     _('ignore changes in the amount of white space')),
    ('B', 'ignore-blank-lines', None,
     _('ignore changes whose lines are all blank')),
    ('U', 'unified', '',
     _('number of lines of context to show'), _('NUM')),
    ('', 'stat', None, _('output diffstat-style summary of changes')),
]

similarityopts = [
    ('s', 'similarity', '',
     _('guess renamed files by similarity (0<=s<=100)'), _('SIMILARITY'))
]

subrepoopts = [
    ('S', 'subrepos', None,
     _('recurse into subrepositories'))
]

# Commands start here, listed alphabetically

@command('^add',
    walkopts + subrepoopts + dryrunopts,
    _('[OPTION]... [FILE]...'))
def add(ui, repo, \*pats, \*\*opts):
    \"\"\"add the specified files on the next commit

    Schedule files to be version controlled and added to the
    repository.

    The files will be added to the repository at the next commit. To
    undo an add before that, see :hg:`forget`.

    If no names are given, add all files to the repository.

    .. container:: verbose

       An example showing how new (unknown) files are added
       automatically by :hg:`add`::

         \$ ls
         foo.c
         \$ hg status
         ? foo.c
         \$ hg add
         adding foo.c
         \$ hg status
         A foo.c

    Returns 0 if all files are successfully added.
    \"\"\"

    m = scmutil.match(repo[None], pats, opts)
    rejected = cmdutil.add(ui, repo, m, opts.get('dry_run'),
                           opts.get('subrepos'), prefix=\"\")
    return rejected and 1 or 0
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'python-mode-slow-lp:803275-base arg teststring)))

(defun python-mode-slow-lp:803275-base ()
  (goto-char 1)
  (assert (eq 5430 (py-end-of-def-or-class)) nil "python-mode-slow-lp:803275-test failed"))

(defun master-file-not-honored-lp:794850-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

# Local Variables:
# py-master-file: \"/usr/tmp/my-master.py\"
# End:

print \"master-file is executed\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'master-file-not-honored-lp:794850-base arg teststring)))

(defun master-file-not-honored-lp:794850-base ()
  (save-excursion
    (set-buffer (get-buffer-create "test-master.py"))
    (erase-buffer)
    (insert "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

print \"Hello, I'm your master!\"
")
    (write-file "/var/tmp/my-master.py"))
  (py-execute-buffer))

(defun py-variable-name-face-lp:798538-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "class Foo(object):
    def summat(cls, x):
          .....
    summat = classmethod(summat)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-variable-name-face-lp:798538-base arg teststring)))

(defun py-variable-name-face-lp:798538-base ()
  (let ((font-lock-verbose nil))
    (font-lock-mode 1)
    (font-lock-fontify-buffer)
    (goto-char 64)
    (sit-for 0.1)
    (assert (eq (get-char-property (point) 'face) 'py-variable-name-face) nil "py-variable-name-face-lp:798538-test failed ")))

(defun colon-causes-error-lp:818665-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

print \"Hello!\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'colon-causes-error-lp:818665-base arg teststring)))

(defun colon-causes-error-lp:818665-base ()
  (insert ":")
  (forward-char -1)
  (assert (looking-at ":") nil "colon-causes-error-lp:818665-test failed"))

(defun if-indentation-lp:818720-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
 # -*- coding: utf-8 -*-

class X():
    def __init__( self ):
        self.lookup = {}

    def y( self, p ):
        p = p.foo()
        if p in self.lookup:
            return self.lookup[ foo ]
        else:
            return None
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'if-indentation-lp:818720-base arg teststring)))

(defun if-indentation-lp:818720-base ()
  (goto-char 196)
  (assert (eq 12 (py-compute-indentation)) nil "if-indentation-lp:818720-test failed"))

(defun closing-parentesis-indent-lp:821820-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
        )
    )
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'closing-parentesis-indent-lp:821820-base arg teststring)))

(defun closing-parentesis-indent-lp:821820-base ()
  (let ((py-closing-list-dedents-bos t))
    (forward-line -1)
    (assert (eq 4 (py-compute-indentation)) nil "closing-parentesis-indent-lp:821820-test failed")))

(defun py-indent-line-lp:822532-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "
if x > 0:
    for i in range(100):
        print i
    else:
    print \"All done\"
elif x < 0:
    print \"x is negative\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-indent-line-lp:822532-base arg teststring)))

(defun py-indent-line-lp:822532-base ()
  (goto-char 53)
  (assert (eq 4 (py-compute-indentation)) nil "py-indent-line-lp:822532-test failed"))

(defun indent-honor-arglist-whitespaces-lp:822540-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python3
# -*- coding: utf-8 -*-

abc( ghi,
    jkl
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-honor-arglist-whitespaces-lp:822540-base arg teststring)))

(defun indent-honor-arglist-whitespaces-lp:822540-base ()
  (forward-line -1)
  (assert (eq 5 (py-compute-indentation)) nil "indent-honor-arglist-whitespaces-lp:822540-test failed"))

(defun comments-indent-honor-setting-lp:824427-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-

if __name__ == '__main__':
    from foo import \*
    foo([\"baz\"]) # list of foo's development directories

# limitations:
#
#   Some comments on limitations:
# asdf
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'comments-indent-honor-setting-lp:824427-base arg teststring)))

(defun comments-indent-honor-setting-lp:824427-base ()
  (goto-char 206)
  (assert (eq 0 (py-compute-indentation)) nil "comments-indent-honor-setting-lp:824427-test failed"))

(defun infinite-loop-after-tqs-lp:826044-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "\"\"\"
hey
\"\"\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'infinite-loop-after-tqs-lp:826044-base arg teststring)))

(defun infinite-loop-after-tqs-lp:826044-base ()
  (assert (eq 0 (py-newline-and-indent)) nil "infinite-loop-after-tqs-lp:826044-test failed"))

(defun closing-list-lp:826144-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if foo:
    bar.append(
        ht(
            T.a('Sorted Foo', href='#Blub', ),
            ' -- foo bar baz--',
            self.Tasdf( afsd ),
            self.Tasdf( asdf ),
            )
        )
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'closing-list-lp:826144-base arg teststring)))

(defun closing-list-lp:826144-base ()
  (goto-char 241)
  (assert (eq 12 (py-compute-indentation)) nil "infinite-loop-after-tqs-lp:826044-test failed")
  (goto-char 251)
  (assert (eq 8 (py-compute-indentation)) nil "infinite-loop-after-tqs-lp:826044-test failed")
  )

(defun py-electric-comment-add-space-lp:828398-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-electric-comment-add-space-lp:828398-base arg teststring)))

(defun py-electric-comment-add-space-lp:828398-base ()
  (let ((py-electric-comment-add-space-p nil))
    (py-electric-comment 1)
    (assert (looking-back "#") nil "py-electric-comment-add-space-lp:828398-test failed")))

(defun py-electric-comment-add-space-t-lp:828398-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-electric-comment-add-space-t-lp:828398-base arg teststring)))

(defun py-electric-comment-add-space-t-lp:828398-base ()
  (let ((py-electric-comment-add-space-p t))
    (py-electric-comment 1)
    (assert (looking-back " ") nil "py-electric-comment-add-space-lp:828398-test failed")))

(defun execute-indented-code-lp:828314-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "if __name__ == \"__main__\":
    print \"hello\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'execute-indented-code-lp:828314-base 2 teststring)))

(defun execute-indented-code-lp:828314-base ()
  (let ((debug-on-error t))
    (goto-char 28)
    (push-mark)
    (progn
      (py-execute-base (point) (progn (end-of-line)(point)))
      (when (interactive-p) (message "%s" "execute-indented-code-lp:828314-test passed")))))

(defun wrong-indentation-of-function-arguments-lp:840891-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "abdc = foo(a=1,
           b=2,
    c=3)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'wrong-indentation-of-function-arguments-lp:840891-base arg teststring)))

(defun wrong-indentation-of-function-arguments-lp:840891-base ()
  (goto-char 38)
  (assert (eq 11 (py-compute-indentation)) nil "wrong-indentation-of-function-arguments-lp:840891-test failed"))

(defun py-shebang-consider-ipython-lp-849293-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/ipython
# -*- coding: utf-8 -*-
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shebang-consider-ipython-lp-849293-base arg teststring)))

(defun py-shebang-consider-ipython-lp-849293-base ()
  (assert (string= "ipython" (py-choose-shell-by-shebang)) nil "py-shebang-consider-ipython-lp-849293-test failed"))

(defun py-shebang-ipython-env-lp-849293-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/env ipython
# -*- coding: utf-8 -*-
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shebang-ipython-env-lp-849293-base arg teststring)))

(defun py-shebang-ipython-env-lp-849293-base ()
  (assert (string= "ipython" (py-choose-shell-by-shebang)) nil "py-shebang-ipython-env-lp-849293-test failed"))

(defun py-hungry-delete-backwards-needs-cc-lp-850595-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring ""))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-hungry-delete-backwards-needs-cc-lp-850595-base arg teststring)))

(defun py-hungry-delete-backwards-needs-cc-lp-850595-base ()
  (assert (functionp 'c-hungry-delete-backwards) nil "py-hungry-delete-backwards-needs-cc-lp-850595-test failed"))

(defun wrong-guess-for-py-indent-offset-lp-852052-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "# The indent offset shouldn't be guessed from backslash
# continuations. I have

from long.pkg.name import long, list, of, \\
     class_and_function, and, function, names

# (note there are five spaces before \"class\", to match with the
# start of the pkg name.)

# Since the indent of backlash-continued lines has no meaning for
# code, it should not be considered.
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'wrong-guess-for-py-indent-offset-lp-852052-base arg teststring)))

(defun wrong-guess-for-py-indent-offset-lp-852052-base ()
  (goto-char 126)
  (assert (eq 4 (py-guess-indent-offset)) nil "wrong-guess-for-py-indent-offset-lp-852052-test failed"))

(defun indent-match-import-pkg-lp-852500-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "from long.pkg.name import long, list, of, \\
     class_and_function, names

# (note there are five spaces before \"class\", to match with the
# start of the pkg name.)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-match-import-pkg-lp-852500-base arg teststring)))

(defun indent-match-import-pkg-lp-852500-base ()
  (goto-char 45)
  (assert (eq 5 (py-compute-indentation)) nil "indent-match-import-pkg-lp-852500-test failed"))

(defun py-shift-line-when-no-region-lp-855565-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/ipython
# -*- coding: utf-8 -*-

if foo:
    print"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shift-line-when-no-region-lp-855565-base arg teststring)))

(defun py-shift-line-when-no-region-lp-855565-base ()
  (goto-char 58)
  (assert (eq 8 (py-shift-right 1)) nil "py-shift-line-when-no-region-lp-855565-test failed"))

(defun highlighting-in-multiline-function-call-arguments-lp:856833-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-

newObj = SomeClassWithManyManyArgs (param0 = val0,
    param1 = val1,
    param2 = val2, param3 = val3)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'highlighting-in-multiline-function-call-arguments-lp:856833-base arg teststring)))

(defun highlighting-in-multiline-function-call-arguments-lp:856833-base ()
  (font-lock-fontify-buffer)
  (goto-char 80)
  ;; (goto-char 106)
  (assert (eq (get-char-property (point) 'face) nil) nil "highlighting-in-multiline-function-call-arguments-lp:856833 test failed "))

(defun py-shift-preserve-active-region-lp-857837-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -\*- coding: utf-8 -\*-

print 'hello'
print 'world'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shift-preserve-active-region-lp-857837-base arg teststring)))

(defun py-shift-preserve-active-region-lp-857837-base ()
  (goto-char 49)
  (assert nil "py-shift-preserve-active-region-lp-857837-test failed"))

(defun variable-highlighted-on-LHS-of-eq-lp-858304-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if someVar == 5:
    doSomething()
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'variable-highlighted-on-LHS-of-eq-lp-858304-base arg teststring)))

(defun variable-highlighted-on-LHS-of-eq-lp-858304-base ()
  (goto-char 55)
  (assert (eq (get-char-property (point) 'face) nil) nil "variable-highlighted-on-LHS-of-eq-lp-858304-test failed"))

(defun indent-guessing-lp-858040-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

some_longer_call(arguments,
         arguments)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-guessing-lp-858040-base arg teststring)))

(defun indent-guessing-lp-858040-base ()
  (goto-char 40)
  (assert (eq 4 py-indent-offset) nil "indent-guessing-lp-858040-test failed"))

(defun indentation-of-from-import-continuation-lines-lp-858041-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

from nicos import session
from nicos import status, loggers
from nicos.utils import DeviceMeta, Param, Override, Value, getVersions, \\
usermethod, tupleof, floatrange, any, none_or

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-of-from-import-continuation-lines-lp-858041-base arg teststring)))

(defun indentation-of-from-import-continuation-lines-lp-858041-base ()
  (goto-char 184)
  (assert (eq 5 (py-compute-indentation)) nil "indentation-of-from-import-continuation-lines-lp-858041-test failed"))

(defun indentation-after-one-line-suites-lp:858044-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if foo: return

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-after-one-line-suites-lp:858044-base arg teststring)))

(defun indentation-after-one-line-suites-lp:858044-base ()
  (goto-char 64)
  (assert (eq 0 (py-compute-indentation)) nil "indentation-after-one-line-suites-lp:858044-test failed"))

(defun py-compute-indentation-wrong-at-eol-lp-858043-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

if maxdepth == 0 or depth < maxdepth:
      item += build_toc(sectionnode, depth+1)
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-compute-indentation-wrong-at-eol-lp-858043-base arg teststring)))

(defun py-compute-indentation-wrong-at-eol-lp-858043-base ()
  (setq py-smart-indentation nil)
  (setq py-indent-offset 4)
  (goto-char 132)
  (assert (eq 4 (py-compute-indentation)) nil "py-compute-indentation-wrong-at-eol-lp-858043-test failed"))

(defun comment-indentation-level-lp-869854-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    def bar():
        x = 1
# asdf

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'comment-indentation-level-lp-869854-base arg teststring)))

(defun comment-indentation-level-lp-869854-base ()
  (goto-char 104)
  (assert (eq 0 (py-compute-indentation))  nil "comment-indentation-level-lp-869854-test failed"))

(defun indentation-wrong-after-multi-line-parameter-list-lp-871698-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo(bar, baz):
    # indenation as it should be

def foo(bar,
        baz):
# this is the next indentation line (none)

class Foo:
    def bar(biz,
            baz):
    # indentation here after newline

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-wrong-after-multi-line-parameter-list-lp-871698-base arg teststring)))

(defun indentation-wrong-after-multi-line-parameter-list-lp-871698-base ()
  (goto-char 68)
  (assert (eq 4 (py-compute-indentation)) nil "indentation-wrong-after-multi-line-parameter-list-lp-871698-test #1 failed")
  (goto-char 115)
  (assert (eq 8 (py-compute-indentation)) nil "indentation-wrong-after-multi-line-parameter-list-lp-871698-test #2 failed")
  (goto-char 201)
  (assert (eq 12 (py-compute-indentation)) nil "indentation-wrong-after-multi-line-parameter-list-lp-871698-test #3 failed")
  (goto-char 223)
  (assert (eq 8 (py-compute-indentation)) nil "indentation-wrong-after-multi-line-parameter-list-lp-871698-test #4 failed")
  )

(defun no-indent-after-continue-lp-872676-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    for i in range(10):
        if i == 7:
            continue
        if i == 9

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'no-indent-after-continue-lp-872676-base arg teststring)))

(defun no-indent-after-continue-lp-872676-base ()
  (goto-char 141)
  (assert (eq 8 (py-compute-indentation)) nil "no-indent-after-continue-lp-872676-test failed"))

(defun indent-after-inline-comment-lp-873372-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

foo = True # the next line is indented incorrectly
           # to here
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-after-inline-comment-lp-873372.txt-base arg teststring)))

(defun indent-after-inline-comment-lp-873372.txt-base ()
  (goto-char 111)
  (assert (eq 0 (py-compute-indentation)) nil "indent-after-inline-comment-lp-873372-test failed"))

(defun else-clause-indentation-lp-874470-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    for i in range(10):
        if i == 7:
            continue
        do_something(i)
    else
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'else-clause-indentation-lp-874470-base arg teststring)))

(defun else-clause-indentation-lp-874470-base ()
  (goto-char 156)
  (assert (eq 4 (py-compute-indentation)) nil "else-clause-indentation-lp-874470-test failed"))

(defun incorrect-use-of-region-in-py-shift-left-lp:875951-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    for i in range(10):
        for j in range(10):
            print i, j
        print 'next'

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'incorrect-use-of-region-in-py-shift-left-lp:875951-base arg teststring)))

(defun incorrect-use-of-region-in-py-shift-left-lp:875951-base ()
  (push-mark 84)
  (goto-char 135)
  (py-shift-left 1 84 135)
  (assert (eq  8 (current-indentation)) nil "incorrect-use-of-region-in-py-shift-left-lp:875951-test failed"))

(defun py-complete-lp:858621-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

pri
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-complete-lp:858621-base 2 teststring)))

(defun py-complete-lp:858621-base ()
  (goto-char 52)
  (ignore-errors (completion-at-point))
  (sit-for 0.1)
  (assert (eq 54 (point)) nil "py-complete-lp:858621-test failed"))

(defun indentation-after-line-with-keyword-lp-883073-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

with_foo = False
    # indents here
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-after-line-with-keyword-lp-883073-base arg teststring)))

(defun indentation-after-line-with-keyword-lp-883073-base ()
  (goto-char 66)
  (assert (eq 0 (py-compute-indentation)) nil "indentation-after-line-with-keyword-lp-883073-test failed"))

(defun indent-after-multiple-except-statements-lp:883815-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    try:
        x = 1
    except SystemError:
        f = 1
except KeyError:
        p = 1
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-after-multiple-except-statements-lp:883815-base arg teststring)))

(defun indent-after-multiple-except-statements-lp:883815-base ()
  (goto-char 121)
  (assert (eq 4 (py-compute-indentation)) nil "indent-after-multiple-except-statements-lp:883815-test failed"))

(defun wrongly-highlighted-as-keywords-lp-885144-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

date_range = 4
date_range_max = 3
latest_sum = 5
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'wrongly-highlighted-as-keywords-lp-885144-base arg teststring)))

(defun wrongly-highlighted-as-keywords-lp-885144-base ()
  (font-lock-fontify-buffer)
  (goto-char 55)
  (sit-for 0.1)
  (assert (eq (get-char-property (point) 'face) 'py-variable-name-face) nil "wrongly-highlighted-as-keywords-lp-885144-test failed"))

(defun glitch-when-indenting-lists-lp-886473-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def foo(bar,
        baz):
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'glitch-when-indenting-lists-lp-886473-base arg teststring)))

(defun glitch-when-indenting-lists-lp-886473-base ()
  (goto-char 61)
  (assert (eq 8 (py-compute-indentation))  nil "glitch-when-indenting-lists-lp-886473-test failed"))

(defun keywords-in-identifiers-highlighted-incorrectly-lp:888338-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def possibly_break():
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'keywords-in-identifiers-highlighted-incorrectly-lp:888338-base arg teststring)))

(defun keywords-in-identifiers-highlighted-incorrectly-lp:888338-base ()
  (font-lock-fontify-buffer)
  (sit-for 0.1)
  (goto-char 55)
  (assert (eq (get-char-property (point) 'face) 'font-lock-function-name-face) nil "keywords-in-identifiers-highlighted-incorrectly-lp:888338-test failed"))

(defun indentation-keyword-lp-885143-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
import sys
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-keyword-lp-885143-base arg teststring)))

(defun indentation-keyword-lp-885143-base ()
  (goto-char 48)
  (assert (eq 0 (py-compute-indentation))  nil "indentation-keyword-lp-885143-test failed"))

(defun py-shell-complete-lp-328836-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-shell-complete-lp-328836-base 2 teststring)))

(defun py-shell-complete-lp-328836-base ()
  (python-dedicated)
  (goto-char (point-max))
  (insert "pri")
  (py-shell-complete)
  (assert (looking-back "print") nil "py-shell-complete-lp-328836-test failed"))

(defun indentation-bug-inside-docstrings-lp-899455-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

def foo():
    \"\"\"This is my docstring.

    There are many others like it, but this one is mine. My docstring is my
    best friend. It is my life. I must master it as I must master my life.
    Without me, my docstring is useless. Without my docstring, I am useless.
    I must write my docstring true. I must write clearer than my enemy, who
    is trying to confuse me. I must enlighten him before he enlightens me. I
    will. Before Guido I swear this creed: my docstring and myself are
    defenders of my codebase, we are the masters of our enemy, we are the
    saviors of my life. So be it, until there is no enemy, but peace. Amen.

    `foo`: str or None
        If None then baz.

    \"\"\"

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indentation-bug-inside-docstrings-lp-899455-base arg teststring)))

(defun indentation-bug-inside-docstrings-lp-899455-base ()
  (goto-char 742)
  (sit-for 0.1)
  (assert (eq 8 (py-compute-indentation)) nil "indentation-bug-inside-docstrings-lp-899455-test failed"))

(defun another-indentation-bug-inside-docstrings-lp:900684-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def is_x_day(date):
    \"\"\"Return True if given date is the X-day.

    \"\"\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'another-indentation-bug-inside-docstrings-lp:900684-base arg teststring)))

(defun another-indentation-bug-inside-docstrings-lp:900684-base ()
  (goto-char 116)
  (sit-for 0.1)
  (assert (eq 4 (py-compute-indentation)) nil "another-indentation-bug-inside-docstrings-lp:900684-test failed"))

(defun indent-offset-not-guessed-when-loading-lp:902890-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
#! /usr/bin/python
# -*- coding: utf-8 -*-
def main():
  if len(sys.argv)==1:
    usage()
    sys.exit()
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'indent-offset-not-guessed-when-loading-lp:902890-base arg teststring)))

(defun indent-offset-not-guessed-when-loading-lp:902890-base ()
  (assert (eq 2 py-indent-offset) nil "indent-offset-not-guessed-when-loading-lp:902890-test failed"))

(defun from-__future__-import-absolute_import-mishighlighted-lp-907084-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import absolute_import
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'from-__future__-import-absolute_import-mishighlighted-lp-907084-base arg teststring)))

(defun from-__future__-import-absolute_import-mishighlighted-lp-907084-base ()
  (font-lock-fontify-buffer)
  (goto-char 82)
  (assert (not (eq (get-char-property (point) 'face) 'font-lock-keyword-face)) nil "from-__future__-import-absolute_import-mishighlighted-lp-907084-test failed"))

(defun automatic-indentation-is-broken-lp-889643-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'automatic-indentation-is-broken-lp-889643-base arg teststring)))

(defun automatic-indentation-is-broken-lp-889643-base ()
  (assert (eq (key-binding (kbd "RET")) 'py-newline-and-indent) nil "automatic-indentation-is-broken-lp-889643-test failed"))

(defun chars-uU-preceding-triple-quoted-get-string-face-lp-909517-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
u\"hi\" and u\"\"\"d\"\"\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'chars-uU-preceding-triple-quoted-get-string-face-lp-909517-base arg teststring)))

(defun chars-uU-preceding-triple-quoted-get-string-face-lp-909517-base ()
  (goto-char 58)
  (assert (eq nil (get-char-property (point) 'face)) nil "chars-uU-preceding-triple-quoted-get-string-face-lp-909517-test failed"))

(defun wrong-type-argument-lp-901541-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
# source: argparse.py
# Author: Steven J. Bethard <steven.bethard@gmail.com>.

\"\"\"Command-line parsing library

This module is an optparse-inspired command-line parsing library that:

    - handles both optional and positional arguments
    - produces highly informative usage messages
    - supports parsers that dispatch to sub-parsers
\"\"\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'wrong-type-argument-lp-901541-base arg teststring)))

(defun wrong-type-argument-lp-901541-base ()
  (goto-char 385)
  (assert (eq 4 (py-compute-indentation)) nil "wrong-type-argument-lp-901541-test failed"))

(defun py-pychecker-run-missing-lp-910783-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-pychecker-run-missing-lp-910783-base arg teststring)))

(defun py-pychecker-run-missing-lp-910783-base ()
  (assert (commandp 'py-pychecker-run) nil "py-pychecker-run-missing-lp-910783-test failed"))

(defun py-forward-into-nomenclature-lp-916818-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def doSomething(blah)
print \"\"\"Es müsste \"müßte\" heißen.\"\"\"
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-forward-into-nomenclature-lp-916818-base arg teststring)))

(defun py-forward-into-nomenclature-lp-916818-base ()
  (goto-char 48)
  (assert (eq 51 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-lp-916818-test #1 failed")
  (assert (eq 54 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-lp-916818-test #2 failed")
  (assert (eq 63 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-lp-916818-test #3 failed")
  (assert (eq 68 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-lp-916818-test #4 failed")
  (goto-char 88)
  (assert (eq 95 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-lp-916818-test #5 failed"))

(defun tab-completion-in-Ipython-buffers-lp-916869-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-

"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'tab-completion-in-Ipython-buffers-lp-916869-base arg teststring)))

(defun tab-completion-in-Ipython-buffers-lp-916869-base ()
  (ipython-dedicated)
  (switch-to-buffer (current-buffer))
  (goto-char (point-max))
  (sit-for 0.1)
  (insert "pri")
  (ipython-complete)
  (sit-for 0.1)
  (assert (looking-back "print") nil "tab-completion-in-Ipython-buffers-lp-916869-test failed"))

(defun py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
def SomeFunction(arg):
    pass
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-base arg teststring)))

(defun py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-base ()
  (goto-char 52)
  (assert (eq 56 (py-forward-into-nomenclature)) nil "py-forward-into-nomenclature-jumps-over-CamelCased-words-lp:919540-test failed"))

(defun py-backward-into-nomenclature-caps-names-lp:919541-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
return SOME_Constant + blah
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-backward-into-nomenclature-caps-names-lp:919541-base arg teststring)))

(defun py-backward-into-nomenclature-caps-names-lp:919541-base ()
  (goto-char 64)
  (assert (eq 60 (py-backward-into-nomenclature)) nil "py-backward-into-nomenclature-caps-names-lp:919541-test failed"))

(defun py-ipython-complete-lp:927136-test (&optional arg load-branch-function)
  (interactive "p")
  (py-shell nil nil "ipython" 'noswitch)
  (let ((teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
impo"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'py-ipython-complete-lp:927136-base arg teststring)))

(defun py-ipython-complete-lp:927136-base ()
  (assert (string= "import" (ipython-complete)) nil "py-ipython-complete-lp:927136-test failed"))

(defun execute-buffer-ipython-fails-lp:928087-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
print 4 + 5
print u'\\xA9'
"))
    (when load-branch-function (funcall load-branch-function))
    (py-bug-tests-intern 'execute-buffer-ipython-fails-lp:928087-base arg teststring)))

(defun execute-buffer-ipython-fails-lp:928087-base ()
  (assert (numberp (py-execute-buffer)) nil "execute-buffer-ipython-fails-lp:928087-test failed"))


(provide 'py-bug-numbered-tests)
;;; py-bug-numbered-tests.el ends here
