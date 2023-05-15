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

(ert-deftest filling-docstring-paragraphs-gibberish-140-test-DqgSN7 ()
  (py-test-with-temp-buffer
      "def _bless_my_loader(module_globals):
    \"\"\"
    In Python 3.14, the end state is to require and use the module's
    __spec__.loader and ignore any __loader__ attribute on the
    module.

    * If you have a __loader__ and a __spec__.loader but they are not the
    same, in Python 3.12 we issue a DeprecationWarning and fall back to
    __loader__ for backward compatibility.  In Python 3.14, we'll flip
    this case to ignoring __loader__ entirely, without error.

    * If you do not have a __spec__ or __spec__.loader, we also issue a
    DeprecationWarning and fall back to __loader__.  In Python 3.14,
    we'll make this case fail with an AttributeError, and ignore
    __loader__.

    * In Python 3.12 and beyond, if you do not have a __loader__, we don't
    care as long as you still have a __spec__.loader, otherwise you get
    an AttributeError, telling you to add a __spec__.loader.

    See GH#97850 for details.
    \"\"\"
    pass"
    (let ((py-docstring-style 'pep-257-nn))
      (goto-char (point-max))
      (search-backward "attribute")
      (fill-paragraph)
      (forward-line -1)
      (should-not (py-empty-line-p))

)))

(ert-deftest filling-docstring-paragraphs-gibberish-140-test-G9xLBO ()
  (py-test-with-temp-buffer
      "def _bless_my_loader(module_globals):
    \"\"\"
    In Python 3.14, the end state is to require and use the module's
    __spec__.loader and ignore any __loader__ attribute on the
    module.

    * If you have a __loader__ and a __spec__.loader but they are not the
    same, in Python 3.12 we issue a DeprecationWarning and fall back to
    __loader__ for backward compatibility.  In Python 3.14, we'll flip
    this case to ignoring __loader__ entirely, without error.

    * If you do not have a __spec__ or __spec__.loader, we also issue a
    DeprecationWarning and fall back to __loader__.  In Python 3.14,
    we'll make this case fail with an AttributeError, and ignore
    __loader__.

    * In Python 3.12 and beyond, if you do not have a __loader__, we don't
    care as long as you still have a __spec__.loader, otherwise you get
    an AttributeError, telling you to add a __spec__.loader.

    See GH#97850 for details.
    \"\"\"
    pass"
    (let ((py-docstring-style 'pep-257-nn))
      (goto-char (point-max))
      (search-backward "__loader__ and a __spec__")
      (fill-paragraph)
      (forward-line 1)
      (back-to-indentation)
      (should (eq (current-column) 6)))))

(ert-deftest fill-docstring-paragraphs-test-9vhmQx ()
  (py-test-with-temp-buffer
      "class DataFrame(NDFrame, OpsMixin):
    \"\"\"
    Two-dimensional, size-mutable, potentially heterogeneous tabular data\.

    Data structure also contains labeled axes (rows and columns)\.
    Arithmetic operations align on both row and column labels\. Can be
    thought of as a dict-like container for Series objects\. The primary
    pandas data structure\.

    Parameters
    ----------
    data : ndarray (structured or homogeneous), Iterable, dict, or DataFrame
        Dict can contain Series, arrays, constants, dataclass or list-like objects\. If
        data is a dict, column order follows insertion-order\. If a dict contains Series
        which have an index defined, it is aligned by its index\.

        \.\. versionchanged:: 0\.25\.0
           If data is a list of dicts, column order follows insertion-order\.

    index : Index or array-like
        Index to use for resulting frame\. Will default to RangeIndex if
        no indexing information part of input data and no index provided\.
    columns : Index or array-like
        Column labels to use for resulting frame when data does not have them,
        defaulting to RangeIndex(0, 1, 2, \.\.\., n)\. If data contains column labels,
        will perform column selection instead\.
    dtype : dtype, default None
        Data type to force\. Only a single dtype is allowed\. If None, infer\.
    copy : bool or None, default None
        Copy data from inputs\.
        For dict data, the default of None behaves like \`\`copy=True\`\`\.  For DataFrame
        or 2d ndarray input, the default of None behaves like \`\`copy=False\`\`\.
        If data is a dict containing one or more Series (possibly of different dtypes),
        \`\`copy=False\`\` will ensure that these inputs are not copied\.

        \.\. versionchanged:: 1\.3\.0

    See Also
    --------
    DataFrame\.from_records : Constructor from tuples, also record arrays\.
    DataFrame\.from_dict : From dicts of Series, arrays, or dicts\.
    read_csv : Read a comma-separated values (csv) file into DataFrame\.
    read_table : Read general delimited file into DataFrame\.
    read_clipboard : Read text from clipboard into DataFrame\.

    Notes
    -----
    Please reference the :ref:\`User Guide <basics\.dataframe>\` for more information\.

    Examples
    --------
    Constructing DataFrame from a dictionary\.

    >>> d = {'col1': \[1, 2], 'col2': \[3, 4]}
    >>> df = pd\.DataFrame(data=d)
    >>> df
       col1  col2
    0     1     3
    1     2     4

    Notice that the inferred dtype is int64\.

    >>> df\.dtypes
    col1    int64
    col2    int64
    dtype: object

    To enforce a single dtype:

    >>> df = pd\.DataFrame(data=d, dtype=np\.int8)
    >>> df\.dtypes
    col1    int8
    col2    int8
    dtype: object

    Constructing DataFrame from a dictionary including Series:

    >>> d = {'col1': \[0, 1, 2, 3], 'col2': pd\.Series(\[2, 3], index=\[2, 3])}
    >>> pd\.DataFrame(data=d, index=\[0, 1, 2, 3])
       col1  col2
    0     0   NaN
    1     1   NaN
    2     2   2\.0
    3     3   3\.0

    Constructing DataFrame from numpy ndarray:

    >>> df2 = pd\.DataFrame(np\.array(\[\[1, 2, 3], \[4, 5, 6], \[7, 8, 9]]),
    \.\.\.                    columns=\['a', 'b', 'c'])
    >>> df2
       a  b  c
    0  1  2  3
    1  4  5  6
    2  7  8  9

    Constructing DataFrame from a numpy ndarray that has labeled columns:

    >>> data = np\.array(\[(1, 2, 3), (4, 5, 6), (7, 8, 9)],
    \.\.\.                 dtype=\[(\"a\", \"i4\"), (\"b\", \"i4\"), (\"c\", \"i4\")])
    >>> df3 = pd\.DataFrame(data, columns=\['c', 'a'])
    \.\.\.
    >>> df3
       c  a
    0  3  1
    1  6  4
    2  9  7

    Constructing DataFrame from dataclass:

    >>> from dataclasses import make_dataclass
    >>> Point = make_dataclass(\"Point\", \[(\"x\", int), (\"y\", int)])
    >>> pd\.DataFrame(\[Point(0, 0), Point(0, 3), Point(2, 3)])
       x  y
    0  0  0
    1  0  3
    2  2  3
    \"\"\"
    pass"
    (goto-char (point-max))
    (search-backward "data : ndarray")
    (py-fill-paragraph)
    (search-forward "Dict")
    (should (eq 8 (current-indentation)))
    ))

(ert-deftest py-ert-moves-up-fill-paragraph-onetwo-KWl8aD ()
  (let ((py-docstring-style 'onetwo))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
      (goto-char 49)
      (fill-paragraph)
      (search-backward "\"\"\"")
      (goto-char (match-end 0))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72)))))

(ert-deftest py-ert-moves-up-fill-paragraph-onetwo-6v2Vqe ()
  (let ((py-docstring-style 'onetwo))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
      (goto-char 49)
      (fill-paragraph)
      (search-backward "\"\"\"")
      (goto-char (match-end 0))
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (fill-paragraph)
      (forward-line 1)
      (should (py-empty-line-p)))))


(ert-deftest py-ert-moves-up-fill-paragraph-pep-257-4M7aUK ()
  (let ((py-docstring-style 'pep-257))
    (py-test-with-temp-buffer-point-min
	"# r1416

def baz():
    \"\"\"Hello there. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy. This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.

    This is a multiline function definition. Don= 't worry, be happy. Be very very happy. Very. happy.
    \"\"\"
    return 7
"
      (goto-char (point-min))
      (font-lock-fontify-region (point-min)(point-max))
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
      (fill-paragraph)
      (forward-line -1)
      (should (py-empty-line-p))
      )))

;;   \"\"\"Process foo, return bar\.

;;   If processing fails throw ProcessingError\.
;;   \"\"\"
(ert-deftest py-ert-pep-257-mtab2Q ()
  (py-test-with-temp-buffer-point-min
      "class DataFrame(NDFrame, OpsMixin):
    \"\"\"

    index : Index or array-like
        Index to use for resulting frame\. Will default to RangeIndex if
        no indexing information part of input data and no index provided\.
    \"\"\"
    pass"
    (let ((py-docstring-style 'pep-257))
      (search-forward "index")
      (fill-paragraph)
      (goto-char (point-max))
      (search-backward "\"")
      (forward-char 1)
      (should (eolp))
      (forward-line -2)
      (should (empty-line-p)))))

(ert-deftest py-ert-pep-257-9HrXY7 ()
  (py-test-with-temp-buffer
      "a='123'"
    (goto-char (point-max))
    (search-backward "2")
    (py-fill-string)
    (forward-char -2)
    (should (eq (char-after) ?'))
    (beginning-of-buffer)
    (should (eq (char-after) ?a))
    (end-of-buffer)
    (should (eq (char-before) ?'))))

(provide 'py-ert-fill-tests)
;;; py-ert-fill-tests.el ends here
