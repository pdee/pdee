;;; py-ert-navigation-tests-1.el --- python-mode navigation tests  -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'setup-ert-tests)

(ert-deftest py-ert-moves-up-def-or-class-bol-iPn4ge-u4t728 ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-up-def-or-class)
    (should (looking-at "class"))))

(ert-deftest py-ert-moves-up-minor-block-bol-sqyjbT-KaTTq2 ()
  (py-test-with-temp-buffer
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
                'python-expression'])

            if b:
                ar_atpt_python_list_roh = ([
                    'python-expression'])"
    (goto-char (point-max))
    (py-up-minor-block-bol)
    (should (looking-at " +if a:"))))

(ert-deftest py-ert-moves-up-block-bol-u0LDDH-OcJjMV ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-up-block-bol)
    (should (looking-at " +def f():"))))

(ert-deftest py-ert-moves-up-block-w7eExs-Gg9b6O ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-up-block)
    (should  (eq (char-after) ?d))))

(ert-deftest py-ert-moves-backward-block-w7eExs-QrymnI ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-backward-block)
    (should (looking-at "if a:"))))

(ert-deftest py-ert-moves-up-minor-block-bol-2-IrCqCB ()
  (py-test-with-temp-buffer
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
        # if c
            if b:
                pass"
    (goto-char (point-max))
    (py-up-minor-block)
    (should (looking-at "if a:"))))

(ert-deftest py-ert-moves-up-def-bol-wTMxJq-KMclSu ()
  (py-test-with-temp-buffer
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
	    \"\"\""
    (goto-char (point-max))
    (py-up-def-bol)
    (should (looking-at " +def __init__"))))

(ert-deftest py-ert-moves-up-class-bol-hObRZJ-cDUW2n ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (py-up-class)
    (should (bolp))))

(ert-deftest py-ert-moves-up-def-or-class-bol-2-UDtFbh ()
  (py-test-with-temp-buffer
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
            pass"
    (goto-char (point-max))
    (py-up-def-or-class)
    (should (looking-at "class"))))

(ert-deftest py-ert-moves-down-block-bol-1-e6MBka ()
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
    (goto-char (point-min))
    (goto-char 264)
    (py-down-block-bol)
    (should (bolp))))

(ert-deftest py-ert-moves-down-def-bol-1-k3V8r3 ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-down-class-bol-ubMUm6-2BctHX ()
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
"
    (goto-char (point-min))
    (py-down-class-bol)
    (should (eobp))))

(ert-deftest py-ert-moves-down-def-or-class-bol-1-82GNQR ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-or-class-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-moves-down-block-1-wznp1L ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-block)
    (should (looking-at "def"))))

(ert-deftest py-ert-moves-down-block-bol-2-SiVW9F ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-block-bol)
    (should (bolp))
    (should (looking-at " +def"))))

(ert-deftest py-ert-moves-down-minor-block-1-KGpcdA ()
  (py-test-with-temp-buffer
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
        if b:
            pass"
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))
    (search-backward "a:")
    (py-down-minor-block)
    (should (eq (char-after) ?i))))

(ert-deftest py-ert-moves-down-minor-block-bol-1-Ebh0gu ()
  (py-test-with-temp-buffer
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
        if b:
            pass"
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-minor-block-bol)
    (should (bolp))
    (should (looking-at " +if"))))

(ert-deftest py-ert-moves-down-def-1-2Dlxio ()
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
    (goto-char (point-min))
    (search-forward "def __init__")
    (py-down-def)
    (should (looking-at "def f()"))))

(ert-deftest py-ert-moves-down-def-2-on7hei ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-moves-down-class-5kkTKq-AFdn8b ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (should (not (py-down-class)))))

(ert-deftest py-ert-moves-down-def-or-class-1-ym0d05 ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "__init__")
    (py-down-def-or-class)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-moves-backward-statement-bol-Njn9my-q4WaPZ ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (should (eq 332 (py-backward-statement-bol)))))

(ert-deftest py-ert-moves-backward-block-bol-x9If7W-IHEoCT ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (indent-to 12)
    (should (eq 317 (py-backward-block-bol)))))

(ert-deftest py-ert-moves-backward-clause-bol-RpODhD-GshxqN ()
  (py-test-with-temp-buffer
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
                "
    (goto-char (point-max))
    (should (eq 317 (py-backward-clause-bol)))))

(ert-deftest py-ert-moves-backward-block-or-clause-bol-ZDM7aD-8EAgcH ()
  (py-test-with-temp-buffer
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
                                  "
    (goto-char (point-max))
    (should (eq 317 (py-backward-block-or-clause-bol)))))

(ert-deftest py-ert-moves-backward-class-bol-PFB3qC-KulNSA ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (py-backward-class-bol)
    (should (bobp))))

(ert-deftest py-ert-moves-backward-def-or-class-bol-xTvIPv-s3x1zu ()
  (py-test-with-temp-buffer
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
                'python-expression',"
    (goto-char (point-max))
    (py-backward-def-or-class-bol)
    (should (looking-at "^ +def"))))

(ert-deftest py-ert-moves-forward-clause-bol-CylN4m ()
  (py-test-with-temp-buffer
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
"
    (goto-char (point-max))
    (goto-char 576)
    (should (eq 594 (py-forward-clause-bol)))))

(ert-deftest py-ert-moves-forward-block-or-clause-bol-MHBxwf ()
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
"
    (search-forward "elif b:")
    (py-forward-block-or-clause-bol)
    (should (looking-at " +else b:"))))

(ert-deftest py-ert-moves-up-position-tests-12-AJXhT7 ()
  (py-test-with-temp-buffer
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
    (goto-char (point-max))
    (search-backward "self")
    (end-of-line)
    (py-forward-statement)
    (should (eq (char-before) ?\]))))

(ert-deftest py-ert-moves-up-copy-statement-test-UFUch0 ()
  (py-test-with-temp-buffer-point-min
   "from foo.bar.baz import something
"
   (goto-char (point-min))
   (py-copy-statement)
   (should (string-match "from foo.bar.baz import something" (car kill-ring)))))

(ert-deftest py-ert-moves-up-honor-dedent-lp-1280982-K6OICS ()
  (py-test-with-temp-buffer
      "def foo():
    def bar():
        asdf
    "
    (goto-char (point-max))
    (py-newline-and-indent)
    (py-electric-backspace)
    (py-newline-and-indent)
    (should (eq 42 (point)))))

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
      (should (py-empty-line-p))
      )))

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
      (eolp)
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (should (py-empty-line-p)))))

(ert-deftest py-ert-moves-up-fill-paragraph-symmetric-i6vspv ()
  (let ((py-docstring-style 'symmetric))
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
      (eolp)
      (forward-line 1)
      (end-of-line)
      (should (<= (current-column) 72))
      (search-forward "\"\"\"")
      (forward-line -1)
      (should (not (py-empty-line-p))))))

(ert-deftest py-ert-moves-up-forward-expression-test-oDlMW8 ()
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

(ert-deftest py-ert-moves-up-backward-expression-test-Y5KVT1 ()
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
      (goto-char (point-max))
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
      (should (eq (char-after) ?\[))))

(ert-deftest py-ert-moves-up-match-paren-test-2-AdZoji ()
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
    (goto-char (point-max))
    (forward-line -3)
    (indent-to 12)
    (py-match-paren)
    (should (eq (char-after) ?a))))

(ert-deftest py-ert-moves-up-match-paren-test-10-aQtWae ()
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
    (goto-char (point-max))
    (forward-line -3)
    (indent-to 8)
    (py-match-paren)
    (should (eq (char-after) ?e))
    (forward-line 3)
    (should (eolp))))

(ert-deftest py-ert-backward-def-or-class-1-iuGWZ9 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (py-backward-def-or-class)
    (should (char-equal ?d (char-after)))))

(ert-deftest py-ert-backward-def-or-class-2-qWAiN5 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def-or-class)
    (should (char-equal ?d (char-after)))))

(ert-deftest py-ert-backward-def-or-class-3-e6MZx1 ()
  (py-test-with-temp-buffer
      "class _Simple(object):
    # emulate something
    def foo(self, element, tag, namespaces=None):
        pass
    def bar(self, element, tag, namespaces=None):
        return list(self.iterfind(element, tag, namespaces))"
    (goto-char (point-max))
    (search-backward "def" nil t 2)
    (py-backward-def-or-class)
    (should (char-equal ?c (char-after)))))

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
    (py-forward-def-or-class)
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
    (py-forward-def-or-class)
    (should (eobp))))

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

(ert-deftest py-backward-block-test-GccKh6 ()
  (py-test-with-temp-buffer
      "if False:
    print(\"Nein\")
else:
    print(\"Ja\")"
    (goto-char(point-max))
    (py-backward-block)
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
    (goto-char (point-min))
    (search-forward "return answer")
    (py-forward-def-or-class)
    (should (looking-back "self.run(statement)" (line-beginning-position)))))

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

(ert-deftest py-ert-backward-indent-test-hJv5Sb-qSF0Qa ()
  (py-test-with-temp-buffer
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (goto-char (point-max))
    (py-backward-indent)
    (should (eq (char-after) ?a))))

(ert-deftest py-ert-forward-indent-test-1-D3Bcke ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass
    def b(self):
        asdef
        asdf
        pass"
    (goto-char (point-min) )
    (search-forward "sdf")
    (py-forward-indent)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-beginning-of-indent-p-test-AAX9Kh ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (should (not (py--beginning-of-indent-p)))
    (py-backward-indent)
    (should (py--beginning-of-indent-p))))

(ert-deftest py-ert-beginning-of-indent-bol-p-test-TqZeal ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdfasde")
    (should (not (py--beginning-of-indent-bol-p)))
    (beginning-of-line)
    (should (py--beginning-of-indent-bol-p))))

(ert-deftest py-ert-backward-comment-test-OGNbDB ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        # sdfasde
        # sdfasde
        # sdfasde
        print(123)"
    (goto-char (point-min) )
    (search-forward "sdfasde" nil t 3)
    (py-backward-comment)
    (should (eq 43 (point)))))

(ert-deftest py-ert-forward-comment-test-ibueq9-1vqzQE ()
  (py-test-with-temp-buffer-point-min
      "class A(object):
    def a(self):
        # sdfasde
        pass"
    (goto-char (point-min) )
    (search-forward "sdf")
    (py-forward-comment)
    (should (eq (char-before) ?e))))

(ert-deftest py-ert-embedded-def-or-class-test-RrkIDD-Cu8cTW ()
  (py-test-with-temp-buffer
      " class Foo:
    def bar(self):
        print(\"\"\"
This is
a nested
string.
\"\"\")
        return True"
    (goto-char (point-max))
    (py-backward-def-or-class)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-nested-def-lp-1594263-test-3Tbta2 ()
  (py-test-with-temp-buffer
      "def decoratorFunctionWithArguments(arg1, arg2, arg3):
    '''print decorated function call data to stdout.

    Usage:

    @decoratorFunctionWithArguments('arg1', 'arg2')
    def func(a, b, c=True):
   pass
    '''

    def wwrap(f):
        print 'Inside wwrap()'
        def wrapped_f(\*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(\*args)
            print 'After f(\*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (forward-line -1)
    (back-to-indentation)
    (py-backward-def-or-class)
    (should (looking-at "def wwrap"))))

(ert-deftest py-forward-def-or-class-1-cmHY16 ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (search-backward "args)'")
    (py-forward-def-or-class)
    (should (eq (char-before) ?'))
    (py-forward-def-or-class)
    (should (eq (char-before) ?f))))

(ert-deftest py-forward-def-or-class-9oeKIr-jqD5z8 ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (goto-char (point-max))
    (search-backward "'")
    (forward-char 1)
    (py-forward-def-or-class)
    (should (eq (char-before) ?f))))

(ert-deftest py-forward-block-1-Ex3K59 ()
  (py-test-with-temp-buffer-point-min
      "if True:
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
        pass"
    (goto-char (point-min))
    (py-forward-block)
    (should (eobp))))

(ert-deftest py-forward-clause-lp-1630952-1-HFqYAb ()
  (py-test-with-temp-buffer
      "def foo(arg1, arg2, arg3):
    '''print decorated function call data to stdout.
    '''
    def bar(f):
        print 'Inside wwrap()'
        def wrapped_f(*args):
            print 'Inside wrapped_f()'
            print 'Decorator arguments:', arg1, arg2, arg3
            f(*args)
            print 'After f(*args)'
        return wrapped_f
    return wwrap"
    (goto-char (point-max))
    (search-backward "args)'")
    (py-forward-clause)
    (should (eq (char-before) ?'))))

(ert-deftest py-up-block-test-Ek86Xk-GRei3c ()
  (py-test-with-temp-buffer
      "
def foo():
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
        "
    (goto-char (point-max))
    (py-up-block)
    (should (looking-at "if True:"))))

(ert-deftest py-forward-statement-test-3-ealLPf ()
  (py-test-with-temp-buffer-point-min
      "print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

print(\"%(language)s has %(number)03d quote types.\" %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-min) )
    (py-forward-statement)
    (py-forward-statement)
    (should (eobp))))

(ert-deftest py-backward-clause-test-p52Dcj ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (search-backward "elif")
    (skip-chars-backward " \t\r\n\f")
    (py-backward-clause)
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py-backward-clause-test-p52Dcj ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (search-backward "elif")
    (skip-chars-backward " \t\r\n\f")
    (py-backward-clause)
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py-backward-clause-test-hPywHz ()
  (py-test-with-temp-buffer
      "def foo():
    if True:
        def bar():
            pass
    elif False:
        def baz():
            pass"
    (goto-char (point-max))
    (sit-for 0.1)
    (search-backward "elif")
    (py-backward-clause)
    (should (looking-at "if True"))))

(ert-deftest py--beginning-of-assignment-p-test-yJqNFd ()
  (py-test-with-temp-buffer-point-min
      "afd = \"asdf\""
    (goto-char (point-min))
    (search-forward "f")
    (should-not (py--beginning-of-assignment-p))))

(ert-deftest py--beginning-of-assignment-p-test-nnyBdy ()
  (py-test-with-temp-buffer-point-min
      "afd = \"asdf\""
    (goto-char (point-min))
    (should (py--beginning-of-assignment-p))))

(ert-deftest py-backward-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "a, b, c = (1, 2, 3)"
    (goto-char (point-max))
    (should (py-backward-assignment))))

(ert-deftest py-forward-assignment-test-nnyBdy()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-wQIiGk ()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
	(search-forward "b")
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
	(search-backward "2")
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-backward-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "print('%(language)s has %(number)03d quote types\.' %
       {'language': \"Python\", \"number\": 2})"
    (goto-char (point-max))
    (should-not (py-backward-assignment))))

(ert-deftest py-backward-assignment-test-nnyBdy()
  (py-test-with-temp-buffer
      "a, b, c = (1, 2, 3)"
    (goto-char (point-max))
    (should (py-backward-assignment))))

(ert-deftest py-backward-assignment-bol-test-ZZ2Txq()
  (py-test-with-temp-buffer
      "    zeit = kugel.zeit
    ausgabe = kugel.ausgabe"
    (goto-char (point-max))
    (py-backward-assignment-bol)
    (should (eq (char-after) 32))
    (should (eq 23 (point)))
    ))

(ert-deftest py-forward-assignment-test-nnyBdy()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-wQIiGk()
    (py-test-with-temp-buffer-point-min
	"a, b, c = (1, 2, 3)"
	(goto-char (point-min))
	(search-forward "b")
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
	(search-backward "2")
      (py-forward-assignment)
      (should (eq (char-before) ?\)))))

(ert-deftest py-forward-assignment-test-2ptHP0()
    (py-test-with-temp-buffer
	"a, b, c = (1, 2, 3)
asdf = []"
      (goto-char (point-max))
      (py-forward-assignment)
      (should (eq (char-before) ?\]))))

(ert-deftest py-forward-assignment-bol-test-ZZ2Txq()
  (py-test-with-temp-buffer-point-min
      "    zeit = kugel.zeit
    ausgabe = kugel.ausgabe"
    (goto-char (point-min))
    (py-forward-assignment-bol)
    (should (eq (char-after) 32))
    (should (eq 23 (point)))
    ))

(ert-deftest py-forward-assignment-bol-test-DXM7p5()
  (py-test-with-temp-buffer-point-min
      "zeit = kugel.zeit
ausgabe = kugel.ausgabe"
    (goto-char (point-min))
    (py-forward-assignment-bol)
    (should (eq (char-after) ?a))
    ))


(ert-deftest py-forward-def-or-class-test-1-YpTSCo ()
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
    (py-forward-def-or-class)
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
      (should (eq 54 (py-forward-indent)))))

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
    (py-forward-def-or-class)
    (should-not (nth 3 (parse-partial-sexp (point-min) (point))))
    (should (eobp))))

(ert-deftest py-backward-indent-tWBEjf ()
  (py-test-with-temp-buffer
      "except:
    print(\"Setze anzahl auf 1\")
    anzahl = 1"
    (goto-char (point-max))
    (py-backward-indent)
    (should (eq (char-after) ?p))))


(ert-deftest py-backward-def-or-class-text-IZvvZ5 ()
  (py-test-with-temp-buffer
      "  d
  "
    (goto-char (point-max))
    (should (eq nil (py-backward-def-or-class)))))

(ert-deftest py-95-reliability-test-LiHlAP ()
  (py-test-with-temp-buffer
      "__version__  = \"Bla   #: current file version"
    (goto-char (point-max))
    (py-backward-top-level)
    (should (py--top-level-form-p))))


(ert-deftest py-mark-indent-Qd3qoQ ()
  (py-test-with-temp-buffer-point-min
      "if __name__ == \"__main__\":
    main()

try:
    anzahl = int(args\[1])
except:
    print(\"Setze anzahl auf 1\")
    anzahl = 1
"
    (goto-char (point-min))
    (search-forward "main()")
    (beginning-of-line)
    (should (eq 39 (cdr (py-mark-indent))))))

(ert-deftest py-py-block-test-TsUyyc ()
  (py-test-with-temp-buffer-point-min
      "def main():
    if len(sys.argv) == 1:
        usage()
"
    (goto-char (point-min))
    (search-forward "if")
    (should (eq 38 (length (py-block))))))

(ert-deftest py-backward-assignment-test-BU1DTH ()
  (py-test-with-temp-buffer
      "try:
    anzahl = int(args[1])
"
    (goto-char (point-max))
    (py-backward-assignment)
    (should (eq 4 (current-column)))))

(ert-deftest py-backward-assignment-bol-test-BU1DTH ()
  (py-test-with-temp-buffer
      "try:
    anzahl = int(args[1])
"
    (goto-char (point-max))
    (py-backward-assignment-bol)
    (should (bolp))))

(ert-deftest py-forward-def-or-class-test-3JzvVW ()
  (py-test-with-temp-buffer-point-min
      "class kugel(object):
    pass

    def pylauf(self):
        return treffer
#        print(\"len(spiel): %s \" % len(spiel))

zeit = kugel\.zeit
ausgabe = kugel\.ausgabe
spiel = kugel\.spiel

# with file(\"roulette-\" + zeit + \"\.csv\", 'w') as datei:
#     for i in range(anzahl):
#         klauf\.pylauf()
#         datei\.write(str(spiel\[i]) + \" \")

#     datei\.write(\"treffer\; schwarz\; gruen\; rot\; pair\; impair\; passe\; manque\; spiel\")
#     pri

''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
'''

asd = 'asdf asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf'

afd = \"asdf asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf\"

a, b, c = (1, 2, 3)
a = b = c = 5
foo: int = 1

weight = 23
print(\"\\nFinal Weight: {}\"\.format(weight))

print(\"%(language)s has %(number)03d quote types\.\" %
       {'language': \"Python\", \"number\": 2})

def example3():
    list(map(lambda tpl: print(f\"{tpl\[0]} {tpl\[1]}\"), enumerate(\[\"yellow\",\"blue\",\"red\"])))"
    (goto-char (point-min))
    (search-forward "return treffer")
    (py-forward-def-or-class)
    (should (looking-back "red\"\]\)\)\)" (line-beginning-position)))
    ))

(ert-deftest py-forward-statement-test-3JzvVW ()
  (py-test-with-temp-buffer-point-min
      "def upload_to_s3(file_name):

    # Instanstiate
    s3_hook=S3Hook(aws_conn_id=S3_CONN_ID)

    # Create file
    sample_file = \"{0}_file_{1}\.txt\"\.format(name, file_name) #swap your name here
    example_file = open(sample_file, \"w+\")"
    (goto-char (point-min))
    (search-forward "CONN_ID)") 
    (py-forward-statement)
    (should (looking-back "file_name)" (line-beginning-position)))))


(provide 'py-ert-navigation-tests-1)
;;; py-ert-navigation-tests-1.el ends here
