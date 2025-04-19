;;; py-ert-scope-tests.el --- testing python-mode.el  -*- lexical-binding: t; -*-

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

(require 'py-setup-ert-tests)

(ert-deftest py-partial-expression-test-2JmcBn ()
  (py-test-point-min
   "foo=1"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (and (should (string= "foo" (py-partial-expression)))
	(py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-partial-expression-test-yS2wLf ()
  (py-test-point-min
   "print(root.getchildren()[0])"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (search-forward "getchildren")
   (and (should (string= "getchildren()[0]" (py-partial-expression)))
	(py-kill-buffer-unconditional (current-buffer)))))

(ert-deftest py-partial-expression-test-TmqVoM ()
  (py-test-point-min
   "print(root.getchildren()[0])"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (search-forward "ro")
   (and (should (string= "root" (py-partial-expression)))
	(py-kill-buffer-unconditional (current-buffer)))))


(ert-deftest py-partial-expression-test-HS6qOA ()
  (py-test
   "def __init__(self):"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "_")
   (should (string= "__init__" (py-partial-expression)))))

(ert-deftest py-ert-which-def-or-class-test-1-0u94OU ()
  (py-test-point-min
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
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (search-forward "kugel")
   (should (string-match "kugel" (py-which-def-or-class)))
   (search-forward "pylauf")
   (should (string-match "kugel.pylauf" (py-which-def-or-class)))))

(ert-deftest py-ert-which-def-or-class-test-2-8ivbIN ()
  (py-test
   "except AttributeError:

    # To fix reloading, force it to create a new foo
    if hasattr(threading.currentThread(), '__decimal_foo__'):
        del threading.currentThread().__decimal_foo__

    def setfoo(foo):
        \"\"\"Set this thread's foo to foo.\"\"\"
        if foo in (DefaultContext, BasicContext, ExtendedContext):
            foo = foo.copy()
            foo.clear_flags()
        threading.currentThread().__decimal_foo__ = foo

    def getfoo():
        \"\"\"Returns this thread's foo.

        If this thread does not yet have a foo, returns
        \"\"\"
        try:
            return threading.currentThread().__decimal_foo__
        except AttributeError:
            foo = Context()
            threading.currentThread().__decimal_foo__ = foo
            return foo

else:
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (should (string= "???" (py-which-def-or-class)))
   (forward-line -3)
   (should (string= "getfoo" (py-which-def-or-class)))))

(ert-deftest py-ert-which-def-or-class-test-3-UXT0vG ()
  (py-test

   "class kugel(object):
    zeit = time.strftime('%Y%m%d--%H-%M-%S')
    # zeit = time.strftime('%Y-%m-%d--%H-%M-%S')
    spiel = []
    gruen = [0]
    rot = [1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36]
    schwarz = [2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35]
    ausgabe = []
    treffer = None
    fertig = ''
    treffer = random.randint(0, 36)

    def foo():
        bar

    def pylauf(self):
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-line -2)
   (should (string= "kugel.foo" (py-which-def-or-class)))))

(ert-deftest py-ert-match-paren-test-1-aUZjkz ()
  (py-test
   "if __name__ == \"__main__\":
    main()"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-char -1)
   (py-match-paren)
   (should (eq (char-after) ?\())))

(ert-deftest py-ert-match-paren-test-2-Y12s7r ()
  (py-test
   "if __name__ == \"__main__\":
    main()"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-char -2)
   (py-match-paren)
   (should (eq (char-after) ?\)))))

(ert-deftest py-ert-match-paren-test-4-yuGPRk ()
  (py-test
   "if __name__ == \"__main__\":
    main()
    "
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-match-paren)
   (should (eq (char-after) ?m))))

(ert-deftest py-ert-match-paren-test-5-Etk4zd ()
  (py-test-point-min
   "if __name__ == \"__main__\":
    main()
    "
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (py-match-paren)
   (should (py-empty-line-p))
   (py-match-paren)
   (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-7-27w0f6 ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (skip-chars-backward "^\]")
   (forward-char -1)
   (py-match-paren)
   (should (eq (char-after) ?\[))
   (py-match-paren)
   (should (eq (char-after) ?\]))))

(ert-deftest py-ert-match-paren-test-8-qsfcTY ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (skip-chars-backward "^:")
   (py-match-paren)
   (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-9-SEatuR ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "pylauf")
   (py-match-paren)
   (should (eq (char-after) ?\"))
   (py-match-paren)
   (should (eq (char-after) ?\"))))

(ert-deftest py-ert-match-paren-test-faMqA3-A1WQ3J ()
  (py-test
   "def main():
    if len(sys.argv) == 1:
        usage()
        sys.exit()
              "
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "if")
   (py-match-paren)
   (should (py-empty-line-p))
   (py-match-paren)
   (should (eq (char-after) ?i))))

(ert-deftest py-ert-match-paren-test-AOADGb-cVptAC ()
  (py-test-point-min
   "import re
import sys
import os
"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (py-match-paren)
   (should (looking-at "import sys"))
   (setq last-command 'py-match-paren)
   (py-match-paren)
   (should (looking-at "import re"))))

(ert-deftest py-ert-match-paren-nonempty-test-6-SA6Izy ()
  (py-test
   "def main():
    if len(sys.argv) == 1:
        usage()
        sys.exit()

    class asdf(object):
        zeit = time.strftime('%Y%m%d--%H-%M-%S')

        def Utf8_Exists(filename):
            return os.path.exists(filename.encode('utf-8'))
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "class")
   (py-match-paren)
   (should (py-empty-line-p))
   (should (eq 4 (current-column)))))

(ert-deftest py-ert-match-paren-nonempty-test-7-i8ISwu ()
  (py-test
   "try:
    anzahl = int(args[1])
except:
    print \"Setze anzahl auf 1\"
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "arg")
   (py-match-paren)
   (should (eq (char-after) ?\())))

(ert-deftest py-ert-match-paren-nonempty-test-8-4Q5jvq ()
  (py-test
   "try:
    anzahl = int(args[1])
except:
    print \"Setze anzahl auf 1\"
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward " int")
   (py-match-paren)
   (should (eq (char-after) ?a))
   (py-match-paren)
   (should (eq (char-before) 32))
   (should (py-empty-line-p))
   (should (eq 4 (current-column)))))

(ert-deftest py-ert-match-paren-test-9-ym4nrm ()
  (py-test
   "if __name__ == \"__main__\":
    main()
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-match-paren)
   (should (eq (char-after) ?i))))

(ert-deftest py-ert-narrow-to-block-test-uDQtR1-tqncYG ()
  (py-test
   "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
                    "
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (skip-chars-backward " \t\r\n\f")
   (py-narrow-to-block)
   (should (< (length (buffer-substring-no-properties (point-min)(point-max))) 50))))

(ert-deftest py-ert-narrow-to-block-test-xnEs46-GPBOHw ()
  (py-test
   "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
        "
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (skip-chars-backward " \t\r\n\f")
   (py-narrow-to-block)
   (should (< (length (buffer-substring-no-properties (point-min)(point-max))) 50))))

(ert-deftest py-ert-narrow-to-block-or-clause-test-43VsYV ()
  (py-test
   "if treffer in gruen:
    # print \"0, Gruen\"
    ausgabe[1] = treffer
    ausgabe[2] = treffer

elif treffer in schwarz:
    # print \"%i, Schwarz\" % (treffer)
    ausgabe[1] = treffer
"
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (skip-chars-backward " \t\r\n\f")
   (py-narrow-to-block-or-clause)
   (should (eq 87 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-clause-test-rHLyyW ()
  (py-test
   "if treffer in gruen:
    # print \"0, Gruen\"
    ausgabe[1] = treffer
    ausgabe[2] = treffer

elif treffer in schwarz:
    # print \"%i, Schwarz\" % (treffer)
    ausgabe[1] = treffer
"
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (py-narrow-to-clause)
   (should (eq 87 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-class-test-MNaZDI ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (search-backward "treffer")
   (py-narrow-to-class)
   (should (eq 710 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-def-test-wGwY45 ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (search-backward "treffer")
   (py-narrow-to-def)
   (sit-for 1) 
   (should (< 477 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-def-or-class-test-46QGK4 ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (search-backward "treffer")
   (py-narrow-to-def-or-class)
   (should (< 480 (length (buffer-substring-no-properties (point-min)(point-max)))))
   (should (> 490 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-narrow-to-statement-test-7WyEtz ()
  (py-test
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
   'python-mode
   'py-verbose-p
   (goto-char(point-max))
   (search-backward "treffer")
   (py-narrow-to-statement)
   (should (eq 32 (length (buffer-substring-no-properties (point-min)(point-max)))))))

(ert-deftest py-ert-bracket-closing-1-4wPEHo ()
  ""
  'python-mode
  'py-verbose-p
  (py-test
   "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (beginning-of-line)
   (let ((py-closing-list-dedents-bos t))
     (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-2-Ef3fSe ()
  ""
  'python-mode
  'py-verbose-p
  (py-test
   "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (forward-char -1)
   (let ((py-closing-list-dedents-bos t))
     (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-3-4Q5V34 ()
  ""
  'python-mode
  'py-verbose-p
  (py-test
   "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (let ((py-closing-list-dedents-bos t))
     (should (eq 0 (py-compute-indentation))))))

(ert-deftest py-ert-bracket-closing-4-q2feIY ()
  ""
  'python-mode
  'py-verbose-p
  (py-test
   "
my_list = [
    1, 2, 3,
    4, 5, 6,
    ]"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (beginning-of-line)
   (let ((py-closing-list-dedents-bos nil)
         (py-indent-list-style 'one-level-to-beginning-of-statement))
     (should (eq 4 (py-compute-indentation))))))

(ert-deftest py-ert-multiple-decorators-test-1-KyE0zL ()
  (py-test
   "@blah
@blub
def foo():
    pass
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (let ((py-mark-decorators t))
     (py-backward-def-or-class)
     (should (bobp)))))

(ert-deftest py-ert-multiple-decorators-test-2-D9kV8N ()
  (py-test
   "@blah
@blub
def foo():
    pass
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (let* (py-mark-decorators
          (erg (py-backward-def-or-class)))
     (should (eq 13 erg)))))

(ert-deftest py-ert-async-backward-block-test-OdiTDQ ()
  (py-test
   "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-backward-block)
   (should (looking-at "async with"))))

(ert-deftest py-ert-async-backward-def-test-lF1w7S ()
  (py-test
   "async def coro(name, lock):
    print('coro {}: waiting for lock'.format(name))
    async with lock:
        print('coro {}: holding the lock'.format(name))
        await asyncio.sleep(1)
        print('coro {}: releasing the lock'.format(name))"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (py-backward-def)
   (should (looking-at "async def"))))

(provide 'py-ert-scope-tests)
;;; py-ert-scope-tests.el ends here
