;;; py-ert-forward-tests.el --- Just some more tests

;; URL: https://gitlab.com/python-mode-devs
;; Keywords: languages, convenience

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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:


;;This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

(require 'py-setup-ert-tests)

(ert-deftest py-ert-forward-block-test ()
  (py-test-with-temp-buffer-point-min
      "if foo:
    for a in b:
        print('%(language)s has %(number)03d quote types.' %
   {'language': \"Python\", \"number\": 2})

elif bar:
    for a in b:
        pass
else:
    for a in b:
        pass
"
    (goto-char (point-min))
    (let (py-font-lock-defaults-p)
      (py-forward-block)
      (should (eq (char-before) ?s)))))

(ert-deftest py-ert-forward-block-or-clause-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "elif")
    (py-forward-block-or-clause)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-class-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \\\"Python\\\", \\\"number\\\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-class)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-clause-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "elif")
    (py-forward-clause)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-def-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-def)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-def-or-class-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-def-or-class)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-elif-block-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "elif")
    (py-forward-elif-block)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-else-block-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "else")
    (py-forward-else-block)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-except-block-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "except")
    (py-forward-except-block)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-expression-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-expression)
    (should (eq (char-before) ?:))))

(ert-deftest py-ert-forward-for-block-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "for")
    (py-forward-for-block)
    (should (eq (char-before) ?\)))))

(ert-deftest py-ert-forward-if-block-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-if-block)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-minor-block-test-0nXANF ()
  (py-test-with-temp-buffer
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (goto-char (point-min))
    (search-forward "if")
    (py-forward-minor-block)
    (should (eq (char-before) ?s))))

(ert-deftest py-ert-forward-minor-block-test-JNJyc4 ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (goto-char (point-min))
    (search-forward "try:")
    (py-forward-minor-block)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-partial-expression-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "Python")
    (py-forward-partial-expression)
    (should (eq (char-before) ?n))))

(ert-deftest py-ert-forward-section-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-section)
    (should (eq (char-before) ?}))))

(ert-deftest py-ert-forward-statement-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-statement)
    (should (eq (char-before) ?:))))

(ert-deftest py-ert-forward-top-level-test ()
  (py-test-with-temp-buffer-point-min
      "# {{
class bar:
    def foo ():
        try:
            if foo:
                for a in b:
                    print('%(language)s has %(number)03d quote types.' %
       {'language': \"Python\", \"number\": 2})

            elif bar:
                for a in b:
                    pass
            else:
                for a in b:
                    pass
# }}
        except:
            block2
"
    (search-forward "if")
    (py-forward-top-level)
    (should (eq (char-before) ?2))))

(ert-deftest py-ert-forward-try-block-test ()
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
                        print(\"asdf\")
        else True:
            pass
        finally:
            pass
"
    (goto-char (point-min))
    (search-forward "try")
    (py-forward-try-block)
    (should (looking-back "pass" (line-beginning-position)))))

(ert-deftest py-ert-forward-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-block-bol)
    (should (looking-at " +except:"))))

(ert-deftest py-ert-forward-clause-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-clause-bol)
    (should (bolp))))

(ert-deftest py-ert-forward-block-or-clause-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-block-or-clause-bol)
    (should (looking-at " +except:"))))

(ert-deftest py-ert-forward-def-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-def-bol)
    (should (eobp))))

(ert-deftest py-ert-forward-class-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-class-bol)
    (should (eobp))))

(ert-deftest py-ert-forward-def-or-class-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-def-or-class-bol)
    (should (eobp))))

(ert-deftest py-ert-forward-if-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-if-block-bol)
    (should (looking-at " +except:"))))

(ert-deftest py-ert-forward-try-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-try-block-bol)
    (should (eobp))))

(ert-deftest py-ert-forward-minor-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-minor-block-bol)
    (should (looking-at " +except:"))))


(ert-deftest py-ert-forward-for-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-for-block-bol)
    (should (looking-at " +except:"))))

(ert-deftest py-ert-forward-top-level-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
        except:
            block2
"
    (goto-char 103)
    (py-forward-top-level-bol)
    (should (eobp))))

(ert-deftest py-ert-forward-statement-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in b:
                    pass
"
    (goto-char 103)
    (py-forward-statement-bol)
    (should (looking-at " +pass"))))

(ert-deftest py-ert-forward-statement-test-1 ()
  (py-test-with-temp-buffer-point-min
	"def wwwwww_wwwwwww(self, text):
		# wwwwww www wwww wwww wwwwwww, w.w. <!-- wwwwww wwwwwwwwww wwww wwww -->
		# wwwwwwwwwww www wwwwwwww wwwwwww.
		self.wwwwww.append(\"<!--%(text)s-->\" % locals())"
    (end-of-line)
    (py-forward-statement)
    (should (eq (char-before) ?\)))))

(provide 'py-ert-forward-tests)
;;; py-ert-forward-tests.el ends here
