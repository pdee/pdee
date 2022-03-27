;;; py-ert-beginning-tests.el --- Just some more tests

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

(require 'py-setup-ert-tests)

(ert-deftest py-ert-beginning-of-block-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-block)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-clause-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-clause)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-block-or-clause-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-block-or-clause)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-def-test ()
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
    (goto-char (point-max))
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-beginning-of-class-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-class)
    (should (eq (char-after) ?c))))

(ert-deftest py-ert-beginning-of-def-or-class-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def-or-class)
    (should (eq (char-after) ?d))))

(ert-deftest py-ert-beginning-of-if-block-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-if-block)
    (should (eq (char-after) ?i))))

(ert-deftest py-ert-beginning-of-try-block-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-try-block)
    (should (eq (char-after) ?t))))

(ert-deftest py-ert-beginning-of-minor-block-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-minor-block)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-for-block-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-for-block)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-top-level-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-top-level)
    (should (eq (char-after) ?c))))

(ert-deftest py-ert-beginning-of-statement-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-statement)
    (should (eq (char-after) ?f))))

(ert-deftest py-ert-beginning-of-expression-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-expression)
    (should (eq (char-after) ?r))))

(ert-deftest py-ert-beginning-of-expression-test-XKIlTr ()
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
    (goto-char (point-max))
    (search-backward "ange")
    (should-not (py--beginning-of-expression-p))))

(ert-deftest py-ert-beginning-of-expression-test-OmllRc ()
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
    (goto-char (point-max))
    (search-backward "range")
    (should (py--beginning-of-expression-p))))

(ert-deftest py-ert-beginning-of-partial-expression-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-partial-expression)
    (should (eq (char-after) ?r))))

(ert-deftest py-ert-beginning-of-block-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-block-bol)
    (should (eq (char-after) ?\ ))))

(ert-deftest py-ert-beginning-of-clause-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-clause-bol)
    (should (eq (char-after) ?\ ))))

(ert-deftest py-ert-beginning-of-block-or-clause-bol-test-6ruqeq ()
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
    (goto-char (point-max))
    ;; (skip-chars-backward " \t\r\n\f")
    (py-backward-block-or-clause-bol)
    (should (looking-at "class"))))

(ert-deftest py-ert-beginning-of-block-or-clause-bol-test-s19jSv ()
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
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (py-backward-block-or-clause-bol)
    (should (looking-at " +except"))))

(ert-deftest py-ert-beginning-of-def-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def-bol)
    (should (eq (char-after) ?\ ))))

(ert-deftest py-ert-beginning-of-class-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-class-bol)
    (should (eq (char-after) ?c))))

(ert-deftest py-ert-beginning-of-def-or-class-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-def-or-class-bol)
    (should (looking-at " +def"))))

(ert-deftest py-ert-beginning-of-if-block-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-if-block-bol)
    (should (looking-at " +if"))))

(ert-deftest py-ert-beginning-of-try-block-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-try-block-bol)
    (should (looking-at " +try"))))

(ert-deftest py-ert-beginning-of-minor-block-bol-test ()
  (py-test-with-temp-buffer
      "# -*- coding: utf-8 -*-
class bar:
    def foo ():
        try:
            if True:
                for a in range(anzahl):
                    pass"
    (goto-char (point-max))
    (py-backward-minor-block-bol)
    (should (looking-at " +for"))))

(ert-deftest py-ert-beginning-of-for-block-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-for-block-bol)
    (should (eq (char-after) ?\ ))))

(ert-deftest py-ert-beginning-of-statement-bol-test ()
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
    (goto-char (point-max))
    (search-backward "pass")
    (py-backward-statement-bol)
    (should (bolp))))

(provide 'py-ert-beginning-tests)
;;; py-ert-beginning-tests.el ends here
