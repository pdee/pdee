;;; py-ert-font-lock-test.el --- py-execute-region tests -*- lexical-binding: t; -*-

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

;;; Code:

;; borrowed from bug#44568: [PATCH] Fix font lock of assignments with
;; type hints in Python

(require 'py-setup-ert-tests)

(ert-deftest py-assignments-with-type-hints-test-bug44568-QLpglI ()
  (py-test
   "var1: int = 5
var2: Mapping[int, int] = {10: 1024}
var3: Mapping[Tuple[int, int], int] = {(2, 5): 32}
var4: Sequence[Sequence[int]] = [[1], [1, 2], [1, 2, 3]]
var5: Sequence[Mapping[str, Sequence[str]]] = [
    {
	'red': ['scarlet', 'vermilion', 'ruby'],
	'green': ['emerald', 'aqua']
    },
    {
	'sword': ['cutlass', 'rapier']
    }
]"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "Sequence" nil nil 2)
   (should (eq (face-at-point) nil))
   (search-backward "var")
   (should (face-equal (face-at-point) 'py-variable-name-face))
   (search-backward "Mapping" nil nil 2)
   (should (eq (face-at-point) nil))
   (search-backward "var")
   (should (face-equal (face-at-point) 'py-variable-name-face))))

(ert-deftest py-syntax-highlighting-for-builtin-functions-55-test-qijqlm ()
  (py-test
   "range(len(list((1, 2, 3))))"
   'python-mode
   'py-verbose-p
   (font-lock-fontify-region (point-min) (point-max))
   ;; (sit-for 0.1)
   (search-forward "l" )
   (should (face-equal (face-at-point) 'py-builtins-face))))

(ert-deftest py-ert-exception-name-face-lp-1294742-7hEOh9 ()
  (py-test
   "ArithmeticError"
   'python-mode
   'py-verbose-p
   (font-lock-fontify-region (point-min) (point-max))
   (goto-char (point-max))
   (sit-for 0.1)
   (forward-char -1)
   (should (face-equal 'py-exception-name-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-keyword-face-lp-1294742-N1cjiI ()
  (py-test-point-min
   " and as assert break continue del elif else except exec finally for global if in is lambda not or pass raise return while with yield"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (font-lock-fontify-region (point-min)(point-max))
   (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
     (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
     (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-builtins-face-lp-1294742-zvZYG5 ()
  (py-test-point-min
   "_ __doc__ __import__ __name__ __package__ abs all any apply basestring bin bool buffer bytearray bytes callable chr classmethod cmp coerce compile complex delattr dict dir divmod enumerate eval execfile file filter float format frozenset getattr globals hasattr hash help hex id input int intern isinstance issubclass iter len list locals long map max min next object oct open ord pow print property range raw_input reduce reload repr reversed round set setattr slice sorted staticmethod str sum super tuple type unichr unicode vars xrange zip"
   'python-mode
   'py-verbose-p
   (font-lock-fontify-region (point-min)(point-max))
   (goto-char (point-min))
   (should (eq (get-char-property (point) 'face) 'py-builtins-face))))

(ert-deftest py-ert-pseudo-keyword-face-lp-1294742-KgocNc ()
  (py-test-point-min
   "  Ellipsis True False None __debug__ NotImplemented"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (font-lock-fontify-region (point-min) (point-max))
   (sit-for 0.2)
   (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
     (should (eq 'py-pseudo-keyword-face (get-char-property (point) 'face)))
     (skip-chars-forward "^ \n"))))

    ;; (should (eq (get-char-property (point) 'face) 'py-pseudo-keyword-face))
(ert-deftest py-ert-object-reference-face-lp-1294742-HCkKIc ()
  (py-test-point-min
   "self cls"
   'python-mode
   'py-verbose-p
   (goto-char (point-min))
   (font-lock-fontify-region (point-min) (point-max))
   (should (eq 'py-object-reference-face (get-char-property (point) 'face)))))

(ert-deftest bug46233-assignment-test-f6YGZs ()
  (py-test
   "def foo(bar: int) -> str:
    spam = bar
    eggs = f'lkjahsd {spam}'
    return eggs
def foo2(bar: int):
    spam = bar
    eggs = f'lkjahsd {spam}'
    return eggs
def foo3(bar):
    spam = bar
    eggs = f'lkjahsd {spam}'
    return eggs
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (font-lock-ensure)
   (search-backward "spam" nil t 2)
   (should (eq (face-at-point) 'py-variable-name-face))
   (search-backward "spam" nil t 2)
   (should (eq (face-at-point) 'py-variable-name-face))
   (search-backward "spam" nil t 2)
   (should (eq (face-at-point) 'py-variable-name-face))))

(ert-deftest bug46233-font-lock-assignment-test-UAIyOJ ()
  (py-test
   "class Student:
    \"\"\"Described a student\.
    Attributes
    ----------
    name : str
        Full name
    idnum : int
        Identification number
    \"\"\"
    name: str = \"\"   ## <--- 'name' is not given the variable face (keeps default)
    idnum: int = 0
"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "idnum")
   (should (eq (face-at-point) 'py-variable-name-face))
   (search-backward "name" nil t 2)
   (should (eq (face-at-point) 'py-variable-name-face))))

;; (ert-deftest py-110-Support-PEP-634-structural-pattern-matching-P6QZmU ()
;;   (py-test
;;       "def sort(seq):
;;     match seq:
;;         case [] | [_]:
;;             return seq"
;;     (goto-char (point-max))
;;     (search-backward "case")
;;     (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
;;     (search-backward "match")
;;     (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))))

(ert-deftest py-110-Support-PEP-634-structural-pattern-matching-s0UJcE ()
  (py-test
   "def simplify_expr(tokens):
    match tokens:
        case \[('('|'\[') as l, *expr, (')'|']') as r] if (l+r) in ('()', '\[]'):
            return simplify_expr(expr)
        case \[0, ('+'|'-') as op, right]:
            return UnaryOp(op, right)
        case \[(int() | float() as left) | Num(left), '+', (int() | float() as right) | Num(right)]:
            return Num(left + right)
        case \[(int() | float()) as value]:
            return Num(value)"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (font-lock-ensure)
   (search-backward "return")
   (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
   (search-backward "as")
   (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
   (search-backward "case")
   (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
   (search-backward "case")
   (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
   (search-backward "match")
   (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))))

(ert-deftest py-keywords-in-identifiers-highlighted-incorrectly-lp-888338-test ()
  (py-test
   "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
def possibly_break():
    pass"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "bly")
   (font-lock-ensure)
   (sit-for 1)
   (should (eq (get-char-property (point) 'face) 'py-def-face))))

(ert-deftest py-invalid-class-function-names-test-Lie51R ()
  (py-test
   "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
def 1possibly_break():
    pass"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (search-backward "bly")
   (should-not (eq (get-char-property (point) 'face) 'py-def-face))))

(ert-deftest py-variable-name-face-test-Lie51R ()
  (py-test
   "[a, b, c] = 1, 2, 3
a, *b, c = range(10)
inst.a, inst.b, inst.c = 'foo', 'bar', 'baz'
(a, b, *c, d) = x, *y = 5, 6, 7, 8, 9
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (font-lock-ensure)
   (sit-for 0.1)
   (search-backward "d")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "c")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "b")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "a")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "c")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "b")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "a")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "c")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "b")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "a")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "c")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "b")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "a")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-variable-name-face-test-FFVt1i ()
  (py-test
   "weight = 23"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (sit-for 0.1)
   (goto-char (point-max))
   (search-backward "w")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-variable-name-face-test-bxtv3F ()
  (py-test
   "class M:
    def __init__(self):
        \"\"\"asdf\"\"\"
        self.a = 1
        self.b = 2"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (sit-for 0.1)
   (goto-char (point-max))
   (search-backward "b")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "a")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-variable-name-face-test-4wdYit ()
  (py-test
   "[a, b, c] = 1, 2, 3
a, *b, c = range(10)
inst.a, inst.b, inst.c = 'foo', 'bar', 'baz'
(a, b, *c, d) = x, *y = 5, 6, 7, 8, 9
"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (font-lock-ensure)
   (sit-for 0.1)
   (search-backward "y")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "x")
   (sit-for 0.1)
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-142-variable-name-face-test-hOUm6Y ()
  (py-test
   "while foo <= 0:
    foo = float(input(\"You can't play for free! Buy some chips: $\"))"
   'python-mode
   'py-verbose-p
   (goto-char (point-max))
   (font-lock-ensure)
   (sit-for 0.1)
   (search-backward "foo")
   (should (eq (get-char-property (point) 'face) 'py-variable-name-face))
   (search-backward "0")
   (should-not (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-142-variable-name-face-test-PkoU08 ()
  (py-test
   "while bet > money:
        bet = float(
            input(f\"Insufficient funds! Maximum bet allowed ${money}\\nPlace your bet $\")
        )"
   'python-mode
   'py-verbose-p
   (font-lock-ensure)
   (goto-char (point-max))
   (search-backward "money:")
   (sit-for 0.1)
   (should-not (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(ert-deftest py-149-font-lock-test-TzOTcJ ()
  (let ((py-underscore-word-syntax-p t)
        (py-python-edit-version "python2"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "print(\"asdf\")"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "print")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'font-lock-keyword-face)))))

(ert-deftest py-149-font-lock-test-zS5Cyr ()
  (let ((py-underscore-word-syntax-p t)
        (py-python-edit-version "python3"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "print(\"asdf\")"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "print")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-builtins-face)))))

(ert-deftest py-149-font-lock-test-FiJp14 ()
  (let ((py-underscore-word-syntax-p nil)
        (py-python-edit-version "python2"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "print(\"asdf\")"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "print")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'font-lock-keyword-face)))))

(ert-deftest py-149-font-lock-test-NuMpks ()
  (let ((py-underscore-word-syntax-p nil)
        (py-python-edit-version "python3"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "print(\"asdf\")"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "print")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-builtins-face)))))

(ert-deftest py-149-font-lock-test-wbAtE2 ()
  (let ((py-underscore-word-syntax-p t)
        (py-python-edit-version "python2"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "def print_this_file(): pass"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "_this_")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-def-face)))))

(ert-deftest py-149-font-lock-test-WuAgdG ()
  (let ((py-underscore-word-syntax-p t)
        (py-python-edit-version "python3"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "def print_this_file(): pass"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "_this_")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-def-face)))))

(ert-deftest py-149-font-lock-test-owsRo3 ()
  (let ((py-underscore-word-syntax-p nil)
        (py-python-edit-version "python2"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "def print_this_file(): pass"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "_this_")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-def-face)))))

(ert-deftest py-149-font-lock-test-ksGdVu ()
  (let ((py-underscore-word-syntax-p nil)
        (py-python-edit-version "python3"
                                'python-mode
                                'py-verbose-p
                                ))
    (py-test
     "def print_this_file(): pass"
     (font-lock-ensure)
     (goto-char (point-max))
     (search-backward "_this_")
     (sit-for 0.1)
     (when py-debug-p
       (message "py-python-edit-version: %s" py-python-edit-version)
       (message "py-underscore-word-syntax-p: %s" py-underscore-word-syntax-p))
     (should (eq (get-char-property (point) 'face) 'py-def-face)))))


(ert-deftest py-ert-flexible-indentation-lp-328842-test-eBQAKK ()
  (py-test-point-min
      "(along, sequence, of_items,
 that, needs, to_be, wrapped) = input_list"
    'python-mode
    'py-verbose-p
    (goto-char (point-min))
    (forward-char 1)
    (should (eq (get-char-property (point) 'face) 'py-variable-name-face))))

(provide 'py-ert-font-lock-test)
;;; py-ert-font-lock-test.el here
