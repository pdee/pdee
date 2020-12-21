;;; py-ert-font-lock-test-1.el --- py-execute-region tests

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
(ert-deftest py-assignments-with-type-hints-test-QLpglI ()
  (py-test-with-temp-buffer
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
    (font-lock-fontify-buffer)
    (goto-char (point-max))
    (search-backward "Sequence" nil nil 2)
    (should (eq (face-at-point) nil))
    (search-backward "var")
    (should (face-equal (face-at-point) 'py-variable-name-face))
    (search-backward "Mapping" nil nil 2)
    (should (eq (face-at-point) nil))
    (search-backward "var")
    (should (face-equal (face-at-point) 'py-variable-name-face))
    ))

(ert-deftest py-syntax-highlighting-for-builtin-functions-55-test-qijqlm ()
  (py-test-with-temp-buffer
      "range(len(list((1, 2, 3))))"
    (goto-char (point-max))
    ;; (goto-char (point-max))
    (font-lock-fontify-region (point-min) (point-max))
    (sit-for 0.1)
    (search-backward "le")
    (should (face-equal (face-at-point) 'py-builtins-face))))

(ert-deftest py-ert-exception-name-face-lp-1294742-7hEOh9 ()
  (py-test-with-temp-buffer
      "ArithmeticError"
    (font-lock-fontify-region (point-min) (point-max))
    (goto-char (point-max))
    (sit-for 0.1)
    (forward-char -1)
    (should (face-equal 'py-exception-name-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-keyword-face-lp-1294742-N1cjiI ()
  (py-test-with-temp-buffer-point-min
      " and as assert break continue del elif else except exec finally for global if in is lambda not or pass raise return while with yield"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (face-equal 'font-lock-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))


(ert-deftest py-ert-builtins-face-lp-1294742-zvZYG5 ()
  (py-test-with-temp-buffer-point-min
      "_ __doc__ __import__ __name__ __package__ abs all any apply basestring bin bool buffer bytearray bytes callable chr classmethod cmp coerce compile complex delattr dict dir divmod enumerate eval execfile file filter float format frozenset getattr globals hasattr hash help hex id input int intern isinstance issubclass iter len list locals long map max min next object oct open ord pow print property range raw_input reduce reload repr reversed round set setattr slice sorted staticmethod str sum super tuple type unichr unicode vars xrange zip"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (should (eq 'py-builtins-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-pseudo-keyword-face-lp-1294742-KgocNc ()
  (py-test-with-temp-buffer-point-min
      "  Ellipsis True False None  __debug__ NotImplemented"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min)(point-max))
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-pseudo-keyword-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(ert-deftest py-ert-object-reference-face-lp-1294742-HCkKIc ()
  (py-test-with-temp-buffer-point-min
      "self cls"
    (goto-char (point-min))
    (font-lock-fontify-region (point-min) (point-max))
    (should (eq 'py-object-reference-face (get-char-property (point) 'face)))))

(provide 'py-ert-font-lock-test-1)
;;; py-ert-font-lock-test-1.el here
