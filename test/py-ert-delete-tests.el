;; py-ert-delete-tests.el --- testing python-mode.el -*- lexical-binding: t; -*-

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

(ert-deftest py-ert-electric-kill-backward-arg-test-b118-yQx574 ()
  (py-test
   "asdf    "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (py-electric-backspace)
   (should (eq ?f (char-before)))))

(ert-deftest py-ert-electric-kill-backward-arg-test-b118-uWff3u ()
  (py-test
   "asdf"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (py-electric-backspace)
   (should (eq (char-before) ?d))))

(ert-deftest extra-trailing-space-120-M6opJl ()
  (py-test
   "def bar():
x = 7"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max)) 
   (beginning-of-line)
   (insert (make-string 4 32))
   (end-of-line)
   (insert (make-string 1 32))
   (py-electric-backspace)
   (should (eolp))
   (should (eq (char-before) ?7))))

(ert-deftest extra-trailing-space-120-pKGvL2 ()
  (py-test
   "def bar():
        x = 7"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward "x")
   (when py-debug-p (whitespace-mode))
   (backward-char 2)
   (py-electric-delete)
   (should (eq (current-column) 4))))

(ert-deftest extra-trailing-space-120-WX8PGG ()
  (py-test
   "def bar():
       x = 7"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (beginning-of-line)
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq (char-before) ?:))))

(ert-deftest extra-trailing-space-120-NahnQx ()
  (py-test
   "def bar():
    x = 7    "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-before) ?7))))

(ert-deftest extra-trailing-space-120-F8qxoR ()
  (py-test
   "def bar():
x = 7         "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (backward-char 3)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-before) ?7))
   ;; (should-not  (char-after))
   ))

(ert-deftest extra-trailing-space-120-YyL25g ()
  (py-test
   "def bar():
    x = 7         "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (backward-char 3)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-before) ?7))
   ;; (should-not  (char-after))
   ))

(ert-deftest delete-test-120-F8qxoR ()
  (py-test
   "var5: Sequence[Mapping[str, Sequence[str]]] = [
    {
     'red': ['scarlet', 'vermilion', 'ruby'],
     'green': ['emerald', 'aqua']
    },
    {
                'sword': ['cutlass', 'rapier']
    }
]"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward "'sword")
   (backward-char)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (skip-chars-forward " \t\r\n\f")
   (should (eq (current-column) 5))
   (should (eq (char-after) ?'))))

(ert-deftest py-ert-deletes-too-much-lp:1300270-dMegYd ()
  (py-test "
x = {'abc':'def',
         'ghi':'jkl'}
"
           'python-mode
           'py-debug-p
           (when py-debug-p (switch-to-buffer (current-buffer)))
           (goto-char 25)
           (py-electric-delete)
           (should (eq 5 (current-indentation)))))

(ert-deftest delete-test-120-dMegYd ()
  (py-test "x"
           'python-mode
           'py-debug-p
           (when py-debug-p (font-lock-ensure))
           (goto-char (point-max))
           (when py-debug-p (switch-to-buffer (current-buffer)))
           (py-electric-backspace)
           (should (bobp))))

(ert-deftest delete-test-120-v32Zaq ()
  (py-test " "
           'python-mode
           'py-debug-p
           (when py-debug-p (font-lock-ensure))
           (goto-char (point-max))
           (when py-debug-p (switch-to-buffer (current-buffer)))
           (py-electric-backspace)
           (should (bobp))))

(ert-deftest delete-test-120-lXSC6t ()
  (py-test
   "var5: Sequence[Mapping[str, Sequence[str]]] = [
    {
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (insert (make-string 8 32))
   (insert "'red': ['scarlet', 'vermilion', 'ruby'],\n")
   (insert (make-string 8 32))
   (insert "'green': ['emerald', 'aqua']")
   (insert (make-string 8 32))
   (insert "\n")
   (insert "    },
    {
")
   (insert (make-string 8 32))
   (insert "'sword': ['cutlass', 'rapier']
    }\n]")
   (search-backward "'aqua")
   (end-of-line)
   (backward-char 4)
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq (char-before) 93))))

(ert-deftest delete-issue-123-M6opJl ()
  (py-test
   "def bar():
    x = 7
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (py-electric-delete)
   (should (eq (char-before) 10))))

(ert-deftest delete-issue-123-zSR3y1 ()
  (py-test
   "def bar():
    x = 7
  "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (beginning-of-line)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-before) 10))))

(ert-deftest delete-issue-123-n2kOH4 ()
  (py-test
   "def bar():
    baz = 7
    bar = 9
    return baz + bar
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (search-backward "7")
   (forward-char 1)
   (py-electric-delete)
   (should (looking-at "    bar = 9"))))

(ert-deftest delete-issue-124-n2kOH4 ()
  (py-test
   "calling(
    123,
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward "1")
   ;; (forward-char 1)
   (py-electric-backspace)
   (should (eq (char-after) ?1))
   (should (eq (char-before) 10))))

(ert-deftest delete-issue-124-8qQxmm ()
  (py-test
   "calling(
123,
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward "1")
   ;; (forward-char 1)
   (py-electric-backspace)
   (should (eq (char-after) ?1))
   (should (eq (char-before) 40))))

(ert-deftest delete-newline-125-8qQxmm ()
  (py-test
   "123
234
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (forward-line -2)
   (py-electric-delete)
   (should (eq (char-after) ?2))))

(ert-deftest delete-newline-126-1FRaeJ ()
  (py-test
   "def test():  a = 'a'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward ":")
   (forward-char 1)
   (py-electric-delete)
   (should (eq (char-before) ?:))
   (should (eq (char-after) 32))))

(ert-deftest delete-newline-126-uWqng3 ()
  (py-test
   "def test(): a = 'a'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward ":")
   (forward-char 1)
   (py-electric-delete)
   (should (eq (char-before) ?:))
   (should (eq (char-after) ?a))))

(ert-deftest py-ert-moves-up-honor-dedent-lp-1280982-K6OICS ()
  (py-test
   "def foo():
    def bar():
        asdf
    "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-newline-and-indent)
   ;; Indent is set back, this is honoured in following lines.
   (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-moves-up-honor-dedent-lp-1280982-APU5fK ()
  (py-test
   "def foo():
    def bar():
        asdf
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-newline-and-indent)
   (py-electric-backspace)
   (should (eq 4 (current-indentation)))))

(ert-deftest delete-newline-126-XSjV1R ()
  (py-test
   "def test():
    a = 'a'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (forward-line -1)
   (end-of-line)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-after) 32))))

(ert-deftest delete-newline-126-tzqfcf ()
  (py-test-point-min
   "def test():
    a = 'a'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-min))
   (py-electric-delete)
   (should (eq (char-after) ?e))))

(ert-deftest extra-trailing-space-120-yC7gXH ()
  (py-test
   "def bar():
    x = 7"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (beginning-of-line)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq (char-before) 10))
   (should (eq (char-after) ?x))))

(ert-deftest delete-issue-123-KoplQh ()
  (py-test
   "def bar():
       x = 7
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (forward-line -1)
   (beginning-of-line)
   (forward-char 3)
   (when py-debug-p (whitespace-mode))
   (py-electric-delete)
   (should (eq 4 (current-indentation)))))

(ert-deftest backspacing-indentation-127-KoplQh ()
  (py-test
   "def test():
    if True:
        print('ok')
        "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq (current-column) 4))))

(ert-deftest backspacing-indentation-127-S0ykuo ()
  (py-test
   "def test():
    if True:
        print('ok')
           "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq (current-column) 8))))

(ert-deftest backspacing-indentation-127-3fQXLA ()
  (py-test
   "def test():
    if True:
        print('ok')
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq (char-before) 41))))

(ert-deftest delete-indentation-128-3fQXLA ()
  (py-test
   "def test():
    if True:
        print('in')
    print('out')"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (forward-line -1)
   ;; at BOL, line is empty
   (py-electric-delete)
   (should (eq 4 (current-indentation)))))

(ert-deftest delete-indentation-128-iLkoVk ()
  (py-test
   "def test():
    if True:
        print('in')
           print('out')"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (beginning-of-line)
   (forward-char 3)
   (py-electric-delete)
   (should (eq 8 (current-indentation)))))

(ert-deftest extra-trailing-space-yC7gXH ()
  (py-test
   "def bar():
    x = 7
            "
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (py-electric-backspace)
   (should (eq 4 (current-indentation)))))

(ert-deftest py-electric-backspace-after-colon-yC7gXH ()
  (py-test
   "def test():          a = 'a'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (when py-debug-p (whitespace-mode))
   (forward-char -9)
   (py-electric-backspace)
   (should (eq (char-before) ?:))
   (should (eq (char-after) 32))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-OJeEWO ()
  (py-test
   "test()"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (search-backward ")")
     (py-electric-backspace)
     (should-not (eq (char-after) 41)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-8exKMk ()
  (py-test
   "test()"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (py-electric-backspace)
     (should-not (eq (char-after) 41))
     (should (eq (char-before) 40)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-WpGhqV ()
  (py-test
   "\"\""
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (backward-char)
     (py-electric-backspace)
     (should-not (eq (char-after) 34)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-tgdTi2 ()
  (py-test
   "\"\""
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (backward-char)
     (py-electric-backspace)
     (should-not (eq (char-after) 34))
     (should-not (eq (char-before) 34)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-Jgj06W ()
  (py-test
   "asdf\"\""
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (py-electric-backspace)
     (should (eq (char-before) 34)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-I69FaW ()
  (py-test
   "asdf\"\""
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (backward-char)
     (py-electric-backspace)
     (should (eq (char-before) ?f)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-55KXXV ()
  (py-test
   "\"asdf\""
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (py-electric-backspace)
     (should (eq (char-before) ?f)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-VY1yk7 ()
  (py-test
   "''"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (backward-char)
     (py-electric-backspace)
     (should-not (eq (char-after) ?'))
     (should-not (eq (char-before) ?')))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-MnlR0p ()
  (py-test
   "asdf''"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (backward-char)
     (py-electric-backspace)
     (should (eq (char-before) ?f)))))

(ert-deftest py-incompatibility-with-electric-pair-mode-133-OtBSzw ()
  (py-test
   "'asdf'"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (call-interactively 'electric-pair-mode t)
   (let ((electric-pair-mode t))
     (goto-char (point-max))
     (py-electric-backspace)
     (should (eq (char-before) ?f)))))

(ert-deftest py-compute-indentation-crasher-breaks-editing-136-HUiwu9 ()
  (py-test
   "def my_func(self):
    this_line() # is bad
    if condition:
        pass
"
   'python-mode
   'py-debug-p
   (when py-debug-p (font-lock-ensure))
   (goto-char (point-max))
   (search-backward "#")
   (py-electric-backspace)
   (should (eq (char-before) 41))))

(provide 'py-ert-delete-tests)
;;; py-ert-delete-tests.el ends here
