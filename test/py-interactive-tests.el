;;; py-interactive-tests.el --- Tests expected to succeed interactively

;; Copyright (C) 2015  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: lisp

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

(ert-deftest py-ert-execute-block-jython-test ()
  (let ((buffer (py--choose-buffer-name "jython")))
    (py-test-with-temp-buffer
        "if True:
    print(\"one\")
    print(\"two\")"
      (py-execute-block-jython)
      (set-buffer buffer)
      (should (search-backward "two")))))

(ert-deftest py-ert-execute-block-ipython2.7-test ()
  (let ((buffer (py--choose-buffer-name "ipython2.7")))
    (py-test-with-temp-buffer
        "if True:
    print(\"one\")
    print(\"two\")"
      (py-execute-block-ipython2.7)
      (set-buffer buffer)
      (should (search-backward "two")))))

(ert-deftest py-ert-execute-block-fast-2 ()
  (py-test-with-temp-buffer-point-min
      "try:
    a
except NameError:
    a=1
finally:
    a+=1
    print(a)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (should (numberp (string-to-number (car (split-string py-result))))))))

(ert-deftest py-ert-kill-statements-test ()
  (py-test-with-temp-buffer
      "with file(\"roulette-\" + zeit + \".csv\", 'w') as datei:
    for i in range(anzahl):
        klauf.pylauf()
        datei.write(str(spiel[i]) + \"\\n\")
    datei.write(\"treffer; schwarz; gruen; rot; pair; impair; passe; manque; spiel\\n\")
    print(''' asdf' asdf asdf asdf asdf asdfasdf asdfasdf a asdf asdf asdf asdfasdfa asdf asdf asdf asdf
''')"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (forward-line -2)
    (back-to-indentation)
    (py-kill-statements)
    (sit-for 0.1)
    (should-not (char-after))
    ;; (should (eobp))
    ;; (should (looking-at "\\'"))
    ))

(ert-deftest py-ert-execute-block-fast ()
  (py-test-with-temp-buffer-point-min
      "if True:
    a = 1
    print(a)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
      (sit-for 0.1 t)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-exception-name-face-lp-1294742 ()
  (py-test-with-temp-buffer-point-min
      " ArithmeticError AssertionError AttributeError BaseException BufferError BytesWarning DeprecationWarning EOFError EnvironmentError Exception FloatingPointError FutureWarning GeneratorExit IOError ImportError ImportWarning IndentationError IndexError KeyError KeyboardInterrupt LookupError MemoryError NameError NoResultFound NotImplementedError OSError OverflowError PendingDeprecationWarning ReferenceError RuntimeError RuntimeWarning StandardError StopIteration SyntaxError SyntaxWarning SystemError SystemExit TabError TypeError UnboundLocalError UnicodeDecodeError UnicodeEncodeError UnicodeError UnicodeTranslateError UnicodeWarning UserWarning ValueError Warning ZeroDivisionError"
    (when py-debug-p (switch-to-buffer (current-buffer))
	  (font-lock-fontify-buffer))
    (while (and (not (eobp))(< 0 (skip-chars-forward " ")))
      (should (eq 'py-exception-name-face (get-char-property (point) 'face)))
      (skip-chars-forward "^ \n"))))

(defun py-shell-complete-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring (concat py-test-shebang "
# -*- coding: utf-8 -*-
impo")))
    (py-bug-tests-intern 'py-shell-complete-base arg teststring)))

(defun py-shell-complete-base ()
  (when (and (interactive-p) py-debug-p) (switch-to-buffer (current-buffer))
	(font-lock-fontify-buffer))
  (sit-for 0.1 t)
  (py-shell-complete)
  (sit-for 0.1)
  (assert (looking-back "import") nil "py-shell-complete-test failed"))

(ert-deftest py-ert-execute-statement-fast-1 ()
  (py-test-with-temp-buffer-point-min
      "print(1)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-execute-statement-fast-2 ()
  (py-test-with-temp-buffer-point-min
      "print(2)"
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement-fast)
      (should (string= "2" py-result)))))

(ert-deftest py-ert-fast-complete-1 ()
  (py-test-with-temp-buffer
      "pri"
    (let ((py-return-result-p t)
	  py-result py-store-result-p)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (py-fast-complete)
      (should (eq (char-before) 40)))))

(provide 'py-interactive-tests)
;;; py-interactive-tests.el ends here
