;;; py-extra-tests.el --- extra tests                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: convenience

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

(ert-deftest py-ert-execute-block-fast-9Ui5ja ()
  (py-test-point-min
      "try:
    a
except NameError:
    a=1
finally:
    a+=1
    print(a)"
    'python-mode
    'py-debug-p
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  (py-debug-p t)
	  py-result)
      (py-execute-block)
      (when py-debug-p (message "py-ert-execute-block-fast-9Ui5ja, py-result: %s" py-result))
      (should (string= "2" py-result)))))

(ert-deftest py-ert-moves-up-execute-statement-python3-dedicated-test ()
  (py-test-with-temp-buffer-point-min
      "print(\"I'm the py-execute-statement-python3-dedicated-test\")"
    (let ((py-debug-p t)
	  py-store-result-p
	  erg)
      (call-interactively 'py-execute-statement-python3-dedicated)
      ;; (sit-for 0.1 t)
      (set-buffer py-buffer-name)
      (goto-char (point-min))
      (should (search-forward "py-execute-statement-python3-dedicated-test" nil t 1)))))

;; (ert-deftest py-ert-execute-block-fast-3 ()
;;   (py-test-with-temp-buffer-point-min
;;       "if True:
;;     a = 1
;;     print(a)"
;;     (let ((py-fast-process-p t)
;; 	  (py-return-result-p nil)
;; 	  (py-debug-p t)
;; 	  py-result)
;;       (py-execute-block)
;;       (when py-debug-p (message "py-ert-execute-block-fast, py-result: %s" py-result))
;;       (sit-for 0.1 t)
;;       (should (string= "1" py-result)))))

(ert-deftest py-ert-exception-name-face-lp-1294742 ()
  (py-test-with-temp-buffer
      "ArithmeticError"
    (forward-char -1)
    (font-lock-fontify-region (point-min) (point-max))
    (should (eq 'py-exception-name-face (get-char-property (point) 'face)))))

(ert-deftest py-ert-execute-block-jython-test ()
  (let ((buffer (py--choose-buffer-name "jython"))
	py-split-window-on-execute py-switch-buffers-on-execute-p)
    (py-test-with-temp-buffer
        "if True:
    print(\"one\")
    print(\"two\")"
      (py-execute-block-jython)
      (sit-for 0.1)
      (with-current-buffer buffer
	(goto-char (point-max))
	(should (search-backward "two"))))))

(ert-deftest py-shell-complete-in-dedicated-shell ()
  ;; (py-test-with-temp-buffer
  (with-current-buffer (python-dedicated)
    ;; (when py-debug-p (switch-to-buffer (current-buffer)))
    ;; (switch-to-buffer (current-buffer))
    (insert "pri")
    (sit-for 1 t)
    (call-interactively 'py-indent-or-complete)
    (sit-for 0.1 t)
    (should (or (looking-back "print.?" (line-beginning-position))))))

(ert-deftest py-ert-execute-statement-fast-1 ()
  (py-test-point-min
      "print(1)"
    'python-mode
    'py-debug-p
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement)
      (should (string= "1" py-result)))))

(ert-deftest py-ert-execute-statement-fast-2 ()
  (py-test-point-min
      "print(2)"
    'python-mode
    'py-debug-p
    (let ((py-fast-process-p t)
	  (py-return-result-p t)
	  py-result py-store-result-p)
      (py-execute-statement-fast)
      (should (string= "2" py-result)))))

;; adapted from python.el
(ert-deftest py-syntax-after-backspace-TwyMwn ()
  (py-test
      "\"\""
    'python-mode
    'py-debug-p
    (goto-char (point-max))
    (should (string= (buffer-string) "\"\""))
    (should (null (nth 3 (parse-partial-sexp (point-min) (point)))))))

(ert-deftest py-complete-in-python-shell-test ()
  (py-kill-buffer-unconditional "*Python*")
  (py-kill-buffer-unconditional "*Python3*")
  (set-buffer (python))
  (goto-char (point-max))
  (insert "pri")
  (py-indent-or-complete)
  (sit-for 0.1)
  (should (or (eq ?t (char-before))(eq ?\( (char-before)))))

(ert-deftest py-ert-execute-statement-fast-test ()
  (py-test-point-min
      "print(123234)"
    'python-mode
    'py-debug-p
    (goto-char (point-min))
    (let (py-split-window-on-execute py-switch-buffers-on-execute-p)
      (py-execute-statement-fast)
      (set-buffer (concat "*" (capitalize py-shell-name) " Fast*"))
      (switch-to-buffer (current-buffer))
      (should (search-backward "123234")))))

(ert-deftest py-ert-fast-complete-1 ()
  (py-test-with-temp-buffer
      "obj"
    (let ((py-return-result-p t)
	  py-result py-store-result-p)
      ;; (when py-debug-p (switch-to-buffer (current-buffer)))
      (py-fast-complete)
      (sit-for 0.1)
      (should (search-backward "ect")))))



(provide 'py-extra-tests)
;;; py-extra-tests.el ends here
