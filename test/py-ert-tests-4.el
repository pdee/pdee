;;; py-interactive-tests.el --- Tests expected to succeed interactively -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

(ert-deftest py-ert-fast-complete-1 ()
  (py-test-with-temp-buffer
      "pri"
    (let ((py-return-result-p t)
	  py-result py-store-result-p)
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (py-fast-complete)
      (should (eq (char-before) 40)))))


(ert-deftest py-ert-shift-right-test-1 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-2 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (py-shift-right 1)
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-3 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (beginning-of-line)
    (py-shift-right 1)
    (forward-line -1)
    (should (eq 4 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-4 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (back-to-indentation)
    (forward-word 1)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))))


(ert-deftest py-ert-shift-right-test-5 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (forward-char -2)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))))

(ert-deftest py-ert-shift-right-test-6 ()
  (py-test-with-temp-buffer
      "def foo():
    if path == '/tmp':
    raise ValueError"
    (forward-char -2)
    (py-shift-right 1)
    (should (eq 8 (current-indentation)))))

;; (ert-deftest window-on-top-lp-1579309-test-1 ()
;;   (py-test-with-temp-buffer
;;       "print(123)"
;;     (let ((old-window (selected-window))
;; 	  (old-point (window-point)))
;;       (message "%s" (window-list))
;;       (py-execute-statement-python3)
;;       (message "%s" (window-list))
;;       (save-window-excursion
;; 	(with-current-buffer "*Python3*"
;; 	  (switch-to-buffer (current-buffer)) 
;; 	  (message "%s" (current-buffer))
;; 	  (message "%s" (window-point))
;; 	  (message "%s" (window-list))))
;;       (message "%s" (window-list))
;;       (should (eq old-window (selected-window)))
;;       (should (eq old-point (window-point))))))

(provide 'py-interactive-tests)
;;; py-interactive-tests.el ends here
