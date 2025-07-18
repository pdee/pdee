;;; py-split-just-two-window-on-execute-test.el --- Test splitting

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

(defun py-split-just-two-window-on-execute-lp-1361531-python-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute 'just-two)
        (teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
print(\"I'm the py-split-just-two-window-on-execute-lp-1361531-python-test\")"))
    (py-bug-tests-intern 'py-split-just-two-window-on-execute-lp-1361531-python-base arg teststring)))

(defun py-split-just-two-window-on-execute-lp-1361531-python-base (arg)
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (when py-debug-p (switch-to-buffer (current-buffer))(font-lock-ensure))
  (let ((py-split-window-on-execute 'just-two)
        (erg1 (progn (py-execute-statement-python) py-output-buffer))
        (erg2 (progn (py-execute-statement-python3) py-output-buffer)))
    (sit-for 0.1 t)
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python (count-windows); %s" (count-windows)))
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python (window-list): %s" (window-list)))
    (and
     (assert (eq 2 (count-windows)) nil "py-split-just-two-window-on-execute-lp-1361531-python-test failed")
     (message "%s" "py-split-just-two-window-on-execute-lp-1361531-python-test passed"))
    (py-kill-buffer-unconditional erg1)
    (py-kill-buffer-unconditional erg2)
    (py-restore-window-configuration)))

(defun py-split-just-two-window-on-execute-lp-1361531-ipython-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute 'just-two)
        (teststring "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
print(\"I'm the py-split-just-two-window-on-execute-lp-1361531-ipython-test\")"))
    (py-bug-tests-intern 'py-split-just-two-window-on-execute-lp-1361531-ipython-base arg teststring)))

(defun py-split-just-two-window-on-execute-lp-1361531-ipython-base (arg)
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (when py-debug-p (switch-to-buffer (current-buffer))(font-lock-ensure))
  (let ((py-split-window-on-execute 'just-two)
        (erg1 (progn (py-execute-statement-ipython) py-output-buffer))
        (erg2 (progn (py-execute-statement-jython) py-output-buffer)))
    (sit-for 0.1 t)
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-ipython (count-windows); %s" (count-windows)))
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-ipython (window-list): %s" (window-list)))
    (and
     (assert (eq 2 (count-windows)) nil "py-split-just-two-window-on-execute-lp-1361531-ipython-test failed")
     (message "%s" "py-split-just-two-window-on-execute-lp-1361531-ipython-test passed"))
    (py-kill-buffer-unconditional erg1)
    (py-kill-buffer-unconditional erg2)
    (py-restore-window-configuration)))

(defun py-split-just-two-window-on-execute-lp-1361531-python2-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute 'just-two)
        (teststring "#! /usr/bin/env python2
# -*- coding: utf-8 -*-
print(\"I'm the py-split-just-two-window-on-execute-lp-1361531-python2-test\")"))
    (py-bug-tests-intern 'py-split-just-two-window-on-execute-lp-1361531-python2-base arg teststring)))

(defun py-split-just-two-window-on-execute-lp-1361531-python2-base (arg)
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (when py-debug-p (switch-to-buffer (current-buffer))(font-lock-ensure))
  (let ((py-split-window-on-execute 'just-two)
        (erg1 (progn (py-execute-statement-python2) py-output-buffer))
        (erg2 (progn (py-execute-statement-python2) py-output-buffer)))
    (sit-for 0.1 t)
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python2 (count-windows); %s" (count-windows)))
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python2 (window-list): %s" (window-list)))
    (and
     (assert (eq 2 (count-windows)) nil "py-split-just-two-window-on-execute-lp-1361531-python2-test failed")
     (message "%s" "py-split-just-two-window-on-execute-lp-1361531-python2-test passed"))
    (py-kill-buffer-unconditional erg1)
    (py-kill-buffer-unconditional erg2)
    (py-restore-window-configuration)))

(defun py-split-just-two-window-on-execute-lp-1361531-jython-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute 'just-two)
        (teststring "#! /usr/bin/env jython
# -*- coding: utf-8 -*-
print(\"I'm the py-split-just-two-window-on-execute-lp-1361531-jython-test\")"))
    (py-bug-tests-intern 'py-split-just-two-window-on-execute-lp-1361531-jython-base arg teststring)))

(defun py-split-just-two-window-on-execute-lp-1361531-jython-base (arg)
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (when py-debug-p (switch-to-buffer (current-buffer))(jit-lock-fontify-now))
  (let ((py-split-window-on-execute 'just-two)
        (erg1 (progn (py-execute-statement-jython) py-output-buffer))
        (erg2 (progn (py-execute-statement-ipython) py-output-buffer)))
    (sit-for 0.1 t)
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-jython (count-windows); %s" (count-windows)))
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-jython (window-list): %s" (window-list)))
    (and
     (assert (eq 2 (count-windows)) nil "py-split-just-two-window-on-execute-lp-1361531-jython-test failed")
     (message "%s" "py-split-just-two-window-on-execute-lp-1361531-jython-test passed"))
    (py-kill-buffer-unconditional erg1)
    (py-kill-buffer-unconditional erg2)
    (py-restore-window-configuration)))

(defun py-split-just-two-window-on-execute-lp-1361531-python3-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute 'just-two)
        (teststring "# -*- coding: utf-8 -*-
print(\"I'm the py-split-just-two-window-on-execute-lp-1361531-python3-test\")"))
    (py-bug-tests-intern 'py-split-just-two-window-on-execute-lp-1361531-python3-base arg teststring)))

(defun py-split-just-two-window-on-execute-lp-1361531-python3-base (arg)
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (when py-debug-p (switch-to-buffer (current-buffer))(jit-lock-fontify-now))
  (let ((py-split-window-on-execute 'just-two)
        (erg1 (progn (py-execute-statement-python3) py-output-buffer))
        (erg2 (progn (py-execute-statement-python) py-output-buffer)))
    (sit-for 0.1 t)
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python3 (count-windows); %s" (count-windows)))
    (when py-debug-p (message "just-two-window-on-execute-lp-1361531-python3 (window-list): %s" (window-list)))
    (and
     (assert (eq 2 (count-windows)) nil "py-split-just-two-window-on-execute-lp-1361531-python3-test failed")
     (message "%s" "py-split-just-two-window-on-execute-lp-1361531-python3-test passed"))
    (py-kill-buffer-unconditional erg1)
    (py-kill-buffer-unconditional erg2)
    (py-restore-window-configuration)))

(provide 'py-split-just-two-window-on-execute-test)
;;; py-split-just-two-window-on-execute-test.el here
