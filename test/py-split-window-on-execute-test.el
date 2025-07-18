;;; py-split-window-on-execute-test.el --- Test splitting

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


(defun py-split-window-on-execute-lp-1361531-python-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute t)
        (teststring "#! /usr/bin/env python
# -*- coding: utf-8 -*-
print(\"I'm the py-split-window-on-execute-lp-1361531-python-test\")"))
    (py-bug-tests-intern 'py-split-window-on-execute-lp-1361531-python-base arg teststring)))

(defun py-split-window-on-execute-lp-1361531-python-base ()
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (py-execute-statement)
  (assert (eq 2 (count-windows)) nil "py-split-window-on-execute-lp-1361531-python-test failed")
  (py-kill-buffer-unconditional (current-buffer)))

(defun py-split-window-on-execute-lp-1361531-ipython-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute t)
        (teststring "#! /usr/bin/env ipython
# -*- coding: utf-8 -*-
print(\"I'm the py-split-window-on-execute-lp-1361531-ipython-test\")"))
    (py-bug-tests-intern 'py-split-window-on-execute-lp-1361531-ipython-base arg teststring)))

(defun py-split-window-on-execute-lp-1361531-ipython-base ()
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (py-execute-statement)
  (assert (eq 2 (count-windows)) nil "py-split-window-on-execute-lp-1361531-ipython-test failed")
  (py-kill-buffer-unconditional (current-buffer)))

(defun py-split-window-on-execute-lp-1361531-python2-test ()
  (interactive)
  (let ((py-split-window-on-execute t)
        (teststring "#! /usr/bin/env python2
# -*- coding: utf-8 -*-
print(\"I'm the py-split-window-on-execute-lp-1361531-python2-test\")"))
    (py-bug-tests-intern 'py-split-window-on-execute-lp-1361531-python2-base)))

(defun py-split-window-on-execute-lp-1361531-python2-base ()
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (py-execute-statement)
  (assert (eq 2 (count-windows)) nil "py-split-window-on-execute-lp-1361531-python2-test failed")
  (py-kill-buffer-unconditional (current-buffer)))

(defun py-split-window-on-execute-lp-1361531-jython-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute t)
        (teststring "#! /usr/bin/env jython
# -*- coding: utf-8 -*-
print(\"I'm the py-split-window-on-execute-lp-1361531-jython-test\")"))
    (py-bug-tests-intern 'py-split-window-on-execute-lp-1361531-jython-base arg teststring)))

(defun py-split-window-on-execute-lp-1361531-jython-base ()
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (py-execute-statement)
  (assert (eq 2 (count-windows)) nil "py-split-window-on-execute-lp-1361531-jython-test failed")
  (py-kill-buffer-unconditional (current-buffer)))

(defun py-split-window-on-execute-lp-1361531-python3-test (&optional arg)
  (interactive "p")
  (let ((py-split-window-on-execute t)
        (teststring "# -*- coding: utf-8 -*-
print(\"I'm the py-split-window-on-execute-lp-1361531-python3-test\")"))
    (py-bug-tests-intern 'py-split-window-on-execute-lp-1361531-python3-base arg teststring)))

(defun py-split-window-on-execute-lp-1361531-python3-base ()
  (when py-debug-p (message "py-split-window-on-execute: %s" py-split-window-on-execute))
  (delete-other-windows)
  (py-execute-statement)
  (assert (eq 2 (count-windows)) nil "py-split-window-on-execute-lp-1361531-python3-test failed")
  (py-kill-buffer-unconditional (current-buffer)))

(provide 'py-split-window-on-execute-test)
;;; py-split-window-on-execute-test.el here
