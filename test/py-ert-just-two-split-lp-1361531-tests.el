;;; py-ert-just-two-split-lp-1361531-tests.el --- Test splitting

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

(require 'setup-ert-tests)

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-python2-test ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python2
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-python2-test\")"
    (delete-other-windows)
    (let* ((py-split-window-on-execute 'just-two)
	   (erg1 (progn (py-execute-statement-python2-dedicated) py-output-buffer))
	   (erg2 (progn (py-execute-statement-python2-dedicated) py-output-buffer)))
      (sit-for 1 t)
      (when py-debug-p (message "(count-windows) %s" (count-windows)))
      (should (eq 2 (count-windows)))
      (py-kill-buffer-unconditional erg1)
      (py-kill-buffer-unconditional erg2)
      (py-restore-window-configuration))))

(ert-deftest py-ert-just-two-split-dedicated-lp-1361531-python3-test ()
  (py-test-with-temp-buffer
      "#! /usr/bin/env python3
# -*- coding: utf-8 -*-
print(\"I'm the py-just-two-split-dedicated-lp-1361531-python3-test\")"
    (delete-other-windows)
    (let* ((py-split-window-on-execute 'just-two)
	   (erg1 (progn (py-execute-statement-python3-dedicated) py-output-buffer))
	   (erg2 (progn (py-execute-statement-python3-dedicated) py-output-buffer)))
      (sit-for 1 t)
      (when py-debug-p (message "(count-windows) %s" (count-windows)))
      (should (eq 2 (count-windows)))
      (py-kill-buffer-unconditional erg1)
      (py-kill-buffer-unconditional erg2)
      (py-restore-window-configuration))))

;; py-ert-just-two-split-lp-1361531-tests.el ends here
(provide 'py-ert-just-two-split-lp-1361531-tests)
