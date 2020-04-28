;;; py-execute-region-commandp-test.el --- Test execute region forms

;; Copyright (C) 2015-2020  Andreas Röhler
;; Author: Andreas Röhler <andreas.roehler@online.de>
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

(ert-deftest py-execute-region-commandp-test ()
  (should (commandp 'py-execute-region)))

(ert-deftest py-execute-region-switch-commandp-test ()
  (should (commandp 'py-execute-region-switch)))

(ert-deftest py-execute-region-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-no-switch)))

(ert-deftest py-execute-region-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-dedicated)))

(ert-deftest py-ert-execute-region-python-commandp-test ()
  (should (commandp 'py-execute-region-python)))

(ert-deftest py-ert-execute-region-python-switch-commandp-test ()
  (should (commandp 'py-execute-region-python-switch)))

(ert-deftest py-ert-execute-region-python-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-python-no-switch)))

(ert-deftest py-ert-execute-region-python-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-python-dedicated)))

(ert-deftest py-ert-execute-region-python2-commandp-test ()
  (should (commandp 'py-execute-region-python2)))

(ert-deftest py-ert-execute-region-python2-switch-commandp-test ()
  (should (commandp 'py-execute-region-python2-switch)))

(ert-deftest py-ert-execute-region-python2-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-python2-no-switch)))

(ert-deftest py-ert-execute-region-python2-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-python2-dedicated)))

(ert-deftest py-ert-execute-region-python3-commandp-test ()
  (should (commandp 'py-execute-region-python3)))

(ert-deftest py-ert-execute-region-python3-switch-commandp-test ()
  (should (commandp 'py-execute-region-python3-switch)))

(ert-deftest py-ert-execute-region-python3-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-python3-no-switch)))

(ert-deftest py-ert-execute-region-python3-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-python3-dedicated)))

(ert-deftest py-ert-execute-region-ipython-commandp-test ()
  (should (commandp 'py-execute-region-ipython)))

(ert-deftest py-ert-execute-region-ipython-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython-switch)))

(ert-deftest py-ert-execute-region-ipython-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython-no-switch)))

(ert-deftest py-ert-execute-region-ipython-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-ipython-dedicated)))

(ert-deftest py-ert-execute-region-ipython2.7-commandp-test ()
  (should (commandp 'py-execute-region-ipython2.7)))

(ert-deftest py-ert-execute-region-ipython2.7-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython2.7-switch)))

(ert-deftest py-ert-execute-region-ipython2.7-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython2.7-no-switch)))

(ert-deftest py-ert-execute-region-ipython2.7-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-ipython2.7-dedicated)))

(ert-deftest py-ert-execute-region-ipython3-commandp-test ()
  (should (commandp 'py-execute-region-ipython3)))

(ert-deftest py-ert-execute-region-ipython3-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython3-switch)))

(ert-deftest py-ert-execute-region-ipython3-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-ipython3-no-switch)))

(ert-deftest py-ert-execute-region-ipython3-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-ipython3-dedicated)))

(ert-deftest py-ert-execute-region-jython-commandp-test ()
  (should (commandp 'py-execute-region-jython)))

(ert-deftest py-ert-execute-region-jython-switch-commandp-test ()
  (should (commandp 'py-execute-region-jython-switch)))

(ert-deftest py-ert-execute-region-jython-no-switch-commandp-test ()
  (should (commandp 'py-execute-region-jython-no-switch)))

(ert-deftest py-ert-execute-region-jython-dedicated-commandp-test ()
  (should (commandp 'py-execute-region-jython-dedicated)))

(provide 'py-execute-region-commandp-test)
;;; py-execute-region-commandp-test.el ends here
