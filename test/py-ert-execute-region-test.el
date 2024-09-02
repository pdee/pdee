;;; py-ert-execute-region-test.el --- py-execute-region tests

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

(require 'py-setup-ert-tests)

(ert-deftest py-ert-execute-region-python-test ()
  (if
      (executable-find "python")
      (py-test-with-temp-buffer
          "print(\"one\")
print(\"two\")"
        (let ((buffer (py--choose-buffer-name "python")))

          (py-execute-region-python (point-min) (point-max))
          ;; (sit-for 0.5 t)
          (set-buffer buffer)
          (goto-char (point-max))
          (should (search-backward "two"))))
    (when py-verbose-p (message "%s" "Don't see a ‘python’ executable"))))

(ert-deftest py-ert-execute-region-python3-test ()
  (py-test-with-temp-buffer
      "print(\"one\")
print(\"two\")"
    (let ((buffer (py--choose-buffer-name "python3")))
      (py-execute-region-python3 (point-min) (point-max))
      (set-buffer buffer)
      (goto-char (point-max))
       (sit-for 1)
      ;; (font-lock-ensure)
      (should (search-backward "two")))))

(ert-deftest py-ert-execute-region-ipython-test ()
  (if
      (ignore-errors (executable-find "ipython"))
      (py-test-with-temp-buffer
          "print(\"one\")
print(\"two\")"
        (let ((buffer (py--choose-buffer-name "ipython")))
          (py-execute-region-ipython (point-min) (point-max))
          (set-buffer buffer)
          ;; (accept-process-output (get-buffer-process buffer) 0.1)
          ;; (switch-to-buffer (current-buffer))
          (goto-char (point-max))
          ;; (sit-for 0.5 t)
          (should (search-backward "two"))))
    (when py-verbose-p
      (message "py-ert-execute-region-ipython-test: %s" "No executable found!"))))


(ert-deftest py-ert-execute-region-ipython3-test ()
  (if
      (ignore-errors (executable-find "ipython3"))
      (let ((buffer (py--choose-buffer-name "ipython3"))
	    (inhibit-point-motion-hooks t)
            ;; (py-debug-p t)
            )
        (py-test-with-temp-buffer
            "print(\"one\")
print(\"two\")"
          (py-execute-region-ipython3 (point-min) (point-max))
          (set-buffer buffer)
          (when py-debug-p (message "current-buffer0: %s" (current-buffer))
                (sit-for 0.1)
                ;; (accept-process-output (get-buffer-process buffer) 1)
                (when py-debug-p (message "current-buffer1: %s" (current-buffer)))
                ;; (goto-char (point-max))
                ;; (switch-to-buffer (current-buffer))
                (when py-debug-p (message "current-buffer2: %s" (current-buffer)))
                (goto-char (point-max))
                (when py-debug-p (message "current-buffer3: %s" (current-buffer)))
                ;; (font-lock-ensure)
                ;; (sit-for 2)
                (when py-debug-p (message "current-buffer4: %s" (current-buffer)))
                (should (search-backward "two")))))
    (when py-verbose-p
      (message "py-ert-execute-region-ipython3-test: %s" "No executable found!"))))

(ert-deftest py-ert-execute-region-jython-test ()
  (if
      (executable-find "jython")
      (py-test-with-temp-buffer
          "print(\"one\")
print(\"two\")"
        (let ((buffer (py--choose-buffer-name "jython")))
          (py-execute-region-jython (point-min) (point-max))
          (set-buffer buffer)
          ;; (switch-to-buffer (current-buffer))
          (goto-char (point-max))
          ;; (font-lock-ensure)
          (sit-for 0.1)
          (should (search-backward "two"))))
    (when py-verbose-p
      (message "py-ert-execute-region-jython-test: %s" "No executable found!"))))

(provide 'py-ert-execute-region-test)
;;; py-ert-execute-region-test.el here
