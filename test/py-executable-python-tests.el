;; py-executable-python-tests.el --- testing python-mode.el -*- lexical-binding: t; -*-

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

(ert-deftest py-dedicated-shell-test-7tw0PH ()
  (if (not (executable-find "python"))
      (message "py-dedicated-shell-test-7tw0PH: %s" "No python executable found!")
    (let ((erg (buffer-name (py-shell nil nil t "python"))))
      (should (< 8 (length erg)))
      (should (eq 0 (string-match "^*Python" erg))))))

(ert-deftest py-shell-python-lp-1398530-test-Haizw1 ()
  (if (not (executable-find "python"))
      (message "py-shell-python-lp-1398530-test-Haizw1: %s" "No python executable found!")
    (when (buffer-live-p (get-buffer "*Python*"))
      (py-kill-buffer-unconditional "*Python*"))
    (py-test-with-temp-buffer
	""
      (when py-debug-p (switch-to-buffer (current-buffer)))
      (let ((py-shell-name "python"))
	(py-shell)
	(sit-for 0.1 t)
	(should (buffer-live-p (get-buffer "*Python*")))))))

(ert-deftest py-shell-python3-lp-1398530-test-gm7LwH ()
  (if (not (executable-find "python3"))
      (message "py-shell-python3-lp-1398530-test-gm7LwH: %s" "No python3 executable found!")
    (when (buffer-live-p (get-buffer "*Python3*"))
      (py-kill-buffer-unconditional "*Python3*"))
    (py-test-with-temp-buffer
	""
      (goto-char (point-max))
      (let ((py-shell-name "python3"))
	(py-shell)
	(sit-for 0.1 t)
	(should (buffer-live-p (get-buffer "*Python3*")))))))

(ert-deftest py-reuse-existing-shell-test-bSPpqY ()
  (let ((python-mode-v5-behavior-p nil))
    "foo"
    ;; kill existing shells
    ;; (py--kill-buffer-unconditional "*Python3*")
    ;; (py--kill-buffer-unconditional "*IPython*")
    ;; (py--kill-buffer-unconditional "*Python*<2>")
    ;; (py--kill-buffer-unconditional "*IPython*<2>")
    (python3)
    (ipython3)
    (sit-for 0.1 t)
    ;; this should not open a "*Python*<2>"
    (python3)
    (ipython3)
    (sit-for 0.1 t)
    (should (not (buffer-live-p (get-buffer "*Python3*<2>"))))
    (should (not (buffer-live-p (get-buffer "*IPython3*<2>"))))
    (should (buffer-live-p (get-buffer "*Python3*")))
    (should (buffer-live-p (get-buffer "*IPython3*")))))

(ert-deftest py-python-mode-v5-behavior-test-bSPpqY ()
  (let ((python-mode-v5-behavior-p t))
    (python3)
    ;; (ipython3)
    (sit-for 0.1)
    (should (buffer-live-p (get-buffer "*Python Output*")))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*Python3*<2>"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*IPython3*<2>"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*Python3*"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*IPython3*"))))))

(ert-deftest py-python-mode-v5-behavior-test-2n0wbL ()
  (let ((python-mode-v5-behavior-p t))
    ;; (python3)
    (ipython3)
    (sit-for 1)
    (should (buffer-live-p (get-buffer "*Python Output*")))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*Python3*<2>"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*IPython3*<2>"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*Python3*"))))
    (sit-for 0.1)
    (should (not (buffer-live-p (get-buffer "*IPython3*"))))
    ))

(provide 'py-executable-python-tests)
;;; py-executable-python-tests.el ends here
