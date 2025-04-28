;;; py-setup-ert-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; URL: https://gitlab.com/python-mode-devs
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

;;; Code:

;; (require 'org)

;; (setq py-debug-p t)

;; (if (file-readable-p "../python-components-mode.el")
;;     (load (expand-file-name "../python-components-mode.el") nil t)
;;   (when (file-readable-p "../python-mode.el")
;;     (load (expand-file-name "../python-mode.el") nil t)))

;; (require 'font-lock)



(defmacro py-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max)))
       ,@body)))

(defmacro py-test-point-min (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (funcall ,mode)
       (goto-char (point-min))
       (and ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (
           ;; (python-indend-offset 4)
           python-indent-guess-indent-offset
           hs-minor-mode py--imenu-create-index-p)
       ;; (and (featurep 'python) (unload-feature 'python))
       (insert ,contents)
       (python-mode)
       (when py-debug-p
         ;; (message "py-python-edit-version: %s" py-python-edit-version)
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (
           ;; (python-indent-offset 4)
           python-indent-guess-indent-offset
           hs-minor-mode py--imenu-create-index-p)
       (python-mode)
       (insert ,contents)
       (goto-char (point-min))
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

;; from jit-lock.el
(defmacro with-buffer-prepared-for-jit-lock (&rest body)
  "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
  (declare (debug t))
  `(let ((inhibit-point-motion-hooks t))
     (with-silent-modifications
       ,@body)))



(provide 'py-setup-ert-tests)
;; py-setup-ert-tests.el ends here
