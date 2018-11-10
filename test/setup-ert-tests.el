;;; setup-ert-tests.el --- Provide needed forms -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Andreas Röhler

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

;;; Code:

(defvar py-debug-p nil
  "Avoid error")

;; (setq py-debug-p t)

(require 'font-lock)

(defmacro py-test (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.

BODY is code to be executed within the temp buffer "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max)))
       ,@body)
  ;; (sit-for 0.1)
  ))

(defmacro py-test-point-min (contents mode verbose &rest body)
  "Create temp buffer inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode)
       (insert ,contents)
       (funcall ,mode)
       (goto-char (point-min))
       (when ,verbose
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min)(point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (python-mode)
       (goto-char (point-min))
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (python-mode)
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




;; (defvar py-forward-text "
;; # {{
;; class bar:
;;     def foo ():
;;         try:
;;             if foo:
;;                 for a in b:
;;                     print('%(language)s has %(number)03d quote types.' %
;;        {'language': \"Python\", \"number\": 2})

;;             elif bar:
;;                 for a in b:
;;                     pass
;;             else:
;;                 for a in b:
;;                     pass
;; # }}
;;         except:
;;             block2
;; ")

;; (defvar py-up-text "
;; def foo():
;;     if True:
;;         def bar():
;;             pass
;;     elif False:
;;         def baz():
;;             pass
;;     else:
;;         try:
;;             1 == 1
;;         except True:
;;             def foo1():
;;                 if True:
;;                     def bar1():
;;                         pass
;;                 elif False:
;;                     def baz1():
;;                         pass
;;                 else:
;;                     try:
;;                         1 == 1
;;                     except True:
;;                         pass
;;                     else True:
;;                         pass
;;                     finally:
;;                         pass
;;         else True:
;;             pass
;;         finally:
;;             pass
;; ")



(defmacro py-bug-tests-intern (testname arg teststring)
  "Just interally. "
  (declare (debug (edebug-form-spec t)))
  `(let ((debug-on-error t)
         (enable-local-variables :all)
         py-load-pymacs-p
         ;; py-split-window-on-execute
         ;; py-switch-buffers-on-execute-p
         py-start-run-py-shell
         proc
         py-fontify-shell-buffer-p
  	 (test-buffer (get-buffer-create (replace-regexp-in-string "\\\\" "" (replace-regexp-in-string "-base$" "-test" (prin1-to-string ,testname))))))
     (with-current-buffer test-buffer
       (delete-other-windows)
       (erase-buffer)
       (fundamental-mode)
       (python-mode)
       (insert ,teststring)
       (when py-debug-p (switch-to-buffer test-buffer))
       (local-unset-key (kbd "RET"))
       (sit-for 0.1)
       (when (and (boundp 'company-mode) company-mode) (company-abort))
       (funcall ,testname ,arg)
       (message "%s" (replace-regexp-in-string "\\\\" "" (concat (replace-regexp-in-string "-base$" "-test" (prin1-to-string ,testname)) " passed")))
       ;; (unless (< 1 arg)
       (unless (eq 2 arg)
  	 (set-buffer-modified-p 'nil)
  	 (and (get-buffer-process test-buffer)
  	      (set-process-query-on-exit-flag (get-buffer-process test-buffer) nil)
  	      (kill-process (get-buffer-process test-buffer)))
  	 (kill-buffer test-buffer)))))

(provide 'setup-ert-tests)
;; setup-ert-tests.el ends here
