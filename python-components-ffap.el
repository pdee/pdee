;;; python-components-ffap.el --- support ffap       -*- lexical-binding: t; -*-

;; Author: lifted from python.el
;; Keywords: languages

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


(defvar py-ffap-p nil)
(defvar py-ffap nil)
(defvar ffap-alist nil)

(defun py--set-ffap-form ()
  (cond ((and py-ffap-p py-ffap)
         (eval-after-load "ffap"
           '(push '(python-mode . py-module-path) ffap-alist))
         (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
         (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
                                  ffap-alist)))
        (t (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
           (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
                                    ffap-alist))
           (setq ffap-alist (remove '(python-mode . py-module-path) ffap-alist)))))

(defcustom py-ffap-p nil

  "Select python-modes way to find file at point.

Default is nil"

  :type '(choice

          (const :tag "default" nil)
          (const :tag "use py-ffap" py-ffap))
  :tag "py-ffap-p"
  :set (lambda (symbol value)
         (set-default symbol value)
         (py--set-ffap-form))
    :group 'python-mode)

;; (defvar py-ffap-string-code
;;   "__FFAP_get_module_path('''%s''')\n"
;;   "Python code used to get a string with the path of a module.")
(defun py--python-send-ffap-setup-code (buffer)
  "For Python see py--python-send-setup-code."
  (py--python-send-setup-code-intern "ffap" buffer))

(defvar py-ffap-setup-code
  "def __FFAP_get_module_path(module):
    try:
        import os
        path = __import__(module).__file__
        if path[-4:] == '.pyc' and os.path.exists(path[0:-1]):
            path = path[:-1]
        return path
    except:
        return ''
"
  "Python code to get a module path.")





;; (defun py--all-shell-mode-setting (buffer)
;; ...
;; (py--python-send-ffap-setup-code buffer)

;;  ffap
(defun py-ffap-module-path (module)
  "Function for ‘ffap-alist’ to return path for MODULE."
  (let ((process (or
                  (and (eq major-mode 'py-shell-mode)
                       (get-buffer-process (current-buffer)))
                  (py-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (py--send-string-no-output
              (format py-ffap-string-code module) process)))
        (when module-file
          (substring-no-properties module-file 1 -1))))))

(eval-after-load "ffap"
  '(progn
     (push '(python-mode . py-ffap-module-path) ffap-alist)
     (push '(py-shell-mode . py-ffap-module-path) ffap-alist)))

(provide 'python-components-ffap)
;;; python-components-ffap.el ends here
