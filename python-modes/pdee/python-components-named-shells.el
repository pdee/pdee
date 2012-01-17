;;; python-components-named-shells.el --- Versioned Python shells

;; Copyright (C) 2011  Andreas Roehler

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(defun python (&optional argprompt)
  "Start an Python interpreter in another window.

With optional  \\[universal-argument] user is prompted
for options to pass to the Python interpreter. "
  (interactive)
  (let ((py-shell-name "python"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python2 (&optional argprompt)
  "Start an Python2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2 interpreter. "
  (interactive)
  (let ((py-shell-name "python2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python2.7 (&optional argprompt)
  "Start an Python2.7 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2.7 interpreter. "
  (interactive)
  (let ((py-shell-name "python2.7"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python3 (&optional argprompt)
  "Start an Python3 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3 interpreter. "
  (interactive)
  (let ((py-shell-name "python3"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-completion-at-point)
    (py-shell argprompt)))

(defun python3.2 (&optional argprompt)
  "Start an Python3.2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3.2 interpreter. "
  (interactive)
  (let ((py-shell-name "python3.2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-completion-at-point)
    (py-shell argprompt)))

(defalias 'iyp 'ipython)
(defalias 'ipy 'ipython)
(defun ipython (&optional argprompt)
  "Start an IPython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the IPython interpreter. "
  (interactive)
  (let* ((py-shell-name "ipython"))
    (py-set-shell-completion-environment)
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))

(defun jython (&optional argprompt)
  "Start an Jython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Jython interpreter. "
  (interactive)
  (let ((py-shell-name "jython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

;;; Installer

(defvar py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-set-shell-completion-environment)
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))
")

(setq py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-set-shell-completion-environment)
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))
")

(defun py-install-search-local ()
  (interactive)
  (let ((erg (split-string (shell-command-to-string (concat "find " default-directory " -maxdepth 9 -type f -name \"*python\"")))))))

(defun py-install-local-shells (&optional local path-prefix)
  "Builds Python-shell commands from executable found in LOCAL.

If LOCAL is empty, shell-command `find' searches beneath current directory.
Eval resulting buffer to install it, see customizable `py-extensions'. "
  (interactive)
  (let* ((local-dir (if local
                        (expand-file-name local)
                      (read-from-minibuffer "Virtualenv directory: " default-directory)))
         (path-separator (if (string-match "/" local-dir)
                             "/"
                           "\\" t))
         (shells (split-string (shell-command-to-string (concat "find " local-dir " -maxdepth 9 -type f -executable -name \"*python\""))))
         erg newshell prefix akt end orig)
    (set-buffer (get-buffer-create py-extensions))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    (dolist (elt shells)
      (setq prefix "")
      (setq curexe (substring elt (1+ (string-match "/[^/]+$" elt))))
      (setq aktpath (substring elt 0 (1+ (string-match "/[^/]+$" elt))))
      (dolist (prf (split-string aktpath (regexp-quote path-separator)))
        (unless (string= "" prf)
          (setq prefix (concat prefix (substring prf 0 1)))))
      (setq orig (point))
      (insert py-shell-template)
      (setq end (point))
      (goto-char orig)
      (when (re-search-forward "\\<NAME\\>" end t 1)
        (replace-match (concat prefix "-" (substring elt (1+ (save-match-data (string-match "/[^/]+$" elt)))))t))
      (goto-char orig)
      (while (search-forward "DOCNAME" end t 1)
        (replace-match (if (string= "ipython" curexe)
                           "IPython"
                         (capitalize curexe)) t))
      (goto-char orig)
      (when (search-forward "FULLNAME" end t 1)
        (replace-match elt t))
      (goto-char (point-max)))
    (emacs-lisp-mode)
    (if (file-readable-p (concat py-install-directory "/" py-extensions))
        (find-file (concat py-install-directory "/" py-extensions)))))

(provide 'python-components-named-shells)
;;; python-components-named-shells.el ends here
