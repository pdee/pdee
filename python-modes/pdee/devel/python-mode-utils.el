;;; python-mode-shell-install.el --- Installing python, python3, ipython and other python shells

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

;; Python-components-mode started from python-mode.el
;; and python.el, where Tim Peters, Barry A. Warsaw,
;; Skip Montanaro, Ken Manheimer, Dave Love and many
;; others wrote major parts. Author of ipython.el's
;; stuff merged is Alexander Schmolck.

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

;; Provides utilities creating python-mode commands

;;; Code:

(defcustom py-shells
  '("python" "python2" "python2.7" "python3" "python3.2" "ipython" "jython")
  "Python-mode will generate commands opening shells mentioned here. Edit this list \w resp. to your machine. "
  :type '(repeat string)
  :group 'python)

(setq py-shells '("python" "python2" "python2.7" "python3" "python3.2" "ipython" "jython"))

(defun py-provide-installed-shells-commands ()
  "Reads py-shells, provides commands opening these shell. "
  (interactive)
  (let ((temp-buffer "*Python Shell Install Buffer*"))
    (set-buffer (get-buffer-create temp-buffer))
    (erase-buffer) 
    (insert ";;; Python named shells")
    (newline)
    (dolist (ele py-shells)
      (insert (concat "(defun " ele " (&optional argprompt)
  \"Start an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter in another window.
   With optional \\\\[universal-argument] user is prompted
    for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt nil \"" ele "\"))\n\n")))
    (insert ";; dedicated\n")
    (dolist (ele py-shells)
      (insert (concat "(defun " ele "-dedicated (&optional argprompt)
  \"Start an "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " dedicated interpreter in another window.
   With optional \\\\[universal-argument] user is prompted
    for options to pass to the "))
      (if (string= "ipython" ele)
          (insert "IPython")
        (insert (capitalize ele)))
      (insert (concat " interpreter. \"
  (interactive)
  (py-set-shell-completion-environment)
  (py-shell argprompt t \"" ele "\"))\n\n"))))
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer)))

(defun py-write-beginning-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "py-write-beginning-position-forms"))
  (erase-buffer)
      (dolist (ele py-shift-forms)
        (insert "
(defun py-beginning-of-" ele "-position ()
  \"Returns beginning of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))
")))

(defun py-write-end-position-forms ()
  (interactive)
  (set-buffer (get-buffer-create "py-write-end-position-forms"))
  (erase-buffer)
      (dolist (ele py-shift-forms)
        (insert "
(defun py-end-of-" ele "-position ()
  \"Returns end of " ele " position. \"
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-" ele ")))
      (when (interactive-p) (message \"%s\" erg))
      erg)))
")))

(setq py-shift-forms (list "paragraph" "block" "clause" "def" "class" "line" "statement"))

(defun py-write-shift-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-shift-forms"))
  (erase-buffer)
      (dolist (ele py-shift-forms)
        (insert (concat "
\(defun py-shift-" ele "-right (&optional arg)
  \"Indent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py-shift-forms-base \"" ele "\" (or arg py-indent-offset))))
        (when (interactive-p) (message \"%s\" erg))
    erg))

\(defun py-shift-" ele "-left (&optional arg)
  \"Dedent " ele " by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \\[universal-argument] to specify a different value.

Returns outmost indentation reached. \"
  (interactive \"\*P\")
  (let ((erg (py-shift-forms-base \"" ele "\" (- (or arg py-indent-offset)))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
"))
  (emacs-lisp-mode)
  (switch-to-buffer (current-buffer))))

(setq py-down-forms (list "block" "clause" "block-or-clause" "def" "class" "def-or-class"))

(defun py-write-down-forms-lc ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-down-forms-lc.el"))
  (erase-buffer)
      (dolist (ele py-down-forms)
        (insert (concat "
\(defun py-down-" ele "-lc ()
  \"Goto beginning of line following end of " ele ".
  Returns position reached, if successful, nil otherwise.

\\\"-lc\\\" stands for \\\"left-corner\\\" - a complementary command travelling left, whilst `py-end-of-" ele "' stops at right corner.

See also `py-down-" ele "': down from current definition to next beginning of " ele " below. \"
  (interactive)
  (let ((erg (py-end-of-" ele ")))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
        (emacs-lisp-mode)
        (switch-to-buffer (current-buffer))))

(defun py-write-down-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-down-forms.el"))
  (erase-buffer)
      (dolist (ele py-down-forms)
        (insert (concat "
\(defun py-down-" ele " ()
  \"Go to the beginning of next " ele " below in buffer.

Returns indentation if " ele " found, nil otherwise. \"
  (interactive)
  (let\* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(or (py-in-string-or-comment-p)(not (looking-at py-" ele "-re))))))
    (when (interactive-p) (message \"%s\" erg))
    erg))
"))
        (emacs-lisp-mode)
        (switch-to-buffer (current-buffer))))

(defun py-write-up-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "py-up-forms"))
  (erase-buffer)
  (dolist (ele py-down-forms)
    (insert (concat "
\(defun py-up-" ele " ()
  \"Goto end of line preceding beginning of " ele ".
  Returns position reached, if successful, nil otherwise.

A complementary command travelling right, whilst `py-beginning-of-" ele "' stops at left corner. \"
  (interactive)
  (let ((erg (py-beginning-of-" ele ")))
    (when erg
      (unless (bobp)
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward \" \\t\\r\\n\\f\")
        (setq erg (point))))
  (when (interactive-p) (message \"%s\" erg))
  erg))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(defun py-write-specifying-shell-forms ()
  " "
  (interactive)
  (set-buffer (get-buffer-create "specifying-shell-forms"))
  (erase-buffer)
  (dolist (ele py-shells)
    (insert (concat "
(defun py-execute-region-" ele " (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter. \"
  (interactive \"r\\nP\")
  (py-execute-base start end async \"" ele "\"))

(defun py-execute-region-" ele "-switch (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. \"
  (interactive \"r\\nP\")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async \"" ele "\")))

(defun py-execute-region-" ele "-no-switch (start end &optional async)
  \"Send the region to a common shell calling the " ele " interpreter.
Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to.\"
  (interactive \"r\\nP\")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async \"" ele "\")))
"))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))))

(provide 'python-mode-shell-install)
;;; python-mode-shell-install.el ends here
