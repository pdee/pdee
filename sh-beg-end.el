;;; sh-beg-end.el --- Something for C-M-a,
;;; C-M-e, M-a and M-e in shell-script-mode

;; Copyright (C) 2007, 2008, 2009, 2010 by Andreas Roehler
;; <andreas.roehler@online.de>

;; Keywords: languages

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General
;; Public License along with GNU Emacs; see the file
;; COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;; Major changes to previous version: 
;; use beginning-of-defun, set beginning-of-defun-function

;;; Commentary:

;; M-p, M-n: jump to the beginning or end of a
;; top-level-form 

;; C-M-a, C-M-e: jump to the beginning or end of a
;; function in sh-mode - "Shell-script"-mode.  

;; M-a, M-e: jump to the beginning or end of statement in
;; a given line, forward or backward next beginning or
;; end.  With argument do this as many times.


;;; Code:

(require 'sh-script)
(require 'misc-utils)


;; Some suggestions for keys
(define-key sh-mode-map "\M-a" 'sh-beginning-of-statement)
(define-key sh-mode-map "\M-e" 'sh-end-of-statement)
;; (define-key sh-mode-map "\C-\M-a" 'sh-beginning-of-function)
;; (define-key sh-mode-map "\C-\M-e" 'sh-end-of-function)
(define-key sh-mode-map "\M-p" 'sh-beginning-of-form)
(define-key sh-mode-map "\M-n" 'sh-end-of-form)
;; (define-key sh-mode-map "%" 'sh-match-paren)

;; (setq ar-beginning-sh-struct-atpt "\\bcase\\b\\|\\bfor\\b\\|\\bfunction\\b\\|\\bif\\b\\|\\bselect\\b\\|\\buntil\\b\\|\\bwhile\\b")

(setq ar-beginning-sh-struct-atpt "\\(\\bcase\\b\\)\\|\\(\\bfor\\b\\)\\|\\(\\bfunction\\b\\)\\|\\(\\bif\\b\\)\\|\\(\\bselect\\b\\)\\|\\(\\buntil\\b\\)\\|\\(\\bwhile\\b\\)")

(defcustom ar-beginning-sh-struct-atpt "\\bcase\\b\\|\\bfor\\b\\|\\bfunction\\b\\|\\bif\\b\\|\\bselect\\b\\|\\buntil\\b\\|\\bwhile\\b"
  "Specify the sh-struct beginning."
  :type 'regexp
  :group 'convenience)

(setq ar-end-sh-struct-atpt "\\bdone\\b\\|\\besac\\b\\|\\bfi\\b") 
(setq sh-match-paren-char "%")

(setq sh-beginning-of-function-regexp "^[A-Za-z_][A-Za-z_0-9]* *() *{")
(defcustom sh-beginning-of-function-regexp "^[A-Za-z_][A-Za-z_0-9]* *() *{"
  "Regexp indicating the beginning of a function to edit."
  :type 'regexp
  :group 'lisp)

(defcustom sh-end-of-function-regexp "^}[ \t]*$\\|;}[ \t]*$"
  "Regexp indicating the end of a function to edit."
  :type 'regexp
  :group 'lisp)
      
(add-hook 'sh-mode-hook '(lambda () (set (make-local-variable 'beginning-of-defun-function) 'sh-beginning-of-function)))

(add-hook 'sh-mode-hook '(lambda () (set (make-local-variable 'end-of-defun-function) 'sh-end-of-function)))

(defun sh-beginning-of-statement (&optional arg)
  "Move point to ARG successive beginnings of commands."
  (interactive "p")
  (let ((arg (or arg 1))
	(pos (point)))
    (back-to-indentation)
    (unless (eq pos (point))
      (setq arg (1- arg)))
    (while (< 0 arg)
      (forward-line (- arg))
      (setq arg (1- arg))
      ;; skip comments and empty lines and closing braces
      (let ((pos (point)))
	(if (forward-comment -1)
	    (while (forward-comment -1) (forward-comment -1))
	  (goto-char pos)))
      (while (or (empty-line-p)
		 (looking-at "}"))
	(forward-line -1))
      (back-to-indentation))))

(defun sh-end-of-statement (&optional arg)
  "Move point to ARG successive ends of commands."
  (interactive "p")
  (let ((arg (or arg 1))
	(pos (point)))
    (end-of-line)
    (skip-chars-backward " \t\r\n" (line-beginning-position))
    (unless (eq pos (point))
      (setq arg (1- arg)))
    (while (< 0 arg)
      (forward-line arg)
      (setq arg (1- arg)))
    (end-of-line)
    (skip-chars-backward " \t\r\n" (line-beginning-position))
    (while (or
	    (empty-line-p)
	    (forward-comment 1))
      (forward-line 1)
      (end-of-line))
    (skip-chars-backward " \t\r\n")))

(defun sh-mark-function ()
  " "
  (interactive)
  (sh-beginning-of-form)
  (push-mark (point) t t)
  (sh-end-of-form)
  (kill-new (buffer-substring-no-properties (mark) (point))))

(defun sh-kill-function ()
  " "
  (interactive)
  (sh-beginning-of-form)
  (push-mark (point) t t)
  (sh-end-of-form)
  (kill-region (mark) (point)))

(defun sh-beginning-of-form (&optional arg bound noerror count comment quote) 
  "Goto opening of a programming structure in this level.
Defaults for options are:
arg 1
bound nil
noerror t
comment nil
quote nil
 "
  (interactive "p")
  (let ((arg (or arg 1))
        ;; (bound (or bound nil))
        (noerror (or noerror t))
        ;; (comment (or comment nil))
        ;; (quote (or quote nil))
        )
    (beginning-of-form-base ar-beginning-sh-struct-atpt ar-end-sh-struct-atpt bound noerror arg comment quote)
    (when (interactive-p) (message "%s" (point))) (point)))

(defun sh-end-of-form (&optional arg iact bound noerror comment quote) 
  "Goto opening of a programming structure in this level. 
Defaults for options are:
arg 1
bound nil
noerror t
comment nil
quote nil
"
  (interactive "p")
  (let ((arg (or arg 1))
        ;; (bound (or bound nil))
        (noerror (or noerror t))
        ;; (comment (or comment nil))
        ;; (quote (or quote nil))
)
    (when (looking-at ar-beginning-sh-struct-atpt)
      (goto-char (match-end 0))) 
    (end-of-form-base ar-beginning-sh-struct-atpt ar-end-sh-struct-atpt bound noerror arg comment quote)
    (forward-char -1)
    (when (interactive-p) (message "%s" (point)))(point)))	

(defun sh-beginning-of-function (&optional arg bound noerror comment quote) 
  "Goto opening of a functions definition. 
Defaults for options are:
arg 1
bound nil
noerror t
comment nil
quote nil
"
  (interactive "p")
  (let ((arg (or arg 1))
        (bound (or bound nil))
        (noerror (or noerror t))
        (comment (or comment nil))
        (quote (or quote nil)))
    (beginning-of-form-base sh-beginning-of-function-regexp nil bound t arg nil t)
    (when (looking-at sh-beginning-of-function-regexp)
      (when (interactive-p) (message "%s" (point))) (point))))

(defun sh-end-of-function (&optional arg bound noerror comment quote) 
  "Goto end of a functions definition. 
Defaults for options are:
arg 1
bound nil
noerror t
comment nil
quote nil
"
  (interactive "p")
  (let ((arg (or arg 1))
        (bound (or bound nil))
        (noerror (or noerror t))
        (comment (or comment nil))
        (quote (or quote nil)))
    (end-of-form-base nil sh-end-of-function-regexp bound t arg)
    (when (interactive-p) (message "%s" (point))) (point)))

(defvar sh-match-paren-key-char "%")

(defun sh-match-paren ()
  "Go to the matching opening/closing. 
First to opening, unless cursor is already there. "
  (interactive)
  (if (looking-at ar-beginning-sh-struct-atpt)
      (sh-end-of-form)
    (sh-beginning-of-form)))

(defvar sh-match-paren-mode nil)

(provide 'sh-beg-end)

;;; sh-beg-end.el ends here






