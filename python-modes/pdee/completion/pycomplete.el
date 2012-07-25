;;; Complete symbols at point using Pymacs.

;; Copyright (C) 2007  Skip Montanaro

;; Original Author: Skip Montanaro
;; Maintainer: Urs Fleisch <ufleisch@users.sourceforge.net>
;; Created:    Oct 2004
;; Keywords:   python pymacs emacs

;; This software is provided as-is, without express or implied warranty.
;; Permission to use, copy, modify, distribute or sell this software,
;; without fee, for any purpose and by any individual or organization, is
;; hereby granted, provided that the above copyright notice and this
;; paragraph appear in all copies.

;; Along with pycomplete.py this file allows programmers to complete Python
;; symbols within the current buffer.  See pycomplete.py for the Python side
;; of things and a short description of what to expect.

(require 'pymacs)

(pymacs-load "pycomplete")

(defun py-complete-enhanced-symbol-before-point ()
  "Return the dotted python symbol before point.

  The symbol is prepended with list, str or dict if it follows such
  a Python literal, e.g. if point is after '[].append', 'list.append'
  will be returned."
  (let* (prefix
         (word (buffer-substring-no-properties
                (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_.")
                  (when (char-equal ?. (char-after))
                    (setq prefix
                          (cond 
                           ((looking-back "\\(\\[\\|,[^[]*\\)\\]") "list")
                           ((looking-back "['\"]") "str")
                           ((looking-back "}") "dict"))))
                  (point))
                (point))))
    (when prefix
      (setq word (concat prefix word)))
    word))

(defalias 'py-find-global-imports 'py-complete-find-global-imports)
(defun py-complete-find-global-imports ()
  "Find Python import statements in buffer."
  (save-excursion
    (let (first-class-or-def imports)
      (goto-char (point-min))
      (setq first-class-or-def
            (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (point-min))
      (setq imports nil)
      (while (re-search-forward
              "^\\(import \\|from \\([A-Za-z_\\.][A-Za-z_0-9\\.]*\\) import \\).*"
              first-class-or-def t)
        (setq imports (cons (buffer-substring-no-properties
                             (match-beginning 0)
                             (match-end 0))
                            imports)))
      (nreverse imports))))

(defun py-complete ()
  "Complete symbol before point using Pymacs. "
  (interactive)
  (let* ((pymacs-forget-mutability t)
         (symbol (py-complete-enhanced-symbol-before-point))
         (completions
          (pycomplete-pycomplete symbol (buffer-file-name)
                                 (py-complete-find-global-imports))))
    (cond ((null completions) ; no matching symbol
           (message "Can't find completion for \"%s\"" symbol)
           (ding))
          ((null (cdr completions))
           (if (string= "" (car completions))
               (tab-to-tab-stop)
             ;; sole completion
             (insert (car completions))))
          (t
           (message "Making completion list...")
           (with-output-to-temp-buffer "*PythonCompletions*"
             (display-completion-list completions))
           (message "Making completion list...%s" "done")))))

(defun py-complete-completions-for-symbol (sym &optional imports)
  "Get possible completions for symbol using statements given in imports."
  (let ((pymacs-forget-mutability t))
    (pycomplete-get-all-completions
     sym (buffer-file-name)
     imports)))

(defun py-complete-docstring-for-symbol (sym &optional imports)
  "Get docstring for symbol using statements given in imports."
  (let ((pymacs-forget-mutability t))
    (pycomplete-pydocstring
     sym (buffer-file-name)
     imports)))

(defun py-complete-completions ()
  "Get possible completions for current statement."
  (py-complete-completions-for-symbol
   (py-complete-enhanced-symbol-before-point) (py-complete-find-global-imports)))

(defun py-complete-python-dotexpr-begin nil
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun py-complete-python-dotexpr-end nil
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)

(defun py-complete-show (string)
  (display-message-or-buffer string "*PythonHelp*"))

(defun py-complete-help (string)
  "get help on a python expression"
  (interactive "sHelp: ")
  (let* ((pymacs-forget-mutability t)
         (help-string
          (pycomplete-pyhelp string (buffer-file-name)
                             (py-complete-find-global-imports))))
    (if (and help-string (> (length help-string) 300))
        (with-output-to-temp-buffer "*Python Help*"
          (princ help-string))
      (py-complete-show help-string))))

(defun py-complete-help-thing-at-point nil
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (py-complete-help sym))))

(set 'py-complete-current-signature nil)

(defun py-complete-signature (function)
  "get signature of a python function or method"
  (let ((pymacs-forget-mutability t))
    (set 'py-complete-current-signature
         (pycomplete-pysignature function (buffer-file-name)
                                 (py-complete-find-global-imports)))))

(defun py-complete-signature-show nil
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (progn
          (py-complete-show (py-complete-signature sym))))))

(defun py-complete-signature-expr nil
  (interactive)
  (require 'thingatpt)
  (let ((dotexpr (read-string "signature on: "
                              (thing-at-point 'python-dotexpr))))
    (if dotexpr
        (py-complete-show
         (py-complete-signature dotexpr)))))

(defun py-complete-electric-lparen nil
  "electricly insert '(', and try to get a signature for the stuff to the left"
  (interactive)
  (py-complete-signature-show)
  (self-insert-command 1))

(defun py-complete-electric-comma nil
  "electricly insert ',', and redisplay latest signature"
  (interactive)
  (self-insert-command 1)
  (if py-complete-current-signature
      (py-complete-show (format "%s" py-complete-current-signature))))

(defun py-complete-goto-definition nil
  "Got to definition of Python function."
  (interactive)
  (require 'thingatpt)
  (let ((sym (thing-at-point 'python-dotexpr)))
    (if sym
        (let ((location
               (pycomplete-pylocation sym (buffer-file-name)
                                      (py-complete-find-global-imports))))
          (if (and location (vectorp location) (= (length location) 2))
              (progn
                (find-file (aref location 0))
                (goto-line (aref location 1)))
            (message "Cannot find the definition!"))))))

(defun py-complete-set-keymap ()
  "Define key map with pycomplete functions."
  (interactive)
  (define-key python-mode-map "\M-\C-i" 'py-complete)
  (define-key python-mode-map "\t" 'py-complete)
  (define-key python-mode-map [f1] 'py-complete-help-thing-at-point)
  (define-key python-mode-map "(" 'py-complete-electric-lparen)
  (define-key python-mode-map "," 'py-complete-electric-comma)
  (define-key python-mode-map [S-f1] 'py-complete-signature-expr)
  (define-key python-mode-map [f2] 'py-complete-goto-definition)
  (define-key python-mode-map [f3] 'py-complete-help))

(provide 'pycomplete)
