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

(defcustom py-complete-set-keymap-p nil
  "If keys shall be defined when calling `py-complete-initialize'.
Default is nil.

See also `py-complete-set-keymap'"
  :type 'boolean
  :group 'python-mode)

(defvar py-complete-variable-index nil
  "An alist with mappings of local variable names to types.")
(defvar py-complete-variable-index-position 0
  "The line-beginning-position when py-complete-variable-index was last updated.
This can be used to skip updating the index when still on the same line.")

(defun py-complete-type-for-value (val)
  "Return name of type for variable assignment value.
If the type cannot be deduced, nil is returned."
  (let ((firstchar (string-to-char val))
        (case-fold-search nil))
    (cond
     ((string= val "None") nil)
     ((equal ?\[ firstchar) "list")
     ((equal ?{ firstchar) "dict")
     ((or (equal ?' firstchar) (equal ?\" firstchar)) "str")
     ((string-match "^[rR]['\"]" val) "str")
     ((string-match "^[uU][rR]?['\"]" val) "unicode")
     ((or (string= val "True") (string= val "False")) "bool")
     ((string-match "^[+\\-]?[0-9]+$" val) "int")
     ((string-match "^[+\\-]?[0-9]+[lL]$" val) "long")
     ((string-match "^[+\\-]?[0-9]+\\(?:\\.[0-9]+\\)?" val) "float")
     ((string-match "^\\(\\(?:[[:word:]\\.]+\\.\\)?_?[A-Z][A-Za-z0-9]+\\)($" val)
      (match-string-no-properties 1 val))
     ((or (string= val "open(") (string= val "file(")) "file"))))

(defun py-complete-variables-in-def (&optional limit)
  "Return an alist with mappings of local variable names to types.
Local variable assignments and parameters of the current function are
parsed. If limit is given, it limits the size of the returned alist."
  (let ((pos (point))
        (i 0)
        (beg 0)
        candidates
        (variable-assignment-re (concat "^[ \t]+\\(\\w+\\)[ \t]*\\(?:=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)[ \t]*\\([({[]\\|[rRuU]*['\"]\\|[+\\-]?[[:word:].]+(?\\)")))
    (save-excursion
      ;; First get the current def and its parameters
      (py-beginning-of-def)
      (when (looking-at (concat py-def-re " *\\([^( ]+\\) *\\(([^:]+\\) *:"))
        (setq beg (match-end 0))
        (let ((params (replace-regexp-in-string
                       "[ )]+$" ""
                       (replace-regexp-in-string
                        "^[ (]+" ""
                        (match-string-no-properties 3)))))
          (dolist (param (split-string params "[ \t\r\n]*,[ \t\r\n]*"))
            ;; Transform param into an assignment string
            (setq param (concat " " param
                                (unless (memq ?= (string-to-list param))
                                  "=None")))
            (if (string-match variable-assignment-re param)
                (push `(,(match-string-no-properties 1 param) .
                        ,(py-complete-type-for-value (match-string-no-properties 2 param)))
                      candidates)))))
      ;; Search backward
      (goto-char pos)
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-backward variable-assignment-re nil t)
                  (> (point) beg))
        (let* ((candidate (match-string-no-properties 1))
               (entry (assoc candidate candidates)))
          (cond ((null entry)
                 (push `(,(match-string-no-properties 1) .
                         ,(py-complete-type-for-value (match-string-no-properties 2)))
                       candidates)
                 (incf i))
                ((not (cdr entry))
                 (setcdr entry (py-complete-type-for-value (match-string-no-properties 2)))))))
      (nreverse candidates))))

(defun py-complete-update-variable-index (&optional limit)
  "Update py-complete-variable-index from the local variables of the current
function. An update is only performed if point was on a different line for
the last update. If limit is given, it limits the size of the index."
  (unless (local-variable-p 'py-complete-variable-index)
    (make-local-variable 'py-complete-variable-index))
  (unless (local-variable-p 'py-complete-variable-index-position)
    (make-local-variable 'py-complete-variable-index-position))
  (let ((linebeg (line-beginning-position)))
    (unless (eq linebeg py-complete-variable-index-position)
      (setq py-complete-variable-index (py-complete-variables-in-def limit))
      (setq py-complete-variable-index-position linebeg))))

(defun py-complete-variable-completions-for-symbol (sym)
  "Get completions for local variables in current def.
If sym is an empty string, all local variables are returned,
else those starting with sym."
  (when (and (stringp sym) (string-match "^\\w*$" sym))
    (py-complete-update-variable-index)
    (let ((symlen (length sym)))
      (if (zerop symlen)
          (mapcar 'car py-complete-variable-index)
        (remove-if-not
         #'(lambda (s) (and (>= (length s) symlen) (string= sym (substring s 0 symlen))))
         (mapcar 'car py-complete-variable-index))))))

(defun py-complete-which-class ()
  "Return current class name based on point.
If no class name is found, return nil."
  (interactive)
  (let (classname)
    (save-excursion
      (save-restriction
        (py-beginning-of-class)
        (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
          (setq classname (match-string-no-properties 2))
          (if (interactive-p)
              (message "%s" classname)))))
    classname))

(defun py-complete-type-before-dotexpr (&optional pos)
  "Get type for expression before dot expression.
The character after pos (point if omitted) must be a dot.
Returns list, str or dict if such an expression is before
the dot, else nil."
  (let ((dotchar (char-after pos)))
    (if (and dotchar (char-equal ?. dotchar))
        (save-excursion
          (if pos
              (goto-char pos))
          (cond
           ((looking-back "\\(\\[\\|,[^[]*\\)\\]") "list")
           ((looking-back "['\"]") "str")
           ((looking-back "}") "dict"))))))

(defun py-complete-substitute-type-for-var (word)
  "Substitute the type for the variable starting the dot-expression word.
Returns the word with replaced variable if known, else the unchanged word."
  (let* (type
         (firstsym (car (split-string word "\\.")))
         (firstlen (length firstsym)))
    (if (string= firstsym "self")
        (setq type (py-complete-which-class))
      (py-complete-update-variable-index)
      (setq type (cdr (assoc firstsym py-complete-variable-index))))
    (if (stringp type)
        (concat type (substring word firstlen))
      word)))

(defun py-complete-python-dotexpr-begin nil
  (re-search-backward "[^a-zA-Z_0-9\\.]")
  (forward-char))

(defun py-complete-python-dotexpr-end nil
  (re-search-forward "[a-zA-Z_0-9\\.]*"))

(put 'python-dotexpr 'beginning-op 'py-complete-python-dotexpr-begin)
(put 'python-dotexpr 'end-op 'py-complete-python-dotexpr-end)

(defun py-complete-enhanced-dotexpr-at-point ()
  "Enhanced (thing-at-point 'python-dotexpr).
The returned word starts with a type if an expression is found before the dot
or if the dot-expression starts with a variable for which the type is known."
  (require 'thingatpt)
  (let ((bounds (bounds-of-thing-at-point 'python-dotexpr)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (word (buffer-substring-no-properties beg end))
               (prefix (py-complete-type-before-dotexpr beg)))
          (if prefix
              (concat prefix word)
            (py-complete-substitute-type-for-var word))))))

(defun py-complete-enhanced-symbol-before-point ()
  "Return the dotted python symbol before point.
The returned word starts with a type if an expression is found before the dot
or if the dot-expression starts with a variable for which the type is known."
  (let* (prefix
         (word (buffer-substring-no-properties
                (save-excursion
                  (skip-chars-backward "a-zA-Z0-9_.")
                  (setq prefix (py-complete-type-before-dotexpr))
                  (point))
                (point))))
    (if prefix
        (concat prefix word)
      (py-complete-substitute-type-for-var word))))

;; Not used anymore
(defun py-find-global-imports ()
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
  (let ((symbol (py-complete-enhanced-symbol-before-point)))
    (if (string= "" symbol)
        (tab-to-tab-stop)
      (let ((completions
             (py-complete-completions-for-symbol symbol)))
        (if completions
            (let* (completion
                   (lastsym (car (last (split-string symbol "\\."))))
                   (lastlen (length lastsym)))
              (cond ((null (cdr completions))
                     (setq completion (car completions)))
                    (t
                     (setq completion (try-completion lastsym completions))
                     (message "Making completion list...")
                     (with-output-to-temp-buffer "*PythonCompletions*"
                       (display-completion-list completions))
                     (message "Making completion list...%s" "done")))
              (when (and (stringp completion)
                         (> (length completion) lastlen))
                (insert (substring completion lastlen))))
          (message "Can't find completion for \"%s\"" symbol)
          (ding))))))

(defun py-complete-completions-for-symbol (sym &optional imports)
  "Get possible completions for symbol using statements given in imports."
  (let ((pymacs-forget-mutability t))
    (append
     (py-complete-variable-completions-for-symbol sym)
     (pycomplete-get-all-completions
      sym (buffer-file-name)
      imports))))

(defun py-complete-docstring-for-symbol (sym &optional imports)
  "Get docstring for symbol using statements given in imports."
  (let ((pymacs-forget-mutability t))
    (pycomplete-pydocstring
     sym (buffer-file-name)
     imports)))

(defun py-complete-completions ()
  "Get possible completions for current statement."
  (py-complete-completions-for-symbol
   (py-complete-enhanced-symbol-before-point)))

(defun py-complete-show (string)
  (display-message-or-buffer string "*PythonHelp*"))

(defun py-complete-help (string)
  "get help on a python expression"
  (interactive "sHelp: ")
  (let* ((pymacs-forget-mutability t)
         (help-string
          (pycomplete-pyhelp string (buffer-file-name))))
    (if (and help-string (> (length help-string) 300))
        (with-output-to-temp-buffer "*Python Help*"
          (princ help-string))
      (py-complete-show help-string))))

(defun py-complete-help-thing-at-point nil
  (interactive)
  (let ((sym (py-complete-enhanced-dotexpr-at-point)))
    (if sym
        (py-complete-help sym))))

(set 'py-complete-current-signature nil)

(defun py-complete-signature (function)
  "get signature of a python function or method"
  (let ((pymacs-forget-mutability t))
    (set 'py-complete-current-signature
         (pycomplete-pysignature function (buffer-file-name)))))

(defun py-complete-signature-show nil
  (let ((sym (py-complete-enhanced-dotexpr-at-point)))
    (if sym
        (progn
          (py-complete-show (py-complete-signature sym))))))

(defun py-complete-signature-expr nil
  (interactive)
  (let ((dotexpr (read-string "signature on: "
                              (py-complete-enhanced-dotexpr-at-point))))
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

(defun py-complete-location (sym)
  "Get definition location of sym in cons form (FILE . LINE)."
  (let ((location (pycomplete-pylocation sym (buffer-file-name))))
    (when (and location (vectorp location) (= (length location) 2))
      (cons (aref location 0) (aref location 1)))))

(defun py-complete-goto-definition nil
  "Got to definition of Python function."
  (interactive)
  (let ((sym (py-complete-enhanced-dotexpr-at-point)))
    (if sym
        (let ((location
               (pycomplete-pylocation sym (buffer-file-name))))
          (if (and location (vectorp location) (= (length location) 2))
              (progn
                (find-file (aref location 0))
                (goto-line (aref location 1)))
            (message "Cannot find the definition!"))))))

(defun py-complete-parse-source ()
  "Parse source code of Python file to get imports and completions."
  (let ((errstr (pycomplete-parse-source (buffer-file-name) t)))
    (if errstr
        (message "%s" errstr))))

(defun py-complete-set-keymap ()
  "Define key map with pycomplete functions."
  (interactive)
  (define-key python-mode-map [C-tab] 'py-complete)
  (define-key python-mode-map [f1] 'py-complete-help-thing-at-point)
  (define-key python-mode-map "(" 'py-complete-electric-lparen)
  (define-key python-mode-map "," 'py-complete-electric-comma)
  (define-key python-mode-map [S-f1] 'py-complete-signature-expr)
  (define-key python-mode-map [f2] 'py-complete-goto-definition)
  (define-key python-mode-map [f3] 'py-complete-help))

(defun py-complete-initialize ()
  "Initialize pycomplete hooks.
Should be called from python-mode-hook. Keys are set when
`py-complete-set-keymap-p' is non-nil."
  (interactive)
  (when py-set-complete-keymap-p
    (py-complete-set-keymap))
	(when py-complete-set-keymap-p
		(py-complete-set-keymap))
  ;; Parse source file after it is saved
  (add-hook 'after-save-hook 'py-complete-parse-source nil 'local)
  ;; Set up auto-complete or company if enabled
  (cond
   ((fboundp 'auto-complete-mode)
    (require 'auto-complete-pycomplete)
    (setq ac-sources '(ac-source-pycomplete)))
   ((fboundp 'company-mode)
    (company-mode t)
    (require 'company-pycomplete)
    (set (make-local-variable 'company-backends)
         '((company-pycomplete))))))

(provide 'pycomplete)
