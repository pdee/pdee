;;; python-components-map.el --- Install a python-mode-map -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>

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

;;

;;; Code:

(defvar py-use-menu-p t
  "If the menu should be loaded.

Default is t")

(defvar py-menu nil
  "Make a dynamically bound variable ‘py-menu’.")

(defvar python-mode-map nil)
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; (define-key map [(control y)] 'py-electric-yank)
        ;; moving point
        (define-key map [(control c) (control p)] 'py-backward-statement)
        (define-key map [(control c) (control n)] 'py-forward-statement)
        (define-key map [(control c) (control u)] 'py-backward-block)
        (define-key map [(control c) (control q)] 'py-forward-block)
        (define-key map [(control meta a)] 'py-backward-def-or-class)
        (define-key map [(control meta e)] 'py-forward-def-or-class)
        ;; (define-key map [(meta i)] 'py-indent-forward-line)
        (define-key map [(control j)] 'py-newline-and-indent)
        ;; Most Pythoneers expect RET `py-newline-and-indent'
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
        (define-key map (kbd "RET") 'py-return-key)
        ;; (define-key map (kbd "RET") 'newline)
        (define-key map [(super backspace)] 'py-dedent)
        ;; (define-key map [(control return)] 'py-newline-and-dedent)
        ;; indentation level modifiers
        (define-key map [(control c) (control l)] 'py-shift-left)
        (define-key map [(control c) (control r)] 'py-shift-right)
        (define-key map [(control c) (<)] 'py-shift-left)
        (define-key map [(control c) (>)] 'py-shift-right)
        ;; (define-key map [(control c) (tab)] 'py-indent-region)
	(define-key map (kbd "C-c TAB") 'py-indent-region)
        (define-key map [(control c) (:)] 'py-guess-indent-offset)
        ;; subprocess commands
        (define-key map [(control c) (control c)] 'py-execute-buffer)
        (define-key map [(control c) (control m)] 'py-execute-import-or-reload)
        (define-key map [(control c) (control s)] 'py-execute-string)
        (define-key map [(control c) (|)] 'py-execute-region)
        (define-key map [(control meta x)] 'py-execute-def-or-class)
        (define-key map [(control c) (!)] 'py-shell)
        (define-key map [(control c) (control t)] 'py-toggle-shell)
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c) (control k)] 'py-mark-block-or-clause)
        (define-key map [(control c) (.)] 'py-expression)
        ;; Miscellaneous
        ;; (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c) (control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c) (control f)] 'py-sort-imports)
        (define-key map [(control c) (\#)] 'py-comment-region)
        (define-key map [(control c) (\?)] 'py-describe-mode)
        (define-key map [(control c) (control e)] 'py-help-at-point)
        (define-key map [(control c) (-)] 'py-up-exception)
        (define-key map [(control c) (=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-def-or-class)
        ;; information
        (define-key map [(control c) (control b)] 'py-submit-bug-report)
        (define-key map [(control c) (control v)] 'py-version)
        (define-key map [(control c) (control w)] 'py-pychecker-run)
        ;; (define-key map (kbd "TAB") 'py-indent-line)
        (define-key map (kbd "TAB") 'py-indent-or-complete)
	;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] 'py-shell-complete)
        ;;   (define-key map [(esc) (tab)] 'py-shell-complete))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (substitute-key-definition 'backward-up-list 'py-up
                                   map global-map)
        (substitute-key-definition 'down-list 'py-down
                                   map global-map)
	(when py-use-menu-p
	  (setq map (py-define-menu map)))
        map))

(defvar py-python-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'comint-send-input)
    (define-key map [(control c)(-)] 'py-up-exception)
    (define-key map [(control c)(=)] 'py-down-exception)
    (define-key map (kbd "TAB") 'py-indent-or-complete)
    (define-key map [(meta tab)] 'py-shell-complete)
    (define-key map [(control c)(!)] 'py-shell)
    (define-key map [(control c)(control t)] 'py-toggle-shell)
    ;; electric keys
    ;; (define-key map [(:)] 'py-electric-colon)
    ;; (define-key map [(\#)] 'py-electric-comment)
    ;; (define-key map [(delete)] 'py-electric-delete)
    ;; (define-key map [(backspace)] 'py-electric-backspace)
    ;; (define-key map [(control backspace)] 'py-hungry-delete-backwards)
    ;; (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
    ;; (define-key map [(control y)] 'py-electric-yank)
    ;; moving point
    (define-key map [(control c)(control p)] 'py-backward-statement)
    (define-key map [(control c)(control n)] 'py-forward-statement)
    (define-key map [(control c)(control u)] 'py-backward-block)
    (define-key map [(control c)(control q)] 'py-forward-block)
    (define-key map [(control meta a)] 'py-backward-def-or-class)
    (define-key map [(control meta e)] 'py-forward-def-or-class)
    (define-key map [(control j)] 'py-newline-and-indent)
    (define-key map [(super backspace)] 'py-dedent)
    ;; (define-key map [(control return)] 'py-newline-and-dedent)
    ;; indentation level modifiers
    (define-key map [(control c)(control l)] 'comint-dynamic-list-input-ring)
    (define-key map [(control c)(control r)] 'comint-previous-prompt)
    (define-key map [(control c)(<)] 'py-shift-left)
    (define-key map [(control c)(>)] 'py-shift-right)
    (define-key map [(control c)(tab)] 'py-indent-region)
    (define-key map [(control c)(:)] 'py-guess-indent-offset)
    ;; subprocess commands
    (define-key map [(control meta h)] 'py-mark-def-or-class)
    (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
    (define-key map [(control c)(.)] 'py-expression)
    ;; Miscellaneous
    ;; (define-key map [(super q)] 'py-copy-statement)
    (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
    (define-key map [(control c)(\#)] 'py-comment-region)
    (define-key map [(control c)(\?)] 'py-describe-mode)
    (define-key map [(control c)(control e)] 'py-help-at-point)
    (define-key map [(control x) (n) (d)] 'py-narrow-to-def-or-class)
    ;; information
    (define-key map [(control c)(control b)] 'py-submit-bug-report)
    (define-key map [(control c)(control v)] 'py-version)
    (define-key map [(control c)(control w)] 'py-pychecker-run)
    (substitute-key-definition 'complete-symbol 'completion-at-point
			       map global-map)
    (substitute-key-definition 'backward-up-list 'py-up
			       map global-map)
    (substitute-key-definition 'down-list 'py-down
			       map global-map)
    map)
  "Used inside a Python-shell.")

(defvar py-ipython-shell-mode-map py-python-shell-mode-map
  "Unless setting of ipython-shell-mode needs to be different, let's save some lines of code and copy ‘py-python-shell-mode-map’ here.")

(defvar py-shell-map py-python-shell-mode-map)

(setq python-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or
	      "if" "and" "del"  "not" "while" "as" "elif" "global"
	      "or" "async with" "with" "assert" "else"  "pass" "yield" "break"
	      "exec" "in" "continue" "finally" "is" "except" "raise"
	      "return"  "async for" "for" "lambda" "await")
             symbol-end)
        (,(rx symbol-start (or "async def" "def" "class") symbol-end) . py-def-class-face)
        (,(rx symbol-start (or "import" "from") symbol-end) . py-import-from-face)
        (,(rx symbol-start (or "try" "if") symbol-end) . py-try-if-face)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        (,(rx symbol-start "async def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
         (1 py-def-class-face) (2 py-class-name-face))
        (,(rx symbol-start
              (or "Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented")
              symbol-end) . py-pseudo-keyword-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 py-decorators-face))
	(,(rx symbol-start (or "cls" "self")
	      symbol-end) . py-object-reference-face)

        ;; Exceptions
        (,(rx word-start
              (or "ArithmeticError" "AssertionError" "AttributeError"
                  "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
                  "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
                  "FutureWarning" "GeneratorExit" "IOError" "ImportError"
                  "ImportWarning" "IndentationError" "IndexError" "KeyError"
                  "KeyboardInterrupt" "LookupError" "MemoryError" "NameError" "NoResultFound"
                  "NotImplementedError" "OSError" "OverflowError"
                  "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
                  "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
                  "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
                  "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
                  "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
                  "UserWarning" "ValueError" "Warning" "ZeroDivisionError"
                  ;; OSError subclasses
                  "BlockIOError" "ChildProcessError" "ConnectionError"
                  "BrokenPipError" "ConnectionAbortedError"
                  "ConnectionRefusedError" "ConnectionResetError"
                  "FileExistsError" "FileNotFoundError" "InterruptedError"
                  "IsADirectoryError" "NotADirectoryError" "PermissionError"
                  "ProcessLookupError" "TimeoutError")
              word-end) . py-exception-name-face)
        ;; Builtins
        (,(rx
	   (or space line-start (not (any ".")))
	   symbol-start
	   (group (or "_" "__doc__" "__import__" "__name__" "__package__" "abs" "all"
		      "any" "apply" "basestring" "bin" "bool" "buffer" "bytearray"
		      "bytes" "callable" "chr" "classmethod" "cmp" "coerce" "compile"
		      "complex" "delattr" "dict" "dir" "divmod" "enumerate" "eval"
		      "execfile" "filter" "float" "format" "frozenset"
		      "getattr" "globals" "hasattr" "hash" "help" "hex" "id" "input"
		      "int" "intern" "isinstance" "issubclass" "iter" "len" "list"
		      "locals" "long" "map" "max" "min" "next" "object" "oct" "open"
		      "ord" "pow" "property" "range" "raw_input" "reduce"
		      "reload" "repr" "reversed" "round" "set" "setattr" "slice"
		      "sorted" "staticmethod" "str" "sum" "super" "tuple" "type"
		      "unichr" "unicode" "vars" "xrange" "zip"))
	   symbol-end) (1 py-builtins-face))
        ("\\([._[:word:]]+\\)\\(?:\\[[^]]+]\\)?[[:space:]]*\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
         (1 py-variable-name-face nil nil))
	;; https://emacs.stackexchange.com/questions/55184/
	;; how-to-highlight-in-different-colors-for-variables-inside-fstring-on-python-mo
	;;
	;; this is the full string.
        ;; group 1 is the quote type and a closing quote is matched
        ;; group 2 is the string part
        ("f\\(['\"]\\{1,3\\}\\)\\([^\\1]+?\\)\\1"
         ;; these are the {keywords}
         ("{[^}]*?}"
          ;; Pre-match form
          (progn (goto-char (match-beginning 0)) (match-end 0))
          ;; Post-match form
          (goto-char (match-end 0))
          ;; face for this match
          (0 font-lock-variable-name-face t)))
	;; assignment
        ;; a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (rx (group (+ (any word ?. ?_))) (* space)
			  (* ?, (* space) (+ (any word ?. ?_)) (* space))
			  (or ":" "=" "+=" "-=" "*=" "/=" "//=" "%=" "**=" ">>=" "<<=" "&=" "^=" "|=")))
                  (res nil))
              (while (and (setq res (re-search-forward re limit t))
                          (goto-char (match-end 1))
                          (nth 1 (parse-partial-sexp (point-min) (point)))
                          ;; (python-syntax-context 'paren)
			  ))
              res))
         (1 py-variable-name-face nil nil))
        ;; Numbers
	;;        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)
	(,(rx symbol-start (1+ digit) symbol-end) . py-number-face)))

(provide 'python-components-map)

;;; python-components-map.el ends here
