;;; python-components-font-lock.el --- Install a python-mode-map -*- lexical-binding: t; -*-


;; URL: https://gitlab.com/python-mode-devs

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
;; (require 'python)

(defmacro py-rx (&rest regexps)
  "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
  `(rx-let ((block-start       (seq symbol-start
                                    (or "def" "class" "if" "elif" "else" "try"
                                        "except" "finally" "for" "while" "with"
                                        ;; Python 3.10+ PEP634
                                        "match" "case"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space)
                                             (or "def" "for" "with")))
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else" "except" "finally")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
            (decorator         (seq line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
            (defun             (seq symbol-start
                                    (or "def" "class"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space) "def"))
                                    symbol-end))
            (if-name-main      (seq line-start "if" (+ space) "__name__"
                                    (+ space) "==" (+ space)
                                    (any ?' ?\") "__main__" (any ?' ?\")
                                    (* space) ?:))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (assignment-target (seq (? ?*)
                                    (* symbol-name ?.) symbol-name
                                    (? ?\[ (+ (not ?\])) ?\])))
            (grouped-assignment-target (seq (? ?*)
                                            (* symbol-name ?.) (group symbol-name)
                                            (? ?\[ (+ (not ?\])) ?\])))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not (or simple-operator ?\n)))
            (operator          (or "==" ">=" "is" "not"
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            (coding-cookie (seq line-start ?# (* space)
                                (or
                                 ;; # coding=<encoding name>
                                 (: "coding" (or ?: ?=) (* space)
                                    (group-n 1 (+ (or word ?-))))
                                 ;; # -*- coding: <encoding name> -*-
                                 (: "-*-" (* space) "coding:" (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) "-*-")
                                 ;; # vim: set fileencoding=<encoding name> :
                                 (: "vim:" (* space) "set" (+ space)
                                    "fileencoding" (* space) ?= (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) ":")))))
     (rx ,@regexps)))

(defun py-font-lock-assignment-matcher (regexp)
  "Font lock matcher for assignments based on REGEXP.
Search for next occurrence if REGEXP matched within a `paren'
context (to avoid, e.g., default values for arguments or passing
arguments by name being treated as assignments) or is followed by
an '=' sign (to avoid '==' being treated as an assignment.  Set
point to the position one character before the end of the
occurrence found so that subsequent searches can detect the '='
sign in chained assignment."
  (lambda (limit)
    (cl-loop while (re-search-forward regexp limit t)
             unless (or
                     ;; (python-syntax-context 'paren)
                     (nth 1 (parse-partial-sexp (point-min) (point))) 
                        (equal (char-after) ?=))
               return (progn (backward-char) t))))

(defconst python-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "if" "and" "del"  "not" "while" "as" "elif" "global"
          "or" "async with" "with" "assert" "else"  "pass" "yield" "break"
          "exec" "in" "continue" "finally" "is" "except" "raise"
          "return"  "async for" "for" "lambda" "await" "match" "case")
         symbol-end)
    (,(rx symbol-start (or "async def" "def" "class") symbol-end) . py-def-class-face)
    (,(rx symbol-start (or "import" "from") symbol-end) . py-import-from-face)
    (,(rx symbol-start (or "try" "if") symbol-end) . py-try-if-face)
    ;; functions
    (,(rx symbol-start "def" (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     ;; (1 font-lock-function-name-face))
     (1 py-def-face))
    (,(rx symbol-start "async def" (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     ;; (1 font-lock-function-name-face))
     (1 py-def-face))
    ;; classes
    (,(rx symbol-start (group "class") (1+ space) (group (seq (any letter ?_) (* (any word ?_)))))
     (1 py-def-class-face) (2 py-class-name-face))
    (,(rx symbol-start
          (or"Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented") symbol-end) . py-pseudo-keyword-face)
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
                  "unichr" "unicode" "vars" "xrange" "zip")) symbol-end) . (1 py-builtins-face))
    ;; #104, GNU bug 44568 font lock of assignments with type hints
    ;; ("\\([._[:word:]]+\\)\\(?:\\[[^]]+]\\)?[[:space:]]*\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
    ;;  (1 py-variable-name-face nil nil))
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
      ;; (0 font-lock-variable-name-face t)))
      (0 py-variable-name-face t)))
    ;; assignment
    ;; a, b, c = (1, 2, 3)
    ;; a, *b, c = range(10)
    ;; inst.a, inst.b, inst.c = 'foo', 'bar', 'baz'
    ;; (a, b, *c, d) = x, *y = 5, 6, 7, 8, 9
    (,(py-font-lock-assignment-matcher
       (py-rx line-start (* space) (? (or "[" "("))
                  grouped-assignment-target (* space) ?, (* space)
                  (* assignment-target (* space) ?, (* space))
                  (? assignment-target (* space))
                  (? ?, (* space))
                  (? (or ")" "]") (* space))
                  (group assignment-operator)))
     (1 py-variable-name-face)
     (,(py-rx grouped-assignment-target)
      (progn
        (goto-char (match-end 1))       ; go back after the first symbol
        (match-beginning 2))            ; limit the search until the assignment
      nil
      (1 py-variable-name-face)))
    (,(lambda (limit)
        (let ((re (rx (* "self")(group (+ (any word ?. ?_))) (* space)
                      (* ?, (* space) (+ (any word ?. ?_)) (* space))
                      (or ":" "=" "+=" "-=" "*=" "/=" "//=" "%=" "**=" ">>=" "<<=" "&=" "^=" "|=")))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (goto-char (match-end 1))
                      (nth 1 (parse-partial-sexp (point-min) (point)))
                      ;; (python-syntax-context 'paren)
		      ))
          res)) . (1 py-variable-name-face nil nil))
    ;; Numbers
    ;;        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)
    (,(rx symbol-start (1+ digit) symbol-end) . py-number-face))
  "Keywords matching font-lock")

(provide 'python-components-font-lock)

;;; python-components-font-lock.el ends here
