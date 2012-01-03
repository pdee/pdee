;;; python-mode.el --- Towards an Python-IDE in Emacs

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2003-2011 https://launchpad.net/python-mode
;;         1995-2002 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

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

;;; Code

(require 'comint)
(require 'custom)
(eval-when-compile (require 'cl))
(require 'compile)
(require 'ansi-color)
(require 'cc-cmds)
(require 'shell)
(require 'help-fns)

(unless (featurep 'xemacs)
  (require 'highlight-indentation))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))
(add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))
(add-to-list 'interpreter-mode-alist (cons (purecopy "jython") 'jython-mode))
(add-to-list 'same-window-buffer-names (purecopy "*Python*"))

(defgroup python nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "6.0.5")

(defsubst py-in-string-or-comment-p ()
  "Return beginning position if point is in a Python literal (a comment or string)."
  (nth 8 (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (syntax-ppss))))

;;; Bindings

(defconst python-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?:\\('\\)\\('\\)\\('\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\)")
     (1 (python-quote-syntax 1) nil lax)
     (2 (python-quote-syntax 2))
     (3 (python-quote-syntax 3)))
    ;; This doesn't really help.
;;;     (,(rx (and ?\\ (group ?\n))) (1 " "))
    ))

(defun python-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.)  We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.

  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
	     (syntax (syntax-ppss)))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  ;; (skip-chars-forward "uUrR")	; skip any prefix
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)			; leading quote (not prefix)
	       (not (match-end 1)))     ; prefix is null
	  (and (= n 1)			; prefix
	       (match-end 1)))          ; non-empty
      (let ((font-lock-syntactic-keywords nil))
	(unless (eq 'string (syntax-ppss-context (syntax-ppss)))
	  (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
)))


;;;; Keymap and syntax

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")

(defvar python-shell-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map [tab]   'tab-to-tab-stop)
    (define-key map "\C-c-" 'py-up-exception)
    (define-key map "\C-c=" 'py-down-exception)
    map)
  "Keymap used in *Python* shell buffers.")

(defvar python-mode-syntax-table nil
    "Syntax table for Python files.")

(setq python-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (let ((symbol (string-to-syntax "_"))
              (sst (standard-syntax-table)))
          (dotimes (i 128)
            (unless (= i ?_)
              (if (equal symbol (aref sst i))
                  (modify-syntax-entry i "." table)))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (modify-syntax-entry ?_ "w" table)
        table))

(defvar py-menu)
(defvar python-mode-map)
(setq python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Mostly taken from python-mode.el.
    (define-key map [(:)] 'py-electric-colon)
    (define-key map [(\#)] 'py-electric-comment)
    (define-key map [(delete)] 'py-electric-delete)
    (define-key map [(backspace)] 'py-electric-backspace)
    (define-key map [(control backspace)] 'py-hungry-delete-backwards)
    ;; moving point
    (define-key map [(control c)(control n)] 'py-end-of-statement)
    (define-key map [(control c)(control a)] 'py-mark-statement)
    (define-key map [(control c)(control p)] 'py-beginning-of-statement)
    (define-key map [(control c)(control u)] 'py-beginning-of-block)
    (define-key map [(control c)(control q)] 'py-end-of-block)
    (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
    (define-key map [(control meta e)] 'py-end-of-def-or-class)
    (define-key map [(control j)] 'py-newline-and-indent)
    ;; Most Pythoneers expect RET `py-newline-and-indent'
    (define-key map (kbd "RET") 'py-newline-and-indent)
    (define-key map [(super backspace)] 'py-dedent)
    ;; (define-key map [(control return)] 'py-newline-and-dedent)
    ;; indentation level modifiers
    (define-key map [(control c)(control l)] 'py-shift-left)
    (define-key map [(control c)(control r)] 'py-shift-right)
    (define-key map [(control c)(<)] 'py-shift-left)
    (define-key map [(control c)(>)] 'py-shift-right)
    (define-key map [(control c)(tab)] 'py-indent-region)
    (define-key map [(control c)(:)] 'py-guess-indent-offset)
    ;; subprocess commands
    (define-key map [(control c)(control c)] 'py-execute-buffer)
    (define-key map [(control c)(control m)] 'py-execute-import-or-reload)
    (define-key map [(control c)(control s)] 'py-execute-string)
    (define-key map [(control c)(|)] 'py-execute-region)
    (define-key map [(control meta x)] 'py-execute-def-or-class)
    (define-key map [(control c)(!)] 'py-shell)
    (define-key map [(control c)(control t)] 'py-toggle-shells)
    (define-key map [(control meta h)] 'py-mark-def-or-class)
    (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
    ;; Miscellaneous
    (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
    (define-key map [(control c)(control f)] 'py-sort-imports)
    (define-key map [(control c)(\#)] 'py-comment-region)
    (define-key map [(control c)(\?)] 'py-describe-mode)
    (define-key map [(control c)(control e)] 'py-describe-symbol)
    (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
    (define-key map [(control c)(-)] 'py-up-exception)
    (define-key map [(control c)(=)] 'py-down-exception)
    (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
    ;; information
    (define-key map [(control c)(control b)] 'py-submit-bug-report)
    (define-key map [(control c)(control v)] 'py-version)
    (define-key map [(control c)(control w)] 'py-pychecker-run)
    (define-key map [(control c)(c)] 'py-compute-indentation)
    (if (featurep 'xemacs)
        (define-key map [(meta tab)] 'py-complete)
      (substitute-key-definition 'complete-symbol 'completion-at-point
                                 map global-map))

    ;; shadow global bindings for newline-and-indent
    (easy-menu-define py-menu map "Python Mode menu"
      '("Python"
        :help "Python-specific features"
        ["Execute statement" py-execute-statement
         :help "Send statement at point to Python interpreter. "]
        ["Execute block" py-execute-block
         :help "Send compound statement at point to Python interpreter. "]
        ["Execute def" py-execute-def
         :help "Send function at point to Python interpreter. "]
        ["Execute region" py-execute-region
         :help "Send region to Python interpreter. "]
        ["Execute buffer" py-execute-buffer
         :help "Send buffer to Python interpreter. "]
        "-"
        ["Copy block" py-copy-block
         :help "Copy innermost compound statement at point"]
        ["Copy def-or-class" py-copy-def-or-class
         :help "Copy innermost definition at point"]
        ["Copy statement" py-copy-statement
         :help "Copy statement at point"]
        ["Copy expression" py-copy-expression
         :help "Copy expression at point"]
        ["Copy partial-expression" py-copy-partial-expression
         :help "\".\" operators delimit a partial-expression expression on it's level"]
        "-"
        ["Beginning of block" py-beginning-of-block
         :help "Go to start of innermost compound statement at point"]
        ["End of block" py-end-of-block
         :help "Go to end of innermost compound statement at point"]
        ["Beginning of Def-or-Class" py-beginning-of-def-or-class
         :help "Go to start of innermost definition at point"]
        ["End of Def-or-Class" py-end-of-def-or-class
         :help "Go to end of innermost function definition at point"]
        ["Beginning of-class" beginning-of-class
         :help "Go to start of class definition "]
        ["End of Class" py-end-of-class
         :help "Go to end of class definition "]
        "-"
        ("Templates..."
         :help "Expand templates for compound statements"
         :filter (lambda (&rest junk)
                   (abbrev-table-menu python-mode-abbrev-table)))
        "-"
        ["Switch to interpreter" py-shell
         :help "Switch to `inferior' Python in separate buffer"]
        ["Import/reload file" py-execute-import-or-reload
         :help "Load into inferior Python session"]
        ["Set default process" py-set-proc
         :help "Make buffer's inferior process the default"
         :active (buffer-live-p py-buffer)]
        ["pychecker-run" py-pychecker-run :help "Run pychecker"]
        ["Debugger" pdb :help "Run pdb under GUD"]
        "-"
        ["Help on symbol" py-describe-symbol
         :help "Use pydoc on symbol at point"]
        ["Complete symbol" completion-at-point
         :help "Complete (qualified) symbol before point"]
        ["Find function" py-find-function
         :help "Try to find source definition of function at point"]
        ["Update imports" py-update-imports
         :help "Update list of top-level imports for completion"]))
    map))
;; Fixme: add toolbar stuff for useful things like symbol help, send
;; region, at least.  (Shouldn't be specific to Python, obviously.)
;; eric has items including: (un)indent, (un)comment, restart script,
;; run script, debug script; also things for profiling, unit testing.

;;; Intern
(defun py-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol -- beginning of line
  eol -- end of line
  bod -- beginning of def or class
  eod -- end of def or class
  bob -- beginning of buffer
  eob -- end of buffer
  boi -- back to indentation
  bos -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (py-beginning-of-def-or-class 'either))
     ((eq position 'eod) (py-end-of-def-or-class 'either))
     ;; Kind of funny, I know, but useful for py-up-exception.
     ((eq position 'bob) (goto-char (point-min)))
     ((eq position 'eob) (goto-char (point-max)))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (py-beginning-of-statement))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
        (point)
      (goto-char here))))


;;; Python specialized rx

;; (eval-when-compile
;;   (defconst python-rx-constituents
;;     (list
;;      `(block-start          . ,(rx symbol-start
;;                                    (or "def" "class" "if" "elif" "else" "try"
;;                                        "except" "finally" "for" "while" "with")
;;                                    symbol-end))
;;      `(decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
;;                                     (* (any word ?_))))
;;      `(defun                . ,(rx symbol-start (or "def" "class") symbol-end))
;;      `(symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
;;      `(open-paren           . ,(rx (or "{" "[" "(")))
;;      `(close-paren          . ,(rx (or "}" "]" ")")))
;;      `(simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
;;      `(not-simple-operator  . ,(rx (not (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
;;      `(operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
;;                                        "=" "%" "**" "//" "<<" ">>" "<=" "!="
;;                                        "==" ">=" "is" "not")))
;;      `(assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
;;                                        ">>=" "<<=" "&=" "^=" "|="))))
;;     "Additional Python specific sexps for `python-rx'"))

;; (defmacro python-rx (&rest regexps)
;;  "Python mode specialized rx macro which supports common python named REGEXPS."
;;  (let ((rx-constituents (append python-rx-constituents rx-constituents)))
;;    (cond ((null regexps)
;;           (error "No regexp"))
;;          ((cdr regexps)
;;           (rx-to-string `(and ,@regexps) t))
;;          (t
;;           (rx-to-string (car regexps) t)))))


;;; Font-lock and syntax

(defvar python-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
                        '("and"      "assert"   "break"     "class"
                          "continue" "def"      "del"       "elif"
                          "else"     "except"   "for"       "from"
                          "global"   "if"       "import"    "in"
                          "is"       "lambda"   "not"       "or"
                          "pass"     "raise"    "as"        "return"
                          "while"    "with"    "yield")
                        "\\|"))
        (kw2 (mapconcat 'identity
                        '("else:" "except:" "finally:" "try:" "lambda:")
                        "\\|"))
        (kw3 (mapconcat 'identity
                        ;; Don't include Ellipsis in this list, since it is
                        ;; already defined as a pseudo keyword.
                        '("__debug__"
                          "__import__" "__name__" "abs" "all" "any" "apply"
                          "basestring" "bin" "bool" "buffer" "bytearray"
                          "callable" "chr" "classmethod" "cmp" "coerce"
                          "compile" "complex" "copyright" "credits"
                          "delattr" "dict" "dir" "divmod" "enumerate" "eval"
                          "exec" "execfile" "exit" "file" "filter" "float"
                          "format" "getattr" "globals" "hasattr" "hash" "help"
                          "hex" "id" "input" "int" "intern" "isinstance"
                          "issubclass" "iter" "len" "license" "list" "locals"
                          "long" "map" "max" "memoryview" "min" "next"
                          "object" "oct" "open" "ord" "pow" "print" "property"
                          "quit" "range" "raw_input" "reduce" "reload" "repr"
                          "round" "set" "setattr" "slice" "sorted"
                          "staticmethod" "str" "sum" "super" "tuple" "type"
                          "unichr" "unicode" "vars" "xrange" "zip"

                          "bin" "bytearray" "bytes" "format"

                          "memoryview" "next" "print")
                        "\\|"))
        (kw4 (mapconcat 'identity
                        ;; Exceptions and warnings
                        '("ArithmeticError" "AssertionError"
                          "AttributeError" "BaseException" "BufferError"
                          "BytesWarning" "DeprecationWarning" "EOFError"
                          "EnvironmentError" "Exception"
                          "FloatingPointError" "FutureWarning" "GeneratorExit"
                          "IOError" "ImportError" "ImportWarning"
                          "IndentationError" "IndexError"
                          "KeyError" "KeyboardInterrupt" "LookupError"
                          "MemoryError" "NameError" "NotImplemented"
                          "NotImplementedError" "OSError" "OverflowError"
                          "PendingDeprecationWarning" "ReferenceError"
                          "RuntimeError" "RuntimeWarning" "StandardError"
                          "StopIteration" "SyntaxError" "SyntaxWarning"
                          "SystemError" "SystemExit" "TabError" "TypeError"
                          "UnboundLocalError" "UnicodeDecodeError"
                          "UnicodeEncodeError" "UnicodeError"
                          "UnicodeTranslateError" "UnicodeWarning"
                          "UserWarning" "ValueError" "Warning"
                          "ZeroDivisionError")
                        "\\|")))
    (list
     ;; decorators
     '("^[ \t]*\\(@[a-zA-Z_][a-zA-Z_0-9.]+\\)\\((.+)\\)?" 1 'py-decorators-face)
     ;; keywords
     (cons (concat "\\_<\\(" kw1 "\\)\\_>[ \n\t(]") 1)
     ;; builtins when they don't appear as object attributes
     (list (concat "\\([ \t(]\\|^\\)\\_<\\(" kw3 "\\)\\_>[ \n\t(]") 2
           'py-builtins-face)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     (cons (concat "\\_<\\(" kw2 "\\)[ \n\t(]") 1)
     ;; Exceptions
     (list (concat "\\_<\\(" kw4 "\\)[ \n\t:,()]") 1 'py-exception-name-face)
     ;; raise stmts
     '("\\_<raise[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; except clauses
     '("\\_<except[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; classes
     '("\\_<class[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 py-class-name-face)
     ;; functions
     '("\\_<def[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\_<\\(self\\|cls\\|Ellipsis\\|True\\|False\\|None\\)\\_>"
       1 py-pseudo-keyword-face)
     '("[ \t]*\\(_\\{0,2\\}[a-zA-Z][a-zA-Z_0-9.]+_\\{0,2\\}\\) *\\(+\\|-\\|*\\|*\\*\\|/\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\)? ?=[^=\n]"
       1 py-variable-name-face)
     ;; XXX, TODO, and FIXME tags
     '("XXX\\|TODO\\|FIXME" 0 py-XXX-tag-face t)
     ;; special marking for string escapes and percent substitutes;
     ;; loops adapted from lisp-mode in font-lock.el
     ;; '((lambda (bound)
     ;;     (catch 'found
     ;;       (while (re-search-forward
     ;;               (concat
     ;;                "\\(\\\\\\\\\\|\\\\x..\\|\\\\u....\\|\\\\U........\\|"
     ;;                "\\\\[0-9][0-9]*\\|\\\\[abfnrtv\"']\\)") bound t)
     ;;         (let ((face (get-text-property (1- (point)) 'face)))
     ;;           (when (or (and (listp face) (memq 'font-lock-string-face face))
     ;;                     (eq 'font-lock-string-face face))
     ;;             (throw 'found t))))))
     ;;   (1 'font-lock-regexp-grouping-backslash prepend))
     ;; '((lambda (bound)
     ;;     (catch 'found
     ;;       (while (re-search-forward "\\(%[^(]\\|%([^)]*).\\)" bound t)
     ;;         (let ((face (get-text-property (1- (point)) 'face)))
     ;;           (when (or (and (listp face) (memq 'font-lock-string-face face))
     ;;                     (eq 'font-lock-string-face face))
     ;;             (throw 'found t))))))
     ;;   (1 'font-lock-regexp-grouping-construct prepend))
     ))
  "Additional expressions to highlight in Python mode.")

(defconst py-font-lock-syntactic-keywords
  '(("[^\\]\\\\\\(?:\\\\\\\\\\)*\\(\\s\"\\)\\1\\(\\1\\)"
     (2
      (7)))
    ("\\([RUBrub]?\\)[Rr]?\\(\\s\"\\)\\2\\(\\2\\)"
     (1
      (py-quote-syntax 1))
     (2
      (py-quote-syntax 2))
     (3
      (py-quote-syntax 3)))))

(defun py-quote-syntax (n)
  "Put `syntax-table' property correctly on triple quote.
Used for syntactic keywords.  N is the match number (1, 2 or 3)."
  ;; Given a triple quote, we have to check the context to know
  ;; whether this is an opening or closing triple or whether it's
  ;; quoted anyhow, and should be ignored.  (For that we need to do
  ;; the same job as `syntax-ppss' to be correct and it seems to be OK
  ;; to use it here despite initial worries.) We also have to sort
  ;; out a possible prefix -- well, we don't _have_ to, but I think it
  ;; should be treated as part of the string.
  ;; Test cases:
  ;;  ur"""ar""" x='"' # """
  ;; x = ''' """ ' a
  ;; '''
  ;; x '"""' x """ \"""" x
  (save-excursion
    (goto-char (match-beginning 0))
    (cond
     ;; Consider property for the last char if in a fenced string.
     ((= n 3)
      (let* ((font-lock-syntactic-keywords nil)
             (syntax (if (featurep 'xemacs)
                         (parse-partial-sexp (point-min) (point))
                       (syntax-ppss))))
        (when (eq t (nth 3 syntax))     ; after unclosed fence
          (goto-char (nth 8 syntax))    ; fence position
          (skip-chars-forward "uUrRbB") ; skip any prefix
          ;; Is it a matching sequence?
          (if (eq (char-after) (char-after (match-beginning 2)))
              (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2)                  ; leading quote (not prefix)
               (= (match-beginning 1) (match-end 1))) ; prefix is null
          (and (= n 1)                  ; prefix
               (/= (match-beginning 1) (match-end 1)))) ; non-empty
      (let ((font-lock-syntactic-keywords nil))
        (unless (eq 'string (syntax-ppss-context (if (featurep 'xemacs)
                                                     (parse-partial-sexp (point-min) (point))
                                                   (syntax-ppss))))
          ;; (eval-when-compile (string-to-syntax "|"))
          (eval-when-compile (string-to-syntax "|")))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

(defvar py-mode-syntax-table nil)
(setq py-mode-syntax-table
      (let ((table (make-syntax-table))
            (tablelookup (if (featurep 'xemacs)
                             'get-char-table
                           'aref)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and isn't a letter.
        (if (featurep 'xemacs)
            (setq table (standard-syntax-table))
          (let ((symbol (string-to-syntax "_"))
                ;; (symbol (string-to-syntax "_"))
                (sst (standard-syntax-table)))
            (dotimes (i 128)
              (unless (= i ?_)
                (if (equal symbol (funcall tablelookup sst i))
                    (modify-syntax-entry i "." table))))))
        (modify-syntax-entry ?$ "." table)
        (modify-syntax-entry ?% "." table)
        ;; exceptions
        (modify-syntax-entry ?# "<" table)
        (modify-syntax-entry ?\n ">" table)
        (modify-syntax-entry ?' "\"" table)
        (modify-syntax-entry ?` "$" table)
        (modify-syntax-entry ?\_ "w" table)
        table))

(defconst python-dotty-syntax-table
  (let ((table (make-syntax-table)))
    (set-char-table-parent table py-mode-syntax-table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table giving `.' symbol syntax.
Otherwise inherits from `py-mode-syntax-table'.")

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity
(defvar py-dotted-expression-syntax-table nil
  "Syntax table used to identify Python dotted expressions.")
(when (not py-dotted-expression-syntax-table)
  (setq py-dotted-expression-syntax-table
        (copy-syntax-table py-mode-syntax-table))
  (modify-syntax-entry ?_ "_" py-dotted-expression-syntax-table)
  (modify-syntax-entry ?. "_" py-dotted-expression-syntax-table))


;;; Indentation

(defcustom py-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-indent-offset)

(defcustom pdb-path '/usr/lib/python2.7/pdb.py
  "Where to find pdb.py. Edit this according to your system.

If you ignore the location `M-x py-guess-pdb-path' might display it.
"
  :type 'variable
  :group 'python)

;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;      gud-pdb-command-name (symbol-name pdb-path))

(defun py-guess-pdb-path ()
  "If py-pdb-path isn't set, find location of pdb.py. "
  (let ((ele (split-string (shell-command-to-string "whereis python")))
        erg)
    (while (or (not erg)(string= "" erg))
      (when (and (string-match "^/" (car ele)) (not (string-match "/man" (car ele))))
        (setq erg (shell-command-to-string (concat "find " (car ele) " -type f -name \"pdb.py\""))))
      (setq ele (cdr ele)))
    (if erg
        (when (interactive-p) (message "%s" erg))
      (when (interactive-p) (message "%s" "pdb.py not found, please customize `pdb-path'")))
    (concat "'" erg)))

(defcustom py-install-directory nil
  "Directory where python-mode.el and it's subdirectories should be installed. Needed for completion and other environment stuff only. "
  :type 'string
  :group 'python)

(defcustom py-guess-py-install-directory-p  t
 "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. "

:type 'boolean
:group 'python
)

(defcustom py-load-pymacs-p  nil
 "If Pymacs as delivered with python-mode.el shall be loaded.
Default is non-nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"

:type 'boolean
:group 'python)

(defcustom py-report-level-p nil
 "If indenting functions should report reached indent level.

Default is nil. "

:type 'boolean
:group 'python)

(defcustom py-hide-show-minor-mode-p  nil
 "If hide-show minor-mode should be on, default is nil. "

:type 'boolean
:group 'python
)

(defcustom py-outline-minor-mode-p  t
 "If outline minor-mode should be on, default is `t'. "

:type 'boolean
:group 'python
)

(defcustom py-start-run-py-shell  t
 "If `python-mode' should start a python-shell, `py-shell'. Default is `t'.

A running python-shell presently is needed by complete-functions. "

:type 'boolean
:group 'python
)

;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty. Default is non-nil. "
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-close-provides-newline)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. "
  :type 'boolean
  :group 'python)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil. "
  :type 'boolean
  :group 'python)

(defcustom py-closing-list-dedents-bos nil
  "If non-nil, closing parentesis dedents onto column of statement, otherwise keeps additional `py-indent-offset', default is nil "
  :type 'boolean
  :group 'python)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
  :type 'boolean
  :group 'python)

(defcustom py-electric-comment-p t
  "If \"#\" should call `py-electric-comment'. Default is `t'. "
  :type 'boolean
  :group 'python)

(defcustom py-electric-comment-add-space-p nil
  "If py-electric-comment should add a space.  Default is `nil'. "
  :type 'boolean
  :group 'python)

(defcustom py-mark-decorators nil
  "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. "
  :type 'boolean
  :group 'python)

(defcustom py-tab-indent t
  "*Non-nil means TAB in Python mode calls `py-indent-line'."
  :type 'boolean
  :group 'python)

(defcustom py-complete-function 'py-shell-complete
  "Function used for completion in buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
		 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python)

(defcustom py-shell-complete-function 'py-completion-at-point
  "Function used for completion in buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
		 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python)

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

(defvar symbol-definition-start-re)
(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")
(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python)

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]?\\([iptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file. ")

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python)
(make-variable-buffer-local 'py-python-command-args)

(set-default 'py-python-command-args  '("-i"))

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)
(defcustom py-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python
  :tag "Jython Command Args")

(defcustom py-cleanup-temporary  t
 "If temporary buffers and files used by functions executing region  should be deleted afterwards. "

:type 'boolean
:group 'python
)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-lhs-inbound-indent)

(defcustom py-rhs-inbound-indent 1
  "When inside a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python)
(make-variable-buffer-local 'py-rhs-inbound-indent)

(defcustom py-continuation-offset 2
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python)

(defcustom py-smart-indentation t
  "*Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Python mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `python-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-smart-indentation)

(defcustom py-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
                 (const :tag "Align to column zero" nil))
  :group 'python)

(defcustom py-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-indent-comments t
  "When t, comment lines are indented. "
  :type 'boolean
  :group 'python)

(defcustom py-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x))))
    (or (funcall ok (getenv "TMPDIR"))
        (funcall ok "/usr/tmp")
        (funcall ok "/tmp")
        (funcall ok "/var/tmp")
        (funcall ok ".")
        (error
         "Couldn't find a usable temp directory -- set `py-temp-directory'")))
  "*Directory used for temporary files created by a *Python* process.
By default, the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory."
  :type 'string
  :group 'python)

(defcustom py-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'python)

(defcustom py-jump-on-exception t
  "*Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python)

(defcustom py-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python)

(defcustom py-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python)

(defcustom py-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `python-mode' tries to calculate the shell to use (either a
CPython or a Jython shell), it looks at the so-called `shebang' line
-- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'python
  )

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'python)
(make-obsolete-variable 'py-jpython-packages 'py-jython-packages nil)

(defcustom py-current-defun-show  t
 "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'."

:type 'boolean
:group 'python)

(defcustom py-current-defun-delay  2
 "When called interactively, `py-current-defun' should wait PY-WHICH-FUNC-DELAY seconds at the definition name found, before returning to previous position. "

:type 'number
:group 'python)

(defcustom py-send-receive-delay  5
 "Seconds to wait for output, used by `python-send-receive'. "

:type 'number
:group 'python)

(defvar py-exec-command nil
  "Mode commands will set this. ")
(make-variable-buffer-local 'py-exec-command)

(defvar py-exec-string-command nil
  "Mode commands will set this. ")
(make-variable-buffer-local 'py-exec-string-command)

(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-bufname)

(defcustom py-master-file nil
  "If non-nil, \\[py-execute-buffer] executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.

Beside you may set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # py-master-file: \"master.py\"
    # End:

"
  :type 'string
  :group 'python)
(make-variable-buffer-local 'py-master-file)

(defvar py-pychecker-history nil)

(defcustom py-pychecker-command "pychecker"
  "*Shell command used to run Pychecker."
  :type 'string
  :group 'python
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "*List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python
  :tag "Pychecker Command Args")

(defvar py-shell-alist
  '(("jython" . 'jython)
    ("python" . 'cpython))
  "*Alist of interpreters and python shells. Used by `py-choose-shell'
to select the appropriate python interpreter mode for a file.")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python)

(defcustom py-shell-switch-buffers-on-execute t
  "When non-nil switch to the Python output buffer. "
  :type 'boolean
  :group 'python)

(defcustom py-split-windows-on-execute t
  "When non-nil split windows, make Python output buffer other. "
  :type 'boolean
  :group 'python)

(defcustom py-hide-show-keywords
  '(
    "class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"
    )
  "Keywords composing visible heads.
Also used by (minor-)outline-mode "
  :type '(repeat string)
  :group 'python)

(defcustom py-hide-show-hide-docstrings t
  "*Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python)

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT
(defvar py-expression-skip-regexp "^ =:#\t\r\n\f"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")
;; (setq py-expression-skip-regexp "^ =:#\t\r\n\f")

(defvar py-expression-looking-regexp "[^ =:#\t\r\n\f)]"
  "py-expression assumes chars indicated possible composing a py-expression, when looking-at or -back. ")
;; (setq py-expression-looking-regexp "[^ =:#\t\r\n\f)]")

(defvar py-not-expression-regexp "[ .=:#\t\r\n\f)]"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")
;; (setq py-not-expression-regexp "[ .=:#\t\r\n\f)]")

(defvar py-minor-expression-skip-regexp "^ .()[]{}=:#\t\r\n\f"
  "py-minor-expression assumes chars indicated possible composing a py-minor-expression, skip it. ")
;; (setq py-minor-expression-skip-regexp "^ .(){}=:#\t\r\n\f")

(defvar py-minor-expression-forward-regexp "^ .)}=:#\t\r\n\f"
  "py-minor-expression assumes chars indicated possible composing a py-minor-expression, skip it. ")

(defvar py-minor-expression-backward-regexp "^ .({=:#\t\r\n\f"
  "py-minor-expression assumes chars indicated possible composing a py-minor-expression, skip it. ")

(defvar py-not-minor-expression-skip-regexp " \\.=:#\t\r\n\f"
  "py-minor-expression assumes chars indicated may not compose a py-minor-expression, skip it. ")

(defvar py-minor-expression-looking-regexp "[^ .=:#\t\r\n\f)]"
  "py-minor-expression assumes chars indicated possible composing a py-minor-expression, when looking-at or -back. ")
;; (setq py-minor-expression-looking-regexp "[^ .=:#\t\r\n\f)]")

(defvar py-not-minor-expression-regexp "[ .=:#\t\r\n\f)]"
  "py-minor-expression assumes chars indicated probably will not compose a py-minor-expression. ")
;; (setq py-not-minor-expression-regexp "[ .=:#\t\r\n\f)]")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py-jump-to-exception.")

(defvar match-paren-no-use-syntax-pps nil)

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;; Constants
(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-block-closing-keywords-re
  "[ \t]*\\<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement. ")

(defconst py-finally-re
  "[ \t]*\\_<finally\\_>[: \n\t]"
  "Regular expression matching keyword which closes a try-block. ")

(defconst py-except-re
  "[ \t]*\\_<except\\_>[: \n\t]"
  "Regular expression matching keyword which composes a try-block. ")

(defconst py-else-re
  "[ \t]*\\_<else\\_>[: \n\t]"
  "Regular expression matching keyword which closes a for- if- or try-block. ")

(defconst py-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]"
  "Regular expression matching keyword which typically closes a function. ")

(defconst py-no-outdent-re "\\(try:\\|except\\(\\s +.*\\)?:\\|while\\s +.*:\\|for\\s +.*:\\|if\\s +.*:\\|elif\\s +.*:\\)\\([ 	]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ 	\n]\\)")

;; (defconst py-no-outdent-re
;;   (concat
;;    "\\("
;;    (mapconcat 'identity
;;               (list "try:"
;;                     "except\\(\\s +.*\\)?:"
;;                     "while\\s +.*:"
;;                     "for\\s +.*:"
;;                     "if\\s +.*:"
;;                     "elif\\s +.*:"
;;                     (concat py-block-closing-keywords-re "[ \t\n]"))
;;               "\\|")
;;    "\\)")
;;   "Regular expression matching lines not to dedent after.")

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defconst py-assignment-re "\\<\\w+\\>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignment. ")

(defconst py-block-re "[ \t]*\\_<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\_<\\(for\\|if\\|try\\)\\_>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-try-block-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\_<\\(def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re "[ \t]*\\_<\\(if\\|else\\|elif\\|while\\|for\\|def\\|class\\|try\\|except\\|finally\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-clause-re "[ \t]*\\_<\\(else\\|elif\\|except\\|finally\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement's clause. ")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[: \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively. ")

(defconst py-try-clause-re "[ \t]*\\_<\\(except\\|else\\|finally\\)\\_>[: \n\t]"
  "Matches the beginning of a compound try-statement's clause. ")

(defconst py-if-re "[ \t]*\\_<if\\_>[ \n\t]"
  "Matches the beginning of a compound statement saying `if'. ")

(defconst py-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a compound statement saying `try'. " )

;; GNU's syntax-ppss-context
(unless (functionp 'syntax-ppss-context)
  (defsubst syntax-ppss-context (ppss)
    (cond
     ((nth 3 ppss) 'string)
     ((nth 4 ppss) 'comment)
     (t nil))))

;; Skip's XE workaround
(unless (fboundp 'string-to-syntax)
  (defun string-to-syntax (s)
    (cond
     ((equal s "|") '(15))
     ((equal s "_") '(3))
     (t (error "Unhandled string: %s" s))))
  )

(defvar py-help-mode-syntax-table
  (let ((st (make-syntax-table py-mode-syntax-table)))
    ;; Don't get confused by apostrophes in the process's output (e.g. if
    ;; you execute "help(os)").
    (modify-syntax-entry ?\' "." st)
    ;; Maybe we should do the same for double quotes?
    (modify-syntax-entry ?\" "." st)
    st))

(defconst py-space-backslash-table
  (let ((table (copy-syntax-table py-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`py-mode-syntax-table' with backslash given whitespace syntax.")

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :group 'python)
(defvar py-XXX-tag-face 'py-XXX-tag-face)

;; ;; Face for None, True, False, self, and Ellipsis
(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis."
  :group 'python)
(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defface py-variable-name-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Face method decorators."
  :group 'python)
(defvar py-variable-name-face 'py-variable-name-face)

;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :group 'python)
(defvar py-decorators-face 'py-decorators-face)

;; Face for builtins
(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python)
(defvar py-builtins-face 'py-builtins-face)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :group 'python)
(defvar py-class-name-face 'py-class-name-face)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python)

(defvar py-exception-name-face 'py-exception-name-face)

;; have to bind py-file-queue before installing the kill-emacs-hook
(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar python-mode-abbrev-table nil)
(define-abbrev-table 'python-mode-abbrev-table ())

(defvar inferior-python-mode-abbrev-table nil
  "Not in use.")
(define-abbrev-table 'inferior-python-mode-abbrev-table ())

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."
  :group 'python
  :type 'hook)

(defcustom imenu-create-index-p t
  "Non-nil means Python mode creates and displays an index menu of functions and global variables. "
  :type 'boolean
  :group 'python)

;; credits to python.el
(defun py-beg-of-defun-function ()
  (set (make-local-variable 'beginning-of-defun-function)
       'py-beginning-of-def-or-class))

(defun py-end-of-defun-function ()
  (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

;; (custom-add-option 'python-mode-hook 'py-imenu-create-index)
(custom-add-option 'python-mode-hook 'py-imenu-create-index-new)
(custom-add-option 'python-mode-hook
		   (lambda ()
		     "Turn off Indent Tabs mode."
		     (setq indent-tabs-mode nil)))
;; (custom-add-option 'python-mode-hook 'turn-on-eldoc-mode)
(custom-add-option 'python-mode-hook 'abbrev-mode)
;; (custom-add-option 'python-mode-hook 'py-setup-brm)

(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook nil)
(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
`python-mode-hook'.")

(defvar py-shell-hook nil
  "*Hook called by `py-shell'.")

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defcustom py-shell-name "python"
  "A default value `py-shell' may look for, if no shell is specified by command. "
  :type 'string
  :group 'python)
(make-variable-buffer-local 'py-shell-name)

(defvar py-keywords "\\_<\\(ArithmeticError\\|AssertionError\\|AttributeError\\|BaseException\\|BufferError\\|BytesWarning\\|DeprecationWarning\\|EOFError\\|Ellipsis\\|EnvironmentError\\|Exception\\|False\\|FloatingPointError\\|FutureWarning\\|GeneratorExit\\|IOError\\|ImportError\\|ImportWarning\\|IndentationError\\|IndexError\\|KeyError\\|KeyboardInterrupt\\|LookupError\\|MemoryError\\|NameError\\|NoneNotImplementedError\\|NotImplemented\\|OSError\\|OverflowError\\|PendingDeprecationWarning\\|ReferenceError\\|RuntimeError\\|RuntimeWarning\\|StandardError\\|StopIteration\\|SyntaxError\\|SyntaxWarning\\|SystemError\\|SystemExit\\|TabError\\|True\\|TypeError\\|UnboundLocalError\\|UnicodeDecodeError\\|UnicodeEncodeError\\|UnicodeError\\|UnicodeTranslateError\\|UnicodeWarning\\|UserWarning\\|ValueError\\|Warning\\|ZeroDivisionError\\|__debug__\\|__import__\\|__name__\\|abs\\|all\\|and\\|any\\|apply\\|as\\|assert\\|basestring\\|bin\\|bool\\|break\\|buffer\\|bytearray\\|callable\\|chr\\|class\\|classmethod\\|cmp\\|coerce\\|compile\\|complex\\|continue\\|copyright\\|credits\\|def\\|del\\|delattr\\|dict\\|dir\\|divmod\\|elif\\|else\\|enumerate\\|eval\\|except\\|exec\\|execfile\\|exit\\|file\\|filter\\|float\\|for\\|format\\|from\\|getattr\\|global\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|if\\|import\\|in\\|input\\|int\\|intern\\|is\\|isinstance\\|issubclass\\|iter\\|lambda\\|len\\|license\\|list\\|locals\\|long\\|map\\|max\\|memoryview\\|min\\|next\\|not\\|object\\|oct\\|open\\|or\\|ord\\|pass\\|pow\\|print\\|property\\|quit\\|raise\\|range\\|raw_input\\|reduce\\|reload\\|repr\\|return\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|unichr\\|unicode\\|vars\\|while\\|with\\|xrange\\|yield\\|zip\\|\\)\\_>"
  "Contents like py-fond-lock-keyword")

(defun py-insert-default-shebang ()
  "Insert in buffer shebang of installed default Python. "
  (interactive "*")
  (let* ((erg (py-python-default-environment))
         (sheb (concat "#! " erg)))
    (insert sheb)))

(defun py-electric-comment (arg)
  "Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.
With \\[universal-argument] \"#\" electric behavior is inhibited inside a string or comment."
  (interactive "*P")
  (if py-electric-comment-p
      (if (ignore-errors (eq 4 (car-safe arg)))
          (insert "#")
        (when (and (eq last-command 'py-electric-comment) (looking-back " "))
          (forward-char -1))
        (if (interactive-p) (self-insert-command (prefix-numeric-value arg))
          (insert "#"))
        (let ((orig (copy-marker (point)))
              (indent (py-compute-indentation)))
          (unless
              ;; (or
               (eq (current-indentation) indent)
            ;; (looking-back "#[ \t]*"))
            (goto-char orig)
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to indent)
            (goto-char orig))
          (when py-electric-comment-add-space-p
            (unless (looking-at "[ \t]")
              (insert " "))))
        (setq last-command this-command))
    (self-insert-command (prefix-numeric-value arg))))

(defun py-electric-colon (arg)
  "Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.
Default is nil, controlled by `py-electric-colon-active-p'"
  (interactive "*P")
  (cond ((not py-electric-colon-active-p)
         (self-insert-command (prefix-numeric-value arg)))
        ((eq 4 (prefix-numeric-value arg))
         (self-insert-command 1))
        (t (self-insert-command (prefix-numeric-value arg))
           (unless (py-in-string-or-comment-p)
             (let ((orig (copy-marker (point)))
                   (indent (py-compute-indentation)))
               (unless (or (eq (current-indentation) indent)
                           (and (py-top-level-form-p)(< (current-indentation) indent)))
                 (beginning-of-line)
                 (delete-horizontal-space)
                 (indent-to indent))
               (goto-char orig))))))

(defun py-top-level-form-p ()
  "Return non-nil, if line starts with a top level definition.

Used by `py-electric-colon', which will not indent than. "
  (let (erg)
      (save-excursion
        (beginning-of-line)
        (setq erg (or (looking-at py-class-re)
                      (looking-at py-def-re))))
      erg))


;; Electric deletion
(defun py-electric-backspace (&optional arg)
  "Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "
  (interactive "*p")
  (let ((arg (or arg 1))
        erg)
    (dotimes (i arg)
      (if (looking-back "^[ \t]+")
          (let* ((remains (% (current-column) py-indent-offset)))
            (if (< 0 remains)
                (delete-char (- remains))
              (indent-line-to (- (current-indentation) py-indent-offset))))
        (delete-char (- 1))))
  (setq erg (current-column))
  (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-electric-delete (&optional arg)
  "Delete following character or levels of whitespace.

With ARG do that ARG times. "
  (interactive "*p")
  (let ((arg (or arg 1)))
    (dotimes (i arg)
      (if (and (or (bolp)(looking-back "^[ \t]+")) (looking-at "[ \t]+"))
          (let* ((remains (% (+ (current-column) (- (match-end 0)(match-beginning 0))) py-indent-offset)))
            (if (< 0 remains)
                (delete-char remains)
              (delete-char py-indent-offset)))
        (delete-char 1)))))

;; (defun py-electric-delete (arg)
;;   "Delete preceding or following character or levels of whitespace.
;;
;; The behavior of this function depends on the variable
;; `delete-key-deletes-forward'.  If this variable is nil (or does not
;; exist, as in older Emacsen and non-XEmacs versions), then this
;; function behaves identically to \\[c-electric-backspace].
;;
;; If `delete-key-deletes-forward' is non-nil and is supported in your
;; Emacs, then deletion occurs in the forward direction, by calling the
;; function in `py-delete-function'.
;;
;; \\[universal-argument] (programmatically, argument ARG) specifies the
;; number of characters to delete (default is 1)."
;;   (interactive "*p")
;;   (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
;;                (delete-forward-p))
;;           (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
;;                delete-key-deletes-forward))
;;       (funcall py-delete-function arg)
;;     (py-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'py-electric-colon 'delete-selection t) ;delsel
(put 'py-electric-colon 'pending-delete t) ;pending-del
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete 'supersede) ;pending-del
(put 'py-electric-delete 'delete-selection 'supersede) ;delsel
(put 'py-electric-delete 'pending-delete 'supersede) ;pending-del


(defun py-indent-line-outmost (&optional arg)
  "Indent the current line to the outmost reasonable indent.

With optional \\[universal-argument] an indent with length `py-indent-offset' is inserted unconditionally "
  (interactive "*P")
  (let* ((need (py-compute-indentation (point)))
         (cui (current-indentation))
         (cuc (current-column)))
    (cond ((eq 4 (prefix-numeric-value arg))
           (insert (make-string py-indent-offset ?\ )))
          (t
           (if (and (eq need cui)(not (eq cuc cui)))
               (back-to-indentation)
             (beginning-of-line)
             (delete-horizontal-space)
             (indent-to need))))))

(defun py-indent-line (&optional arg)
  "Indent the current line according to Python rules.

When called interactivly with \\[universal-argument], ignore dedenting rules for block closing statements
\(e.g. return, raise, break, continue, pass)

This function is normally used by `indent-line-function' resp.
\\[indent-for-tab-command].
Returns current indentation "
  (interactive "P")
  (let ((cui (current-indentation))
        (indent (py-compute-indentation))
        (col (current-column)))
    (if (interactive-p)
        (progn
          (beginning-of-line)
          (delete-horizontal-space)
          (if (eq 4 (prefix-numeric-value arg))
              (indent-to (+ indent py-indent-offset)))
          (indent-to indent))
      (if py-tab-indent
          (cond ((eq indent cui)
                 (when (eq this-command last-command)
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (if (<= (line-beginning-position) (+ (point) (- col cui)))
                       (forward-char (- col cui))
                     (beginning-of-line))))
                ((< cui indent)
                 (if (eq this-command last-command)
                     (progn
                       (beginning-of-line)
                       (delete-horizontal-space)
                       (indent-to (+ (* (/ cui py-indent-offset) py-indent-offset) py-indent-offset))
                       (forward-char (- col cui)))
                   (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to indent)
                   (forward-char (- col cui))))
                (t (beginning-of-line)
                   (delete-horizontal-space)
                   (indent-to indent)
                   (if (<= (line-beginning-position) (+ (point) (- col cui)))
                       (forward-char (- col cui))
                     (beginning-of-line))))
        (insert-tab))))
  (when (and (interactive-p) py-report-level-p)(message "%s" (current-indentation)))
  (current-indentation))

(defun py-newline-and-indent ()
  "Add a newline and indent to outmost reasonable indent.
When indent is set back manually, this is honoured in following lines. "
  (interactive "*")
  (let ((ci (current-indentation))
        erg)
    (if (< ci (current-column))         ; if point beyond indentation
        (progn
          (newline)
          (setq erg (indent-to-column (py-compute-indentation))))
      (beginning-of-line)
      (insert-char ?\n 1)
      (insert (make-string (setq erg (py-compute-indentation)) ?\ ))
      ;; (move-to-column erg)
      (when (looking-at "\\([ \t]+\\)") (delete-region (match-beginning 1) (match-end 1))))
    (when (and (looking-at "[ \t]+")
               (nth 1 (if (featurep 'xemacs)
                          (parse-partial-sexp (point-min) (point))
                        (syntax-ppss))))
      (delete-region (match-beginning 0) (match-end 0)))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defalias 'py-newline-and-close-block 'py-newline-and-dedent)
(defun py-newline-and-dedent ()
  "Add a newline and indent to one level below current.
Returns column. "
  (interactive "*")
  (let ((cui (current-indentation))
        erg)
    (newline)
    (when (< 0 cui)
      (setq erg (- (py-compute-indentation) py-indent-offset))
      (indent-to-column erg))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-guess-indent-offset (&optional global orig)
  "Guess a value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value.
With optional argument GLOBAL change the global value of `py-indent-offset'. "
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
      (let ((lastindent (if
                             (py-beginning-of-statement-p)
                             (current-indentation)
                           (progn
                          (py-down-statement)
                          (current-indentation)))))
        (while (and (eq lastindent (current-indentation))
                  (not (eobp))
                  (setq erg (point))
                  (py-down-statement)
                  (< erg (point))
                    (not (py-guessed-sanity-check (setq erg (abs (- lastindent (current-indentation))))))))
      (if (py-guessed-sanity-check erg)
                (progn
          (funcall (if global 'kill-local-variable 'make-local-variable)
                   'py-indent-offset)
              (setq py-indent-offset erg))
        (setq py-indent-offset (default-value 'py-indent-offset)))
        (when (interactive-p)
          (message "%s value of py-indent-offset:  %d"
                   (if global "Global" "Local")
                   py-indent-offset))
      py-indent-offset)))

(defun py-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun py-comment-indent-function ()
  "Python version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position)))
      (and comment-start-skip
           (re-search-forward comment-start-skip eol t)
           (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1))))))

(defun py-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.

The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (py-end-of-def-or-class class)
    (let ((end (point)))
      (py-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))



(defalias 'py-shift-region-left 'py-shift-left)
(defun py-shift-left (&optional count start end)
  "Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "
  (interactive "p")
  (let ((erg (py-shift-intern (- count) start end)))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defalias 'py-shift-region-right 'py-shift-right)
(defun py-shift-right (&optional count beg end)
  "Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "
  (interactive "p")
  (let ((erg (py-shift-intern count beg end)))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-intern (count &optional start end)
  (save-excursion
    (let* ((inhibit-point-motion-hooks t)
           deactivate-mark
           (beg (cond (start)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-beginning))))
                      (t (line-beginning-position))))
           (end (cond (end)
                      ((region-active-p)
                       (save-excursion
                         (goto-char
                          (region-end))))
                      (t (line-end-position))))
           (orig end))
      (setq beg (copy-marker beg))
      (setq end (copy-marker end))
      (dotimes (i (abs count))
        (if (< 0 count)
            (indent-rigidly beg end py-indent-offset)
          (indent-rigidly beg end (- py-indent-offset))))
      (push-mark beg t)
      (goto-char end)
      (skip-chars-backward " \t\r\n\f"))
    (py-indentation-of-statement)))

;; make general form below work also in these cases
(defalias 'py-beginning-of-paragraph 'backward-paragraph)
(defalias 'py-end-of-paragraph 'forward-paragraph)
(defalias 'py-beginning-of-line 'beginning-of-line)
(defalias 'py-end-of-line 'end-of-line)

;; Shifting forms start
(defun py-shift-forms-base (form arg &optional beg end)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (orig (copy-marker (point)))
         (beg (cond (beg)
                    ((region-active-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (line-beginning-position)))
                    (t (save-excursion
                         (funcall begform)
                         (line-beginning-position)))))
         (end (cond (end)
                    ((region-active-p)
                     (region-end))
                    (t (funcall endform))))
         (erg (py-shift-intern arg beg end)))
    (goto-char orig)
    erg))

(defun py-shift-paragraph-right (&optional arg)
  "Indent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-paragraph-left (&optional arg)
  "Dedent paragraph by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "paragraph" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-block-right (&optional arg)
  "Indent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-block-left (&optional arg)
  "Dedent block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "block" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-clause-right (&optional arg)
  "Indent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-clause-left (&optional arg)
  "Dedent clause by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "clause" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-def-right (&optional arg)
  "Indent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-def-left (&optional arg)
  "Dedent def by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "def" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-class-right (&optional arg)
  "Indent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-class-left (&optional arg)
  "Dedent class by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "class" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-line-right (&optional arg)
  "Indent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-line-left (&optional arg)
  "Dedent line by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "line" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-statement-right (&optional arg)
  "Indent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (or arg py-indent-offset))))
        (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-shift-statement-left (&optional arg)
  "Dedent statement by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "
  (interactive "*P")
  (let ((erg (py-shift-forms-base "statement" (- (or arg py-indent-offset)))))
    (when (and (interactive-p) py-report-level-p) (message "%s" erg))
    erg))

(defun py-indent-region (start end &optional indent-offset)
  "Reindent a region of Python code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `py-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Python), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")                 ; region; raw prefix arg
  (save-excursion
    (goto-char end) (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((py-indent-offset (prefix-numeric-value
                             (or indent-offset py-indent-offset)))
          (indents '(-1))               ; stack of active indent levels
          (target-column 0)             ; column to which to indent
          (base-shifted-by 0)           ; amount last base line was shifted
          (indent-base (if (looking-at "[ \t\n]")
                           (py-compute-indentation)
                         0))
          ci)
      (while (< (point) end)
        (setq ci (current-indentation))
        ;; figure out appropriate target column
        (cond
         ((or (eq (following-char) ?#)  ; comment in column 1
              (looking-at "[ \t]*$"))   ; entirely blank
          (setq target-column 0))
         ((py-preceding-line-backslashed-p)      ; shift relative to base line
          (setq target-column (+ ci base-shifted-by)))
         (t                             ; new base line
          (if (> ci (car indents))      ; going deeper; push it
              (setq indents (cons ci indents))
            ;; else we should have seen this indent before
            (setq indents (memq ci indents)) ; pop deeper indents
            (if (null indents)
                (error "Bad indentation in region, at line %d"
                       (save-restriction
                         (widen)
                         (1+ (count-lines 1 (point)))))))
          (setq target-column (+ indent-base
                                 (* py-indent-offset
                                    (- (length indents) 2))))
          (setq base-shifted-by (- target-column ci))))
        ;; shift as needed
        (if (/= ci target-column)
            (progn
              (delete-horizontal-space)
              (indent-to target-column)))
        (forward-line 1))))
  (set-marker end nil))
;; Shifting forms end

(defun py-beginning-of-paragraph-position ()
  "Returns beginning of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-paragraph)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-block-position ()
  "Returns beginning of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-clause-position ()
  "Returns beginning of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-def-position ()
  "Returns beginning of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-class-position ()
  "Returns beginning of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-line-position ()
  "Returns beginning of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-beginning-of-statement-position ()
  "Returns beginning of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-beginning-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-paragraph-position ()
  "Returns end of paragraph position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-paragraph)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-block-position ()
  "Returns end of block position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-block)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-clause-position ()
  "Returns end of clause position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-clause)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-def-position ()
  "Returns end of def position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-class-position ()
  "Returns end of class position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-def-or-class-position ()
  "Returns end of def resp. class position, non-greedy. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-def-or-class)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-line-position ()
  "Returns end of line position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-line)))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-end-of-statement-position ()
  "Returns end of statement position. "
  (interactive)
  (save-excursion
    (let ((erg (py-end-of-statement)))
      (when (interactive-p) (message "%s" erg))
      erg)))

;; Declarations start
(defun py-bounds-of-declarations ()
  "Bounds of consecutive multitude of assigments resp. statements around point.

Indented same level, which don't open blocks.
Typically declarations resp. initialisations of variables following
a class or function definition.
See also py-bounds-of-statements "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (line-beginning-position))
      ;; look upward first
      (while (and
              (progn
                (unless (py-beginning-of-statement-p)
                  (py-beginning-of-statement))
                (line-beginning-position))
              (py-beginning-of-statement)
                  (not (py-beginning-of-block-p))
              (eq (current-indentation) orig-indent))
        (setq beg (line-beginning-position)))
      (goto-char orig)
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char beg)
      (if (and beg end)
          (progn
            (when (interactive-p) (message "%s %s" beg end))
            (cons beg end))
        (when (interactive-p) (message "%s" nil))
        nil))))

(defalias 'py-backward-declarations 'py-beginning-of-declarations)
(defun py-beginning-of-declarations ()
  "Got to the beginning of assigments resp. statements in current level which don't open blocks.
"
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-of-declarations 'py-end-of-declarations)
(defun py-end-of-declarations ()
  "Got to the end of assigments resp. statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-copy-declarations 'py-declarations)
(defun py-declarations ()
  "Copy and mark assigments resp. statements in current level which don't open blocks or start with a keyword.

See also `py-statements', which is more general, taking also simple statements starting with a keyword. "
  (interactive)
  (let* ((bounds (py-bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-declarations ()
  "Delete variables declared in current level.

Store deleted variables in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-declarations))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;; Declarations end

;; Statements start
(defun py-bounds-of-statements ()
  "Bounds of consecutive multitude of statements around point.

Indented same level, which don't open blocks. "
  (interactive)
  (let* ((orig-indent (progn
                        (back-to-indentation)
                        (unless (py-beginning-of-statement-p)
                          (py-beginning-of-statement))
                        (unless (py-beginning-of-block-p)
                          (current-indentation))))
         (orig (point))
         last beg end)
    (when orig-indent
      (setq beg (point))
      (while (and (setq last beg)
                  (setq beg
                        (when (py-beginning-of-statement)
                          (line-beginning-position)))
                  (not (py-in-string-p))
                  (not (py-beginning-of-block-p))
                  (eq (current-indentation) orig-indent)))
      (setq beg last)
      (goto-char orig)
      (setq end (line-end-position))
      (while (and (setq last (line-end-position))
                  (setq end (py-down-statement))
                  (not (py-beginning-of-block-p))
                  ;; (not (looking-at py-keywords))
                  ;; (not (looking-at "pdb\."))
                  (not (py-in-string-p))
                  (eq (py-indentation-of-statement) orig-indent)))
      (setq end last)
      (goto-char orig)
      (if (and beg end)
          (progn
            (when (interactive-p) (message "%s %s" beg end))
            (cons beg end))
        (when (interactive-p) (message "%s" nil))
        nil))))

(defalias 'py-backward-statements 'py-beginning-of-statements)
(defun py-beginning-of-statements ()
  "Got to the beginning of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (car bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-of-statements 'py-end-of-statements)
(defun py-end-of-statements ()
  "Got to the end of statements in current level which don't open blocks. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (erg (cdr bounds)))
    (when erg (goto-char erg))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-copy-statements 'py-statements)
(defun py-statements ()
  "Copy and mark simple statements in current level which don't open blocks.

More general than py-declarations, which would stop at keywords like a print-statement. "
  (interactive)
  (let* ((bounds (py-bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (exchange-point-and-mark))))

(defun py-kill-statements ()
  "Delete statements declared in current level.

Store deleted statements in kill-ring "
  (interactive "*")
  (let* ((bounds (py-bounds-of-statements))
         (beg (car bounds))
         (end (cdr bounds)))
    (when (and beg end)
      (goto-char beg)
      (push-mark)
      (goto-char end)
      (kill-new (buffer-substring-no-properties beg end))
      (delete-region beg end))))
;; Statements end

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start py-block-comment-prefix))
    (comment-region beg end arg)))

(defun py-join-words-wrapping (words separator line-prefix line-length)
  (let ((lines ())
        (current-line line-prefix))
    (while words
      (let* ((word (car words))
             (maybe-line (concat current-line word separator)))
        (if (> (length maybe-line) line-length)
            (setq lines (cons (substring current-line 0 -1) lines)
                  current-line (concat line-prefix word separator " "))
          (setq current-line (concat maybe-line " "))))
      (setq words (cdr words)))
    (setq lines (cons (substring
                       current-line 0 (- 0 (length separator) 1)) lines))
    (mapconcat 'identity (nreverse lines) "\n")))

(defun py-fill-comment (&optional justify)
  "Fill the comment paragraph at point"
  (let (;; Non-nil if the current line contains a comment.
        has-comment

        ;; If has-comment, the appropriate fill-prefix for the comment.
        comment-fill-prefix)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*#[# \t]*")
        (setq has-comment t
              comment-fill-prefix (buffer-substring (match-beginning 0)
                                                    (match-end 0))))

       ;; A line with some code, followed by a comment? Remember that the hash
       ;; which starts the comment shouldn't be part of a string or character.
       ((progn
          (while (not (looking-at "#\\|$"))
            (skip-chars-forward "^#\n\"'\\")
            (cond
             ((eq (char-after (point)) ?\\) (forward-char 2))
             ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
          (looking-at "#+[\t ]*"))
        (setq has-comment t)
        (setq comment-fill-prefix
              (concat (make-string (current-column) ? )
                      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
        (fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
        (narrow-to-region

         ;; Find the first line we should include in the region to fill.
         (save-excursion
           (while (and (zerop (forward-line -1))
                       (looking-at "^[ \t]*#")))

           ;; We may have gone to far.  Go forward again.
           (or (looking-at "^[ \t]*#")
               (forward-line 1))
           (point))

         ;; Find the beginning of the first line past the region to fill.
         (save-excursion
           (while (progn (forward-line 1)
                         (looking-at "^[ \t]*#")))
           (point)))

        ;; Lines with only hashes on them can be paragraph boundaries.
        (let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
              (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
              (fill-prefix comment-fill-prefix))
          ;;(message "paragraph-start %S paragraph-separate %S"
          ;;paragraph-start paragraph-separate)
          (fill-paragraph justify))))
    t))

(defun py-fix-this-indent (indent)
  (unless (and (eq (current-indentation) (current-column))
               (eq (current-column) indent))
    (beginning-of-line)
    (indent-to-column indent)
    (delete-region
     (point)
     (progn (skip-chars-forward " \t") (point)))))

(defun py-fill-string (start &optional justify)
  "Fill the paragraph around (point) in the string starting at start"
  ;; basic strategy: narrow to the string and call the default
  ;; implementation
  (let (;; the start of the string's contents
        string-start
        ;; the end of the string's contents
        string-end
        ;; length of the string's delimiter
        delim-length
        ;; The string delimiter
        delim)

    (save-excursion
      (goto-char start)
      (if (looking-at "\\([urbURB]*\\(?:'''\\|\"\"\"\\|'\\|\"\\)\\)\\\\?\n?")
          (setq string-start (match-end 0)
                delim-length (- (match-end 1) (match-beginning 1))
                delim (buffer-substring-no-properties (match-beginning 1)
                                                      (match-end 1)))
        (error "The parameter start is not the beginning of a python string"))

      ;; if the string is the first token on a line and doesn't start with
      ;; a newline, fill as if the string starts at the beginning of the
      ;; line. this helps with one line docstrings
      (save-excursion
        (beginning-of-line)
        (and (/= (char-before string-start) ?\n)
             (looking-at (concat "[ \t]*" delim))
             (setq string-start (point))))

      ;; move until after end of string, then the end of the string's contents
      ;; is delim-length characters before that
      (forward-sexp)
      (setq string-end (- (point) delim-length)))

    ;; Narrow to the string's contents and fill the current paragraph
    (save-restriction
      (narrow-to-region string-start string-end)
      (let ((ends-with-newline (= (char-before (point-max)) ?\n)))
        (fill-paragraph justify)
        (if (and (not ends-with-newline)
                 (= (char-before (point-max)) ?\n))
            ;; the default fill-paragraph implementation has inserted a
            ;; newline at the end. Remove it again.
            (save-excursion
              (goto-char (point-max))
              (delete-char -1)))))

    ;; return t to indicate that we've done our work
    t))

;; designed to word independently from syntax, not needed for now
;; (defun py-fill-string (beg end &optional justify kind this-fill-column)
;;   "Fill the paragraph around (point) in the string starting at start"
;;   (save-excursion
;;     (let* ((start (cond ((or (eq kind 'triplequoted-dq)
;;                              (eq kind 'triplequoted-sq))
;;                          (- beg 3))
;;                         ((or (eq kind 'doublequoted)
;;                              (eq kind 'singlequoted))
;;                          (- beg 1))
;;                         (t beg)))
;;            (indent (progn (goto-char start) (current-column)))
;;            (orig beg)
;;            (fill-column-old fill-column)
;;            empty)
;;       (when this-fill-column (setq fill-column this-fill-column))
;;       (goto-char beg)
;;       (setq empty (looking-at "[ \t]*$"))
;;       (when empty
;;         (forward-line 1))
;;       (beginning-of-line)
;;       (narrow-to-region beg end)
;;       (while (and (<= beg (point))
;;                   (< (setq beg (point))
;;                      (progn (forward-paragraph)(point))))
;;         (save-restriction
;;           (narrow-to-region beg (point))
;;           (goto-char beg)
;;           (when (looking-at "[ \t]*$")
;;             (forward-line 1) (setq beg (point)))
;;           (py-fix-this-indent indent)
;;           ;;           (when py-just-one-whitespace
;;           ;;             (while (re-search-forward "[ \n\r\t]+" nil t 1)
;;           ;;               (replace-match " ")))
;;           (fill-region beg end justify)
;;           (unless (empty-line-p)
;;             (newline))
;;           (widen)))
;;       (when this-fill-column (setq fill-column fill-column-old)))))

(defun py-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Python comments and strings.

If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.
"
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let ((pps
             (if (featurep 'xemacs)
                 (parse-partial-sexp (point-min) (point))
               (syntax-ppss))))
        (cond
         ;; inside a comment
         ((nth 4 pps)
          (py-fill-comment justify))
         ;; only whitespace before the comment start
         ((save-excursion (beginning-of-line) (looking-at "[ \t]*#"))
          (py-fill-comment justify))
         ;; inside a string
         ((nth 3 pps)
          (py-fill-string (nth 8 pps)))
         ;; opening quote of a string
         ((progn (save-excursion (forward-char 1)(nth 3 pps)))
          (save-excursion
            (forward-char 1)
            (py-fill-string (nth 8 pps)))))))))

(defun py-insert-super ()
  "Insert a function \"super()\" from current environment.

As example given in Python v3.1 documentation » The Python Standard Library »

class C(B):
    def method(self, arg):
        super().method(arg) # This does the same thing as:
                               # super(C, self).method(arg)"
  (interactive "*")
  (let* ((orig (point))
         (funcname (progn
                 (py-beginning-of-def)
                 (when (looking-at (concat py-def-re " *\\([^(]+\\) *(\\(?:[^),]*\\),? *\\([^)]*\\))"))
                   (match-string-no-properties 2))))
         (args (match-string-no-properties 3))
         (erg (py-which-python))
         classname)
    (if (< erg 3)
        (progn
          (py-beginning-of-class)
          (when (looking-at (concat py-class-re " *\\([^( ]+\\)"))
            (setq classname (match-string-no-properties 2)))
          (goto-char orig)
          ;; super(C, self).method(arg)"
          (insert (concat "super(" classname ", self)." funcname "(" args ")")))
      (goto-char orig)
      (insert (concat "super()." funcname "(" args ")")))))

;;; python-components-intern.el

(defun py-nesting-level (&optional pps)
  "Accepts the output of `parse-partial-sexp'. "
  (interactive)
  (let* ((pps (or (ignore-errors (nth 0 pps))
                 (if (featurep 'xemacs)
                     (parse-partial-sexp (point-min) (point))
                   (syntax-ppss))))
        (erg (nth 0 pps)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-count-indentation 'py-compute-indentation)
(defun py-compute-indentation (&optional orig origline closing line inside repeat)
  "Compute Python indentation.

When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of dedenting."
  (interactive "P")
  (save-excursion
    (save-restriction
      (widen)
      (let* ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             ;; closing indicates: when started, looked
             ;; at a single closing parenthesis
             (closing closing)
             ;; line: moved already a line backward
             (line line)
             (pps (if (featurep 'xemacs)
                      (parse-partial-sexp (point-min) (point))
                    (syntax-ppss)))
             ;; in a recursive call already
             (repeat repeat)
             ;; inside: started inside a list
             (inside inside)
             erg indent this-line)
        (unless repeat (setq inside (nth 1 pps))
                (setq repeat t))
        (setq indent
              (cond
               ((and (bobp)
                     (eq origline (py-count-lines)))
                (current-indentation))
               ((and (bobp)(py-statement-opens-block-p))
                (+ (if py-smart-indentation (py-guess-indent-offset nil orig) py-indent-offset) (current-indentation)))
               ((and (bobp)(not (py-statement-opens-block-p)))
                (current-indentation))
               ;; (py-in-triplequoted-string-p)
               ((and (nth 3 pps)(nth 8 pps))
                (if (eq origline (py-count-lines))
                    (progn
                      (forward-line -1)
                      (end-of-line)
                      (skip-chars-backward " \t\r\n\f")
                      (if (ignore-errors (< (nth 2 (if (featurep 'xemacs)
                                        (parse-partial-sexp (point-min) (point))
                                      (syntax-ppss))) (line-beginning-position)))
                          (current-indentation)
                (ignore-errors (goto-char (nth 2 pps)))
                (py-line-backward-maybe)
                (back-to-indentation)
                        (py-compute-indentation orig origline closing line inside repeat)))
                  (current-indentation)))
               ((and (looking-at "\"\"\"\\|'''")(not (bobp)))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               ;; comments
               ((nth 8 pps)
                (if (eq origline (py-count-lines))
                    (progn
                      (goto-char (nth 8 pps))
                      (py-line-backward-maybe)
                      (skip-chars-backward " \t")
                      (py-compute-indentation orig origline closing line inside repeat))
                  (goto-char (nth 8 pps))
                  (if (and line (or py-indent-honors-inline-comment (looking-back "^[ \t]*")))
                      (current-column)
                    (forward-char -1)
                    (py-compute-indentation orig origline closing line inside repeat))))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*")(not (eq (line-beginning-position) (point-min))))
                (forward-line -1)
                (end-of-line)
                (setq line t)
                (py-compute-indentation orig origline closing line inside repeat))
               ;; lists
               ((nth 1 pps)
                (cond ((and inside (not line))
                (when (and (eq (point) orig) (looking-at "[ \t]*\\()\\)[ \t]*$"))
                  (setq closing (match-beginning 0)))
                (save-excursion
                  (goto-char (nth 1 pps))
                  (setq this-line (py-count-lines))
                  (cond
                   ((< 0 (- origline this-line))
                    (if (< 1 (- origline this-line))
                        (if closing
                            (if py-closing-list-dedents-bos
                                (current-indentation)
                              (+ (current-indentation) py-indent-offset))
                          (py-fetch-previous-indent orig))
                      (cond ((looking-at "\\s([ \t]*$")
                             (if
                                 (progn
                                   (save-excursion
                                     (back-to-indentation)
                                     (looking-at py-block-or-clause-re)))
                                 (progn
                                   (back-to-indentation)
                                   (+ (current-column) (* 2 py-indent-offset)))
                               (back-to-indentation)
                               (+ (current-column) py-indent-offset)))
                            ((looking-at "\\s([ \t]*\\([^ \t]+.*\\)$")
                             (goto-char (match-beginning 1))
                             (current-column))
                            (t (+ (current-column) (* (nth 0 pps)))))))
                   (t (back-to-indentation)
                      (py-beginning-of-statement)
                      (py-compute-indentation orig origline closing line inside repeat)))))
                      ((and (not inside) line)
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
                      ((not inside)
                (progn (goto-char (+ py-lhs-inbound-indent (nth 1 pps)))
                       (when (looking-at "[ \t]+")
                         (goto-char (match-end 0)))
                       (current-column)))
                      (t
                (goto-char (nth 1 pps))
                       (py-compute-indentation orig origline closing line inside repeat))))
               ((py-preceding-line-backslashed-p)
                (progn
                  (py-beginning-of-statement)
                  (setq this-line (py-count-lines))
                  (if (< 1 (- origline this-line))
                      (py-fetch-previous-indent orig)
                    (if (looking-at "from +\\([^ \t\n]+\\) +import")
                        5
                      (+ (current-indentation) py-continuation-offset)))))
               ((looking-at py-no-outdent-re)
                (if (eq (py-count-lines) origline)
                    (progn
                      (back-to-indentation)
                      (py-line-backward-maybe)
                      (py-compute-indentation orig origline closing line inside repeat))
                  (current-indentation)))
               ((and (looking-at py-block-closing-keywords-re)(eq (py-count-lines) origline))
                (py-beginning-of-block-or-clause)
                (+  (if py-smart-indentation (py-guess-indent-offset nil orig) py-indent-offset)(current-indentation)))
               ((looking-at py-block-closing-keywords-re)
                (py-beginning-of-block-or-clause nil (current-indentation))
                (current-indentation))
               ((and (looking-at py-elif-re) (eq (py-count-lines) origline))
                (py-line-backward-maybe)
                (car (py-clause-lookup-keyword py-elif-re -1)))
               ((and (looking-at py-clause-re)(eq origline (py-count-lines)))
                (cond ((looking-at py-finally-re)
                       (car (py-clause-lookup-keyword py-finally-re -1)))
                      ((looking-at py-except-re)
                       (car (py-clause-lookup-keyword py-except-re -1)))
                      ((looking-at py-else-re)
                       (car (py-clause-lookup-keyword py-else-re -1 (current-indentation))))
                      ((looking-at py-elif-re)
                       (car (py-clause-lookup-keyword py-elif-re -1)))))
               ((looking-at py-block-or-clause-re)
                (cond ((eq origline (py-count-lines))
                       (py-line-backward-maybe)
                       (py-compute-indentation orig origline closing line inside t))
                      (t (+ (if py-smart-indentation (py-guess-indent-offset nil orig) py-indent-offset)(current-indentation)))))
               ((looking-at py-block-closing-keywords-re)
                (py-beginning-of-block)
                (current-indentation))
               ((and (< (current-indentation) (current-column)))
                (back-to-indentation)
                (unless line
                  (setq inside
                        (if (featurep 'xemacs)
                            (nth 1 (parse-partial-sexp (point-min) (point)))
                          (nth 1 (syntax-ppss)))))
                (py-compute-indentation orig origline closing line inside repeat))
               ((not (py-beginning-of-statement-p))
                (if (bobp)
                    (current-column)
                  (if (eq (point) orig)
                      (progn
                        (py-line-backward-maybe)
                        (py-compute-indentation orig origline closing line inside repeat))
                  (py-beginning-of-statement)
                    (py-compute-indentation orig origline closing line inside repeat))))
               ((py-statement-opens-block-p)
                (if (< (py-count-lines) origline)
                    (+ (if py-smart-indentation (py-guess-indent-offset nil orig) py-indent-offset) (current-indentation))
                  (py-compute-indentation orig origline closing line inside t)))
               ((and (< (py-count-lines) origline)(looking-at py-assignment-re))
                (current-indentation))
               ((looking-at py-assignment-re)
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               ((and (eq origline (py-count-lines))
                     (save-excursion (and (setq erg (py-go-to-keyword py-block-or-clause-re -1))
                                          (ignore-errors (< orig (py-end-of-block-or-clause))))))
                (+ (car erg) (if py-smart-indentation (py-guess-indent-offset nil orig) py-indent-offset)))
               ((and (eq origline (py-count-lines))
                     (py-beginning-of-statement-p))
                (py-beginning-of-statement)
                (py-compute-indentation orig origline closing line inside repeat))
               (t (current-indentation))))
        (when (interactive-p) (message "%s" indent))
        indent))))

(defun py-line-backward-maybe ()
  (skip-chars-backward " \t\f" (line-beginning-position))
  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
    (setq line t)))

(defun py-fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(defun py-continuation-offset (&optional arg)
  "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. "
  (interactive "p")
  (let ((erg (if (eq 1 arg)
                 py-continuation-offset
               (when (numberp arg)
                 (prog1
                     arg
                   (setq py-continuation-offset arg))))))
    (when (interactive-p) (message "%s" py-continuation-offset))
    py-continuation-offset))

(defalias 'pios 'py-indentation-of-statement)
(defalias 'ios 'py-indentation-of-statement)
(defun py-indentation-of-statement ()
  "Returns the indenation of the statement at point. "
  (interactive)
  (let ((erg (save-excursion
               (back-to-indentation)
               (or (py-beginning-of-statement-p)
                   (py-beginning-of-statement))
               (current-indentation))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-in-list-p 'py-list-beginning-position)
(defun py-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or start (point-min)))
         (erg
          (if (featurep 'xemacs)
              (nth 1 (parse-partial-sexp ppstart (point)))
            (nth 1 (syntax-ppss)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for `parse-partial-sexp'."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (if (featurep 'xemacs)
                  (parse-partial-sexp ppstart (point))
                (syntax-ppss)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (interactive-p) (message "%s" end))
    end))

(defun py-continuation-line-p ()
  "Return t if current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (py-preceding-line-backslashed-p)
        (< 0 (py-nesting-level)))))

(defun py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\r\n\f")
    (let ((erg (and (eq (char-before (point)) ?\\ )
                    (py-escaped))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  (interactive)
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (let ((erg (and (eq (char-before (point)) ?\\ )
                    (py-escaped))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-escaped (&optional iact)
  "Return t if char is preceded by an odd number of backslashes. "
  (interactive "p")
  (lexical-let ((orig (point))
                erg)
    (setq erg (< 0 (abs (% (skip-chars-backward "\\\\")2))))
    (goto-char orig)
    (when iact (message "%s" erg))
    erg))

(defun py-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (save-restriction
    (widen)
    (let* ((pps
            (if (featurep 'xemacs)
                (parse-partial-sexp (line-beginning-position) (point))
              (syntax-ppss)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (looking-at (concat "^[ \t]*" comment-start-skip))
          (setq erg (point))))
      erg)))

(defun py-in-triplequoted-string-p ()
  "Returns character address of start tqs-string, nil if not inside. "
  (interactive)
  (let* ((pps
          (if (featurep 'xemacs)
              (parse-partial-sexp (point-min) (point))
            (syntax-ppss)))
         (erg (when (and (nth 3 pps) (nth 8 pps))(nth 2 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\"\"\\|''''")
                            (goto-char (match-end 0))
                            (setq pps (if (featurep 'xemacs)
                                          (parse-partial-sexp (point-min) (point))
                                        (syntax-ppss)))
                            (when (and (nth 3 pps) (nth 8 pps)) (nth 2 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-in-string-p ()
  "Returns character address of start of string, nil if not inside. "
  (interactive)
  (let* ((pps
          (if (featurep 'xemacs)
              (parse-partial-sexp (point-min) (point))
            (syntax-ppss)))
         (erg (when (nth 3 pps) (nth 8 pps))))
    (save-excursion
      (unless erg (setq erg
                        (progn
                          (when (looking-at "\"\\|'")
                            (forward-char 1)
                            (setq pps (if (featurep 'xemacs)
                                          (parse-partial-sexp (point-min) (point))
                                        (syntax-ppss)))
                            (when (nth 3 pps) (nth 8 pps)))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-in-statement-p ()
  "Returns list of beginning and end-position if inside.

Result is useful for booleans too: (when (py-in-statement-p)...)
will work.
"
  (interactive)
  (let ((orig (point))
        beg end erg)
    (save-excursion
      (setq end (py-end-of-statement))
      (setq beg (py-beginning-of-statement))
      (when (and (<= beg orig)(<= orig end))
        (setq erg (cons beg end))
        (when (interactive-p) (message "%s" erg))
        erg))))

(defun py-beginning-of-expression-p ()
  "Returns position, if cursor is at the beginning of a expression, nil otherwise. "
  (interactive)
  (let ((orig (point)))
    (save-excursion
      (py-end-of-expression)
      (py-beginning-of-expression)
      (when (or (eq orig (point)))
        (when (interactive-p)
          (message "%s" orig))
        orig))))

(defun py-beginning-of-partial-expression-p ()
  "Returns position, if cursor is at the beginning of a expression, nil otherwise. "
  (interactive)
  (let ((orig (point)))
    (save-excursion
      (py-end-of-partial-expression)
      (py-beginning-of-partial-expression)
      (when (or (eq orig (point)))
        (when (interactive-p)
          (message "%s" orig))
        orig))))

(defun py-beginning-of-statement-p ()
  "Returns position, if cursor is at the beginning of a statement, nil otherwise. "
  (interactive)
  (let ((orig (point)))
    (save-excursion
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (or (eq orig (point)))
        (when (interactive-p)
          (message "%s" orig))
        orig))))

(defalias 'py-beginning-of-block-p 'py-statement-opens-block-p)
(defun py-statement-opens-block-p (&optional regexp)
  "Return position if the current statement opens a block
in stricter or wider sense.

For stricter sense specify regexp. "
  (interactive)
  (let* ((regexp (or regexp py-block-re))
         (erg (py-statement-opens-base regexp)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (py-end-of-statement)
      (py-beginning-of-statement)
      (when (and
             (looking-back "^[ \t]*") (<= (line-beginning-position)(point))(looking-at regexp))
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-beginning-of-clause-p 'py-statement-opens-clause-p)
(defun py-statement-opens-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-clause-re))

(defalias 'py-beginning-of-block-or-clause-p 'py-statement-opens-block-or-clause-p)
(defun py-statement-opens-block-or-clause-p ()
  "Return position if the current statement opens block or clause. "
  (interactive)
  (py-statement-opens-base py-block-or-clause-re))

(defalias 'py-beginning-of-class-p 'py-statement-opens-class-p)
(defun py-statement-opens-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-class-re))

(defalias 'py-beginning-of-def-p 'py-statement-opens-def-p)
(defun py-statement-opens-def-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-re))

(defalias 'py-beginning-of-def-or-class-p 'py-statement-opens-def-or-class-p)
(defun py-statement-opens-def-or-class-p ()
  "Return `t' if the statement opens a functions or class definition, nil otherwise. "
  (interactive)
  (py-statement-opens-base py-def-or-class-re))

(defun py-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (unless (py-beginning-of-statement-p) (py-beginning-of-statement))
    (prog1
        (looking-at py-block-closing-keywords-re)
      (goto-char here))))

(defun py-end-base (regexp orig &optional iact)
  "Used internal by functions going to the end forms. "
  (let ((erg (if (py-statement-opens-block-p regexp)
                 (point)
               (py-go-to-keyword regexp -1)
               (when (py-statement-opens-block-p regexp)
                 (point))))
        ind)
    (if erg
        (progn
          (setq ind (+ py-indent-offset (current-indentation)))
          (py-end-of-statement)
          (forward-line 1)
          (setq erg (py-travel-current-indent (cons ind (point)))))
      (py-look-downward-for-beginning regexp)
      (unless (eobp)(py-end-base regexp orig iact)))
    (if (< orig (point))
        (setq erg (point))
      (setq erg (py-look-downward-for-beginning regexp))
      (when erg (py-end-base regexp orig iact)))
    (when iact (message "%s" erg))
    erg))

(defun py-look-downward-for-beginning (regexp)
  "When above any beginning of FORM, search downward. "
  (let ((erg (re-search-forward regexp nil (quote move) 1)))
    (if (and erg (not (py-in-string-or-comment-p))
             (not (py-in-list-p)))
        erg
      (unless (eobp)
        (py-look-downward-for-beginning regexp)))))

(defun py-current-defun (&optional iact)
  "Go to the outermost method or class definition in current scope.

Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable.
Returns name of class or methods definition, if found, nil otherwise.

See customizable variables `py-current-defun-show' and `py-current-defun-delay'."
  (interactive "p")
  (save-restriction
    (widen)
    (save-excursion
      (let ((erg (when (py-beginning-of-def-or-class 'either)
                   (forward-word 1)
                   (skip-chars-forward " \t")
                   (prin1-to-string (symbol-at-point)))))
        (when (and erg py-current-defun-show (push-mark (point) t t) (skip-chars-forward "^ (")
                   (exchange-point-and-mark)
                   (sit-for py-current-defun-delay)))
        (when iact (message (prin1-to-string erg)))
        erg))))

;; electric characters
(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
                (looking-at py-clause-re))
         ;; short circuit infloop on illegal construct
         (not (bobp))
         (progn (forward-line -1)
                (py-beginning-of-statement)
                (back-to-indentation)
                (when (looking-at py-blank-or-comment-re)
                  (backward-to-indentation 1))
                (not (looking-at py-no-outdent-re))))))

(defun py-sort-imports ()
  "Sort multiline imports.

Put point inside the parentheses of a multiline import and hit
\\[py-sort-imports] to sort the imports lexicographically"
  (interactive)
  (save-excursion
    (let ((open-paren (save-excursion (progn (up-list -1) (point))))
          (close-paren (save-excursion (progn (up-list 1) (point))))
          sorted-imports)
      (goto-char (1+ open-paren))
      (skip-chars-forward " \n\t")
      (setq sorted-imports
            (sort
             (delete-dups
              (split-string (buffer-substring
                             (point)
                             (save-excursion (goto-char (1- close-paren))
                                             (skip-chars-backward " \n\t")
                                             (point)))
                            ", *\\(\n *\\)?"))
             ;; XXX Should this sort case insensitively?
             'string-lessp))
      ;; Remove empty strings.
      (delete-region open-paren close-paren)
      (goto-char open-paren)
      (insert "(\n")
      (insert (py-join-words-wrapping (remove "" sorted-imports) "," "    " 78))
      (insert ")"))))

(defun py-in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  (let* ((lim (or lim (point-min)))
         (state (if (featurep 'xemacs)
                    (parse-partial-sexp lim (point))
                  (syntax-ppss))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment))))

(defun empty-line-p (&optional iact)
  "Returns t if cursor is at an line with nothing but whitespace-characters, nil otherwise."
  (interactive "p")
  (save-excursion
    (let ((erg (progn
                 (beginning-of-line)
                 (looking-at "\\s-*$"))))
      (when iact
        (message "%s" erg))
      erg)))

(defun py-count-lines (&optional start end)
  "Count lines in buffer, optional without given boundaries.
Ignores common region.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
  (interactive)
  (save-restriction
    (widen)
    (let ((beg (cond (start)
                           (t (point-min))))
                (end (cond (end)
                           (t (point))))
                erg)
    (if (featurep 'xemacs)
        (setq erg (count-lines beg end))
      (setq erg (1+ (count-matches "[\n\C-m]" beg end))))
    (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-which-function ()
  "Return the name of the function or class, if curser is in, return nil otherwise. "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((orig (point))
            (erg (if (and (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+")) (not (py-in-string-or-comment-p)))
                     (match-string-no-properties 2)
                   (progn
                     (py-beginning-of-def-or-class)
                     (when (looking-at (concat py-def-or-class-re " +\\([^(]+\\)(.+"))
                       (match-string-no-properties 2))))))
        (if (and erg (< orig (py-end-of-def-or-class)))
            (when (interactive-p) (message "%s" erg))
          (setq erg nil)
          (when (interactive-p) (message "%s" "Not inside a function or class"))
          erg)))))

(defconst py-help-address "python-mode@python.org"
  "List dealing with usage and developing python-mode.

Also accepts submission of bug reports, whilst a ticket at
http://launchpad.net/python-mode
is preferable for that. ")

;;; python-components-move.el

;; Block
(defalias 'py-previous-block 'py-beginning-of-block)
(defalias 'py-goto-block-up 'py-beginning-of-block)
(defalias 'py-backward-block 'py-beginning-of-block)
(defun py-beginning-of-block (&optional indent)
  "Looks up for nearest opening block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-re -1 indent)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-beginning-of-if-block ()
  "Looks up for nearest opening if-block, i.e. compound statement

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-if-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-beginning-of-try-block ()
  "Looks up for nearest opening try-block, i.e. compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-try-block-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-block 'py-end-of-block)
(defalias 'py-goto-beyond-block 'py-end-of-block)
(defun py-end-of-block ()
  "Go to the end of a compound statement.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((orig (point)))
    (py-end-base py-block-re orig (interactive-p))))

;; Block or clause
(defalias 'py-previous-block-or-clause 'py-beginning-of-block-or-clause)
(defalias 'py-goto-block-or-clause-up 'py-beginning-of-block-or-clause)
(defalias 'py-backward-block-or-clause 'py-beginning-of-block-or-clause)
(defun py-beginning-of-block-or-clause (&optional arg indent)
  "Looks up for nearest opening clause or block.

With universal argument looks for next compound statements
i.e. blocks only.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")

  (let* ((regexp (if arg
                    py-block-re
                  py-block-or-clause-re))
        (erg (ignore-errors (cdr (py-go-to-keyword regexp -1 indent)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-block-or-clause 'py-end-of-block-or-clause)
(defalias 'py-goto-beyond-block-or-clause 'py-end-of-block-or-clause)
(defun py-end-of-block-or-clause (&optional arg)
  "Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.
Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive "P")
  (let ((regexp (if arg
                    py-block-re
                  py-block-or-clause-re))
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Class
(defalias 'beginning-of-class 'py-beginning-of-class)
(defalias 'py-backward-class 'py-beginning-of-class)
(defalias 'py-previous-class 'py-beginning-of-class)
(defun py-beginning-of-class ()
  "Move point to start of next `class'.

See also `py-beginning-of-def-or-class'.
Returns position reached, if any, nil otherwise."
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-class-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-class 'py-end-of-class)
(defalias 'py-next-class 'py-end-of-class)
(defun py-end-of-class (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let ((orig (point))
        (regexp py-class-re))
    (py-end-base regexp orig iact)))

;; Clause
(defalias 'py-previous-clause 'py-beginning-of-clause)
(defalias 'py-goto-clause-up 'py-beginning-of-clause)
(defalias 'py-backward-clause 'py-beginning-of-clause)
(defun py-beginning-of-clause ()
  "Looks up for nearest opening clause, i.e. a compound statements
subform.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-block-or-clause-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'py-forward-clause 'py-end-of-clause)
(defalias 'py-goto-beyond-clause 'py-end-of-clause)
(defun py-end-of-clause ()
  "Without arg, go to the end of a compound statement.

With arg , move point to end of clause at point.

Returns position reached, if any, nil otherwise.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html"
  (interactive)
  (let ((regexp py-block-or-clause-re)
        (orig (point)))
    (py-end-base regexp orig (interactive-p))))

;; Method Definition or Class
(defun py-beginning-of-def ()
  "Move point to start of `def'.

Returns position reached, if any, nil otherwise "
  (interactive)
  (let ((erg (ignore-errors (cdr (py-go-to-keyword py-def-re -1)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-def (&optional iact)
  "Move point beyond next method definition.

Returns position reached, if any, nil otherwise."
  (interactive "p")
  (let* ((orig (point))
         (regexp py-def-re))
    (py-end-base regexp orig iact)))

(defalias 'py-backward-def-or-class 'py-beginning-of-def-or-class)
(defalias 'py-previous-def-or-class 'py-beginning-of-def-or-class)
(defun py-beginning-of-def-or-class (&optional arg)
  "Move point to start of `def' or `class', whatever is next.

With optional universal arg CLASS, move to the beginn of class definition.
Returns position reached, if any, nil otherwise "
  (interactive "P")
  (let* ((regexp (if (eq 4 (prefix-numeric-value arg))
                     py-class-re
                   py-def-or-class-re))
         (res (ignore-errors (cdr (py-go-to-keyword regexp -1))))
         (erg
          (when (looking-at regexp)
            res)))
    (when (interactive-p) (message "%s" (prin1-to-string erg)))
    erg))

(defalias 'end-of-def-or-class 'py-end-of-def-or-class)
(defalias 'py-forward-def-or-class 'py-end-of-def-or-class)
(defalias 'py-next-def-or-class 'py-end-of-def-or-class)
(defun py-end-of-def-or-class (&optional arg)
  "Move point beyond next `def' or `class' definition.

With optional universal arg, move to the end of class exclusively.
Returns position reached, if any, nil otherwise."
  (interactive "P")
  (let* ((orig (point))
         (regexp
          (cond ((eq 4 (prefix-numeric-value arg))
                 py-class-re)
                (t py-def-or-class-re))))
    (py-end-base regexp orig (interactive-p))))

;; Expression
(defalias 'py-backward-expression 'py-beginning-of-expression)
(defun py-beginning-of-expression (&optional orig origline done)
  "Go to the beginning of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes.
"
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (when (looking-at "\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)")
        (goto-char (1- (match-beginning 0)))
        (skip-chars-backward " \t\r\n\f")
        (forward-char -1))
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps (if (featurep 'xemacs)
                     (parse-partial-sexp (point-min) (point))
                   (syntax-ppss)))
            (done done)
            erg)
        (setq erg
              (cond
               ;; if in string
               ((and (nth 3 pps)(nth 8 pps)
                     (save-excursion
                       (ignore-errors
                         (goto-char (nth 2 pps)))))
                (goto-char (nth 2 pps))
                (py-beginning-of-expression orig origline))
               ;; comments left, as strings are done
               ((nth 8 pps)
                (goto-char (1- (nth 8 pps)))
                (py-beginning-of-expression orig origline))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
                (forward-line -1)
                (unless (bobp)
                  (end-of-line)
                  (py-beginning-of-expression orig origline)))
               ;; character address of start of innermost containing list; nil if none.
               ((nth 1 pps)
                (goto-char (nth 1 pps))
                (when
                    (not (looking-back "[ \t]+"))
                  (skip-chars-backward py-expression-skip-regexp))
                (py-beginning-of-expression orig origline))
               ;; inside expression
               ((and (eq (point) orig) (not (bobp)) (looking-back py-expression-looking-regexp))
                (skip-chars-backward py-expression-skip-regexp)
                (py-beginning-of-expression orig origline))
               ((looking-at py-expression-looking-regexp)
                (point))
               (t (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))(point)))))
        (when (interactive-p) (message "%s" erg))
        erg))))

(defalias 'py-forward-expression 'py-end-of-expression)
(defun py-end-of-expression (&optional orig origline done)
  "Go to the end of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference

Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (eobp)
      (let*
          ((orig (or orig (point)))
           (origline (or origline (py-count-lines)))
           (pps (if (featurep 'xemacs)
                    (parse-partial-sexp (point-min) (point))
                  (syntax-ppss)))
           (done done)
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
        (cond
         ((and (empty-line-p)(not done)(not (eobp)))
          (while
              (and (empty-line-p)(not done)(not (eobp)))
            (forward-line 1))
          (py-end-of-expression orig origline done))
         ;; inside string
         ((nth 3 pps)
          (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (goto-char (match-end 0)))
          (while
              (and (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
                   (nth 3
                        (if (featurep 'xemacs)
                            (parse-partial-sexp (point-min) (point))
                          (syntax-ppss)))))
          (py-end-of-expression orig origline done))
         ;; in comment
         ((nth 4 pps)
          (forward-line 1)
          (py-end-of-expression orig origline done))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*")(not done))
          (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                      (beginning-of-line)))
          (end-of-line)
          ;;          (setq done t)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-expression orig origline done))
         ;; start of innermost containing list; nil if none.
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (let ((parse-sexp-ignore-comments t))
            (forward-list)
            (py-end-of-expression orig origline done)))
         ((and (not done)(looking-at py-not-expression-regexp)(not (eobp)))
          (skip-chars-forward py-not-expression-regexp)
          (py-end-of-expression orig origline done))
         ((and (not done)(looking-at py-expression-skip-regexp)(not (eobp)))
          (skip-chars-forward py-not-expression-regexp)
          (forward-char -1)
          (py-end-of-expression orig origline done))
         ((and (looking-at py-expression-looking-regexp)(not (eobp)))
          (forward-char 1)
          (setq done (< 0 (skip-chars-forward py-expression-skip-regexp)))
          (when done (forward-char -1))
          (setq done t)
          (py-end-of-expression orig origline done)))
        (unless (eq (point) orig)
          (setq erg (point)))
        (when (interactive-p) (message "%s" erg))
        erg))))

;; Partial- or Minor Expression
(defalias 'py-backward-partial-expression 'py-beginning-of-partial-expression)
(defalias 'py-beginning-of-minor-expression 'py-beginning-of-partial-expression)
(defun py-beginning-of-partial-expression (&optional orig origline done)
  "Go to the beginning of a minor python expression.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (when (looking-at "\\(=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\)")
        (goto-char (1- (match-beginning 0)))
        (skip-chars-backward " \t\r\n\f")
        (forward-char -1))
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps (if (featurep 'xemacs)
                     (parse-partial-sexp (point-min) (point))
                   (syntax-ppss)))
            (done done)
            erg)
        (setq erg
              (cond
               ;; if in string
               ((and (nth 3 pps)(nth 8 pps)
                     (save-excursion
                       (ignore-errors
                         (goto-char (nth 2 pps)))))
                (goto-char (nth 2 pps))
                (py-beginning-of-partial-expression orig origline))
               ;; comments left, as strings are done
               ((nth 8 pps)
                (goto-char (1- (nth 8 pps)))
                (py-beginning-of-partial-expression orig origline))
               ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
                (forward-line -1)
                (unless (bobp)
                  (end-of-line)
                  (py-beginning-of-partial-expression orig origline)))
               ((nth 1 pps)
                (skip-chars-backward py-minor-expression-backward-regexp)
                (point))
               ((and (eq (point) orig) (not (bobp)) (looking-back py-minor-expression-looking-regexp))
                (skip-chars-backward py-minor-expression-skip-regexp)
                (py-beginning-of-partial-expression orig origline))
               ((looking-at py-minor-expression-looking-regexp)
                (point))
               (t (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))(point)))))
        (when (interactive-p) (message "%s" erg))
        erg))))

(defalias 'py-forward-partial-expression 'py-end-of-partial-expression)
(defalias 'py-end-of-minor-expression 'py-end-of-partial-expression)
(defun py-end-of-partial-expression (&optional orig origline done)
  "Go to the end of a minor python expression.

\".\" operators delimit a minor expression on their level.
Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes. "
  (interactive)
  (save-restriction
    (widen)
    (unless (eobp)
      (let*
          ((orig (or orig (point)))
           (origline (or origline (py-count-lines)))
           (pps (if (featurep 'xemacs)
                    (parse-partial-sexp (point-min) (point))
                  (syntax-ppss)))
           (done done)
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
        (cond
         ((and (empty-line-p)(not done)(not (eobp)))
          (while
              (and (empty-line-p)(not done)(not (eobp)))
            (forward-line 1))
          (py-end-of-partial-expression orig origline done))
         ;; inside string
         ((nth 3 pps)
          (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
            (goto-char (match-end 0)))
          (while
              (and (re-search-forward "[^\\]\"\"\"\\|[^\\]'''\\|[^\\]\"\\|[^\\]'" nil (quote move) 1)
                   (nth 3
                        (if (featurep 'xemacs)
                            (parse-partial-sexp (point-min) (point))
                          (syntax-ppss)))))
          (py-end-of-partial-expression orig origline done))
         ;; in comment
         ((nth 4 pps)
          (forward-line 1)
          (py-end-of-partial-expression orig origline done))
         ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*")(not done))
          (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                      (beginning-of-line)))
          (end-of-line)
          ;;          (setq done t)
          (skip-chars-backward " \t\r\n\f")
          (py-end-of-partial-expression orig origline done))
         ((and (nth 1 pps) (<= orig (nth 1 pps)))
          (goto-char (nth 1 pps))
          (let ((parse-sexp-ignore-comments t))
            (forward-list)
            (setq done t)
            (py-end-of-partial-expression orig origline done)))
         ((and (looking-at "\\.")(< orig (point)))
          (point))
         ((and (not done)(looking-at "\\.\\|=\\|:\\|+\\|-\\|*\\|/\\|//\\|&\\|%\\||\\|\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!="))
          (goto-char (match-end 0))
          (when (< 0 (skip-chars-forward " \t\r\n\f"))
            (forward-char 1))
          (py-end-of-partial-expression orig origline done))
         ((and (not done)(looking-at py-minor-expression-looking-regexp)(not (eobp)))
          (skip-chars-forward py-minor-expression-forward-regexp)
          (setq done t)
          (py-end-of-partial-expression orig origline done))
         ((and (not done)(looking-at py-not-minor-expression-regexp)(not (eobp)))
          (skip-chars-forward py-not-minor-expression-skip-regexp)
          (py-end-of-partial-expression orig origline done))
         ((and (eq (point) orig) (not (eobp)))
          (forward-char 1)
          (py-end-of-partial-expression orig origline done)))
        (unless (eq (point) orig)
          (setq erg (point)))
        (when (interactive-p) (message "%s" erg))
        erg))))

;; Statement
(defalias 'py-backward-statement 'py-beginning-of-statement)
(defalias 'py-previous-statement 'py-beginning-of-statement)
(defalias 'py-statement-backward 'py-beginning-of-statement)
(defun py-beginning-of-statement (&optional orig origline done)
  "Go to the initial line of a simple statement.

For beginning of compound statement use py-beginning-of-block.
For beginning of clause py-beginning-of-clause.

Referring python program structures see for example:
http://docs.python.org/reference/compound_stmts.html
"
  (interactive)
  (save-restriction
    (widen)
    (unless (bobp)
      (let ((orig (or orig (point)))
            (cui (current-indentation))
            (origline (or origline (py-count-lines)))
            (pps
             (if (featurep 'xemacs)
                 (parse-partial-sexp (point-min) (point))
               (syntax-ppss)))
            erg done)
        (cond
         ((and (bobp) (eq (point) orig)))
         ((and (not (< (point) orig))(not (eq 0 (skip-chars-backward " \t\r\n\f"))))
          (setq done t)
          (py-beginning-of-statement orig origline done))
         ((empty-line-p)
          (forward-line -1)
          (while (and (not (bobp))(empty-line-p))
            (forward-line -1))
          (end-of-line)
          (py-beginning-of-statement orig origline done))
         ((nth 8 pps)
          (setq done t)
          (goto-char (nth 8 pps))
          (py-beginning-of-statement orig origline done))
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (forward-line -1)
          (unless (bobp)
            (end-of-line)
            (py-beginning-of-statement orig origline done)))
         ((py-continuation-line-p)
          (forward-line -1)
          (py-beginning-of-statement orig origline done))
         ;; character address of start of innermost containing list; nil if none.
         ((nth 1 pps)
          (goto-char (nth 1 pps))
          (forward-char -1)
          (setq done t)
          (when (< (point) orig) (setq erg (point)))
          (py-beginning-of-statement orig origline done))
         ((and (eq (current-indentation) (current-column))
               (eq (point) orig) (not (bolp)))
          (beginning-of-line)
          (py-beginning-of-statement orig origline done))
         ((not (eq (current-column) (current-indentation)))
          (back-to-indentation)
          (setq erg (point))
          (setq done t)
          (py-beginning-of-statement orig origline done))
         ((and (eq (point) orig)(or (bolp) (<= (current-column)(current-indentation))))
          (forward-line -1)
          (end-of-line)
          (skip-chars-backward " \t")
          (py-beginning-of-statement orig origline done)))
        (unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*"))
          (when (< (point) orig)(setq erg (point))))
        (when (interactive-p) (message "%s" erg))
        erg))))

(defalias 'py-statement-forward 'py-end-of-statement)
(defalias 'py-next-statement 'py-end-of-statement)
(defalias 'py-forward-statement 'py-end-of-statement)
(defun py-end-of-statement (&optional orig origline done)
  "Go to the point just beyond the final line of the current statement. "
  (interactive)
  (save-restriction
    (widen)
    (let (erg)
      (unless (eobp)
        (let*
            ((orig (or orig (point)))
             (origline (or origline (py-count-lines)))
             (pps
              (if (featurep 'xemacs)
                  (parse-partial-sexp (point-min) (point))
                (syntax-ppss)))
             ;; use by scan-lists
             parse-sexp-ignore-comments)
          (cond
           ((and (empty-line-p) (not (eobp)))
            (while
                (and (empty-line-p) (not (eobp)))
              (forward-line 1))
            (py-end-of-statement orig origline done))
           ((and (not done)(looking-at "\"\"\"\\|'''\\|\"\\|'"))
            (goto-char (match-end 0))
            (while (and (re-search-forward (match-string-no-properties 0) nil (quote move) 1)(setq done t)
                        (nth 3
                             (if (featurep 'xemacs)
                                 (parse-partial-sexp (point-min) (point))
                               (syntax-ppss)))))
            (py-end-of-statement orig origline done))
           ;; inside string
           ((nth 8 pps)
            (cond
             ((nth 3 pps)
              (goto-char (nth 8 pps))
            (when (looking-at "\"\"\"\\|'''")
              (goto-char (match-end 0))
                (while (and (re-search-forward (match-string-no-properties 0) nil (quote move) 1)
                            (setq done nil)
                            (nth 3
                                 (if (featurep 'xemacs)
                                     (parse-partial-sexp (point-min) (point))
                                   (syntax-ppss)))))
            (setq done t)
            (end-of-line)
            (skip-chars-backward " \t\r\n\f" (line-beginning-position))
            (setq erg (point))
                (py-end-of-statement orig origline done)))
           ;; in comment
                  ((nth 4 pps)
            (if (eobp)
                nil
              (forward-line 1)
              (end-of-line)
              (skip-chars-backward " \t\r\n\f" (line-beginning-position))
              (setq erg (point))
              (setq done t)
                (py-end-of-statement orig origline done)))))
           ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*"))
            (while (and (looking-at "[ \t]*#") (forward-line 1)(not (eobp))
                        (beginning-of-line))
              (setq done t))
            (end-of-line)
            (when (and done (looking-at "[ \t]*$") (not (looking-back "^[ \t]*")))
              (py-beginning-of-comment)
              (skip-chars-backward " \t\r\n\f"))
            (py-end-of-statement orig origline done))
           ((py-current-line-backslashed-p)
            (py-forward-line)
            (setq done t)
            (py-end-of-statement orig origline done))
           ;; start of innermost containing list; nil if none.
           ((nth 1 pps)
            (when (< orig (point))
              (setq orig (point)))
            (goto-char (nth 1 pps))
            (let ((parse-sexp-ignore-comments t))
              (if (ignore-errors (forward-list))
                  (progn
                    (when (looking-at ":[ \t]*$")
                      (forward-char 1))
                    (setq done t)
                    (py-end-of-statement orig origline done))
                (goto-char orig))))
           ((eq (point) orig)
            (cond ((not (looking-at "[ \t]*$"))
            (end-of-line)
            (py-beginning-of-comment)
            (skip-chars-backward " \t")
            (if (< orig (point))
                (py-end-of-statement orig origline t)
              (py-forward-line)
              (py-end-of-statement orig origline done)))
                  ((and (looking-at "[ \t]*$")(not (eobp)))
            (py-forward-line)
            (setq done t)
            (py-end-of-statement orig origline done))
                  ((not (eobp))
            (py-forward-line)
                   (py-end-of-statement orig origline done))))
           ((and (bolp) (not (empty-line-p)))
            (end-of-line)
            (skip-chars-backward " \t\r\n\f" (line-beginning-position))
            (py-beginning-of-comment)
            (setq done t)
            (py-end-of-statement orig origline done))
           ((looking-at "\\.\\([A-Za-z_][A-Za-z_0-9]*\\)")
            (forward-char 1)
            (skip-chars-forward "A-Za-z_0-9")
            (forward-char 1)
            (py-end-of-statement orig origline done)))
      (unless (or (eq (point) orig)(empty-line-p)
                  (if (featurep 'xemacs)
                      (nth 4 (parse-partial-sexp (point-min) (point)))
                    (nth 4 (syntax-ppss)))
                  (eq 0 (current-column)))
        (setq erg (point)))
      (when (interactive-p) (message "%s" erg))
          ;; (message "%s" erg)
          erg)))))

(defun py-goto-statement-below ()
  "Goto beginning of next statement. "
  (interactive)
  (let ((orig (point))
                (erg (py-end-of-statement)))
    (py-beginning-of-statement)
    (when (< (point) orig)
      (goto-char erg)
      (py-end-of-statement)
      (py-beginning-of-statement))))

;; Mark forms
(defun py-mark-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "expression")
  (exchange-point-and-mark))

(defun py-mark-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.
\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions. "
  (interactive)
  (py-mark-base "partial-expression")
  (exchange-point-and-mark))

(defun py-mark-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "statement")
  (exchange-point-and-mark))

(defun py-mark-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "block")
  (exchange-point-and-mark))

(defun py-mark-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "block-or-clause")
  (exchange-point-and-mark))

(defun py-mark-def-or-class (&optional arg)
  "Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or (eq 4 (prefix-numeric-value arg))  py-mark-decorators)))
    (py-mark-base "def-or-class" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-class (&optional arg)
  "Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
    (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py-mark-base "class" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-def (&optional arg)
  "Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are marked too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
    (let ((py-mark-decorators (or arg py-mark-decorators)))
    (py-mark-base "def" py-mark-decorators)
    (exchange-point-and-mark)))

(defun py-mark-clause ()
  "Mark clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (py-mark-base "clause")
  (exchange-point-and-mark))

(defun py-beginning-of-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful "
  (interactive)
  (back-to-indentation)
  (while (and (not (looking-at "@\\w+"))(not (empty-line-p))(not (bobp))(forward-line -1))
    (back-to-indentation))
  (let ((erg (when (looking-at "@\\w+")(match-beginning 0))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-end-of-decorator ()
    "Go to the end of a decorator.

Returns position if succesful "
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at "@\\w+")
      (setq erg (py-beginning-of-decorator)))
    (when erg
      (if
          (re-search-forward py-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            (py-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (py-in-list-p)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-mark-base (form &optional py-mark-decorators)
  (let* ((begform (intern-soft (concat "py-beginning-of-" form)))
         (endform (intern-soft (concat "py-end-of-" form)))
         (begcheckform (intern-soft (concat "py-beginning-of-" form "-p")))
         (orig (point))
         beg end erg)
    (setq beg (if
                  (setq beg (funcall begcheckform))
                  beg
                (funcall begform)))
    (when py-mark-decorators
      (save-excursion
        (when (setq erg (py-beginning-of-decorator))
        (setq beg erg))))
    (setq end (funcall endform))
    (push-mark beg t t)
    (unless end (when (< beg (point))
                  (setq end (point))))
    (when (interactive-p) (message "%s %s" beg end))
    (cons beg end)))

;; Copying

(defalias 'py-expression 'py-copy-expression)
(defun py-copy-expression ()
  "Mark expression at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-partial-expression 'py-copy-partial-expression)
(defalias 'py-minor-expression 'py-partial-expression)
(defun py-copy-partial-expression ()
  "Mark partial-expression at point.

Returns beginning and end positions of marked area, a cons.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions.

Given the function below, `py-partial-expression'
called at pipe symbol would copy and return:

def usage():
    print \"\"\"Usage: %s
    ....\"\"\" % (
        os.path.basename(sys.argv[0]))
------------|-------------------------
==> path

        os.path.basename(sys.argv[0]))
------------------|-------------------
==> basename(sys.argv[0]))

        os.path.basename(sys.argv[0]))
--------------------------|-----------
==> sys

        os.path.basename(sys.argv[0]))
------------------------------|-------
==> argv[0]

while `py-expression' would copy and return

(
        os.path.basename(sys.argv[0]))

;;;;;

Also for existing commands a shorthand is defined:

\(defalias 'py-statement 'py-copy-statement)"

  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-statement 'py-copy-statement)
(defun py-copy-statement ()
  "Mark statement at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "statement")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block 'py-copy-block)
(defun py-copy-block ()
  "Mark block at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-block-or-clause 'py-copy-block-or-clause)
(defun py-copy-block-or-clause ()
  "Mark block-or-clause at point.

Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-def 'py-copy-def)
(defun py-copy-def (&optional arg)
  "Mark def at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defun py-copy-def-or-class (&optional arg)
  "Mark def-or-class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."
  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "def-or-class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-class 'py-copy-class)
(defun py-copy-class (&optional arg)
  "Mark class at point.

With universal argument or `py-mark-decorators' set to `t' decorators are copied too.
Returns beginning and end positions of marked area, a cons."

  (interactive "P")
  (let ((py-mark-decorators (or arg py-mark-decorators))
        (erg (py-mark-base "class" py-mark-decorators)))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

(defalias 'py-clause 'py-copy-clause)
(defun py-copy-clause ()
  "Mark clause at point.
  Returns beginning and end positions of marked area, a cons. "
  (interactive)
  (let ((erg (py-mark-base "clause")))
    (kill-new (buffer-substring-no-properties (car erg) (cdr erg)))))

;; Deleting
(defun py-kill-expression ()
  "Delete expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive)
  (let ((erg (py-mark-base "expression")))
    (kill-region (car erg) (cdr erg))))

(defalias 'py-kill-minor-expression 'py-kill-partial-expression)
(defun py-kill-partial-expression ()
  "Delete partial-expression at point.
  Stores data in kill ring. Might be yanked back using `C-y'.

\".\" operators delimit a partial-expression expression on it's level, that's the difference to compound expressions."
  (interactive)
  (let ((erg (py-mark-base "partial-expression")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-statement ()
  "Delete statement at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "statement")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block ()
  "Delete block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-block-or-clause ()
  "Delete block-or-clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "block-or-clause")))
    (kill-region (region-beginning) (region-end))))

(defun py-kill-def-or-class ()
  "Delete def-or-class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def-or-class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-class ()
  "Delete class at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "class")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-def ()
  "Delete def at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "def")))
    (kill-region (car erg) (cdr erg))))

(defun py-kill-clause ()
  "Delete clause at point.

Stores data in kill ring. Might be yanked back using `C-y'. "
  (interactive "*")
  (let ((erg (py-mark-base "clause")))
    (kill-region (car erg) (cdr erg))))

;;; Helper functions

(defun py-forward-line (&optional arg)
  "Goes to end of line after forward move.

Travels right-margin comments. "
  (interactive "p")
  (let ((arg (or arg 1)))
    (forward-line arg)
    (end-of-line)
    (skip-chars-backward " \t")
    (py-beginning-of-comment)
    (skip-chars-backward " \t")))

(defun py-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any. "
  (interactive)
  (save-restriction
    (widen)
    (let ((pps
                   (if (featurep 'xemacs)
                       (parse-partial-sexp (line-beginning-position) (point))
                     (syntax-ppss))))
      (when (nth 4 pps)
        (goto-char
         (nth 8 pps))))))

(defun py-clause-lookup-keyword (regexp arg &optional indent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (origline (py-count-lines))
        (stop (if (< 0 arg)'(eobp)'(bobp)))
        (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
        (count 1)
        (maxindent (or indent (current-indentation)))
        (complement-re
         (cond ((or (string-match "finally" regexp)
                    (string-match "except" regexp))
                py-try-re)
               ((string-match "elif" regexp)
                py-if-re)
               ((string-match "else" regexp)
                py-minor-block-re)))
        (first t)
        erg done)
    (while (and (not (eval stop))
                (< 0 count)
                (or done (setq erg (funcall function))))
      (setq done nil)
      (when (and first (< maxindent (current-indentation)))
        (setq maxindent (current-indentation))
        (setq first nil))
      (when (< (current-indentation) maxindent)
        (unless (looking-at py-block-or-clause-re)
          (setq maxindent (current-indentation)))
        ;; (message "%s %s" count indent)
        ;; nesting
        (cond
         ((and (looking-at "\\_<finally\\>[: \n\t]")(save-match-data (string-match regexp "finally")))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try"))))))
         ((and (looking-at "\\_<expcept\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<else\\>[: \n\t]")(save-match-data (string-match "else" regexp)))
          (setq indent (current-indentation))
          (setq count (1+ count))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               (not (and (eq indent (current-indentation)) (looking-at "try\\|if"))))))
         ((and (looking-at "\\_<elif\\>[ \n\t]")(save-match-data (string-match "elif" regexp)))
          (setq indent (current-indentation))
          (while
              (and
               (not (eval stop))
               (funcall function)
               (setq done t)
               ;; doesn't mean nesting yet
               (setq count (1- count))
               (not (and (eq indent (current-indentation)) (looking-at "if"))))))
         ((and (looking-at complement-re)(< (current-indentation) maxindent))
          (setq count (1- count)))
         (t (when (and (string-match "except" regexp)(looking-at py-block-re))
              (setq count (1- count)))))))
    (when erg
      (if (looking-at py-def-or-class-re)
          (setq erg (cons (+ (current-indentation) py-indent-offset) erg))
        (setq erg (cons (current-indentation) erg))))
    erg))

(defun py-go-to-keyword (regexp arg &optional maxindent)
  "Returns a list, whose car is indentation, cdr position. "
  (let ((orig (point))
        (origline (py-count-lines))
        (stop (if (< 0 arg)'(eobp)'(bobp)))
        (function (if (< 0 arg) 'py-end-of-statement 'py-beginning-of-statement))
        (maxindent maxindent)
        done erg cui)
    (while (and (or (not done)(eq origline (py-count-lines)))
                (not (eval stop)))
      (funcall function)
      (when (and (looking-at regexp)(if maxindent
                                      (< (current-indentation) maxindent)t))
        (setq erg (point))
        (setq done t)))
    (when erg (setq erg (cons (current-indentation) erg)))
    erg))

(defun py-leave-comment-or-string-backward (&optional pos)
  "If inside a comment or string, leave it backward. "
  (interactive)
  (let ((pps
         (if (featurep 'xemacs)
             (parse-partial-sexp (point-min) (point))
           (syntax-ppss))))
    (when (nth 8 pps)
      (goto-char (1- (nth 8 pps))))))

(defun py-beginning-of-list-pps (&optional iact last ppstart orig done)
  "Go to the beginning of a list.
Optional ARG indicates a start-position for `parse-partial-sexp'.
Return beginning position, nil if not inside."
  (interactive "p")
  (let* ((orig (or orig (point)))
         (ppstart (or ppstart (re-search-backward "^[a-zA-Z]" nil t 1) (point-min)))
         erg)
    (unless done (goto-char orig))
    (setq done t)
    (if
        (setq erg (nth 1 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (syntax-ppss))))
        (progn
          (setq last erg)
          (goto-char erg)
          (py-beginning-of-list-pps iact last ppstart orig done))
      (when iact (message "%s" last))
      last)))

(when (featurep 'thing-at-point-utils)
  (defun py-beginning-of-list (&optional iact orig limit done last)
    "Go to beginning of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-beginning-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last))
            (when iact (message "%s" last))
            last)))))

  (defun py-end-of-list (&optional iact orig limit done last)
    "Go to end of any parentized, braced or bracketed expression in statement. "
    (interactive "p")
    (save-restriction
      (let ((orig (or orig (point)))
            (done done)
            (limit (or limit (re-search-backward "^[a-zA-Z]" nil t 1)))
            (last last))
        (unless (or done (not limit)) (narrow-to-region limit (point-max)))
        (setq done t)
        (goto-char orig)
        (let* ((pt (car-safe (ar-in-parentized-p-atpt)))
               (br (car-safe (ar-in-braced-p-atpt)))
               (bk (car-safe (ar-in-bracketed-p-atpt)))
               (erg (car (sort (delq nil (list pt br bk)) '<))))
          (if erg
              (progn
                (goto-char (1- erg))
                (setq last erg)
                (py-end-of-list iact (1- erg) limit done last))
            (when last
              (goto-char last)
              (match-paren)
              (setq last (1+ (point)))
              (when iact (message "%s" last))
              last)))))))

;; Complementary left corner commands start
(defun py-down-block-lc ()
  "Goto beginning of line following end of block.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-block' stops at right corner.

See also `py-down-block': down from current definition to next beginning of block below. "
  (interactive)
  (let ((erg (py-end-of-block)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-clause-lc ()
  "Goto beginning of line following end of clause.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-clause' stops at right corner.

See also `py-down-clause': down from current definition to next beginning of clause below. "
  (interactive)
  (let ((erg (py-end-of-clause)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-down-def-lc ()
  "Goto beginning of line following end of def.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-def' stops at right corner.

See also `py-down-def': down from current definition to next beginning of def below. "
  (interactive)
  (let ((erg (py-end-of-def)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-down-class-lc ()
  "Goto beginning of line following end of class.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-class' stops at right corner.

See also `py-down-class': down from current definition to next beginning of class below. "
  (interactive)
  (let ((erg (py-end-of-class)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

(defun py-down-statement-lc ()
  "Goto beginning of line following end of statement.

Returns position reached, if successful, nil otherwise.

\"-lc\" stands for \"left-corner\" - a complementary command travelling left, whilst `py-end-of-statement' stops at right corner.

See also `py-down-statement': down from current definition to next beginning of statement below. "
  (interactive)
  (let ((erg (py-end-of-statement)))
    (when erg
      (unless (eobp)
        (forward-line 1)
        (beginning-of-line)
        (setq erg (point))))
  (when (interactive-p) (message "%s" erg))
  erg))

;; Complementary left corner commands end

;; Py-down commands start
(defun py-down-statement ()
  "Go to the beginning of next statement below in buffer.

Returns indentation if statement found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (progn
        (when (setq erg (py-end-of-statement))
          (if (< orig (setq erg (py-beginning-of-statement-position)))
              (goto-char erg)
            (setq erg (py-end-of-statement))
            (when erg
              (py-beginning-of-statement))))
        (when erg
          (setq erg (current-column)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-block ()
  "Go to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (re-search-forward py-block-re nil (quote move))
                  (nth 8 (if (featurep 'xemacs)
                             (parse-partial-sexp ppstart (point))
                           (syntax-ppss)))))
    (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-down-clause ()
  "Go to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-clause-re)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-block-or-clause ()
  "Go to the beginning of next block-or-clause below in buffer.

Returns indentation if block-or-clause found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-block-or-clause-re)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-def ()
  "Go to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-def-re)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-class ()
  "Go to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-class-re)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-down-def-or-class ()
  "Go to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "
  (interactive)
  (let* ((orig (point))
         erg)
    (if (eobp)
        (setq erg nil)
      (while (and (setq erg (py-down-statement))(not (looking-at py-def-or-class-re)))))
    (when (interactive-p) (message "%s" erg))
    erg))
;; Py-down commands end

;; ripped from cc-mode
(defun py-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.

With \\[universal-argument] (programmatically, optional argument ARG), do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
        (re-search-forward
         "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
         (point-max) t arg)
      (while (and (< arg 0)
                  (re-search-backward
                   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
                   (point-min) 0))
        (forward-char 1)
        (setq arg (1+ arg))))))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.

With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (py-forward-into-nomenclature (- arg)))

(defalias 'py-match-paren 'match-paren)

(defun match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg \C-u insert a `%'. "
  (interactive "P")
  (let ((parse-sexp-ignore-comments t))
    (if arg
        (self-insert-command (if (numberp arg) arg 1))
      (cond
       ((and (not match-paren-no-use-syntax-pps) (looking-at "\\s("))
        (forward-list 1)
        (backward-char 1))
       ((and (not match-paren-no-use-syntax-pps)(looking-at "\\s)"))
        (forward-char 1) (backward-list 1))
       ;; if match-paren-no-syntax-pps
       ((looking-at "(")
        (ar-parentized-end-atpt))
       ((looking-at ")")
        (ar-parentized-beginning-atpt))
       ((looking-at "\\\[")
        (ar-bracketed-end-atpt))
       ((looking-at "]")
        (ar-bracketed-beginning-atpt))
       ((looking-at "{")
        (ar-braced-end-atpt))
       ((looking-at "}")
        (ar-braced-beginning-atpt))
       (t (self-insert-command 1))))))

(defun py-travel-current-indent (listing)
  "Moves down until clause is closed, i.e. current indentation is reached.

Takes a list, INDENT and START position. "
  (let ((start (ignore-errors (cdr listing)))
        (indent (ignore-errors (car listing)))
        last)
    (if start
        (progn
          (goto-char start)
          (while (and (setq last (point))(not (eobp))(py-end-of-statement)
                      (<= indent (progn (save-excursion (py-beginning-of-statement)(current-indentation))))
                      (not (and (empty-line-p)(or (nth 0 (syntax-ppss)))(nth 8 (syntax-ppss))))))
          (when last (goto-char last))
          last))))

;;; python-mode-execute.el

(defcustom py-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.

If a file is loaded into a buffer that is in one of these major modes, it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python)

(defcustom py-shell-prompt-alist
  '(("ipython" . "^In \\[[0-9]+\\]: *")
    (t . "^>>> "))
  "Alist of Python input prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python
  :version "24.1")

(defcustom py-shell-continuation-prompt-alist
  '(("ipython" . "^   [.][.][.]+: *")
    (t . "^[.][.][.] "))
  "Alist of Python continued-line prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt for
continued lines.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python
  :version "24.1")

(defcustom py-cleanup-temporary  t
 "If temporary buffers and files used by functions executing region  should be deleted afterwards. "

:type 'boolean
:group 'python
)

(defvar py-prev-dir/file nil
  "Caches (directory . file) pair used in the last `py-load-file' command.
Used for determining the default in the next one.")

(defvar py-exception-buffer nil)

(defvar py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

(defvar py-execute-keep-temporary-file-p nil
  "For tests only. Excute functions delete temporary files default. ")

(defun py-toggle-execute-keep-temporary-file-p ()
  "Toggle py-execute-keep-temporary-file-p "
  (interactive)
  (setq py-execute-keep-temporary-file-p
        (not py-execute-keep-temporary-file-p))
  (when (interactive-p) (message "py-execute-keep-temporary-file-p: %s" py-execute-keep-temporary-file-p)))

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;;remove ansi terminal escape sequences from string
  (setq string (ansi-color-filter-apply string))
  (when (and (string-match py-shell-input-prompt-1-regexp string)
             py-file-queue)
    (if py-shell-switch-buffers-on-execute
        (pop-to-buffer (current-buffer)))
    (ignore-errors (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
        (let ((pyproc (get-buffer-process (current-buffer))))
          (py-execute-file pyproc (car py-file-queue))))))

(defun py-guess-default-python ()
  "If any Python is installed. Used by `py-shell' if `py-shell-name' is neither selected nor has a customized default value. "
  (interactive)
  (let* ((cmd (or (py-choose-shell) "python"))
         (erg (executable-find cmd)))
    (when (interactive-p)
      (if erg
          (message "%s" cmd)
        (message "%s" "Could not detect Python on your system")))))

(defun py-process-name (&optional name dedicated)
  "Return the name of the running Python process, `get-process' willsee it. "
  (let* ((name (cond (dedicated
                      (make-temp-name (concat (or name py-shell-name) "-")))
                     ((string-match "\*" (buffer-name))
                      (replace-regexp-in-string "\*" "" (buffer-name)))
                     (t (or name py-shell-name))))
         (erg (if (string= "ipython" name)
                  "IPython"
                (capitalize name))))
    erg))

;; from ipython.el
(defun py-dirstack-hook ()
  ;; the following is to synchronize dir-changes
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

(defalias 'py-dedicated-shell 'py-shell-dedicated)
(defun py-shell-dedicated (&optional argprompt)
   "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
"
  (interactive "P")
  (py-shell argprompt t))

(defun py-shell (&optional argprompt dedicated)
  "Start an interactive Python interpreter in another window.

With optional \\[universal-argument] user is prompted by
`py-choose-shell' for command and options to pass to the Python
interpreter.
Returns variable `py-process-name' used by function `get-process'.
"
  (interactive "P")
  ;; Set or select the shell if not ready
  (if (eq 4 (prefix-numeric-value argprompt))
      (py-choose-shell '(4))
    (when (null py-shell-name)
      (py-guess-default-python)))
  (let ((args py-python-command-args)
        (py-process-name (py-process-name py-shell-name dedicated)))
    ;; comint
    (if (not (equal (buffer-name) py-process-name))
        (switch-to-buffer-other-window
         (apply 'make-comint py-process-name py-shell-name nil args))
      (apply 'make-comint py-process-name py-shell-name nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp (concat py-shell-input-prompt-1-regexp "\\|"
                                       py-shell-input-prompt-2-regexp "\\|"
                                       "^([Pp]db) "))
    (add-hook 'comint-output-filter-functions
              'py-comint-output-filter-function)
    (setq comint-input-sender 'py-shell-simple-send)
    (setq comint-input-ring-file-name
          (if (string-equal py-shell-name "ipython")
              (if (getenv "IPYTHONDIR")
                  (concat (getenv "IPYTHONDIR") "/history") "~/.ipython/history")
            (if (getenv "PYTHONHISTORY")
                (concat (getenv "PYTHONHISTORY") "/" py-shell-name "_history")
              (concat "~/." py-shell-name "_history"))))
    ;; (message "comint-input-ring-file-name: %s" comint-input-ring-file-name)
    (comint-read-input-ring t)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          #'shell-write-history-on-exit)
    ;; pdbtrack
    (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
    (setq py-pdbtrack-do-tracking-p t)
    ;;
    (set-syntax-table py-mode-syntax-table)
    (ansi-color-for-comint-mode-on)
    (use-local-map py-shell-map)
    ;; ToDo: has only effect \w IPython
    (add-hook 'py-shell-hook 'py-dirstack-hook)
    (run-hooks 'py-shell-hook)
    (when (interactive-p) (message "%s" py-process-name))
    py-process-name))

;;; Python named shells
(defun python (&optional argprompt)
  "Start an Python interpreter in another window.

With optional \\[universal-argument] user is prompted
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
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun python3.2 (&optional argprompt)
  "Start an Python3.2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3.2 interpreter. "
  (interactive)
  (let ((py-shell-name "python3.2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

(defun ipython (&optional argprompt)
  "Start an IPython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the IPython interpreter. "
  (interactive)
  (let ((py-shell-name "ipython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'ipython-complete)
    (py-shell argprompt)))

(defun jython (&optional argprompt)
  "Start an Jython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Jython interpreter. "
  (interactive)
  (let ((py-shell-name "jython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt)))

;;; Python dedicated shells
(defun python-dedicated (&optional argprompt)
  "Start an unique Python interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python interpreter. "
  (interactive)
  (let ((py-shell-name "python"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))

(defun python2-dedicated (&optional argprompt)
  "Start an unique Python2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2 interpreter. "
  (interactive)
  (let ((py-shell-name "python2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))

(defun python2.7-dedicated (&optional argprompt)
  "Start an unique Python2.7 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python2.7 interpreter. "
  (interactive)
  (let ((py-shell-name "python2.7"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))

(defun python3-dedicated (&optional argprompt)
  "Start an unique Python3 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3 interpreter. "
  (interactive)
  (let ((py-shell-name "python3"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))

(defun python3.2-dedicated (&optional argprompt)
  "Start an unique Python3.2 interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Python3.2 interpreter. "
  (interactive)
  (let ((py-shell-name "python3.2"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))

(defun ipython-dedicated (&optional argprompt)
  "Start an unique IPython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the IPython interpreter. "
  (interactive)
  (let ((py-shell-name "ipython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'ipython-complete)
    (py-shell argprompt t)))

(defun jython-dedicated (&optional argprompt)
  "Start an unique Jython interpreter in another window.

With optional \\[universal-argument] user is prompted
for options to pass to the Jython interpreter. "
  (interactive)
  (let ((py-shell-name "jython"))
    (local-unset-key [tab])
    (define-key py-shell-map [tab] 'py-shell-complete)
    (py-shell argprompt t)))


;; Code execution commands
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python
  :version "23.3")
(defvar py-execute-directory nil
  "Stores the file's directory-name py-execute-... functions act upon. ")

(defun py-which-execute-file-command (filename)
  "Return the command appropriate to Python version.

Per default it's \"(format \"execfile(r'%s') # PYTHON-MODE\\n\" filename)\" for Python 2 series."
  (interactive)
  (let* ((erg (py-which-python))
         (cmd (if (< erg 3)
                  (format "execfile(r'%s') # PYTHON-MODE\n" filename)
                (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" filename filename))))
    (when (interactive-p) (message "%s" (prin1-to-string cmd)))
    cmd))

(defun py-execute-region-no-switch (start end &optional async)
  "Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', buffer with region stays current.
 "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute nil))
    (py-execute-base start end async)))

(defun py-execute-region-switch (start end &optional async)
  "Send the region to a common shell calling a Python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to.
"
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
  (py-execute-base start end async)))

(defun py-execute-region (start end &optional async)
  "Send the region to a common shell calling a Python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async))

(defun py-execute-region-dedicated (start end &optional async shell)
  "Get the region processed by an unique Python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async shell t))

(defun py-execute-base (start end &optional async shell dedicated)
  "Adapt the variables used in the process. "
  (let* ((regbuf (current-buffer))
         (py-execute-directory (or (ignore-errors (file-name-directory (buffer-file-name))) (getenv "HOME")))
         (strg (buffer-substring-no-properties start end))
	 (name-raw (or shell (py-choose-shell)))
         (name (py-process-name name-raw))
         (temp (make-temp-name name))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (filebuf (get-buffer-create file))
         (proc (if dedicated
                   (get-process (py-shell nil dedicated))
                 (get-process (py-shell))))
         (procbuf (if dedicated
                      (buffer-name (get-buffer (current-buffer)))
                    (buffer-name (get-buffer (concat "*" name "*"))))))
    ;; py-shell might kill temp-buffer, bug?
    ;; (set-buffer regbuf)
    (py-execute-intern strg procbuf proc temp file filebuf name py-execute-directory)))

(defun py-execute-intern (strg &optional procbuf proc temp file filebuf name py-execute-directory)
  (let (
        (pec (if (string-match "Python3" name)
         (format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file)
         (format "execfile(r'%s') # PYTHON-MODE\n" file)))
        shell)
    (set-buffer filebuf)
    (erase-buffer)
    (insert strg)
    ;; (switch-to-buffer (current-buffer))
    (py-fix-start (point-min)(point-max))
    (py-if-needed-insert-shell name)
    (py-insert-coding)
    (py-insert-execute-directory)

    (cond
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
        (set-buffer filebuf)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((tempbuf (generate-new-buffer-name py-output-buffer))
             (arg (if (string-match name "Python")
                      "-u" "")))
        (start-process name tempbuf shell arg file)
        (pop-to-buffer tempbuf)
        (py-postprocess-output-buffer tempbuf)
        ;; TBD: clean up the temporary file!
        ))
     (proc
      ;; use the existing python shell
      (set-buffer filebuf)
      (write-region (point-min) (point-max) file nil t nil 'ask)
      (set-buffer-modified-p 'nil)
      (kill-buffer filebuf)
      (sit-for 0.1)
      (if (file-readable-p file)
          (progn
            (py-execute-file proc file pec)
            (setq py-exception-buffer (cons file (current-buffer)))
            (if py-shell-switch-buffers-on-execute
                (progn
                  (pop-to-buffer procbuf)
                  (goto-char (point-max)))
              (when (buffer-live-p regbuf) (pop-to-buffer regbuf))
              (message "Output buffer: %s" procbuf))
            (sit-for 0.1)
            (unless py-execute-keep-temporary-file-p
              (delete-file file)
              (when (buffer-live-p file)
                (kill-buffer file))))
        (message "File not readable: %s" "Do you have write permissions?"))))))

(defun py-execute-string (string &optional async)
  "Send the argument STRING to a Python interpreter.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Python command: ")
  (with-temp-buffer
    (insert string)
    (py-execute-region (point-min) (point-max) async)))

(defun py-shell-command-on-region (start end)
  "Execute region in a shell.

Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\\xA9' "
  (interactive "r")
  (let* ((regbuf (current-buffer))
         (shell (or (py-choose-shell-by-shebang)
                    (py-choose-shell-by-import)
                    py-shell-name))
         (cmd (if (string-equal shell
                                "Jython")
                  "jython -" "python")))
    (with-temp-buffer
      (insert-buffer-substring regbuf start end)
      (shell-command-on-region (point-min) (point-max)
                               cmd py-output-buffer)
      ;; shell-command-on-region kills the output buffer if it never
      ;; existed and there's no output from the command
      (if (not (get-buffer py-output-buffer))
          (message "No output.")
        (setq py-exception-buffer py-output-buffer)
        (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
          (when py-shell-switch-buffers-on-execute
            (pop-to-buffer py-output-buffer))
          (if err-p
              (pop-to-buffer py-exception-buffer)))))))

(defun py-ipython-shell-command-on-region (start end)
  "Execute region in a shell.

Avoids writing to temporary files.

Caveat: Can't be used for expressions containing
Unicode strings like u'\\xA9' "
  (interactive "r")
  (let* ((regbuf (current-buffer))
         (shell "ipython")
         (cmd "ipython")
         (prompt_in1 ""))
    (with-temp-buffer
      (insert-buffer-substring regbuf start end)
      (shell-command-on-region (point-min) (point-max)
                               cmd py-output-buffer)
      ;; shell-command-on-region kills the output buffer if it never
      ;; existed and there's no output from the command
      (if (not (get-buffer py-output-buffer))
          (message "No output.")
        (setq py-exception-buffer py-output-buffer)
        (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
          (when py-shell-switch-buffers-on-execute
            (pop-to-buffer py-output-buffer))
          (if err-p
              (pop-to-buffer py-exception-buffer)))))))

(defun py-send-region-ipython (start end)
  "Execute the region through an ipython shell. "
  (interactive "r")
  ;; Skip ahead to the first non-blank line
  (let* ((name (concat "*" "IPython" "*"))
         (py-shell-name "ipython")
         (regbuf (current-buffer))
         (first (progn (and (buffer-live-p (get-buffer name))
                            (processp (get-process name))
                            (buffer-name (get-buffer name)))))
         (procbuf (or first (progn
                              (py-shell)
                              (buffer-name (get-buffer name)))))
         (cmd "#-*- coding: utf-8 -*-\n")
         ;; (lines (count-lines start end))
         shell)
    (setq cmd (concat cmd (buffer-substring-no-properties start end)))
    ;; Set the shell either to the #! line command, or to the
    ;; py-shell-name buffer local variable.
    (setq shell (or (py-choose-shell-by-shebang)
                    (py-choose-shell-by-import)
                    py-shell-name))
    (set-buffer procbuf)
    (goto-char (point-max))
    (switch-to-buffer procbuf)
    (insert cmd)
    (comint-send-input)
;;    (ipython-send-and-indent)
    ;; (when (< 1 lines)
;;      (goto-char (point-max))
    ;; (comint-send-input))
    ))

(defun ipython-get-indenting-buffer ()
 "Return a temporary buffer set in python-mode. Create one if necessary."
 (let ((buf (get-buffer-create ipython-indenting-buffer-name)))
   (set-buffer buf)
   (unless (eq major-mode 'python-mode)
     (python-mode))
   buf))

(defvar ipython-autoindent t
 "If non-nil, enable autoindent for IPython shell through python-mode.")

(defun ipython-send-and-indent ()
 "Send the current line to IPython, and calculate the indentation for
the next line."
 (interactive)
 (if ipython-autoindent
     (let ((line (buffer-substring (point-at-bol) (point)))
           (after-prompt1)
           (after-prompt2))
       (save-excursion
           (comint-bol t)
           (if (looking-at py-shell-input-prompt-1-regexp)
               (setq after-prompt1 t)
             (setq after-prompt2 (looking-at py-shell-input-prompt-2-regexp)))
           (with-current-buffer (ipython-get-indenting-buffer)
             (when after-prompt1
               (erase-buffer))
             (when (or after-prompt1 after-prompt2)
               (delete-region (point-at-bol) (point))
               (insert line)
               (newline-and-indent))))))
 ;; send input line to ipython interpreter
 (comint-send-input))

(defun py-execute-region-in-shell (start end &optional async)
  "Execute the region in a Python shell. "
  (interactive "r\nP")
  (let* ((regbuf (current-buffer))
         (name (concat "*" py-shell-name "*"))
         (first (progn (and (buffer-live-p (get-buffer name))
                            (processp (get-process py-shell-name))
                            (buffer-name (get-buffer name)))))
         (procbuf (or first (progn
                              (py-shell)
                              (buffer-name (get-buffer name)))))
         (proc (get-process py-shell-name))
         (temp (make-temp-name py-shell-name))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (temp (get-buffer-create file))
         (py-line-number-offset 0)
         shell cmd)
    ;; Write the contents of the buffer, watching out for indented regions.
    (save-excursion
      (set-buffer regbuf)
      (goto-char start)
      (beginning-of-line)
      (while (and (looking-at "\\s *$")
                  (< (point) end))
        (forward-line 1))
      (setq start (point))
      (or (< start end)
          (error "Region is empty"))
      (setq py-line-number-offset (count-lines 1 start))
      (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
        (setq cmd "#-*- coding: utf-8 -*-\n")
        (when needs-if
          (setq cmd (concat cmd "if 1:\n"))
          (setq py-line-number-offset (- py-line-number-offset 1)))
        (setq cmd (concat cmd (buffer-substring-no-properties start end)))
        ;; Set the shell either to the #! line command, or to the
        ;; py-shell-name buffer local variable.
        (setq shell (or (py-choose-shell-by-shebang)
                        (py-choose-shell-by-import)
                        py-shell-name))))
    (cond
     ;; always run the code in its own asynchronous subprocess
     (async
      ;; User explicitly wants this to run in its own async subprocess
      (save-excursion
        (set-buffer temp)
        (write-region (point-min) (point-max) file nil 'nomsg))
      (let* ((temp (generate-new-buffer-name py-output-buffer))
             ;; TBD: a horrible hack, but why create new Custom variables?
             (arg (if (string-match py-shell-name "Python")
                      "-u" "")))
        (start-process py-shell-name temp shell arg file)
        (pop-to-buffer temp)
        (py-postprocess-output-buffer temp)
        ;; TBD: clean up the temporary file!
))
     ;; if the Python interpreter shell is running, queue it up for
     ;; execution there.
     (proc
      ;; use the existing python shell
      (set-buffer procbuf)
      (goto-char (point-max))
      (insert cmd)
      (switch-to-buffer (current-buffer))
      (if (functionp 'ipython-send-and-indent)
          (ipython-send-and-indent)
        (comint-send-input))
      (setq py-exception-buffer (cons file (current-buffer)))
      (switch-to-buffer procbuf))
     (t
      ;; this part is in py-shell-command-on-region now.
      (let ((cmd
             (concat shell (if (string-equal py-shell-name
                                             "Jython")
                               " -" ""))))
        ;; otherwise either run it synchronously in a subprocess
        (save-excursion
          (set-buffer temp)
          (shell-command-on-region (point-min) (point-max)
                                   cmd py-output-buffer))
        ;; shell-command-on-region kills the output buffer if it never
        ;; existed and there's no output from the command
        (if (not (get-buffer py-output-buffer))
            (message "No output.")
          (setq py-exception-buffer (current-buffer))
          (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
            (pop-to-buffer py-output-buffer)
            (if err-p
                (pop-to-buffer py-exception-buffer)))))))
    ;; Clean up after ourselves.
    (kill-buffer temp)))

(defun py-if-needed-insert-shell (&optional name)
  (let ((erg (if name (downcase name)
               (or (py-choose-shell-by-shebang)
                   (py-choose-shell-by-import)
                   py-shell-name))))
    (goto-char (point-min))
    (while (empty-line-p) (delete-region (point) (1+ (line-end-position))))
    (unless (looking-at py-shebang-regexp)
      (if (string-match (concat "^" erg) "ipython")
          (progn
            (shell-command "type ipython" t)
            (switch-to-buffer (current-buffer))
            (when (looking-at "[^/\n\r]+")
              (replace-match "#! ")))
        (insert (concat py-shebang-startstring " " erg "\n"))))
    (end-of-line)
    (newline)))

(defun py-insert-execute-directory ()
  (goto-char (point-min))
  (if (re-search-forward py-encoding-string-re nil (quote move))
      (progn
        (newline)
        (insert (concat "import os; os.chdir(\"" py-execute-directory "\")\n")))
    (goto-char (point-min))
    (forward-line 2)
    (newline)
    (insert (concat "import os; os.chdir(\"" py-execute-directory "\")\n"))))

(defun py-insert-coding ()
  ;; (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (unless (re-search-forward py-encoding-string-re nil t)
  (goto-char (point-min))
    (if (re-search-forward py-shebang-regexp nil t 1)
        (progn
          (newline)
          (insert (concat py-encoding-string "\n")))
      (insert (concat py-encoding-string "\n")))))

(defun py-if-needed-insert-if ()
  "Internal use by py-execute... functions.
Inserts an incentive true form \"if 1:\\n.\" "
  (let ((needs-if (/= (py-point 'bol) (py-point 'boi))))
    (when needs-if
      (insert "if 1:\n")
      (setq py-line-number-offset (- py-line-number-offset 1)))))

(defun py-fix-start (start end)
  "Internal use by py-execute... functions.
Avoid empty lines at the beginning. "
  (goto-char start)
  (let ((beg (copy-marker start)))
    (while (empty-line-p)
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (back-to-indentation)
    (unless (eq (current-indentation) 0)
      (py-shift-left (current-indentation) start end))
    (setq py-line-number-offset (count-lines 1 start))
    beg))

(defun py-fetch-py-master-file ()
  "Lookup if a `py-master-file' is specified.

See also doku of variable `py-master-file' "
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^ *# Local Variables:" nil (quote move) 1)
        (when
            (re-search-forward (concat "^\\( *# py-master-file: *\\)\"\\([^ \t]+\\)\" *$") nil t 1)
          (setq py-master-file (match-string-no-properties 2))))))
  (when (interactive-p) (message "%s" py-master-file)))

(defun py-execute-import-or-reload (&optional async dedicated)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
          (py-choose-shell)
          (let ((proc (if dedicated
                          (get-process (py-shell nil dedicated))
                        (get-process (py-shell)))))
          ;; Maybe save some buffers
          (save-some-buffers (not py-ask-about-save) nil)
            (py-execute-file proc file
           (if (string-match "\\.py$" file)
               (let ((m (py-qualified-module-name (expand-file-name file))))
		 (format "import sys\nif sys.modules.has_key('%s'):\n reload(%s)\nelse:\n import %s\n"
                         m m m))
             ;; (format "execfile(r'%s')\n" file)
                               (py-which-execute-file-command file)))))
      ;; else
      (py-execute-buffer async))))

(defun py-qualified-module-name (file)
  "Find the qualified module name for filename FILE.

Basically, this goes down the directory tree as long as there are __init__.py files there."
  (let ((rec #'(lambda (d f)
		 (let* ((dir (file-name-directory d))
			(initpy (concat dir "__init__.py")))
		   (if (file-exists-p initpy)
		       (let ((d2 (directory-file-name d)))
			 (funcall rec (file-name-directory d2)
				(concat (file-name-nondirectory d2) "." f)))
		     f)))))
    (funcall rec (file-name-directory file)
	     (file-name-sans-extension (file-name-nondirectory file)))))

(defun py-execute-buffer (&optional async)
  "Send the contents of the buffer to a Python interpreter.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.
If there is a *Python* process buffer, it is used.
If a clipping restriction is in effect, only the accessible portion of the buffer is sent. A trailing newline will be supplied if needed.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region (point-min) (point-max) async))))

(defun py-execute-buffer-no-switch (&optional async)
  "Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute'.

Buffer called from is current afterwards again."
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region-no-switch (point-min) (point-max) async))))

(defun py-execute-buffer-switch (&optional async)
  "Like `py-execute-buffer', but ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "P")
  (save-excursion
    (let ((old-buffer (current-buffer)))
      (or py-master-file (py-fetch-py-master-file))
      (if py-master-file
          (let* ((filename (expand-file-name py-master-file))
                 (buffer (or (get-file-buffer filename)
                             (find-file-noselect filename))))
            (set-buffer buffer)))
      (py-execute-region-switch (point-min) (point-max) async))))

;; Specifying shells start

(defun py-execute-region-python (start end &optional async)
  "Send the region to a common shell calling the python interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python"))

(defun py-execute-region-python-switch (start end &optional async)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python")))

(defun py-execute-region-python-no-switch (start end &optional async)
  "Send the region to a common shell calling the python interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python")))

(defun py-execute-region-python2 (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python2"))

(defun py-execute-region-python2-switch (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python2")))

(defun py-execute-region-python2-no-switch (start end &optional async)
  "Send the region to a common shell calling the python2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python2")))

(defun py-execute-region-python2.7 (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python2.7"))

(defun py-execute-region-python2.7-switch (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python2.7")))

(defun py-execute-region-python2.7-no-switch (start end &optional async)
  "Send the region to a common shell calling the python2.7 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python2.7")))

(defun py-execute-region-python3 (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python3"))

(defun py-execute-region-python3-switch (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python3")))

(defun py-execute-region-python3-no-switch (start end &optional async)
  "Send the region to a common shell calling the python3 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python3")))

(defun py-execute-region-python3.2 (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "python3.2"))

(defun py-execute-region-python3.2-switch (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "python3.2")))

(defun py-execute-region-python3.2-no-switch (start end &optional async)
  "Send the region to a common shell calling the python3.2 interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "python3.2")))

(defun py-execute-region-ipython (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "ipython"))

(defun py-execute-region-ipython-switch (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "ipython")))

(defun py-execute-region-ipython-no-switch (start end &optional async)
  "Send the region to a common shell calling the ipython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "ipython")))

(defun py-execute-region-jython (start end &optional async)
  "Send the region to a common shell calling the jython interpreter. "
  (interactive "r\nP")
  (py-execute-base start end async "jython"))

(defun py-execute-region-jython-switch (start end &optional async)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will being switched to. "
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute t))
    (py-execute-base start end async "jython")))

(defun py-execute-region-jython-no-switch (start end &optional async)
  "Send the region to a common shell calling the jython interpreter.

Ignores setting of `py-shell-switch-buffers-on-execute', output-buffer will not being switched to."
  (interactive "r\nP")
  (let ((py-shell-switch-buffers-on-execute))
    (py-execute-base start end async "jython")))

;; Specifying shells end

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun py-execute-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-execute-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

(defun py-process-file (filename &optional output-buffer error-buffer)
  "Process \"python filename\".

Optional OUTPUT-BUFFER and ERROR-BUFFER might be given. "
  (interactive "fDatei:")
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (output-buffer (or output-buffer (make-temp-name "py-process-file-output"))))
    (unless (buffer-live-p output-buffer)
      (set-buffer (get-buffer-create output-buffer)))
    (shell-command (concat "python " filename) output-buffer error-buffer)
    (when (interactive-p) (switch-to-buffer output-buffer))))

(defun py-exec-execfile-region (start end &optional async)
  "Execute the region in a Python interpreter. "
  (interactive "r\nP")
  (let ((strg (buffer-substring-no-properties start end)))
    (py-exec-execfile-base strg async (interactive-p))))

(defun py-exec-execfile-base (strg async iact)
  (let* ((temp (make-temp-name (concat (buffer-name) "-")))
         (file (concat (expand-file-name temp py-temp-directory) ".py"))
         (imports (py-find-imports))
         shell cmd header)
    (with-temp-buffer
      (insert imports)
      (insert strg)
;;      (py-if-needed-insert-if)
      (setq shell (py-choose-shell))
      (py-insert-coding)
      (py-if-needed-insert-shell shell)
      (setq header (buffer-substring-no-properties (point-min) (point)))
      (switch-to-buffer (current-buffer))
      (setq cmd (py-which-execute-file-command file))
      (write-file file))
    (py-exec-execfile file cmd header (concat temp "-output"))
    (set-buffer (concat temp "-output"))
    (when iact (switch-to-buffer (current-buffer)))
    (when (file-readable-p file)
      (delete-file file))
    (when iact (message "Output goes to buffer: %s" temp))
    (concat temp "-output")))

(defun py-exec-execfile (filename cmd header &optional output-buffer error-buffer)
  "Process \"python filename\",
Optional OUTPUT-BUFFER and ERROR-BUFFER might be given.')
"
  (interactive "fDatei:")
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (exec-execfile (concat (make-temp-name (concat filename "-exec-execfile.py")))))
    (set-buffer (get-buffer-create exec-execfile))
    (insert header)
    (insert cmd)
    (write-file exec-execfile)
    (if output-buffer
        (progn
          (set-buffer (get-buffer-create output-buffer))
          (erase-buffer)
          (switch-to-buffer (current-buffer))
          (shell-command (concat "python " exec-execfile) output-buffer error-buffer))
      (with-temp-buffer
        (shell-command (concat "python " exec-execfile) output-buffer error-buffer)))))

;; Execute forms at point
(defun py-execute-block ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-p)
                       (py-beginning-of-block))
                 (push-mark)))
          (end (py-end-of-block)))
      (py-execute-region beg end))))

(defun py-execute-block-or-clause ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-block-or-clause-p)
                       (py-beginning-of-block-or-clause))
                 (push-mark)))
          (end (py-end-of-block-or-clause)))
      (py-execute-region beg end))))

(defun py-execute-class ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-class-p)
                       (py-beginning-of-class))
                 (push-mark)))
          (end (py-end-of-class)))
      (py-execute-region beg end))))

(defun py-execute-clause ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-clause-p)
                       (py-beginning-of-clause))
                 (push-mark)))
          (end (py-end-of-clause)))
      (py-execute-region beg end))))

(defun py-execute-def ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-p)
                       (py-beginning-of-def))
                 (push-mark)))
          (end (py-end-of-def)))
      (py-execute-region beg end))))

(defun py-execute-def-or-class ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-def-or-class-p)
                       (py-beginning-of-def-or-class))
                 (push-mark)))
          (end (py-end-of-def-or-class)))
      (py-execute-region beg end))))

(defun py-execute-expression ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-expression-p)
                       (py-beginning-of-expression))
                 (push-mark)))
          (end (py-end-of-expression)))
      (py-execute-region beg end))))

(defun py-execute-partial-expression ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-partial-expression-p)
                       (py-beginning-of-partial-expression))
                 (push-mark)))
          (end (py-end-of-partial-expression)))
      (py-execute-region beg end))))

(defun py-execute-statement ()
  "Send python-form at point as is to Python interpreter. "
  (interactive)
  (save-excursion
    (let ((beg (prog1
                   (or (py-beginning-of-statement-p)
                       (py-beginning-of-statement))
                 (push-mark)))
          (end (py-end-of-statement)))
      (py-execute-region beg end))))

;; Python subprocess utilities and filters
(defun py-execute-file (proc filename &optional cmd)
  "Send to Python interpreter process PROC, in Python version 2.. \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
        (procbuf (process-buffer proc))
        (comint-scroll-to-bottom-on-output t)
        (msg (format "## executing %s...\n" filename))
        (cmd (cond (cmd)
                   (py-exec-command)
                   (t (py-which-execute-file-command filename)))))
    (unwind-protect
        (save-excursion
          (set-buffer procbuf)
          (goto-char (point-max))
          (move-marker (process-mark proc) (point))
          (funcall (process-filter proc) proc msg)))
    (set-buffer curbuf)
    (set-buffer procbuf)
    (process-send-string proc cmd)
    (goto-char (process-mark proc))
    ;; doubles prompt
    ;; (comint-send-input)
    ))

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward py-traceback-line-re nil t)
        (setq file (match-string 1)
              line (string-to-number (match-string 2))
              bol (py-point 'bol))
        (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                     'face 'highlight)))
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line py-line-number-offset)
      (setq err-p t))
    err-p))

(defun py-jump-to-exception (file line py-line-number-offset)
  "Jump to the Python code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
                       (if (consp py-exception-buffer)
                           (cdr py-exception-buffer)
                         py-exception-buffer))
                      ((and (consp py-exception-buffer)
                            (string-equal file (car py-exception-buffer)))
                       (cdr py-exception-buffer))
                      ((ignore-errors (find-file-noselect file)))
                      ;; could not figure out what file the exception
                      ;; is pointing to, so prompt for it
                      (t (find-file (read-file-name "Exception file: "
                                                    nil
                                                    file t))))))
    ;; Fiddle about with line number
    (setq line (+ py-line-number-offset line))

    (pop-to-buffer buffer)
    ;; Force Python mode
    (unless(eq major-mode 'python-mode)
        (python-mode))
    (goto-char (point-min))
    (forward-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.

With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
        (py-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.

With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
         (buffer (if proc "*Python*" py-output-buffer)))
    (if top
        (py-find-next-exception 'bob buffer 're-search-forward "Top")
      (py-find-next-exception 'bol buffer 're-search-backward "Top"))))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (if (and file line)
        (py-jump-to-exception file line py-line-number-offset)
      (error "%s of traceback" errwhere))))

;;; python-mode-send.el

(defun py-output-buffer-filter (&optional beg end)
  "Clear output buffer from py-shell-input prompt etc. "
  (interactive "*")
  (let ((beg (cond (beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (goto-char beg)
    (while (re-search-forward (concat "\\(" py-shell-input-prompt-1-regexp "\\|" py-shell-input-prompt-2-regexp "\\|" "^In \\[[0-9]+\\]: *" "\\)") nil (quote move) 1)
      (replace-match ""))
    (goto-char beg)))

(defun py-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (python-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (python-proc) "\n")))

;;; python-components-pdb.el

;; pdbtrack constants
(defconst py-pdbtrack-stack-entry-regexp
   (concat ".*\\("py-shell-input-prompt-1-regexp">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

;; ipython.el
;; Recognize the ipython pdb, whose prompt is 'ipdb>' or  'ipydb>'
;;instead of '(Pdb)'
(defvar py-pdbtrack-input-prompt)
(setq py-pdbtrack-input-prompt "\n[(<]*[Ii]?[Pp]y?db[>)]+ ")
(defvar pydb-pydbtrack-input-prompt)
(setq pydb-pydbtrack-input-prompt "\n[(]*ipydb[>)]+ ")

;; pydb-328837.diff
;; (defconst py-pydbtrack-stack-entry-regexp
;;   "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
;;   "Regular expression pdbtrack uses to find a stack trace entry for pydb.
;;
;; The debugger outputs program-location lines that look like this:
;;    (/usr/bin/zonetab2pot.py:15): makePOT")

(defconst py-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst py-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst py-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar py-pdbtrack-is-tracking-p nil)

;;; Pdbtrack

(defun py-pdbtrack-overlay-arrow (activation)
  "Activate or de arrow at beginning-of-line in current buffer."
  ;; This was derived/simplified from edebug-overlay-arrow
  (cond (activation
         (setq overlay-arrow-position (make-marker))
         (setq overlay-arrow-string "=>")
         (set-marker overlay-arrow-position (line-beginning-position) (current-buffer))
         (setq py-pdbtrack-is-tracking-p t))
        (overlay-arrow-position
         (setq overlay-arrow-position nil)
         (setq py-pdbtrack-is-tracking-p nil))
        ))

(defun py-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`py-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt matching `py-pdbtrack-input-prompt'
at the beginning of the line.

If the traceback target file path is invalid, we look for the most
recently visited python-mode buffer which either has the name of the
current function \(or class) or which defines the function \(or
class).  This is to provide for remote scripts, eg, Zope's 'Script
\(Python)' - put a _copy_ of the script in a buffer named for the
script, and set to python-mode, and pdbtrack will find it.)"
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') command to reveal the overlay arrow.
  (let* ((origbuf (current-buffer))
         (currproc (get-buffer-process origbuf)))

    (if (not (and currproc py-pdbtrack-do-tracking-p))
        (py-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              py-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat py-pdbtrack-input-prompt "$") block))
            (py-pdbtrack-overlay-arrow nil)

          (setq target (py-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (message "pdbtrack: %s" target)

            (setq target_lineno (car target))
            (setq target_buffer (cadr target))
            (setq target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
;;            (goto-line target_lineno)
            (goto-char (point-min))
            (forward-line target_lineno)
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (py-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)

            )))))
  )

(defun py-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem as best as we can determine."

  (if (and (not (string-match py-pdbtrack-stack-entry-regexp block))
	   ;; (not (string-match py-pydbtrack-stack-entry-regexp block))
)
      "Traceback cue not found"
    (let* ((filename (match-string
		      py-pdbtrack-marker-regexp-file-group block))
           (lineno (string-to-number (match-string
				   py-pdbtrack-marker-regexp-line-group
				   block)))
           (funcname (match-string py-pdbtrack-marker-regexp-funcname-group
				   block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (py-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (save-excursion
                            (set-buffer funcbuffer)
                            (count-lines
                             (point-min)
                             (max (point-min)
                                  (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                (buffer-substring (point-min)
                                                                  (point-max)))
                                  ))))))
             (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun py-pdbtrack-grub-for-buffer (funcname lineno)
  "Find most recent buffer itself named or having function funcname.

We walk the buffer-list history for python-mode buffers that are
named for funcname or define a function funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (save-excursion (set-buffer buf)
                               (string= major-mode "python-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (save-excursion
                                   (set-buffer buf)
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))


;; pdbtrack functions
(defun py-pdbtrack-toggle-stack-tracking (arg)
  "Set variable `py-pdbtrack-do-tracking-p'. "
  (interactive "P")
  (if (not (get-buffer-process (current-buffer)))
      (error "No process associated with buffer '%s'" (current-buffer)))
  ;; missing or 0 is toggle, >0 turn on, <0 turn off
  (if (or (not arg)
          (zerop (setq arg (prefix-numeric-value arg))))
      (setq py-pdbtrack-do-tracking-p (not py-pdbtrack-do-tracking-p))
    (setq py-pdbtrack-do-tracking-p (> arg 0)))
  (message "%sabled Python's pdbtrack"
           (if py-pdbtrack-do-tracking-p "En" "Dis")))

(defun turn-on-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 1))

(defun turn-off-pdbtrack ()
  (interactive)
  (py-pdbtrack-toggle-stack-tracking 0))

;;; python-components-help.el

(defun py-fetch-docu ()
  "Lookup in current buffer for the doku for the symbol at point.

Useful for newly defined symbol, not known to python yet. "
  (interactive)
  (let* ((symb (prin1-to-string (symbol-at-point)))
         (args (py-expression))
         erg)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (concat py-def-or-class-re " *" symb) nil (quote move) 1)
        (forward-line 1)
        (when (looking-at "[ \t]*\"\"\"\\|[ \t]*'''\\|[ \t]*'[^]+\\|[ \t]*\"[^\"]+")
          (goto-char (match-end 0))
          (setq erg (buffer-substring-no-properties (match-beginning 0) (re-search-forward "\"\"\"\\|'''" nil 'move)))
          (when erg
            (set-buffer (get-buffer-create "*Python-Help*"))
            (erase-buffer)
            (when (interactive-p) (switch-to-buffer (current-buffer)))
            (insert erg)))))))

(defun py-find-imports ()
  (let* (imports
         (erg
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^import *[A-Za-z_][A-Za-z_0-9].*\\|^from +[A-Za-z_][A-Za-z_0-9]+ +import .*" nil t)
        (setq imports
              (concat
               imports
                    (buffer-substring-no-properties (match-beginning 0) (match-end 0)) "\n"))))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defvar python-imports nil
  "Set by `py-find-imports'.")
(make-variable-buffer-local 'python-imports)

(defun python-find-imports ()
  "Find top-level imports, updating `python-imports'."
  (interactive)
  (save-excursion
      (let (lines)
	(goto-char (point-min))
	(while (re-search-forward "^import\\>\\|^from\\>" nil t)
	  (unless (syntax-ppss-context (syntax-ppss))
	    (let ((start (line-beginning-position)))
	      ;; Skip over continued lines.
	      (while (and (eq ?\\ (char-before (line-end-position)))
			  (= 0 (forward-line 1)))
		t)
	      (push (buffer-substring start (line-beginning-position 2))
		    lines))))
	(setq python-imports
	      (if lines
		  (apply #'concat
			 (nreverse lines))
		"None"))
	(when lines
	  (set-text-properties 0 (length python-imports) nil python-imports)
	  ;; The output ends up in the wrong place if the string we
	  ;; send contains newlines (from the imports).
	  (setq python-imports
		(replace-regexp-in-string "\n" "\\n"
					  (format "%S" python-imports) t t)))))
  (when (interactive-p) (message "%s" (car (read-from-string python-imports))))
  python-imports)

(defalias 'py-help-at-point 'py-describe-symbol)
(defun py-describe-symbol ()
  "Print help on symbol at point. "
  (interactive)
  (lexical-let* ((sym (prin1-to-string (symbol-at-point)))
                 (origfile (buffer-file-name))
                 (temp (make-temp-name (buffer-name)))
                 (file (concat (expand-file-name temp py-temp-directory) ".py"))
                 (cmd (py-find-imports))
                 (no-quotes (save-excursion
                              (skip-chars-backward "A-Za-z_0-9.")
                              (and (looking-at "[A-Za-z_0-9.]+")
                                   (string-match "\\." (match-string-no-properties 0))))))
    (setq cmd (concat "import pydoc\n"
                      cmd))
    (if no-quotes
        (setq cmd (concat cmd
                          "try: pydoc.help(" sym ")\n"))
      (setq cmd (concat cmd "try: pydoc.help('" sym "')\n")))
    (setq cmd (concat cmd
                      "except:
    print 'No help available on:', \"" sym "\""))
    (with-temp-buffer
      (insert cmd)
      (write-file file))
    (py-process-file file "*Python-Help*")
    (when (file-readable-p file)
      (delete-file file))))


;;; Documentation
(defun py-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
          funckind funcname func funcdoc
          (start 0) mstart end
          keys)
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
        (setq mstart (match-beginning 0) end (match-end 0)
              funckind (substring str (match-beginning 1) (match-end 1))
              funcname (substring str (match-beginning 2) (match-end 2))
              func (intern funcname))
        (princ (substitute-command-keys (substring str start mstart)))
        (cond
         ((equal funckind "c")          ; command
          (setq funcdoc (documentation func)
                keys (concat
                      "Key(s): "
                      (mapconcat 'key-description
                                 (where-is-internal func python-mode-map)
                                 ", "))))
         ((equal funckind "v")          ; variable
          (setq funcdoc (documentation-property func 'variable-documentation)
                keys (if (assq func locals)
                         (concat
                          "Local/Global values: "
                          (prin1-to-string (symbol-value func))
                          " / "
                          (prin1-to-string (default-value func)))
                       (concat
                        "Value: "
                        (prin1-to-string (symbol-value func))))))
         (t                             ; unexpected
          (error "Error in py-dump-help-string, tag `%s'" funckind)))
        (princ (format "\n-> %s:\t%s\t%s\n\n"
                       (if (equal funckind "c") "Command" "Variable")
                       funcname keys))
        (princ funcdoc)
        (terpri)
        (setq start end))
      (princ (substitute-command-keys (substring str start))))
    (if (featurep 'xemacs) (print-help-return-message)
      (help-print-return-message))
    ))

(defun py-describe-mode ()
  "Dump long form of Python-mode docs."
  (interactive)
  (py-dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-shell-name\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-shell-name
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b # a very wordy single-line comment that ends up being
\t #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The Python-mode commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.

@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-left]\t shift line or region left by py-indent-offset
\t\\[py-shift-right]\t shift line or region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by Python-mode is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t c = d
\t _
Python-mode cannot know whether that's what you intended, or whether
\tif a > 0:
\t c = d
\t_
was your intent.  In general, Python-mode either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace

The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset

The remaining `indent' functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-left
%c:py-shift-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq py-indent-offset 4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
`python-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))

;; (require 'info-look)
;; The info-look package does not always provide this function (it
;; appears this is the case with XEmacs 21.1)
(when (fboundp 'info-lookup-maybe-add-help)
  (info-lookup-maybe-add-help
   :mode 'python-mode
   :regexp "[a-zA-Z0-9_]+"
   :doc-spec '(("(python-lib)Module Index")
               ("(python-lib)Class-Exception-Object Index")
               ("(python-lib)Function-Method-Variable Index")
               ("(python-lib)Miscellaneous Index"))))

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defun py-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.

The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (py-send-string string)
  (let ((proc (python-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
                    (accept-process-output proc py-send-receive-delay)
        (if (null python-preoutput-result)
          (message "No output from: %s, maybe set `py-send-receive-delay' onto a higher value " string))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

(defun py-find-function (name)
  "Find source of definition of function NAME.

Interactively, prompt for name."
  (interactive
   (let ((symbol (with-syntax-table py-dotted-expression-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Find location of (default %s): " symbol)
			  "Find location of: ")
			nil nil symbol))))
  (unless python-imports
    (error "Not called from buffer visiting Python file"))
  (let* ((loc (py-send-receive (format "emacs.location_of (%S, %s)"
					   name python-imports)))
	 (loc (car (read-from-string loc)))
	 (file (car loc))
	 (line (cdr loc)))
    (unless file (error "Don't know where `%s' is defined" name))
    (pop-to-buffer (find-file-noselect file))
    (when (integerp line)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun py-update-imports ()
  "Returns `python-imports'.

Imports done are displayed in message buffer. "
  (interactive)
  (save-excursion
    (let ((oldbuf (current-buffer))
          (orig (point))
          erg)
      (mapc 'py-execute-string (split-string (car (read-from-string (py-find-imports))) "\n" t))
      (setq erg (car (read-from-string python-imports)))
      (set-buffer oldbuf)
      (goto-char orig)
      (when (interactive-p)
        (switch-to-buffer (current-buffer))
        (message "%s" erg))
      erg)))

;;; python-components-extensions.el
(defcustom py-match-paren-mode nil
  "*Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. "
  :type 'boolean
  :group 'python)

(defcustom py-match-paren-key "%"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-kill-empty-line t
  "*If t, py-indent-forward-line kills empty lines. "
  :type 'boolean
  :group 'python)

(defun py-indent-forward-line (&optional arg)
  "Indent and move one line forward to next indentation.
Returns column of line reached.

If `py-kill-empty-line' is non-nil, delete an empty line.
When closing a form, use py-close-block et al, which will move and indent likewise.
With \\[universal argument] just indent.
"
  (interactive "*P")
  (let ((orig (point))
        erg)
    (unless (eobp)
      (if (and (py-in-comment-p)(not py-indent-comments))
          (forward-line 1)
        (py-indent-line-outmost)
        (unless (eq 4 (prefix-numeric-value arg))
          (if (eobp) (newline)
            (progn (forward-line 1))
            (when (and py-kill-empty-line (empty-line-p) (not (looking-at "[ \t]*\n[[:alpha:]]")) (not (eobp)))
              (delete-region (line-beginning-position) (line-end-position)))))))
    (back-to-indentation)
    (when (or (eq 4 (prefix-numeric-value arg)) (< orig (point))) (setq erg (current-column)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-dedent-forward-line (&optional arg)
  "Dedent line and move one line forward. "
  (interactive "*p")
  (py-dedent arg)
  (forward-line 1)
  (end-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun py-dedent (&optional arg)
  "Dedent line according to `py-indent-offset'.

With arg, do it that many times.
If point is between indent levels, dedent to next level.
Return indentation reached, if dedent done, nil otherwise.

Affected by `py-dedent-keep-relative-column'. "
  (interactive "*p")
  (let ((orig (copy-marker (point)))
        erg)
    (dotimes (i arg)
      (let* ((cui (current-indentation))
             (remain (% cui py-indent-offset))
             (indent (* py-indent-offset (/ cui py-indent-offset))))
        (beginning-of-line)
        (fixup-whitespace)
        (if (< 0 remain)
            (indent-to-column indent)
          (indent-to-column (- cui py-indent-offset)))))
    (when (< (point) orig)
      (setq erg (current-column)))
    (if py-dedent-keep-relative-column
        (goto-char orig)
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-intern (regexp)
  "Core function, internal used only. "
  (let ((cui (ignore-errors (car (py-go-to-keyword regexp -1)))))
    (py-end-base regexp (point) (interactive-p))
    (forward-line 1)
    (if py-close-provides-newline
        (unless (empty-line-p) (split-line))
      (fixup-whitespace))
    (indent-to-column cui)
    cui))

(defun py-close-def ()
  "Set indent level to that of beginning of function definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-def-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-class ()
  "Set indent level to that of beginning of class definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-class-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-clause ()
  "Set indent level to that of beginning of clause definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-clause-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-close-block ()
  "Set indent level to that of beginning of block definition.

If final line isn't empty and `py-close-block-provides-newline' non-nil, insert a newline. "
  (interactive "*")
  (let ((erg (py-close-intern py-block-re)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-class-at-point ()
  "Return class definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (py-beginning-of-class))
	   (end (py-end-of-class))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (interactive-p) (message "%s" res))
      res)))

(defun py-function-at-point ()
  "Return functions definition as string.

With interactive call, send it to the message buffer too. "
  (interactive)
  (save-excursion
    (let* ((beg (py-beginning-of-function))
	   (end (py-end-of-function))
	   (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
      (when (interactive-p) (message "%s" res))
      res)))

(defun py-beginning-of-function (&optional class)
  "Jump to the beginning of function. Returns point. "
  (interactive "P")
  (let ((pos (py-beginning-of-def-or-class class)))
    (when (interactive-p) (message "%s" pos))
    pos))

(defun py-end-of-function (&optional class)
  "Jump to the end of function. "
  (interactive "P")
  (let ((pos (py-end-of-def-or-class class)))
    (when (interactive-p) (message "%s" pos))
    pos))

;; Functions for marking regions

(defun py-line-at-point ()
  "Return line as string.
  With interactive call, send it to the message buffer too. "
  (interactive)
  (let* ((beg (line-beginning-position))
	 (end (line-end-position))
	 (res (when (and (numberp beg)(numberp end)(< beg end)) (buffer-substring-no-properties beg end))))
    (when (interactive-p) (message "%s" res))
    res))

(defun py-looking-at-keywords-p ()
  "If looking at a python keyword. Returns t or nil. "
  (interactive)
  (let* ((kwds1 (car (nth 1 (eval (eval (quote (car font-lock-defaults)))))))
         (kwds3 (car (nth 3 (eval (eval (quote (car font-lock-defaults)))))))
	 (res
	  (or
           (looking-at kwds1)
           (looking-at kwds3))))
    (when (interactive-p) (message "looking-at keywords: %s" res))
    res))

(defun py-match-paren-mode (&optional arg)
  "py-match-paren-mode nil oder t"
  (interactive "P")
  (if (or (eq 4 (prefix-numeric-value arg)) (not py-match-paren-mode))
      (setq py-match-paren-mode t)
    (setq py-match-paren-mode nil)))

(defun py-match-paren ()
  "Goto to the opening or closing of block before or after point.

With arg, do it that many times.
 Closes unclosed block if jumping from beginning. "
  (interactive)
  (let ((cuc (current-column))
	(cid (current-indentation)))
    (py-beginning-of-block-or-clause)
    (if (< cuc (current-indentation))
	(goto-char cuc)
      (back-to-indentation)
      (when (eq (point) cuc)
	(py-end-of-block)))))

;; from sh-beg-end.el. Introduced here for convenience.

(defun py-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-shell-name " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun eva ()
  "Put \"eval(...)\" forms around strings at point. "
  (interactive "*")
  (skip-chars-forward " \t\r\n\f")
  (let* ((bounds (ar-bounds-of-word-atpt))
         (beg (car bounds))
         (end (cdr bounds)))
    (goto-char end)
    (insert ")")
    (goto-char beg)
    (insert "eval(")))

(defun pst-here ()
  "Kill previous \"pdb.set_trace()\" and insert it at point. "
  (interactive "*")
  (let ((orig (copy-marker (point))))
    (search-backward "pdb.set_trace()")
    (replace-match "")
    (when (empty-line-p)
      (delete-region (line-beginning-position) (line-end-position)))
    (goto-char orig)
    (insert "pdb.set_trace()")))

(defalias 'druck 'py-printform-insert)
(defun py-printform-insert (&optional arg)
  "Inserts a print statement out of current `(car kill-ring)' by default, inserts ARG instead if delivered. "
  (interactive "*")
  (lexical-let* ((name (string-strip (or arg (car kill-ring))))
                 (form (cond ((eq major-mode 'python-mode)
                              (concat "print \"" name ": %s \" % " name)))))
    (insert form)))

(defun py-line-to-printform-python2 (&optional arg)
  "Transforms the item on current in a print statement. "
  (interactive "*")
  (lexical-let* ((name (thing-at-point 'word))
                 (form (cond ((eq major-mode 'python-mode)
                              (concat "print \"" name ": %s \" % " name)))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert form))
  (forward-line 1)
  (back-to-indentation))

;;; python-components-imenu.el

;; Imenu definitions

;; (defcustom index-function-list (list 'py-imenu-create-index-function 'py-imenu-create-index-new)
;;   " "
;;   :type '(repeat function)
;;   :group 'python)

(defvar py-imenu-class-regexp
  (concat                               ; <<classes>>
   "\\("                                ;
   "^[ \t]*"                            ; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"     ; class name
                                        ; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"                            ; and the final :
   "\\)"                                ; >>classes<<
   )
  "Regexp for Python classes for use with the Imenu package."
  )

(defvar py-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ;
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
                                        ;   function arguments...
   ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Python methods/functions for use with the Imenu package."
  )

(defvar py-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Python regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar py-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Python regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar py-imenu-generic-expression
  (cons
   (concat
    py-imenu-class-regexp
    "\\|"                               ; or...
    py-imenu-method-regexp
    )
   py-imenu-method-no-arg-parens)
  "Generic Python expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[py-imenu-create-index] for a better
alternative for finding the index.")

;; These next two variables are used when searching for the Python
;; class/definitions. Just saving some time in accessing the
;; generic-python-expression, really.
(defvar py-imenu-generic-regexp nil)
(defvar py-imenu-generic-parens nil)

(defcustom py-imenu-show-method-args-p nil
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'python)

(defun py-switch-imenu-index-function ()
  "For development only. Good old renamed `py-imenu-create-index'-function hangs with medium size files already. Working `py-imenu-create-index-new' is active by default.

Switch between classic index machine `py-imenu-create-index'-function and new `py-imenu-create-index-new'.

The former may provide a more detailed report, thus delivering two different index-machines is considered. "
  (interactive)
  (if (eq major-mode 'python-mode)
      (progn
        (if (eq imenu-create-index-function 'py-imenu-create-index-new)
            (setq imenu-create-index-function #'py-imenu-create-index)
          (setq imenu-create-index-function #'py-imenu-create-index-new))
        (when (interactive-p) (message "imenu-create-index-function: %s" (prin1-to-string imenu-create-index-function))))
    (error "%s" "Only available in buffers set to python-mode")))

(defun py-imenu-create-index-function ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
        py-imenu-generic-parens (if py-imenu-show-method-args-p
                                    py-imenu-method-arg-parens
                                  py-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (py-imenu-create-index-engine nil))

(defun py-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Python.

Finds all definitions (classes, methods, or functions) in a Python
file for the Imenu package.

Returns a possibly nested alist of the form

        (INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

        (INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Python classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
        sub-method-alist
        looking-p
        def-name prev-name
        cur-indent def-pos
        (class-paren (first py-imenu-generic-parens))
        (def-paren (second py-imenu-generic-parens)))
    (setq looking-p
          (re-search-forward py-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
        ;; used to set def-name to this value but generic-extract-name
        ;; is new to imenu-1.14. this way it still works with
        ;; imenu-1.11
        ;;(imenu--generic-extract-name py-imenu-generic-parens))
        (let ((cur-paren (if (match-beginning class-paren)
                             class-paren def-paren)))
          (setq def-name
                (buffer-substring-no-properties (match-beginning cur-paren)
                                                (match-end cur-paren))))
        (save-match-data
          (py-beginning-of-def-or-class))
        (beginning-of-line)
        (setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
            (or (match-beginning class-paren)
                (match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
          (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
          (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; Skip code in comments and strings
       ((py-in-literal))
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
        (push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
        ;; the point is currently on the expression we're supposed to
        ;; start on, so go back to the last expression. The recursive
        ;; call will find this place again and add it to the correct
        ;; list
        (re-search-backward py-imenu-generic-regexp (point-min) 'move)
        (setq sub-method-alist (py-imenu-create-index-engine cur-indent))
        (if sub-method-alist
            ;; we put the last element on the index-alist on the start
            ;; of the submethod alist so the user can still get to it.
            (let ((save-elmt (pop index-alist)))
              (push (cons prev-name
                          (cons save-elmt sub-method-alist))
                    index-alist))))
       ;; found less indented expression, we're done.
       (t
        (setq looking-p nil)
        (re-search-backward py-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
           (setq looking-p
                 (re-search-forward py-imenu-generic-regexp
                                    (point-max) 'move))))
    (nreverse index-alist)))

(defvar imenu-max-items)
(defun py-imenu-create-index-new-intern (&optional thisend)
  (let* ((pos (match-beginning 0))
         (name (match-string-no-properties 2))
         (classname (concat "class " name))
         (thisend (or thisend (save-match-data (py-end-of-def-or-class-position))))
         sublist)
    (while (and (re-search-forward "^[ \t]*\\(?:\\(def\\|class\\)\\)[ \t]+\\(?:\\(\\sw+\\)\\)" (or thisend end) t 1)(not (py-in-string-or-comment-p)))
      (let* ((pos (match-beginning 0))
             (name (match-string-no-properties 2))
             (classname (concat "class " name))
             (thisend (or thisend (save-match-data (py-end-of-def-or-class-position)))))
        (if (string= "class" (match-string-no-properties 1))
            (py-imenu-create-index-new-intern (save-match-data (py-end-of-def-or-class-position)))
          (push (cons (concat " " name) pos) sublist))))
    (if classname
        (progn
          (setq sublist (nreverse sublist))
          (push (cons classname pos) sublist)
          (push (cons classname sublist) index-alist))
      (push sublist index-alist))))

(defun py-imenu-create-index-new (&optional beg end)
  "`imenu-create-index-function' for Python. "
  (set (make-local-variable 'imenu-max-items) 99)
  (let ((orig (point))
        (beg (cond (beg)
                   (t (point-min))))
        (end (cond (end)
                   (t (point-max))))
        index-alist vars thisend sublist classname)
    (goto-char beg)
    (while (and (re-search-forward "^[ \t]*\\(?:\\(def\\|class\\)\\)[ \t]+\\(?:\\(\\sw+\\)\\)" end t 1)(not (py-in-string-or-comment-p)))
      (if (save-match-data (string= "class" (match-string-no-properties 1)))
          (py-imenu-create-index-new-intern)
        (let ((pos (match-beginning 0))
              (name (match-string-no-properties 2)))
          (push (cons (concat " " name) pos) sublist))))
    ;; Look for module variables.
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\sw+\\)[ \t]*=" end t)
      (unless (py-in-string-or-comment-p)
        (let ((pos (match-beginning 1))
              (name (match-string-no-properties 1)))
          (push (cons name pos) vars))))
    (setq index-alist (nreverse index-alist))
    (when vars
      (push (cons "Module variables"
                  (nreverse vars))
            index-alist))
    (goto-char orig)
    index-alist))

;;; python-components-completion.el

(defun python-symbol-completions (symbol)
  "Return a list of completions of the string SYMBOL from Python process.
The list is sorted.
Uses `python-imports' to load modules against which to complete."
  (when (stringp symbol)
    (let ((completions
	   (condition-case ()
	       (car (read-from-string
		     (python-send-receive
		      (format "emacs.complete(%S,%s)"
			      (substring-no-properties symbol)
			      python-imports))))
	     (error nil))))
      (sort
       ;; We can get duplicates from the above -- don't know why.
       (delete-dups completions)
       #'string<))))

(defun py-completion-at-point ()
  (interactive "*")
  (let* ((start (when (skip-chars-backward "[[:alnum:]_]")(point)))
         (end (progn (skip-chars-forward "[[:alnum:]_]")(point)))
         (completion (when start
                       (python-symbol-completions (buffer-substring-no-properties start end)))))
    (when completion (delete-region start end)
          (insert (car completion)))))

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")
(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapc #' (lambda (key)
             (define-key py-mode-output-map key
               #'(lambda () (interactive) (beep))))
           (where-is-internal 'self-insert-command))
  )

(setq py-shell-map
      (let ((map (copy-keymap comint-mode-map)))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                     map global-map)
        (define-key map [tab] 'py-shell-complete)
        (define-key map (kbd "RET") 'comint-send-input)
        (define-key map "\C-c-" 'py-up-exception)
        (define-key map "\C-c=" 'py-down-exception)
        map))

(defun py-choose-shell-by-shebang ()
  "Choose shell by looking at #! on the first line.

Returns the specified Python resp. Jython shell command name. "
  (interactive)
  ;; look for an interpreter specified in the first line
  ;; similar to set-auto-mode (files.el)
  (let* (erg
        (interpreter (save-excursion
                       (goto-char (point-min))
                       (when (looking-at py-shebang-regexp)
                         (setq erg (match-string-no-properties 0))
                         (substring erg (string-match "[ijp]+ython" erg))))))
    (when (interactive-p) (message "%s" interpreter))
    interpreter))

(defun py-choose-shell-by-import ()
  "Choose CPython or Jython mode based imports.

If a file imports any packages in `py-jython-packages', within
`py-import-check-point-max' characters from the start of the file,
return `jython', otherwise return nil."
  (let (mode)
    (save-excursion
      (goto-char (point-min))
      (while (and (not mode)
                  (search-forward-regexp
                   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
                   py-import-check-point-max t))
        (setq mode (and (member (match-string 4) py-jython-packages)
                        'jython))))
    mode))

(defun py-which-python ()
  "Returns version of Python of current default environment, a number. "
  (interactive)
  (let* ((cmd (py-choose-shell))
         (erg (shell-command-to-string (concat cmd " --version")))
         (version (when (string-match "\\([0-9]\\.[0-9]+\\)" erg)
                    (substring erg 7 (1- (length erg))))))
    (when (interactive-p)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-default-environment ()
  "Returns path of Python default installation. "
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (interactive-p)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    erg))

(defalias 'python-toggle-shells 'py-toggle-shells)
(defun py-toggle-shells (&optional arg)
  "Toggles between the CPython and Jython default interpreter.

ARG might be a python-version string to set to.
With \\[universal-argument]) user is prompted for the command to use.
If no arg given and py-shell-name not set yet, shell is set according to `py-shell-name' "
  (interactive "P")
  (let ((name (cond ((eq 4 (prefix-numeric-value arg))
                     (read-from-minibuffer "Python Shell and args:"))
                    ((ignore-errors (stringp arg))
                     arg)
                    (t
                     (if (string-match "python" py-shell-name)
                         "jython"
                       "python"))))
        msg)
    (if (string-match "python" name)
        (setq py-shell-name name
              py-which-bufname (capitalize name)
              msg "CPython"
              mode-name (capitalize name))
      (setq py-shell-name name
            py-which-bufname (capitalize name)
            msg "Jython"
            mode-name (capitalize name)))
    (force-mode-line-update)
    (message "Using the %s shell, %s" msg py-shell-name)
    (setq py-output-buffer (format "*%s Output*" py-which-bufname))))

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional arg)
  "Looks for an appropriate mode function.
This does the following:
 - reads py-shell-name
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-shell-name'

With \\[universal-argument]) user is prompted to specify a reachable Python version."
  (interactive "P")
  (let ((erg (cond ((eq 4 (prefix-numeric-value arg))
                    (read-from-minibuffer "Python Shell: " py-shell-name))
                   ((py-choose-shell-by-shebang))
                   ((py-choose-shell-by-import))
                   (t py-shell-name))))
    (when (interactive-p) (message "%s" erg))
    (setq py-shell-name erg)
    erg))

(defvar inferior-python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'py-load-file)
    (define-key map "\C-c\C-v" 'python-check)
    ;; Note that we _can_ still use these commands which send to the
    ;; Python process even at the prompt iff we have a normal prompt,
    ;; i.e. '>>> ' and not '... '.  See the comment before
    ;; py-send-region.  Fixme: uncomment these if we address that.
    map))

(defun py-load-pymacs ()
  "Load Pymacs as delivered with python-mode.el.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (if (or (not (boundp 'py-install-directory)) (not (stringp py-install-directory)))
      (error "`py-install-directory' not set, see INSTALL")
    (load (concat py-install-directory "/pymacs/pymacs.el") nil t)
    (add-to-list 'load-path (concat py-install-directory "/pymacs/pymacs.el"))
    (setenv "PYMACS_PYTHON" "python2.7")
    (autoload 'pymacs-apply "pymacs")
    (autoload 'pymacs-call "pymacs")
    (autoload 'pymacs-eval "pymacs")
    (autoload 'pymacs-exec "pymacs")
    (autoload 'pymacs-load "pymacs")
    (require 'pymacs)))

(defun py-guess-py-install-directory ()
  (interactive)
  (let* ((bufn (buffer-file-name))
         (erg (when (or (string-match "python-mode.el" bufn)(string-match "python-components-mode.el" bufn)) (file-name-directory (buffer-file-name)))))
    (when erg
      (add-to-list 'load-path erg)
      (setq py-install-directory erg)
      (when (interactive-p) (message "%s" erg))
      erg)))

(defun py-set-load-path ()
  "Include needed subdirs of python-mode directory. "
  (interactive)
  (cond (py-install-directory
         (add-to-list 'load-path (expand-file-name py-install-directory))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/completion"))
         (add-to-list 'load-path (concat py-install-directory "/pymacs"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/test"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/tools")))
        ((when py-guess-py-install-directory-p
         (py-guess-py-install-directory)))
        (t (error "Please set `py-install-directory', see INSTALL"))
        (when (interactive-p) (message "%s" load-path))))

(defvar skeleton-further-elements)
(define-derived-mode python-mode fundamental-mode "Python"
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS

VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-shell-name\t\tshell command to invoke Python interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed

\\{python-mode-map}"
  :group 'python
  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords nil nil nil nil
				   (font-lock-syntactic-keywords
				    . python-font-lock-syntactic-keywords)
				   ;; This probably isn't worth it.
				   ;; (font-lock-syntactic-face-function
				   ;;  . python-font-lock-syntactic-face-function)
                                   ))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (add-to-list 'hs-special-modes-alist
               (list
                'python-mode
                ;; start regex
                (concat (if py-hide-show-hide-docstrings
                            "^\\s-*\"\"\"\\|" "")
                        (mapconcat 'identity
                                   (mapcar #'(lambda (x) (concat "^\\s-*" x "\\>"))
                                           py-hide-show-keywords)
                                   "\\|"))
                ;; end regex
                nil
                ;; comment-start regex
                "#"
                ;; forward-sexp function
                (lambda (arg)
                  (py-goto-beyond-block)
                  (skip-chars-backward " \t\n"))
                nil))
  ;; (set (make-local-variable 'outline-regexp)
  ;;      (rx (* space) (or "class" "def" "elif" "else" "except" "finally"
  ;;       		 "for" "if" "try" "while" "with")
  ;;          symbol-end))
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'outline-level) #'python-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (if py-hide-show-hide-docstrings
                   "^\\s-*\"\"\"\\|" "")
               (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\>"))
                                  py-hide-show-keywords)
                          "\\|")))
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)

  ;; (set (make-local-variable 'indent-line-function) #'python-indent-line)
  ;; (set (make-local-variable 'indent-region-function) #'python-indent-region)

  ;; (set (make-local-variable 'fill-paragraph-function) 'python-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (make-local-variable 'python-saved-check-command)
  ;; (set (make-local-variable 'beginning-of-defun-function)
  ;; 'python-beginning-of-defun)
  (set (make-local-variable 'beginning-of-defun-function)
       'py-beginning-of-def-or-class)
  ;; (set (make-local-variable 'end-of-defun-function) 'python-end-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
  (add-hook 'which-func-functions 'python-which-func nil t)
  (when (and imenu-create-index-p (ignore-errors (require 'imenu)))
    (setq imenu-create-index-function #'py-imenu-create-index-new)
    ;;    (setq imenu-create-index-function #'py-imenu-create-index)
    (setq imenu-generic-expression py-imenu-generic-expression)
    (when (fboundp 'imenu-add-to-menubar)
      (imenu-add-to-menubar (format "%s-%s" "IM" mode-name))
      (remove-hook 'imenu-add-menubar-index 'python-mode-hook)))
  (set (make-local-variable 'eldoc-documentation-function)
       #'python-eldoc-function)
  (add-hook 'eldoc-mode-hook
            (lambda () (run-python nil t)) ; need it running
            nil t)
  ;; (add-hook 'completion-at-point-functions
  ;; 'python-completion-at-point nil 'local)
  (add-hook 'completion-at-point-functions
            py-complete-function nil 'local)
  ;; Python defines TABs as being 8-char wide.
  (set (make-local-variable 'tab-width) 8)
  ;; Now do the automagical guessing
  (when py-smart-indentation
    (if (bobp)
        (save-excursion
          (save-restriction
            (widen)
            ;; (switch-to-buffer (current-buffer))
            (while (and (not (eobp))
                        (or
                         (let ((erg (syntax-ppss)))
                           (or (nth 1 erg) (nth 8 erg)))
                         (eq 0 (current-indentation))))
              (forward-line 1))
            (back-to-indentation)
            (py-guess-indent-offset)))
      (py-guess-indent-offset)))
  (when (/= tab-width py-indent-offset)
    (setq indent-tabs-mode nil))
  ;; Set the default shell if not already set
  (when (null py-shell-name)
    (py-toggle-shells (py-choose-shell)))
  ;; (py-set-load-path)
  (when py-load-pymacs-p (py-load-pymacs)
        (find-file (concat py-install-directory "/completion/pycomplete.el"))
        (eval-buffer)
        (kill-buffer "pycomplete.el"))
  (define-key inferior-python-mode-map (kbd "<tab>")
    'python-shell-completion-complete-or-indent)
  ;; add the menu
  (if py-menu
      (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  ;; shell-complete end
  ;; Run the mode hook.  Note that py-mode-hook is deprecated.
  (run-mode-hooks
   (if python-mode-hook
       'python-mode-hook
     'py-mode-hook))
  (when py-start-run-py-shell
    ;; py-shell may split window, provide restore
    (window-configuration-to-register 213465879)
    (unless (get-process (py-process-name))
      (let ((oldbuf (current-buffer)))
        (save-excursion
          (py-shell)
          (set-buffer oldbuf))))
    (jump-to-register 213465879))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (interactive-p) (message "python-mode loaded from: %s" "python-mode.el")))

(defadvice pdb (before gud-query-cmdline activate)
   "Provide a better default command line when called interactively."
   (interactive
    (list (gud-query-cmdline pdb-path
        (file-name-nondirectory buffer-file-name)))))

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

(define-derived-mode python2-mode python-mode "Python2"
  "Edit and run code used by Python version 2 series. "
  :group 'Python
  :abbrev nil
  (set (make-local-variable 'py-exec-command) '(format "execfile(r'%s') # PYTHON-MODE\n" filename))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'%s') # PYTHON-MODE\n" string))
  (py-toggle-shells "python2"))

(define-derived-mode python3-mode python-mode "Python3"
  "Edit and run code used by Python version 3 series. "
  :group 'Python
  :abbrev nil
  (set (make-local-variable 'py-exec-command) '(format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'(%s)') # PYTHON-MODE\n" string))
  (py-toggle-shells "python3"))

;; Utilities

(defun py-def-or-class-beginning-position ()
  "Returns beginning position of function or class definition. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-beginning-of-def-or-class 'either)(point))))
    (prog1
        (point)
      (when (interactive-p) (message "%s" pos))
      (goto-char here))))

(defun py-def-or-class-end-position ()
  "Returns end position of function or class definition. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-end-of-def-or-class 'either) (point))))
    (prog1
        (point)
      (when (interactive-p) (message "%s" pos))
      (goto-char here))))

(defun py-statement-beginning-position ()
  "Returns beginning position of statement. "
  (interactive)
  (let ((here (point))
        (pos (progn (py-beginning-of-statement)(point))))
    (prog1
        (point)
      (when (interactive-p) (message "%s" pos))
      (goto-char here))))

(defun py-statement-end-position ()
  "Returns end position of statement. "
  (interactive)
  (let (erg)
    (save-excursion
      (setq erg (py-end-of-statement)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-current-indentation ()
  "Returns beginning position of code in line. "
  (interactive)
  (let ((here (point))
        (pos (progn (back-to-indentation)(point))))
    (prog1
        (point)
      (when (interactive-p) (message "%s" pos))
      (goto-char here))))

(make-obsolete 'jpython-mode 'jython-mode nil)
(define-derived-mode jython-mode python-mode  "Jython"
  "Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'."
  :group 'python
  (py-toggle-shells "jython"))

;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.

(let ((modes '(("jython" . jython-mode)
               ("python" . python-mode)
               ("python3" . python-mode)
               )))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist)
               (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . python-mode) auto-mode-alist))

(defun py-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py-kill-emacs-hook)
(add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

(add-hook 'python-mode-hook '(lambda ()(set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)))

(add-hook 'python-mode-hook '(lambda ()(set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)))

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

(defvar py-python-command py-shell-name)
(defvar py-jpython-command py-shell-name)
(defvar py-jython-command py-shell-name)
(defvar py-default-interpreter py-shell-name)
(defvar python-command py-shell-name)

;;;; Utility stuff

(defcustom python-default-interpreter 'cpython
  "*Which Python interpreter is used by default.
The value for this variable can be either `cpython' or `jpython'.

When the value is `cpython', the variables `python-python-command' and
`python-python-command-args' are consulted to determine the interpreter
and arguments to use.

When the value is `jpython', the variables `python-jpython-command' and
`python-jpython-command-args' are consulted to determine the interpreter
and arguments to use.

Note that this variable is consulted only the first time that a Python
mode buffer is visited during an Emacs session.  After that, use
\\[py-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Python (a.k.a. CPython)" cpython)
		 (const :tag "JPython" jpython))
  :group 'python)

(defcustom python-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python)

(defcustom python-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python
  :tag "JPython Command Args")

;; for toggling between CPython and JPython
(defvar python-which-shell nil)
(defvar python-which-args  python-python-command-args)
(defvar python-which-bufname "Python")
(make-variable-buffer-local 'python-which-shell)
(make-variable-buffer-local 'python-which-args)
(make-variable-buffer-local 'python-which-bufname)

(defcustom python-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.

When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell interaction buffers and the *Python* buffer.

When using pdb to debug a Python program, pdbtrack notices the
pdb prompt and presents the line in the source file where the
program is stopped in a pop-up buffer.  It's similar to what
gud-mode does for debugging C programs with gdb, but without
having to restart the program."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'python-pdbtrack-do-tracking-p)

(defcustom python-pdbtrack-minor-mode-string " PDB"
  "*Minor-mode sign to be displayed when pdbtrack is active."
  :type 'string
  :group 'python)

;; Add a designator to the minor mode strings
(or (assq 'python-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(python-pdbtrack-is-tracking-p python-pdbtrack-minor-mode-string)
	  minor-mode-alist))

;; Bind python-file-queue before installing the kill-emacs-hook.
(defvar python-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defcustom python-shell-prompt-alist
  '(("ipython" . "^In \\[[0-9]+\\]: *")
    (t . "^>>> "))
  "Alist of Python input prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `python-python-command' for the python process and
REGEXP is a regular expression matching the Python prompt.
PROGRAM can also be t, which specifies the default when no other
element matches `python-python-command'."
  :type 'string
  :group 'python
  :version "24.1")

(defcustom python-shell-continuation-prompt-alist
  '(("ipython" . "^   [.][.][.]+: *")
    (t . "^[.][.][.] "))
  "Alist of Python continued-line prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `python-python-command' for the python process and
REGEXP is a regular expression matching the Python prompt for
continued lines.
PROGRAM can also be t, which specifies the default when no other
element matches `python-python-command'."
  :type 'string
  :group 'python
  :version "24.1")

(defvar python-pdbtrack-is-tracking-p nil)

(defconst python-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst python-pdbtrack-input-prompt "\n[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression pdbtrack uses to recognize a pdb prompt.")

(defconst python-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

;;;; Inferior mode stuff (following cmuscheme).

(defcustom python-python-command "python"
  "Shell command to run Python interpreter.
Any arguments can't contain whitespace."
  :group 'python
  :type 'string)

(defcustom python-jython-command "jython"
  "Shell command to run Jython interpreter.
Any arguments can't contain whitespace."
  :group 'python
  :type 'string)

(defvar python-command python-python-command
  "Actual command used to run Python.
May be `python-python-command' or `python-jython-command', possibly
modified by the user.  Additional arguments are added when the command
is used by `run-python' et al.")

(defvar python-buffer nil
  "*The current Python process buffer.

Commands that send text from source buffers to Python processes have
to choose a process to send to.  This is determined by buffer-local
value of `python-buffer'.  If its value in the current buffer,
i.e. both any local value and the default one, is nil, `run-python'
and commands that send to the Python process will start a new process.

Whenever \\[run-python] starts a new process, it resets the default
value of `python-buffer' to be the new process's buffer and sets the
buffer-local value similarly if the current buffer is in Python mode
or Inferior Python mode, so that source buffer stays associated with a
specific sub-process.

Use \\[py-set-proc] to set the default value from a buffer with a
local value.")
(make-variable-buffer-local 'python-buffer)

(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx line-start (1+ (any " \t")) "File \""
	  (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	  "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
	  (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
	  "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defvar inferior-python-mode-syntax-table
  (let ((st (make-syntax-table py-mode-syntax-table)))
    ;; Don't get confused by apostrophes in the process's output (e.g. if
    ;; you execute "help(os)").
    (modify-syntax-entry ?\' "." st)
    ;; Maybe we should do the same for double quotes?
    ;; (modify-syntax-entry ?\" "." st)
    st))

;; Autoloaded.
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

(defvar python--prompt-regexp nil)

(defun python--set-prompt-regexp ()
  (let ((prompt  (cdr-safe (or (assoc python-python-command
				      python-shell-prompt-alist)
			       (assq t python-shell-prompt-alist))))
	(cprompt (cdr-safe (or (assoc python-python-command
				      python-shell-continuation-prompt-alist)
			       (assq t python-shell-continuation-prompt-alist)))))
    (set (make-local-variable 'comint-prompt-regexp)
	 (concat "\\("
		 (mapconcat 'identity
			    (delq nil (list prompt cprompt "^([Pp]db) "))
			    "\\|")
		 "\\)"))
    (set (make-local-variable 'python--prompt-regexp) prompt)))

;; Fixme: This should inherit some stuff from `python-mode', but I'm
;; not sure how much: at least some keybindings, like C-c C-f;
;; syntax?; font-locking, e.g. for triple-quoted strings?
(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for interacting with an inferior Python process.
A Python process can be started with \\[run-python].

Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
that order.

You can send text to the inferior Python process from other buffers
containing Python source.
 * \\[py-switch-to-python] switches the current buffer to the Python
    process buffer.
 * \\[py-send-region] sends the current region to the Python process.
 * \\[py-send-region-and-go] switches to the Python process buffer
    after sending the text.
For running multiple processes in multiple buffers, see `run-python' and
`python-buffer'.

\\{inferior-python-mode-map}"
  :group 'python
  (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'python-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'python-preoutput-filter
	    nil t)
  (python--set-prompt-regexp)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-shell-minor-mode 1))

(defcustom inferior-python-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python)

(defcustom python-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python
  :version "23.3")

(defun python-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `inferior-python-filter-regexp'."
  (not (string-match inferior-python-filter-regexp str)))

;; Fixme: Loses with quoted whitespace.
(defun python-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (python-args-to-list (substring string (+ 1 where)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if pos (python-args-to-list (substring string pos))))))))

(defvar python-preoutput-continuation nil
  "If non-nil, funcall this when `python-preoutput-filter' sees `_emacs_ok'.")

(defvar python-preoutput-leftover nil)
(defvar python-preoutput-skip-next-prompt nil)

;; Using this stops us getting lines in the buffer like
;; >>> ... ... >>>
;; Also look for (and delete) an `_emacs_ok' string and call
;; `python-preoutput-continuation' if we get it.
(defun python-preoutput-filter (s)
  "`comint-preoutput-filter-functions' function: ignore prompts not at bol."
  (when python-preoutput-leftover
    (setq s (concat python-preoutput-leftover s))
    (setq python-preoutput-leftover nil))
  (let ((start 0)
        (res ""))
    ;; First process whole lines.
    (while (string-match "\n" s start)
      (let ((line (substring s start (setq start (match-end 0)))))
        ;; Skip prompt if needed.
        (when (and python-preoutput-skip-next-prompt
                   (string-match comint-prompt-regexp line))
          (setq python-preoutput-skip-next-prompt nil)
          (setq line (substring line (match-end 0))))
        ;; Recognize special _emacs_out lines.
        (if (and (string-match "\\`_emacs_out \\(.*\\)\n\\'" line)
                 (local-variable-p 'python-preoutput-result))
            (progn
              (setq python-preoutput-result (match-string 1 line))
              (set (make-local-variable 'python-preoutput-skip-next-prompt) t))
          (setq res (concat res line)))))
    ;; Then process the remaining partial line.
    (unless (zerop start) (setq s (substring s start)))
    (cond ((and (string-match comint-prompt-regexp s)
                ;; Drop this prompt if it follows an _emacs_out...
                (or python-preoutput-skip-next-prompt
                    ;; ... or if it's not gonna be inserted at BOL.
                    ;; Maybe we could be more selective here.
                    (if (zerop (length res))
                        (not (bolp))
                      (string-match ".\\'" res))))
           ;; The need for this seems to be system-dependent:
           ;; What is this all about, exactly?  --Stef
           ;; (if (and (eq ?. (aref s 0)))
           ;;     (accept-process-output (get-buffer-process (current-buffer)) 1))
           (setq python-preoutput-skip-next-prompt nil)
           res)
          ((let ((end (min (length "_emacs_out ") (length s))))
             (eq t (compare-strings s nil end "_emacs_out " nil end)))
           ;; The leftover string is a prefix of _emacs_out so we don't know
           ;; yet whether it's an _emacs_out or something else: wait until we
           ;; get more output so we can resolve this ambiguity.
           (set (make-local-variable 'python-preoutput-leftover) s)
           res)
          (t (concat res s)))))

(defvar python-version-checked nil)
(defun python-check-version (cmd)
  "Check that CMD runs a suitable version of Python."
  ;; Fixme:  Check on Jython.
  (unless (or python-version-checked
	      (equal 0 (string-match (regexp-quote python-python-command)
				     cmd)))
    (unless (shell-command-to-string cmd)
      (error "Can't run Python command `%s'" cmd))
    (let* ((res (shell-command-to-string
                 (concat cmd
                         " -c \"from sys import version_info;\
print version_info >= (2, 2) and version_info < (3, 0)\""))))
      (unless (string-match "True" res)
	(error "Only Python versions >= 2.2 and < 3.0 are supported")))
    (setq python-version-checked t)))

;;; Py-send stuff liftet from python.el, alternates py-execute-...
(defun run-python (&optional cmd noshow new)
  "Run an inferior Python process, input and output via buffer *Python*.

CMD is the Python command to run.  NOSHOW non-nil means don't
show the buffer automatically.

Interactively, a prefix arg means to prompt for the initial
Python command line (default is `python-command').

A new process is started if one isn't running attached to
`python-buffer', or if called from Lisp with non-nil arg NEW.
Otherwise, if a process is already running in `python-buffer',
switch to that buffer.

This command runs the hook `inferior-python-mode-hook' after
running `comint-mode-hook'.  Type \\[describe-mode] in the
process buffer for a list of commands.

By default, Emacs inhibits the loading of Python modules from the
current working directory, for security reasons.  To disable this
behavior, change `python-remove-cwd-from-path' to nil."
  (interactive (if current-prefix-arg
		   (list (read-string "Run Python: " python-command) nil t)
		 (list python-command)))
  (require 'ansi-color) ; for ipython
  (unless cmd (setq cmd python-command))
  (python-check-version cmd)
  (setq python-command cmd)
  ;; Fixme: Consider making `python-buffer' buffer-local as a buffer
  ;; (not a name) in Python buffers from which `run-python' &c is
  ;; invoked.  Would support multiple processes better.
  (when (or new (not (comint-check-proc python-buffer)))
    (with-current-buffer
	(let* ((cmdlist
		(append (python-args-to-list cmd) '("-i")
			(if python-remove-cwd-from-path
			    '("-c" "import sys; sys.path.remove('')"))))
	       (path (getenv "PYTHONPATH"))
	       (process-environment	; to import emacs.py
		(cons (concat "PYTHONPATH="
			      (if path (concat path path-separator))
			      data-directory)
		      process-environment))
               ;; If we use a pipe, unicode characters are not printed
               ;; correctly (Bug#5794) and IPython does not work at
               ;; all (Bug#5390).
	       (process-connection-type t))
	  (apply 'make-comint-in-buffer "Python"
		 (generate-new-buffer "*Python*")
		 (car cmdlist) nil (cdr cmdlist)))
      (setq-default python-buffer (current-buffer))
      (setq python-buffer (current-buffer))
      (accept-process-output (get-buffer-process python-buffer) 5)
      (inferior-python-mode)
      ;; Load function definitions we need.
      ;; Before the preoutput function was used, this was done via -c in
      ;; cmdlist, but that loses the banner and doesn't run the startup
      ;; file.  The code might be inline here, but there's enough that it
      ;; seems worth putting in a separate file, and it's probably cleaner
      ;; to put it in a module.
      ;; Ensure we're at a prompt before doing anything else.
      (py-send-string "import emacs")
      ;; The following line was meant to ensure that we're at a prompt
      ;; before doing anything else.  However, this can cause Emacs to
      ;; hang waiting for a response, if that Python function fails
      ;; (i.e. raises an exception).
      ;; (python-send-receive "print '_emacs_out ()'")
      ))
  (if (derived-mode-p 'python-mode)
      (setq python-buffer (default-value 'python-buffer))) ; buffer-local
  ;; Without this, help output goes into the inferior python buffer if
  ;; the process isn't already running.
  (sit-for 1 t)        ;Should we use accept-process-output instead?  --Stef
  (unless noshow (pop-to-buffer python-buffer t)))

(defun python-send-command (command)
  "Like `py-send-string' but resets `compilation-shell-minor-mode'."
  (when (python-check-comint-prompt)
    (with-current-buffer (process-buffer (python-proc))
      (goto-char (point-max))
      (compilation-forget-errors)
      (py-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

(defun py-send-region (start end)
  "Send the region to the inferior Python process."
  ;; The region is evaluated from a temporary file.  This avoids
  ;; problems with blank lines, which have different semantics
  ;; interactively and in files.  It also saves the inferior process
  ;; buffer filling up with interpreter prompts.  We need a Python
  ;; function to remove the temporary file when it has been evaluated
  ;; (though we could probably do it in Lisp with a Comint output
  ;; filter).  This function also catches exceptions and truncates
  ;; tracebacks not to mention the frame of the function itself.
  ;;
  ;; The `compilation-shell-minor-mode' parsing takes care of relating
  ;; the reference to the temporary file to the source.
  ;;
  ;; Fixme: Write a `coding' header to the temp file if the region is
  ;; non-ASCII.
  (interactive "r")
  (let* ((f (make-temp-file "py"))
	 (command
          ;; IPython puts the FakeModule module into __main__ so
          ;; emacs.eexecfile becomes useless.
          (if (string-match "^ipython" python-command)
              (format "execfile %S" f)
            (format "emacs.eexecfile(%S)" f)))
	 (orig-start (copy-marker start)))
    (when (save-excursion
	    (goto-char start)
	    (/= 0 (current-indentation))) ; need dummy block
      (save-excursion
	(goto-char orig-start)
	;; Wrong if we had indented code at buffer start.
	(set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f nil 'nomsg))
    (write-region start end f t 'nomsg)
    (python-send-command command)
    (with-current-buffer (process-buffer (python-proc))
      ;; Tell compile.el to redirect error locations in file `f' to
      ;; positions past marker `orig-start'.  It has to be done *after*
      ;; `python-send-command''s call to `compilation-forget-errors'.
      (compilation-fake-loc orig-start f))))

(defun py-send-buffer ()
  "Send the current buffer to the inferior Python process."
  (interactive)
  (py-send-region (point-min) (point-max)))

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.

With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (python-proc)) t) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun py-send-region-and-go (start end)
  "Send the region to the inferior Python process.

Then switch to the process buffer."
  (interactive "r")
  (py-send-region start end)
  (py-switch-to-python t))

(defcustom python-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.

If a file is loaded into a buffer that is in one of these major modes, it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python)

(defvar python-prev-dir/file nil
  "Caches (directory . file) pair used in the last `py-load-file' command.
Used for determining the default in the next one.")

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the inferior Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive (comint-get-source "Load Python file: " python-prev-dir/file
				  python-source-modes
				  t))	; because execfile needs exact name
  (comint-check-source file-name)     ; Check to see if buffer needs saving.
  (setq python-prev-dir/file (cons (file-name-directory file-name)
				   (file-name-nondirectory file-name)))
  (with-current-buffer (process-buffer (python-proc)) ;Runs python if needed.
    ;; Fixme: I'm not convinced by this logic from python-mode.el.
    (python-send-command
     (if (string-match "\\.py\\'" file-name)
	 (let ((module (file-name-sans-extension
			(file-name-nondirectory file-name))))
	   (format "emacs.eimport(%S,%S)"
		   module (file-name-directory file-name)))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

(defun python-proc ()
  "Return the current Python process.

See variable `python-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `python-buffer'.
  (unless (comint-check-proc python-buffer)
    (run-python nil t))
  (get-buffer-process (if (derived-mode-p 'inferior-python-mode)
			  (current-buffer)
			python-buffer)))

(defun py-set-proc ()
  "Set the default value of `python-buffer' to correspond to this buffer.

If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from \\[py-send-region] et al when used
in a buffer that doesn't have a local value of `python-buffer'."
  (interactive)
  (if (local-variable-p 'python-buffer)
      (setq-default python-buffer python-buffer)
    (error "No local value of `python-buffer'")))

;;; Python-el completion and help

(defvar view-return-to-alist)
(defvar python-imports)			; forward declaration

(defun python-send-string (string)
  "Evaluate STRING in inferior Python process."
  (interactive "sPython command: ")
  (comint-send-string (python-proc) string)
  (unless (string-match "\n\\'" string)
    ;; Make sure the text is properly LF-terminated.
    (comint-send-string (python-proc) "\n"))
  (when (string-match "\n[ \t].*\n?\\'" string)
    ;; If the string contains a final indented line, add a second newline so
    ;; as to make sure we terminate the multiline instruction.
    (comint-send-string (python-proc) "\n")))

(defun python-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.

The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (python-send-string string)
  (let ((proc (python-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
	(while (progn
		 (accept-process-output proc 5)
		 (null python-preoutput-result)))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

(defun python-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.

If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (with-current-buffer (process-buffer (or proc (python-proc)))
    (save-excursion
      (save-match-data
	(re-search-backward (concat python--prompt-regexp " *\\=")
			    nil t)))))

;; Fixme:  Is there anything reasonable we can do with random methods?
;; (Currently only works with functions.)
(defun python-eldoc-function ()
  "`eldoc-documentation-function' for Python.

Only works when point is in a function name, not its arg list, for
instance.  Assumes an inferior Python is running."
  (let ((symbol (with-syntax-table python-dotty-syntax-table
		  (current-word))))
    ;; This is run from timers, so inhibit-quit tends to be set.
    (with-local-quit
      ;; First try the symbol we're on.
      (or (and symbol
	       (python-send-receive (format "emacs.eargs(%S, %s)"
					    symbol python-imports)))
	  ;; Try moving to symbol before enclosing parens.
	  (let ((s (syntax-ppss)))
	    (unless (zerop (car s))
	      (when (eq ?\( (char-after (nth 1 s)))
		(save-excursion
		  (goto-char (nth 1 s))
		  (skip-syntax-backward "-")
		  (let ((point (point)))
		    (skip-chars-backward "a-zA-Z._")
		    (if (< (point) point)
			(python-send-receive
			 (format "emacs.eargs(%S, %s)"
				 (buffer-substring-no-properties (point) point)
				 python-imports))))))))))))

;;; Info-look functionality.

(declare-function info-lookup-maybe-add-help "info-look" (&rest arg))

(defun python-after-info-look ()
  "Set up info-look for Python.

Used with `eval-after-load'."
  (let* ((version (let ((s (shell-command-to-string (concat python-command
							    " -V"))))
		    (string-match "^Python \\([0-9]+\\.[0-9]+\\>\\)" s)
		    (match-string 1 s)))
	 ;; Whether info files have a Python version suffix, e.g. in Debian.
	 (versioned
	  (with-temp-buffer
	    (with-no-warnings (Info-mode))
	    (condition-case ()
		;; Don't use `info' because it would pop-up a *info* buffer.
		(with-no-warnings
		  (Info-goto-node (format "(python%s-lib)Miscellaneous Index"
					  version))
		  t)
	      (error nil)))))
    (info-lookup-maybe-add-help
     :mode 'python-mode
     :regexp "[[:alnum:]_]+"
     :doc-spec
     ;; Fixme: Can this reasonably be made specific to indices with
     ;; different rules?  Is the order of indices optimal?
     ;; (Miscellaneous in -ref first prefers lookup of keywords, for
     ;; instance.)
     (if versioned
	 ;; The empty prefix just gets us highlighted terms.
	 `((,(concat "(python" version "-ref)Miscellaneous Index") nil "")
	   (,(concat "(python" version "-ref)Module Index" nil ""))
	   (,(concat "(python" version "-ref)Function-Method-Variable Index"
		     nil ""))
	   (,(concat "(python" version "-ref)Class-Exception-Object Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Module Index" nil ""))
	   (,(concat "(python" version "-lib)Class-Exception-Object Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Function-Method-Variable Index"
		     nil ""))
	   (,(concat "(python" version "-lib)Miscellaneous Index" nil "")))
       '(("(python-ref)Miscellaneous Index" nil "")
	 ("(python-ref)Module Index" nil "")
	 ("(python-ref)Function-Method-Variable Index" nil "")
	 ("(python-ref)Class-Exception-Object Index" nil "")
	 ("(python-lib)Module Index" nil "")
	 ("(python-lib)Class-Exception-Object Index" nil "")
	 ("(python-lib)Function-Method-Variable Index" nil "")
	 ("(python-lib)Miscellaneous Index" nil ""))))))
(eval-after-load "info-look" '(python-after-info-look))

;;; Miscellany.

(defcustom python-jython-packages '("java" "javax" "org" "com")
  "Packages implying `jython-mode'.
If these are imported near the beginning of the buffer, `python-mode'
actually punts to `jython-mode'."
  :type '(repeat string)
  :group 'python)

;; Called from `python-mode', this causes a recursive call of the
;; mode.  See logic there to break out of the recursion.
(defun python-maybe-jython ()
  "Invoke `jython-mode' if the buffer appears to contain Jython code.
The criterion is either a match for `jython-mode' via
`interpreter-mode-alist' or an import of a module from the list
`python-jython-packages'."
  ;; The logic is taken from python-mode.el.
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((interpreter (if (looking-at auto-mode-interpreter-regexp)
			     (match-string 2))))
	(if (and interpreter (eq 'jython-mode
				 (cdr (assoc (file-name-nondirectory
					      interpreter)
					     interpreter-mode-alist))))
	    (jython-mode)
	  (if (catch 'done
		(while (re-search-forward
			(rx line-start (or "import" "from") (1+ space)
			    (group (1+ (not (any " \t\n.")))))
			(+ (point-min) 10000) ; Probably not worth customizing.
			t)
		  (if (member (match-string 1) python-jython-packages)
		      (throw 'done t))))
	      (jython-mode)))))))

;;; Modes

;; pdb tracking is alert once this file is loaded, but takes no action if
;; `python-pdbtrack-do-tracking-p' is nil.
(add-hook 'comint-output-filter-functions 'python-pdbtrack-track-stack-file)

(defvar outline-heading-end-regexp)
(defvar eldoc-documentation-function)
(defvar python-mode-running)            ;Dynamically scoped var.



(defun python-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;; TBD: this should probably use split-string
  (when (and (string-match python--prompt-regexp string)
	     python-file-queue)
    (condition-case nil
        (delete-file (car python-file-queue))
      (error nil))
    (setq python-file-queue (cdr python-file-queue))
    (if python-file-queue
	(let ((pyproc (get-buffer-process (current-buffer))))
	  (python-execute-file pyproc (car python-file-queue))))))

(defun python-pdbtrack-overlay-arrow (activation)
  "Activate or deactivate arrow at beginning-of-line in current buffer."
  (if activation
      (progn
        (setq overlay-arrow-position (make-marker)
              overlay-arrow-string "=>"
              python-pdbtrack-is-tracking-p t)
        (set-marker overlay-arrow-position
                    (save-excursion (beginning-of-line) (point))
                    (current-buffer)))
    (setq overlay-arrow-position nil
          python-pdbtrack-is-tracking-p nil)))

(defun python-pdbtrack-track-stack-file (text)
  "Show the file indicated by the pdb stack entry line, in a separate window.

Activity is disabled if the buffer-local variable
`python-pdbtrack-do-tracking-p' is nil.

We depend on the pdb input prompt being a match for
`python-pdbtrack-input-prompt'.

If the traceback target file path is invalid, we look for the
most recently visited python-mode buffer which either has the
name of the current function or class, or which defines the
function or class.  This is to provide for scripts not in the
local filesytem (e.g., Zope's 'Script \(Python)', but it's not
Zope specific).  If you put a copy of the script in a buffer
named for the script and activate python-mode, then pdbtrack will
find it."
  ;; Instead of trying to piece things together from partial text
  ;; (which can be almost useless depending on Emacs version), we
  ;; monitor to the point where we have the next pdb prompt, and then
  ;; check all text from comint-last-input-end to process-mark.
  ;;
  ;; Also, we're very conservative about clearing the overlay arrow,
  ;; to minimize residue.  This means, for instance, that executing
  ;; other pdb commands wipe out the highlight.  You can always do a
  ;; 'where' (aka 'w') PDB command to reveal the overlay arrow.

  (let* ((origbuf (current-buffer))
	 (currproc (get-buffer-process origbuf)))

    (if (not (and currproc python-pdbtrack-do-tracking-p))
        (python-pdbtrack-overlay-arrow nil)

      (let* ((procmark (process-mark currproc))
             (block (buffer-substring (max comint-last-input-end
                                           (- procmark
                                              python-pdbtrack-track-range))
                                      procmark))
             target target_fname target_lineno target_buffer)

        (if (not (string-match (concat python-pdbtrack-input-prompt "$") block))
            (python-pdbtrack-overlay-arrow nil)

          (setq target (python-pdbtrack-get-source-buffer block))

          (if (stringp target)
              (progn
                (python-pdbtrack-overlay-arrow nil)
                (message "pdbtrack: %s" target))

            (setq target_lineno (car target)
                  target_buffer (cadr target)
                  target_fname (buffer-file-name target_buffer))
            (switch-to-buffer-other-window target_buffer)
            (goto-char (point-min))
            (forward-line (1- target_lineno))
            (message "pdbtrack: line %s, file %s" target_lineno target_fname)
            (python-pdbtrack-overlay-arrow t)
            (pop-to-buffer origbuf t)
            ;; in large shell buffers, above stuff may cause point to lag output
            (goto-char procmark)
            )))))
  )

(defun python-pdbtrack-get-source-buffer (block)
  "Return line number and buffer of code indicated by block's traceback text.

We look first to visit the file indicated in the trace.

Failing that, we look for the most recently visited python-mode buffer
with the same name or having the named function.

If we're unable find the source code we return a string describing the
problem."

  (if (not (string-match python-pdbtrack-stack-entry-regexp block))

      "Traceback cue not found"

    (let* ((filename (match-string 1 block))
           (lineno (string-to-number (match-string 2 block)))
           (funcname (match-string 3 block))
           funcbuffer)

      (cond ((file-exists-p filename)
             (list lineno (find-file-noselect filename)))

            ((setq funcbuffer (python-pdbtrack-grub-for-buffer funcname lineno))
             (if (string-match "/Script (Python)$" filename)
                 ;; Add in number of lines for leading '##' comments:
                 (setq lineno
                       (+ lineno
                          (with-current-buffer funcbuffer
                            (if (equal (point-min)(point-max))
                                0
                              (count-lines
                               (point-min)
                               (max (point-min)
                                    (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
                                                  (buffer-substring
                                                   (point-min) (point-max)))
                                    )))))))
               (list lineno funcbuffer))

            ((= (elt filename 0) ?\<)
             (format "(Non-file source: '%s')" filename))

            (t (format "Not found: %s(), %s" funcname filename)))
      )
    )
  )

(defun python-pdbtrack-grub-for-buffer (funcname lineno)
  "Find recent python-mode buffer named, or having function named funcname."
  (let ((buffers (buffer-list))
        buf
        got)
    (while (and buffers (not got))
      (setq buf (car buffers)
            buffers (cdr buffers))
      (if (and (with-current-buffer buf
                 (string= major-mode "python-mode"))
               (or (string-match funcname (buffer-name buf))
                   (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
                                         funcname "\\s-*(")
                                 (with-current-buffer buf
                                   (buffer-substring (point-min)
                                                     (point-max))))))
          (setq got buf)))
    got))

;; Python subprocess utilities and filters
(defun python-execute-file (proc filename)
  "Send to Python interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
	(procbuf (process-buffer proc))
;	(comint-scroll-to-bottom-on-output t)
	(msg (format "## working on region in file %s...\n" filename))
        ;; add some comment, so that we can filter it out of history
	(cmd (format "execfile(r'%s') # PYTHON-MODE\n" filename)))
    (unwind-protect
	(with-current-buffer procbuf
	  (goto-char (point-max))
	  (move-marker (process-mark proc) (point))
	  (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

;; from pycomplete.el
(defun py-find-global-imports ()
  (save-excursion
    (let (first-class-or-def imports)
      (goto-char (point-min))
      (setq first-class-or-def
	    (re-search-forward "^ *\\(def\\|class\\) " nil t))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(import \\|from \\([A-Za-z_][A-Za-z_0-9]*\\) import \\).*"
	      nil t)
	(setq imports (append imports
			      (list (buffer-substring
				     (match-beginning 0)
				     (match-end 0))))))
      imports)))

;;; Code Completion.

;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defvar python-imports "None"
  "String of top-level import statements updated by `py-find-imports'.")
(make-variable-buffer-local 'python-imports)

;;; Python Shell Complete
;; Author: Lukasz Pankowski

(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;; need to clear py-shell-input-lines if primary prompt found
;; (defun py-comint-output-filter-function (string)
;;   "Watch output for Python prompt and exec next file waiting in queue.
;; This function is appropriate for `comint-output-filter-functions'."
;;   ;; TBD: this should probably use split-string
;;   (when (and (or (string-equal string ">>> ")
;; 		 (and (>= (length string) 5)
;; 		      (string-equal (substring string -5) "\n>>> ")))
;; 	     (or (setq py-shell-input-lines nil)
;; 		 py-file-queue))
;;     (pop-to-buffer (current-buffer))
;;     (py-safe (delete-file (car py-file-queue)))
;;     (setq py-file-queue (cdr py-file-queue))
;;     (if py-file-queue
;; 	(let ((pyproc (get-buffer-process (current-buffer))))
;; 	  (py-execute-file pyproc (car py-file-queue))))))

(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(if (functionp 'comint-redirect-send-command-to-process)
    (progn
      (defalias
	'py-shell-redirect-send-command-to-process
	'comint-redirect-send-command-to-process)
      (defalias
	'py-shell-dynamic-simple-complete
	'comint-dynamic-simple-complete))

  ;; XEmacs

  (make-variable-buffer-local 'comint-redirect-completed)
  (defvar py-shell-redirect-output-buffer nil)
  (make-variable-buffer-local 'py-shell-redirect-output-buffer)
  (defvar py-shell-redirect-orginal-output-filter nil)
  (make-variable-buffer-local 'py-shell-redirect-orginal-output-filter)

  (defun py-shell-redirect-filter-function (proc string)
    (let ((procbuf (process-buffer proc))
	  outbuf prompt-pos)
      (with-current-buffer procbuf
	(setq outbuf py-shell-redirect-output-buffer
	      prompt-pos (string-match comint-prompt-regexp string)))
      (if prompt-pos
	  (setq string (substring string 0 prompt-pos)))
      (save-excursion
	(set-buffer outbuf)
	(goto-char (point-max))
	(insert string))
      (if prompt-pos
	  (with-current-buffer procbuf
	    (set-process-filter proc py-shell-redirect-orginal-output-filter)
	    (setq comint-redirect-completed t))))
    "")

  (defun py-shell-redirect-send-command-to-process
    (command output-buffer process echo no-display)
    "Note: ECHO and NO-DISPLAY are ignored"
    ;; prepear
    (with-current-buffer (process-buffer process)
      (setq comint-redirect-completed nil
	    py-shell-redirect-output-buffer (get-buffer output-buffer)
	    py-shell-redirect-orginal-output-filter (process-filter process)))
    (set-process-filter process 'py-shell-redirect-filter-function)
    ;; run
    (comint-simple-send process command))

  (defun py-shell-dynamic-simple-complete (stub candidates)
    (let ((completion (try-completion stub (mapcar 'list candidates))))
      (cond
       ((null completion)
	nil)
       ((eq completion t)
	(message "Sole completion")
	'sole)
       ((> (length completion) (length stub))
	(insert (substring completion (length stub)))
	(if (eq (try-completion completion (mapcar 'list candidates)) t)
	    (progn (message "Completed")
		   'sole)
	  (message "Partially completed")
	  'partial))
       (t
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list (sort candidates 'string<)))
	'listed)))))

(defun py-shell-execute-string-now (string)
  "Send to Python interpreter process PROC \"exec STRING in {}\".
and return collected output"
  (let* ((proc
          ;; (get-process py-which-bufname)
          (get-process (py-process-name)))
	 (cmd (format "exec '''%s''' in {}"
		      (mapconcat 'identity (split-string string "\n") "\\n")))
	(procbuf (process-buffer proc))
	(outbuf (get-buffer-create " *pyshellcomplete-output*"))
	(lines (reverse py-shell-input-lines)))
    (if (and proc (not py-file-queue))
	(unwind-protect
	    (condition-case nil
		(progn
		  (if lines
		      (with-current-buffer procbuf
			(py-shell-redirect-send-command-to-process
			 "\C-c" outbuf proc nil t)
			;; wait for output
			(while (not comint-redirect-completed)
			  (accept-process-output proc 1))))
		  (with-current-buffer outbuf
		    (delete-region (point-min) (point-max)))
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     cmd outbuf proc nil t)
		    (while (not comint-redirect-completed) ; wait for output
		      (accept-process-output proc 1)))
		  (with-current-buffer outbuf
		    (buffer-substring (point-min) (point-max))))
	      (quit (with-current-buffer procbuf
		      (interrupt-process proc comint-ptyp)
		      (while (not comint-redirect-completed) ; wait for output
			(accept-process-output proc 1)))
		    (signal 'quit nil)))
	    (if (with-current-buffer procbuf comint-redirect-completed)
		(while lines
		  (with-current-buffer procbuf
		    (py-shell-redirect-send-command-to-process
		     (car lines) outbuf proc nil t))
		  (accept-process-output proc 1)
		  (setq lines (cdr lines))))))))

(defun py-dot-word-before-point ()
  (buffer-substring
   (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point))
   (point)))

;;;###autoload
(defun py-shell-complete ()
  "Complete word before point, if any. Otherwise insert TAB. "
  (interactive)
  (let ((word (py-dot-word-before-point))
	result)
    (if (equal word "")
	(tab-to-tab-stop)	   ; non nil so the completion is over
      (setq result (py-shell-execute-string-now (format "
def print_completions(namespace, text, prefix=''):
   for name in namespace:
       if name.startswith(text):
           print prefix + name

def complete(text):
    import __builtin__
    import __main__
    if '.' in text:
        terms = text.split('.')
        try:
            if hasattr(__main__, terms[0]):
                obj = getattr(__main__, terms[0])
            else:
                obj = getattr(__builtin__, terms[0])
            for term in terms[1:-1]:
                obj = getattr(obj, term)
            print_completions(dir(obj), terms[-1], text[:text.rfind('.') + 1])
        except AttributeError:
            pass
    else:
        import keyword
        print_completions(keyword.kwlist, text)
        print_completions(dir(__builtin__), text)
        print_completions(dir(__main__), text)
complete('%s')
" word)))
      (if (eq result nil)
	  (message "Could not do completion as the Python process is busy")
	(let ((comint-completion-addsuffix nil)
	      (completions (if (split-string "\n" "\n")
			       (split-string result "\n" t) ; XEmacs
			     (split-string result "\n"))))
	  (py-shell-dynamic-simple-complete word completions))))))

;; (add-hook 'py-shell-hook
;;           '(lambda ()
;;              (require 'py-shell-complete) ; nil t)
;;              (when (functionp 'py-shell-complete)
;;                ;; this should be set in py-shell
;;                (setq comint-input-sender 'py-shell-simple-send)
;;                (local-set-key [tab] 'py-shell-complete))))

;;; IPython Shell Complete

;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

(defvar ipython-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(setq ipython-completion-command-string                                   "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n")

(defun ipython-complete ()
  "Complete the python symbol before point.

Only knows about the stuff in the current *Python* session."
  (interactive "*")
  (let* ((completion-command-string ipython-completion-command-string)
         (ugly-return nil)
         (sep ";")
         (python-process (or (get-buffer-process (current-buffer))
                                        ;XXX hack for .py buffers
                             (get-process py-which-bufname)))
         ;; XXX currently we go backwards to find the beginning of an
         ;; expression part; a more powerful approach in the future might be
         ;; to let ipython have the complete line, so that context can be used
         ;; to do things like filename completion etc.
         (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                              (point)))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))
         (completions nil)
         (completion-table nil)
         completion
         (comint-output-filter-functions
          (append comint-output-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                                        ;(message (format "DEBUG filtering: %s" string))
                      (setq ugly-return (concat ugly-return string))
                      (delete-region comint-last-output-start
                                     (process-mark (get-buffer-process (current-buffer)))))))))
                                        ;(message (format "#DEBUG pattern: '%s'" pattern))
    (process-send-string python-process
                         (format completion-command-string pattern))
    (accept-process-output python-process)

                                        ;(message (format "DEBUG return: %s" ugly-return))
    (setq completions
          (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
    (setq completion-table (loop for str in completions
                                 collect (list str nil)))
    (setq completion (try-completion pattern completion-table))
    (cond ((eq completion t))
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern)
           (ding))
          ((not (string= pattern completion))
           (delete-region beg end)
           (insert completion))
          (t
           (message "Making completion list...")
           (with-output-to-temp-buffer "*Python Completions*"
             (display-completion-list (all-completions pattern completion-table)))
           (message "Making completion list...%s" "done")))))

;;; Pychecker
(defun py-pychecker-run (command)
  "*Run pychecker (default on the file currently visited)."
  (interactive
   (let ((default
           (if (buffer-file-name)
           (format "%s %s %s" py-pychecker-command
                   (mapconcat 'identity py-pychecker-command-args " ")
                       (buffer-file-name))
             (format "%s %s" py-pychecker-command
                     (mapconcat 'identity py-pychecker-command-args " "))))
         (last (when py-pychecker-history
                 (let* ((lastcmd (car py-pychecker-history))
                        (cmd (cdr (reverse (split-string lastcmd))))
                        (newcmd (reverse (cons (buffer-file-name) cmd))))
                   (mapconcat 'identity newcmd " ")))))

     (list
      (if (fboundp 'read-shell-command)
          (read-shell-command "Run pychecker like this: "
                              (if last
                                  last
                                default)
                              'py-pychecker-history)
        (read-string "Run pychecker like this: "
                     (if last
                         last
                       default)
                     'py-pychecker-history)))))
  (save-some-buffers (not py-ask-about-save) nil)
  (if (fboundp 'compilation-start)
      ;; Emacs.
      (compilation-start command)
    ;; XEmacs.
    (when (featurep 'xemacs)
      (compile-internal command "No more errors"))))

(provide 'python-mode)
;;; python-mode.el ends here
