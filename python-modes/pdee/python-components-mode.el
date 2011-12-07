;;; python-components-mode.el --- Towards an Python Development Emacs Environment,

;; Maintainer: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: languages, processes, python, oop

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

(defconst py-version "This is experimental `python-components-mode' not released yet, see https://code.launchpad.net/~a-roehler/python-mode/python-mode-components")


;;; Bindings

(defvar py-mode-map nil)

(setq py-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
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
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
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
        (mapc #'(lambda (key)
                  (define-key map key 'py-newline-and-indent))
              (where-is-internal 'newline-and-indent))
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

(defvar py-font-lock-keywords
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
     (cons (concat "\\<\\(" kw1 "\\)\\>[ \n\t(]") 1)
     ;; builtins when they don't appear as object attributes
     (list (concat "\\([ \t(]\\|^\\)\\<\\(" kw3 "\\)\\>[ \n\t(]") 2
           'py-builtins-face)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     (cons (concat "\\<\\(" kw2 "\\)[ \n\t(]") 1)
     ;; Exceptions
     (list (concat "\\<\\(" kw4 "\\)[ \n\t:,()]") 1 'py-exception-name-face)
     ;; raise stmts
     '("\\<raise[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; except clauses
     '("\\<except[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
     ;; classes
     '("\\<class[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 py-class-name-face)
     ;; functions
     '("\\<def[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ;; pseudo-keywords
     '("\\<\\(self\\|cls\\|Ellipsis\\|True\\|False\\|None\\)\\>"
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
  (interactive)
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

(defcustom py-load-pymacs-p  nil
 "If Pymacs as delivered with python-mode.el shall be loaded.
Default is non-nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"

:type 'boolean
:group 'python)

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

(defun py-set-load-path ()
  "Include needed subdirs of python-mode directory. "
  (interactive)
  (cond (py-install-directory
         (add-to-list 'load-path (expand-file-name py-install-directory))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/completion"))
         (add-to-list 'load-path (concat py-install-directory "/pymacs"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/test"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/tools")))
        (t (error "Please set `py-install-directory', see INSTALL"))
        (when (interactive-p) (message "%s" load-path))))

;; don't require `py-install-directory' for now
(when (boundp 'py-install-directory) (py-set-load-path))

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

(defcustom py-complete-function 'py-completion-at-point
  "Function used for completion in buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
		 (const :tag "Pymacs based py-complete" py-complete)
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

;; (setq py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-")

(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")
(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python)

;; (setq py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[^ \t\n]*\\([ijp]ython[^ \t\n]*$\\)")

(defcustom py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[^ \t\n]*\\([iptj]ython[^ \t\n]*$\\)"
  "Detecting the shell in head of file. "
  :type 'regexp
  :group 'python)

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
  "*Keywords that can be hidden by hide-show"
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
  "[ \t]*\\<\\(return\\|raise\\|break\\|continue\\|pass\\)\\>[ \n\t]"
  "Matches the beginning of a class, method or compound statement. ")

(defconst py-finally-re
  "[ \t]*\\<finally\\>[: \n\t]"
  "Regular expression matching keyword which closes a try-block. ")

(defconst py-except-re
  "[ \t]*\\<except\\>[: \n\t]"
  "Regular expression matching keyword which composes a try-block. ")

(defconst py-else-re
  "[ \t]*\\<else\\>[: \n\t]"
  "Regular expression matching keyword which closes a for- if- or try-block. ")

(defconst py-return-re
  ".*:?[ \t]*\\<\\(return\\)\\>[ \n\t]"
  "Regular expression matching keyword which typically closes a function. ")

(defconst py-no-outdent-re "\\(try:\\|except\\(\\s +.*\\)?:\\|while\\s +.*:\\|for\\s +.*:\\|if\\s +.*:\\|elif\\s +.*:\\)\\([ 	]*\\<\\(return\\|raise\\|break\\|continue\\|pass\\)\\>[ 	\n]\\)")

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

(defconst py-block-re "[ \t]*\\<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\>[: \n\t]"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\<\\(for\\|if\\|try\\)\\>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-try-block-re "[ \t]*\\<try\\>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-class-re "[ \t]*\\<\\(class\\)\\>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\<\\(def\\|class\\)\\>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\<\\(def\\)\\>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re "[ \t]*\\<\\(if\\|else\\|elif\\|while\\|for\\|def\\|class\\|try\\|except\\|finally\\|with\\)\\>[: \n\t]"
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-clause-re "[ \t]*\\<\\(else\\|elif\\|except\\|finally\\)\\>[: \n\t]"
  "Matches the beginning of a compound statement's clause. ")

(defconst py-elif-re "[ \t]*\\<\\elif\\>[: \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively. ")

(defconst py-try-clause-re "[ \t]*\\<\\(except\\|else\\|finally\\)\\>[: \n\t]"
  "Matches the beginning of a compound try-statement's clause. ")

(defconst py-if-re "[ \t]*\\<if\\>[ \n\t]"
  "Matches the beginning of a compound statement saying `if'. ")

(defconst py-try-re "[ \t]*\\<try\\>[: \n\t]"
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
  "Face for builtins like TypeError, object, open, and exec."
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

(eval-when-compile
  (add-to-list 'load-path default-directory))
(require 'python-components-edit)
(require 'python-components-intern)
(require 'python-components-move)
(require 'python-components-execute)
(require 'python-components-send)
(require 'python-components-pdb)
;;(require 'python-components-skeletons)
(require 'python-components-help)
(require 'python-components-extensions)
;; (require 'thingatpt-python-expressions)
(require 'python-components-imenu)
(require 'python-components-completion)
(require 'python-components-named-shells)
(require 'python-components-dedicated-shells)
(require 'python-components-shell-complete)
(require 'python-components-electric)

;;(require 'components-shell-completion)

(require 'python-mode-test)

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

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")

(setq py-shell-map
      (let ((map (copy-keymap comint-mode-map)))
        ;; (substitute-key-definition 'complete-symbol 'completion-at-point
        ;; (substitute-key-definition 'complete-symbol 'py-completion-at-point
        ;; map global-map)
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

(defun python-mode ()
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-shell-name\t\tshell command to invoke Python interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (interactive)
  ;; (unload-python-el)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  (make-local-variable 'fill-paragraph-function)
  ;;
  (set-syntax-table py-mode-syntax-table)
  ;; from python.el, version "22.1"
  (set (make-local-variable 'font-lock-defaults)
       '(py-font-lock-keywords nil nil nil nil
                               (font-lock-syntactic-keywords
                                . py-font-lock-syntactic-keywords)))
  (setq major-mode 'python-mode
        mode-name "Python"
        local-abbrev-table python-mode-abbrev-table
        paragraph-separate "^[ \t]*$"
        paragraph-start "^[ \t]*$"
        require-final-newline t
        comment-start "#"
        comment-end ""
        comment-start-skip "^[ \t]*#+ *"
        comment-column 40
        comment-indent-function 'py-comment-indent-function
        indent-region-function 'py-indent-region
        indent-line-function 'py-indent-line
        ;; tell add-log.el how to find the current function/method/variable
        add-log-current-defun-function 'py-current-defun

        fill-paragraph-function 'py-fill-paragraph)
  (use-local-map py-mode-map)
  ;; add the menu
  (if py-menu
      (easy-menu-add py-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Install Imenu if available
  (when (ignore-errors (require 'imenu))
    (setq imenu-create-index-function #'py-imenu-create-index-new)
    ;;    (setq imenu-create-index-function #'py-imenu-create-index)
    (setq imenu-generic-expression py-imenu-generic-expression)
    (when (fboundp 'imenu-add-to-menubar)
      (imenu-add-to-menubar (format "%s-%s" "IM" mode-name))
      (remove-hook 'imenu-add-menubar-index 'python-mode-hook)))
  ;; Add support for HideShow
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
  ;; Now do the automagical guessing
  (if py-smart-indentation
      (let ((offset py-indent-offset))
        ;; Nil if this fails to guess a good value
        (if (and (ignore-errors (py-guess-indent-offset))
                 (<= py-indent-offset 8)
                 (>= py-indent-offset 2))
            (setq offset py-indent-offset))
        (setq py-indent-offset offset)
        ;; Only turn indent-tabs-mode off if tab-width !=
        ;; py-indent-offset.  Never turn it on, because the user must
        ;; have explicitly turned it off.
        (if (/= tab-width py-indent-offset)
            (setq indent-tabs-mode nil))))
  ;; Set the default shell if not already set
  (when (null py-shell-name)
    (py-toggle-shells (py-choose-shell)))
  ;; (py-set-load-path)
  (when py-load-pymacs-p (py-load-pymacs)
        (find-file (concat py-install-directory "/completion/pycomplete.el"))
        (eval-buffer)
        (kill-buffer "pycomplete.el"))
  (add-hook 'python-mode-hook 'py-beg-of-defun-function)
  (add-hook 'python-mode-hook 'py-end-of-defun-function)
  (set (make-local-variable 'eldoc-documentation-function)
       #'python-eldoc-function)
  (add-hook 'eldoc-mode-hook
	    (lambda () (run-python nil t)) ; need it running
	    nil t)
  (add-hook 'completion-at-point-functions
            py-complete-function nil 'local)
  ;; shell-complete start
  (define-key inferior-python-mode-map [remap complete-symbol]
    'completion-at-point)
  ;; (add-hook 'completion-at-point-functions
  ;; 'python-shell-completion-complete-at-point nil 'local)
  ;; (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  ;; 'python-shell-completion-complete-at-point)
  (define-key inferior-python-mode-map (kbd "<tab>")
    'python-shell-completion-complete-or-indent)
  ;; shell-complete end
  ;; Run the mode hook.  Note that py-mode-hook is deprecated.
  (run-mode-hooks
   (if python-mode-hook
       'python-mode-hook
     'py-mode-hook))
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
\\[python-toggle-shells] to change the interpreter shell."
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

(defconst python-pdbtrack-input-prompt "\n[(<]*[Ii]?[Pp]y?db[>)]+ "
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

(defvar inferior-python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'py-load-file)
    (define-key map "\C-c\C-v" 'python-check)
    ;; Note that we _can_ still use these commands which send to the
    ;; Python process even at the prompt iff we have a normal prompt,
    ;; i.e. '>>> ' and not '... '.  See the comment before
    ;; py-send-region.  Fixme: uncomment these if we address that.

    ;; (define-key map [(meta ?\t)] 'python-complete-symbol)
    ;; (define-key map "\C-c\C-f" 'python-describe-symbol)
    map))

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

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

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

(autoload 'comint-check-proc "comint")

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

(autoload 'comint-get-source "comint")

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

;;;; Context-sensitive help.

(defvar view-return-to-alist)
;; (eval-when-compile (autoload 'help-buffer "help-fns"))

(defvar python-imports)			; forward declaration

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

;;;; Info-look functionality.

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

;;;; Miscellany.

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

;;; Completion.

;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defvar python-imports "None"
  "String of top-level import statements updated by `py-find-imports'.")
(make-variable-buffer-local 'python-imports)

(defun py-find-imports ()
  "Find top-level imports, updating `python-imports'."
  (interactive)
  (save-excursion
      (let (lines)
	(goto-char (point-min))
	(while (re-search-forward "^import\\>[ \n\t]\\|^from\\>[ \n\t]" nil t)
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
;; This is probably best left out since you're unlikely to need the
;; doc for a function in the buffer and the import will lose if the
;; Python sub-process' working directory isn't the same as the
;; buffer's.
;; 			 (if buffer-file-name
;; 			     (concat
;; 			      "import "
;; 			      (file-name-sans-extension
;; 			       (file-name-nondirectory buffer-file-name))))
			 (nreverse lines))
		"None"))
	(when lines
	  (set-text-properties 0 (length python-imports) nil python-imports)
	  ;; The output ends up in the wrong place if the string we
	  ;; send contains newlines (from the imports).
	  (setq python-imports
		(replace-regexp-in-string "\n" "\\n"
					  (format "%S" python-imports) t t))))))

;;;; FFAP support

(defun python-module-path (module)
  "Function for `ffap-alist' to return path to MODULE."
  (python-send-receive (format "emacs.modpath (%S)" module)))

(eval-after-load "ffap"
  '(push '(python-mode . python-module-path) ffap-alist))

;;;; Find-function support

;; Fixme: key binding?

(defun python-find-function (name)
  "Find source of definition of function NAME.

Interactively, prompt for name."
  (interactive
   (let ((symbol (with-syntax-table python-dotty-syntax-table
		   (current-word)))
	 (enable-recursive-minibuffers t))
     (list (read-string (if symbol
			    (format "Find location of (default %s): " symbol)
			  "Find location of: ")
			nil nil symbol))))
  (unless python-imports
    (error "Not called from buffer visiting Python file"))
  (let* ((loc (python-send-receive (format "emacs.location_of (%S, %s)"
					   name python-imports)))
	 (loc (car (read-from-string loc)))
	 (file (car loc))
	 (line (cdr loc)))
    (unless file (error "Don't know where `%s' is defined" name))
    (pop-to-buffer (find-file-noselect file))
    (when (integerp line)
      (goto-char (point-min))
      (forward-line (1- line)))))


;;;; Bicycle Repair Man support

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'brm-init "bikemacs")

;; I'm not sure how useful BRM really is, and it's certainly dangerous
;; the way it modifies files outside Emacs...  Also note that the
;; current BRM loses with tabs used for indentation -- I submitted a
;; fix <URL:http://www.loveshack.ukfsn.org/emacs/bikeemacs.py.diff>.
(defun python-setup-brm ()
  "Set up Bicycle Repair Man refactoring tool (if available).

Note that the `refactoring' features change files independently of
Emacs and may modify and save the contents of the current buffer
without confirmation."
  (interactive)
  (condition-case data
      (unless (fboundp 'brm-rename)
	(pymacs-load "bikeemacs" "brm-") ; first line of normal recipe
	(let ((py-mode-map (make-sparse-keymap)) ; it assumes this
	      (features (cons 'python-mode features))) ; and requires this
	  (brm-init)			; second line of normal recipe
	  (remove-hook 'python-mode-hook ; undo this from `brm-init'
		       '(lambda () (easy-menu-add brm-menu)))
	  (easy-menu-define
	    python-brm-menu py-mode-map
	    "Bicycle Repair Man"
	    '("BicycleRepairMan"
	      :help "Interface to navigation and refactoring tool"
	      "Queries"
	      ["Find References" brm-find-references
	       :help "Find references to name at point in compilation buffer"]
	      ["Find Definition" brm-find-definition
	       :help "Find definition of name at point"]
	      "-"
	      "Refactoring"
	      ["Rename" brm-rename
	       :help "Replace name at point with a new name everywhere"]
	      ["Extract Method" brm-extract-method
	       :active (and mark-active (not buffer-read-only))
	       :help "Replace statements in region with a method"]
	      ["Extract Local Variable" brm-extract-local-variable
	       :active (and mark-active (not buffer-read-only))
	       :help "Replace expression in region with an assignment"]
	      ["Inline Local Variable" brm-inline-local-variable
	       :help
	       "Substitute uses of variable at point with its definition"]
	      ;; Fixme:  Should check for anything to revert.
	      ["Undo Last Refactoring" brm-undo :help ""]))))
    (error (error "BicycleRepairMan setup failed: %s" data))))

;;;; Modes.


(defvar outline-heading-end-regexp)
(defvar eldoc-documentation-function)
(defvar python-mode-running)            ;Dynamically scoped var.



;; pdbtrack features

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

;; Author: Lukasz Pankowski, patch sent for lp:328836
(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

;;; need to clear py-shell-input-lines if primary prompt found

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
;; 	  (py-execute-file pyproc (car py-file-queue))))
;;     ))

;;;

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
  (defvar py-shell-redirect-output-buffer)
  (make-variable-buffer-local 'py-shell-redirect-output-buffer)
  (defvar py-shell-redirect-orginal-output-filter)
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

(defun py-proc ()
  "Return the current Python process.
See variable `python-buffer'.  Starts a new process if necessary."
  ;; Fixme: Maybe should look for another active process if there
  ;; isn't one for `python-buffer'.
  (unless (comint-check-proc python-buffer)
    (run-python nil t))
  (get-buffer-process (if (derived-mode-p 'inferior-python-mode)
			  (current-buffer)
			python-buffer)))

(defun py-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.
The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (python-send-string string)
  (let ((proc (py-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
	(while (progn
		 (accept-process-output proc 5)
		 (null python-preoutput-result)))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

;; IPython Completion start

;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

(defvar ipython-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(setq ipython-completion-command-string                                   "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n")

;; (defun py-shell-complete ()
;;   "Try to complete the python symbol before point. Only knows about the stuff
;; in the current *Python* session."
;;   (interactive)
;;   (let* ((completion-command-string ipython-completion-command-string)
;;          (ugly-return nil)
;;          (sep ";")
;;          (python-process (or (get-buffer-process (current-buffer))
;;                                         ;XXX hack for .py buffers
;;                              (get-process py-which-bufname)))
;;          ;; XXX currently we go backwards to find the beginning of an
;;          ;; expression part; a more powerful approach in the future might be
;;          ;; to let ipython have the complete line, so that context can be used
;;          ;; to do things like filename completion etc.
;;          (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
;;                               (point)))
;;          (end (point))
;;          (pattern (buffer-substring-no-properties beg end))
;;          (completions nil)
;;          (completion-table nil)
;;          completion
;;          (comint-output-filter-functions
;;           (append comint-output-filter-functions
;;                   '(ansi-color-filter-apply
;;                     (lambda (string)
;;                                         ;(message (format "DEBUG filtering: %s" string))
;;                       (setq ugly-return (concat ugly-return string))
;;                       (delete-region comint-last-output-start
;;                                      (process-mark (get-buffer-process (current-buffer)))))))))
;;                                         ;(message (format "#DEBUG pattern: '%s'" pattern))
;;     (process-send-string python-process
;;                          (format completion-command-string pattern))
;;     (accept-process-output python-process)
;;
;;                                         ;(message (format "DEBUG return: %s" ugly-return))
;;     (setq completions
;;           (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
;;     (setq completion-table (loop for str in completions
;;                                  collect (list str nil)))
;;     (setq completion (try-completion pattern completion-table))
;;     (cond ((eq completion t))
;;           ((null completion)
;;            (message "Can't find completion for \"%s\"" pattern)
;;            (ding))
;;           ((not (string= pattern completion))
;;            (delete-region beg end)
;;            (insert completion))
;;           (t
;;            (message "Making completion list...")
;;            (with-output-to-temp-buffer "*Python Completions*"
;;              (display-completion-list (all-completions pattern completion-table)))
;;            (message "Making completion list...%s" "done")))))

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

;; Completion start

(provide 'python-components-mode)
(provide 'python-mode)
;;; python-components-mode.el ends her
