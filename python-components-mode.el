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

;; commands-python-mode.org in directory doc reports
;; available commands, also a menu is provided

;; as for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'. Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; `py-expression' composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; `py-partial-expression' beginns with a "(", which is
;; not taken as proposal.

;;; Code

;; (add-to-list 'load-path py-install-directory)
;; (add-to-list 'load-path (concat py-install-directory "extensions"))
;; make it easier to run from different branches
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (concat default-directory "extensions"))

(require 'comint)
(require 'hippie-exp)
(require 'comint)
(require 'custom)
(require 'cl)
(require 'compile)
(require 'ansi-color)
(require 'cc-cmds)
(require 'shell)
;; (require 'python)
(require 'flymake)
(require 'python-components-macros)
(require 'python-components-nomacros)
(require 'thingatpt)

(defgroup python-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "This is experimental `python-components-mode' not released yet, see https://code.launchpad.net/~a-roehler/python-mode/python-mode-components")

;;; User definable variables
(defcustom py-install-directory ""
  "Directory where python-mode.el and it's subdirectories should be installed. Needed for completion and other environment stuff only. "

  :type 'string
  :group 'python-mode)

(defcustom python-mode-modeline-display "Py"
  "String to display in Emacs modeline "

  :type 'string
  :group 'python-mode)

(defcustom py-extensions "py-extensions.el"
  "File where extensions to python-mode.el should be installed. Used by virtualenv support. "

  :type 'string
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives "

  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-fontify-shell-buffer-p)

(defcustom py-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-prepare-autopair-mode-p nil
  "If autopair-mode stuff should be loaded. Default is `nil'

When non-nil, M-x `autopair-mode' will toggle it.
See also `autopair-mode-on'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-no-completion-calls-dabbrev-expand-p t
  "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-no-completion-p t
  "If completion function should insert a TAB when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-org-cycle-p nil
  "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-start-run-py-shell t
  "If `python-mode' should start a python-shell, `py-shell'. Default is `t'. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-start-run-ipython-shell t "If
`python-mode' should start an ipython-shell. Default
is `t'.

A running ipython-shell presently is needed by
`ipython-complete', otherwise first try will fail. "

  :type 'boolean :group 'python-mode)

(defcustom ipython-complete-use-separate-shell-p nil

  "If `ipython-complete' should use a separate shell. Thus prompt-counter is not incremented by completion. "
  :type 'boolean :group 'python-mode)

(defcustom py-outline-minor-mode-p t
  "If outline minor-mode should be on, default is `t'. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-guess-py-install-directory-p t
  "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs as delivered with python-mode.el shall be loaded.
Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"

  :type 'boolean
  :group 'python-mode)

(defcustom py-verbose-p nil
  "If functions should report results.

Default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-smart-operator-mode-p nil
  "If python-mode calls (py-smart-operator-mode-on)

Default is non-nil. "

  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-smart-operator-mode-p)

(defcustom py-sexp-function nil
  "When set, it's value is called instead of `forward-sexp', `backward-sexp'

Default is nil. "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-end-of-partial-expression" py-end-of-partial-expression)
          (const :tag "py-end-of-expression" py-end-of-expression))
  :group 'python-mode)
(make-variable-buffer-local 'py-sexp-function)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty. Default is non-nil. "
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-close-provides-newline)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-dedents-bos nil
  "If non-nil, closing parentesis dedents onto column of statement, otherwise keeps additional `py-indent-offset', default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-greedy-p nil
  "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-newline-and-indent-p nil
  "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-comment-p t
  "If \"#\" should call `py-electric-comment'. Default is `t'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-comment-add-space-p nil
  "If py-electric-comment should add a space.  Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-mark-decorators nil
  "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-tab-indent t
  "*Non-nil means TAB in Python mode calls `py-indent-line'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-autopair-mode t
  "Load `autopair-mode' written by Joao Tavora <joaotavora [at] gmail.com>.

URL: http://autopair.googlecode.com "
  :type 'boolean
  :group 'python-mode)

(defcustom py-complete-function 'py-shell-complete
  "When set, enforces function todo completion, default is nil.

Normally python-mode, resp. inferior-python-mode know best which function to use. "
  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-completion-at-point" py-completion-at-point)
          (const :tag "Pymacs based py-complete" py-complete)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "IPython's ipython-complete" ipython-complete)
          )
  :group 'python-mode)
(make-variable-buffer-local 'py-complete-function)

(defcustom ipython-complete-function 'ipython-complete
  "Function used for completion in IPython shell buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
                 (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)
(make-variable-buffer-local 'ipython-complete-function)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python-mode)

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
(make-variable-buffer-local 'py-python-command-args)

(defcustom py-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python-mode
  :tag "Jython Command Args")

(defcustom py-message-executing-temporary-file t
  "If execute functions using a temporary file should message it. Default is `t'.

Messaging increments the prompt counter of IPython shell. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-lhs-inbound-indent)

(defcustom py-rhs-inbound-indent 1
  "When inside a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-rhs-inbound-indent)

(defcustom py-cleanup-temporary t
  "If temporary buffers and files used by functions executing region should be deleted afterwards. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-continuation-offset 2
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python-mode)

(defcustom py-indent-tabs-mode nil
  "Python-mode starts `indent-tabs-mode' with the value specified here, default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-smart-indentation t
  "*Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

 1. `py-indent-offset' is guessed from existing code in the buffer.
 Only guessed values between 2 and 8 are considered.  If a valid
 guess can't be made (perhaps because you are visiting a new
 file), then the value in `py-indent-offset' is used.

 2. `tab-width' is setq to `py-indent-offset' if not equal
 already. `indent-tabs-mode' inserts one tab one
 indentation level, otherwise spaces are used.

 Note that both these settings occur *after* `python-mode-hook' is run,
 so if you want to defeat the automagic configuration, you must also
 set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-smart-indentation)

(defcustom py-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
                 (const :tag "Align to column zero" nil))
  :group 'python-mode)

(defcustom py-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
 `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-indent-offset 4
  "*Amount of offset per level of indentation.
 `\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)

(defcustom pdb-path '/usr/lib/python2.7/pdb.py
  "Where to find pdb.py. Edit this according to your system.

If you ignore the location `M-x py-guess-pdb-path' might display it.
 "
  :type 'variable
  :group 'python-mode)

(defcustom py-indent-comments t
  "When t, comment lines are indented. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-separator-char ?\/
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function `py-separator-char'. "
  :type 'character
  :group 'python-mode)

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from `py-temp-directory'. Default is the empty string. "
  :type 'string
  :group 'python-mode)

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
  :group 'python-mode)

(defcustom py-jump-on-exception t
  "*Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python-mode)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python-mode)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python-mode)

(defcustom py-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python-mode)

(defcustom py-pdbtrack-do-tracking-p t
  "*Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb."
  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-filename-mapping nil
  "Supports mapping file paths when opening file buffers in pdbtrack.
When non-nil this is an alist mapping paths in the Python interpreter
to paths in Emacs."
  :type 'alist
  :group 'python-mode)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python-mode)

(defcustom py-import-check-point-max
  20000
  "Maximum number of characters to search for a Java-ish import statement.
When `python-mode' tries to calculate the shell to use (either a
CPython or a Jython shell), it looks at the so-called `shebang' line
                           -- i.e. #! line.  If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :group 'python-mode)

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-current-defun-show t
  "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'."

  :type 'boolean
  :group 'python-mode)

(defcustom py-current-defun-delay 2
  "When called interactively, `py-current-defun' should wait PY-WHICH-FUNC-DELAY seconds at the definition name found, before returning to previous position. "

  :type 'number
  :group 'python-mode)

(defcustom py-send-receive-delay 5
  "Seconds to wait for output, used by `python-send-receive'. "

  :type 'number
  :group 'python-mode)

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
  :group 'python-mode)
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "*Shell command used to run Pychecker."
  :type 'string
  :group 'python-mode
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "*List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python-mode
  :tag "Pychecker Command Args")

(defcustom py-pyflakes-command "pyflakes"
  "*Shell command used to run Pyflakes."
  :type 'string
  :group 'python-mode
  :tag "Pyflakes Command")

(defcustom py-pyflakes-command-args '("")
  "*List of string arguments to be passed to pyflakes.

Default is \"\""
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakes Command Args")

(defcustom py-pep8-command "pep8"
  "*Shell command used to run pep8."
  :type 'string
  :group 'python-mode
  :tag "PEP 8 Command")

(defcustom py-pep8-command-args '("")
  "*List of string arguments to be passed to pylint.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "PEP 8 Command Args")

(defcustom py-pyflakespep8-command (concat py-install-directory "pyflakespep8.py")
  "*Shell command used to run `pyflakespep8'."
  :type 'string
  :group 'python-mode
  :tag "Pyflakespep8 Command")

(defcustom py-pyflakespep8-command-args '("")
  "*List of string arguments to be passed to pyflakespep8.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakespep8 Command Args")

(defcustom py-pylint-command "pylint"
  "*Shell command used to run Pylint."
  :type 'string
  :group 'python-mode
  :tag "Pylint Command")

(defcustom py-pylint-command-args '("--errors-only")
  "*List of string arguments to be passed to pylint.

Default is \"--errors-only\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pylint Command Args")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
first line of input."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.  Setting this
variable will only effect new shells."
  :type 'boolean
  :group 'python-mode)

(defcustom py-shell-switch-buffers-on-execute-p t
  "When non-nil switch to the new Python shell. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-honor-IPYTHONDIR-p nil
  "When non-nil ipython-history file is constructed by $IPYTHONDIR
followed by \"/history\". Default is nil.

Otherwise value of py-ipython-history is used. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-ipython-history "~/.ipython/history"
  "ipython-history default file. Used when py-honor-IPYTHONDIR-p is nil (default) "

  :type 'string
  :group 'python-mode)

(defcustom py-honor-PYTHONHISTORY-p nil
  "When non-nil python-history file is set by $PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-python-history "~/.python_history"
  "python-history default file. Used when py-honor-PYTHONHISTORY-p is nil (default) "

  :type 'string
  :group 'python-mode)

(defcustom py-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-split-windows-on-execute-p t
  "When non-nil split windows. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-max-split-windows 2
  "When split windows is enabled the maximum windows to allow
  before reusing other windows."
  :type 'number
  :group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions. "
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
                 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :group 'python-mode)
(make-variable-buffer-local 'py-split-windows-on-execute-function)

(defcustom py-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads. "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "*Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python-mode)

(defcustom py-hide-comments-when-hiding-all t
  "*Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads. "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python-mode)

(defcustom py-shell-prompt-alist
  '(("ipython" . "^In \\[[0-9]+\\]: ")
    (t . "^>>> "))
  "Alist of Python input prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-continuation-prompt-alist
  '(("ipython" . "^   [.][.][.]+: ")
    (t . "^[.][.][.] "))
  "Alist of Python continued-line prompts.
Each element has the form (PROGRAM . REGEXP), where PROGRAM is
the value of `py-shell-name' for the python process and
REGEXP is a regular expression matching the Python prompt for
continued lines.
PROGRAM can also be t, which specifies the default when no other
element matches `py-shell-name'."
  :type 'string
  :group 'python-mode)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."

  :group 'python-mode
  :type 'hook)

(defcustom py-shell-name "python"
  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if no shell is specified by command. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-name)

(defcustom py-shell-toggle-1 py-shell-name
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-toggle-1)

(defcustom py-shell-toggle-2 "python3"
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-toggle-2)

(defcustom python-guess-indent t
  "Non-nil means Python mode guesses `py-indent-offset' for the buffer."
  :type 'boolean
  :group 'python-mode)

(defcustom py-imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu of functions and global variables. "
  :type 'boolean
  :group 'python-mode)

(defcustom python-indent-string-contents t
  "Non-nil means indent contents of multi-line strings together.
This means indent them the same as the preceding non-blank line.
Otherwise preserve their indentation.

This only applies to `doc' strings, i.e. those that form statements;
the indentation is preserved in others."
  :type '(choice (const :tag "Align with preceding" t)
                 (const :tag "Preserve indentation" nil))
  :group 'python-mode)

(defcustom python-honour-comment-indentation nil
  "Non-nil means indent relative to preceding comment line.
Only do this for comments where the leading comment character is
followed by space.  This doesn't apply to comment lines, which
are always indented in lines with preceding comments."
  :type 'boolean
  :group 'python-mode)

(defcustom python-continuation-offset 4
  "Number of columns of additional indentation for continuation lines.
Continuation lines follow a backslash-terminated line starting a
statement."
  :group 'python-mode
  :type 'integer)

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
  :group 'python-mode)

(defcustom python-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)

(defcustom python-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python-mode
  :tag "JPython Command Args")

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
  :group 'python-mode)
(make-variable-buffer-local 'python-pdbtrack-do-tracking-p)

(defcustom python-pdbtrack-minor-mode-string " PDB"
  "*Minor-mode sign to be displayed when pdbtrack is active."
  :type 'string
  :group 'python-mode)

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
  :group 'python-mode)

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
  :group 'python-mode)

(defcustom python-python-command "python"
  "Shell command to run Python interpreter.
Any arguments can't contain whitespace."
  :group 'python-mode
  :type 'string)

(defcustom python-jython-command "jython"
  "Shell command to run Jython interpreter.
Any arguments can't contain whitespace."
  :group 'python-mode
  :type 'string)

(defcustom py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)

(defcustom python-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `python-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python-mode)

(defcustom python-jython-packages '("java" "javax" "org" "com")
  "Packages implying `jython-mode'.
If these are imported near the beginning of the buffer, `python-mode'
actually punts to `jython-mode'."
  :type '(repeat string)
  :group 'python-mode)

(defcustom python-use-skeletons nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition.

Kept for compatibility reasons.
Don't activate this, with some probability it will mess up abbrev edits, leaving abbrev-mode unusable. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-match-paren-mode nil
  "*Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
                               `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-kill-empty-line t
  "*If t, py-indent-forward-line kills empty lines. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-imenu-show-method-args-p nil
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'python-mode)

(defcustom py-use-local-default nil
  "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-underscore-word-syntax-p t
  "If underscore chars should be of syntax-class `word', not of `symbol'.

Underscores in word-class makes `forward-word' etc. travel the indentifiers. Default is `t'.

See bug report at launchpad, lp:940812 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-edit-only-p nil
  "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-force-py-shell-name-p nil
  "When `t', execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence. "

  :type 'boolean
  :group 'python-mode)

(defcustom python-mode-v5-behavior-p nil
  "Execute region through `shell-command-on-region' as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :group 'python-mode)

(defcustom py-trailing-whitespace-smart-delete-p nil
  "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected "
  :type 'boolean
  :group 'python-mode)

(defcustom py-warn-tmp-files-left-p nil
  "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' "

  :type 'boolean
  :group 'python-mode)

(defcustom py-shell-local-path ""
  "If `py-use-local-default' is non-nil, `py-shell' will use EXECUTABLE indicated here incl. path. "

  :type 'string
  :group 'python-mode)

(defcustom py-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running. "
  :type 'float
  :group 'python-mode)

(defcustom python-shell-setup-codes '(python-shell-completion-setup-code
                                      python-ffap-setup-code
                                      python-eldoc-setup-code
                                      py-emacs-import-code
                                      )
  "List of code run by `python-shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'python-mode)

(defcustom python-shell-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
	  (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
	  "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
	  (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
	  "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python."
  :type '(alist string)
  :group 'python-mode)

(defcustom python-shell-completion-setup-code
  "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :group 'python-mode
  :safe 'stringp)

(defcustom python-shell-completion-string-code
  "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get a string of completions separated by semicolons."
  :type 'string
  :group 'python-mode
  :safe 'stringp)

(defcustom python-shell-module-completion-string-code ""
  "Python code used to get completions separated by semicolons for imports.

For IPython v0.11, add the following line to
`python-shell-completion-setup-code':

from IPython.core.completerlib import module_completion

and use the following as the value of this variable:

';'.join(module_completion('''%s'''))"
  :type 'string
  :group 'python-mode
  :safe 'stringp)

(defcustom py-imenu-create-index-function 'py-imenu-create-index-new
  "Switch between `py-imenu-create-index-new', which also lists modules variables,  and series 5. index-machine"
  :type '(choice (const :tag "'py-imenu-create-index-new, also lists modules variables " py-imenu-create-index-new)
                 (const :tag "py-imenu-create-index, series 5. index-machine" py-imenu-create-index-function))
  :group 'python-mode)

(defcustom python-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `python-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python-mode)

(defcustom inferior-python-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)

(defcustom python-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
an inferior Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :group 'python-mode)

(defcustom strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom py-fill-docstring-style 'pep-257-nn
  "Implemented styles are DJANGO, ONETWO, PEP-257, PEP-257-NN,
SYMMETRIC, and NIL.

A value of NIL won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

DJANGO:

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

ONETWO:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

PEP-257-NN:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

SYMMETRIC:

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'python-mode
  :safe (lambda (val)
          (memq val '(django onetwo pep-257 pep-257-nn symmetric nil))))

(defcustom py-execute-directory nil
  "When set, stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of `py-execute-buffer' and related commands. See also `py-use-current-dir-when-execute-p'"
    :type 'string
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p nil
  "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'"
  :type 'boolean
  :group 'python-mode)

;; the python-el way
(defcustom python-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :group 'python-mode)

;;; defvarred Variables
(defvar py-emacs-import-code "import emacs")

(defvar python-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol
syntax or has word syntax and isn't a letter.")

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
        (when py-underscore-word-syntax-p
          (modify-syntax-entry ?_ "w" table))
        table))

(defvar python-local-version nil
  "Used internally. ")
(make-variable-buffer-local 'python-local-version)

(defvar py-local-command nil
  "Returns locally used executable-name. ")
(make-variable-buffer-local 'py-local-command)

(defvar py-local-versioned-command nil
  "Returns locally used executable-name including its version. ")
(make-variable-buffer-local 'py-local-versioned-command)

(defvar ipython-completion-command-string nil
  "Either ipython0.10-completion-command-string or ipython0.11-completion-command-string.

ipython0.11-completion-command-string also covers version 0.12")
(make-variable-buffer-local 'ipython-completion-command-string)

(defvar ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar py-local-complete-function nil
  "Set by python-mode-hook resp. to environment.

`py-complete-function', when set, overrides it. ")
(make-variable-buffer-local 'py-local-complete-function)

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]?\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file. ")

(defvar py-separator-char 47
  "Values set by defcustom only will not be seen in batch-mode. ")

(defvar py-temp-directory
  (let ((ok '(lambda (x)
               (and x
                    (setq x (expand-file-name x)) ; always true
                    (file-directory-p x)
                    (file-writable-p x)
                    x)))
        erg)
    (or
     (and (not (string= "" py-custom-temp-directory))
          (if (funcall ok py-custom-temp-directory)
              (setq erg (expand-file-name py-custom-temp-directory))
            (if (file-directory-p (expand-file-name py-custom-temp-directory))
                (error "py-custom-temp-directory set but not writable")
              (error "py-custom-temp-directory not an existing directory"))))
     (and (funcall ok (getenv "TMPDIR"))
          (setq erg (getenv "TMPDIR")))
     (and (funcall ok (getenv "TEMP/TMP"))
          (setq erg (getenv "TEMP/TMP")))
     (and (funcall ok "/usr/tmp")
          (setq erg "/usr/tmp"))
     (and (funcall ok "/tmp")
          (setq erg "/tmp"))
     (and (funcall ok "/var/tmp")
          (setq erg "/var/tmp"))
     (and (eq system-type 'darwin)
          (funcall ok "/var/folders")
          (setq erg "/var/folders"))
     (and (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
          (funcall ok (concat "c:" (char-to-string py-separator-char) "Users"))
          (setq erg (concat "c:" (char-to-string py-separator-char) "Users")))
     ;; (funcall ok ".")
     (error
      "Couldn't find a usable temp directory -- set `py-temp-directory'"))
    (when erg (setq py-temp-directory erg)))
  "*Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
                          /usr/tmp, /tmp, /var/tmp, or the current directory.

                          `py-custom-temp-directory' will take precedence when setq ")

(defvar py-pdbtrack-input-prompt)

(defvar py-pydbtrack-input-prompt)

(defvar py-exec-command nil
  "Mode commands will set this. ")
(make-variable-buffer-local 'py-exec-command)

(defvar py-exec-string-command nil
  "Mode commands will set this. ")
(make-variable-buffer-local 'py-exec-string-command)

(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-bufname)

(defvar py-pychecker-history nil)

(defvar py-pyflakes-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pylint-history nil)

(defvar py-shell-alist
  '(("jython" . 'jython)
    ("python" . 'cpython))
  "*Alist of interpreters and python shells. Used by `py-choose-shell'
to select the appropriate python interpreter mode for a file.")

(defvar ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:"
  "A regular expression to match the IPython input prompt. ")

(defvar ipython-de-output-prompt-regexp "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defvar py-shell-switch-buffers-on-execute-p t
  "When non-nil switch to the new Python shell.

You may customize this variable ")

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar python-command "python"
  "Used for `py-completion-at-point', derived from python.el." )

(defvar py-python-command py-shell-name)

(defvar py-jpython-command py-shell-name)

(defvar py-jython-command py-shell-name)

(defvar py-default-interpreter py-shell-name)

(defvar hs-hide-comments-when-hiding-all nil
  "Defined in hideshow.el, silence compiler warnings here. ")

(defvar python-which-shell nil)
(make-variable-buffer-local 'python-which-shell)

(defvar python-which-args python-python-command-args)
(make-variable-buffer-local 'python-which-args)

(defvar python-which-bufname "Python")
(make-variable-buffer-local 'python-which-bufname)

(defvar py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/\\|^__pyfile = open('''\\|^execfile(r'[.+]/tmp/")

(defvar py-force-local-shell-p nil
  "Used internally, see `toggle-force-local-shell'. ")

(defvar python-mode-v5-behavior nil)

(defvar py-shell-complete-debug nil
  "For interal use when debugging." )

(defvar py-completion-last-window-configuration nil
  "Internal use: restore py-restore-window-configuration when completion is done resp. abandoned. ")

(defvar python-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol
syntax or has word syntax and isn't a letter.")

(defvar view-return-to-alist)

(defvar python-imports)
(make-variable-buffer-local 'python-imports)

(defvar py-prev-dir/file nil
  "Caches (directory . file) pair used in the last `py-load-file' command.
Used for determining the default in the next one.")

(defvar py-exception-buffer nil)

(defvar py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string. ")

(defvar py-labelled-re "[ \\t]*:[[:print:]]+"
  "When looking at label. ")
;; (setq py-labelled-re "[ \\t]*:[[:graph:]]+")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-skip-chars "^ (:=#\t\r\n\f"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-looking-re "[^ =#\t\r\n\f]+"
  "py-expression assumes chars indicated possible composing a py-expression, when looking-at or -back. ")

(defvar py-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")

(defvar py-not-expression-chars " .=#\t\r\n\f"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")

(defvar py-partial-expression-skip-chars "^ .()[]{}=:#\t\r\n\f"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")

(defvar py-partial-expression-forward-regexp "[^ .()}=:#\t\r\n\f]"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")

(defvar py-partial-expression-skip-backward-chars "^ .\"(){}[]=:#\t\r\n\f"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")

(defvar py-not-partial-expression-skip-chars " \\.=:#\t\r\n\f"
  "py-partial-expression assumes chars indicated may not compose a py-partial-expression, skip it. ")

(defvar py-partial-expression-looking-regexp "[^ ).=:#\t\r\n\f]"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, when looking-at or -back. ")

(defvar py-not-partial-expression-regexp "[ ).=:#\t\r\n\f]"
  "py-partial-expression assumes chars indicated probably will not compose a py-partial-expression. ")

(defvar py-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of Python operators inclusive whitespaces around.

See also `py-assignment-regexp' ")

(defvar py-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `py-operator-regexp' ")

(defvar py-delimiter-regexp "\\(,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs. ")

(defvar py-delimiter-chars ",;."
  "Chars delimiting elements of lists or other programming constructs. ")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py-jump-to-exception.")

(defvar match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^ \t>]+>[^0-9]+\\([0-9]+\\)\\|^[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.
Inludes Python shell-prompt in order to stop further searches. ")

(defvar py-bol-forms-last-indent nil
  "For internal use. Stores indent from last py-end-of-FORM-bol command.
When this-command is py-beginning-of-FORM-bol, last-command's indent will be considered in order to jump onto right beginning position.")
(make-variable-buffer-local 'py-bol-forms-last-indent)

(defvar py-XXX-tag-face 'py-XXX-tag-face)

(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defvar py-variable-name-face 'py-variable-name-face)

(defvar py-number-face 'py-number-face)

(defvar py-decorators-face 'py-decorators-face)

(defvar py-builtins-face 'py-builtins-face)

(defvar py-class-name-face 'py-class-name-face)

(defvar py-exception-name-face 'py-exception-name-face)

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
                                 `python-mode-hook'.")

(defvar py-shell-hook nil
  "*Hook called by `py-shell'.")

(defvar python-font-lock-keywords)

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar python-dotty-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Python files.
It makes underscores and dots word constituent chars.")

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

Use \\[python-set-proc] to set the default value from a buffer with a
local value.")
(make-variable-buffer-local 'python-buffer)

(defvar inferior-python-mode-syntax-table
  (let ((st (make-syntax-table python-mode-syntax-table)))
    ;; Don't get confused by apostrophes in the process's output (e.g. if
    ;; you execute "help(os)").
    (modify-syntax-entry ?\' "." st)
    ;; Maybe we should do the same for double quotes?
    ;; (modify-syntax-entry ?\" "." st)
    st))

(defvar python-preoutput-result nil
  "Data from last `_emacs_out' line seen by the preoutput filter.")

(defvar python-preoutput-continuation nil
  "If non-nil, funcall this when `py-preoutput-filter' sees `_emacs_ok'.")

(defvar python-preoutput-leftover nil)

(defvar python-preoutput-skip-next-prompt nil)

(defvar python-version-checked nil)

(defvar python-prev-dir/file nil
  "Caches (directory . file) pair used in the last `python-load-file' command.
Used for determining the default in the next one.")

(defvar python-prev-dir/file nil
  "Caches (directory . file) pair used in the last `python-load-file' command.
Used for determining the default in the next one.")

(defvar python-imports "None"
  "String of top-level import statements updated by `python-find-imports'.")

(defvar python-default-template "if"
  "Default template to expand by `python-expand-template'.
Updated on each expansion.")

(defvar outline-heading-end-regexp)

(defvar python-mode-running)            ;Dynamically scoped var.

(defvar python--prompt-regexp nil)

(defvar py-shell-input-lines nil
  "Collect input lines send interactively to the Python process in
order to allow injecting completion command between keyboard interrupt
and resending the lines later. The lines are stored in reverse order")

(defvar py-shell-map (make-sparse-keymap)
  "Keymap used in *Python* shell buffers.")

(defvar inferior-python-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    ;; (substitute-key-definition 'complete-symbol 'py-shell-complete map global-map)
    (substitute-key-definition 'complete-symbol 'completion-at-point map global-map)
    (define-key map (kbd "RET") 'comint-send-input)
    (if py-complete-function
        (define-key map [tab] 'py-complete-function)
      (define-key map [tab] 'python-completion-at-point))
    (define-key map "\C-c-" 'py-up-exception)
    (define-key map "\C-c=" 'py-down-exception)))

(defvar python-shell-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "RET") 'comint-send-input)
    (substitute-key-definition 'complete-symbol 'completion-at-point map global-map)
    ;; (if py-complete-function
    ;;     (progn
    ;;         (substitute-key-definition 'complete-symbol py-complete-function
    ;;                            map global-map)
    ;;     (define-key map [tab] py-complete-function))
    ;;   (define-key map [tab] 'py-completion-at-point))
    (define-key map "\C-c-" 'py-up-exception)
    (define-key map "\C-c=" 'py-down-exception)

    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'python-load-file)
    (define-key map "\C-c\C-v" 'python-check)
    ;; Note that we _can_ still use these commands which send to the
    ;; Python process even at the prompt iff we have a normal prompt,
    ;; i.e. '>>> ' and not '... '.  See the comment before
    ;; py-send-region.  Fixme: uncomment these if we address that.

    ;; (define-key map [(meta ?\t)] 'python-complete-symbol)
    ;; (define-key map "\C-c\C-f" 'python-describe-symbol)
    map))

(defvar py-already-guessed-indent-offset nil
  "Internal use by py-indent-line, use the guess already computed. ")
(make-variable-buffer-local 'py-already-guessed-indent-offset)

;;; Constants
(defconst python-dotty-syntax-table
  (let ((table (make-syntax-table)))
    (set-char-table-parent table python-mode-syntax-table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table giving `.' symbol syntax.
Otherwise inherits from `python-mode-syntax-table'.")

(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "regular expression matching a blank or comment line.")

(defconst py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement. ")

(setq py-block-closing-keywords-re
  "[ \t]*\\_<return\\|raise\\|break\\|continue\\|pass\\_>[ \n\t]")

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

;; (setq py-no-outdent-re "[ 	]*\\_<\\(try:\\|except\\(\\s +.*\\)?:\\|while\\s +.*:\\|for\\s +.*:\\|if\\s +.*:\\|elif\\s +.*:\\|return\\|raise\\|break\\|continue\\|pass\\)\\_>[ 	\n]*")

(defconst py-assignment-re "\\_<\\w+\\_>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignment. ")

(defconst py-block-re "[ \t]*\\_<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\_<\\(for\\|if\\|try\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of an `for', `if', `try' or `with' block. ")

(defconst py-try-block-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of an `if' or `try' block. ")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\_<\\(def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re "[ \t]*\\_<\\(if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-extended-block-or-clause-re "[ \t]*\\_<\\(def\\|class\\|if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[: \n\t]"
  "Matches the beginning of a compound statement or it's clause.
Includes def and class. ")

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

(defconst python-compilation-regexp-alist
  ;; FIXME: maybe these should move to compilation-error-regexp-alist-alist.
  ;;   The first already is (for CAML), but the second isn't.  Anyhow,
  ;;   these are specific to the inferior buffer.  -- fx
  `((,(rx bol (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    ;; pdb stack trace
    (,(rx bol "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Python.")

(defconst py-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (py-quote-syntax 1) t t)
     (2 (py-quote-syntax 2) t t)
     (3 (py-quote-syntax 3) t t)
     (6 (py-quote-syntax 1) t t))))

(custom-add-option 'python-mode-hook 'imenu-add-menubar-index)
(custom-add-option 'python-mode-hook
                   (lambda ()
                     "Turn off Indent Tabs mode."
                     (setq indent-tabs-mode nil)))
(custom-add-option 'python-mode-hook 'turn-on-eldoc-mode)
(custom-add-option 'python-mode-hook 'abbrev-mode)
(custom-add-option 'python-mode-hook 'py-find-imports)

;; IPython Completion start

;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

;; https://github.com/ipython
;; commit 1dd379d857f836c9e8af4576cecaeb413fcba4e5
;; Date:   Tue Feb 14 19:47:04 2012 -0800
;; "print(';'.join(get_ipython().complete('%s', '%s')[1])) #PYTHON-MODE SILENT\n"

;; (setq ipython-complete-function 'py-completion-at-point)

;; (setq py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-")

(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")

(setq py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]?\\([biptj]+ython[^ \t\n]*\\)")

(set-default 'py-python-command-args  '("-i"))
(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)

(put 'py-indent-offset 'safe-local-variable 'integerp)

;; (defvar py-separator-char 47
;;  (setq py-separator-char 47)

;; ipython.el
;; Recognize the ipython pdb, whose prompt is 'ipdb>' or  'ipydb>'
;;instead of '(Pdb)'
(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ ")
(setq py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ ")

;; pydb-328837.diff

;; prevent ipython.el's setting
(setq ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:" )

;; (setq ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:")

;; ipython.el
;; (defvar ipython-de-input-prompt-regexp "\\(?:
;; In \\[[0-9]+\\]: *.*
;; ----+> \\(.*
;; \\)[\n]?\\)\\|\\(?:
;; In \\[[0-9]+\\]: *\\(.*
;; \\)\\)\\|^[ ]\\{3\\}[.]\\{3,\\}: *\\(.*
;; \\)"
;;   "A regular expression to match the IPython input prompt and the python
;; command after it. The first match group is for a command that is rewritten,
;; the second for a 'normal' command, and the third for a multiline command.")

;; (custom-add-option 'python-mode-hook 'python-setup-brm)

;; (defvar python-command py-shell-name)

;; for toggling between CPython and JPython

(setq py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/\\|^__pyfile = open('''\\|^execfile(r'[.+]/tmp/")

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

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
        (when py-underscore-word-syntax-p
          (modify-syntax-entry ?_ "w" table))
        table))

;;; py-expression variables start

;; (setq py-expression-skip-regexp "[^ (=#\t\r\n\f]")

;; (setq py-expression-skip-chars "^ (=#\t\r\n\f")

;; (setq py-expression-looking-re "[^ =#\t\r\n\f)]")

;; (setq py-not-expression-regexp "[ .=#\t\r\n\f)]+")

;; (setq py-not-expression-chars "[ .=#\t\r\n\f)]+")

;; (setq py-partial-expression-skip-chars "^ .(){}=:#\t\r\n\f")

(setq py-partial-expression-forward-regexp "^ .()}=:#\t\r\n\f")

;; (setq py-partial-expression-skip-backward-chars "^ .\"(){}\[]=:#\t\r\n\f")

;; (setq py-not-partial-expression-skip-chars " )\]\\.=:#\t\r\n\f")

;; (setq py-partial-expression-looking-regexp "[^ ).=:#\t\r\n\f]")

;; (setq py-not-partial-expression-regexp "[ .=:#\t\r\n\f)]")

;; (setq py-operator-regexp "[ \t]*\\(+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*")

;; (setq  py-assignment-regexp "[ \t]*=[^=]")

;; (setq py-delimiter-regexp "\\(,\\|;\\:\\)[ \t\n]")

;;;

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;;; Constants

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

;; (setq py-traceback-line-re
;; "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^ \t>]+>[^0-9]+\\([0-9]+\\)\\|^[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)")

;; (setq py-block-or-clause-re "[ \t]*\\_<\\(if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[: \n\t]")

;; (setq py-clause-re "[ \t]*\\_<\\(else\\|elif\\|except\\|finally\\)\\_>[: \n\t]")

;;;

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
     (t (error "Unhandled string: %s" s)))))

;; (defvar py-help-mode-syntax-table
;;   (let ((st (make-syntax-table py-mode-syntax-table)))
;;     ;; Don't get confused by apostrophes in the process's output (e.g. if
;;     ;; you execute "help(os)").
;;     (modify-syntax-entry ?\' "." st)
;;     ;; Maybe we should do the same for double quotes?
;;     (modify-syntax-entry ?\" "." st)
;;     st))
;;
;; (defconst py-space-backslash-table
;;   (let ((table (copy-syntax-table py-mode-syntax-table)))
;;     (modify-syntax-entry ?\\ " " table)
;;     table)
;;   "`py-mode-syntax-table' with backslash given whitespace syntax.")

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :group 'python-mode)

;; ;; Face for None, True, False, self, and Ellipsis
(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis."
  :group 'python-mode)

(defface py-variable-name-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Face method decorators."
  :group 'python-mode)

(defface py-number-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Highlight numbers. "
  :group 'python-mode)

;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :group 'python-mode)

;; Face for builtins
(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python-mode)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :group 'python-mode)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python-mode)

;; have to bind py-file-queue before installing the kill-emacs-hook

(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook nil)

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-choose-shell-by-shebang ()
  "Choose shell by looking at #! on the first line.

Returns the specified Python resp. Jython shell command name. "
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
        (dolist (ele erg)
          (when (string-match "[bijp]+ython" ele)
            (setq res ele)))))
    (when (and py-verbose-p (interactive-p)) (message "%s" res))
    res))

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

(defun py-choose-shell-by-path (&optional file-separator-char)
  "Select Python executable according to version desplayed in path, current buffer-file is selected from.

Returns versioned string, nil if nothing appropriate found "
  (interactive)
  (lexical-let ((path (buffer-file-name))
                (file-separator-char (or file-separator-char (char-to-string py-separator-char)))
                erg)
    (when (and path file-separator-char
               (string-match (concat file-separator-char "[iI]?[pP]ython[0-9.]+" file-separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat file-separator-char "[iI]?[pP]ython[0-9.]+" file-separator-char) path)) (1- (match-end 0)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-which-python ()
  "Returns version of Python of current environment, a number. "
  (interactive)
  (let* ((cmd (py-choose-shell))
         (erg (shell-command-to-string (concat cmd " --version")))
         ;; Result: "bpython version 0.9.7.1 on top of Python 2.7\n(C) 2008-2010 Bob Farrell, Andreas Stuehrk et al. See AUTHORS for detail.\n"

         (version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                         (match-string-no-properties 2 erg))
                        ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                         (substring erg 7 (1- (length erg)))))))
    (when (interactive-p)
      (if erg
          (when py-verbose-p (message "%s" erg))
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-current-environment ()
  "Returns path of current Python installation. "
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (interactive-p)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    erg))

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional arg pyshell dedicated)
  "Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With \\[universal-argument] 4 is called `py-switch-shell' see docu there.
"
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
      (py-switch-shell '(4))
    (let* ((erg (cond (py-force-py-shell-name-p
                       py-shell-name)
                      (py-use-local-default
                       (if (not (string= "" py-shell-local-path))
                           (expand-file-name py-shell-local-path)
                         (message "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'")))
                      ((comint-check-proc (current-buffer))
                       (process-name (get-buffer-process (current-buffer))))
                      ((py-choose-shell-by-shebang))
                      ((py-choose-shell-by-import))
                      ((py-choose-shell-by-path))
                      (py-shell-name py-shell-name)
                      (t (default-value 'py-shell-name))))
           (cmd (if py-edit-only-p erg
                  (executable-find erg))))
      (if cmd
          (when (interactive-p)
            (message "%s" cmd))
        (when (interactive-p) (message "%s" "Could not detect Python on your system. Maybe set `py-edit-only-p'?")))
      erg)))


(defun py-normalize-directory (directory &optional file-separator-char)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (let* ((file-separator-char (or file-separator-char (char-to-string py-separator-char)))
         (erg (cond ((string-match (concat file-separator-char "$") directory)
                     directory)
                    ((not (string= "" directory))
                     (concat directory file-separator-char)))))
    (unless erg (when py-verbose-p (message "Warning: directory is empty")))
    erg))

(defun py-install-directory-check ()
  "Do some sanity check for `py-install-directory'.

Returns `t' if successful. "
  (interactive)
  (let ((erg (and (boundp 'py-install-directory) (stringp py-install-directory) (< 1 (length py-install-directory)))))
    (when (interactive-p) (message "py-install-directory-check: %s" erg))
    erg))

(defun py-guess-py-install-directory ()
  "Takes value of user directory aka $HOME
if `(locate-library \"python-mode\")' is not succesful.

Used only, if `py-install-directory' is empty. "
  (interactive)
  (let ((erg (file-name-directory (locate-library "python-mode"))))
    (if erg
        (setq py-install-directory erg)
      (setq py-install-directory (expand-file-name "~/")))
    (when (and py-verbose-p (interactive-p)) (message "Setting py-install-directory to: %s" py-install-directory))
    py-install-directory))

(defun py-load-pymacs ()
  "Load Pymacs as delivered with python-mode.el.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (let ((pyshell (py-choose-shell))
        (path (getenv "PYTHONPATH"))
        (py-install-directory (cond ((string= "" py-install-directory)
                                     (py-guess-py-install-directory))
                                    (t (py-normalize-directory py-install-directory)))))
    ;; Python side
    ;; If Pymacs has not been loaded before, prepend py-install-directory to
    ;; PYTHONPATH, so that the Pymacs delivered with python-mode is used.
    (setenv "PYTHONPATH" (concat
                          (unless (featurep 'pymacs)
                            (concat (expand-file-name py-install-directory)
                                    path-separator))
                          (expand-file-name py-install-directory) "completion"
                          (if path (concat path-separator path))))

    (if (py-install-directory-check)
        (progn
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (require 'pymacs)
          (add-to-list 'load-path (concat (expand-file-name py-install-directory) "completion"))
          (require 'pycomplete)
          (add-hook 'python-mode-hook 'py-complete-initialize))
      (error "`py-install-directory' not set, see INSTALL"))))

(when py-load-pymacs-p (py-load-pymacs))

(defun py-set-load-path ()
  "Include needed subdirs of python-mode directory. "
  (interactive)
  (let ((py-install-directory (py-normalize-directory py-install-directory (char-to-string py-separator-char))))
    (cond ((and (not (string= "" py-install-directory))(stringp py-install-directory))
           (add-to-list 'load-path (expand-file-name py-install-directory))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "completion"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "extensions"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "test"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "tools"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "autopair"))
           )
          ((when py-guess-py-install-directory-p
             (let ((guessed-py-install-directory (py-guess-py-install-directory)))
               (when guessed-py-install-directory
                 (add-to-list 'load-path guessed-py-install-directory)))))
          (t (error "Please set `py-install-directory', see INSTALL"))
          (when (interactive-p) (message "%s" load-path)))))

;; (when (boundp 'py-install-directory) (py-set-load-path))
(py-set-load-path)

(require 'python-components-edit)
(require 'python-components-intern)
(require 'python-components-move)
(require 'python-components-execute)
(require 'python-components-send)
(require 'python-components-pdb)
(require 'python-components-help)
(require 'python-components-extensions)
;; (require 'thingatpt-python-expressions)
(require 'python-components-imenu)
;; (require 'python-components-completion)
(require 'python-components-named-shells)
(require 'python-components-shell-complete)
(require 'python-components-electric)
(require 'virtualenv)
;;(require 'components-shell-completion)
(require 'python-components-skeletons)
(require 'python-components-re-forms)
(require 'python-components-up-down)
(require 'python-components-bol-forms)
(require 'python-components-exec-forms)
(require 'python-extended-executes)
;; (require 'python-mode-test)
(require 'column-marker)
(require 'feg-python-el-extracts)
(require 'python-abbrev-propose)
(require 'py-smart-operator)
(require 'python-extended-executes-test)
(require 'python-components-switches)
(require 'python-components-paragraph)
(require 'python-components-shift-forms)

;;; Python specialized rx, thanks Fabian F. Gallina
(eval-when-compile
  (defconst python-rx-constituents
    `((block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|="))))
    "Additional Python specific sexps for `python-rx'"))

(defmacro python-rx (&rest regexps)
  "Python mode specialized rx macro which supports common python named REGEXPS."
  (let ((rx-constituents (append python-rx-constituents rx-constituents)))
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (rx-to-string `(and ,@regexps) t))
          (t
           (rx-to-string (car regexps) t)))))


;;; Font-lock and syntax
(setq python-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
                 "assert" "else" "if" "pass" "yield" "break" "import"
                 "print" "exec" "in" "continue" "finally" "is" "except" "raise"
                 "return" "def" "for" "lambda" "try")
             symbol-end)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-keyword-face) (2 py-class-name-face))
        ;; (,(rx symbol-start
        ;; (or "raise" "except")
        ;; symbol-end) . py-exception-name-face)
        ;; already pseudo-keyword
        ;; (,(rx symbol-start
        ;;       (or "None" "True" "False" "__debug__" "NotImplemented")
        ;;       symbol-end) . font-lock-constant-face)
        (,(rx symbol-start
              (or "cls" "self" "cls" "Ellipsis" "True" "False" "None"  "__debug__" "NotImplemented")
              symbol-end) . py-pseudo-keyword-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 py-decorators-face))
        ;; Builtin Exceptions
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
                  "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
              word-end) . py-exception-name-face)
        ;; (,(rx (or space line-start) symbol-start "range
        ;; Builtins
        (,(rx (or space line-start) symbol-start
              (or "_" "__doc__" "__import__" "__name__" "__package__" "abs" "all"
                  "any" "apply" "basestring" "bin" "bool" "buffer" "bytearray"
                  "bytes" "callable" "chr" "classmethod" "cmp" "coerce" "compile"
                  "complex" "delattr" "dict" "dir" "divmod" "enumerate" "eval"
                  "execfile" "file" "filter" "float" "format" "frozenset"
                  "getattr" "globals" "hasattr" "hash" "help" "hex" "id" "input"
                  "int" "intern" "isinstance" "issubclass" "iter" "len" "list"
                  "locals" "long" "map" "max" "min" "next" "object" "oct" "open"
                  "ord" "pow" "print" "property" "range" "raw_input" "reduce"
                  "reload" "repr" "reversed" "round" "set" "setattr" "slice"
                  "sorted" "staticmethod" "str" "sum" "super" "tuple" "type"
                  "unichr" "unicode" "vars" "xrange" "zip")
              symbol-end) . py-builtins-face)
        ;; (,(python-rx line-start (* (any " \t"))(group (** 0 2 "_") word (0+ (or word ?_))(** 0 2 "_"))(* (any " \t")) assignment-operator)
        ;; 1 py-variable-name-face)
        ;; asignations
        ;; support for a = b = c = 5
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_)))
                                 (? ?\[ (+ (not (any ?\]))) ?\]) (* space)
                                 assignment-operator)))
              (when (re-search-forward re limit t)
                (while (and (py-info-ppss-context 'paren)
                            (re-search-forward re limit t)))
                (if (and (not (py-info-ppss-context 'paren))
                         (not (equal (char-after (point-marker)) ?=)))
                    t
                  (set-match-data nil)))))
         (1 py-variable-name-face nil nil))
        ;; support for a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                                 (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                 ?, (* space) (+ (any word ?. ?_)) (* space)
                                 assignment-operator)))
              (when (and (re-search-forward re limit t)
                         (goto-char (nth 3 (match-data))))
                (while (and (py-info-ppss-context 'paren)
                            (re-search-forward re limit t))
                  (goto-char (nth 3 (match-data))))
                (if (not (py-info-ppss-context 'paren))
                    t
                  (set-match-data nil)))))
         (1 py-variable-name-face nil nil))
        ;; (,(rx (or space line-start) symbol-start "range" symbol-end) . py-builtins-face)
        ;; Numbers
        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)))

;; (defconst py-font-lock-syntactic-keywords
;;   ;; Make outer chars of matching triple-quote sequences into generic
;;   ;; string delimiters.  Fixme: Is there a better way?
;;   ;; First avoid a sequence preceded by an odd number of backslashes.
;;   `((,(concat "\\(?:\\([RUru]\\)[Rr]?\\|^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
;;               "\\(?:\\('\\)'\\('\\)\\|\\(?2:\"\\)\"\\(?3:\"\\)\\)")
;;      (3 (py-quote-syntax)))))

(defun py-quote-syntax (n)
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

;; (defun py-quote-syntax ()
;;   "Put `syntax-table' property correctly on triple quote.
;; Used for syntactic keywords.  N is the match number (1, 2 or 3)."
;;   ;; Given a triple quote, we have to check the context to know
;;   ;; whether this is an opening or closing triple or whether it's
;;   ;; quoted anyhow, and should be ignored.  (For that we need to do
;;   ;; the same job as `syntax-ppss' to be correct and it seems to be OK
;;   ;; to use it here despite initial worries.)  We also have to sort
;;   ;; out a possible prefix -- well, we don't _have_ to, but I think it
;;   ;; should be treated as part of the string.
;;
;;   ;; Test cases:
;;   ;;  ur"""ar""" x='"' # """
;;   ;; x = ''' """ ' a
;;   ;; '''
;;   ;; x '"""' x """ \"""" x
;;   (save-excursion
;;     (goto-char (match-beginning 0))
;;     (let ((syntax (save-match-data (syntax-ppss))))
;;       (cond
;;        ((eq t (nth 3 syntax))           ; after unclosed fence
;;         ;; Consider property for the last char if in a fenced string.
;;         (goto-char (nth 8 syntax))	; fence position
;;         (skip-chars-forward "uUrR")	; skip any prefix
;;         ;; Is it a matching sequence?
;;         (if (eq (char-after) (char-after (match-beginning 2)))
;;             (put-text-property (match-beginning 3) (match-end 3)
;;                                'syntax-table (string-to-syntax "|"))))
;;        ((match-end 1)
;;         ;; Consider property for initial char, accounting for prefixes.
;;         (put-text-property (match-beginning 1) (match-end 1)
;;                            'syntax-table (string-to-syntax "|")))
;;        (t
;;         ;; Consider property for initial char, accounting for prefixes.
;;         (put-text-property (match-beginning 2) (match-end 2)
;;                            'syntax-table (string-to-syntax "|"))))
;;       )))

(defun py-info-ppss-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be 'comment, 'string or 'paren.  It returns the start
character address of the specified TYPE."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond ((eq type 'comment)
           (and (nth 4 ppss)
                (nth 8 ppss)))
          ((eq type 'string)
           (nth 8 ppss))
          ((eq type 'paren)
           (nth 1 ppss))
          (t nil))))

;; ;; Credits to github.com/fgallina/python.el/issues42
;; (defvar font-lock-number "[0-9]+\\([eE][+-]?[0-9]*\\)?")
;; (defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
;;

;;
;; (defconst py-font-lock-syntactic-keywords
;;   ;; Make outer chars of matching triple-quote sequences into generic
;;   ;; string delimiters.  Fixme: Is there a better way?
;;   ;; First avoid a sequence preceded by an odd number of backslashes.
;;   `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
;;               "\\(?:\\('\\)\\('\\)\\('\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\)")
;;      (1 (python-quote-syntax 1) nil lax)
;;      (2 (python-quote-syntax 2))
;;      (3 (python-quote-syntax 3)))
;;     ;; This doesn't really help.
;; ;;;     (,(rx (and ?\\ (group ?\n))) (1 " "))
;;     ))
;;

;; An auxiliary syntax table which places underscore and dot in the
;; symbol class for simplicity


;;; Keymap
(defvar python-mode-map)
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; moving point
        (define-key map [(control c)(control p)] 'py-beginning-of-statement)
        (define-key map [(control c)(control n)] 'py-end-of-statement)
        (define-key map [(control c)(control u)] 'py-beginning-of-block)
        (define-key map [(control c)(control q)] 'py-end-of-block)
        (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
        (define-key map [(control meta e)] 'py-end-of-def-or-class)

        ;; (define-key map [(meta i)] 'py-indent-forward-line)
        (define-key map [(control j)] 'py-newline-and-indent)
        ;; Most Pythoneers expect RET `py-newline-and-indent'
        ;; (define-key map (kbd "RET") 'py-newline-and-dedent)
        (define-key map (kbd "RET") 'py-newline-and-indent)
        ;; (define-key map (kbd "RET") 'newline)
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
        (define-key map [(control c)(control t)] 'py-toggle-shell)
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
        (define-key map [(control c)(.)] 'py-expression)
        ;; Miscellaneous
        (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c)(control f)] 'py-sort-imports)
        (define-key map [(control c)(\#)] 'py-comment-region)
        (define-key map [(control c)(\?)] 'py-describe-mode)
        (define-key map [(control c)(control e)] 'py-describe-symbol)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
        (define-key map (kbd "TAB") 'py-indent-line)
        ;; (if py-complete-function
        ;;     (progn
        ;;       (define-key map [(meta tab)] py-complete-function)
        ;;       (define-key map [(esc) (tab)] py-complete-function))
        ;;   (define-key map [(meta tab)] 'py-shell-complete)
        ;;   (define-key map [(esc) (tab)] 'py-shell-complete))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (easy-menu-define py-menu map "Python Tools"
          `("PyTools"
            :help "Python mode tools"

            ["Customize Python mode" (customize-group 'python-mode)
             :help "Open the customization buffer for Python mode"]

            "-"

            ["pychecker-run" py-pychecker-run
             :help "`py-pychecker-run'
Run pychecker"]

            ("Pylint ... "
             :help "Extendet report options
call `easy_install pylint' if not available"

             ["pylint-run" py-pylint-run
              :help "`pylint-run'
Pylint will display a number of messages as it analyzes the code,
as well as some statistics about the number of warnings and
errors found in different files - unless called with arg \"--errors-only\". The messages are classified
under various categories such as errors and warnings

Pylint checks length of lines of code, if variable names are
well-formed according to your coding standard, if declared
interfaces are truly implemented, and much more. Additionally, it
is possible to write plugins.

call `easy_install pylint' if not available
"]

             ["pylint-help" pylint-help
              :help "`pylint-help'
List extendet report options
"]
             ["pylint-flymake-mode" pylint-flymake-mode
              :help "`pylint-flymake-mode'
Toggle flymake-mode running `pylint'
"])

            ("pep8 ... "
             :help "Check formatting
call `easy_install pep8' if not available"

             ["pep8-run" py-pep8-run
              :help "`py-pep8-run'
Check formatting (default on the file currently visited)
call `easy_install pep8' if not available
"]

             ["pep8-help" py-pep8-help
              :help "`py-pep8-help'
Display help for pep8 format checker)
"]

             ["pep8-flymake-mode" pep8-flymake-mode
              :help "`pep8-flymake-mode'
Toggle flymake-mode running `pep8'
"])

            ("Pyflakes ... " :help "Non intrusive code
             checker call `easy_install pyflakes' if
             not available"

             ["pyflakes-run" py-pyflakes-run :help
              "`py-pyflakes-run' Run pyflakes call
              `easy_install pyflakes' if not
              available"]

             ["pyflakes-help" py-pyflakes-help :help
              "`py-pyflakes-help' Display help for
              Pyflakes "]

             ["pyflakes-flymake-mode" pyflakes-flymake-mode :help
              "`pyflakes-flymake-mode'
Toggle flymake-mode running `pyflakes' "])

            ("Pyflakes-pep8 ... " :help
             "Non intrusive code checker running `pyflakes' and `pep8'
call `easy_install pyflakes' and `easy_install pep8' if basics not available"

             ["pyflakespep8-run" py-pyflakespep8-run :help
              "`py-pyflakespep8-run' Run `pyflakespep8'
call `easy_install pyflakes' if not available"]

             ["pyflakespep8-help" py-pyflakespep8-help :help
              "`py-pyflakespep8-help' Display help for
              Pyflakespep8 "]

             ["pyflakespep8-flymake-mode" pyflakespep8-flymake-mode :help
              "`pyflakespep8-flymake-mode'
Toggle flymake-mode running `pyflakespep8' "])

            "-"
            ("Abbrevs"
             :help "see also `py-add-abbrev'"
             :filter (lambda (&rest junk)
                       (abbrev-table-menu python-mode-abbrev-table)))
            ["add-abbrev" py-add-abbrev
             :help "Defines python-mode specific abbrev for last expressions before point.
Argument is how many `py-partial-expression's form the expansion; or zero means the region is the expansion. "]

            ("Skeletons"
             :help "See also templates in YASnippet"

             ["if" py-if
              :help "Inserts if-statement"]
             ["py-else" py-else
              :help "Inserts else-statement"]
             ["py-while" py-while
              :help "Inserts while-statement"]
             ["py-for" py-for
              :help "Inserts for-statement"]
             ["py-try/finally" py-try/finally
              :help "Inserts py-try/finally-statement"]
             ["py-try/except" py-try/except
              :help "Inserts py-try/except-statement"])

            "-"

            ["Import/reload file" py-execute-import-or-reload
             :help "`py-execute-import-or-reload'
Load into inferior Python session"]

            ["Debugger" pdb
             :help "`pdb'
Run pdb under GUD"]
            "-"

            ("Modes"
             :help "Toggle useful modes like `highlight-indentation'"

             ["Toggle highlight-indentation" py-toggle-highlight-indentation
              :help "M-x `highlight-indentation' switches this minor mode "]

             ["Highlight-indentation on" highlight-indentation-on
              :help "M-x `highlight-indentation-on' switches this minor mode on "]

             ["Highlight-indentation off" highlight-indentation-off
              :help "M-x `highlight-indentation-off' switches this minor mode off "]
             "-"

             ["Toggle autopair-mode" py-toggle-autopair-mode
              :help "Toggles py-autopair minor-mode "]

             ["Autopair on" py-autopair-mode-on
              :help "Switches autopair minor-mode on "]

             "-"

             ["Toggle py-smart-indentation" toggle-py-smart-indentation
              :help "Toggles py-smart-indentation minor-mode "]

             ["Py-smart-indentation on" py-smart-indentation-mode-on
              :help "Switches py-smart-indentation minor-mode on "]

             ["Toggle py-smart-indentation" py-toggle-smart-indentation
              :help "Toggles py-smart-indentation minor-mode off"]

             "-"

             ["Toggle py-smart-operator" py-toggle-smart-operator
              :help "Toggles py-smart-operator minor-mode"]

             ["Py-smart-operator off" py-smart-operator-mode-off
              :help "Switches py-smart-operator minor-mode off "]

             ["Py-smart-operator on" py-smart-operator-mode-on
              :help "Switches py-smart-operator minor-mode on "]

             "-"

             ["Toggle indent-tabs-mode" py-toggle-indent-tabs-mode
              :help "See also `py-indent-tabs-mode-on', `-off' "]

             ["Switch indent-tabs-mode on" py-indent-tabs-mode-on
              :help "`py-indent-tabs-mode-on'"]

             ["Switch indent-tabs-mode off" py-indent-tabs-mode-off
              :help "`py-indent-tabs-mode-off'"])

            ["Help on symbol" py-describe-symbol
             :help "`py-describe-symbol'
Use pydoc on symbol at point"]
            ["Complete symbol" py-shell-complete
             :help "`py-shell-complete'
Complete (qualified) symbol before point"]
            ["Find function" py-find-function
             :help "`py-find-function'
Try to find source definition of function at point"]

            ["Switch index-function" py-switch-imenu-index-function
             :help "`py-switch-imenu-index-function'
Switch between `py-imenu-create-index' from 5.1 series and `py-imenu-create-index-new'."]

            ["Update imports" py-update-imports
             :help "`py-update-imports'
Update list of top-level imports for completion"]
            "-"
            ))
        ;; Menu py-execute forms
        (easy-menu-define py-menu map "Execute Python"
          `("PyExec"
            :help "Python-specific features"

            ["Execute statement" py-execute-statement
             :help "`py-execute-statement'
       Send statement at point to Python interpreter. "]

            ["Execute block" py-execute-block
             :help "`py-execute-block'
       Send block at point to Python interpreter. "]

            ["Execute def" py-execute-def
             :help "`py-execute-def'
       Send def at point to Python interpreter. "]

            ["Execute class" py-execute-class
             :help "`py-execute-class'
       Send class at point to Python interpreter. "]

            ["Execute region" py-execute-region
             :help "`py-execute-region'
       Send region at point to Python interpreter. "]

            ["Execute file" py-execute-file
             :help "`py-execute-file'
       Send file at point to Python interpreter. "]
            ;; statement

            ("Execute statement ... "
            :help "Execute statement functions"

            ["py-execute-statement-python" py-execute-statement-python
            :help "Execute statement through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-statement-ipython" py-execute-statement-ipython
            :help "Execute statement through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-statement-python3" py-execute-statement-python3
            :help "Execute statement through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-statement-python2" py-execute-statement-python2
            :help "Execute statement through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-statement-python2.7" py-execute-statement-python2.7
            :help "Execute statement through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-statement-jython" py-execute-statement-jython
            :help "Execute statement through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-statement-python3.2" py-execute-statement-python3.2
            :help "Execute statement through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-statement-python3.3" py-execute-statement-python3.3
            :help "Execute statement through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-statement-bpython" py-execute-statement-bpython
            :help "Execute statement through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-statement-python-dedicated" py-execute-statement-python-dedicated
:help "Execute statement through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-ipython-dedicated" py-execute-statement-ipython-dedicated
:help "Execute statement through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-python3-dedicated" py-execute-statement-python3-dedicated
:help "Execute statement through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-python2-dedicated" py-execute-statement-python2-dedicated
:help "Execute statement through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-python2.7-dedicated" py-execute-statement-python2.7-dedicated
:help "Execute statement through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-jython-dedicated" py-execute-statement-jython-dedicated
:help "Execute statement through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-python3.2-dedicated" py-execute-statement-python3.2-dedicated
:help "Execute statement through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-python3.3-dedicated" py-execute-statement-python3.3-dedicated
:help "Execute statement through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-statement-bpython-dedicated" py-execute-statement-bpython-dedicated
:help "Execute statement through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-statement-python-switch" py-execute-statement-python-switch
:help "Execute statement through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-statement-ipython-switch" py-execute-statement-ipython-switch
:help "Execute statement through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-statement-python3-switch" py-execute-statement-python3-switch
:help "Execute statement through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-statement-python2-switch" py-execute-statement-python2-switch
:help "Execute statement through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-statement-python2.7-switch" py-execute-statement-python2.7-switch
:help "Execute statement through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-statement-jython-switch" py-execute-statement-jython-switch
:help "Execute statement through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-statement-python3.2-switch" py-execute-statement-python3.2-switch
:help "Execute statement through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-statement-python3.3-switch" py-execute-statement-python3.3-switch
:help "Execute statement through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-statement-bpython-switch" py-execute-statement-bpython-switch
:help "Execute statement through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-statement-python-dedicated-switch" py-execute-statement-python-dedicated-switch
:help "Execute statement through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-ipython-dedicated-switch" py-execute-statement-ipython-dedicated-switch
:help "Execute statement through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-python3-dedicated-switch" py-execute-statement-python3-dedicated-switch
:help "Execute statement through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-python2-dedicated-switch" py-execute-statement-python2-dedicated-switch
:help "Execute statement through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-python2.7-dedicated-switch" py-execute-statement-python2.7-dedicated-switch
:help "Execute statement through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-jython-dedicated-switch" py-execute-statement-jython-dedicated-switch
:help "Execute statement through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-python3.2-dedicated-switch" py-execute-statement-python3.2-dedicated-switch
:help "Execute statement through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-python3.3-dedicated-switch" py-execute-statement-python3.3-dedicated-switch
:help "Execute statement through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-statement-bpython-dedicated-switch" py-execute-statement-bpython-dedicated-switch
:help "Execute statement through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))            ;; block

            ("Execute block ... "
            :help "Execute block functions"

            ["py-execute-block-python" py-execute-block-python
            :help "Execute block through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-block-ipython" py-execute-block-ipython
            :help "Execute block through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-block-python3" py-execute-block-python3
            :help "Execute block through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-block-python2" py-execute-block-python2
            :help "Execute block through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-block-python2.7" py-execute-block-python2.7
            :help "Execute block through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-block-jython" py-execute-block-jython
            :help "Execute block through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-block-python3.2" py-execute-block-python3.2
            :help "Execute block through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-block-python3.3" py-execute-block-python3.3
            :help "Execute block through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-block-bpython" py-execute-block-bpython
            :help "Execute block through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-block-python-dedicated" py-execute-block-python-dedicated
:help "Execute block through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-ipython-dedicated" py-execute-block-ipython-dedicated
:help "Execute block through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-python3-dedicated" py-execute-block-python3-dedicated
:help "Execute block through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-python2-dedicated" py-execute-block-python2-dedicated
:help "Execute block through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-python2.7-dedicated" py-execute-block-python2.7-dedicated
:help "Execute block through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-jython-dedicated" py-execute-block-jython-dedicated
:help "Execute block through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-python3.2-dedicated" py-execute-block-python3.2-dedicated
:help "Execute block through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-python3.3-dedicated" py-execute-block-python3.3-dedicated
:help "Execute block through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-block-bpython-dedicated" py-execute-block-bpython-dedicated
:help "Execute block through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-block-python-switch" py-execute-block-python-switch
:help "Execute block through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-block-ipython-switch" py-execute-block-ipython-switch
:help "Execute block through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-block-python3-switch" py-execute-block-python3-switch
:help "Execute block through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-block-python2-switch" py-execute-block-python2-switch
:help "Execute block through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-block-python2.7-switch" py-execute-block-python2.7-switch
:help "Execute block through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-block-jython-switch" py-execute-block-jython-switch
:help "Execute block through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-block-python3.2-switch" py-execute-block-python3.2-switch
:help "Execute block through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-block-python3.3-switch" py-execute-block-python3.3-switch
:help "Execute block through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-block-bpython-switch" py-execute-block-bpython-switch
:help "Execute block through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-block-python-dedicated-switch" py-execute-block-python-dedicated-switch
:help "Execute block through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-ipython-dedicated-switch" py-execute-block-ipython-dedicated-switch
:help "Execute block through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-python3-dedicated-switch" py-execute-block-python3-dedicated-switch
:help "Execute block through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-python2-dedicated-switch" py-execute-block-python2-dedicated-switch
:help "Execute block through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-python2.7-dedicated-switch" py-execute-block-python2.7-dedicated-switch
:help "Execute block through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-jython-dedicated-switch" py-execute-block-jython-dedicated-switch
:help "Execute block through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-python3.2-dedicated-switch" py-execute-block-python3.2-dedicated-switch
:help "Execute block through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-python3.3-dedicated-switch" py-execute-block-python3.3-dedicated-switch
:help "Execute block through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-block-bpython-dedicated-switch" py-execute-block-bpython-dedicated-switch
:help "Execute block through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))            ;; def

            ("Execute def ... "
            :help "Execute def functions"

            ["py-execute-def-python" py-execute-def-python
            :help "Execute def through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-def-ipython" py-execute-def-ipython
            :help "Execute def through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-def-python3" py-execute-def-python3
            :help "Execute def through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-def-python2" py-execute-def-python2
            :help "Execute def through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-def-python2.7" py-execute-def-python2.7
            :help "Execute def through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-def-jython" py-execute-def-jython
            :help "Execute def through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-def-python3.2" py-execute-def-python3.2
            :help "Execute def through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-def-python3.3" py-execute-def-python3.3
            :help "Execute def through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-def-bpython" py-execute-def-bpython
            :help "Execute def through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-def-python-dedicated" py-execute-def-python-dedicated
:help "Execute def through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-ipython-dedicated" py-execute-def-ipython-dedicated
:help "Execute def through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-python3-dedicated" py-execute-def-python3-dedicated
:help "Execute def through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-python2-dedicated" py-execute-def-python2-dedicated
:help "Execute def through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-python2.7-dedicated" py-execute-def-python2.7-dedicated
:help "Execute def through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-jython-dedicated" py-execute-def-jython-dedicated
:help "Execute def through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-python3.2-dedicated" py-execute-def-python3.2-dedicated
:help "Execute def through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-python3.3-dedicated" py-execute-def-python3.3-dedicated
:help "Execute def through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-def-bpython-dedicated" py-execute-def-bpython-dedicated
:help "Execute def through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-def-python-switch" py-execute-def-python-switch
:help "Execute def through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-def-ipython-switch" py-execute-def-ipython-switch
:help "Execute def through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-def-python3-switch" py-execute-def-python3-switch
:help "Execute def through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-def-python2-switch" py-execute-def-python2-switch
:help "Execute def through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-def-python2.7-switch" py-execute-def-python2.7-switch
:help "Execute def through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-def-jython-switch" py-execute-def-jython-switch
:help "Execute def through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-def-python3.2-switch" py-execute-def-python3.2-switch
:help "Execute def through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-def-python3.3-switch" py-execute-def-python3.3-switch
:help "Execute def through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-def-bpython-switch" py-execute-def-bpython-switch
:help "Execute def through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-def-python-dedicated-switch" py-execute-def-python-dedicated-switch
:help "Execute def through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-ipython-dedicated-switch" py-execute-def-ipython-dedicated-switch
:help "Execute def through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-python3-dedicated-switch" py-execute-def-python3-dedicated-switch
:help "Execute def through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-python2-dedicated-switch" py-execute-def-python2-dedicated-switch
:help "Execute def through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-python2.7-dedicated-switch" py-execute-def-python2.7-dedicated-switch
:help "Execute def through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-jython-dedicated-switch" py-execute-def-jython-dedicated-switch
:help "Execute def through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-python3.2-dedicated-switch" py-execute-def-python3.2-dedicated-switch
:help "Execute def through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-python3.3-dedicated-switch" py-execute-def-python3.3-dedicated-switch
:help "Execute def through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-def-bpython-dedicated-switch" py-execute-def-bpython-dedicated-switch
:help "Execute def through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))            ;; class

            ("Execute class ... "
            :help "Execute class functions"

            ["py-execute-class-python" py-execute-class-python
            :help "Execute class through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-class-ipython" py-execute-class-ipython
            :help "Execute class through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-class-python3" py-execute-class-python3
            :help "Execute class through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-class-python2" py-execute-class-python2
            :help "Execute class through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-class-python2.7" py-execute-class-python2.7
            :help "Execute class through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-class-jython" py-execute-class-jython
            :help "Execute class through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-class-python3.2" py-execute-class-python3.2
            :help "Execute class through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-class-python3.3" py-execute-class-python3.3
            :help "Execute class through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-class-bpython" py-execute-class-bpython
            :help "Execute class through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-class-python-dedicated" py-execute-class-python-dedicated
:help "Execute class through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-ipython-dedicated" py-execute-class-ipython-dedicated
:help "Execute class through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-python3-dedicated" py-execute-class-python3-dedicated
:help "Execute class through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-python2-dedicated" py-execute-class-python2-dedicated
:help "Execute class through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-python2.7-dedicated" py-execute-class-python2.7-dedicated
:help "Execute class through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-jython-dedicated" py-execute-class-jython-dedicated
:help "Execute class through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-python3.2-dedicated" py-execute-class-python3.2-dedicated
:help "Execute class through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-python3.3-dedicated" py-execute-class-python3.3-dedicated
:help "Execute class through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-class-bpython-dedicated" py-execute-class-bpython-dedicated
:help "Execute class through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-class-python-switch" py-execute-class-python-switch
:help "Execute class through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-class-ipython-switch" py-execute-class-ipython-switch
:help "Execute class through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-class-python3-switch" py-execute-class-python3-switch
:help "Execute class through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-class-python2-switch" py-execute-class-python2-switch
:help "Execute class through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-class-python2.7-switch" py-execute-class-python2.7-switch
:help "Execute class through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-class-jython-switch" py-execute-class-jython-switch
:help "Execute class through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-class-python3.2-switch" py-execute-class-python3.2-switch
:help "Execute class through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-class-python3.3-switch" py-execute-class-python3.3-switch
:help "Execute class through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-class-bpython-switch" py-execute-class-bpython-switch
:help "Execute class through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-class-python-dedicated-switch" py-execute-class-python-dedicated-switch
:help "Execute class through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-ipython-dedicated-switch" py-execute-class-ipython-dedicated-switch
:help "Execute class through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-python3-dedicated-switch" py-execute-class-python3-dedicated-switch
:help "Execute class through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-python2-dedicated-switch" py-execute-class-python2-dedicated-switch
:help "Execute class through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-python2.7-dedicated-switch" py-execute-class-python2.7-dedicated-switch
:help "Execute class through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-jython-dedicated-switch" py-execute-class-jython-dedicated-switch
:help "Execute class through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-python3.2-dedicated-switch" py-execute-class-python3.2-dedicated-switch
:help "Execute class through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-python3.3-dedicated-switch" py-execute-class-python3.3-dedicated-switch
:help "Execute class through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-class-bpython-dedicated-switch" py-execute-class-bpython-dedicated-switch
:help "Execute class through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))            ;; region

            ("Execute region ... "
            :help "Execute region functions"

            ["py-execute-region-python" py-execute-region-python
            :help "Execute region through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-region-ipython" py-execute-region-ipython
            :help "Execute region through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-region-python3" py-execute-region-python3
            :help "Execute region through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-region-python2" py-execute-region-python2
            :help "Execute region through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-region-python2.7" py-execute-region-python2.7
            :help "Execute region through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-region-jython" py-execute-region-jython
            :help "Execute region through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-region-python3.2" py-execute-region-python3.2
            :help "Execute region through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-region-python3.3" py-execute-region-python3.3
            :help "Execute region through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-region-bpython" py-execute-region-bpython
            :help "Execute region through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-region-python-dedicated" py-execute-region-python-dedicated
:help "Execute region through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-ipython-dedicated" py-execute-region-ipython-dedicated
:help "Execute region through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-python3-dedicated" py-execute-region-python3-dedicated
:help "Execute region through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-python2-dedicated" py-execute-region-python2-dedicated
:help "Execute region through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-python2.7-dedicated" py-execute-region-python2.7-dedicated
:help "Execute region through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-jython-dedicated" py-execute-region-jython-dedicated
:help "Execute region through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-python3.2-dedicated" py-execute-region-python3.2-dedicated
:help "Execute region through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-python3.3-dedicated" py-execute-region-python3.3-dedicated
:help "Execute region through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-region-bpython-dedicated" py-execute-region-bpython-dedicated
:help "Execute region through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-region-python-switch" py-execute-region-python-switch
:help "Execute region through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-region-ipython-switch" py-execute-region-ipython-switch
:help "Execute region through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-region-python3-switch" py-execute-region-python3-switch
:help "Execute region through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-region-python2-switch" py-execute-region-python2-switch
:help "Execute region through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-region-python2.7-switch" py-execute-region-python2.7-switch
:help "Execute region through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-region-jython-switch" py-execute-region-jython-switch
:help "Execute region through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-region-python3.2-switch" py-execute-region-python3.2-switch
:help "Execute region through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-region-python3.3-switch" py-execute-region-python3.3-switch
:help "Execute region through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-region-bpython-switch" py-execute-region-bpython-switch
:help "Execute region through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-region-python-dedicated-switch" py-execute-region-python-dedicated-switch
:help "Execute region through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-ipython-dedicated-switch" py-execute-region-ipython-dedicated-switch
:help "Execute region through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-python3-dedicated-switch" py-execute-region-python3-dedicated-switch
:help "Execute region through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-python2-dedicated-switch" py-execute-region-python2-dedicated-switch
:help "Execute region through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-python2.7-dedicated-switch" py-execute-region-python2.7-dedicated-switch
:help "Execute region through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-jython-dedicated-switch" py-execute-region-jython-dedicated-switch
:help "Execute region through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-python3.2-dedicated-switch" py-execute-region-python3.2-dedicated-switch
:help "Execute region through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-python3.3-dedicated-switch" py-execute-region-python3.3-dedicated-switch
:help "Execute region through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-region-bpython-dedicated-switch" py-execute-region-bpython-dedicated-switch
:help "Execute region through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))            ;; file

            ("Execute file ... "
            :help "Execute file functions"

            ["py-execute-file-python" py-execute-file-python
            :help "Execute file through a Python interpreter.
        With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-file-ipython" py-execute-file-ipython
            :help "Execute file through an IPython interpreter.
        With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-file-python3" py-execute-file-python3
            :help "Execute file through a Python3 interpreter.
        With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-file-python2" py-execute-file-python2
            :help "Execute file through a Python2 interpreter.
        With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-file-python2.7" py-execute-file-python2.7
            :help "Execute file through a Python2.7 interpreter.
        With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-file-jython" py-execute-file-jython
            :help "Execute file through a Jython interpreter.
        With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-file-python3.2" py-execute-file-python3.2
            :help "Execute file through a Python3.2 interpreter.
        With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-file-python3.3" py-execute-file-python3.3
            :help "Execute file through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-file-bpython" py-execute-file-bpython
            :help "Execute file through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated

            ["py-execute-file-python-dedicated" py-execute-file-python-dedicated
:help "Execute file through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-ipython-dedicated" py-execute-file-ipython-dedicated
:help "Execute file through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-python3-dedicated" py-execute-file-python3-dedicated
:help "Execute file through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-python2-dedicated" py-execute-file-python2-dedicated
:help "Execute file through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-python2.7-dedicated" py-execute-file-python2.7-dedicated
:help "Execute file through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-jython-dedicated" py-execute-file-jython-dedicated
:help "Execute file through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-python3.2-dedicated" py-execute-file-python3.2-dedicated
:help "Execute file through a unique Python3.2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-python3.3-dedicated" py-execute-file-python3.3-dedicated
:help "Execute file through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]

            ["py-execute-file-bpython-dedicated" py-execute-file-bpython-dedicated
:help "Execute file through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-shell-switch-buffers-on-execute-p'. "]
            ("Ignoring defaults ... "
             :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"            ;; switch

            ["py-execute-file-python-switch" py-execute-file-python-switch
:help "Execute file through a Python interpreter.
With \\[universal-argument] use an unique Python interpreter. "]

            ["py-execute-file-ipython-switch" py-execute-file-ipython-switch
:help "Execute file through an IPython interpreter.
With \\[universal-argument] use an unique IPython interpreter. "]

            ["py-execute-file-python3-switch" py-execute-file-python3-switch
:help "Execute file through a Python3 interpreter.
With \\[universal-argument] use an unique Python3 interpreter. "]

            ["py-execute-file-python2-switch" py-execute-file-python2-switch
:help "Execute file through a Python2 interpreter.
With \\[universal-argument] use an unique Python2 interpreter. "]

            ["py-execute-file-python2.7-switch" py-execute-file-python2.7-switch
:help "Execute file through a Python2.7 interpreter.
With \\[universal-argument] use an unique Python2.7 interpreter. "]

            ["py-execute-file-jython-switch" py-execute-file-jython-switch
:help "Execute file through a Jython interpreter.
With \\[universal-argument] use an unique Jython interpreter. "]

            ["py-execute-file-python3.2-switch" py-execute-file-python3.2-switch
:help "Execute file through a Python3.2 interpreter.
With \\[universal-argument] use an unique Python3.2 interpreter. "]

            ["py-execute-file-python3.3-switch" py-execute-file-python3.3-switch
:help "Execute file through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

            ["py-execute-file-bpython-switch" py-execute-file-bpython-switch
:help "Execute file through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
            ;; dedicated-switch

            ["py-execute-file-python-dedicated-switch" py-execute-file-python-dedicated-switch
:help "Execute file through a unique Python interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-ipython-dedicated-switch" py-execute-file-ipython-dedicated-switch
:help "Execute file through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-python3-dedicated-switch" py-execute-file-python3-dedicated-switch
:help "Execute file through a unique Python3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-python2-dedicated-switch" py-execute-file-python2-dedicated-switch
:help "Execute file through a unique Python2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-python2.7-dedicated-switch" py-execute-file-python2.7-dedicated-switch
:help "Execute file through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-jython-dedicated-switch" py-execute-file-jython-dedicated-switch
:help "Execute file through a unique Jython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-python3.2-dedicated-switch" py-execute-file-python3.2-dedicated-switch
:help "Execute file through a unique Python3.2 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-python3.3-dedicated-switch" py-execute-file-python3.3-dedicated-switch
:help "Execute file through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]

            ["py-execute-file-bpython-dedicated-switch" py-execute-file-bpython-dedicated-switch
:help "Execute file through a unique Bpython interpreter.
Switch to output buffer; ignores `py-shell-switch-buffers-on-execute-p' "]
))))

        ;; Menu command forms
        (easy-menu-define py-menu map "Python Mode Commands"
          `("PyEdit"
            :help "Python-specific features"
            ("Block ... "
             ["Beginning of block" py-beginning-of-block
              :help "`py-beginning-of-block'
Go to start of innermost compound statement at point"]
             ["End of block" py-end-of-block
              :help "`py-end-of-block'
Go to end of innermost compound statement at point"]

             ["Down block" py-down-block
              :help "`py-down-block'

Go to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "]

             ["Up block" py-up-block
              :help "`py-up-block'

Go upwards to the beginning of next block below in buffer.

Returns indentation if block found, nil otherwise. "]

             ["Copy block" py-copy-block
              :help "`py-copy-block'
Copy innermost compound statement at point"]

             ["Kill block" py-kill-block
              :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete block" py-delete-block
              :help "`py-delete-block'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]
             
             ["Shift block right" py-shift-block-right
              :help "`py-shift-block-right'
Shift block right. "]
             
             ["Shift block left" py-shift-block-left
              :help "`py-shift-block-left'
Shift block left. "]

             )
            ("Def-or-class ... "

             ["Beginning of Def-or-Class" py-beginning-of-def-or-class
              :help "`py-beginning-of-def-or-class'
Go to start of innermost definition at point"]

             ["End of Def-or-Class" py-end-of-def-or-class
              :help "`py-end-of-def-or-class'
Go to end of innermost function definition at point"]

             ["Down def-or-class" py-down-def-or-class
              :help "`py-down-def-or-class'

Go to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "]

             ["Up def-or-class" py-up-def-or-class
              :help "`py-up-def-or-class'

Go upwards to the beginning of next def-or-class below in buffer.

Returns indentation if def-or-class found, nil otherwise. "]

             ["Copy Def-or-Class" py-copy-def-or-class
              :help "`py-copy-def-or-class'
Copy innermost definition at point"]

             ["Kill def-or-class" py-kill-def-or-class
              :help "`py-kill-def-or-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete def-or-class" py-delete-def-or-class
              :help "`py-delete-def-or-class'
Delete def-or-class at point, don't store deleted string in kill-ring"]

             ["Shift def-or-class right" py-shift-def-or-class-right
              :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

             ["Shift def-or-class left" py-shift-def-or-class-left
              :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

             )

            ("Clause ... "

             ["Beginning of clause" py-beginning-of-clause
              :help "`py-beginning-of-clause'
Go to start of innermost compound statement at point"]
             ["End of clause" py-end-of-clause
              :help "`py-end-of-clause'
Go to end of innermost compound statement at point"]

             ["Down clause" py-down-clause
              :help "`py-down-clause'

Go to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "]

             ["Up clause" py-up-clause
              :help "`py-up-clause'

Go upwards to the beginning of next clause below in buffer.

Returns indentation if clause found, nil otherwise. "]

             ["Copy clause" py-copy-clause
              :help "`py-copy-clause'
Copy innermost compound statement at point"]

             ["Kill clause" py-kill-clause
              :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete clause" py-delete-clause
              :help "`py-delete-clause'
Delete innermost compound statement at point, don't store deleted string in kill-ring"]

            ["Shift clause right" py-shift-clause-right
             :help "`py-shift-clause-right'
Shift clause right. "]

            ["Shift clause left" py-shift-clause-left
             :help "`py-shift-clause-left'
Shift clause left. "]

            )

            ("Statement ... "

             ["Beginning of Statement" py-beginning-of-statement
              :help "`py-beginning-of-statement'
Go to start of innermost definition at point"]

             ["End of Statement" py-end-of-statement
              :help "`py-end-of-statement'
Go to end of innermost function definition at point"]

             ["Copy statement" py-copy-statement
              :help "`py-copy-statement'
Copy innermost definition at point"]

             ["Kill statement" py-kill-statement
              :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete statement" py-delete-statement
              :help "`py-delete-statement'
Delete statement at point, don't store deleted string in kill-ring"]


             ["Shift statement right" py-shift-statement-right
              :help "`py-shift-statement-right'
Shift statement right. "]

             ["Shift statement left" py-shift-statement-left
              :help "`py-shift-statement-left'
Shift statement left. "]

             )

            ("Expression ..."

             ["Beginning of expression" py-beginning-of-expression
              :help "Go to the beginning of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

             ["End of expression" py-end-of-expression
              :help "`py-end-of-expression'
Go to the end of a compound python expression.

A a compound python expression might be concatenated by \".\" operator, thus composed by minor python expressions.

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

             ["Beginning of expression" py-beginning-of-expression
              :help "`py-beginning-of-expression'
Go to start of a Python expression"]

             ["End of expression" py-end-of-expression
              :help "`py-end-of-expression'
Go to end of a Python expression"]

             ["Copy expression" py-copy-expression
              :help "`py-copy-expression'
Copy expression at point"]

             ["Kill expression" py-kill-expression
              :help "`py-kill-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete expression" py-delete-expression
              :help "`py-delete-expression'
Delete expression at point, don't store deleted string in kill-ring"]

             )

            ("Partial expression ..."

             ["Beginning of minor expression" py-beginning-of-partial-expression
              :help "`py-beginning-of-partial-expression'
Go to start of an minor expression

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

             ["End of partial-expression" py-end-of-partial-expression
              :help "`py-end-of-partial-expression'
Go to end of an partial-expression

Expression here is conceived as the syntactical component of a statement in Python. See http://docs.python.org/reference
Operators however are left aside resp. limit py-expression designed for edit-purposes."]

             ["Copy partial expression" py-copy-partial-expression
              :help "`py-copy-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]

             ["Kill partial-expression" py-kill-partial-expression
              :help "`py-kill-partial-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete partial-expression" py-delete-partial-expression
              :help "`py-delete-partial-expression'
Delete partial-expression at point, don't store deleted string in kill-ring"]

             )

            ("Class ... "

             ["Beginning of Class" py-beginning-of-class
              :help "`py-beginning-of-class'
Go to start of innermost definition at point"]

             ["End of Class" py-end-of-class
              :help "`py-end-of-class'
Go to end of innermost function definition at point"]

             ["Down class" py-down-class
              :help "`py-down-class'

Go to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "]

             ["Up class" py-up-class
              :help "`py-up-class'

Go upwards to the beginning of next class below in buffer.

Returns indentation if class found, nil otherwise. "]

             ["Copy class" py-copy-class
              :help "`py-copy-class'
Copy innermost definition at point"]

             ["Kill class" py-kill-class
              :help "`py-kill-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete class" py-delete-class
              :help "`py-delete-class'
Delete class at point, don't store deleted string in kill-ring"]


             ["Shift class right" py-shift-class-right
              :help "`py-shift-class-right'
Shift class right. "]

             ["Shift class left" py-shift-class-left
              :help "`py-shift-class-left'
Shift class left. "]

             )

            ("Def ... "

             ["Beginning of Def" py-beginning-of-def
              :help "`py-beginning-of-def'
Go to start of innermost definition at point"]

             ["End of Def" py-end-of-def
              :help "`py-end-of-def'
Go to end of innermost function definition at point"]

             ["Down def" py-down-def
              :help "`py-down-def'

Go to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. "]

             ["Up def" py-up-def
              :help "`py-up-def'

Go upwards to the beginning of next def below in buffer.

Returns indentation if def found, nil otherwise. "]

             ["Copy def" py-copy-def
              :help "`py-copy-def'
Copy innermost definition at point"]

             ["Kill def" py-kill-def
              :help "`py-kill-def'
Delete innermost compound statement at point, store deleted string in kill-ring"]

             ["Delete def" py-delete-def
              :help "`py-delete-def'
Delete def at point, don't store deleted string in kill-ring"]


             ["Shift def right" py-shift-def-right
              :help "`py-shift-def-right'
Shift def right. "]

             ["Shift def left" py-shift-def-left
              :help "`py-shift-def-left'
Shift def left. "]

             )
            "-"

            (" Block bol ... "

             ["Beginning of block bol" py-beginning-of-block-bol
              :help "`py-beginning-of-block-bol'
Go to beginning of line at beginning of block.

Returns position reached, if successful, nil otherwise. "]

             ["End of block bol" py-end-of-block-bol
              :help "`py-end-of-block-bol'
Go to beginning of line following end of block.

Returns position reached, if successful, nil otherwise. "]

             ["Up block bol" py-up-block-bol
              :help "`py-up-block-bol'
Go to next block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down block bol" py-down-block-bol
              :help "`py-down-block-bol'
Go to next block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark block bol" py-mark-block-bol
              :help "`py-mark-block-bol'
Mark block at point. "]

             ["Copy block bol" py-copy-block-bol
              :help "`py-copy-block-bol'
Copy block at point. "]

             ["Kill block bol" py-kill-block-bol
              :help "`py-kill-block-bol'
Kill block at point. "]

             ["Delete block bol" py-delete-block-bol
              :help "`py-delete-block-bol'
Delete block at point. "]

             ["Shift block right" py-shift-block-right
              :help "`py-shift-block-right'
Shift block right. "]
             
             ["Shift block left" py-shift-block-left
              :help "`py-shift-block-left'
Shift block left. "]

             )

            (" Clause bol ... "

             ["Beginning of clause bol" py-beginning-of-clause-bol
              :help "`py-beginning-of-clause-bol'
Go to beginning of line at beginning of clause.

Returns position reached, if successful, nil otherwise. "]

             ["End of clause bol" py-end-of-clause-bol
              :help "`py-end-of-clause-bol'
Go to beginning of line following end of clause.

Returns position reached, if successful, nil otherwise. "]

             ["Up clause bol" py-up-clause-bol
              :help "`py-up-clause-bol'
Go to next clause upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down clause bol" py-down-clause-bol
              :help "`py-down-clause-bol'
Go to next clause downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark clause bol" py-mark-clause-bol
              :help "`py-mark-clause-bol'
Mark clause at point. "]

             ["Copy clause bol" py-copy-clause-bol
              :help "`py-copy-clause-bol'
Copy clause at point. "]

             ["Kill clause bol" py-kill-clause-bol
              :help "`py-kill-clause-bol'
Kill clause at point. "]

             ["Delete clause bol" py-delete-clause-bol
              :help "`py-delete-clause-bol'
Delete clause at point. "]


             ["Shift clause right" py-shift-clause-right
              :help "`py-shift-clause-right'
Shift clause right. "]

             ["Shift clause left" py-shift-clause-left
              :help "`py-shift-clause-left'
Shift clause left. "]

             )

            (" Block-Or-Clause bol ... "

             ["Beginning of block-or-clause bol" py-beginning-of-block-or-clause-bol
              :help "`py-beginning-of-block-or-clause-bol'
Go to beginning of line at beginning of block-or-clause.

Returns position reached, if successful, nil otherwise. "]

             ["End of block-or-clause bol" py-end-of-block-or-clause-bol
              :help "`py-end-of-block-or-clause-bol'
Go to beginning of line following end of block-or-clause.

Returns position reached, if successful, nil otherwise. "]

             ["Up block-or-clause bol" py-up-block-or-clause-bol
              :help "`py-up-block-or-clause-bol'
Go to next block-or-clause upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down block-or-clause bol" py-down-block-or-clause-bol
              :help "`py-down-block-or-clause-bol'
Go to next block-or-clause downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark block-or-clause bol" py-mark-block-or-clause-bol
              :help "`py-mark-block-or-clause-bol'
Mark block-or-clause at point. "]

             ["Copy block-or-clause bol" py-copy-block-or-clause-bol
              :help "`py-copy-block-or-clause-bol'
Copy block-or-clause at point. "]

             ["Kill block-or-clause bol" py-kill-block-or-clause-bol
              :help "`py-kill-block-or-clause-bol'
Kill block-or-clause at point. "]

             ["Delete block-or-clause bol" py-delete-block-or-clause-bol
              :help "`py-delete-block-or-clause-bol'
Delete block-or-clause at point. "]

             ["Shift block-or-clause right" py-shift-block-or-clause-right
              :help "`py-shift-block-or-clause-right'
Shift block-or-clause right. "]

             ["Shift block-or-clause left" py-shift-block-or-clause-left
              :help "`py-shift-block-or-clause-left'
Shift block-or-clause left. "]

             )
            
            (" Def bol ... "

             ["Beginning of def bol" py-beginning-of-def-bol
              :help "`py-beginning-of-def-bol'
Go to beginning of line at beginning of def.

Returns position reached, if successful, nil otherwise. "]

             ["End of def bol" py-end-of-def-bol
              :help "`py-end-of-def-bol'
Go to beginning of line following end of def.

Returns position reached, if successful, nil otherwise. "]

             ["Up def bol" py-up-def-bol
              :help "`py-up-def-bol'
Go to next def upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down def bol" py-down-def-bol
              :help "`py-down-def-bol'
Go to next def downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark def bol" py-mark-def-bol
              :help "`py-mark-def-bol'
Mark def at point. "]

             ["Copy def bol" py-copy-def-bol
              :help "`py-copy-def-bol'
Copy def at point. "]

             ["Kill def bol" py-kill-def-bol
              :help "`py-kill-def-bol'
Kill def at point. "]

             ["Delete def bol" py-delete-def-bol
              :help "`py-delete-def-bol'
Delete def at point. "]

             ["Shift def right" py-shift-def-right
              :help "`py-shift-def-right'
Shift def right. "]

             ["Shift def left" py-shift-def-left
              :help "`py-shift-def-left'
Shift def left. "]

             )

            (" Class bol ... "
             ["Beginning of class bol" py-beginning-of-class-bol
              :help "`py-beginning-of-class-bol'
Go to beginning of line at beginning of class.

Returns position reached, if successful, nil otherwise. "]

             ["End of class bol" py-end-of-class-bol
              :help "`py-end-of-class-bol'
Go to beginning of line following end of class.

Returns position reached, if successful, nil otherwise. "]

             ["Up class bol" py-up-class-bol
              :help "`py-up-class-bol'
Go to next class upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down class bol" py-down-class-bol
              :help "`py-down-class-bol'
Go to next class downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark class bol" py-mark-class-bol
              :help "`py-mark-class-bol'
Mark class at point. "]

             ["Copy class bol" py-copy-class-bol
              :help "`py-copy-class-bol'
Copy class at point. "]

             ["Kill class bol" py-kill-class-bol
              :help "`py-kill-class-bol'
Kill class at point. "]

             ["Delete class bol" py-delete-class-bol
              :help "`py-delete-class-bol'
Delete class at point. "]

             ["Shift class right" py-shift-class-right
              :help "`py-shift-class-right'
Shift class right. "]

             ["Shift class left" py-shift-class-left
              :help "`py-shift-class-left'
Shift class left. "]

             )

            (" Def-Or-Class bol ... "
             ["Beginning of def-or-class bol" py-beginning-of-def-or-class-bol
              :help "`py-beginning-of-def-or-class-bol'
Go to beginning of line at beginning of def-or-class.

Returns position reached, if successful, nil otherwise. "]

             ["End of def-or-class bol" py-end-of-def-or-class-bol
              :help "`py-end-of-def-or-class-bol'
Go to beginning of line following end of def-or-class.

Returns position reached, if successful, nil otherwise. "]

             ["Up def-or-class bol" py-up-def-or-class-bol
              :help "`py-up-def-or-class-bol'
Go to next def-or-class upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Down def-or-class bol" py-down-def-or-class-bol
              :help "`py-down-def-or-class-bol'
Go to next def-or-class downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise. "]

             ["Mark def-or-class bol" py-mark-def-or-class-bol
              :help "`py-mark-def-or-class-bol'
Mark def-or-class at point. "]

             ["Copy def-or-class bol" py-copy-def-or-class-bol
              :help "`py-copy-def-or-class-bol'
Copy def-or-class at point. "]

             ["Kill def-or-class bol" py-kill-def-or-class-bol
              :help "`py-kill-def-or-class-bol'
Kill def-or-class at point. "]

             ["Delete def-or-class bol" py-delete-def-or-class-bol
              :help "`py-delete-def-or-class-bol'
Delete def-or-class at point. "]

             ["Shift def-or-class right" py-shift-def-or-class-right
              :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]
             
             ["Shift def-or-class left" py-shift-def-or-class-left
              :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

             )

            (" Statement bol ... "
             ["Beginning of statement bol" py-beginning-of-statement-bol
              :help "`py-beginning-of-statement-bol'
Go to beginning of line at beginning of statement.

Returns position reached, if successful, nil otherwise. "]

             ["End of statement bol" py-end-of-statement-bol
              :help "`py-end-of-statement-bol'
Go to beginning of line following end of statement.

Returns position reached, if successful, nil otherwise. "]

             ["Mark statement bol" py-mark-statement-bol
              :help "`py-mark-statement-bol'
Mark statement at point. "]

             ["Copy statement bol" py-copy-statement-bol
              :help "`py-copy-statement-bol'
Copy statement at point. "]

             ["Kill statement bol" py-kill-statement-bol
              :help "`py-kill-statement-bol'
Kill statement at point. "]

             ["Delete statement bol" py-delete-statement-bol
              :help "`py-delete-statement-bol'
Delete statement at point. "]

             ["Shift statement right" py-shift-statement-right
              :help "`py-shift-statement-right'
Shift statement right. "]

             ["Shift statement left" py-shift-statement-left
              :help "`py-shift-statement-left'
Shift statement left. "]

             )
            "-"
            ["Backward into nomenclature" py-backward-into-nomenclature
             :help " `py-backward-into-nomenclature'
Go backward into nomenclature

A nomenclature is a fancy way of saying AWordWithMixedCaseNotUnderscores. "]
            ["Forward into nomenclature" py-forward-into-nomenclature
             :help " `py-forward-into-nomenclature'
Go forward into nomenclature

A nomenclature is a fancy way of saying AWordWithMixedCaseNotUnderscores. "]
            "-"
            ("Filling"
             :help "see also customizable `py-fill-docstring-style'"

             ["Fill string" py-fill-string
              :help " `py-fill-string'

Uses value of `py-fill-docstring-style', if set. "]

             ["Fill paragraph" py-fill-paragraph
              :help " `py-fill-paragraph'

Uses value of `py-fill-docstring-style', if set. "]

             ["Fill comment" py-fill-comment
              :help " `py-fill-comment'

Fill comment at point. "]

             ["Fill string django-style " py-fill-string-django
              :help " `py-fill-string-django'

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'
 "]

             ["py fill string onetwo" py-fill-string-onetwo
              :help " `py-fill-string-onetwo'
One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'"]

             ["py fill string pep 257" py-fill-string-pep-257
              :help " `py-fill-string-pep-257'
PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style' "]

             ["py fill string pep 257" py-fill-string-pep-257
              :help " `py-fill-string-pep-257'

PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'"]

             ["py fill string pep 257 nn" py-fill-string-pep-257-nn
              :help " `py-fill-string-pep-257-nn'

PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'"]

             ["py fill string symmetric" py-fill-string-symmetric
              :help " `py-fill-string-symmetric'

Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-fill-docstring-style'"]

             )))

        ;; Python shell menu
        (easy-menu-define py-menu map "Python Shells"
          `("PyShell"
            :help "Python Shells"
            ["Default interpreter" py-shell
             :help "`py-shell'
Switch to `inferior' Python in separate buffer"]

            ;; ["Toggle enforcement of default interpreter" toggle-force-py-shell-name-p
            ;; :help "If customized default `py-shell-name' should be enforced upon execution. "]

            ["Enforce py-shell-name" force-py-shell-name-p-on
             :help "Enforce customized default `py-shell-name' should upon execution. "]

            ["Don't enforce default interpreter" force-py-shell-name-p-off
             :help "Make execute commands guess interpreter from environment"]

            ;; ["Enforce locally Python shell sessions interpreter " toggle-force-local-shell
            ;; :help "If locally indicated Python shell should be taken and
            ;; enforced upon sessions execute commands. "]

            ["Enforce local Python shell " py-force-local-shell-on
             :help "Locally indicated Python being enforced upon sessions execute commands. "]

            ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
             :help "Restore `py-shell-name' default value and `behaviour'. "]

            "-"

            ["python" python
             :help "`python'
Start an Python interpreter.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'."]
            ["ipython" ipython
             :help "`ipython'
Start an IPython interpreter.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'."]
            ["python3" python3
             :help "`python3'
Start an Python3 interpreter.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."]
            ["python2" python2
             :help "`python2'
Start an Python2 interpreter.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."]
            ["python2.7" python2.7
             :help "`python2.7'
Start an Python2.7 interpreter.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."]
            ["jython" jython
             :help "`jython'
Start an Jython interpreter.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."]
            ["python3.2" python3.2
             :help "`python3.2'
Start an Python3.2 interpreter.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."]

            ["python3.3" python3.3
             :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

            "-"
            ["python-dedicated" python-dedicated
             :help "`python-dedicated'
Start an unique Python interpreter in another window.

Optional C-u prompts for options to pass to the Python interpreter. See `py-python-command-args'."]
            ["ipython-dedicated" ipython-dedicated
             :help "`ipython-dedicated'
Start an unique IPython interpreter in another window.

Optional C-u prompts for options to pass to the IPython interpreter. See `py-python-command-args'."]
            ["python3-dedicated" python3-dedicated
             :help "`python3-dedicated'
Start an unique Python3 interpreter in another window.

Optional C-u prompts for options to pass to the Python3 interpreter. See `py-python-command-args'."]
            ["python2-dedicated" python2-dedicated
             :help "`python2-dedicated'
Start an unique Python2 interpreter in another window.

Optional C-u prompts for options to pass to the Python2 interpreter. See `py-python-command-args'."]
            ["python2.7-dedicated" python2.7-dedicated
             :help "`python2'.7-dedicated
Start an unique Python2.7 interpreter in another window.

Optional C-u prompts for options to pass to the Python2.7 interpreter. See `py-python-command-args'."]
            ["jython-dedicated" jython-dedicated
             :help "`jython-dedicated'
Start an unique Jython interpreter in another window.

Optional C-u prompts for options to pass to the Jython interpreter. See `py-python-command-args'."]
            ["python3.2-dedicated" python3.2-dedicated
             :help "`python3.2-dedicated'
Start an unique Python3.2 interpreter in another window.

Optional C-u prompts for options to pass to the Python3.2 interpreter. See `py-python-command-args'."]

            ;;
            "-"

            ["Toggle split-windows-on-execute" py-toggle-split-windows-on-execute
             :help "Switch boolean `py-split-windows-on-execute-p'."]

            ["Switch split-windows-on-execute ON" py-split-windows-on-execute-on
             :help "Switch `py-split-windows-on-execute-p' ON. "]

            ["Switch split-windows-on-execute OFF" py-split-windows-on-execute-off
             :help "Switch `py-split-windows-on-execute-p' OFF. "]

            ["Toggle shell-switch-buffers-on-execute" py-toggle-shell-switch-buffers-on-execute
             :help "Switch boolean `py-switch-buffers-on-execute-p'."]
            ["Switch shell-switch-buffers-on-execute ON" py-shell-switch-buffers-on-execute-on
             :help "Switch `py-switch-buffers-on-execute-p' ON. "]
            ["Switch shell-switch-buffers-on-execute OFF" py-shell-switch-buffers-on-execute-off
             :help "Switch `py-switch-buffers-on-execute-p' OFF. "]))
        map))

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;; hook doesn't get unloaded
;; (defalias 'python-pdbtrack-track-stack-file 'py-pdbtrack-track-stack-file)

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defun py-python-version (&optional executable verbose)
  "Returns versions number of a Python EXECUTABLE, string.

If no EXECUTABLE given, `py-shell-name' is used.
Interactively output of `--version' is displayed. "
  (interactive)
  (let* ((executable (or executable py-shell-name))
         (erg (string-strip (shell-command-to-string (concat executable " --version")))))
    (when (interactive-p) (message "%s" erg))
    (unless verbose (setq erg (cadr (split-string erg))))
    erg))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;;; Utility stuff
(declare-function compilation-shell-minor-mode "compile" (&optional arg))

;;dereived from shipped python.el
(defun py-history-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `py-history-filter-regexp'."
  (not (string-match py-history-filter-regexp str)))

;; Fixme: Loses with quoted whitespace.
(defun py-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (py-args-to-list (substring string (+ 1 where)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if pos (py-args-to-list (substring string pos))))))))

(defun py-check-version (cmd)
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

;;;###autoload
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
behavior, change `py-remove-cwd-from-path' to nil."
  (interactive (if current-prefix-arg
		   (list (read-string "Run Python: " python-command) nil t)
		 (list python-command)))
  (require 'ansi-color) ; for ipython
  (unless cmd (setq cmd python-command))
  (py-check-version cmd)
  (setq python-command cmd)
  ;; Fixme: Consider making `python-buffer' buffer-local as a buffer
  ;; (not a name) in Python buffers from which `run-python' &c is
  ;; invoked.  Would support multiple processes better.
  (when (or new (not (comint-check-proc python-buffer)))
    (with-current-buffer
	(let* ((cmdlist
		(append (py-args-to-list cmd) '("-i")
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

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun python-send-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-send-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

(autoload 'comint-get-source "comint")

(defun python-load-file (file-name)
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
    (py-send-command
     (if (string-match "\\.py\\'" file-name)
	 (let ((module (file-name-sans-extension
			(file-name-nondirectory file-name))))
	   (format "emacs.eimport(%S,%S)"
		   module (file-name-directory file-name)))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

(defun python-set-proc ()
  "Set the default value of `python-buffer' to correspond to this buffer.
If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from \\[py-send-region] et al when used
in a buffer that doesn't have a local value of `python-buffer'."
  (interactive)
  (if (local-variable-p 'python-buffer)
      (setq-default python-buffer python-buffer)
    (error "No local value of `python-buffer'")))
;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun python-send-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (py-send-region (progn (beginning-of-defun) (point))
                                      (progn (end-of-defun) (point)))))

(defun python-check-comint-prompt (&optional proc)
  "Return non-nil if and only if there's a normal prompt in the inferior buffer.
If there isn't, it's probably not appropriate to send input to return Eldoc
information etc.  If PROC is non-nil, check the buffer for that process."
  (with-current-buffer (process-buffer (or proc (python-proc)))
    (save-excursion
      (save-match-data
	(re-search-backward (concat python--prompt-regexp " *\\=")
			    nil t)))))

(defun py-send-command (command)
  "Like `py-send-string' but resets `compilation-shell-minor-mode'."
  (when (python-check-comint-prompt)
    (with-current-buffer (process-buffer (python-proc))
      (goto-char (point-max))
      (compilation-forget-errors)
      (py-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

(defun python-load-file (file-name)
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
    (py-send-command
     (if (string-match "\\.py\\'" file-name)
         (let ((module (file-name-sans-extension
                        (file-name-nondirectory file-name))))
           (format "emacs.eimport(%S,%S)"
                   module (file-name-directory file-name)))
       (format "execfile(%S)" file-name)))
    (message "%s loaded" file-name)))

(defun py-proc (&optional dedicated)
  "Return the current Python process.

See variable `python-buffer'.  Starts a new process if necessary."
  (if (and (not dedicated) (comint-check-proc python-buffer))
      (get-buffer-process python-buffer)
    (when py-verbose-p (message "py-proc: Please wait while starting a Python shell, as completion needs it"))
    (py-shell nil dedicated)))

;;; Miscellany.

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
                        (rx bol (or "import" "from") (1+ space)
                            (group (1+ (not (any " \t\n.")))))
                        (+ (point-min) 10000) ; Probably not worth customizing.
                        t)
                  (if (member (match-string 1) python-jython-packages)
                      (throw 'done t))))
              (jython-mode)))))))

(defun python-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `py-indent-offset'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count py-indent-offset))
  (indent-rigidly start end count))

(defun python-outline-level ()
  "`outline-level' function for Python mode.
The level is the number of `py-indent-offset' steps of indentation
of current line."
  (1+ (/ (current-indentation) py-indent-offset)))

;;; need to clear py-shell-input-lines if primary prompt found
(defun py-shell-simple-send (proc string)
  (setq py-shell-input-lines (cons string py-shell-input-lines))
  (comint-simple-send proc string))

(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

;; Fixme:  Provide a find-function-like command to find source of a
;; definition (separate from BicycleRepairMan).  Complicated by
;; finding the right qualified name.

;;; Completion.
;; we use Dave Love's excellent stuff here
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

;; Fixme: Should font-lock try to run this when it deals with an import?
;; Maybe not a good idea if it gets run multiple times when the
;; statement is being edited, and is more likely to end up with
;; something syntactically incorrect.
;; However, what we should do is to trundle up the block tree from point
;; to extract imports that appear to be in scope, and add those.
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

;; Fixme: This fails the first time if the sub-process isn't already
;; running.  Presumably a timing issue with i/o to the process.
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
  "Completion adapting the python.el-way, as shipped with Emacs23.

Stuff originallye23 authored by Dave Love, errors are mine -ar "
  (interactive "*")
  ;;
  (add-hook 'comint-preoutput-filter-functions #'py-preoutput-filter
	    nil t)
  (let ((end (point))
	(start (save-excursion
		 (and (re-search-backward
		       (rx (or buffer-start (regexp "[^[:alnum:]._]"))
			   (group (1+ (regexp "[[:alnum:]._]"))) point)
		       nil t)
		      (match-beginning 1)))))
    (when start
      (list start end
            (completion-table-dynamic 'python-symbol-completions)))))

(defun py-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.

The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (or (python-shell-send-string-no-output string)
      (let ((proc (py-proc)))
        (with-current-buffer (process-buffer proc)
          (when (python-check-comint-prompt proc)
            (set (make-local-variable 'python-preoutput-result) nil)
            (accept-process-output proc 5)
            (prog1 python-preoutput-result
              (kill-local-variable 'python-preoutput-result)))))))

(defun python-send-receive (string)
  "Send STRING to inferior Python (if any) and return result.
The result is what follows `_emacs_out' in the output.
This is a no-op if `python-check-comint-prompt' returns nil."
  (py-send-string string)
  (let ((proc (python-proc)))
    (with-current-buffer (process-buffer proc)
      (when (python-check-comint-prompt proc)
	(set (make-local-variable 'python-preoutput-result) nil)
	(while (progn
		 (accept-process-output proc 5)
		 (null python-preoutput-result)))
	(prog1 python-preoutput-result
	  (kill-local-variable 'python-preoutput-result))))))

;;;; FFAP support
(defun python-module-path (module)
  "Function for `ffap-alist' to return path to MODULE."
  (py-send-receive (format "emacs.modpath (%S)" module)))

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

;;; Skeletons
(eval-when-compile
  ;; Define a user-level skeleton and add it to the abbrev table.
  (defmacro def-python-skeleton (name &rest elements)
    (let* ((name (symbol-name name))
           (function (intern (concat "python-insert-" name))))
      `(progn
         ;; Usual technique for inserting a skeleton, but expand
         ;; to the original abbrev instead if in a comment or string.
         ;; (when python-use-skeletons
         ;;   (define-abbrev python-mode-abbrev-table ,name ""
         ;;     ',function
         ;;     nil t))                      ; system abbrev
         (define-skeleton ,function
           ,(format "Insert Python \"%s\" template." name)
           ,@elements)))))
(put 'def-python-skeleton 'lisp-indent-function 2)

(def-python-skeleton if
    "Condition: "
  "if " str ":" \n
  > -1	   ; Fixme: I don't understand the spurious space this removes.
  _ \n
  ("other condition, %s: "
   <			; Avoid wrong indentation after block opening.
   "elif " str ":" \n
   > _ \n nil)
  '(python-else) | ^)

(define-skeleton python-else
  "Auxiliary skeleton."
  nil
  (unless (eq ?y (read-char "Add `else' clause? (y for yes or RET for no) "))
    (signal 'quit t))
  < "else:" \n
  > _ \n)

(def-python-skeleton while
    "Condition: "
  "while " str ":" \n
  > -1 _ \n
  '(python-else) | ^)

(def-python-skeleton for
    "Target, %s: "
  "for " str " in " (skeleton-read "Expression, %s: ") ":" \n
  > -1 _ \n
  '(python-else) | ^)

(def-python-skeleton try/except
    nil
  "try:" \n
  > -1 _ \n
  ("Exception, %s: "
   < "except " str '(python-target) ":" \n
   > _ \n nil)
  < "except:" \n
  > _ \n
  '(python-else) | ^)

(define-skeleton python-target
  "Auxiliary skeleton."
  "Target, %s: " ", " str | -2)

(def-python-skeleton try/finally
    nil
  "try:" \n
  > -1 _ \n
  < "finally:" \n
  > _ \n)

(def-python-skeleton def
    "Name: "
  "def " str " (" ("Parameter, %s: " (unless (equal ?\( (char-before)) ", ")
                   str) "):" \n
                   "\"\"\"" - "\"\"\"" \n     ; Fixme:  extra space inserted -- why?).
                   > _ \n)

(def-python-skeleton class
    "Name: "
  "class " str " (" ("Inheritance, %s: "
		     (unless (equal ?\( (char-before)) ", ")
		     str)
  & ")" | -2				; close list or remove opening
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)


;;;; Modes.

;; pdb tracking is alert once this file is loaded, but takes no action if

;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;      gud-pdb-command-name (symbol-name pdb-path))

(unless py-separator-char (setq py-separator-char (py-separator-char)))

;;; Hooks
;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py-kill-emacs-hook)
;; done inside py-shell
;; (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)

(remove-hook 'python-mode-hook 'python-setup-brm)
(add-hook 'python-mode-hook
          #'(lambda ()
              (when py-smart-indentation
                (if (bobp)
                    (save-excursion
                      (save-restriction
                        (widen)
                        (while (and (not (eobp))
                                    (or
                                     (let ((erg (syntax-ppss)))
                                       (or (nth 1 erg) (nth 8 erg)))
                                     (eq 0 (current-indentation))))
                          (forward-line 1))
                        (back-to-indentation)
                        (py-guess-indent-offset)))
                  (py-guess-indent-offset)))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode py-indent-tabs-mode)
            (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
            (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
            ;; (orgstruct-mode 1)
            ))

(add-hook 'python-mode-hook 'python-find-imports)

(when py-sexp-function
  (add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'forward-sexp-function) py-sexp-function))))

(when py-warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py-warn-tmp-files-left))


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
        (message "%s" erg)
      (message "%s" "pdb.py not found, please customize `pdb-path'"))
    erg))

;; credits to python.el
;; (defun py-beg-of-defun-function ()
;;   (set (make-local-variable 'beginning-of-defun-function)
;;        'py-beginning-of-def-or-class))
;;
;; (defun py-end-of-defun-function ()
;;   (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

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
           (where-is-internal 'self-insert-command)))

(setq py-shell-map
      (let ((map (copy-keymap comint-mode-map)))
        (substitute-key-definition 'complete-symbol 'completion-at-point
                                   map global-map)
        (define-key map (kbd "RET") 'comint-send-input)
        (define-key map "\C-c-" 'py-up-exception)
        (define-key map "\C-c=" 'py-down-exception)
        ;; defined three times... one should succed
        (define-key map (kbd "TAB") 'py-shell-complete)
        (define-key map [tab] 'py-shell-complete)
        (define-key map "\t" 'py-shell-complete)
        (define-key map [(meta tab)] 'py-shell-complete)
        map))

(defalias 'py-toggle-shell 'py-switch-shell)
(defun py-switch-shell (&optional arg)
  "Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

\\[universal-argument] `py-toggle-shell' prompts to specify a reachable Python command.
\\[universal-argument] followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
\\[universal-argument] followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     \"MY-PATH-TO-SHELL\")
"
  (interactive "P")
  (let ((name (cond ((eq 2 (prefix-numeric-value arg))
                     "python2")
                    ((eq 3 (prefix-numeric-value arg))
                     "python3")
                    ((eq 4 (prefix-numeric-value arg))
                     (string-strip
                      (read-from-minibuffer "Python Shell: " py-shell-name) "\" " "\" "
                      ))
                    ((eq 5 (prefix-numeric-value arg))
                     "jython")
                    (t (if (string-match py-shell-name
                                         py-shell-toggle-1)
                           py-shell-toggle-2
                         py-shell-toggle-1))))
        erg msg)
    (cond ((or (string= "ipython" name)
               (string= "IPython" name))
           (setq py-shell-name name
                 py-which-bufname "IPython"
                 msg "IPython"
                 mode-name "IPython"))
          ((string-match "python3" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare name)
                 msg "CPython"
                 mode-name (py-buffer-name-prepare name)))
          ((string-match "jython" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare name)
                 msg "Jython"
                 mode-name (py-buffer-name-prepare name)))
          ((string-match "python" name)
           (setq py-shell-name name
                 py-which-bufname (py-buffer-name-prepare name)
                 msg "CPython"
                 mode-name py-which-bufname))
          (t
           (setq py-shell-name name
                 py-which-bufname name
                 msg name
                 mode-name name)))
    ;; py-edit-only-p has no interpreter
    ;; (if py-edit-only-p
    ;; (setq erg py-shell-name)
    (setq erg (executable-find py-shell-name))
    ;;)
    (if erg
        (progn
          (force-mode-line-update)
          (when (interactive-p)
            (message "Using the %s shell, %s" msg erg))
          (setq py-output-buffer (format "*%s Output*" py-which-bufname)))
      (error (concat "Could not detect " py-shell-name " on your sys
tem")))))

;; (unless (string= (buffer-name (current-buffer)) py-which-bufname)
;;   (unless (buffer-live-p py-which-bufname)
;;     (py-shell nil nil py-shell-name)
;;     (set-buffer (get-buffer-create py-which-bufname))
;;     (when py-switch-buffers-on-execute-p
;;       (switch-to-buffer (current-buffer)) )))

(defun py-toggle-local-default-use ()
  (interactive)
  "Toggle boolean value of `py-use-local-default'.

Returns `py-use-local-default'

See also `py-install-local-shells'
Installing named virualenv shells is the preffered way,
as it leaves your system default unchanged."
  (setq py-use-local-default (not py-use-local-default))
  (when (interactive-p) (message "py-use-local-default set to %s" py-use-local-default))
  py-use-local-default)

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

;; FixMe: for unknown reasons this is not done by mode
(if (file-readable-p abbrev-file-name)
    (add-hook 'python-mode-hook '(lambda () (load abbrev-file-name nil t)))
  (message "Warning: %s" "no abbrev-file found, customize `abbrev-file-name' in order to make mode-specific abbrevs work. "))

;;;
(add-to-list 'hs-special-modes-alist
             (list
              'python-mode
              ;; start regex
              (concat (if py-hide-show-hide-docstrings
                          "^\\s-*\"\"\"\\|" "")
                      (mapconcat 'identity
                                 (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                         py-hide-show-keywords)
                                 "\\|"))
              ;; end regex
              nil
              ;; comment-start regex
              "#"
              ;; forward-sexp function
              (lambda (arg)
                (py-end-of-block))
              nil))

;; Fixme: This should inherit some stuff from `python-mode', but I'm
;; not sure how much: at least some keybindings, like C-c C-f;
;; syntax?; font-locking, e.g. for triple-quoted strings?
;; (define-derived-mode inferior-python-mode comint-mode "Inferior Python"
;;   "Major mode for interacting with an inferior Python process.
;; A Python process can be started with \\[py-shell].
;;
;; Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
;; that order.
;;
;; You can send text to the inferior Python process from other buffers
;; containing Python source.
;;  * \\[python-switch-to-python] switches the current buffer to the Python
;;     process buffer.
;;  * \\[py-send-region] sends the current region to the Python process.
;;  * \\[py-send-region-and-go] switches to the Python process buffer
;;     after sending the text.
;; For running multiple processes in multiple buffers, see `run-python' and
;; `python-buffer'.
;;
;; \\{inferior-python-mode-map}"
;;   :group 'python-mode
;;   (setq mode-line-process '(":%s")))

;;;

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

(defun python-input-filter (str)
  "`comint-input-filter' function for inferior Python.
Don't save anything for STR matching `inferior-python-filter-regexp'."
  (not (string-match inferior-python-filter-regexp str)))

;; Using this stops us getting lines in the buffer like
;; >>> ... ... >>>
;; Also look for (and delete) an `_emacs_ok' string and call
;; `python-preoutput-continuation' if we get it.
(defun py-preoutput-filter (s)
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
              (set (make-local-variable 'python-preoutput-skip-next-prompt) t)
              python-preoutput-result)
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

(make-obsolete 'jpython-mode 'jython-mode nil)
(autoload 'comint-check-proc "comint")

(defun py-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.
With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (py-shell)) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

;;; Hooks
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (when py-load-highlight-indentation-p
;;               (unless (featurep 'highlight-indentation)
;;                 (load (concat (py-normalize-directory py-install-directory) "extensions" (char-to-string py-separator-char) "highlight-indentation.el"))))))


(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

;;; interpreter-mode-alist

;;;
;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.


;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))

(let ((modes '(("jython" . jython-mode)
               ("python" . python-mode)
               ("python2" . python-mode)
               ("python2.6" . python-mode)
               ("python2.7" . python-mode)
               ("python3" . python-mode)
               ("python3.0" . python-mode)
               ("python3.1" . python-mode)
               ("python3.2" . python-mode)
               ("python3.3" . python-mode))))
  (while modes
    (when (not (assoc (car modes) interpreter-mode-alist))
      (push (car modes) interpreter-mode-alist))
    (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist)
               (rassq 'jython-mode auto-mode-alist)))
  (push '("\\.py$" . python-mode) auto-mode-alist))

;; (add-to-list 'interpreter-mode-alist (cons (purecopy "jython") 'jython-mode))
;; (add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))
;; (add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))

;;;
(define-derived-mode inferior-python-mode comint-mode "Inferior Python"
  "Major mode for interacting with an inferior Python process.
A Python process can be started with \\[run-python] or \\[py-shell].

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
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (set (make-local-variable 'comint-prompt-regexp)
             (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
                    (concat "\\("
                            (mapconcat 'identity
                                       (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                       "\\|")
                            "\\)"))
                   (t (concat "\\("
                              (mapconcat 'identity
                                         (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
                                         "\\|")
                              "\\)"))))
  ;; (define-key inferior-python-mode-map [remap complete-symbol]
  ;;   'completion-at-point)
  ;; (add-hook 'completion-at-point-functions
  ;;           'python-shell-completion-complete-at-point nil 'local)
  ;; (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  ;;              'python-shell-completion-complete-at-point)
  ;; (define-key inferior-python-mode-map "\t"
  ;;   'python-shell-completion-complete-or-indent)

  (if py-complete-function
      (add-hook 'completion-at-point-functions
                py-complete-function nil 'local)
    (add-hook 'completion-at-point-functions
              'py-completion-at-point nil 'local))
  (compilation-shell-minor-mode 1))

(define-derived-mode python-shell-mode comint-mode "Python Shell"
  "Major mode for interacting with an inferior Python process.

Hooks `comint-mode-hook' and `inferior-python-mode-hook' are run in
that order.

You can send text to the inferior Python process from other buffers
containing Python source.

\\{python-shell-mode-map}"
  (setq mode-line-process '(":%s"))

  (set (make-local-variable 'comint-input-filter) 'python-input-filter)

  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (if py-complete-function
      (add-hook 'completion-at-point-functions
                py-complete-function nil 'local)
    (add-hook 'completion-at-point-functions
              'py-completion-at-point nil 'local))
  (python--set-prompt-regexp)
  (compilation-shell-minor-mode 1)
  (substitute-key-definition 'complete-symbol 'completion-at-point map global-map)
  ;; (substitute-key-definition 'complete-symbol 'py-shell-complete
  ;; python-shell-mode-map global-map)

  (define-key python-shell-mode-map (kbd "RET") 'comint-send-input)
  (if py-complete-function
      (define-key python-shell-mode-map [tab] py-complete-function)
    (define-key python-shell-mode-map [tab] 'python-completion-at-point))
  (define-key python-shell-mode-map "\C-c-" 'py-up-exception)
  (define-key python-shell-mode-map "\C-c=" 'py-down-exception))

(define-derived-mode python-mode fundamental-mode python-mode-modeline-display
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
  :group 'python-mode
;;; Local vars
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (set (make-local-variable 'font-lock-defaults)
       '(python-font-lock-keywords nil nil nil nil
                                   (font-lock-syntactic-keywords
                                    . py-font-lock-syntactic-keywords)
                                   ;; This probably isn't worth it.
                                   ;; (font-lock-syntactic-face-function
                                   ;;  . python-font-lock-syntactic-face-function)
                                   ))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py-comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":\\s-*\n")
  (set (make-local-variable 'outline-level) #'python-outline-level)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  ;; (set (make-local-variable 'fill-paragraph-function) 'python-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (make-local-variable 'python-saved-check-command)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'eldoc-documentation-function)
       #'py-eldoc-function)
  (set (make-local-variable 'skeleton-further-elements)
       '((< '(backward-delete-char-untabify (min py-indent-offset
                                                 (current-column))))
         (^ '(- (1+ (current-indentation))))))
  (py-set-load-path)
  ;; (add-to-list 'load-path py-install-directory)
  ;; (add-to-list 'load-path (concat py-install-directory "extensions"))
  (when py-prepare-autopair-mode-p
    (load (concat (py-normalize-directory py-install-directory) "autopair" (char-to-string py-separator-char) "autopair.el") nil t)
    (add-hook 'python-mode-hook
              #'(lambda ()
                  (setq autopair-handle-action-fns
                        (list #'autopair-default-handle-action
                              #'autopair-python-triple-quote-action)))))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  ;; (cond
  ;; (py-complete-function
  ;; (add-hook 'completion-at-point-functions
  ;; py-complete-function nil 'local))
  (add-hook 'completion-at-point-functions
            'py-completion-at-point nil 'local)

  ;; (py-load-pymacs-p
  ;;  (add-hook 'completion-at-point-functions
  ;;            'py-complete-completion-at-point nil 'local))
  ;; (t
  ;;  (add-hook 'completion-at-point-functions
  ;;            'py-shell-complete nil 'local)))
  (when (and py-imenu-create-index-p (fboundp 'imenu-add-to-menubar)(ignore-errors (require 'imenu)))
    (set (make-local-variable 'imenu-create-index-function) 'py-imenu-create-index-function)
    (imenu-add-to-menubar "PyIndex"))
  ;; (when py-imenu-create-index-p (imenu-add-to-menubar "PyIndex"))

  ;; Now guess `py-indent-offset'

  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  ;; (py-send-string "import emacs")

  (when py-start-run-py-shell
    ;; py-shell may split window, provide restore
    (window-configuration-to-register 213465879)
    (unless (get-process (py-process-name))
      (let ((oldbuf (current-buffer)))
        (save-excursion
          (py-shell)
          (set-buffer oldbuf))))
    ;; (jump-to-register 213465879)
    )
  ;; (run-mode-hooks 'python-mode-hook)
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when py-smart-operator-mode-p
    (unless (featurep 'py-smart-operator)
      (load (concat (py-normalize-directory py-install-directory) "extensions/py-smart-operator.el")))
    (py-smart-operator-mode-on))
  (when (interactive-p) (message "python-mode loaded from: %s" "python-components-mode.el")))

(define-derived-mode python2-mode python-mode "Python2"
  "Edit and run code used by Python version 2 series. "
  :group 'python-mode
  (set (make-local-variable 'py-exec-command) '(format "execfile(r'%s') # PYTHON-MODE\n" filename))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'%s') # PYTHON-MODE\n" string))
  (py-toggle-shell "python2"))

(define-derived-mode python3-mode python-mode "Python3"
  "Edit and run code used by Python version 3 series. "
  :group 'python-mode
  (set (make-local-variable 'py-exec-command) '(format "exec(compile(open('%s').read(), '%s', 'exec')) # PYTHON-MODE\n" file file))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'(%s)') # PYTHON-MODE\n" string))
  (py-toggle-shell "python3"))

(define-derived-mode jython-mode python-mode "Jython"
  "Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'."

  :group 'python-mode
  (py-toggle-shell "jython"))

(provide 'python-components-mode)
(provide 'python)
(provide 'python-mode)
;;; python-components-mode.el ends here
