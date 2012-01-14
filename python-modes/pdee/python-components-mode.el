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

(eval-when-compile
  (require 'compile)
  (require 'hippie-exp))

(require 'comint)
(require 'custom)
(eval-when-compile (require 'cl))
(require 'compile)
(require 'ansi-color)
(require 'cc-cmds)
(require 'shell)

(defgroup python-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "This is experimental `python-components-mode' not released yet, see https://code.launchpad.net/~a-roehler/python-mode/python-mode-components")

(defcustom py-install-directory nil
  "Directory where python-mode.el and it's subdirectories should be installed. Needed for completion and other environment stuff only. "

  :type 'string
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-use-number-face-p nil
  "If digits incl. hex-digits should get an own py-number-face.

Default is nil. With large files fontifying numbers may cause a
delay. Setting of `py-use-number-face-p' has visible effect only
when `py-number-face' was customized differently than inherited
default face. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-start-run-py-shell t
  "If `python-mode' should start a python-shell, `py-shell'. Default is `t'.

A running python-shell presently is needed by complete-functions. "

  :type 'boolean
  :group 'python-mode)

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

(defcustom py-report-level-p nil
  "If indenting functions should report reached indent level.

Default is nil. "

  :type 'boolean
  :group 'python-mode)

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

(defcustom py-complete-function 'py-completion-at-point
  "Function used for completion in buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
		 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)

(defcustom py-shell-complete-function 'py-completion-at-point
  "Function used for completion in buffers. "
  :type '(choice (const :tag "py-completion-at-point" py-completion-at-point)
                 (const :tag "py-shell-complete" py-shell-complete)
		 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python-mode)

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

;; (setq py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-")

(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")
(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python-mode)

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]?\\([iptj]+ython[^ \t\n]*\\)"
    "Detecting the shell in head of file. ")

(setq py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]?\\([iptj]+ython[^ \t\n]*\\)")

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
(make-variable-buffer-local 'py-python-command-args)

(set-default 'py-python-command-args  '("-i"))

(make-obsolete-variable 'py-jpython-command-args 'py-jython-command-args nil)

(defcustom py-jython-command-args '("-i")
  "*List of string arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :group 'python-mode
  :tag "Jython Command Args")

(defcustom py-cleanup-temporary t
  "If temporary buffers and files used by functions executing region should be deleted afterwards. "

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

(defcustom py-continuation-offset 2
  "*Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python-mode)

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

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "*String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :group 'python-mode)

;; ipython.el
;; Recognize the ipython pdb, whose prompt is 'ipdb>' or  'ipydb>'
;;instead of '(Pdb)'
(defvar py-pdbtrack-input-prompt)
(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ ")
(defvar py-pydbtrack-input-prompt)
(setq py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ ")

;; pydb-328837.diff
;; (defconst py-pydbtrack-stack-entry-regexp
;;   "^(\\([-a-zA-Z0-9_/.]*\\):\\([0-9]+\\)):[ \t]?\\(.*\n\\)"
;;   "Regular expression pdbtrack uses to find a stack trace entry for pydb.
;;
;; The debugger outputs program-location lines that look like this:
;;    (/usr/bin/zonetab2pot.py:15): makePOT")

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
(make-obsolete-variable 'py-jpython-packages 'py-jython-packages nil)

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

(defvar py-shell-alist
  '(("jython" . 'jython)
    ("python" . 'cpython))
  "*Alist of interpreters and python shells. Used by `py-choose-shell'
to select the appropriate python interpreter mode for a file.")

(defcustom py-shell-input-prompt-1-regexp "^>>> "
  "*A regular expression to match the input prompt of the shell."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "^[.][.][.] "
  "*A regular expression to match the input prompt of the shell after the
  first line of input."
  :type 'string
  :group 'python-mode)

;; ipython.el
(defvar ipython-de-input-prompt-regexp "\\(?:
In \\[[0-9]+\\]: *.*
----+> \\(.*
\\)[\n]?\\)\\|\\(?:
In \\[[0-9]+\\]: *\\(.*
\\)\\)\\|^[ ]\\{3\\}[.]\\{3,\\}: *\\(.*
\\)"
  "A regular expression to match the IPython input prompt and the python
command after it. The first match group is for a command that is rewritten,
the second for a 'normal' command, and the third for a multiline command.")

(defvar ipython-de-output-prompt-regexp "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defcustom py-shell-switch-buffers-on-execute t
  "When non-nil switch to the Python output buffer. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-split-windows-on-execute t
  "When non-nil split windows, make Python output buffer other. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-hide-show-keywords
  '(
    "class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"
    )
  "Keywords composing visible heads.
Also used by (minor-)outline-mode "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "*Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python-mode)

(defcustom py-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python-mode)

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
  :group 'python-mode)

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
  :group 'python-mode)

(defcustom py-cleanup-temporary t
  "If temporary buffers and files used by functions executing region should be deleted afterwards. "

  :type 'boolean
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

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar py-python-command py-shell-name)
(defvar py-jpython-command py-shell-name)
(defvar py-jython-command py-shell-name)
(defvar py-default-interpreter py-shell-name)
(defvar python-command py-shell-name)

(defcustom python-indent 4
  "Number of columns for a unit of indentation in Python mode.
See also `\\[python-guess-indent]'"
  :group 'python-mode
  :type 'integer)
(put 'python-indent 'safe-local-variable 'integerp)

(defcustom python-guess-indent t
  "Non-nil means Python mode guesses `python-indent' for the buffer."
  :type 'boolean
  :group 'python-mode)

(defcustom imenu-create-index-p t
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

;; for toggling between CPython and JPython
(defvar python-which-shell nil)
(defvar python-which-args python-python-command-args)
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
the if condition."
  :type 'boolean
  :group 'python-mode)

(defcustom py-match-paren-mode nil
  "*Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in py-mode-map.
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

;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defvar py-prev-dir/file nil
  "Caches (directory . file) pair used in the last `py-load-file' command.
Used for determining the default in the next one.")

(defvar py-exception-buffer nil)

(defvar py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

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
  "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^ \t>]+>[^0-9]+\\([0-9]+\\)\\|^[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.
Inludes Python shell-prompt in order to stop further searches. ")
;; (setq py-traceback-line-re
;; "^IPython\\|^In \\[[0-9]+\\]: *\\|^>>>\\|^[^ \t>]+>[^0-9]+\\([0-9]+\\)\\|^[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)")

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
(defvar py-XXX-tag-face 'py-XXX-tag-face)

;; ;; Face for None, True, False, self, and Ellipsis
(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False, Ellipsis."
  :group 'python-mode)
(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defface py-variable-name-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Face method decorators."
  :group 'python-mode)
(defvar py-variable-name-face 'py-variable-name-face)

(defface py-number-face
  '((t (:inherit default)))
  ;; '((t (:inherit 'font-lock-variable-name-face)))
  "Highlight numbers. "
  :group 'python-mode)
(defvar py-number-face 'py-number-face)

;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :group 'python-mode)
(defvar py-decorators-face 'py-decorators-face)

;; Face for builtins
(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :group 'python-mode)
(defvar py-builtins-face 'py-builtins-face)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :group 'python-mode)
(defvar py-class-name-face 'py-class-name-face)

;; XXX, TODO, and FIXME comments and such
(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "."
  :group 'python-mode)

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

(defvar jython-mode-hook nil
  "*Hook called by `jython-mode'. `jython-mode' also calls
`python-mode-hook'.")
(make-obsolete-variable 'jpython-mode-hook 'jython-mode-hook nil)

(defvar py-shell-hook nil
  "*Hook called by `py-shell'.")

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))



(defun py-load-pymacs ()
  "Load Pymacs as delivered with python-mode.el.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (let* ((pyshell (py-choose-shell))
         (path (getenv "PYTHONPATH"))
         (pymacs-installed-p
          (string-match (expand-file-name (concat py-install-directory "/Pymacs")) path)))
    ;; Python side
    (unless pymacs-installed-p
      (setenv "PYTHONPATH" (concat
                            (if path (concat path path-separator))
                            (expand-file-name py-install-directory) "/Pymacs")))

    (if (or (not (boundp 'py-install-directory)) (not (stringp py-install-directory)))
        (error "`py-install-directory' not set, see INSTALL")
      (load (concat py-install-directory "/pymacs.el") nil t)
      (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                  "python"
                                pyshell))
      (autoload 'pymacs-apply "pymacs")
      (autoload 'pymacs-call "pymacs")
      (autoload 'pymacs-eval "pymacs")
      (autoload 'pymacs-exec "pymacs")
      (autoload 'pymacs-load "pymacs")
      (require 'pymacs))
    (unwind-protect
        (progn
          (find-file (concat py-install-directory "/completion/pycomplete.el"))
          (eval-buffer)))
    (kill-buffer "pycomplete.el")))

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
         ;; (add-to-list 'load-path (concat py-install-directory "/pymacs"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/test"))
         (add-to-list 'load-path (concat (expand-file-name py-install-directory) "/tools")))
        ((when py-guess-py-install-directory-p
         (py-guess-py-install-directory)))
        (t (error "Please set `py-install-directory', see INSTALL"))
        (when (interactive-p) (message "%s" load-path))))

(when (boundp 'py-install-directory) (py-set-load-path))
;; don't require `py-install-directory' for now
(py-set-load-path)

(add-to-list 'interpreter-mode-alist (cons (purecopy "jython") 'jython-mode))
(add-to-list 'interpreter-mode-alist (cons (purecopy "python") 'python-mode))
(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))
(add-to-list 'same-window-buffer-names (purecopy "*Python*"))

;;; Python specialized rx

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

(eval-when-compile
  (defconst python-rx-constituents
    (list
     `(block-start          . ,(rx symbol-start
                                   (or "def" "class" "if" "elif" "else" "try"
                                       "except" "finally" "for" "while" "with")
                                   symbol-end))
     `(decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
     `(defun                . ,(rx symbol-start (or "def" "class") symbol-end))
     `(symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
     `(open-paren           . ,(rx (or "{" "[" "(")))
     `(close-paren          . ,(rx (or "}" "]" ")")))
     `(simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
     `(not-simple-operator  . ,(rx (not (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
     `(operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
     `(assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
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

(defun python-info-ppss-context (type &optional syntax-ppss)
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

;; Credits to github.com/fgallina/python.el/issues42
(defvar font-lock-number "[0-9]+\\([eE][+-]?[0-9]*\\)?")
(defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")

(defvar python-font-lock-keywords)
(setq python-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
                 "assert" "else" "if" "pass" "yield" "break" "import"
                 "print" "exec" "in" "continue" "finally" "is"
                 "return" "def" "for" "lambda" "try" "self")
             symbol-end)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start (group "class") (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-keyword-face) (2 py-class-name-face))
        (,(rx symbol-start
              (or "raise" "except")
              symbol-end) . py-exception-name-face)
        ;; Constants
        (,(rx symbol-start
              ;; copyright, license, credits, quit, exit are added by the
              ;; site module and since they are not intended to be used in
              ;; programs they are not added here either.
              (or "None" "True" "False" "__debug__" "NotImplemented")
              symbol-end) . font-lock-constant-face)
        (,(rx symbol-start
              (or "cls" "self" "cls" "Ellipsis" "True" "False" "None")
              symbol-end) . py-pseudo-keyword-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 py-decorators-face))
        ;; '("\\_<raise[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_.]*\\)" 1 py-exception-name-face)
        ;; '("[ \t]*\\(_\\{0,2\\}[a-zA-Z][a-zA-Z_0-9.]+_\\{0,2\\}\\) *\\(+\\|-\\|*\\|*\\*\\|/\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\)? ?=[^=\n]"
        (,(python-rx line-start (* (any " \t"))(group (** 0 2 "_") word (0+ (or word ?_))(** 0 2 "_"))(* (any " \t")) assignment-operator)
         1 py-variable-name-face)

        ;; Builtin Exceptions
        (,(rx symbol-start
              (or "ArithmeticError" "AssertionError" "AttributeError"
                  "BaseException" "BufferError" "BytesWarning" "DeprecationWarning"
                  "EOFError" "EnvironmentError" "Exception" "FloatingPointError"
                  "FutureWarning" "GeneratorExit" "IOError" "ImportError"
                  "ImportWarning" "IndentationError" "IndexError" "KeyError"
                  "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
                  "NotImplementedError" "OSError" "OverflowError"
                  "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
                  "RuntimeWarning" "StandardError" "StopIteration" "SyntaxError"
                  "SyntaxWarning" "SystemError" "SystemExit" "TabError" "TypeError"
                  "UnboundLocalError" "UnicodeDecodeError" "UnicodeEncodeError"
                  "UnicodeError" "UnicodeTranslateError" "UnicodeWarning"
                  "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
              symbol-end) . py-exception-name-face)
        ;; Builtins
        (,(rx symbol-start
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
              symbol-end) . font-lock-builtin-face)
        ;; asignations
        ;; support for a = b = c = 5
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_)))
                                 (? ?\[ (+ (not (any ?\]))) ?\]) (* space)
                                 assignment-operator)))
              (when (re-search-forward re limit t)
                (while (and (python-info-ppss-context 'paren)
                            (re-search-forward re limit t)))
                (if (and (not (python-info-ppss-context 'paren))
                         (not (equal (char-after (point-marker)) ?=)))
                    t
                  (set-match-data nil)))))
         (1 font-lock-variable-name-face nil nil))
        ;; support for a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                                 (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                 ?, (* space) (+ (any word ?. ?_)) (* space)
                                 assignment-operator)))
              (when (and (re-search-forward re limit t)
                         (goto-char (nth 3 (match-data))))
                (while (and (python-info-ppss-context 'paren)
                            (re-search-forward re limit t))
                  (goto-char (nth 3 (match-data))))
                (if (not (python-info-ppss-context 'paren))
                    t
                  (set-match-data nil)))))
         (1 font-lock-variable-name-face nil nil))))

(when py-use-number-face-p
  (add-to-list 'python-font-lock-keywords '("\\([0-9]+\\([eE][+-]?[0-9]*\\)?\\|0[xX][0-9a-fA-F]+\\)" 1 py-number-face)))

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

(defvar python-dotty-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Python files.
It makes underscores and dots word constituent chars.")


;;; Keymap
(defvar python-mode-map)
(setq python-mode-map
      (let ((map (make-sparse-keymap)))
        ;; Mostly taken from python-mode.el.
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        (define-key map [(control c)(control a)] 'py-mark-statement)
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
            ["Customize Python mode" (customize-group 'python-mode)
             :help "Open the customization buffer for Python mode"]
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

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map [(meta p)] 'py-beginning-of-statement)
            (define-key python-mode-map [(meta n)] 'py-end-of-statement)
            (defvar py-mode-map python-mode-map))
          (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
          (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class))

(defvar python-shell-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map [tab]   'tab-to-tab-stop)
    (define-key map "\C-c-" 'py-up-exception)
    (define-key map "\C-c=" 'py-down-exception)
    map)
  "Keymap used in *Python* shell buffers.")

(defvar python-mode-syntax-table
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
    table))

;;;; Utility stuff

(defsubst python-in-string/comment ()
  "Return non-nil if point is in a Python literal (a comment or string)."
  ;; We don't need to save the match data.
  (nth 8 (syntax-ppss)))

(defconst python-space-backslash-table
  (let ((table (copy-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?\\ " " table)
    table)
  "`python-mode-syntax-table' with backslash given whitespace syntax.")

(defun python-skip-comments/blanks (&optional backward)
  "Skip comments and blank lines.
BACKWARD non-nil means go backwards, otherwise go forwards.
Backslash is treated as whitespace so that continued blank lines
are skipped.  Doesn't move out of comments -- should be outside
or at end of line."
  (let ((arg (if backward
		 ;; If we're in a comment (including on the trailing
		 ;; newline), forward-comment doesn't move backwards out
		 ;; of it.  Don't set the syntax table round this bit!
		 (let ((syntax (syntax-ppss)))
		   (if (nth 4 syntax)
		       (goto-char (nth 8 syntax)))
		   (- (point-max)))
	       (point-max))))
    (with-syntax-table python-space-backslash-table
      (forward-comment arg))))

(defun python-backslash-continuation-line-p ()
  "Non-nil if preceding line ends with backslash that is not in a comment."
  (and (eq ?\\ (char-before (line-end-position 0)))
       (not (syntax-ppss-context (syntax-ppss)))))

(defun python-continuation-line-p ()
  "Return non-nil if current line continues a previous one.
The criteria are that the previous line ends in a backslash outside
comments and strings, or that point is within brackets/parens."
  (or (python-backslash-continuation-line-p)
      (let ((depth (syntax-ppss-depth
		    (save-excursion ; syntax-ppss with arg changes point
		      (syntax-ppss (line-beginning-position))))))
	(or (> depth 0)
	    (if (< depth 0)	  ; Unbalanced brackets -- act locally
		(save-excursion
		  (condition-case ()
		      (progn (backward-up-list) t) ; actually within brackets
		    (error nil))))))))

(defun python-comment-line-p ()
  "Return non-nil if and only if current line has only a comment."
  (save-excursion
    (end-of-line)
    (when (eq 'comment (syntax-ppss-context (syntax-ppss)))
      (back-to-indentation)
      (looking-at (rx (or (syntax comment-start) line-end))))))

(defun python-blank-line-p ()
  "Return non-nil if and only if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\s-*$")))

(defun python-beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun python-open-block-statement-p (&optional bos)
  "Return non-nil if statement at point opens a block.
BOS non-nil means point is known to be at beginning of statement."
  (save-excursion
    (unless bos (python-beginning-of-statement))
    (looking-at (rx (and (or "if" "else" "elif" "while" "for" "def"
			     "class" "try" "except" "finally" "with")
			 symbol-end)))))

(defun python-close-block-statement-p (&optional bos)
  "Return non-nil if current line is a statement closing a block.
BOS non-nil means point is at beginning of statement.
The criteria are that the line isn't a comment or in string and
 starts with keyword `raise', `break', `continue' or `pass'."
  (save-excursion
    (unless bos (python-beginning-of-statement))
    (back-to-indentation)
    (looking-at (rx (or "return" "raise" "break" "continue" "pass")
		    symbol-end))))

(defun python-outdent-p ()
  "Return non-nil if current line should outdent a level."
  (save-excursion
    (back-to-indentation)
    (and (looking-at (rx (and (or "else" "finally" "except" "elif")
			      symbol-end)))
	 (not (python-in-string/comment))
	 ;; Ensure there's a previous statement and move to it.
	 (zerop (python-previous-statement))
	 (not (python-close-block-statement-p t))
	 ;; Fixme: check this
	 (not (python-open-block-statement-p)))))

;;;; Indentation.

;; Add a designator to the minor mode strings
(or (assq 'python-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(python-pdbtrack-is-tracking-p python-pdbtrack-minor-mode-string)
	  minor-mode-alist))

;; Bind python-file-queue before installing the kill-emacs-hook.
(defvar python-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar python-pdbtrack-is-tracking-p nil)

(defconst python-pdbtrack-stack-entry-regexp
  "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst python-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
  "Regular expression pdbtrack uses to recognize a pdb prompt.")

(defconst python-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defun python-guess-indent ()
  "Guess step for indentation of current buffer.
Set `python-indent' locally to the value guessed."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (done indent)
	(while (and (not done) (not (eobp)))
	  (when (and (re-search-forward (rx ?: (0+ space)
					    (or (syntax comment-start)
						line-end))
					nil 'move)
		     (python-open-block-statement-p))
	    (save-excursion
	      (python-beginning-of-statement)
	      (let ((initial (current-indentation)))
		(if (zerop (python-next-statement))
		    (setq indent (- (current-indentation) initial)))
		(if (and indent (>= indent 2) (<= indent 8)) ; sanity check
		    (setq done t))))))
	(when done
	  (when (/= indent (default-value 'python-indent))
	    (set (make-local-variable 'python-indent) indent)
	    (unless (= tab-width python-indent)
	      (setq indent-tabs-mode nil)))
	  indent)))))

;; Alist of possible indentations and start of statement they would
;; close.  Used in indentation cycling (below).
(defvar python-indent-list nil
  "Internal use.")
;; Length of the above
(defvar python-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar python-indent-index nil
  "Internal use.")

(defun python-calculate-indentation ()
  "Calculate Python indentation for line at point."
  (setq python-indent-list nil
	python-indent-list-length 1)
  (save-excursion
    (beginning-of-line)
    (let ((syntax (syntax-ppss))
	  start)
      (cond
       ((eq 'string (syntax-ppss-context syntax)) ; multi-line string
	(if (not python-indent-string-contents)
	    (current-indentation)
	  ;; Only respect `python-indent-string-contents' in doc
	  ;; strings (defined as those which form statements).
	  (if (not (save-excursion
		     (python-beginning-of-statement)
		     (looking-at (rx (or (syntax string-delimiter)
					 (syntax string-quote))))))
	      (current-indentation)
	    ;; Find indentation of preceding non-blank line within string.
	    (setq start (nth 8 syntax))
	    (forward-line -1)
	    (while (and (< start (point)) (looking-at "\\s-*$"))
	      (forward-line -1))
	    (current-indentation))))
       ((python-continuation-line-p)   ; after backslash, or bracketed
	(let ((point (point))
	      (open-start (cadr syntax))
	      (backslash (python-backslash-continuation-line-p))
	      (colon (eq ?: (char-before (1- (line-beginning-position))))))
	  (if open-start
	      ;; Inside bracketed expression.
	      (progn
		(goto-char (1+ open-start))
		;; Look for first item in list (preceding point) and
		;; align with it, if found.
		(if (with-syntax-table python-space-backslash-table
		      (let ((parse-sexp-ignore-comments t))
			(condition-case ()
			    (progn (forward-sexp)
				   (backward-sexp)
				   (< (point) point))
			  (error nil))))
		    ;; Extra level if we're backslash-continued or
		    ;; following a key.
		    (if (or backslash colon)
			(+ python-indent (current-column))
                      (current-column))
		  ;; Otherwise indent relative to statement start, one
		  ;; level per bracketing level.
		  (goto-char (1+ open-start))
		  (python-beginning-of-statement)
		  (+ (current-indentation) (* (car syntax) python-indent))))
	    ;; Otherwise backslash-continued.
	    (forward-line -1)
	    (if (python-continuation-line-p)
		;; We're past first continuation line.  Align with
		;; previous line.
		(current-indentation)
	      ;; First continuation line.  Indent one step, with an
	      ;; extra one if statement opens a block.
	      (python-beginning-of-statement)
	      (+ (current-indentation) python-continuation-offset
		 (if (python-open-block-statement-p t)
		     python-indent
		   0))))))
       ((bobp) 0)
       ;; Fixme: Like python-mode.el; not convinced by this.
       ((looking-at (rx (0+ space) (syntax comment-start)
			(not (any " \t\n")))) ; non-indentable comment
	(current-indentation))
       ((and python-honour-comment-indentation
	     ;; Back over whitespace, newlines, non-indentable comments.
	     (catch 'done
	       (while (cond ((bobp) nil)
			    ((not (forward-comment -1))
			     nil)	; not at comment start
			    ;; Now at start of comment -- trailing one?
			    ((/= (current-column) (current-indentation))
			     nil)
			    ;; Indentable comment, like python-mode.el?
			    ((and (looking-at (rx (syntax comment-start)
						  (or space line-end)))
				  (/= 0 (current-column)))
			     (throw 'done (current-column)))
			    ;; Else skip it (loop).
			    (t))))))
       (t
	(python-indentation-levels)
	;; Prefer to indent comments with an immediately-following
	;; statement, e.g.
	;;       ...
	;;   # ...
	;;   def ...
	(when (and (> python-indent-list-length 1)
		   (python-comment-line-p))
	  (forward-line)
	  (unless (python-comment-line-p)
	    (let ((elt (assq (current-indentation) python-indent-list)))
	      (setq python-indent-list
		    (nconc (delete elt python-indent-list)
			   (list elt))))))
	(caar (last python-indent-list)))))))

;;;; Cycling through the possible indentations with successive TABs.

;; These don't need to be buffer-local since they're only relevant
;; during a cycle.

(defun python-initial-text ()
  "Text of line following indentation and ignoring any trailing comment."
  (save-excursion
    (buffer-substring (progn
			(back-to-indentation)
			(point))
		      (progn
			(end-of-line)
			(forward-comment -1)
			(point)))))

(defconst python-block-pairs
  '(("else" "if" "elif" "while" "for" "try" "except")
    ("elif" "if" "elif")
    ("except" "try" "except")
    ("finally" "else" "try" "except"))
  "Alist of keyword matches.
The car of an element is a keyword introducing a statement which
can close a block opened by a keyword in the cdr.")

(defun python-first-word ()
  "Return first word (actually symbol) on the line."
  (save-excursion
    (back-to-indentation)
    (current-word t)))

(defun python-indentation-levels ()
  "Return a list of possible indentations for this line.
It is assumed not to be a continuation line or in a multi-line string.
Includes the default indentation and those which would close all
enclosing blocks.  Elements of the list are actually pairs:
\(INDENTATION . TEXT), where TEXT is the initial text of the
corresponding block opening (or nil)."
  (save-excursion
    (let ((initial "")
	  levels indent)
      ;; Only one possibility immediately following a block open
      ;; statement, assuming it doesn't have a `suite' on the same line.
      (cond
       ((save-excursion (and (python-previous-statement)
			     (python-open-block-statement-p t)
			     (setq indent (current-indentation))
			     ;; Check we don't have something like:
			     ;;   if ...: ...
			     (if (progn (python-end-of-statement)
					(python-skip-comments/blanks t)
					(eq ?: (char-before)))
				 (setq indent (+ python-indent indent)))))
	(push (cons indent initial) levels))
       ;; Only one possibility for comment line immediately following
       ;; another.
       ((save-excursion
	  (when (python-comment-line-p)
	    (forward-line -1)
	    (if (python-comment-line-p)
		(push (cons (current-indentation) initial) levels)))))
       ;; Fixme: Maybe have a case here which indents (only) first
       ;; line after a lambda.
       (t
	(let ((start (car (assoc (python-first-word) python-block-pairs))))
	  (python-previous-statement)
	  ;; Is this a valid indentation for the line of interest?
	  (unless (or (if start		; potentially only outdentable
			  ;; Check for things like:
			  ;;   if ...: ...
			  ;;   else ...:
			  ;; where the second line need not be outdented.
			  (not (member (python-first-word)
				       (cdr (assoc start
						   python-block-pairs)))))
		      ;; Not sensible to indent to the same level as
		      ;; previous `return' &c.
		      (python-close-block-statement-p))
	    (push (cons (current-indentation) (python-initial-text))
		  levels))
	  (while (python-beginning-of-block)
	    (when (or (not start)
		      (member (python-first-word)
			      (cdr (assoc start python-block-pairs))))
	      (push (cons (current-indentation) (python-initial-text))
		    levels))))))
      (prog1 (or levels (setq levels '((0 . ""))))
	(setq python-indent-list levels
	      python-indent-list-length (length python-indent-list))))))

;; This is basically what `python-indent-line' would be if we didn't
;; do the cycling.
(defun python-indent-line-1 (&optional leave)
  "Subroutine of `python-indent-line'.
Does non-repeated indentation.  LEAVE non-nil means leave
indentation if it is valid, i.e. one of the positions returned by
`python-calculate-indentation'."
  (let ((target (python-calculate-indentation))
	(pos (- (point-max) (point))))
    (if (or (= target (current-indentation))
	    ;; Maybe keep a valid indentation.
	    (and leave python-indent-list
		 (assq (current-indentation) python-indent-list)))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))))

(defun python-indent-line ()
  "Indent current line as Python code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command
different from `indent-for-tab-command', i.e. successive TABs do
the cycling."
  (interactive)
  (if (and (eq this-command 'indent-for-tab-command)
	   (eq last-command this-command))
      (if (= 1 python-indent-list-length)
	  (message "Sole indentation")
	(progn (setq python-indent-index
		     (% (1+ python-indent-index) python-indent-list-length))
	       (beginning-of-line)
	       (delete-horizontal-space)
	       (indent-to (car (nth python-indent-index python-indent-list)))
	       (if (python-block-end-p)
		   (let ((text (cdr (nth python-indent-index
					 python-indent-list))))
		     (if text
			 (message "Closes: %s" text))))))
    (python-indent-line-1)
    (setq python-indent-index (1- python-indent-list-length))))

(defun python-indent-region (start end)
  "`indent-region-function' for Python.
Leaves validly-indented lines alone, i.e. doesn't indent to
another valid position."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (or (bolp) (forward-line 1))
    (while (< (point) end)
      (or (and (bolp) (eolp))
	  (python-indent-line-1 t))
      (forward-line 1))
    (move-marker end nil)))

(defun python-block-end-p ()
  "Non-nil if this is a line in a statement closing a block,
or a blank line indented to where it would close a block."
  (and (not (python-comment-line-p))
       (or (python-close-block-statement-p t)
	   (< (current-indentation)
	      (save-excursion
		(python-previous-statement)
		(current-indentation))))))

;;;; Movement.

;; Fixme:  Define {for,back}ward-sexp-function?  Maybe skip units like
;; block, statement, depending on context.

(defun python-beginning-of-defun ()
  "`beginning-of-defun-function' for Python.
Finds beginning of innermost nested class or method definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (let ((ci (current-indentation))
	(def-re (rx line-start (0+ space) (or "def" "class") (1+ space)
		    (group (1+ (or word (syntax symbol))))))
	found lep) ;; def-line
    (if (python-comment-line-p)
	(setq ci most-positive-fixnum))
    (while (and (not (bobp)) (not found))
      ;; Treat bol at beginning of function as outside function so
      ;; that successive C-M-a makes progress backwards.
      ;;(setq def-line (looking-at def-re))
      (unless (bolp) (end-of-line))
      (setq lep (line-end-position))
      (if (and (re-search-backward def-re nil 'move)
	       ;; Must be less indented or matching top level, or
	       ;; equally indented if we started on a definition line.
	       (let ((in (current-indentation)))
		 (or (and (zerop ci) (zerop in))
		     (= lep (line-end-position)) ; on initial line
		     ;; Not sure why it was like this -- fails in case of
		     ;; last internal function followed by first
		     ;; non-def statement of the main body.
                     ;; 		     (and def-line (= in ci))
		     (= in ci)
		     (< in ci)))
	       (not (python-in-string/comment)))
	  (setq found t)))
    found))

(defun python-end-of-defun ()
  "`end-of-defun-function' for Python.
Finds end of innermost nested class or method definition."
  (let ((orig (point))
	(pattern (rx line-start (0+ space) (or "def" "class") space)))
    ;; Go to start of current block and check whether it's at top
    ;; level.  If it is, and not a block start, look forward for
    ;; definition statement.
    (when (python-comment-line-p)
      (end-of-line)
      (forward-comment most-positive-fixnum))
    (if (not (python-open-block-statement-p))
	(python-beginning-of-block))
    (if (zerop (current-indentation))
	(unless (python-open-block-statement-p)
	  (while (and (re-search-forward pattern nil 'move)
		      (python-in-string/comment))) ; just loop
	  (unless (eobp)
	    (beginning-of-line)))
      ;; Don't move before top-level statement that would end defun.
      (end-of-line)
      (python-beginning-of-defun))
    ;; If we got to the start of buffer, look forward for
    ;; definition statement.
    (if (and (bobp) (not (looking-at "def\\|class")))
	(while (and (not (eobp))
		    (re-search-forward pattern nil 'move)
		    (python-in-string/comment)))) ; just loop
    ;; We're at a definition statement (or end-of-buffer).
    (unless (eobp)
      (python-end-of-block)
      ;; Count trailing space in defun (but not trailing comments).
      (skip-syntax-forward " >")
      (unless (eobp)			; e.g. missing final newline
	(beginning-of-line)))
    ;; Catch pathological cases like this, where the beginning-of-defun
    ;; skips to a definition we're not in:
    ;; if ...:
    ;;     ...
    ;; else:
    ;;     ...  # point here
    ;;     ...
    ;;     def ...
    (if (< (point) orig)
	(goto-char (point-max)))))

(defun python-beginning-of-statement ()
  "Go to start of current statement.
Accounts for continuation lines, multi-line strings, and
multi-line bracketed expressions."
  (beginning-of-line)
  (python-beginning-of-string)
  (let (point)
    (while (and (python-continuation-line-p)
		(if point
		    (< (point) point)
		  t))
      (beginning-of-line)
      (if (python-backslash-continuation-line-p)
	  (progn
	    (forward-line -1)
	    (while (python-backslash-continuation-line-p)
	      (forward-line -1)))
	(python-beginning-of-string)
	(python-skip-out))
      (setq point (point))))
  (back-to-indentation))

(defun python-skip-out (&optional forward syntax)
  "Skip out of any nested brackets.
Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil it is the state returned by `syntax-ppss' at point.
Return non-nil if and only if skipping was done."
  (let ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
	(forward (if forward -1 1)))
    (unless (zerop depth)
      (if (> depth 0)
	  ;; Skip forward out of nested brackets.
	  (condition-case ()		; beware invalid syntax
	      (progn (backward-up-list (* forward depth)) t)
	    (error nil))
	;; Invalid syntax (too many closed brackets).
	;; Skip out of as many as possible.
	(let (done)
	  (while (condition-case ()
		     (progn (backward-up-list forward)
			    (setq done t))
		   (error nil)))
	  done)))))

(defun python-end-of-statement ()
  "Go to the end of the current statement and return point.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines.
On a comment line, go to end of line."
  (end-of-line)
  (while (let (comment)
	   ;; Move past any enclosing strings and sexps, or stop if
	   ;; we're in a comment.
	   (while (let ((s (syntax-ppss)))
		    (cond ((eq 'comment (syntax-ppss-context s))
			   (setq comment t)
			   nil)
			  ((eq 'string (syntax-ppss-context s))
			   ;; Go to start of string and skip it.
                           (let ((pos (point)))
                             (goto-char (nth 8 s))
                             (condition-case () ; beware invalid syntax
                                 (progn (forward-sexp) t)
                               ;; If there's a mismatched string, make sure
                               ;; we still overall move *forward*.
                               (error (goto-char pos) (end-of-line)))))
			  ((python-skip-out t s))))
	     (end-of-line))
	   (unless comment
	     (eq ?\\ (char-before))))	; Line continued?
    (end-of-line 2))			; Try next line.
  (point))

(defun python-previous-statement (&optional count)
  "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (python-next-statement (- count))
    (python-beginning-of-statement)
    (while (and (> count 0) (not (bobp)))
      (python-skip-comments/blanks t)
      (python-beginning-of-statement)
      (unless (bobp) (setq count (1- count))))
    count))

(defun python-next-statement (&optional count)
  "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0)
      (python-previous-statement (- count))
    (beginning-of-line)
    (let (bogus)
      (while (and (> count 0) (not (eobp)) (not bogus))
	(python-end-of-statement)
	(python-skip-comments/blanks)
	(if (eq 'string (syntax-ppss-context (syntax-ppss)))
	    (setq bogus t)
	  (unless (eobp)
	    (setq count (1- count))))))
    count))

(defun python-beginning-of-block (&optional arg)
  "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`python-end-of-block' instead.
If point is on the first line of a block, use its outer block.
If current statement is in column zero, don't move and return nil.
Otherwise return non-nil."
  (interactive "p")
  (unless arg (setq arg 1))
  (cond
   ((zerop arg))
   ((< arg 0) (python-end-of-block (- arg)))
   (t
    (let ((point (point)))
      (if (or (python-comment-line-p)
	      (python-blank-line-p))
	  (python-skip-comments/blanks t))
      (python-beginning-of-statement)
      (let ((ci (current-indentation)))
	(if (zerop ci)
	    (not (goto-char point))	; return nil
	  ;; Look upwards for less indented statement.
	  (if (catch 'done
;;; This is slower than the below.
;;; 	  (while (zerop (python-previous-statement))
;;; 	    (when (and (< (current-indentation) ci)
;;; 		       (python-open-block-statement-p t))
;;; 	      (beginning-of-line)
;;; 	      (throw 'done t)))
		(while (and (zerop (forward-line -1)))
		  (when (and (< (current-indentation) ci)
			     (not (python-comment-line-p))
			     ;; Move to beginning to save effort in case
			     ;; this is in string.
			     (progn (python-beginning-of-statement) t)
			     (python-open-block-statement-p t))
		    (beginning-of-line)
		    (throw 'done t)))
		(not (goto-char point))) ; Failed -- return nil
	      (python-beginning-of-block (1- arg)))))))))

(defun python-end-of-block (&optional arg)
  "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `python-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block,
don't move and return nil.  Otherwise return t."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (python-beginning-of-block (- arg))
    (while (and (> arg 0)
		(let* ((point (point))
		       (_ (if (python-comment-line-p)
			      (python-skip-comments/blanks t)))
		       (ci (current-indentation))
		       (open (python-open-block-statement-p)))
		  (if (and (zerop ci) (not open))
		      (not (goto-char point))
		    (catch 'done
		      (while (zerop (python-next-statement))
			(when (or (and open (<= (current-indentation) ci))
				  (< (current-indentation) ci))
			  (python-skip-comments/blanks t)
			  (beginning-of-line 2)
			  (throw 'done t)))))))
      (setq arg (1- arg)))
    (zerop arg)))

(defvar python-which-func-length-limit 40
  "Non-strict length limit for `python-which-func' output.")

(defun python-which-func ()
  (let ((function-name (python-current-defun python-which-func-length-limit)))
    (set-text-properties 0 (length function-name) nil function-name)
    function-name))


;;;; Imenu.

;; For possibily speeding this up, here's the top of the ELP profile
;; for rescanning pydoc.py (2.2k lines, 90kb):
;; Function Name Call Count Elapsed Time Average Time
;; ====================================  ==========  =============  ============
;; python-imenu-create-index 156 2.430906 0.0155827307
;; python-end-of-defun 155 1.2718260000 0.0082053290
;; python-end-of-block 155 1.1898689999 0.0076765741
;; python-next-statement 2970 1.024717 0.0003450225
;; python-end-of-statement 2970 0.4332190000 0.0001458649
;; python-beginning-of-defun 265 0.0918479999 0.0003465962
;; python-skip-comments/blanks 3125 0.0753319999 2.410...e-05

(defvar python-recursing)
(defun python-imenu-create-index ()
  "`imenu-create-index-function' for Python.

Makes nested Imenu menus from nested `class' and `def' statements.
The nested menus are headed by an item referencing the outer
definition; it has a space prepended to the name so that it sorts
first with `imenu--sort-by-name' (though, unfortunately, sub-menus
precede it)."
  (unless (boundp 'python-recursing)	; dynamically bound below
    ;; Normal call from Imenu.
    (goto-char (point-min))
    ;; Without this, we can get an infloop if the buffer isn't all
    ;; fontified.  I guess this is really a bug in syntax.el.  OTOH,
    ;; _with_ this, imenu doesn't immediately work; I can't figure out
    ;; what's going on, but it must be something to do with timers in
    ;; font-lock.
    ;; This can't be right, especially not when jit-lock is not used.  --Stef
    ;; (unless (get-text-property (1- (point-max)) 'fontified)
    ;;   (font-lock-fontify-region (point-min) (point-max)))
    )
  (let (index-alist)			; accumulated value to return
    (while (re-search-forward
	    (rx line-start (0+ space)	; leading space
		(or (group "def") (group "class"))	   ; type
		(1+ space) (group (1+ (or word ?_))))	   ; name
	    nil t)
      (unless (python-in-string/comment)
	(let ((pos (match-beginning 0))
	      (name (match-string-no-properties 3)))
	  (if (match-beginning 2)	; def or class?
	      (setq name (concat "class " name)))
	  (save-restriction
	    (narrow-to-defun)
	    (let* ((python-recursing t)
		   (sublist (python-imenu-create-index)))
	      (if sublist
		  (progn (push (cons (concat " " name) pos) sublist)
			 (push (cons name sublist) index-alist))
		(push (cons name pos) index-alist)))))))
    (unless (boundp 'python-recursing)
      ;; Look for module variables.
      (let (vars)
	(goto-char (point-min))
	(while (re-search-forward
		(rx line-start (group (1+ (or word ?_))) (0+ space) "=")
		nil t)
	  (unless (python-in-string/comment)
	    (push (cons (match-string 1) (match-beginning 1))
		  vars)))
	(setq index-alist (nreverse index-alist))
	(if vars
	    (push (cons "Module variables"
			(nreverse vars))
		  index-alist))))
    index-alist))

;;;; `Electric' commands.

(defun python-electric-colon (arg)
  "Insert a colon and maybe outdent the line if it is a statement like `else'.
With numeric ARG, just insert that many colons.  With \\[universal-argument],
just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (and (not arg)
       (eolp)
       (python-outdent-p)
       (not (python-in-string/comment))
       (> (current-indentation) (python-calculate-indentation))
       (python-indent-line)))		; OK, do it
(put 'python-electric-colon 'delete-selection t)

(defun python-backspace (arg)
  "Maybe delete a level of indentation on the current line.
Do so if point is at the end of the line's indentation outside
strings and comments.
Otherwise just call `backward-delete-char-untabify'.
Repeat ARG times."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (python-continuation-line-p)
	  (python-in-string/comment))
      (backward-delete-char-untabify arg)
    ;; Look for the largest valid indentation which is smaller than
    ;; the current indentation.
    (let ((indent 0)
	  (ci (current-indentation))
	  (indents (python-indentation-levels))
	  initial)
      (dolist (x indents)
	(if (< (car x) ci)
	    (setq indent (max indent (car x)))))
      (setq initial (cdr (assq indent indents)))
      (if (> (length initial) 0)
	  (message "Closes %s" initial))
      (delete-horizontal-space)
      (indent-to indent))))
(put 'python-backspace 'delete-selection 'supersede)


;;;; Inferior mode stuff (following cmuscheme).

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

Use \\[python-set-proc] to set the default value from a buffer with a
local value.")
(make-variable-buffer-local 'python-buffer)

(defvar inferior-python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; This will inherit from comint-mode-map.
    (define-key map "\C-c\C-l" 'python-load-file)
    (define-key map "\C-c\C-v" 'python-check)
    ;; Note that we _can_ still use these commands which send to the
    ;; Python process even at the prompt iff we have a normal prompt,
    ;; i.e. '>>> ' and not '... '.  See the comment before
    ;; python-send-region.  Fixme: uncomment these if we address that.

    ;; (define-key map [(meta ?\t)] 'python-complete-symbol)
    ;; (define-key map "\C-c\C-f" 'python-describe-symbol)
    map))

(defvar inferior-python-mode-syntax-table
  (let ((st (make-syntax-table python-mode-syntax-table)))
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
  (let ((prompt (cdr-safe (or (assoc python-python-command
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
 * \\[python-switch-to-python] switches the current buffer to the Python
    process buffer.
 * \\[python-send-region] sends the current region to the Python process.
 * \\[python-send-region-and-go] switches to the Python process buffer
    after sending the text.
For running multiple processes in multiple buffers, see `run-python' and
`python-buffer'.

\\{inferior-python-mode-map}"
  :group 'python-mode
  (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'comint-input-filter) 'python-input-filter)
  (add-hook 'comint-preoutput-filter-functions #'python-preoutput-filter
	    nil t)
  (python--set-prompt-regexp)
  (set (make-local-variable 'compilation-error-regexp-alist)
       python-compilation-regexp-alist)
  (compilation-shell-minor-mode 1))

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
      (python-send-string "import emacs")
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
  "Like `python-send-string' but resets `compilation-shell-minor-mode'."
  (when (python-check-comint-prompt)
    (with-current-buffer (process-buffer (python-proc))
      (goto-char (point-max))
      (compilation-forget-errors)
      (python-send-string command)
      (setq compilation-last-buffer (current-buffer)))))

(defun python-send-region (start end)
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

(defun python-send-buffer ()
  "Send the current buffer to the inferior Python process."
  (interactive)
  (python-send-region (point-min) (point-max)))

;; Fixme: Try to define the function or class within the relevant
;; module, not just at top level.
(defun python-send-defun ()
  "Send the current defun (class or method) to the inferior Python process."
  (interactive)
  (save-excursion (python-send-region (progn (beginning-of-defun) (point))
				      (progn (end-of-defun) (point)))))

(defun python-switch-to-python (eob-p)
  "Switch to the Python process buffer, maybe starting new process.
With prefix arg, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer (process-buffer (python-proc)) t) ;Runs python if needed.
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun python-send-region-and-go (start end)
  "Send the region to the inferior Python process.
Then switch to the process buffer."
  (interactive "r")
  (python-send-region start end)
  (python-switch-to-python t))

(defvar python-prev-dir/file nil
  "Caches (directory . file) pair used in the last `python-load-file' command.
Used for determining the default in the next one.")

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

(defun python-set-proc ()
  "Set the default value of `python-buffer' to correspond to this buffer.
If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from \\[python-send-region] et al when used
in a buffer that doesn't have a local value of `python-buffer'."
  (interactive)
  (if (local-variable-p 'python-buffer)
      (setq-default python-buffer python-buffer)
    (error "No local value of `python-buffer'")))

;;;; Context-sensitive help.

(defconst python-dotty-syntax-table
  (let ((table (make-syntax-table)))
    (set-char-table-parent table python-mode-syntax-table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table giving `.' symbol syntax.
Otherwise inherits from `python-mode-syntax-table'.")

(defvar view-return-to-alist)
(defvar python-imports)			; forward declaration

;; Fixme: Should this actually be used instead of info-look, i.e. be
;; bound to C-h S?  [Probably not, since info-look may work in cases
;; where this doesn't.]
;; (defun python-describe-symbol (symbol)
;;   "Get help on SYMBOL using `help'.
;; Interactively, prompt for symbol.
;;
;; Symbol may be anything recognized by the interpreter's `help'
;; command -- e.g. `CALLS' -- not just variables in scope in the
;; interpreter.  This only works for Python version 2.2 or newer
;; since earlier interpreters don't support `help'.
;;
;; In some cases where this doesn't find documentation, \\[info-lookup-symbol]
;; will."
;;   ;; Note that we do this in the inferior process, not a separate one, to
;;   ;; ensure the environment is appropriate.
;;   (interactive
;;    (let ((symbol (with-syntax-table python-dotty-syntax-table
;; 		   (current-word)))
;; 	 (enable-recursive-minibuffers t))
;;      (list (read-string (if symbol
;; 			    (format "Describe symbol (default %s): " symbol)
;; 			  "Describe symbol: ")
;; 			nil nil symbol))))
;;   (if (equal symbol "") (error "No symbol"))
;;   ;; Ensure we have a suitable help buffer.
;;   ;; Fixme: Maybe process `Related help topics' a la help xrefs and
;;   ;; allow C-c C-f in help buffer.
;;   (let ((temp-buffer-show-hook		; avoid xref stuff
;; 	 (lambda ()
;; 	   (toggle-read-only 1)
;; 	   (setq view-return-to-alist
;; 		 (list (cons (selected-window) help-return-method))))))
;;     (with-output-to-temp-buffer (help-buffer)
;;       (with-current-buffer standard-output
;;  	;; Fixme: Is this actually useful?
;; 	(help-setup-xref (list 'python-describe-symbol symbol)
;; 			 (called-interactively-p 'interactive))
;; 	(set (make-local-variable 'comint-redirect-subvert-readonly) t)
;; 	(help-print-return-message))))
;;   (comint-redirect-send-command-to-process (format "emacs.ehelp(%S, %s)"
;; 						   symbol python-imports)
;;                                            "*Help*" (python-proc) nil nil))

(add-to-list 'debug-ignored-errors "^No symbol")

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

(defun python-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation."
  (interactive "P")
  (or (fill-comment-paragraph justify)
      (save-excursion
	(end-of-line)
	(let* ((syntax (syntax-ppss))
	       (orig (point))
	       start end)
	  (cond ((nth 4 syntax)	; comment.   fixme: loses with trailing one
		 (let (fill-paragraph-function)
		   (fill-paragraph justify)))
		;; The `paragraph-start' and `paragraph-separate'
		;; variables don't allow us to delimit the last
		;; paragraph in a multi-line string properly, so narrow
		;; to the string and then fill around (the end of) the
		;; current line.
		((eq t (nth 3 syntax))	; in fenced string
		 (goto-char (nth 8 syntax)) ; string start
		 (setq start (line-beginning-position))
		 (setq end (condition-case () ; for unbalanced quotes
                               (progn (forward-sexp)
                                      (- (point) 3))
                             (error (point-max)))))
		((re-search-backward "\\s|\\s-*\\=" nil t) ; end of fenced string
		 (forward-char)
		 (setq end (point))
		 (condition-case ()
		     (progn (backward-sexp)
			    (setq start (line-beginning-position)))
		   (error nil))))
	  (when end
	    (save-restriction
	      (narrow-to-region start end)
	      (goto-char orig)
	      ;; Avoid losing leading and trailing newlines in doc
	      ;; strings written like:
	      ;;   """
	      ;;   ...
	      ;;   """
	      (let ((paragraph-separate
		     ;; Note that the string could be part of an
		     ;; expression, so it can have preceding and
		     ;; trailing non-whitespace.
		     (concat
		      (rx (or
			   ;; Opening triple quote without following text.
			   (and (* nonl)
				(group (syntax string-delimiter))
				(repeat 2 (backref 1))
				;; Fixme:  Not sure about including
				;; trailing whitespace.
				(* (any " \t"))
				eol)
			   ;; Closing trailing quote without preceding text.
			   (and (group (any ?\" ?')) (backref 2)
				(syntax string-delimiter))))
		      "\\(?:" paragraph-separate "\\)"))
		    fill-paragraph-function)
		(fill-paragraph justify))))))) t)

(defun python-shift-left (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent))
  (when (> count 0)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
	(if (and (< (current-indentation) count)
		 (not (looking-at "[ \t]*$")))
	    (error "Can't shift all lines enough"))
	(forward-line))
      (indent-rigidly start end (- count)))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun python-shift-right (start end &optional count)
  "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count python-indent))
  (indent-rigidly start end count))

(defun python-outline-level ()
  "`outline-level' function for Python mode.
The level is the number of `python-indent' steps of indentation
of current line."
  (1+ (/ (current-indentation) py-indent-offset)))

;; Fixme: Consider top-level assignments, imports, &c.
(defun python-current-defun (&optional length-limit)
  "`add-log-current-defun-function' for Python."
  (save-excursion
    ;; Move up the tree of nested `class' and `def' blocks until we
    ;; get to zero indentation, accumulating the defined names.
    (let ((accum)
	  (length -1))
      (catch 'done
	(while (or (null length-limit)
		   (null (cdr accum))
		   (< length length-limit))
	  (let ((started-from (point)))
	    (python-beginning-of-block)
	    (end-of-line)
	    (beginning-of-defun)
	    (when (= (point) started-from)
	      (throw 'done nil)))
	  (when (looking-at (rx (0+ space) (or "def" "class") (1+ space)
				(group (1+ (or word (syntax symbol))))))
	    (push (match-string 1) accum)
	    (setq length (+ length 1 (length (car accum)))))
	  (when (= (current-indentation) 0)
	    (throw 'done nil))))
      (when accum
	(when (and length-limit (> length length-limit))
	  (setcar accum ".."))
	(mapconcat 'identity accum ".")))))

(defun python-mark-block ()
  "Mark the block around point.
Uses `python-beginning-of-block', `python-end-of-block'."
  (interactive)
  (push-mark)
  (python-beginning-of-block)
  (push-mark (point) nil t)
  (python-end-of-block)
  (exchange-point-and-mark))

;; Fixme:  Provide a find-function-like command to find source of a
;; definition (separate from BicycleRepairMan).  Complicated by
;; finding the right qualified name.

;;;; Completion.

;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html
(defvar python-imports "None"
  "String of top-level import statements updated by `python-find-imports'.")
(make-variable-buffer-local 'python-imports)

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

(defun python-completion-at-point ()
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

;;;; Skeletons

(define-abbrev-table 'python-mode-abbrev-table ()
  "Abbrev table for Python mode."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda () (not (python-in-string/comment))))

(eval-when-compile
  ;; Define a user-level skeleton and add it to the abbrev table.
  (defmacro def-python-skeleton (name &rest elements)
    (let* ((name (symbol-name name))
           (function (intern (concat "python-insert-" name))))
      `(progn
         ;; Usual technique for inserting a skeleton, but expand
         ;; to the original abbrev instead if in a comment or string.
         (when python-use-skeletons
           (define-abbrev python-mode-abbrev-table ,name ""
             ',function
             nil t))                      ; system abbrev
         (define-skeleton ,function
           ,(format "Insert Python \"%s\" template." name)
           ,@elements)))))
(put 'def-python-skeleton 'lisp-indent-function 2)

;; From `skeleton-further-elements' set below:
;;  `<': outdent a level;
;;  `^': delete indentation on current line and also previous newline.
;;       Not quite like `delete-indentation'.  Assumes point is at
;;       beginning of indentation.

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

(defvar python-default-template "if"
  "Default template to expand by `python-expand-template'.
Updated on each expansion.")

(defun python-expand-template (name)
  "Expand template named NAME.
Interactively, prompt for the name with completion."
  (interactive
   (list (completing-read (format "Template to expand (default %s): "
				  python-default-template)
			  python-mode-abbrev-table nil t nil nil
                          python-default-template)))
  (if (equal "" name)
      (setq name python-default-template)
    (setq python-default-template name))
  (let ((sym (abbrev-symbol name python-mode-abbrev-table)))
    (if sym
        (abbrev-insert sym)
      (error "Undefined template: %s" name))))

;;;; Modes.

;; pdb tracking is alert once this file is loaded, but takes no action if
(defvar outline-heading-end-regexp)
(defvar eldoc-documentation-function)
(defvar python-mode-running)            ;Dynamically scoped var.

;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;      gud-pdb-command-name (symbol-name pdb-path))

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
  :group 'python-mode
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
  ;; (rx (* space) (or "class" "def" "elif" "else" "except" "finally"
  ;; "for" "if" "try" "while" "with")
  ;; symbol-end))
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
    ;; (setq imenu-create-index-function #'py-imenu-create-index)
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
  (when python-use-skeletons
    (set (make-local-variable 'skeleton-further-elements)
         '((< '(backward-delete-char-untabify (min python-indent
                                                   (current-column))))
           (^ '(- (1+ (current-indentation)))))))
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
  (when py-load-pymacs-p (py-load-pymacs))
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
  (when (interactive-p) (message "python-mode loaded from: %s" "python-components-mode.el")))


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

(unless (featurep 'xemacs)
  (ignore-errors (require 'highlight-indentation)))

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

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

(defalias 'py-hungry-delete-forward 'c-hungry-delete-forward)
(defalias 'py-hungry-delete-backwards 'c-hungry-delete-backwards)

(define-derived-mode python2-mode python-mode "Python2"
  "Edit and run code used by Python version 2 series. "
  :group 'python-mode
  :abbrev nil
  (set (make-local-variable 'py-exec-command) '(format "execfile(r'%s') # PYTHON-MODE\n" filename))
  (set (make-local-variable 'py-exec-string-command) '(format "exec(r'%s') # PYTHON-MODE\n" string))
  (py-toggle-shells "python2"))

(define-derived-mode python3-mode python-mode "Python3"
  "Edit and run code used by Python version 3 series. "
  :group 'python-mode
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
  (let (erg)
    (save-excursion
      (setq erg
            (progn
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
               (t (error "Unknown buffer position requested: %s" position))) (point))))
    erg))

(make-obsolete 'jpython-mode 'jython-mode nil)
(define-derived-mode jython-mode python-mode "Jython"
  "Major mode for editing Jython files.
Like `python-mode', but sets up parameters for Jython subprocesses.
Runs `jython-mode-hook' after `python-mode-hook'."

  :group 'python-mode
  (py-toggle-shells "jython"))

;; It's handy to add recognition of Python files to the
;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; can specify different `derived-modes' based on the #! line, but
;; with the latter, we can't.  So we just won't add them if they're
;; already added.

(let ((modes '(("jython" . jython-mode)
               ("python" . python-mode)
               ("python3" . python-mode))))
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

;; hook doesn't get unloaded
(defalias 'python-pdbtrack-track-stack-file 'py-pdbtrack-track-stack-file)

;; (add-hook 'python-mode-hook '(lambda ()(set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)))
;;
;; (add-hook 'python-mode-hook '(lambda ()(set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)))

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;;;; Utility stuff

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

;; (defvar inferior-python-mode-syntax-table
;;   (let ((st (make-syntax-table py-mode-syntax-table)))
;;     ;; Don't get confused by apostrophes in the process's output (e.g. if
;;     ;; you execute "help(os)").
;;     (modify-syntax-entry ?\' "." st)
;;     ;; Maybe we should do the same for double quotes?
;;     ;; (modify-syntax-entry ?\" "." st)
;;     st))

(declare-function compilation-shell-minor-mode "compile" (&optional arg))

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

(defvar python-imports)			; forward declaration
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
