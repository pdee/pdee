;;; python-components-mode.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.2.3

;; Keywords: languages, processes, python, oop

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2015-2020 https://gitlab.com/groups/python-mode-devs
;;         2003-2014 https://launchpad.net/python-mode
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

;; Includes a minor mode for handling a Python/IPython shell, and can
;; take advantage of Pymacs when installed.

;; See documentation in README.org, README.DEVEL.org

;; Please report bugs at
;; https://gitlab.com/python-mode-devs/python-mode/issues

;; available commands are documented in directory "doc" as
;; commands-python-mode.org

;; As for `py-add-abbrev':
;; Similar to `add-mode-abbrev', but uses
;; `py-partial-expression' before point for expansion to
;; store, not `word'.  Also provides a proposal for new
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

;;; Code:

(require 'ansi-color)
(ignore-errors (require 'subr-x))
(require 'cc-cmds)
(require 'comint)
(require 'compile)
(require 'custom)
(require 'ert)
(require 'flymake)
(require 'hippie-exp)
(require 'shell)
(require 'thingatpt)
(require 'which-func)
(require 'tramp)
(require 'tramp-sh)
(require 'org-loaddefs)
;; (require 'org)

(defgroup python-mode nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defconst py-version "6.2.3")

(defvar py-install-directory nil
  "Make sure it exists")

(defcustom py-install-directory ""
  "Directory where python-mode.el and it's subdirectories should be installed.

Needed for completion and other environment stuff only."

  :type 'string
  :tag "py-install-directory"
  :group 'python-mode)

;; (setq py-install-directory (ignore-errors (file-name-directory (buffer-file-name))))

(defcustom py-font-lock-defaults-p t
 "If fontification is not required,

avoiding it might speed up things."

  :type 'boolean
  :tag "py-font-lock-defaults-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-eldoc-mode-p nil
  "If eldoc-mode is loaded by python-mode.

Default is nil"
  :type 'boolean
  :tag "py-eldoc-mode-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-pythonpath ""
  "Define $PYTHONPATH here, if needed.

Emacs doesn't read .bashrc"

  :type 'string
  :tag "py-pythonpath"
  :group 'python-mode)

(defcustom python-mode-modeline-display "Py"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python-mode-modeline-display"
  :group 'python-mode)

(defcustom py-python2-modeline-display "Py2"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python2-mode-modeline-display"
  :group 'python-mode)

(defcustom py-python3-modeline-display "Py3"
  "String to display in Emacs modeline."

  :type 'string
  :tag "python3-mode-modeline-display"
  :group 'python-mode)

(defcustom py-ipython-modeline-display "IPy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "ipython-modeline-display"
  :group 'python-mode)

(defcustom py-jython-modeline-display "Jy"
  "String to display in Emacs modeline."

  :type 'string
  :tag "jython-modeline-display"
  :group 'python-mode)

(defcustom py-extensions "py-extensions.el"
  "File where extensions to python-mode.el should be installed.

Used by virtualenv support."

  :type 'string
  :tag "py-extensions"
  :group 'python-mode)

(defcustom info-lookup-mode "python"
  "Which Python documentation should be queried.

Make sure it's accessible from Emacs by \\<emacs-lisp-mode-map> \\[info] ...
See INSTALL-INFO-FILES for help."

  :type 'string
  :tag "info-lookup-mode"
  :group 'python-mode)

(defcustom py-fast-process-p nil
  "Use `py-fast-process'.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Results arrive in output buffer, which is not in comint-mode"

  :type 'boolean
  :tag "py-fast-process-p"
  :group 'python-mode
  :safe 'booleanp)

;; credits to python.el
(defcustom py-shell-compilation-regexp-alist
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
  "`compilation-error-regexp-alist' for py-shell."
  :type '(alist string)
  :tag "py-shell-compilation-regexp-alist"
  :group 'python-mode)

(defcustom py-shift-require-transient-mark-mode-p t
  "If py-shift commands on regions should require variable ‘transient-mark-mode’.

Default is t"

  :type 'boolean
  :tag "py-shift-require-transient-mark-mode-p"
  :group 'python-mode
  :safe 'booleanp)

(defvar py-fast-output-buffer "*Python Fast*"
  "Internally used. ‘buffer-name’ for fast-processes.")

(defvar py-this-result nil
  "Internally used, store return-value.")

(defvar py-coding-re
  "\\(# *coding[ \t]*=\\|#[ \t]*\-*\-[ \t]*coding:\\|#[ \t]*encoding:\\)[ \t]*\\([[:graph:]+]\\)"
  "")

(setq py-coding-re
  "\\(# *coding[ \t]*=\\|#[ \t]*\-*\-[ \t]*coding:\\|#[ \t]*encoding:\\)[ \t]*\\([[:graph:]+]\\)")

(defcustom py-comment-auto-fill-p nil
  "When non-nil, fill comments.

Defaut is nil"

  :type 'boolean
  :tag "py-comment-auto-fill-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-sexp-use-expression-p nil
  "If non-nil, ‘forward-sexp’ will call ‘py-forward-expression’.

Respective ‘backward-sexp’ will call ‘py-backward-expression’
Default is t"
  :type 'boolean
  :tag "py-sexp-use-expression-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-session-p t
  "If commands would use an existing process.

Default is t"

  :type 'boolean
  :tag "py-session-p"
  :group 'python-mode
  :safe 'booleanp)

(defun py-toggle-session-p (&optional arg)
  "Switches boolean variable ‘py-session-p’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-session-p (not py-session-p))
  (when arg (message "py-session-p: %s" py-session-p)))

(defcustom py-max-help-buffer-p nil
  "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil.  In ‘help-buffer’, \"q\" will close it."

  :type 'boolean
  :tag "py-max-help-buffer-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-highlight-error-source-p nil
  "Respective code in source-buffer will be highlighted.

Default is nil.

\\<python-mode-map> `py-remove-overlays-at-point' removes that highlighting."
  :type 'boolean
  :tag "py-highlight-error-source-p"
  :group 'python-mode)

(defcustom py-set-pager-cat-p nil
  "If the shell environment variable $PAGER should set to `cat'.

Avoids lp:783828,
 \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os'"

  :type 'boolean
  :tag "py-set-pager-cat-p"
  :group 'python-mode)

(defcustom py-empty-line-closes-p nil
  "When non-nil, dedent after empty line following block.

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil"

  :type 'boolean
  :tag "py-empty-line-closes-p"
  :group 'python-mode)

(defcustom py-prompt-on-changed-p t
  "Ask for save before a changed buffer is sent to interpreter.

Default is t"

  :type 'boolean
  :tag "py-prompt-on-changed-p"
  :group 'python-mode)

(defcustom py-dedicated-process-p nil
  "If commands executing code use a dedicated shell.

Default is nil

When non-nil and `py-session-p', an existing dedicated process is re-used instead of default - which allows executing stuff in parallel."
  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defcustom py-store-result-p nil
  "Put resulting string of `py-execute-...' into ‘kill-ring’.

Default is nil"

  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defvar py-shell--font-lock-buffer "*PSFLB*"
  "May contain the `py-buffer-name' currently fontified." )

(setq py-shell--font-lock-buffer "*PSFLB*")

(defvar py-return-result-p nil
  "Internally used.

When non-nil, return resulting string of `py-execute-...'.
Imports will use it with nil.
Default is nil")

(defcustom py--execute-use-temp-file-p nil
 "Assume execution at a remote machine.

 where write-access is not given."

 :type 'boolean
 :tag "py--execute-use-temp-file-p"
 :group 'python-mode)

(defvar py--match-paren-forward-p nil
  "Internally used by `py-match-paren'.")

(defvar py-new-session-p t
  "Internally used.  See lp:1393882.

Restart ‘py-shell’ once with new Emacs/‘python-mode’.")

(defcustom py-electric-close-active-p nil
  "Close completion buffer if no longer needed.

Works around a bug in `choose-completion'.
Default is nil"
  :type 'boolean
  :tag "py-electric-close-active-p"
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil."

  :type 'boolean
  :tag "py-hide-show-minor-mode-p"
  :group 'python-mode)

(defcustom py-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil.

If non-nil and variable ‘abbrev-mode’ on, block-skeletons will inserted.
Pressing \"if<SPACE>\" for example will prompt for the if-condition."

  :type 'boolean
  :tag "py-load-skeletons-p"
  :group 'python-mode)

(defcustom py-if-name-main-permission-p t
  "Allow execution of code inside blocks started.

by \"if __name__== '__main__':\".
Default is non-nil"

  :type 'boolean
  :tag "py-if-name-main-permission-p"
  :group 'python-mode)

(defcustom py-use-font-lock-doc-face-p nil
  "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.
Call \\<emacs-lisp-mode-map> \\[customize-face] in order to have a visible effect."

  :type 'boolean
  :tag "py-use-font-lock-doc-face-p"
  :group 'python-mode)

(defcustom py-empty-comment-line-separates-paragraph-p t
  "Consider paragraph start/end lines with nothing inside but comment sign.

Default is  non-nil"
  :type 'boolean
  :tag "py-empty-comment-line-separates-paragraph-p"
  :group 'python-mode)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil."
  :type 'boolean
  :tag "py-indent-honors-inline-comment"
  :group 'python-mode)

(defcustom py-auto-fill-mode nil
  "If ‘python-mode’ should set ‘fill-column’.

according to values
in `py-comment-fill-column' and `py-docstring-fill-column'.
Default is  nil"

  :type 'boolean
  :tag "py-auto-fill-mode"
  :group 'python-mode)

(defcustom py-error-markup-delay 4
  "Seconds error's are highlighted in exception buffer."

  :type 'integer
  :tag "py-error-markup-delay"
  :group 'python-mode)

(defcustom py-fast-completion-delay 0.1
  "Used by ‘py-fast-send-string’."

  :type 'float
  :tag "py-fast-completion-delay"
  :group 'python-mode)

(defcustom py-new-shell-delay
    (if (eq system-type 'windows-nt)
      2.0
    1.0)

  "If a new comint buffer is connected to Python, commands like completion might need some delay."

  :type 'float
  :tag "py-new-shell-delay"
  :group 'python-mode)

(defcustom py-autofill-timer-delay 1
  "Delay when idle before functions ajusting  `py-docstring-fill-column', `py-comment-fill-column' are called."
  :type 'integer
  :tag "py-autofill-timer-delay"
  :group 'python-mode)

(defcustom py-docstring-fill-column 72
  "Value of `fill-column' to use when filling a docstring.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :tag "py-docstring-fill-column"
  :group 'python-mode)

(defcustom py-comment-fill-column 79
  "Value of `fill-column' to use when filling a comment.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
		 (const :tag "Use the current `fill-column'" t))
  :tag "py-comment-fill-column"
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If t, related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives"

  :type 'boolean
  :tag "py-fontify-shell-buffer-p"
  :group 'python-mode)

(defvar py-modeline-display ""
  "Internally used")

(defcustom py-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil.  Note: when `py-shell-name' is specified with path, it's shown as an acronym in ‘buffer-name’ already."

  :type 'boolean
  :tag "py-modeline-display-full-path-p"
  :group 'python-mode)

(defcustom py-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil"
  :type 'boolean
  :tag "py-modeline-acronym-display-home-p"
  :group 'python-mode)

(defun py-autopair-check ()
  "Check, if ‘autopair-mode’ is available.

Give some hints, if not."
  (interactive)
  (if (featurep 'autopair)
      't
    (progn
      (message "py-autopair-check: %s" "Don't see autopair.el. Make sure, it's installed. If not, maybe see source: URL: http://autopair.googlecode.com")
      nil)))

(defvar highlight-indent-active nil)
(defvar autopair-mode nil)

(defvar-local py--editbeg nil
  "Internally used by `py-edit-docstring' and others")

(defvar-local py--editend nil
  "Internally used by `py-edit-docstring' and others")

(defvar py--oldbuf nil
  "Internally used by `py-edit-docstring'.")

(defvar py-edit-buffer "Edit docstring"
  "Name of the temporary buffer to use when editing.")

(defvar py--edit-register nil)

(defvar py-result nil
  "Internally used.  May store result from Python process.")

(defvar py-error nil
  "Internally used.  Takes the error-messages from Python process.")

(defvar py-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used.")

(defvar py-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used.")

(defcustom py-timer-close-completions-p t
  "If `py-timer-close-completion-buffer' should run, default is non-nil."

  :type 'boolean
  :tag "py-timer-close-completions-p"
  :group 'python-mode)

(defcustom py-autopair-mode nil
  "If ‘python-mode’ calls (autopair-mode-on)

Default is nil
Load `autopair-mode' written by Joao Tavora <joaotavora [at] gmail.com>
URL: http://autopair.googlecode.com"
  :type 'boolean
  :tag "py-autopair-mode"
  :group 'python-mode)

(defcustom py-indent-no-completion-p nil
  "If completion function should insert a TAB when no completion found.

Default is nil"
  :type 'boolean
  :tag "py-indent-no-completion-p"
  :group 'python-mode)

(defcustom py-company-pycomplete-p nil
  "Load company-pycomplete stuff.  Default is  nil."

  :type 'boolean
  :tag "py-company-pycomplete-p"
  :group 'python-mode)

(defvar py-last-position nil
    "Used by ‘py-help-at-point’.

Avoid repeated call at identic pos.")

(defvar py-auto-completion-mode-p nil
  "Internally used by `py-auto-completion-mode'.")

(defvar py-complete-last-modified nil
  "Internally used by `py-auto-completion-mode'.")

(defvar py--auto-complete-timer nil
  "Internally used by `py-auto-completion-mode'.")

(defvar py-auto-completion-buffer nil
  "Internally used by `py-auto-completion-mode'.")

(defvar py--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See `py-auto-completion-mode'")

(defcustom py-auto-complete-p nil
  "Run python-mode's built-in auto-completion via ‘py-complete-function’.  Default is  nil."

  :type 'boolean
  :tag "py-auto-complete-p"
  :group 'python-mode)
(make-variable-buffer-local 'py-auto-complete-p)

(defcustom py-tab-shifts-region-p nil
  "If t, TAB will indent/cycle the region, not just the current line.

Default is  nil
See also `py-tab-indents-region-p'"

  :type 'boolean
  :tag "py-tab-shifts-region-p"
  :group 'python-mode)

(defcustom py-tab-indents-region-p nil
  "When t and first TAB doesn't shift, ‘indent-region’ is called.

Default is  nil
See also `py-tab-shifts-region-p'"

  :type 'boolean
  :tag "py-tab-indents-region-p"
  :group 'python-mode)

(defcustom py-block-comment-prefix-p t
  "If py-comment inserts ‘py-block-comment-prefix’.

Default is t"

  :type 'boolean
  :tag "py-block-comment-prefix-p"
  :group 'python-mode)

(defcustom py-org-cycle-p nil
  "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>.

Default is nil."
  :type 'boolean
  :tag "py-org-cycle-p"
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If `py-complete-initialize'.

Sets up enviroment for Pymacs based py-complete.
 Should load it's keys into `python-mode-map'
Default is nil.
See also resp. edit `py-complete-set-keymap'"

  :type 'boolean
  :tag "py-set-complete-keymap-p"
  :group 'python-mode)

(defcustom py-outline-minor-mode-p t
  "If outline minor-mode should be on, default is t."
  :type 'boolean
  :tag "py-outline-minor-mode-p"
  :group 'python-mode)

(defvar py-guess-py-install-directory-p nil)
(defcustom py-guess-py-install-directory-p nil
  "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from variable `buffer-file-name'."
  :type 'boolean
  :tag "py-guess-py-install-directory-p"
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  :type 'boolean
  :tag "py-load-pymacs-p"
  :group 'python-mode)

(defcustom py-verbose-p nil
  "If functions should report results.

Default is nil."
  :type 'boolean
  :tag "py-verbose-p"
  :group 'python-mode)

(defcustom py-sexp-function nil
  "Called instead of `forward-sexp', `backward-sexp'.

Default is nil."

  :type '(choice

          (const :tag "default" nil)
          (const :tag "py-end-of-partial-expression" py-end-of-partial-expression)
          (const :tag "py-end-of-expression" py-end-of-expression))
  :tag "py-sexp-function"
  :group 'python-mode)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty.

Default is non-nil.
When non-nil, `py-end-of-def' and related will work faster"
  :type 'boolean
  :tag "py-close-provides-newline"
  :group 'python-mode)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line.  Default is t - keep relative position."
  :type 'boolean
  :tag "py-dedent-keep-relative-column"
  :group 'python-mode)

(defcustom py-indent-list-style 'line-up-with-first-element
  "Sets the basic indentation style of lists.

Setting here might be ignored in case of canonical indent.

‘line-up-with-first-element’ indents to 1+ column of opening delimiter
def foo (a,
         b):

‘one-level-to-beginning-of-statement’ adds ‘py-indent-offset’ to beginning of statement
def long_function_name(
    var_one, var_two, var_three,
    var_four):
    print(var_one)

‘one-level-from-first-element’ adds ‘py-indent-offset’ from first element
def foo():
    if (foo &&
            baz):
        bar()"
  :type '(choice
          (const :tag "line-up-with-first-element" line-up-with-first-element)
          (const :tag "one-level-to-beginning-of-statement" one-level-to-beginning-of-statement)
          (const :tag "one-level-from-first-element" one-level-from-first-element))
  :tag "py-indent-list-style"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-list-style)

(defcustom py-closing-list-dedents-bos nil
  "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6
]

result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f'
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6
    ]
result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f'
    )

Examples from PEP8
URL: https://www.python.org/dev/peps/pep-0008/#indentation"
  :type 'boolean
  :tag "py-closing-list-dedents-bos"
  :group 'python-mode)

(defvar py-imenu-max-items 99)
(defcustom py-imenu-max-items 99
 "Python-mode specific `imenu-max-items'."
 :type 'number
 :tag "py-imenu-max-items"
 :group 'python-mode)

(defcustom py-closing-list-space 1
  "Number of chars, closing parenthesis outdent from opening, default is 1."
  :type 'number
  :tag "py-closing-list-space"
  :group 'python-mode)

(defcustom py-max-specpdl-size 99
  "Heuristic exit.
e
Limiting number of recursive calls by ‘py-forward-statement’ and related.
Default is ‘max-specpdl-size’.

This threshold is just an approximation.  It might set far higher maybe.

See lp:1235375. In case code is not to navigate due to errors, variable `which-function-mode' and others might make Emacs hang.  Rather exit than."

  :type 'number
  :tag "py-max-specpdl-size"
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil."
  :type 'boolean
  :tag "py-closing-list-keeps-space"
  :group 'python-mode)

(defcustom py-electric-kill-backward-p nil
  "Affects `py-electric-backspace'.  Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string[0:1]
--------------^

==>

my_string[]
----------^

In result cursor is insided emptied delimited form."

  :type 'boolean
  :tag "py-electric-kill-backward-p"
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.

Default is nil.  See lp:837065 for discussions.
See also `py-electric-colon-bobl-only'"
  :type 'boolean
  :tag "py-electric-colon-active-p"
  :group 'python-mode)

(defcustom py-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block.

See lp:1207405 resp. `py-electric-colon-active-p'"

  :type 'boolean
  :tag "py-electric-colon-bobl-only"
  :group 'python-mode)

(defcustom py-electric-yank-active-p nil
  "When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nil"
  :type 'boolean
  :tag "py-electric-yank-active-p"
  :group 'python-mode)

(defcustom py-electric-colon-greedy-p nil
  "If ‘py-electric-colon’ should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level."
  :type 'boolean
  :tag "py-electric-colon-greedy-p"
  :group 'python-mode)

(defcustom py-electric-colon-newline-and-indent-p nil
  "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is nil."
  :type 'boolean
  :tag "py-electric-colon-newline-and-indent-p"
  :group 'python-mode)

(defcustom py-electric-comment-p nil
  "If \"#\" should call `py-electric-comment'. Default is nil."
  :type 'boolean
  :tag "py-electric-comment-p"
  :group 'python-mode)

(defcustom py-electric-comment-add-space-p nil
  "If ‘py-electric-comment’ should add a space.  Default is nil."
  :type 'boolean
  :tag "py-electric-comment-add-space-p"
  :group 'python-mode)

(defcustom py-mark-decorators nil
  "If ‘py-mark-def-or-class’ functions should mark decorators too.  Default is nil."
  :type 'boolean
  :tag "py-mark-decorators"
  :group 'python-mode)

(defcustom py-defun-use-top-level-p nil
 "If ‘beginning-of-defun’, ‘end-of-defun’ calls function ‘top-level’ form.

Default is nil.

beginning-of defun, ‘end-of-defun’ forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

‘mark-defun’ marks function ‘top-level’ form at point etc."

 :type 'boolean
  :tag "py-defun-use-top-level-p"
 :group 'python-mode)

(defcustom py-tab-indent t
  "Non-nil means TAB in Python mode calls `py-indent-line'."
  :type 'boolean
  :tag "py-tab-indent"
  :group 'python-mode)

(defcustom py-return-key 'newline
  "Which command <return> should call."
  :type '(choice

          (const :tag "default" py-newline-and-indent)
          (const :tag "newline" newline)
          (const :tag "py-newline-and-indent" py-newline-and-indent)
          (const :tag "py-newline-and-dedent" py-newline-and-dedent)
          )
  :tag "py-return-key"
  :group 'python-mode)

(defcustom py-complete-function 'py-fast-complete
  "When set, enforces function todo completion, default is `py-fast-complete'.

Might not affect IPython, as `py-shell-complete' is the only known working here.
Normally ‘python-mode’ knows best which function to use."
  :type '(choice

          (const :tag "default" nil)
          (const :tag "Pymacs and company based py-complete" py-complete)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "py-indent-or-complete" py-indent-or-complete)
	  (const :tag "py-fast-complete" py-fast-complete)
          )
  :tag "py-complete-function"
  :group 'python-mode)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file."
  :type 'string
  :tag "py-encoding-string"
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file."
  :type 'string
  :tag "py-shebang-startstring"
  :group 'python-mode)

(defcustom py-flake8-command ""
  "Which command to call flake8.

If empty, ‘python-mode’ will guess some"
  :type 'string
  :tag "py-flake8-command"
  :group 'python-mode)

(defcustom py-flake8-command-args ""
  "Arguments used by flake8.

Default is the empty string."
  :type 'string
  :tag "py-flake8-command-args"
  :group 'python-mode)

(defvar py-flake8-history nil
  "Used by flake8, resp. ‘py-flake8-command’.

Default is nil.")

(defcustom py-message-executing-temporary-file t
  "If execute functions using a temporary file should message it.

Default is t.
Messaging increments the prompt counter of IPython shell."
  :type 'boolean
  :tag "py-message-executing-temporary-file"
  :group 'python-mode)

(defcustom py-execute-no-temp-p nil
  "Seems Emacs-24.3 provided a way executing stuff without temporary files."
  :type 'boolean
  :tag "py-execute-no-temp-p"
  :group 'python-mode)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis."
  :type 'integer
  :tag "py-lhs-inbound-indent"
  :group 'python-mode)

(defcustom py-continuation-offset 2
  "Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line."
  :type 'integer
  :tag "py-continuation-offset"
  :group 'python-mode)

(defcustom py-indent-tabs-mode nil
  "Python-mode starts `indent-tabs-mode' with the value specified here, default is nil."
  :type 'boolean
  :tag "py-indent-tabs-mode"
  :group 'python-mode)

(defcustom py-smart-indentation nil
  "Guess `py-indent-offset'.  Default is nil.

Setting it to t seems useful only in cases where customizing
`py-indent-offset' is no option - for example because the
indentation step is unknown or differs inside the code.

When this variable is non-nil, `py-indent-offset' is guessed from existing code in the buffer, which might slow down the proceeding."

  :type 'boolean
  :tag "py-smart-indentation"
  :group 'python-mode)

(defcustom py-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
 `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :tag "py-block-comment-prefix"
  :group 'python-mode)

(defcustom py-indent-offset 4
  "Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :tag "py-indent-offset"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to `py-indent-offset' indicates"
  :type 'integer
  :tag "py-backslashed-lines-indent-offset"
  :group 'python-mode)

(defcustom py-shell-completion-native-output-timeout 5.0
  "Time in seconds to wait for completion output before giving up."
  :version "25.1"
  :type 'float
  :tag "py-shell-completion-native-output-timeout"
  :group 'python-mode)

(defcustom py-shell-completion-native-try-output-timeout 1.0
  "Time in seconds to wait for *trying* native completion output."
  :version "25.1"
  :type 'float
  :tag "py-shell-completion-native-try-output-timeout"
  :group 'python-mode)

(defvar py-shell--first-prompt-received-output-buffer nil)
(defvar py-shell--first-prompt-received nil)

(defcustom py-shell-first-prompt-hook nil
  "Hook run upon first (non-pdb) shell prompt detection.
This is the place for shell setup functions that need to wait for
output.  Since the first prompt is ensured, this helps the
current process to not hang while waiting.  This is useful to
safely attach setup code for long-running processes that
eventually provide a shell."
  :version "25.1"
  :type 'hook
  :tag "py-shell-first-prompt-hook"
  :group 'python-mode)

(defvar py-shell--parent-buffer nil)

(defvar py-shell--package-depth 10)

(defcustom py-indent-comments t
  "When t, comment lines are indented."
  :type 'boolean
  :tag "py-indent-comments"
  :group 'python-mode)

(defcustom py-uncomment-indents-p nil
  "When non-nil, after uncomment indent lines."
  :type 'boolean
  :tag "py-uncomment-indents-p"
  :group 'python-mode)

(defcustom py-separator-char 47
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function `py-separator-char'."
  :type 'character
  :tag "py-separator-char"
  :group 'python-mode)

(and
 ;; used as a string finally
 ;; kept a character not to break existing customizations
 (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from `py-temp-directory'.  Default is the empty string."
  :type 'string
  :tag "py-custom-temp-directory"
  :group 'python-mode)

(defcustom py-beep-if-tab-change t
  "Ring the bell if `tab-width' is changed.
If a comment of the form

                           \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :tag "py-beep-if-tab-change"
  :group 'python-mode)

(defcustom py-jump-on-exception t
  "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :tag "py-jump-on-exception"
  :group 'python-mode)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :tag "py-ask-about-save"
  :group 'python-mode)

(defcustom py-delete-function 'delete-char
  "Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :tag "py-delete-function"
  :group 'python-mode)

(defcustom py-import-check-point-max
  20000
  "Max number of characters to search Java-ish import statement.

When `python-mode' tries to calculate the shell
-- either a CPython or a Jython shell --
it looks at the so-called `shebang'.
If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :tag "py-import-check-point-max
"
  :group 'python-mode)

;; (setq py-shells
;; (list
;; ""
;; 'ipython
;; 'ipython2.7
;; 'ipython3
;; 'jython
;; 'python
;; 'python2
;; 'python3
;; 'pypy
;; ))

(defcustom py-known-shells
  (list
   "ipython"
   "ipython2.7"
   "ipython3"
   "jython"
   "python"
   "python2"
   "python3"
   "pypy"
   )
  "A list of available shells instrumented for commands.
 Expects its executables installed

Edit for your needs."
  :type '(repeat string)
  :tag "py-shells"
  :group 'python-mode)

(defcustom py-known-shells-extended-commands
  (list "ipython"
	"python"
	"python3"
	"pypy"
	)
  "A list of shells instrumented for finer grained commands
like ‘py-execute-statement-ipython’
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
  :tag "py-shells"
  :group 'python-mode)

(defun py-install-named-shells-fix-doc (ele)
  (cond ((string-match "^i" ele)
	 (concat "I" (capitalize (substring ele 1))))
	((string-match "^pypy" ele)
	 "PyPy")
	(t (capitalize ele))))

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply `jython-mode'."
  :type '(repeat string)
  :tag "py-jython-packages
"
  :group 'python-mode)

(defcustom py-current-defun-show t
  "If `py-current-defun' should jump to the definition.

Highlights it while waiting PY-WHICH-FUNC-DELAY seconds.
Afterwards returning to previous position.

Default is t."

  :type 'boolean
  :tag "py-current-defun-show"
  :group 'python-mode)

(defcustom py-current-defun-delay 2
  "When called interactively, `py-current-defun' should wait PY-WHICH-FUNC-DELAY seconds at the definition name found, before returning to previous position."

  :type 'number
  :tag "py-current-defun-delay"
  :group 'python-mode)

(defcustom py-python-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python-send-delay"
  :group 'python-mode)

(defcustom py-python3-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python3-send-delay"
  :group 'python-mode)

(defcustom py-ipython-send-delay 1
  "Seconds to wait for output, used by `py--send-...' functions.

See also ‘py-python-send-delay’"

  :type 'number
  :tag "py-ipython-send-delay"
  :group 'python-mode)

(defcustom py-master-file nil
  "Execute the named master file instead of the buffer's file.

Default is nil.
With relative path variable `default-directory' is prepended.

Beside you may set this variable in the file's local
variable section, e.g.:

                           # Local Variables:
                           # py-master-file: \"master.py\"
                           # End:"
  :type 'string
  :tag "py-master-file"
  :group 'python-mode)
(make-variable-buffer-local 'py-master-file)

(defcustom py-pychecker-command "pychecker"
  "Shell command used to run Pychecker."
  :type 'string
  :tag "py-pychecker-command"
  :group 'python-mode)

(defcustom py-pychecker-command-args "--stdlib"
  "String arguments to be passed to pychecker."
  :type 'string
  :tag "py-pychecker-command-args"
  :group 'python-mode)

(defcustom py-pyflakes-command "pyflakes"
  "Shell command used to run Pyflakes."
  :type 'string
  :tag "py-pyflakes-command"
  :group 'python-mode)

(defcustom py-pyflakes-command-args ""
  "String arguments to be passed to pyflakes.

Default is \"\""
  :type 'string
  :tag "py-pyflakes-command-args"
  :group 'python-mode)

(defcustom py-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
  :tag "py-pep8-command"
  :group 'python-mode)

(defcustom py-pep8-command-args ""
  "String arguments to be passed to pylint.

Default is \"\""
  :type 'string
  :tag "py-pep8-command-args"
  :group 'python-mode)

(defcustom py-pyflakespep8-command (concat py-install-directory "/pyflakespep8.py")
  "Shell command used to run `pyflakespep8'."
  :type 'string
  :tag "py-pyflakespep8-command"
  :group 'python-mode)

(defcustom py-pyflakespep8-command-args ""
  "String arguments to be passed to pyflakespep8.

Default is \"\""
  :type 'string
  :tag "py-pyflakespep8-command-args"
  :group 'python-mode)

(defcustom py-pylint-command "pylint"
  "Shell command used to run Pylint."
  :type 'string
  :tag "py-pylint-command"
  :group 'python-mode)

(defcustom py-pylint-command-args '("--errors-only")
  "String arguments to be passed to pylint.

Default is \"--errors-only\""
  :type '(repeat string)
  :tag "py-pylint-command-args"
  :group 'python-mode)

(defvar py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *"
  "Recognize the prompt.")

(defcustom py-shell-input-prompt-1-regexp ">>> "
  "A regular expression to match the input prompt of the shell."
  :type 'regexp
  :tag "py-shell-input-prompt-1-regexp"
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "[.][.][.]:? "
  "A regular expression to match the input prompt.

Applies to the shell after the first line of input."
  :type 'string
  :tag "py-shell-input-prompt-2-regexp"
  :group 'python-mode)

(defvar py-shell-ipython-input-prompt-1-regexp "In \\[[0-9]+\\]: "
  "Regular Expression matching top-level input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defvar py-shell-ipython-input-prompt-2-regexp "   \\.\\.\\.: "
  "Regular Expression matching top-level input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defcustom py-shell-input-prompt-2-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :version "24.4"
  :tag "py-shell-input-prompt-2-regexps"
  :group 'python-mode)

(defcustom py-shell-input-prompt-regexps
  '(">>> " "\\.\\.\\. "                 ; Python
    "In \\[[0-9]+\\]: "                 ; IPython
    "   \\.\\.\\.: "                    ; IPython
    ;; Using ipdb outside IPython may fail to cleanup and leave static
    ;; IPython prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat regexp)
  :version "24.4"
  :tag "py-shell-input-prompt-regexps"
  :group 'python-mode)

(defvar py-ipython-output-prompt-re "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defcustom py-shell-output-prompt-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts."
  :type '(repeat string)
  :version "24.4"
  :tag "py-shell-output-prompt-regexps"
  :group 'python-mode)

(defvar py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ "
  "Recognize the pydb-prompt.")
;; (setq py-pdbtrack-input-prompt "^[(< \t]*[Ii]?[Pp]y?db[>)]*.*")

(defvar py-ipython-input-prompt-re "In \\[?[0-9 ]*\\]?: *\\|^[ ]\\{3\\}[.]\\{3,\\}: *"
  "A regular expression to match the IPython input prompt.")

(defvar py-shell-prompt-regexp
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by `py-fast-filter'.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defvar py-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)")
  "Internally used by `py-fast-filter'.
‘ansi-color-filter-apply’ might return
Result: \"\\nIn [10]:    ....:    ....:    ....: 1\\n\\nIn [11]: \"")

(defcustom py-shell-prompt-detect-p nil
  "Non-nil enables autodetection of interpreter prompts."
  :type 'boolean
  :safe 'booleanp
  :version "24.4"
  :tag "py-shell-prompt-detect-p"
  :group 'python-mode)

(defcustom py-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.

Setting this variable will only effect new shells."
  :type 'boolean
  :tag "py-shell-prompt-read-only"
  :group 'python-mode)

(setq py-fast-filter-re
  (concat "\\("
	  (mapconcat 'identity
		     (delq nil
			   (list
			    py-shell-input-prompt-1-regexp
			    py-shell-input-prompt-2-regexp
			    py-ipython-input-prompt-re
			    py-ipython-output-prompt-re
			    py-pdbtrack-input-prompt
			    py-pydbtrack-input-prompt
			    "[.]\\{3,\\}:? *"
			    ))
		     "\\|")
	  "\\)"))

(defcustom py-honor-IPYTHONDIR-p nil
  "When non-nil ipython-history file is constructed by $IPYTHONDIR.

Default is nil.
Otherwise value of ‘py-ipython-history’ is used."
  :type 'boolean
  :tag "py-honor-IPYTHONDIR-p"
  :group 'python-mode)

(defcustom py-ipython-history "~/.ipython/history"
  "Ipython-history default file.

Used when ‘py-honor-IPYTHONDIR-p’ is nil - th default"

  :type 'string
  :tag "py-ipython-history"
  :group 'python-mode)

(defcustom py-honor-PYTHONHISTORY-p nil
  "When non-nil python-history file is set by $PYTHONHISTORY.

Default is nil.
Otherwise value of ‘py-python-history’ is used."
  :type 'boolean
  :tag "py-honor-PYTHONHISTORY-p"
  :group 'python-mode)

(defcustom py-python-history "~/.python_history"
  "Python-history default file. Used when ‘py-honor-PYTHONHISTORY-p’ is nil (default)."

  :type 'string
  :tag "py-python-history"
  :group 'python-mode)

(defcustom py-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer.

If `py-keep-windows-configuration' is t, this will take precedence over setting here."

  :type 'boolean
  :tag "py-switch-buffers-on-execute-p"
  :group 'python-mode)
;; made buffer-local as pdb might need t in all circumstances
(make-variable-buffer-local 'py-switch-buffers-on-execute-p)

(defcustom py-split-window-on-execute 'just-two
  "When non-nil split windows.

Default is just-two - when code is send to interpreter.
Splits screen into source-code buffer and current ‘py-shell’ result.
Other buffer will be hidden that way.

When set to t, ‘python-mode’ tries to reuse existing windows
and will split only if needed.

With 'always, results will displayed in a new window.

Both t and `always' is experimental still.

For the moment: If a multitude of python-shells/buffers should be
visible, open them manually and set `py-keep-windows-configuration' to t.

See also `py-keep-windows-configuration'"
  :type '(choice
          (const :tag "default" just-two)
	  (const :tag "reuse" t)
          (const :tag "no split" nil)
	  (const :tag "just-two" just-two)
          (const :tag "always" always))
  :tag "py-split-window-on-execute"
  :group 'python-mode)

(defun py-toggle-py-split-window-on-execute ()
  "Toggle between customized value and nil"
  (interactive)
  (setq py-split-window-on-execute (not py-split-window-on-execute))
  (when (called-interactively-p 'interactive)
    (message "py-split-window-on-execute: %s" py-split-window-on-execute)
    py-split-window-on-execute))

(defcustom py-split-window-on-execute-threshold 3
  "Maximal number of displayed windows.

Honored, when `py-split-window-on-execute' is t, i.e. \"reuse\".
Don't split when max number of displayed windows is reached."
  :type 'number
  :tag "py-split-window-on-execute-threshold"
  :group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions."
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
                 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :tag "py-split-windows-on-execute-function"
  :group 'python-mode)

(defcustom py-shell-fontify-p 'input
  "Fontify current input in Python shell. Default is input.

INPUT will leave output unfontified.

At any case only current input gets fontified."
  :type '(choice (const :tag "Default" all)
                 (const :tag "Input" input)
		 (const :tag "Nil" nil)
                 )
  :tag "py-shell-fontify-p"
  :group 'python-mode)

(defcustom py-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads."
  :type '(repeat string)
  :tag "py-hide-show-keywords
"
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "Controls if doc strings can be hidden by hide-show."
  :type 'boolean
  :tag "py-hide-show-hide-docstrings"
  :group 'python-mode)

(defcustom py-hide-comments-when-hiding-all t
  "Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :tag "py-hide-comments-when-hiding-all"
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads."
  :type '(repeat string)
  :tag "py-outline-mode-keywords
"
  :group 'python-mode)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."

  :type 'hook
  :tag "python-mode-hook"
  :group 'python-mode
  )

(defcustom py-shell-name
  (if (eq system-type 'windows-nt)
      "C:/Python27/python"
    "python")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for.

If no shell is specified by command.

On Windows default is C:/Python27/python
--there is no garantee it exists, please check your system--

Else python"
  :type 'string
  :tag "py-shell-name
"
  :group 'python-mode)

(defvar py-default-interpreter py-shell-name)

(defvar py-tempfile nil
  "Internally used.")

(defcustom py-python-command
  (if (eq system-type 'windows-nt)
      ;; "C:\\Python27\\python.exe"
      "python"
   ;; "C:/Python33/Lib/site-packages/IPython"
    "python")

  "Make sure directory in in the PATH-variable.

Windows: edit in \"Advanced System Settings/Environment Variables\"
Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :tag "py-python-command
"
  :group 'python-mode)

(defcustom py-python-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
  :tag "py-python-command-args"
  :group 'python-mode)

(defcustom py-python2-command
  (if (eq system-type 'windows-nt)
      "C:\\Python27\\python"
    ;; "python2"
    "python2")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in
\"Advanced System Settings/Environment Variables\"
Commonly
\"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :tag "py-python2-command
"
  :group 'python-mode)

(defcustom py-python2-command-args '("-i")
  "String arguments to be used when starting a Python shell."
  :type '(repeat string)
  :tag "py-python2-command-args"
  :group 'python-mode)

;; "/usr/bin/python3"
(defcustom py-python3-command
  (if (eq system-type 'windows-nt)
    "C:/Python33/python"
    "python3")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for.

Unless shell is specified by command.

On Windows see C:/Python3/python.exe
--there is no garantee it exists, please check your system--

At GNU systems see /usr/bin/python3"

  :type 'string
  :tag "py-python3-command
"
  :group 'python-mode)

(defcustom py-python3-command-args '("-i")
  "String arguments to be used when starting a Python3 shell."
  :type '(repeat string)
  :tag "py-python3-command-args"
  :group 'python-mode)

(defcustom py-ipython-command
  (if (eq system-type 'windows-nt)
      ;; "ipython"
    "C:\\Python27\\python"
    ;; "C:/Python33/Lib/site-packages/IPython"
    ;; "/usr/bin/ipython"
    "ipython")

  "A PATH/TO/EXECUTABLE or default value.

`M-x IPython RET' may look for,
Unless IPython-shell is specified by command.

On Windows default is \"C:\\\\Python27\\\\python.exe\"
While with Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython.exe\"

Else /usr/bin/ipython"

  :type 'string
  :tag "py-ipython-command
"
  :group 'python-mode)

(defcustom py-ipython-command-args
  (if (eq system-type 'windows-nt)
      '("-i" "C:\\Python27\\Scripts\\ipython-script.py")
    ;; --simple-prompt seems to exist from IPython 5.
    (if (string-match "^[0-4]" (shell-command-to-string (concat "ipython" " -V")))
	'("--pylab" "--automagic")
      '("--pylab" "--automagic" "--simple-prompt")))
  "String arguments to be used when starting a IPython shell.

At Windows make sure ipython-script.py is PATH.
Also setting PATH/TO/SCRIPT here should work, for example;
C:\\Python27\\Scripts\\ipython-script.py
With Anaconda the following is known to work:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython-script-py\""
  :type '(repeat string)
  :tag "py-ipython-command-args"
  :group 'python-mode)

(defcustom py-jython-command
  (if (eq system-type 'windows-nt)
      '("jython")
    '("/usr/bin/jython"))

  "A PATH/TO/EXECUTABLE or default value.
`M-x Jython RET' may look for, if no Jython-shell is specified by command.

Not known to work at windows
Default /usr/bin/jython"

  :type '(repeat string)
  :tag "py-jython-command
"
  :group 'python-mode)

(defcustom py-jython-command-args '("-i")
  "String arguments to be used when starting a Jython shell."
  :type '(repeat string)
  :tag "py-jython-command-args"
  :group 'python-mode)

(defcustom py-shell-toggle-1 py-python2-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'."
  :type 'string
  :tag "py-shell-toggle-1"
  :group 'python-mode)

(defcustom py-shell-toggle-2 py-python3-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'."
  :type 'string
  :tag "py-shell-toggle-2"
  :group 'python-mode)

(defcustom py--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu of functions and global variables."
  :type 'boolean
  :tag "py--imenu-create-index-p"
  :group 'python-mode)

(defvar py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defvar py-cleanup-p nil
  "Internally used.

Cleanup Python shell when output is used in other places.")

(defcustom py-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in ‘python-mode-map’.
Customize `py-match-paren-key' which key to use."
  :type 'boolean
  :tag "py-match-paren-mode"
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
                               `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :tag "py-match-paren-key"
  :group 'python-mode)

(defcustom py-kill-empty-line t
  "If t, ‘py-indent-forward-line’ kills empty lines."
  :type 'boolean
  :tag "py-kill-empty-line"
  :group 'python-mode)

(defcustom py-imenu-show-method-args-p nil
  "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :tag "py-imenu-show-method-args-p"
  :group 'python-mode)

(defcustom py-use-local-default nil
  "If t, ‘py-shell’ will use `py-shell-local-path'.

Alternative to default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available."
  :type 'boolean
  :tag "py-use-local-default"
  :group 'python-mode)

(defcustom py-edit-only-p nil
  "Don't check for installed Python executables.

Default is nil.

See bug report at launchpad, lp:944093."
  :type 'boolean
  :tag "py-edit-only-p"
  :group 'python-mode)

(defcustom py-force-py-shell-name-p nil
  "When t, execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence."

  :type 'boolean
  :tag "py-force-py-shell-name-p"
  :group 'python-mode)

(defcustom python-mode-v5-behavior-p nil
  "Execute region through `shell-command-on-region'.

As v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :tag "python-mode-v5-behavior-p"
  :group 'python-mode)

(defun py-toggle-python-mode-v5-behavior ()
  "Switch the values of ‘python-mode-v5-behavior-p’"
  (interactive)
  (setq python-mode-v5-behavior-p (not python-mode-v5-behavior-p))
  (when (called-interactively-p 'interactive)
    (message "python-mode-v5-behavior-p: %s" python-mode-v5-behavior-p)))

(defun py-toggle-py-verbose-p ()
  "Switch the values of ‘py-verbose-p’

Default is nil.
If on, messages value of ‘py-result’ for instance."
  (interactive)
  (setq py-verbose-p (not py-verbose-p))
  (when (called-interactively-p 'interactive)
    (message "py-verbose-p: %s" py-verbose-p)))

(defcustom py-trailing-whitespace-smart-delete-p nil
  "Default is nil.

When t, ‘python-mode’ calls
\(add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected"
  :type 'boolean
  :tag "py-trailing-whitespace-smart-delete-p"
  :group 'python-mode)

(defcustom py-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is t. See lp:1100892"
  :type 'boolean
  :tag "py-newline-delete-trailing-whitespace-p"
  :group 'python-mode)

(defcustom py--warn-tmp-files-left-p nil
  "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534."
  :type 'boolean
  :tag "py--warn-tmp-files-left-p"
  :group 'python-mode)

(defcustom py-complete-ac-sources '(ac-source-pycomplete)
  "List of ‘auto-complete’ sources assigned to `ac-sources'.

In `py-complete-initialize'.

Default is known to work an Ubuntu 14.10 - having python-
mode, pymacs and auto-complete-el, with the following minimal
Emacs initialization:

\(require 'pymacs)
\(require 'auto-complete-config)
\(ac-config-default)"
  :type 'hook
  :tag "py-complete-ac-sources"
  :options '(ac-source-pycomplete ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
  :group 'python-mode)

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
  :type 'boolean
  :tag "py-remove-cwd-from-path"
  :group 'python-mode)

(defcustom py-shell-local-path ""
  "If `py-use-local-default' is non-nil, `py-shell' will use EXECUTABLE indicated here incl. path."

  :type 'string
  :tag "py-shell-local-path"
  :group 'python-mode)

(defcustom py-python-edit-version ""
  "When not empty, fontify according to Python version specified.

Default is the empty string, a useful value \"python3\" maybe.

When empty, version is guessed via `py-choose-shell'."

  :type 'string
  :tag "py-python-edit-version"
  :group 'python-mode)

(defcustom py-ipython-execute-delay 0.3
  "Delay needed by execute functions when no IPython shell is running."
  :type 'float
  :tag "py-ipython-execute-delay"
  :group 'python-mode)

(defvar py-shell-completion-setup-code
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
  "Code used to setup completion in Python processes.")

(defvar py-shell-module-completion-code "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get completions separated by semicolons for imports.")

(defvar py-ipython-module-completion-code
  "import IPython
version = IPython.__version__
if \'0.10\' < version:
    from IPython.core.completerlib import module_completion
"
  "For IPython v0.11 or greater.
Use the following as the value of this variable:

';'.join(module_completion('''%s'''))")

(defvar py-ipython-module-completion-string
  "';'.join(module_completion('''%s'''))"
  "See also `py-ipython-module-completion-code'.")

(defcustom py--imenu-create-index-function 'py--imenu-index
  "Switch between `py--imenu-create-index-new', which also lists modules variables,  and series 5. index-machine."
  :type '(choice
	  (const :tag "'py--imenu-create-index-new, also lists modules variables " py--imenu-create-index-new)

	  (const :tag "py--imenu-create-index, series 5. index-machine" py--imenu-create-index)
	  (const :tag "py--imenu-index, honor type annotations" py--imenu-index)

	  )
  :tag "py--imenu-create-index-function"
  :group 'python-mode)

(defvar py-line-re "^"
  "Used by generated functions." )

(defvar py-input-filter-re "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defvar strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'.")

(defvar strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'.")

(defcustom py-docstring-style 'pep-257-nn
  "Implemented styles:

 are DJANGO, ONETWO, PEP-257, PEP-257-NN,SYMMETRIC, and NIL.

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
  :tag "py-docstring-style"
  :group 'python-mode)

(defcustom py-execute-directory nil
  "Stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of `py-execute-buffer' and related commands.
See also `py-use-current-dir-when-execute-p'"
  :type 'string
  :tag "py-execute-directory"
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p t
  "Current directory used for output.

See also `py-execute-directory'"
  :type 'boolean
  :tag "py-use-current-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-keep-shell-dir-when-execute-p nil
  "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'"
  :type 'boolean
  :tag "py-keep-shell-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-fileless-buffer-use-default-directory-p t
  "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shell."
  :type 'boolean
  :tag "py-fileless-buffer-use-default-directory-p"
  :group 'python-mode)

(defcustom py-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :tag "py-check-command"
  :group 'python-mode)

(defvar py-this-abbrevs-changed nil
  "Internally used by ‘python-mode-hook’.")

(defvar py-buffer-name nil
  "Internal use.

The buffer last output was sent to.")

(defvar py-orig-buffer-or-file nil
  "Internal use.")

(defcustom py-keep-windows-configuration nil
  "Takes precedence over:

 `py-split-window-on-execute' and `py-switch-buffers-on-execute-p'.
See lp:1239498

To suppres window-changes due to error-signaling also.
Set `py-keep-windows-configuration' onto 'force

Default is nil"

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" 'force))
  :tag "py-keep-windows-configuration"
  :group 'python-mode)

(defvar py-output-buffer "*Python Output*"
      "Used if ‘python-mode-v5-behavior-p’ is t.

Otherwise output buffer is created dynamically according to Python version and kind of process-handling")
;; (make-variable-buffer-local 'py-output-buffer)

(defcustom py-force-default-output-buffer-p nil
  "Enforce sending output to the default output ‘buffer-name’.

Set by defvar ‘py-output-buffer’
Bug #31 - wrong fontification caused by string-delimiters in output"

  :type 'boolean
  :tag "py-force-default-output-buffer-p"
  :group 'python-mode)

(defcustom py-shell-unbuffered t
  "Should shell output be unbuffered?.
When non-nil, this may prevent delayed and missing output in the
Python shell.  See commentary for details."
  :type 'boolean
  :safe 'booleanp
  :tag "py-shell-unbuffered"
  :group 'python-mode)

(defcustom py-shell-process-environment nil
  "List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
When this variable is non-nil, values are exported into the
process environment before starting it.  Any variables already
present in the current environment are superseded by variables
set here."
  :type '(repeat string)
  :tag "py-shell-process-environment"
  :group 'python-mode)

(defcustom py-shell-extra-pythonpaths nil
  "List of extra pythonpaths for Python shell.
When this variable is non-nil, values added at the beginning of
the PYTHONPATH before starting processes.  Any values present
here that already exists in PYTHONPATH are moved to the beginning
of the list so that they are prioritized when looking for
modules."
  :type '(repeat string)
  :tag "py-shell-extra-pythonpaths"
  :group 'python-mode)

(defcustom py-shell-exec-path nil
  "List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :tag "py-shell-exec-path"
  :group 'python-mode)

(defcustom py-shell-remote-exec-path nil
  "List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
`py-shell-exec-path' will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Python shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :tag "py-shell-remote-exec-path"
  :group 'python-mode)

(defcustom py-shell-virtualenv-root nil
  "Path to virtualenv root.
This variable, when set to a string, makes the environment to be
modified such that shells are started within the specified
virtualenv."
  :type '(choice (const nil) string)
  :tag "py-shell-virtualenv-root"
  :group 'python-mode)

(defvar py-eldoc-window-configuration nil
  "Keeps window-configuration when function ‘eldoc-mode’ is called.")

(defvar py-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        if hasattr(obj, 'startswith'):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(
                    *inspect.getargspec(target))
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args)
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
    try:
        exec('print doc')
    except SyntaxError:
        print(doc)"
  "Python code to setup documentation retrieval.")

(defcustom py-python-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        try:
            str_type = basestring
            argspec_function = inspect.getargspec
        except NameError:
            str_type = str
            argspec_function = inspect.getfullargspec
        if isinstance(obj, str_type):
            obj = eval(obj, globals())
        doc = inspect.getdoc(obj)
        if not doc and callable(obj):
            target = None
            if inspect.isclass(obj) and hasattr(obj, '__init__'):
                target = obj.__init__
                objtype = 'class'
            else:
                target = obj
                objtype = 'def'
            if target:
                args = inspect.formatargspec(*argspec_function(target))
                name = obj.__name__
                doc = '{objtype} {name}{args}'.format(
                    objtype=objtype, name=name, args=args
                )
        else:
            doc = doc.splitlines()[0]
    except:
        doc = ''
    return doc"
  "Python code to setup documentation retrieval."
  :type 'string
  :tag "py-python-eldoc-setup-code"
  :group 'python-mode)

(defcustom py-python-eldoc-string-code
  "__PYDOC_get_help('''%s''')"
  "Python code used to get a string with the documentation of an object."
  :type 'string
  :tag "py-python-eldoc-string-code"
  :group 'python-mode)

(defvar py-shell-completion-native-redirect-buffer
  " *Py completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defvar py-shell--block-prompt nil
  "Input block prompt for inferior python shell.
Do not set this variable directly, instead use
`py-shell-prompt-set-calculated-regexps'.")

(defvar py-shell-output-filter-in-progress nil)
(defvar py-shell-output-filter-buffer nil)

(defvar py-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior python shell.
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ isn't run.")

(defvar py-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior python shell.

py-shell-prompt-set-calculated-regexps
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ isn't run.
")

(defvar py-shell-prompt-output-regexp ""
  "See py-shell-prompt-output-regexps")

(defvar py-shell-prompt-output-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts.")

(defvar py-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of ‘syntax-class’ `word', not of `symbol'.
Underscores in word-class makes `forward-word'.
Travels the indentifiers. Default is t.
See also command `toggle-py-underscore-word-syntax-p'")

(defvar py-autofill-timer nil)
(defvar py-fill-column-orig fill-column)

;; defvared value isn't updated maybe
(defvar python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode")
  "Internally used. Reports the ‘python-mode’ branch.")

;; defvared value isn't updated maybe
(setq python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
	  (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode"))

(unless (fboundp 'string-to-syntax)
  ;; Skip's XE workaround
  (defun string-to-syntax (s)
    (cond
     ((equal s "|") '(15))
     ((equal s "_") '(3))
     (t (error "Unhandled string: %s" s)))))

(defvar python-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol.

Syntax or has word syntax and isn't a letter.")

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
        (if py-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

(defvar py-local-command nil
  "Returns locally used executable-name.")
(make-variable-buffer-local 'py-local-command)

(defvar py-local-versioned-command nil
  "Returns locally used executable-name including its version.")
(make-variable-buffer-local 'py-local-versioned-command)

(defvar py-ipython-completion-command-string nil
  "Select command according to IPython version.

Either ‘py-ipython0.10-completion-command-string’
or ‘py-ipython0.11-completion-command-string’.

‘py-ipython0.11-completion-command-string’ also covers version 0.12")

(defvar py-ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar py-ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions.")

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file.")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file.")
;; (setq py-shebang-regexp   "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)")

(defvar py-separator-char "/"
  "Values set by defcustom only will not be seen in batch-mode.")

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
                (error "Py-custom-temp-directory set but not writable")
              (error "Py-custom-temp-directory not an existing directory"))))
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
          (funcall ok (concat "c:" py-separator-char "Users"))
          (setq erg (concat "c:" py-separator-char "Users")))
     ;; (funcall ok ".")
     (error
      "Couldn't find a usable temp directory -- set `py-temp-directory'"))
    (when erg (setq py-temp-directory erg)))
  "Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
                          /usr/tmp, /tmp, /var/tmp, or the current directory.

                          `py-custom-temp-directory' will take precedence when setq")

(defvar py-exec-command nil
  "Internally used.")

(defvar py-which-bufname "Python")

(defvar py-pychecker-history nil)

(defvar py-pyflakes-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pylint-history nil)

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar hs-hide-comments-when-hiding-all t
  "Defined in hideshow.el, silence compiler warnings here.")

(defvar py-force-local-shell-p nil
  "Used internally, see `toggle-force-local-shell'.")

(defvar py-shell-complete-debug nil
  "For interal use when debugging, stores completions." )

(defcustom py-debug-p nil
  "When non-nil, keep resp. store information useful for debugging.

Temporary files are not deleted. Other functions might implement
some logging etc."
  :type 'boolean
  :tag "py-debug-p"
  :group 'python-mode)

(defcustom py-shell-complete-p nil
  "Enable native completion.

Set TAB accordingly. "

  :type 'boolean
  :tag "py-shell-complete-p"
  :group 'python-mode)
(make-variable-buffer-local 'py-shell-complete-p)

(defcustom py-section-start "# {{"
  "Delimit arbitrary chunks of code."
  :type 'string
  :tag "py-section-start"
  :group 'python-mode)

(defcustom py-section-end "# }}"
  "Delimit arbitrary chunks of code."
  :type 'string
  :tag "py-section-end"
  :group 'python-mode)

(defvar py-section-re py-section-start)

(defvar py-last-window-configuration nil
  "Internal use: restore ‘py-restore-window-configuration’ when completion is done resp. abandoned.")

(defvar py-exception-buffer nil
  "Will be set internally, let-bound, remember source buffer where error might occur.")

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar py-labelled-re "[ \\t]*:[[:graph:]]+"
  "When looking at label.")
;; (setq py-labelled-re "[ \\t]*:[[:graph:]]+")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "Py-expression assumes chars indicated possible composing a ‘py-expression’, skip it.")

(defvar py-expression-skip-chars "^ (=#\t\r\n\f"
  "Py-expression assumes chars indicated possible composing a ‘py-expression’, skip it.")

(setq py-expression-skip-chars "^ [{(=#\t\r\n\f")

(defvar py-expression-re "[^ =#\t\r\n\f]+"
  "Py-expression assumes chars indicated possible composing a ‘py-expression’, when ‘looking-at’ or -back.")

(defcustom py-paragraph-re paragraph-start
  "Allow Python specific ‘paragraph-start’ var."
  :type 'string
  :tag "py-paragraph-re"
  :group 'python-mode)

(defvar py-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "Py-expression assumes chars indicated probably will not compose a ‘py-expression’.")

(defvar py-not-expression-chars " #\t\r\n\f"
  "Py-expression assumes chars indicated probably will not compose a ‘py-expression’.")

(defvar py-partial-expression-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
  "Py-partial-expression assumes chars indicated possible composing a ‘py-partial-expression’, skip it.")
;; (setq py-partial-expression-backward-chars "^] .=,\"'()[{}:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")

(defvar py-partial-expression-re (concat "[" py-partial-expression-backward-chars (substring py-partial-expression-forward-chars 1) "]+"))
(setq py-partial-expression-re (concat "[" py-partial-expression-backward-chars "]+"))

(defvar py-statement-re py-partial-expression-re)
(defvar py-indent-re ".+"
  "This var is introduced for regularity only.")
(setq py-indent-re ".+")

(defvar py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of Python syntactical meaningful characters.

See also `py-assignment-re'")

;; (setq py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*")

(defvar py-delimiter-re "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs.")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of ‘py-execute-region’.

A subsequent ‘py-up-exception’ needs the line number where the region
started, in order to jump to the correct file line.
This variable is set in ‘py-execute-region’ and used in ‘py--jump-to-exception’.")

(defvar py-match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar py-XXX-tag-face 'py-XXX-tag-face)

(defvar py-pseudo-keyword-face 'py-pseudo-keyword-face)

(defvar py-variable-name-face 'py-variable-name-face)

(defvar py-number-face 'py-number-face)

(defvar py-decorators-face 'py-decorators-face)

(defvar py-object-reference-face 'py-object-reference-face)

(defvar py-builtins-face 'py-builtins-face)

(defvar py-class-name-face 'py-class-name-face)

(defvar py-exception-name-face 'py-exception-name-face)

(defvar py-import-from-face 'py-import-from-face)

(defvar py-def-class-face 'py-def-class-face)

(defvar py-try-if-face 'py-try-if-face)

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar jython-mode-hook nil
  "Hook called by `jython-mode'.
`jython-mode' also calls `python-mode-hook'.")

(defvar py-shell-hook nil
  "Hook called by `py-shell'.")

(defvar python-font-lock-keywords nil)

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?."_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar python-default-template "if"
  "Default template to expand by `python-expand-template'.
Updated on each expansion.")

(defvar py-already-guessed-indent-offset nil
  "Internal use by ‘py-indent-line’.

When `this-command' is `eq' to `last-command', use the guess already computed.")
(make-variable-buffer-local 'py-already-guessed-indent-offset)

(defvar py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-shell argprompt)
    (when (called-interactively-p 'interactive)
      (switch-to-buffer (current-buffer))
      (goto-char (point-max)))))
")

;; Constants
(defconst py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement.")

(setq py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]")

(defconst py-finally-re
  "[ \t]*\\_<finally:"
  "Regular expression matching keyword which closes a try-block.")

(defconst py-except-re "[ \t]*\\_<except\\_>"
  "Matches the beginning of a `except' block.")

;; (defconst py-except-re
;;   "[ \t]*\\_<except\\_>[:( \n\t]*"
;;   "Regular expression matching keyword which composes a try-block.")

(defconst py-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defconst py-decorator-re
  "[ \t]*@[^ ]+\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function.")

(defcustom py-outdent-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   )
  "Used by ‘py-outdent-re’."
  :type '(repeat string)
  :tag "py-outdent-re-raw"
  :group 'python-mode
  )

(defconst py-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt py-outdent-re-raw 'symbols)
   "[)\t]*")
  "Regular expression matching statements to be dedented one level.")

(defcustom py-no-outdent-re-raw
  (list
   "break"
   "continue"
   "import"
   "pass"
   "raise"
   "return")
  "Uused by ‘py-no-outdent-re’."
  :type '(repeat string)
  :tag "py-no-outdent-re-raw"
  :group 'python-mode)

(defconst py-no-outdent-re
  (concat
   "[ \t]*"
   (regexp-opt py-no-outdent-re-raw 'symbols)
   "[)\t]*$")
"Regular expression matching lines not to augment indent after.

See ‘py-no-outdent-re-raw’ for better readable content")

(defconst py-assignment-re "\\(\\_<\\w+\\_>[[:alnum:]:, \t]*[ \t]*\\)\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)\\(.*\\)"
  "If looking at the beginning of an assignment.")

;; 'name':
(defconst py-dict-re "'\\_<\\w+\\_>':")

(defvar py-block-re-raw (list
		       "async def"
		       "async for"
		       "async with"
		       "class"
		       "def"
		       "for"
		       "if"
		       "try"
		       "while"
		       "with"
		       )
  "Used by ‘py-block-re’")

(defconst py-block-re (concat
		       ;; "[ \t]*"
		       (regexp-opt py-block-re-raw 'symbols)
		       "[:( \n\t]"
		       )
  "Matches the beginning of a compound statement.")

(defconst py-minor-block-re-raw (list
				      "async for"
				      "async with"
				      "except"
				      "for"
				      "if"
				      "try"
				      "with"
				      )
  "Matches the beginning of an `for', `if', `try', `except' or `with' block.")

(defconst py-minor-block-re
  (concat
   "[ \t]*"
   (regexp-opt py-minor-block-re-raw 'symbols)
   "[:( \n\t]")

  "Regular expression matching lines not to augment indent after.

See ‘py-minor-block-re-raw’ for better readable content")

(defconst py-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a `try' block.")

(defconst py-for-re "[ \t]*\\_<\\(async for\\|for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
  "Matches the beginning of a `try' block.")

(defconst py-if-re "[ \t]*\\_<if\\_> +[^\n\r\f]+ *[: \n\t]"
  "Matches the beginning of an `if' block.")

(defconst py-else-re "[ \t]*\\_<else:[ \n\t]"
  "Matches the beginning of an `else' block.")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[( \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively.")

;; (defconst py-elif-block-re "[ \t]*\\_<elif\\_> +[[:alpha:]_][[:alnum:]_]* *[: \n\t]"
;;   "Matches the beginning of an `elif' block.")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition.")

(defconst py-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]+\\([[:alnum:]_]*\\)"
  "Matches the beginning of a class- or functions definition.

Second group grabs the name")

;; (setq py-def-or-class-re "[ \t]*\\_<\\(async def\\|class\\|def\\)\\_>[ \n\t]")

;; (defconst py-def-re "[ \t]*\\_<\\(async def\\|def\\)\\_>[ \n\t]"
(defconst py-def-re "[ \t]*\\_<\\(def\\|async def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition.")

(defcustom py-block-or-clause-re-raw
  (list
   "async for"
   "async with"
   "async def"
   "async class"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement or it's clause."
  :type '(repeat string)
  :tag "py-block-or-clause-re-raw"
  :group 'python-mode)

(defvar py-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-block-or-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defcustom py-block-re-raw
  (list
   "async with"
   "except"
   "for"
   "if"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement but not it's clause."
  :type '(repeat string)
  :tag "py-block-re-raw"
  :group 'python-mode)

(defvar py-block-re
  (concat
   "[ \t]*"
   (regexp-opt  py-block-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defcustom py-extended-block-or-clause-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with")
  "Matches the beginning of a compound statement or it's clause."
  :type '(repeat string)
  :tag "py-extended-block-or-clause-re-raw"
  :group 'python-mode)

(defconst py-extended-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-extended-block-or-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defconst py-clause-re py-extended-block-or-clause-re)

(defcustom py-minor-clause-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   )
  "Matches the beginning of a clause."
    :type '(repeat string)
    :tag "py-minor-clause-re-raw"
    :group 'python-mode)

(defconst py-minor-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-minor-clause-re-raw 'symbols)
   "[( \t]*.*:?")
  "See ‘py-minor-clause-re-raw’, which it reads.")

(defcustom py-top-level-re
  (concat
   "^[a-zA-Z_]"
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "[( \t]*.*:?")
  "A form which starts at zero indent level, but is not a comment."
  :type '(regexp)
  :tag "py-top-level-re"
  :group 'python-mode
  )

(defvar py-comment-re comment-start
  "Needed for normalized processing.")

(defconst py-block-keywords
   (regexp-opt py-block-or-clause-re-raw 'symbols)
  "Matches known keywords opening a block.

Customizing `py-block-or-clause-re-raw'  will change values here")

(defconst py-try-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:")
  "Matches the beginning of a compound try-statement's clause.")

(defcustom py-compilation-regexp-alist
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
  "Fetch errors from Py-shell.
hooked into `compilation-error-regexp-alist'"
  :type '(alist string)
  :tag "py-compilation-regexp-alist"
  :group 'python-mode)

(defun py--quote-syntax (n)
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
      (let* ((syntax (parse-partial-sexp (point-min) (point))))
	(when (eq t (nth 3 syntax))	; after unclosed fence
	  (goto-char (nth 8 syntax))	; fence position
	  ;; (skip-chars-forward "uUrR")	; skip any prefix
	  ;; Is it a matching sequence?
	  (if (eq (char-after) (char-after (match-beginning 2)))
	      (eval-when-compile (string-to-syntax "|"))))))
     ;; Consider property for initial char, accounting for prefixes.
     ((or (and (= n 2) ; leading quote (not prefix)
	       (not (match-end 1)))     ; prefix is null
	  (and (= n 1) ; prefix
	       (match-end 1)))          ; non-empty
      (unless (eq 'string (syntax-ppss-context (parse-partial-sexp (point-min) (point))))
	(eval-when-compile (string-to-syntax "|"))))
     ;; Otherwise (we're in a non-matching string) the property is
     ;; nil, which is OK.
     )))

(defconst py-font-lock-syntactic-keywords
  ;; Make outer chars of matching triple-quote sequences into generic
  ;; string delimiters.  Fixme: Is there a better way?
  ;; First avoid a sequence preceded by an odd number of backslashes.
  `((,(concat "\\(?:^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
              "\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\(?4:\"\\)\\(?5:\"\\)\\(?6:\"\\)\\|\\(?1:\"\\)\\(?2:\"\\)\\(?3:\"\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)\\(?4:'\\)\\(?5:'\\)\\(?6:'\\)\\|\\(?1:'\\)\\(?2:'\\)\\(?3:'\\)")
     (1 (py--quote-syntax 1) t t)
     (2 (py--quote-syntax 2) t t)
     (3 (py--quote-syntax 3) t t)
     (6 (py--quote-syntax 1) t t))))

(defconst py-windows-config-register 313465889
  "Internal used.")

(defvar py-windows-config nil
  "Completion stores ‘py-windows-config-register’ here.")

(put 'py-indent-offset 'safe-local-variable 'integerp)

;; testing
(defvar py-ert-test-default-executables
  (list "python" "python3" "ipython")
  "Serialize tests employing dolist.")

(defcustom py-shell-unfontify-p t
  "Run `py--run-unfontify-timer' unfontifying the shell banner-text.

Default is nil"

  :type 'boolean
  :tag "py-shell-unfontify-p"
  :group 'python-mode)

;; Pdb
;; #62, pdb-track in a shell buffer
(defcustom pdb-track-stack-from-shell-p t
  "If t, track source from shell-buffer.

Default is t.
Add hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file"

  :type 'boolean
  :tag "pdb-track-stack-from-shell-p"
  :group 'python-mode)

(defcustom py-update-gud-pdb-history-p t
  "If pdb should provide suggestions WRT file to check and ‘py-pdb-path’.

Default is t
See lp:963253"
  :type 'boolean
  :tag "py-update-gud-pdb-history-p"
  :group 'python-mode)

(defcustom py-pdb-executable nil
  "Indicate PATH/TO/pdb.

Default is nil
See lp:963253"
  :type 'string
  :tag "py-pdb-executable"
  :group 'python-mode)

(defcustom py-pdb-path
  (if (or (eq system-type 'ms-dos)(eq system-type 'windows-nt))
      (quote c:/python27/python\ -i\ c:/python27/Lib/pdb.py)
    '/usr/lib/python2.7/pdb.py)
  "Where to find pdb.py.  Edit this according to your system.
  For example \"/usr/lib/python3.4\" might be an option too.

If you ignore the location `M-x py-guess-pdb-path' might display it."
  :type 'variable
  :tag "py-pdb-path"
  :group 'python-mode)

(defvar py-python-ms-pdb-command ""
  "MS-systems might use that.")

(defcustom py-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression matching pdb input prompt of Python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :tag "py-shell-prompt-pdb-regexp"
  :group 'python-mode)

(defcustom py-pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :safe 'stringp
  :tag "py-pdbtrack-stacktrace-info-regexp"
  :group 'python-mode)

(defvar py-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`py-pdbtrack-set-tracked-buffer' instead.")

(defvar py-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defcustom py-pdbtrack-do-tracking-p t
  "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the *Python* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as ‘gud-mode’ does for debugging C programs with gdb."
  :type 'boolean
  :tag "py-pdbtrack-do-tracking-p"
  :group 'python-mode)
(make-variable-buffer-local 'py-pdbtrack-do-tracking-p)

(defcustom py-pdbtrack-filename-mapping nil
  "Supports mapping file paths when opening file buffers in pdbtrack.
When non-nil this is an alist mapping paths in the Python interpreter
to paths in Emacs."
  :type 'alist
  :tag "py-pdbtrack-filename-mapping"
  :group 'python-mode)

(defcustom py-pdbtrack-minor-mode-string " PDB"
  "String to use in the minor mode list when pdbtrack is enabled."
  :type 'string
  :tag "py-pdbtrack-minor-mode-string"
  :group 'python-mode)

(defconst py-pdbtrack-stack-entry-regexp
   (concat ".*\\("py-shell-input-prompt-1-regexp">\\|"py-ipython-input-prompt-re">\\|>\\) *\\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>()]+\\)()")
  "Regular expression pdbtrack uses to find a stack trace entry.")

(defconst py-pdbtrack-marker-regexp-file-group 2
  "Group position in gud-pydb-marker-regexp that matches the file name.")

(defconst py-pdbtrack-marker-regexp-line-group 3
  "Group position in gud-pydb-marker-regexp that matches the line number.")

(defconst py-pdbtrack-marker-regexp-funcname-group 4
  "Group position in gud-pydb-marker-regexp that matches the function name.")

(defconst py-pdbtrack-track-range 10000
  "Max number of characters from end of buffer to search for stack entry.")

(defvar py-pdbtrack-is-tracking-p nil)

(defvar py-shell--font-lock-buffer nil)

(defvar py--docbeg nil
  "internally used by py--write-edit")

(defvar py--docend nil
  "internally used by py--write-edit")

(defcustom py-completion-setup-code
  "
def __PYTHON_EL_get_completions(text):
    completions = []
    completer = None

    try:
        import readline

        try:
            import __builtin__
        except ImportError:
            # Python 3
            import builtins as __builtin__
        builtins = dir(__builtin__)

        is_ipython = ('__IPYTHON__' in builtins or
                      '__IPYTHON__active' in builtins)
        splits = text.split()
        is_module = splits and splits[0] in ('from', 'import')

        if is_ipython and is_module:
            from IPython.core.completerlib import module_completion
            completions = module_completion(text.strip())
        elif is_ipython and '__IP' in builtins:
            completions = __IP.complete(text)
        elif is_ipython and 'get_ipython' in builtins:
            completions = get_ipython().Completer.all_completions(text)
        else:
            # Try to reuse current completer.
            completer = readline.get_completer()
            if not completer:
                # importing rlcompleter sets the completer, use it as a
                # last resort to avoid breaking customizations.
                import rlcompleter
                completer = readline.get_completer()
            if getattr(completer, 'PYTHON_EL_WRAPPED', False):
                completer.print_mode = False
            i = 0
            while True:
                completion = completer(text, i)
                if not completion:
                    break
                i += 1
                completions.append(completion)
    except:
        pass
    finally:
        if getattr(completer, 'PYTHON_EL_WRAPPED', False):
            completer.print_mode = True
    return completions"
  "Code used to setup completion in inferior Python processes."
  :type 'string
  :tag "py-completion-setup-code"
  :group 'python-mode)

(defcustom py-shell-completion-string-code
  "';'.join(__PYTHON_EL_get_completions('''%s'''))"
  "Python code used to get a string of completions separated by semicolons.
The string passed to the function is the current python name or
the full statement in the case of imports."
  :type 'string
  :tag "py-shell-completion-string-code"
  :group 'python-mode)

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

 ;; GNU's syntax-ppss-context
(unless (functionp 'syntax-ppss-context)
  (defsubst syntax-ppss-context (ppss)
    (cond
     ((nth 3 ppss) 'string)
     ((nth 4 ppss) 'comment)
     (t nil))))

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :tag "py-XXX-tag-face"
  :group 'python-mode)

(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also `py-object-reference-face'"
  :tag "py-pseudo-keyword-face"
  :group 'python-mode)

(defface py-object-reference-face
  '((t (:inherit py-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method., commonly \"cls\" and \"self\""
  :tag "py-object-reference-face"
  :group 'python-mode)

(defface py-variable-name-face
  '((t (:inherit default)))
  "Face method decorators."
  :tag "py-variable-name-face"
  :group 'python-mode)

(defface py-number-face
 '((t (:inherit default)))
  "Highlight numbers."
  :tag "py-number-face"
  :group 'python-mode)

(defface py-try-if-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-try-if-face"
  :group 'python-mode)

(defface py-import-from-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-import-from-face"
  :group 'python-mode)

(defface py-def-class-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords."
  :tag "py-def-class-face"
  :group 'python-mode)

 ;; PEP 318 decorators
(defface py-decorators-face
  '((t (:inherit font-lock-keyword-face)))
  "Face method decorators."
  :tag "py-decorators-face"
  :group 'python-mode)

(defface py-builtins-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for builtins like TypeError, object, open, and exec."
  :tag "py-builtins-face"
  :group 'python-mode)

(defface py-class-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for classes."
  :tag "py-class-name-face"
  :group 'python-mode)

(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Python exceptions."
  :tag "py-exception-name-face"
  :group 'python-mode)

;; subr-x.el might not exist yet
;; #73, Byte compilation on Emacs 25.3 fails on different trim-right signature
(unless (or (functionp 'string-trim) (functionp 'string-trim-right)(functionp 'string-trim-left))
  (defsubst string-trim-left (strg &optional regexp)
    "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
    (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") strg)
	(replace-match "" t t strg)
      strg))

  (defsubst string-trim-right (strg &optional regexp)
    "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
    (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") strg)
	(replace-match "" t t strg)
      strg))

  (defsubst string-trim (strg &optional trim-left trim-right)
    "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
    (string-trim-left (string-trim-right strg trim-right) trim-left))

  (defsubst string-blank-p (strg)
    "Check whether STRING is either empty or only whitespace."
    (string-match-p "\\`[ \t\n\r]*\\'" strg))

  (defsubst string-remove-prefix (prefix strg)
    "Remove PREFIX from STRING if present."
    (if (string-prefix-p prefix strg)
	(substring strg (length prefix))
      strg)))

(defun py-toggle-imenu-create-index ()
  "Toggle value of ‘py--imenu-create-index-p’"
  (interactive)
  (setq py--imenu-create-index-p (not py--imenu-create-index-p))
  (when (called-interactively-p 'interactive)
    (message "py--imenu-create-index-p: %s" py--imenu-create-index-p)))

(defun py-toggle-shell-completion ()
  "Switches value of buffer-local var ‘py-shell-complete-p’"
  (interactive)
    (setq py-shell-complete-p (not py-shell-complete-p))
    (when (called-interactively-p 'interactive)
      (message "py-shell-complete-p: %s" py-shell-complete-p)))

(defun py--python-send-setup-code-intern (name buffer)
  (save-excursion
    (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-" name "-setup-code.py"))
	  py-return-result-p py-store-result-p)
      (unless (file-readable-p setup-file)
	(with-temp-buffer
	  (insert (eval (car (read-from-string (concat "py-" name "-setup-code")))))
	  (write-file setup-file)))
      (py--execute-file-base (get-buffer-process buffer) setup-file nil buffer nil t)
      ;; (when py-verbose-p (message "%s" (concat name " setup-code sent to " (process-name (get-buffer-process buffer)))))
      )))

(defun py--python-send-completion-setup-code (buffer)
  "For Python see py--python-send-setup-code."
  (py--python-send-setup-code-intern "shell-completion" buffer))

(defun py--python-send-eldoc-setup-code (buffer)
  "For Python see py--python-send-setup-code."
  (py--python-send-setup-code-intern "eldoc" buffer))

(defun py--ipython-import-module-completion ()
  "Setup IPython v0.11 or greater.

Used by `py-ipython-module-completion-string'"
  (let ((setup-file (concat (py--normalize-directory py-temp-directory) "py-ipython-module-completion.py")))
    (unless (file-readable-p setup-file)
      (with-temp-buffer
	(insert py-ipython-module-completion-code)
	(write-file setup-file)))
    (py--execute-file-base nil setup-file nil (current-buffer) nil t)))

(defun py--at-raw-string ()
  "If at beginning of a raw-string."
  (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R))))

(defun py--docstring-p (pos)
  "Check to see if there is a docstring at POS."
  (save-excursion
    (let ((erg
	   (progn
	     (goto-char pos)
	     (and (looking-at "\"\"\"\\|'''")
		  ;; https://github.com/swig/swig/issues/889
		  ;; def foo(rho, x):
		  ;;     r"""Calculate :math:`D^\nu \rho(x)`."""
		  ;;     return True
		  (if (py--at-raw-string)
		      (progn
			(forward-char -1)
			(point))
		    (point))))))
      (when (and erg (py-backward-statement))
	(when (looking-at py-def-or-class-re)
	  erg)))))

(defun py--font-lock-syntactic-face-function (state)
  "STATE expected as result von (parse-partial-sexp (point-min) (point)."
  (if (nth 3 state)
      (if (py--docstring-p (nth 8 state))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-choose-shell-by-shebang (&optional shebang)
  "Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name."
  (interactive)
  ;; look for an interpreter specified in the first line
  (let* (erg res)
    (save-excursion
      (goto-char (point-min))
      (when (looking-at py-shebang-regexp)
        (if shebang
            (setq erg (match-string-no-properties 0))
          (setq erg (split-string (match-string-no-properties 0) "[#! \t]"))
          (dolist (ele erg)
            (when (string-match "[bijp]+ython" ele)
              (setq res ele))))))
    (when (and py-verbose-p (called-interactively-p 'any)) (message "%s" res))
    res))

(defun py--choose-shell-by-import ()
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

(defun py-choose-shell-by-path (&optional separator-char)
  "SEPARATOR-CHAR according to system ‘path-separator’.

Select Python executable according to version desplayed in path.
Returns versioned string, nil if nothing appropriate found"
  (interactive)
  (let ((path (py--buffer-filename-remote-maybe))
	(separator-char (or separator-char py-separator-char))
                erg)
    (when (and path separator-char
               (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat separator-char "[iI]?[pP]ython[0-9.]+" separator-char) path)) (1- (match-end 0)))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun py-which-python (&optional shell)
  "Return version of Python of current environment, a number.
Optional argument SHELL selected shell."
  (interactive)
  (let* ((cmd (or shell (py-choose-shell)))
	 (treffer (string-match "\\([23]*\\.?[0-9\\.]*\\)$" cmd))
         version erg)
    (if treffer
        ;; if a number if part of python name, assume it's the version
        (setq version (substring-no-properties cmd treffer))
      (setq erg (shell-command-to-string (concat cmd " --version")))
      (setq version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                           (match-string-no-properties 2 erg))
                          ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                           (substring erg 7 (1- (length erg)))))))
    (when (called-interactively-p 'any)
      (if version
          (when py-verbose-p (message "%s" version))
        (message "%s" "Could not detect Python on your system")))
    (string-to-number version)))

(defun py-python-current-environment ()
  "Return path of current Python installation."
  (interactive)
  (let* ((cmd (py-choose-shell))
         (denv (shell-command-to-string (concat "type " cmd)))
         (erg (substring denv (string-match "/" denv))))
    (when (called-interactively-p 'any)
      (if erg
          (message "%s" erg)
        (message "%s" "Could not detect Python on your system")))
    erg))

 ;; requested by org-mode still
(defalias 'py-toggle-shells 'py-choose-shell)

(defun py--cleanup-process-name (res)
  "Make res ready for use by `executable-find'.

Returns RES or substring of RES"
  (if (string-match "<" res)
      (substring res 0 (match-beginning 0))
    res))

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional shell)
  "Return an appropriate executable as a string.

Does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py--choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name
Return nil, if no executable found."
  (interactive)
  ;; org-babel uses ‘py-toggle-shells’ with arg, just return it
  (or shell
      (let* (res
	     done
	     (erg
	      (cond (py-force-py-shell-name-p
		     (default-value 'py-shell-name))
		    (py-use-local-default
		     (if (not (string= "" py-shell-local-path))
			 (expand-file-name py-shell-local-path)
		       (message "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'")))
		    ((and (not py-fast-process-p)
			  (comint-check-proc (current-buffer))
			  (setq done t)
			  (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
		     (setq res (process-name (get-buffer-process (current-buffer))))
		     (py--cleanup-process-name res))
		    ((py-choose-shell-by-shebang))
		    ((py--choose-shell-by-import))
		    ((py-choose-shell-by-path))
		    (t (or
			(default-value 'py-shell-name)
			"python"))))
	     (cmd (if (or
		       ;; comint-check-proc was succesful
		       done
		       py-edit-only-p) erg
		    (executable-find erg))))
	(if cmd
	    (when (called-interactively-p 'any)
	      (message "%s" cmd))
	  (when (called-interactively-p 'any) (message "%s" "Could not detect Python on your system. Maybe set `py-edit-only-p'?")))
	erg)))


(defun py--normalize-directory (directory)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (cond ((string-match (concat py-separator-char "$") directory)
         directory)
        ((not (string= "" directory))
         (concat directory py-separator-char))))

(defun py--normalize-pythonpath (pythonpath)
  "Make sure PYTHONPATH ends with a colon.

Returns PYTHONPATH"
  (let ((erg (cond ((string-match (concat path-separator "$") pythonpath)
                    pythonpath)
                   ((not (string= "" pythonpath))
                    (concat pythonpath path-separator))
		   (t pythonpath))))
    erg))

(defun py-install-directory-check ()
  "Do some sanity check for `py-install-directory'.

Returns t if successful."
  (interactive)
  (let ((erg (and (boundp 'py-install-directory) (stringp py-install-directory) (< 1 (length py-install-directory)))))
    (when (called-interactively-p 'any) (message "py-install-directory-check: %s" erg))
    erg))

(defun py-guess-py-install-directory ()
  "Takes value of user directory aka $HOME.

If `(locate-library \"python-mode\")' is not succesful.

Used only, if `py-install-directory' is empty."
  (interactive)
  (let ((erg (cond ((locate-library "python-mode")
		    (file-name-directory (locate-library "python-mode")))
		   ((ignore-errors (string-match "python-mode" (py--buffer-filename-remote-maybe)))
		    (file-name-directory (py--buffer-filename-remote-maybe)))
		   ((string-match "python-mode" (buffer-name))
		    default-directory))))
    (cond ((and (or (not py-install-directory) (string= "" py-install-directory)) erg)
	   (setq py-install-directory erg))
	  (t (setq py-install-directory (expand-file-name "~/")))))
  (when (and py-verbose-p (called-interactively-p 'any)) (message "Setting py-install-directory to: %s" py-install-directory))
  py-install-directory)

(defun py--fetch-pythonpath ()
  "Consider settings of ‘py-pythonpath’."
  (if (string= "" py-pythonpath)
      (getenv "PYTHONPATH")
    (concat (py--normalize-pythonpath (getenv "PYTHONPATH")) py-pythonpath)))

(defun py-load-pymacs ()
  "Load Pymacs as delivered.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.ca"
  (interactive)
  (let ((pyshell (py-choose-shell))
        (path (py--fetch-pythonpath))
        (py-install-directory (cond ((string= "" py-install-directory)
                                     (py-guess-py-install-directory))
                                    (t (py--normalize-directory py-install-directory)))))
    (if (py-install-directory-check)
        (progn
          ;; If Pymacs has not been loaded before, prepend py-install-directory to
          ;; PYTHONPATH, so that the Pymacs delivered with python-mode is used.
          (unless (featurep 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  (expand-file-name py-install-directory)
                                  (if path (concat path-separator path)))))
          (setenv "PYMACS_PYTHON" (if (string-match "IP" pyshell)
                                      "python"
                                    pyshell))
          (require 'pymacs))
      (error "`py-install-directory' not set, see INSTALL"))))

(when py-load-pymacs-p (py-load-pymacs))

(when (and py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (py--fetch-pythonpath))
           (py-install-directory (cond ((string= "" py-install-directory)
                                        (py-guess-py-install-directory))
                                       (t (py--normalize-directory py-install-directory))))
           (pycomplete-directory (concat (expand-file-name py-install-directory) "completion")))
      (if (py-install-directory-check)
          (progn
            ;; If the Pymacs process is already running, augment its path.
            (when (and (get-process "pymacs") (fboundp 'pymacs-exec))
              (pymacs-exec (concat "sys.path.insert(0, '" pycomplete-directory "')")))
            (require 'pymacs)
            (setenv "PYTHONPATH" (concat
                                  pycomplete-directory
                                  (if path (concat path-separator path))))
            (push pycomplete-directory load-path)
            (require 'pycomplete)
            (add-hook 'python-mode-hook 'py-complete-initialize))
        (error "`py-install-directory' not set, see INSTALL")))))

(when (functionp 'py-load-pycomplete)
  (py-load-pycomplete))

(defun py-set-load-path ()
  "Include needed subdirs of ‘python-mode’ directory."
  (interactive)
  (let ((install-directory (py--normalize-directory py-install-directory)))
    (cond ((and (not (string= "" install-directory))(stringp install-directory))
           (push (expand-file-name install-directory) load-path)
           (push (concat (expand-file-name install-directory) "completion")  load-path)
           (push (concat (expand-file-name install-directory) "extensions")  load-path)
           (push (concat (expand-file-name install-directory) "test") load-path)
           (push (concat (expand-file-name install-directory) "tools")  load-path)
           (push (concat (expand-file-name install-directory) "autopair")  load-path))
          (t (error "Please set `py-install-directory', see INSTALL"))
          (when (called-interactively-p 'any) (message "%s" load-path)))))

(unless py-install-directory
  (push default-directory  load-path)
  (push (concat default-directory "extensions")  load-path))

(defun py-count-lines (&optional beg end)
  "Count lines in accessible part until current line.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115
Optional argument BEG specify beginning.
Optional argument END specify end."
  (interactive)
  (save-excursion
    (let ((count 0)
	  (beg (or beg (point-min)))
	  (end (or end (point))))
      (save-match-data
	(if (or (eq major-mode 'comint-mode)
		(eq major-mode 'py-shell-mode))
	    (if
		(re-search-backward py-shell-prompt-regexp nil t 1)
		(goto-char (match-end 0))
	      ;; (when py-debug-p (message "%s"  "py-count-lines: Don't see a prompt here"))
	      (goto-char beg))
	  (goto-char beg)))
      (while (and (< (point) end)(not (eobp)) (skip-chars-forward "^\n" end))
        (setq count (1+ count))
        (unless (or (not (< (point) end)) (eobp)) (forward-char 1)
                (setq count (+ count (abs (skip-chars-forward "\n" end))))))
      (when (bolp) (setq count (1+ count)))
      (when (and py-debug-p (called-interactively-p 'any)) (message "%s" count))
      count)))

(defmacro py-escaped ()
  "Return t if char is preceded by an odd number of backslashes."
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro py-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

(defmacro py-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (py-escaped))))

(defun py--escape-doublequotes (start end)
  "Escape doublequotes in region by START END."
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp)) (< 0 (abs (skip-chars-forward "^\"" end))))
	(when (eq (char-after) ?\")
	  (unless (py-escaped)
	    (insert "\\")
	    (forward-char 1)))))))

(defun py--escape-open-paren-col1 (start end)
  "Start from position START until position END."
  (goto-char start)
  (while (re-search-forward "^(" end t 1)
    (insert "\\")
    (end-of-line)))

(and py-company-pycomplete-p (require 'company-pycomplete))

(defcustom py-empty-line-p-chars "^[ \t\r]*$"
  "empty-line-p-chars"
  :type 'regexp
  :tag "py-empty-line-p-chars"
  :group 'python-mode)

(defcustom py-default-working-directory ""
  "If not empty used by ‘py-set-current-working-directory’"
  :type 'string
  :tag "py-default-working-directory"
  :group 'python-mode)



(defun py-empty-line-p (&optional iact)
  "Return t if cursor is at an empty line, nil otherwise.
Optional argument INTERACT bla."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when (and iact py-verbose-p)
      (message "%s" (looking-at py-empty-line-p-chars)))
    (looking-at py-empty-line-p-chars)))

(defun py-toggle-closing-list-dedents-bos (&optional arg)
  "Switches boolean variable ‘py-closing-list-dedents-bos’.

With optional ARG message state switched to"
  (interactive "p")
  (setq py-closing-list-dedents-bos (not py-closing-list-dedents-bos))
  (when arg (message "py-closing-list-dedents-bos: %s" py-closing-list-dedents-bos)))

(defmacro py-test-with-temp-buffer-point-min (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (python-mode)
       (goto-char (point-min))
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

(defmacro py-test-with-temp-buffer (contents &rest body)
  "Create temp buffer in `python-mode' inserting CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
 at the end of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let (hs-minor-mode py--imenu-create-index-p)
       (insert ,contents)
       (python-mode)
       (when py-debug-p
	 (switch-to-buffer (current-buffer))
	 (font-lock-fontify-region (point-min) (point-max)))
       ,@body)))

;; from jit-lock.el
(defmacro with-buffer-prepared-for-jit-lock (&rest body)
  "Execute BODY in current buffer, overriding several variables.
Preserves the `buffer-modified-p' state of the current buffer."
  (declare (debug t))
  `(let ((inhibit-point-motion-hooks t))
     (with-silent-modifications
       ,@body)))

(require 'python-components-menu)
(require 'python-components-extra)
(require 'python-components-map)
(require 'python-components-switches)
(require 'python-components-edit)
(require 'python-components-backward-forms)
(require 'python-components-move)
(require 'python-components-kill-forms)
(require 'python-components-mark-forms)
(require 'python-components-copy-forms)
(require 'python-components-delete-forms)
(require 'python-components-execute)
(require 'python-components-send)
(require 'python-components-shell-complete)
(require 'python-components-pdbtrack)
(require 'python-components-pdb)
(require 'python-components-help)
(require 'python-components-extensions)
(require 'python-components-imenu)
;; now installed by py-load-named-shells
;; (require 'python-components-named-shells)
(require 'python-components-electric)
(require 'python-components-virtualenv)
(require 'python-components-booleans-beginning-forms)
(require 'python-components-booleans-end-forms)
(require 'python-components-beginning-position-forms)
(require 'python-components-end-position-forms)
(require 'python-components-forward-forms)
(require 'python-components-up-down)
(require 'python-components-exec-forms)
(require 'python-components-extended-executes)
(require 'python-abbrev-propose)
(require 'python-components-paragraph)
(require 'python-components-shift-forms)
(require 'python-components-execute-file)
(require 'python-components-comment)
(require 'python-components-forms-code)
(require 'python-components-fast-forms)
(require 'python-components-auto-fill)
(require 'python-components-fast-complete)
(require 'python-components-intern)
(require 'python-components-named-shells)
(require 'python-components-close-forms)
(require 'python-components-section-forms)
(require 'python-components-narrow)
(require 'python-components-ffap)
(require 'python-components-shell-menu)
(require 'python-components-hide-show)
(require 'python-components-foot)

(provide 'python-components-mode)
;;; python-components-mode.el ends here
