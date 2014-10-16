;;; python-components-mode.el --- Towards an Python Development Emacs Environment

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

(provide 'python-components-mode)
(provide 'python-mode)

(require 'ansi-color)
(require 'cc-cmds)
(require 'cl)
(require 'comint)
(require 'compile)
(require 'custom)
(require 'flymake)
(require 'hippie-exp)
(require 'python-components-macros)
(require 'python-components-nomacros)
(require 'shell)
(require 'thingatpt)
;; (require 'python)

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

(defcustom info-lookup-mode "python"
  "Which Python documentation should be queried.

Make sure it's accessible from Emacs by M-x info RET ...
See INSTALL-INFO-FILES for help. "

  :type 'string
  :group 'python-mode)

(defcustom py-fast-process-p nil
  "Use `py-fast-process'.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Results arrive in output buffer, which is not in comint-mode"

  :type 'boolean
  :group 'python-mode)

(defcustom py-shell-unfontify-p t
 "Run `py--run-unfontify-timer' unfontifying the shell banner-text.

Default is nil "

:type 'boolean
:group 'python-mode)

(defcustom py-session-p t
  "If commands would use an existing process.

If nil, a maybe existing process at py-buffer-name would be killed and re-started

See also `py-dedicated-process-p'
"

  :type 'boolean
  :group 'python-mode)

(defcustom py-max-help-buffer-p nil
 "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  "

:type 'boolean
:group 'python-mode)

(defcustom py-highlight-error-source-p nil
 "When py-execute-... commands raise an error, respective code in source-buffer will be highlighted. Default is nil.

M-x `py-remove-overlays-at-point' removes that highlighting.
 "
:type 'boolean
:group 'python-mode)

(defcustom py-set-pager-cat-p nil
 "If the shell environment variable $PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' "

:type 'boolean
:group 'python-mode)

(defcustom py-empty-line-closes-p nil
 "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents."

:type 'boolean
:group 'python-mode)

(defcustom py-prompt-on-changed-p t
 "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is `t'"

:type 'boolean
:group 'python-mode)

(defcustom py-dedicated-process-p nil
  "If commands executing code use a dedicated shell.

Default is nil

When non-nil and `py-session-p', an existing dedicated process is re-used instead of default - which allows executing stuff in parallel.
")

(defcustom py-store-result-p nil
 "When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked.

Default is nil"

:type 'boolean
:group 'python-mode)

(defvar py-return-result-p t
 "Internally used. When non-nil, return resulting string of `py-execute-...' functions. Imports will use it with nil.

Default is t")

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-load-skeletons-p nil
  "If skeleton definitions should be loaded, default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-if-name-main-permission-p t
  "Allow execution of code inside blocks started
by \"if __name__== '__main__':\".

Default is non-nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-use-font-lock-doc-face-p nil
  "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.
Call M-x `customize-face' in order to have a visible effect. "

  :type 'boolean
  :group 'python-mode)

(defcustom empty-comment-line-separates-paragraph-p t
  "Consider paragraph start/end lines with nothing inside but comment sign.

Default is  non-nil"
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-inline-comment nil
  "If non-nil, indents to column of inlined comment start.
Default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-auto-fill-mode nil
  "If python-mode should set fill-column

according values in `py-comment-fill-column' and `py-docstring-fill-column'.
Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defcustom py-error-markup-delay 4
  "Seconds error's are highlighted in exception buffer. "

  :type 'integer
  :group 'python-mode)

(defcustom py-fast-completion-delay 0.1
  "Used by py--fast-send-string-intern. "

  :type 'float
  :group 'python-mode)

(defcustom py-new-shell-delay
    (if (eq system-type 'windows-nt)
      2.0
    0.2)

  "If a new comint buffer is connected to Python, commands like completion might need some delay. "

  :type 'float
  :group 'python-mode)

(defcustom py-ac-length-min 2
  "Auto-complete takes action only with minimum length of word before point, set here.

Default is 2"

  :type 'integer
  :group 'python-mode)

(defcustom py-completion-delay 4
  "Seconds completion-buffer is shown, if any. "

  :type 'integer
  :group 'python-mode)

(defcustom py-autofill-timer-delay 1
  "Delay when idle before functions ajusting  `py-docstring-fill-column' resp. `py-comment-fill-column' are called. "
  :type 'integer

  :group 'python-mode)

(defcustom py-docstring-fill-column 72
  "Value of `fill-column' to use when filling a docstring.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :group 'python-mode)

(defcustom py-comment-fill-column 79
  "Value of `fill-column' to use when filling a comment.
Any non-integer value means do not use a different value of
`fill-column' when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current `fill-column'" t))
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives "

  :type 'boolean
  :group 'python-mode)
;; (make-variable-buffer-local 'py-fontify-shell-buffer-p)

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

(defun py-smart-operator-check ()
  "Check, if smart-operator-mode is loaded resp. available.

Give some hints, if not."
  (interactive)
  (if (featurep 'smart-operator)
      't
    (progn
      (and (boundp 'py-smart-operator-mode-p) py-smart-operator-mode-p (message "%s" "Don't see smart-operator.el. Make sure, it's installed. See in menu Options, Manage Emacs Packages. Or get it from source: URL: http://xwl.appspot.com/ref/smart-operator.el")
           nil))))

(defun py-autopair-check ()
  "Check, if autopair-mode is available.

Give some hints, if not."
  (interactive)
  (if (featurep 'autopair)
      't
    (progn
      (message "py-autopair-check: %s" "Don't see autopair.el. Make sure, it's installed. If not, maybe see source: URL: http://autopair.googlecode.com")
      nil)))

(defvar smart-operator-mode nil)
(defvar highlight-indent-active nil)
(defvar autopair-mode nil)

(defvar py-result nil
  "Internally used. May store result from Python process. ")

(defvar py-error nil
  "Internally used. Takes the error-messages from Python process. ")

(defvar py-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used")

(defvar py-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used")

(defcustom py-timer-close-completions-p t
 "If `py-timer-close-completion-buffer' should run, default is non-nil. "

:type 'boolean
:group 'python-mode)

(defcustom py-smart-operator-mode-p nil
  "If python-mode calls (smart-operator-mode-on)

Default is nil. "

  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (and (py-smart-operator-check)
              (set-default symbol value)
              (smart-operator-mode (if value 1 0)))))
;; (make-variable-buffer-local 'py-smart-operator-mode-p)

(defvar py-autopair-mode nil)
(defcustom py-autopair-mode nil
  "If python-mode calls (autopair-mode-on)

Default is nil
Load `autopair-mode' written by Joao Tavora <joaotavora [at] gmail.com>
URL: http://autopair.googlecode.com "
  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (and
          ;; (py-autopair-check)
              (set-default symbol value)
              (autopair-mode (if value 1 0)))))
;; (make-variable-buffer-local 'py-autopair-mode)

(defcustom py-no-completion-calls-dabbrev-expand-p t
  "If completion function should call dabbrev-expand when no completion found. Default is `t'

See  `py-indent-no-completion-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-no-completion-p nil
  "If completion function should insert a TAB when no completion found. Default is `nil'

See also `py-no-completion-calls-dabbrev-expand-p'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-company-pycomplete-p nil
  "Load company-pycomplete stuff. Default is  nil"

  :type 'boolean
  :group 'python-mode)

(defvar py-auto-completion-mode-p nil
  "Internally used by `py-auto-completion-mode'")

(defvar py-complete-last-modified nil
  "Internally used by `py-auto-completion-mode'")

(defvar py--auto-complete-timer nil
  "Internally used by `py-auto-completion-mode'")

(defvar py-auto-completion-buffer nil
  "Internally used by `py-auto-completion-mode'")

(defvar py--auto-complete-timer nil)
;; (make-variable-buffer-local 'py--auto-complete-timer)

(defcustom py--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See `py-auto-completion-mode'"

  :type 'number
  :group 'python-mode)

(defcustom py-auto-complete-p nil
  "Run python-mode's built-in auto-completion via py-complete-function. Default is  nil"

  :type 'boolean
  :group 'python-mode)
(make-variable-buffer-local 'py-auto-complete-p)

(defcustom py-tab-shifts-region-p nil
  "If `t', TAB will indent/cycle the region, not just the current line.

Default is  nil
See also `py-tab-indents-region-p'"

  :type 'boolean
  :group 'python-mode)

(defcustom py-tab-indents-region-p nil
  "When `t' and first TAB doesn't shift, indent-region is called.

Default is  nil
See also `py-tab-shifts-region-p'"

  :type 'boolean
  :group 'python-mode)

(defcustom py-block-comment-prefix-p t
  "If py-comment inserts py-block-comment-prefix.

Default is t"

  :type 'boolean
  :group 'python-mode)

(defcustom py-org-cycle-p nil
  "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' "

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
  "If Pymacs related stuff should be loaded.

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

(defcustom py-sexp-function nil
  "When set, it's value is called instead of `forward-sexp', `backward-sexp'

Default is nil. "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "py-end-of-partial-expression" py-end-of-partial-expression)
          (const :tag "py-end-of-expression" py-end-of-expression))
  :group 'python-mode)
;; (make-variable-buffer-local 'py-sexp-function)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block isn't empty. Default is non-nil.

When non-nil, `py-end-of-def' and related will work faster"
  :type 'boolean
  :group 'python-mode)
;; (make-variable-buffer-local 'py-close-provides-newline)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-honors-multiline-listing nil
  "If `t', indents to 1+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-indent-paren-spanned-multilines-p nil
  "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
"
  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-dedents-bos nil
  "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6,
]

result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6,
    ]
result = some_function_that_takes_arguments(
    'a', 'b', 'c',
    'd', 'e', 'f',
    )

Examples from PEP8"

  :type 'boolean
  :group 'python-mode)

(defcustom py-closing-list-space 1
  "Number of chars, closing parenthesis outdent from opening, default is 1 "
  :type 'number
  :group 'python-mode)

(defcustom py-max-specpdl-size max-specpdl-size
  "Heuristic exit. Limiting number of recursive calls by py-end-of-statement and related functions. Default is max-specpdl-size.

This threshold is just an approximation. It might set far higher maybe.

See lp:1235375. In case code is not to navigate due to errors, `which-function-mode' and others might make Emacs hang. Rather exit than. "

  :type 'number
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-kill-backward-p nil
  "Affects `py-electric-backspace'. Default is nil.

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
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions.

See also `py-electric-colon-bobl-only' "
  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block

See lp:1207405 resp. `py-electric-colon-active-p' "

  :type 'boolean
  :group 'python-mode)

(defcustom py-electric-yank-active-p nil
  " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nil"
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

(defcustom py-electric-comment-p nil
  "If \"#\" should call `py-electric-comment'. Default is `nil'. "
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

(defcustom py-defun-use-top-level-p nil
 "When non-nil, keys C-M-a, C-M-e address top-level form.

Default is nil.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc."

:type 'boolean
:group 'python-mode)

(defcustom py-tab-indent t
  "Non-nil means TAB in Python mode calls `py-indent-line'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-return-key 'py-newline-and-indent
  "Which command <return> should call. "
  :type '(choice
          (const :tag "default" py-newline-and-indent)
          (const :tag "newline" newline)
          (const :tag "py-newline-and-indent" py-newline-and-indent)
          (const :tag "py-newline-and-dedent" py-newline-and-dedent)
          )
  :group 'python-mode)

(defcustom py-complete-function 'py-fast-complete
  "When set, enforces function todo completion, default is `py-fast-complete'.

Might not affect IPython, as `py-shell-complete' is the only known working here.
Normally python-mode knows best which function to use. "
  :type '(choice
          (const :tag "default" nil)
          (const :tag "Pymacs and company based py-complete" py-complete)
          (const :tag "py-shell-complete" py-shell-complete)
          (const :tag "py-indent-or-complete" py-indent-or-complete)
	  (const :tag "py-fast-complete" py-fast-complete)
          )
  :group 'python-mode)
;; (make-variable-buffer-local 'py-complete-function)

(defcustom ipython-complete-function 'ipython-complete
  "Function used for completion in IPython shell buffers. "
  :type '(choice (const :tag "py-shell-complete" py-shell-complete)
                 (const :tag "Pymacs based py-complete" py-complete)
                 (const :tag "IPython's ipython-complete" ipython-complete))
  :group 'python-mode)
;; (make-variable-buffer-local 'ipython-complete-function)

(defcustom py-encoding-string " # -*- coding: utf-8 -*-"
  "Default string specifying encoding of a Python file. "
  :type 'string
  :group 'python-mode)

(defcustom py-shebang-startstring "#! /bin/env"
  "Detecting the shell in head of file. "
  :type 'string
  :group 'python-mode)

(defcustom py-flake8-command ""
  "Which command to call flake8.

If empty, python-mode will guess some "
  :type 'string
  :group 'python-mode)

(defcustom py-flake8-command-args ""
  "Arguments used by flake8.

Default is the empty string. "
  :type 'string
  :group 'python-mode)

(defvar py-flake8-history nil
  "Used by flake8, resp. py-flake8-command.

Default is nil. ")

(defcustom py-message-executing-temporary-file t
  "If execute functions using a temporary file should message it. Default is `t'.

Messaging increments the prompt counter of IPython shell. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-execute-no-temp-p nil
  "Seems Emacs-24.3 provided a way executing stuff without temporary files. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-lhs-inbound-indent 1
  "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. "
  :type 'integer
  :group 'python-mode)
;; (make-variable-buffer-local 'py-lhs-inbound-indent)

(defcustom py-cleanup-temporary t
  "If temporary buffers and files used by functions executing region should be deleted afterwards. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-continuation-offset 2
  "Additional amount of offset to give for some continuation lines.
Continuation lines are those that immediately follow a backslash
terminated line. "
  :type 'integer
  :group 'python-mode)

(defcustom py-indent-tabs-mode nil
  "Python-mode starts `indent-tabs-mode' with the value specified here, default is nil. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-smart-indentation t
  "Should `python-mode' try to automagically set some indentation variables?
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
;; (make-variable-buffer-local 'py-smart-indentation)

(defcustom py-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
 `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-indent-offset 4
  "Amount of offset per level of indentation.
 `\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to `py-indent-offset' indicates "
  :type 'integer
  :group 'python-mode)
;; (make-variable-buffer-local 'py-backslashed-lines-indent-offset)

(defcustom pdb-path '/usr/lib/python2.7/pdb.py
  "Where to find pdb.py. Edit this according to your system.

If you ignore the location `M-x py-guess-pdb-path' might display it."
  :type 'variable
  :group 'python-mode)

(defcustom py-indent-comments t
  "When t, comment lines are indented. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-uncomment-indents-p nil
  "When non-nil, after uncomment indent lines. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-separator-char 47
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function `py-separator-char'. "
  :type 'character
  :group 'python-mode)
;; (make-variable-buffer-local 'py-separator-char)
;; used as a string finally
;; kept a character not to break existing customizations
(and (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from `py-temp-directory'. Default is the empty string. "
  :type 'string
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
  :group 'python-mode)

(defcustom py-jump-on-exception t
  "Jump to innermost exception frame in Python output buffer.
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

(defcustom py-delete-function 'delete-char
  "Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python-mode)

(defcustom py-pdbtrack-do-tracking-p t
  "Controls whether the pdbtrack feature is enabled or not.
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
  "String to use in the minor mode list when pdbtrack is enabled."
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

(defcustom py--send-receive-delay 5
  "Seconds to wait for output, used by `py--send-receive'. "

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
  "Shell command used to run Pychecker."
  :type 'string
  :group 'python-mode
  :tag "Pychecker Command")

(defcustom py-pychecker-command-args '("--stdlib")
  "List of string arguments to be passed to pychecker."
  :type '(repeat string)
  :group 'python-mode
  :tag "Pychecker Command Args")

(defcustom py-pyflakes-command "pyflakes"
  "Shell command used to run Pyflakes."
  :type 'string
  :group 'python-mode
  :tag "Pyflakes Command")

(defcustom py-pyflakes-command-args '("")
  "List of string arguments to be passed to pyflakes.

Default is \"\""
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakes Command Args")

(defcustom py-pep8-command "pep8"
  "Shell command used to run pep8."
  :type 'string
  :group 'python-mode
  :tag "PEP 8 Command")

(defcustom py-pep8-command-args '("")
  "List of string arguments to be passed to pylint.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "PEP 8 Command Args")

(defcustom py-pyflakespep8-command (concat py-install-directory "/pyflakespep8.py")
  "Shell command used to run `pyflakespep8'."
  :type 'string
  :group 'python-mode
  :tag "Pyflakespep8 Command")

(defcustom py-pyflakespep8-command-args '("")
  "List of string arguments to be passed to pyflakespep8.

Default is \"\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pyflakespep8 Command Args")

(defcustom py-pylint-command "pylint"
  "Shell command used to run Pylint."
  :type 'string
  :group 'python-mode
  :tag "Pylint Command")

(defcustom py-pylint-command-args '("--errors-only")
  "List of string arguments to be passed to pylint.

Default is \"--errors-only\" "
  :type '(repeat string)
  :group 'python-mode
  :tag "Pylint Command Args")

(defcustom py-shell-input-prompt-1-regexp ">>> "
  "A regular expression to match the input prompt of the shell."
  :type 'regexp
  :group 'python-mode)

(defcustom py-shell-input-prompt-2-regexp "[.][.][.] "
  "A regular expression to match the input prompt of the shell after the
first line of input."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-prompt-read-only t
  "If non-nil, the python prompt is read only.  Setting this
variable will only effect new shells."
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
  "When non-nil switch to the Python output buffer.

If `py-keep-windows-configuration' is t, this will take precedence over setting here. "

  :type 'boolean
  :group 'python-mode)

(defcustom py-split-windows-on-execute-p 'just-two
  "When non-nil split windows.

Default is just-two - when code is send to interpreter, split screen into source-code buffer and current py-shell result.

Other buffer will be hidden that way.

When set to `t', python-mode tries to reuse existing windows and will split only if needed. However, this feature is experimental still. Same with 'always.

For the moment: If a multitude of python-shells/buffers should be
visible, open them manually and set `py-keep-windows-configuration' to `t'.

"
      :type '(choice
          (const :tag "smart" t)
          (const :tag "no split" nil)
	  (const :tag "just-two" just-two)
          (const :tag "always" always))

  :group 'python-mode)

(defcustom py-split-windows-on-execute-function 'split-window-vertically
  "How window should get splitted to display results of py-execute-... functions. "
  :type '(choice (const :tag "split-window-vertically" split-window-vertically)
                 (const :tag "split-window-horizontally" split-window-horizontally)
                 )
  :group 'python-mode)

(defcustom py-hide-show-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads. "
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-hide-show-hide-docstrings t
  "Controls if doc strings can be hidden by hide-show"
  :type 'boolean
  :group 'python-mode)

(defcustom py-hide-comments-when-hiding-all t
  "Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with")
  "Keywords composing visible heads. "
  :type '(repeat string)
  :group 'python-mode)

(defcustom python-mode-hook nil
  "Hook run when entering Python mode."

  :group 'python-mode
  :type 'hook)

;;; Default shells
(defcustom py-shell-name
  (if (eq system-type 'windows-nt)
      "C:/Python27/python"
    ;; "python"
    "python")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if no shell is specified by command.

On Windows default is C:/Python27/python
--there is no garantee it exists, please check your system--

Else python"
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-name)
(defvar py-default-interpreter py-shell-name)

(defcustom py-python-command
  (if (eq system-type 'windows-nt)
      ;; "C:\\Python27\\python.exe"
      "python"
   ;; "C:/Python33/Lib/site-packages/IPython"
    "python")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in \"Advanced System Settings/Environment Variables\" Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python-command)

(defcustom py-python-command-args '("-i")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python-command-args)

(defcustom py-python2-command
  (if (eq system-type 'windows-nt)
      "C:\\Python27\\python"
    ;; "python2"
    "python2")

  "Make sure, the directory where python.exe resides in in the PATH-variable.

Windows: If needed, edit in \"Advanced System Settings/Environment Variables\" Commonly \"C:\\\\Python27\\\\python.exe\"
With Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\python.exe\"

Else /usr/bin/python"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python2-command)

(defcustom py-python2-command-args '("-i")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)

(defcustom py-python3-command
  (if (eq system-type 'windows-nt)
      ;; "python3"
    "C:/Python33/python"
    ;; "/usr/bin/python3"
    "python3")

  "A PATH/TO/EXECUTABLE or default value `py-shell' may look for, if
  no shell is specified by command.

On Windows see C:/Python3/python.exe
--there is no garantee it exists, please check your system--

At GNU systems see /usr/bin/python3"

  :type 'string
  :group 'python-mode)

(defcustom py-python3-command-args '("-i")
  "List of string arguments to be used when starting a Python3 shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-python3-command-args)

(defcustom py-ipython-command
  (if (eq system-type 'windows-nt)
      ;; "ipython"
    "C:\\Python27\\python"
    ;; "C:/Python33/Lib/site-packages/IPython"
    ;; "/usr/bin/ipython"
    "ipython")

  "A PATH/TO/EXECUTABLE or default value `M-x IPython RET' may look for, if no IPython-shell is specified by command.

On Windows default is \"C:\\\\Python27\\\\python.exe\"
While with Anaconda for example the following works here:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython.exe\"

Else /usr/bin/ipython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-ipython-command)

(defcustom py-ipython-command-args
  (if (eq system-type 'windows-nt)
      '("-i" "C:\\Python27\\Scripts\\ipython-script.py")
    '("--pylab"))
  "List of string arguments to be used when starting a Python shell.
At Windows make sure ipython-script.py is PATH. Also setting PATH/TO/SCRIPT here should work, for example;
C:\\Python27\\Scripts\\ipython-script.py
With Anaconda the following is known to work:
\"C:\\\\Users\\\\My-User-Name\\\\Anaconda\\\\Scripts\\\\ipython-script-py\"
"
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-ipython-command-args)

(defcustom py-jython-command
  (if (eq system-type 'windows-nt)
      "jython"
    "/usr/bin/jython")

  "A PATH/TO/EXECUTABLE or default value `M-x Jython RET' may look for, if no Jython-shell is specified by command.

Not known to work at windows
Default /usr/bin/jython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-jython-command)

(defcustom py-jython-command-args '("")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-jython-command-args)

(defcustom py-bpython-command
  (if (eq system-type 'windows-nt)
      ;; not known to work at windows
      ""
    "/usr/bin/bpython")

  "A PATH/TO/EXECUTABLE or default value `M-x Bpython RET' may look for, if no Bpython-shell is specified by command.

Not known to work at windows
Default /usr/bin/bpython"

  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-bpython-command)

(defcustom py-bpython-command-args '("")
  "List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python-mode)
;; (make-variable-buffer-local 'py-bpython-command-args)

(defcustom py-shell-toggle-1 py-python2-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-toggle-1)

(defcustom py-shell-toggle-2 py-python3-command
  "A PATH/TO/EXECUTABLE or default value used by `py-toggle-shell'. "
  :type 'string
  :group 'python-mode)
;; (make-variable-buffer-local 'py-shell-toggle-2)

;;;

(defcustom py--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu of functions and global variables. "
  :type 'boolean
  :group 'python-mode)

(defvar py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/\\|^__pyfile = open('''\\|^execfile(r'[.+]/tmp/")

(defcustom py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)

;; (setq py-history-filter-regexp "\\`\\s-*\\S-?\\S-?\\s-*\\'\\|'''/tmp/\\|^__pyfile = open('''\\|^execfile(r'[.+]/tmp/")

(defcustom py-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
                               `...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python-mode)

(defcustom py-kill-empty-line t
  "If t, py-indent-forward-line kills empty lines. "
  :type 'boolean
  :group 'python-mode)

(defcustom py-imenu-show-method-args-p nil
  "Controls echoing of arguments of functions & methods in the Imenu buffer.
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

(defcustom py-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 "
  :type 'boolean
  :group 'python-mode)

(defcustom py--warn-tmp-files-left-p nil
  "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 "
  :type 'boolean
  :group 'python-mode)

(defcustom py-complete-ac-sources '(ac-source-pycomplete)
  "List of auto-complete sources assigned to `ac-sources' in `py-complete-initialize'."
  :type 'hook
  :options '(ac-source-pycomplete ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)
  :group 'python-mode)

(defcustom py-remove-cwd-from-path t
  "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion)."
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

(defcustom py-setup-codes '(py-shell-completion-setup-code
                                      py-ffap-setup-code
                                      py-eldoc-setup-code
                                      )
  "List of code run by `python-shell-send-setup-codes'."
  :type '(repeat symbol)
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


(defcustom python-shell-module-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))"
  "Python code used to get completions separated by semicolons for imports.

For IPython v0.11, add the following line to
`py-shell-completion-setup-code':

from IPython.core.completerlib import module_completion

and use the following as the value of this variable:

';'.join(module_completion('''%s'''))"
  :type 'string
  :group 'python-mode)

(defcustom py--imenu-create-index-function 'py--imenu-create-index-new
  "Switch between `py--imenu-create-index-new', which also lists modules variables,  and series 5. index-machine"
  :type '(choice (const :tag "'py--imenu-create-index-new, also lists modules variables " py--imenu-create-index-new)
                 (const :tag "py--imenu-create-index, series 5. index-machine" py-imenu-create-index))
  :group 'python-mode)

(defcustom python-source-modes '(python-mode jython-mode)
  "Used to determine if a buffer contains Python source code.
If a file is loaded into a buffer that is in one of these major modes,
it is considered Python source by `py-load-file', which uses the
value to determine defaults."
  :type '(repeat function)
  :group 'python-mode)

(defcustom py-input-filter-re "\\`\\s-*\\S-?\\S-?\\s-*\\'"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters."
  :type 'regexp
  :group 'python-mode)
(defvaralias 'inferior-python-filter-regexp 'py-input-filter-re)

(defcustom strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

  :type 'string
  :group 'convenience)

(defcustom py-docstring-style 'pep-257-nn
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
  :group 'python-mode)

(defcustom py-execute-directory nil
  "When set, stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of `py-execute-buffer' and related commands. See also `py-use-current-dir-when-execute-p'"
    :type 'string
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p t
  "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-keep-shell-dir-when-execute-p nil
  "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'"
  :type 'boolean
  :group 'python-mode)

(defcustom py-fileless-buffer-use-default-directory-p t
  "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shell"
  :type 'boolean
  :group 'python-mode)

(defcustom py-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :group 'python-mode)

(defvar py-this-abbrevs-changed nil
  "Internally used by python-mode-hook")

(defvar py-ffap-p nil)
(defvar py-ffap nil)
(defvar ffap-alist nil)

(defvar py-buffer-name nil
  "Internal use. ")

(defvar py-orig-buffer-or-file nil
  "Internal use. ")

(defun py--set-ffap-form ()
  (cond ((and py-ffap-p py-ffap)
         (eval-after-load "ffap"
           '(push '(python-mode . py-module-path) ffap-alist))
         (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
         (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
                                  ffap-alist)))
        (t (setq ffap-alist (remove '(python-mode . py-ffap-module-path) ffap-alist))
           (setq ffap-alist (remove '(py-shell-mode . py-ffap-module-path)
                                    ffap-alist))
           (setq ffap-alist (remove '(python-mode . py-module-path) ffap-alist)))))

(defcustom py-ffap-p nil

  "Select python-modes way to find file at point.

Default is nil "

  :type '(choice
          (const :tag "default" nil)
          (const :tag "use py-ffap" py-ffap))
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (py--set-ffap-form)))

(defcustom py-keep-windows-configuration nil
  "Takes precedence over `py-split-windows-on-execute-p' and `py-switch-buffers-on-execute-p'.

See lp:1239498

To suppres window-changes due to error-signaling also, set `py-keep-windows-configuration' onto 'force

Default is nil "

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" 'force))
  :group 'python-mode)

(defvar py-output-buffer "*Python Output*"
    "Currently unused.

Output buffer is created dynamically according to Python version and kind of process-handling")
(make-variable-buffer-local 'py-output-buffer)

;; the python-el way
(defcustom py-ffap-string-code
  "__FFAP_get_module_path('''%s''')\n"
  "Python code used to get a string with the path of a module."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-prompt-regexp ">>> "
  "Regular Expression matching top\-level input prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode)
(defvar py-shell-prompt-regexp ">>> ")

(defcustom py-ffap-setup-code
  "def __FFAP_get_module_path(module):
    try:
        import os
        path = __import__(module).__file__
        if path[-4:] == '.pyc' and os.path.exists(path[0:-1]):
            path = path[:-1]
        return path
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
  :group 'python-mode)

(defcustom py-eldoc-setup-code
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
  "Python code to setup documentation retrieval."
  :type 'string
  :group 'python-mode)

(defcustom py-shell-prompt-output-regexp ""
  "Regular Expression matching output prompt of python shell.
It should not contain a caret (^) at the beginning."
  :type 'string
  :group 'python-mode)

;;; defvarred Variables
(defvar py-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of syntax-class `word', not of `symbol'.
Underscores in word-class makes `forward-word' etc. travel the indentifiers. Default is `t'.
See also command `toggle-py-underscore-word-syntax-p' ")

(defvar py-autofill-timer nil)
(defvar py-fill-column-orig fill-column)

(defvar python-mode-message-string "python-components-mode.el"
  "Internally used. Reports the python-mode branch in use.")

;; Skip's XE workaround
(unless (fboundp 'string-to-syntax)
  (defun string-to-syntax (s)
    (cond
     ((equal s "|") '(15))
     ((equal s "_") '(3))
     (t (error "Unhandled string: %s" s)))))

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
        (if py-underscore-word-syntax-p
            (modify-syntax-entry ?\_ "w" table)
          (modify-syntax-entry ?\_ "_" table))
        table))

;; (setq python-mode-syntax-table
;;       (let ((table (make-syntax-table)))
;;         ;; Give punctuation syntax to ASCII that normally has symbol
;;         ;; syntax or has word syntax and isn't a letter.
;;         (let ((symbol (string-to-syntax "_"))
;;               (sst (standard-syntax-table)))
;;           (dotimes (i 128)
;;             (unless (= i ?_)
;;               (if (equal symbol (aref sst i))
;;                   (modify-syntax-entry i "." table)))))
;;         (modify-syntax-entry ?$ "." table)
;;         (modify-syntax-entry ?% "." table)
;;         ;; exceptions
;;         (modify-syntax-entry ?# "<" table)
;;         (modify-syntax-entry ?\n ">" table)
;;         (modify-syntax-entry ?' "\"" table)
;;         (modify-syntax-entry ?` "$" table)
;;         (modify-syntax-entry ?\_ "w" table)
;;         table))

;; (if py-underscore-word-syntax-p
;;     (modify-syntax-entry ?\_ "w" python-mode-syntax-table)
;;   (modify-syntax-entry ?\_ "_" python-mode-syntax-table))

(defvar py-local-command nil
  "Returns locally used executable-name. ")
(make-variable-buffer-local 'py-local-command)

(defvar py-local-versioned-command nil
  "Returns locally used executable-name including its version. ")
(make-variable-buffer-local 'py-local-versioned-command)

(defvar ipython-completion-command-string nil
  "Either ipython0.10-completion-command-string or ipython0.11-completion-command-string.

ipython0.11-completion-command-string also covers version 0.12")
;; (make-variable-buffer-local 'ipython-completion-command-string)

(defvar ipython0.10-completion-command-string
  "print(';'.join(__IP.Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar ipython0.11-completion-command-string
  "print(';'.join(get_ipython().Completer.all_completions('%s'))) #PYTHON-MODE SILENT\n"
  "The string send to ipython to query for all possible completions")

(defvar py-encoding-string-re "^[ \t]*#[ \t]*-\\*-[ \t]*coding:.+-\\*-"
  "Matches encoding string of a Python file. ")

(defvar py-shebang-regexp "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)"
  "Detecting the shell in head of file. ")
(setq py-shebang-regexp   "#![ \t]?\\([^ \t\n]+\\)[ \t]*\\([biptj]+ython[^ \t\n]*\\)")

(defvar py-separator-char "/"
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

                          `py-custom-temp-directory' will take precedence when setq ")

(defvar py-pdbtrack-input-prompt nil)

(defvar py-pydbtrack-input-prompt nil)

(defvar py-exec-command nil
  "Internally used. ")
;; (make-variable-buffer-local 'py-exec-command)

(defvar py-which-bufname "Python")
;; (make-variable-buffer-local 'py-which-bufname)

(defvar py-pychecker-history nil)

(defvar py-pyflakes-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pylint-history nil)

(defvar ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:"
  "A regular expression to match the IPython input prompt. ")

(defvar ipython-de-output-prompt-regexp "^Out\\[[0-9]+\\]: "
  "A regular expression to match the output prompt of IPython.")

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar hs-hide-comments-when-hiding-all t
  "Defined in hideshow.el, silence compiler warnings here. ")

(defvar py-force-local-shell-p nil
  "Used internally, see `toggle-force-local-shell'. ")

(defvar py-shell-complete-debug nil
  "For interal use when debugging, stores completions." )

(defcustom py-debug-p nil
  "When non-nil, keep resp. store information useful for debugging.

Temporary files are not deleted. Other functions might implement
some logging etc. "
  :type 'boolean
  :group 'python-mode)

(defvar py-completion-last-window-configuration nil
  "Internal use: restore py-restore-window-configuration when completion is done resp. abandoned. ")

(defvar py-exception-buffer nil
  "Will be set internally, let-bound, remember source buffer where error might occur. ")

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string. ")

(defvar py-labelled-re "[ \\t]*:[[:print:]]+"
  "When looking at label. ")
(setq py-labelled-re "[ \\t]*:[[:graph:]]+")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-skip-chars "^ (:=#\t\r\n\f"
  "py-expression assumes chars indicated possible composing a py-expression, skip it. ")

(defvar py-expression-re "[^ =#\t\r\n\f]+"
  "py-expression assumes chars indicated possible composing a py-expression, when looking-at or -back. ")

(defvar py-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")

(defvar py-not-expression-chars " #\t\r\n\f"
  "py-expression assumes chars indicated probably will not compose a py-expression. ")
;; (setq py-not-expression-chars " #\t\r\n\f")

(defvar py-partial-expression-backward-chars "^ =,\"'()[]{}:#\t\r\n\f"
  "py-partial-expression assumes chars indicated possible composing a py-partial-expression, skip it. ")
;; (setq py-partial-expression-backward-chars "^ =,\"'()[]{}:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")

(defvar py-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of Python operators inclusive whitespaces around.

See also `py-assignment-regexp' ")

(defvar py-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `py-operator-regexp' ")

(defvar py-delimiter-regexp "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs. ")

(defvar py-line-number-offset 0
  "When an exception occurs as a result of py-execute-region, a
subsequent py-up-exception needs the line number where the region
started, in order to jump to the correct file line.  This variable is
set in py-execute-region and used in py--jump-to-exception.")

(defvar match-paren-no-use-syntax-pps nil)

(defvar py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar py-bol-forms-last-indent nil
  "For internal use. Stores indent from last py-end-of-FORM-bol command.
When this-command is py-beginning-of-FORM-bol, last-command's indent will be considered in order to jump onto right beginning position.")
;; (make-variable-buffer-local 'py-bol-forms-last-indent)

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
  "Hook called by `jython-mode'. `jython-mode' also calls
                                 `python-mode-hook'.")

(defvar py-shell-hook nil
  "Hook called by `py-shell'.")

(defvar python-font-lock-keywords nil)

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar python-default-template "if"
  "Default template to expand by `python-expand-template'.
Updated on each expansion.")

(defvar py-already-guessed-indent-offset nil
  "Internal use by py-indent-line.

When `this-command' is `eq' to `last-command', use the guess already computed. ")
(make-variable-buffer-local 'py-already-guessed-indent-offset)

(defvar py-shell-template "
\(defun NAME (&optional argprompt)
  \"Start an DOCNAME interpreter in another window.

With optional \\\\[universal-argument] user is prompted
for options to pass to the DOCNAME interpreter. \"
  (interactive \"P\")
  (let\* ((py-shell-name \"FULLNAME\"))
    (py-shell argprompt)
    (when (interactive-p) (switch-to-buffer (current-buffer))
          (goto-char (point-max)))))
")

(defvar py-fast-filter-re (concat "\\("
			       (mapconcat 'identity
					  (delq nil (list py-shell-input-prompt-1-regexp py-shell-input-prompt-2-regexp ipython-de-input-prompt-regexp ipython-de-output-prompt-regexp py-pdbtrack-input-prompt py-pydbtrack-input-prompt))
					  "\\|")
			       "\\)")
  "Internally used by `py-fast-filter'. ")

;;; Constants
(defconst py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]"
  "Matches the beginning of a class, method or compound statement. ")

(setq py-block-closing-keywords-re
  "[ \t]*\\_<return\\|raise\\|break\\|continue\\|pass\\_>[ \n\t]")

(defconst py-finally-re
  "[ \t]*\\_<finally\\_>[: \n\t]"
  "Regular expression matching keyword which closes a try-block. ")

(defconst py-except-re
  "[ \t]*\\_<except\\_>[:( \n\t]*"
  "Regular expression matching keyword which composes a try-block. ")

(defconst py-else-re
  "[ \t]*\\_<else\\_>[: \n\t]*"
  "Regular expression matching keyword which closes a for- if- or try-block. ")

(defconst py-return-re
  ".*:?[ \t]*\\_<\\(return\\)\\_>[ \n\t]*"
  "Regular expression matching keyword which typically closes a function. ")

(defconst py-no-outdent-1-re-raw
  (list
   "elif"
   "else"
   "except"
   "for"
   "if"
   "try"
   "while"
   ))

(defconst py-no-outdent-2-re-raw
  (list
   "break"
   "continue"
   "pass"
   "raise"
   "return"
   ))

(defconst py-no-outdent-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt py-no-outdent-1-re-raw)
   "\\)\\_>[( \t]+.*:[( \t]\\_<\\("
   (regexp-opt py-no-outdent-2-re-raw)
   "\\)\\_>[)\t]*$")
  "Regular expression matching lines not to augment indent after.

See py-no-outdent-1-re-raw, py-no-outdent-2-re-raw for better readable content ")

(defconst py-assignment-re "\\_<\\w+\\_>[ \t]*\\(=\\|+=\\|*=\\|%=\\|&=\\|^=\\|<<=\\|-=\\|/=\\|**=\\||=\\|>>=\\|//=\\)"
  "If looking at the beginning of an assignment. ")

(defconst py-block-re "[ \t]*\\_<\\(class\\|def\\|for\\|if\\|try\\|while\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement. ")

(defconst py-minor-block-re "[ \t]*\\_<\\(for\\|if\\|try\\|with\\)\\_>[:( \n\t]*"
  "Matches the beginning of an `for', `if', `try' or `with' block. ")

(defconst py-try-block-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a `try' block. ")

(defconst py-if-block-re "[ \t]*\\_<if\\_>[: \n\t]"
  "Matches the beginning of an `if' block. ")

(defconst py-elif-block-re "[ \t]*\\_<elif\\_>[: \n\t]"
  "Matches the beginning of an `elif' block. ")

(defconst py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
  "Matches the beginning of a class definition. ")

(defconst py-def-or-class-re "[ \t]*\\_<\\(def\\|class\\)\\_>[ \n\t]"
  "Matches the beginning of a class- or functions definition. ")

(defconst py-def-re "[ \t]*\\_<\\(def\\)\\_>[ \n\t]"
  "Matches the beginning of a functions definition. ")

(defconst py-block-or-clause-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   "for"
   "if"
   "try"
   "while"
   "with")
  "Matches the beginning of a compound statement or it's clause. ")

(defvar py-block-or-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-block-or-clause-re-raw, which it reads. ")

(defconst py-clause-re
  (concat
   "[ \t]*\\_<\\("
   (mapconcat 'identity
              (list
               "elif"
               "else"
               "except"
               "finally")
              "\\|")
   "\\)\\_>[( \t]*.*:?")
  "Regular expression matching lines not to augment indent after.")

(defconst py-extended-block-or-clause-re-raw
  (list
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
  "Matches the beginning of a compound statement or it's clause. ")

(defconst py-extended-block-or-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-block-or-clause-re-raw, which it reads. ")

(defconst py-top-level-form-re
  (concat
   "^\\_<[a-zA-Z_]\\|^\\_<\\("
   (regexp-opt  py-extended-block-or-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "A form which starts at zero indent level, but is not a comment. ")

;; (defconst py-extended-block-or-clause-re "[ \t]*\\_<\\(def\\|class\\|if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>[:( \n\t]*"
;;   "Matches the beginning of a compound statement or it's clause.
;; Includes def and class. ")

(defconst py-block-keywords
  (concat
   "\\_<\\("
   (regexp-opt py-block-or-clause-re-raw)
   "\\)\\_>")
  "Matches known keywords opening a block. ")

;; (defconst py-block-keywords "\\_<\\(def\\|class\\|if\\|else\\|elif\\|while\\|for\\|try\\|except\\|finally\\|with\\)\\_>"
;;   "Matches known keywords opening a block. ")

(defconst py-clause-re-raw
  (list
   "elif"
   "else"
   "except"
   "finally"
   )
  "Matches the beginning of a clause. ")

(defconst py-clause-re
  (concat
   "[ \t]*\\_<\\("
   (regexp-opt  py-clause-re-raw)
   "\\)\\_>[( \t]*.*:?")
  "See py-clause-re-raw, which it reads. ")

;; (defconst py-clause-re
;;   (concat
;;    "[ \t]*\\_<\\("
;;    (mapconcat 'identity
;;               (list
;;                "elif"
;;                "else"
;;                "except"
;;                "finally")
;;               "\\|")
;;    "\\)\\_>[( \t]*.*:?")
;;   "Regular expression matching lines not to augment indent after.")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[:( \n\t]*"
  "Matches the beginning of a compound if-statement's clause exclusively. ")

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
  "Matches the beginning of a compound try-statement's clause. ")

(defconst py-if-re "[ \t]*\\_<if\\_>[( \n\t]*"
  "Matches the beginning of a compound statement saying `if'. ")

(defconst py-try-re "[ \t]*\\_<try\\_>[:( \n\t]*"
  "Matches the beginning of a compound statement saying `try'. " )

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
hooked into `compilation-error-regexp-alist'  "
  :type '(alist string)
  :group 'python-mode)

(defun py--quote-syntax (n)
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

;; IPython Completion start

;; see also
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2008-01/msg00076.html

;; https://github.com/ipython
;; commit 1dd379d857f836c9e8af4576cecaeb413fcba4e5
;; Date:   Tue Feb 14 19:47:04 2012 -0800
;; "print(';'.join(get_ipython().complete('%s', '%s')[1])) #PYTHON-MODE SILENT\n"

(defconst py-windows-config-register 313465889
  "Internal used")

;; (windows-config (window-configuration-to-register 313465889))
(defvar py-windows-config nil
  "Completion stores py-windows-config-register here")

(setq symbol-definition-start-re "^[ \t]*(\\(defun\\|defvar\\|defcustom\\)")

(put 'py-indent-offset 'safe-local-variable 'integerp)

;; ipython.el
;; Recognize the ipython pdb, whose prompt is 'ipdb>' or  'ipydb>'
;;instead of '(Pdb)'
(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ ")
(setq py-pydbtrack-input-prompt "^[(]*ipydb[>)]+ ")

;; pydb-328837.diff

;; prevent ipython.el's setting
(setq ipython-de-input-prompt-regexp "In \\[[0-9]+\\]:\\|^[ ]\\{3\\}[.]\\{3,\\}:" )

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;;; Constants

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
  :group 'python-mode)

(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also `py-object-reference-face'"
  :group 'python-mode)

(defface py-object-reference-face
  '((t (:inherit py-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method., commonly \"cls\" and \"self\""
  :group 'python-mode)

(defface py-variable-name-face
  '((t (:inherit default)))
  "Face method decorators."
  :group 'python-mode)

(defface py-number-face
 '((t (:inherit default)))
;;  '((t (:inherit 'font-lock-variable-name-face)))
  "Highlight numbers. "
  :group 'python-mode)

(defface py-try-if-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
  :group 'python-mode)

(defface py-import-from-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
  :group 'python-mode)

(defface py-def-class-face
  '((t (:inherit font-lock-keyword-face)))
  "Highlight keywords. "
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

(defun py--delete-all-but-first-prompt ()
  "Don't let prompts from setup-codes sent clutter buffer. "
  (let (last erg)
    (when (re-search-backward py-fast-filter-re nil t 1)
      (setq erg (match-end 0))
      (while (and (re-search-backward py-fast-filter-re nil t 1) (setq erg (match-end 0))))
      (delete-region erg (point-max))))
  (goto-char (point-max)))

(defun py--shell-send-setup-code (process)
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`py-setup-codes' list."
  (let ((erg (string-match "^i" (process-name process))))
    (dolist (code py-setup-codes)
      ;; (message "%s" code)
      ;; `py--fast-send-string' doesn't work with IPython for now
      ;; wants magic %paste %cpaste
      (if erg
	  (progn
	    (py--send-string-no-output
	     (py--fix-start (symbol-value code)) process)
	    (sit-for py-new-shell-delay)
	    (py--delete-all-but-first-prompt))
	(py--fast-send-string-no-output (py--fix-start (symbol-value code)) process (buffer-name (process-buffer process)))))))

(defun py--docstring-p (&optional beginning-of-string-position)
  "Check to see if there is a docstring at POS."
  (let* (pps
	 (pos (or beginning-of-string-position
		  (and (nth 3 (setq pps (syntax-ppss))) (nth 8 pps)))))
    (save-restriction
      (widen)
      (save-excursion
	(py-beginning-of-statement)
        (and (looking-at "'''\\|\"\"\"")
	     (point))))))

(defun py--font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (py--docstring-p (nth 8 state))
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook nil))

(defun py-choose-shell-by-shebang (&optional shebang)
  "Choose shell by looking at #! on the first line.

If SHEBANG is non-nil, returns the shebang as string,
otherwise the Python resp. Jython shell command name. "
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
    (when (and py-verbose-p (interactive-p)) (message "%s" res))
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

(defun py-choose-shell-by-path (&optional py-separator-char)
  "Select Python executable according to version desplayed in path, current buffer-file is selected from.

Returns versioned string, nil if nothing appropriate found "
  (interactive)
  (lexical-let ((path (buffer-file-name))
                (py-separator-char (or py-separator-char py-separator-char))
                erg)
    (when (and path py-separator-char
               (string-match (concat py-separator-char "[iI]?[pP]ython[0-9.]+" py-separator-char) path))
      (setq erg (substring path
                           (1+ (string-match (concat py-separator-char "[iI]?[pP]ython[0-9.]+" py-separator-char) path)) (1- (match-end 0)))))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun py-which-python ()
  "Returns version of Python of current environment, a number. "
  (interactive)
  (let* (treffer (cmd (py-choose-shell))
         version erg)
    (setq treffer (string-match "\\([23]*\\.?[0-9\\.]*\\)$" cmd))
    (if treffer
        ;; if a number if part of python name, assume it's the version
        (setq version (substring-no-properties cmd treffer))
      (setq erg (shell-command-to-string (concat cmd " --version")))
      ;; Result: "bpython version 0.9.7.1 on top of Python 2.7\n(C) 2008-2010 Bob Farrell, Andreas Stuehrk et al. See AUTHORS for detail.\n"

      (setq version (cond ((string-match (concat "\\(on top of Python \\)" "\\([0-9]\\.[0-9]+\\)") erg)
                           (match-string-no-properties 2 erg))
                          ((string-match "\\([0-9]\\.[0-9]+\\)" erg)
                           (substring erg 7 (1- (length erg)))))))
    (when (interactive-p)
      (if version
          (when py-verbose-p (message "%s" version))
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

;; requested by org-mode still
(defalias 'py-toggle-shells 'py-choose-shell)

(defalias 'py-which-shell 'py-choose-shell)
(defun py-choose-shell (&optional arg pyshell py-dedicated-process-p py-edit-only-p)
  "Return an appropriate executable as a string.

Returns nil, if no executable found.

This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py--choose-shell-by-import'
 - look if Path/To/File indicates a Python version
 - if not successful, return default value of `py-shell-name'

When interactivly called, messages the shell name, Emacs would in the given circtumstances.

With \\[universal-argument] 4 is called `py-switch-shell' see docu there."
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
      (py-switch-shell '(4))
    (let* (res
	   (erg (cond (py-force-py-shell-name-p
                       (default-value 'py-shell-name))
                      (py-use-local-default
                       (if (not (string= "" py-shell-local-path))
                           (expand-file-name py-shell-local-path)
                         (message "Abort: `py-use-local-default' is set to `t' but `py-shell-local-path' is empty. Maybe call `py-toggle-local-default-use'")))
                      ((and py-fast-process-p
			    (comint-check-proc (current-buffer))
                            (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
		       (progn
			 (setq res (process-name (get-buffer-process (current-buffer))))
			 (if (string-match "<" res)
			     ;; executable-find can't see a python<1>
			     (substring res 0 (match-beginning 0))
			   res)))
		      ((and (not py-fast-process-p)
			    (comint-check-proc (current-buffer))
                            (string-match "ython" (process-name (get-buffer-process (current-buffer)))))
                       (process-name (get-buffer-process (current-buffer))))
                      ((py-choose-shell-by-shebang))
                      ((py--choose-shell-by-import))
                      ((py-choose-shell-by-path))
                      (t (or
                          (default-value 'py-shell-name)
                          "python"))))
           (cmd (if py-edit-only-p erg
                  (executable-find erg))))
      (if cmd
          (when (interactive-p)
            (message "%s" cmd))
        (when (interactive-p) (message "%s" "Could not detect Python on your system. Maybe set `py-edit-only-p'?")))
      erg)))


(defun py--normalize-directory (directory)
  "Make sure DIRECTORY ends with a file-path separator char.

Returns DIRECTORY"
  (let ((erg (cond ((string-match (concat py-separator-char "$") directory)
                    directory)
                   ((not (string= "" directory))
                    (concat directory py-separator-char)))))
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
  (let ((erg (cond ((locate-library "python-mode")
                    (file-name-directory (locate-library "python-mode")))
                   ((and (buffer-file-name)(string-match "python-mode" (buffer-file-name)))
                    (file-name-directory (buffer-file-name)))
                   ((string-match "python-mode" (buffer-name))
                    default-directory))))
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

(when (or py-load-pymacs-p (featurep 'pymacs))
  (defun py-load-pycomplete ()
    "Load Pymacs based pycomplete."
    (interactive)
    (let* ((path (getenv "PYTHONPATH"))
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
            (add-to-list 'load-path pycomplete-directory)
            (require 'pycomplete)
            (add-hook 'python-mode-hook 'py-complete-initialize))
        (error "`py-install-directory' not set, see INSTALL")))))

(and (or (eq py-complete-function 'py-complete-completion-at-point) py-load-pymacs-p (featurep 'pymacs))
  (py-load-pycomplete))

(defun py-set-load-path ()
  "Include needed subdirs of python-mode directory. "
  (interactive)
  (let ((py-install-directory (py--normalize-directory py-install-directory)))
    (cond ((and (not (string= "" py-install-directory))(stringp py-install-directory))
           (add-to-list 'load-path (expand-file-name py-install-directory))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "completion"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "extensions"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "test"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "tools"))
           (add-to-list 'load-path (concat (expand-file-name py-install-directory) "autopair")))
          (py-guess-py-install-directory-p
	   (let ((guessed-py-install-directory (py-guess-py-install-directory)))
	     (when guessed-py-install-directory
	       (add-to-list 'load-path guessed-py-install-directory))))
          (t (error "Please set `py-install-directory', see INSTALL"))
          (when (interactive-p) (message "%s" load-path)))))

;; (when (boundp 'py-install-directory) (py-set-load-path))
;; (py-set-load-path)

(unless py-install-directory
  (add-to-list 'load-path default-directory)
  (add-to-list 'load-path (concat default-directory "extensions")))

(require 'python-components-edit)
(require 'python-components-intern)
(require 'python-components-beginning-forms)
(require 'python-components-move)
(require 'python-components-execute)
(require 'python-components-send)
(require 'python-components-shell-complete)
(require 'python-components-pdb)
(require 'python-components-help)
(require 'python-components-extensions)
;; (require 'thingatpt-python-expressions)
(require 'python-components-imenu)
;; (require 'python-components-completion)
(require 'python-components-named-shells)
(require 'python-components-electric)
(require 'python-components-virtualenv)
;;(require 'components-shell-completion)
(require 'python-components-re-forms)
(require 'python-components-up-down)
(require 'python-components-bol-forms)
(require 'python-components-exec-forms)
(require 'python-components-delete)
(require 'python-components-copy)
(require 'python-extended-executes)
;; (require 'python-mode-test)
;; (require 'column-marker)
(require 'python-abbrev-propose)
(require 'python-components-switches)
(require 'python-components-paragraph)
(require 'python-components-shift-forms)
(require 'python-components-execute-file)
(require 'python-components-comment)
(require 'python-components-forms)
(require 'python-components-forms-code)
(require 'python-components-fast-forms)
(require 'python-components-auto-fill)
(require 'python-components-hide-show)
(require 'python-components-fast-complete)
(require 'highlight-indentation)

(and py-load-skeletons-p (require 'python-components-skeletons))
(and py-company-pycomplete-p (require 'company-pycomplete))

;; toggle-py-underscore-word-syntax-p must be known already
;; circular: toggle-py-underscore-word-syntax-p sets and calls it
(defcustom py-underscore-word-syntax-p t
  "If underscore chars should be of syntax-class `word', not of `symbol'.

Underscores in word-class makes `forward-word' etc. travel the indentifiers. Default is `t'.

See bug report at launchpad, lp:940812 "
  :type 'boolean
  :group 'python-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (toggle-py-underscore-word-syntax-p (if value 1 0))))

;;; Python specialized rx, thanks Fabian F. Gallina
;; (eval-when-compile
;;   (defconst python-rx-constituents
;;     `((block-start          . ,(rx symbol-start
;;                                    (or "def" "class" "if" "elif" "else" "try"
;;                                        "except" "finally" "for" "while" "with")
;;                                    symbol-end))
;;       (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
;;                                    (* (any word ?_))))
;;       (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
;;       (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
;;                                    (+ space) "==" (+ space)
;;                                    (any ?' ?\") "__main__" (any ?' ?\")
;;                                    (* space) ?:))
;;       (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
;;       (open-paren           . ,(rx (or "{" "[" "(")))
;;       (close-paren          . ,(rx (or "}" "]" ")")))
;;       (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
;;       ;; FIXME: rx should support (not simple-operator).
;;       (not-simple-operator  . ,(rx
;;                                 (not
;;                                  (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
;;       ;; FIXME: Use regexp-opt.
;;       (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
;;                                        "=" "%" "**" "//" "<<" ">>" "<=" "!="
;;                                        "==" ">=" "is" "not")))
;;       ;; FIXME: Use regexp-opt.
;;       (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
;;                                        ">>=" "<<=" "&=" "^=" "|="))))
;;     "Additional Python specific sexps for `python-rx'"))

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
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''"))))))
    "Additional Python specific sexps for `python-rx'")

  (defmacro python-rx (&rest regexps)
    "Python mode specialized rx macro.
This variant of `rx' supports common python named REGEXPS."
    (let ((rx-constituents (append python-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

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
             (or
	      "if" "and" "del"  "not" "while" "as" "elif" "global"
	      "or" "with" "assert" "else"  "pass" "yield" "break"
	      "exec" "in" "continue" "finally" "is" "except" "raise"
	      "return"  "for" "lambda")
             symbol-end)
        (,(rx symbol-start (or "def" "class") symbol-end) . py-def-class-face)
        (,(rx symbol-start (or "import" "from") symbol-end) . py-import-from-face)
        (,(rx symbol-start (or "try" "if") symbol-end) . py-try-if-face)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
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
                  "UserWarning" "ValueError" "Warning" "ZeroDivisionError")
              word-end) . py-exception-name-face)
        ;; (,(rx (or space line-start) symbol-start "range
        ;; Builtins
        (,(rx
	   (or space line-start (not (any ".")))
	   symbol-start
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
        ("\\([._[:word:]]+\\)\\(?:\\[[^]]+]\\)?[[:space:]]*\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
         (1 py-variable-name-face nil nil))
        ;; a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                                 (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                 ?, (* space) (+ (any word ?. ?_)) (* space)
                                 assignment-operator))
                  (res nil))
              (while (and (setq res (re-search-forward re limit t))
                          (goto-char (match-end 1))
                          (nth 1 (syntax-ppss))
                          ;; (python-syntax-context 'paren)
                          ))
              res))
         (1 py-variable-name-face nil nil))
        ;; Numbers
;;        (,(rx symbol-start (or (1+ digit) (1+ hex-digit)) symbol-end) . py-number-face)
	(,(rx symbol-start (1+ digit) symbol-end) . py-number-face)
	))

;; (defconst py-font-lock-syntactic-keywords
;;   ;; Make outer chars of matching triple-quote sequences into generic
;;   ;; string delimiters.  Fixme: Is there a better way?
;;   ;; First avoid a sequence preceded by an odd number of backslashes.
;;   `((,(concat "\\(?:\\([RUru]\\)[Rr]?\\|^\\|[^\\]\\(?:\\\\.\\)*\\)" ;Prefix.
;;               "\\(?:\\('\\)'\\('\\)\\|\\(?2:\"\\)\"\\(?3:\"\\)\\)")
;;      (3 (py--quote-syntax)))))


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
        ;; (define-key map [(control y)] 'py-electric-yank)
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
        (define-key map (kbd "RET") py-return-key)
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
        (define-key map [(control c)(control e)] 'py-help-at-point)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
        ;; information
        (define-key map [(control c)(control b)] 'py-submit-bug-report)
        (define-key map [(control c)(control v)] 'py-version)
        (define-key map [(control c)(control w)] 'py-pychecker-run)
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

        (and (ignore-errors (require 'easymenu) t)
             ;; (easy-menu-define py-menu map "Python Tools"
             ;;           `("PyTools"
             (easy-menu-define
               py-menu map "Python Mode menu"
               `("Python"
                 ("Interpreter"
                  ["Default interpreter..." py-shell
                   :help " `py-shell'

Start an interactive Python interpreter.

Interactively, C-u 4 prompts for a buffer.
C-u 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

. "]
                  ("Other"
                   :help "Alternative Python Shells"

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

                   ["python3.3" python3.3
                    :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

                   ["python3.4" python3.4
                    :help "`python3.3'
Start an Python3.4 interpreter.

Optional C-u prompts for options to pass to the Python3.4 interpreter. See `py-python-command-args'."]

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

                   "-"
                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter.

Uses a dedicated shell. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter.

Uses a dedicated shell. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2.7 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter.

Uses a dedicated shell. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3.3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter.

Uses a dedicated shell. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]))
                  "-"

		  ["Toggle shell" py-toggle-shell
		   :help " `py-toggle-shell'

Toggles between the interpreter customized in `py-shell-toggle-1' resp\. `py-shell-toggle-2'\. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default\.

ARG might be a python-version string to set to\.

C-u `py-toggle-shell' prompts to specify a reachable Python command\.
C-u followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell\.
C-u followed by numerical arg 5 opens a Jython shell\.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     "MY-PATH-TO-SHELL")"]

                  ["Kill shell unconditional" py-kill-shell-unconditional
                   :help " `py-kill-shell-unconditional'

With optional argument SHELL\.

Otherwise kill default (I)Python shell\.
Kill buffer and its process.
Receives a buffer-name as argument "]

                  ["Kill default shell unconditional" py-kill-default-shell-unconditional
                   :help " `py-kill-default-shell-unconditional'

Kill buffer \"*Python*\" and its process\.  "])

                 "-"
                 ("Mark"

                  ["Mark block" py-mark-block
                   :help " `py-mark-block'

Mark block at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark minor block" py-mark-minor-block
                   :help " `py-mark-minor-block'

Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Returns beginning and end positions of marked area, a cons. "]

                  ["Mark def or class" py-mark-def-or-class
                   :help " `py-mark-def-or-class'

Mark def-or-class at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark statement" py-mark-statement
                   :help "`py-mark-statement'
Mark statement at point"]

                  ["Mark top level" py-mark-top-level
                   :help " `py-mark-top-level'

Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark clause" py-mark-clause
                   :help "`py-mark-clause'
Mark innermost compound statement at point"]

                  ["Mark def" py-mark-def
                   :help "`py-mark-def'
Mark innermost definition at point"]

                  ["Mark expression" py-mark-expression
                   :help "`py-mark-expression'
Mark expression at point"]

                  ["Mark partial expression" py-mark-partial-expression
                   :help "`py-mark-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]

                  ["Mark class" py-mark-class
                   :help "`py-mark-class'
Mark innermost definition at point"]

                  ["Mark comment" py-mark-comment
                   :help "`py-mark-comment'
Mark commented section at point"]

                  ("BOL forms"

		   ["Mark block bol" py-mark-block-bol
		    :help "`py-mark-block-bol'
Mark block at point reaching beginning-of-line. "]

		   ["Mark clause bol" py-mark-clause-bol
		    :help "`py-mark-clause-bol'
Mark clause at point reaching beginning-of-line. "]

		   ["Mark block-or-clause bol" py-mark-block-or-clause-bol
		    :help "`py-mark-block-or-clause-bol'
Mark block-or-clause at point reaching beginning-of-line. "]

		   ["Mark def bol" py-mark-def-bol
		    :help "`py-mark-def-bol'
Mark def at point reaching beginning-of-line. "]

		   ["Mark class bol" py-mark-class-bol
		    :help "`py-mark-class-bol'
Mark class at point reaching beginning-of-line. "]

		   ["Mark def-or-class bol" py-mark-def-or-class-bol
		    :help "`py-mark-def-or-class-bol'
Mark def-or-class at point reaching beginning-of-line. "]

		   ["Mark if-block bol" py-mark-if-block-bol
		    :help "`py-mark-if-block-bol'
Mark if-block at point reaching beginning-of-line. "]

		   ["Mark try-block bol" py-mark-try-block-bol
		    :help "`py-mark-try-block-bol'
Mark try-block at point reaching beginning-of-line. "]

		   ["Mark minor-block bol" py-mark-minor-block-bol
		    :help "`py-mark-minor-block-bol'

Mark minor-block at point reaching beginning-of-line.
A minor block is started by a `for', `if', `try' or `with'."]))

		 "-"

                 ["Shift region left" py-shift-region-left
                  :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                 ["Shift region right" py-shift-region-right
                  :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "]

                 "-"

                 ("Comment"
                  ["Comment Region"   py-comment-region (point) (mark)
                   :help "Like `comment-region' but uses double hash (`#') comment starter." ]
                  ["Uncomment" py-uncomment
                   :help " `py-uncomment'

Uncomment commented lines at point.

If region is active, restrict uncommenting at region . "]

                  ["Uncomment Region"     (py-comment-region (point) (mark) '(4))
                   :help "(py-comment-region (point) (mark) '(4))" ]
                  "-"
                  ["Comment block" py-comment-block
                   :help " `py-comment-block'
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment minor-block" py-comment-minor-block
                   :help " `py-comment-minor-block'
Comments minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment top level" py-comment-top-level
                   :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment clause" py-comment-clause
                   :help " `py-comment-clause'
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment block or clause" py-comment-block-or-clause
                   :help " `py-comment-block-or-clause'
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def" py-comment-def
                   :help " `py-comment-def'
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment class" py-comment-class
                   :help " `py-comment-class'
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def or class" py-comment-def-or-class
                   :help " `py-comment-def-or-class'
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment statement" py-comment-statement
                   :help " `py-comment-statement'
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                 "-"

                 ("Move"

		  ["Backward same level" py-backward-same-level
		   :help " `py-backward-same-level'

Go form backward keeping indent level if possible\.

If inside a delimited form --string or list-- go to its beginning\.
If not at beginning of a statement or block, go to its beginning\.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point\.
If no further element at same level, go one level up. "]

                  ["Beginning of block" py-beginning-of-block
                   :help " `py-beginning-of-block'

Go to beginning block, skip whitespace at BOL. "]

                  ["Go to end of block" py-end-of-block]

                  "-"

                  ["Beginning of def or class" py-beginning-of-def-or-class
                   :help " `py-beginning-of-def-or-class'

Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "]

                  ["End of def or class" py-end-of-def-or-class
                   :help " `py-end-of-def-or-class'

Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too. "]

                  "-"

                  ["Beginning of statement" py-beginning-of-statement
                   :help " `py-beginning-of-statement'

Go to the initial line of a simple statement. "]

                  ["End of statement" py-end-of-statement
                   :help " `py-end-of-statement'

Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "]

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

                  ("BOL forms"
                   ("Beginning"

                    ["Beginning of block bol" py-beginning-of-block-bol
                     :help " `py-beginning-of-block-bol'

Go to beginning block, go to beginning-of-line\.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of clause bol" py-beginning-of-clause-bol
                     :help " `py-beginning-of-clause-bol'

Go to beginning clause, go to beginning-of-line\.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
                     :help " `py-beginning-of-block-or-clause-bol'

Go to beginning block-or-clause, go to beginning-of-line\.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def bol" py-beginning-of-def-bol
                     :help " `py-beginning-of-def-bol'

Go to beginning def, go to beginning-of-line\.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of class bol" py-beginning-of-class-bol
                     :help " `py-beginning-of-class-bol'

Go to beginning class, go to beginning-of-line\.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def or class bol" py-beginning-of-def-or-class-bol
                     :help " `py-beginning-of-def-or-class-bol'

Go to beginning def-or-class, go to beginning-of-line\.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of if block bol" py-beginning-of-if-block-bol
                     :help " `py-beginning-of-if-block-bol'

Go to beginning if-block, go to beginning-of-line\.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of try block bol" py-beginning-of-try-block-bol
                     :help " `py-beginning-of-try-block-bol'

Go to beginning try-block, go to beginning-of-line\.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of minor block bol" py-beginning-of-minor-block-bol
                     :help " `py-beginning-of-minor-block-bol'

Go to beginning minor-block, go to beginning-of-line\.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of statement bol" py-beginning-of-statement-bol
                     :help " `py-beginning-of-statement-bol'

Goto beginning of line where statement starts\.
  Returns position reached, if successful, nil otherwise\.

See also `py-up-statement': up from current definition to next beginning of statement above\.  "])
                   ("End"

                    ["End of block bol" py-end-of-block-bol
                     :help " `py-end-of-block-bol'

Goto beginning of line following end of block\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block': down from current definition to next beginning of block below\.  "]

                    ["End of clause bol" py-end-of-clause-bol
                     :help " `py-end-of-clause-bol'

Goto beginning of line following end of clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-clause': down from current definition to next beginning of clause below\.  "]

                    ["End of block or clause bol" py-end-of-block-or-clause-bol
                     :help " `py-end-of-block-or-clause-bol'

Goto beginning of line following end of block-or-clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below\.  "]

                    ["End of def bol" py-end-of-def-bol
                     :help " `py-end-of-def-bol'

Goto beginning of line following end of def\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def': down from current definition to next beginning of def below\.  "]

                    ["End of class bol" py-end-of-class-bol
                     :help " `py-end-of-class-bol'

Goto beginning of line following end of class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-class': down from current definition to next beginning of class below\.  "]

                    ["End of def or class bol" py-end-of-def-or-class-bol
                     :help " `py-end-of-def-or-class-bol'

Goto beginning of line following end of def-or-class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below\.  "]

                    ["End of if block bol" py-end-of-if-block-bol
                     :help " `py-end-of-if-block-bol'

 "]

                    ["End of try block bol" py-end-of-try-block-bol
                     :help " `py-end-of-try-block-bol'

 "]

                    ["End of minor block bol" py-end-of-minor-block-bol
                     :help " `py-end-of-minor-block-bol'

 "]

                    ["End of statement bol" py-end-of-statement-bol
                     :help " `py-end-of-statement-bol'

Goto beginning of line following end of statement\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-statement': down from current definition to next beginning of statement below\.  "]))

                  "-"

                  ("More"

                   ["Up level" py-up
                    :help " `py-up'
Go to beginning one level above of compound statement or definition at point. "]

                   ["Down level" py-down
                    :help " `py-down'
Go to beginning one level below of compound statement or definition at point. "]

                   "-"

                   ["Beginning of top level" py-beginning-of-top-level
                    :help " `py-beginning-of-top-level'

Go to the very beginning of top-level form at point. "]

                   ["End of top level" py-end-of-top-level
                    :help " `py-end-of-top-level'

Go to end of top-level form at point. "]

                   "-"

                   ["Beginning of block current-column" py-beginning-of-block-current-column
                    :help " `py-beginning-of-block-current-column'

Reach next beginning of block upwards which starts at current column.

Return position. "]

                   "-"

                   ["Move to start of def" py-beginning-of-def t]

                   ["Move to end of def"   py-end-of-def t]

                   "-"

                   ["Beginning of clause" py-beginning-of-clause
                    :help " `py-beginning-of-clause'

Go to beginning clause, skip whitespace at BOL. "]

                   ["End of clause" py-end-of-clause
                    :help " `py-end-of-clause'

Go to end of clause. "]

                   "-"

                   ["Beginning of comment" py-beginning-of-comment
                    :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                   ["End of comment" py-end-of-comment
                    :help " `py-end-of-comment'

Go to end of comment at point. "]

                   "-"

                   ["Go to start of expression" (py-beginning-of-expression t) t]
                   ["Move to end of expression" (py-end-of-expression t) t]

                   "-"

                   ["Go to start of minor-expression" (py-beginning-of-minor-expression t) t]

                   ["Move to end of minor-expression" (py-end-of-minor-expression t) t]
                   "-"

                   ["Beginning of minor block" py-beginning-of-minor-block
                    :help " `py-beginning-of-minor-block'

Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'.

"]

                   ["End of minor block" py-end-of-minor-block
                    :help " `py-end-of-minor-block'

Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'. "]))

                 "-"

                 ("Copy "
                  ["Copy statement" py-copy-statement
                   :help "`py-copy-statement'
Copy statement at point"]

                  ["Copy top level" py-copy-top-level
                   :help " `py-copy-top-level'

Copy top-level form at point. "]

                  ["Copy clause" py-copy-clause
                   :help "`py-copy-clause'
Copy innermost clause at point"]

                  ["Copy block" py-copy-block
                   :help "`py-copy-block'
Copy innermost block at point"]

                  ["Copy minor block" py-copy-minor-block
                   :help " `py-copy-minor-block'

Copy minor-block at point.

Store data in kill ring, so it might yanked back.
A minor block is started by a `for', `if', `try' or `with'. "]

                  ["Copy def" py-copy-def
                   :help "`py-copy-def'
Copy innermost definition at point"]
                  ["Copy expression" py-copy-expression
                   :help "`py-copy-expression'
Copy expression at point"]
                  ["Copy partial expression" py-copy-partial-expression
                   :help "`py-copy-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]
                  ["Copy class" py-copy-class
                   :help "`py-copy-class'
Copy innermost definition at point"]

                  ["Copy Def-or-Class" py-copy-def-or-class
                   :help "`py-copy-def-or-class'
Copy innermost definition at point"]

                  ("BOL forms"

                   ["Copy block bol" py-copy-block-bol
                    :help " `py-copy-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy clause bol" py-copy-clause-bol
                    :help " `py-copy-clause-bol'

Delete clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy block or clause bol" py-copy-block-or-clause-bol
                    :help " `py-copy-block-or-clause-bol'

Delete block-or-clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def bol" py-copy-def-bol
                    :help " `py-copy-def-bol'

Delete def, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy class bol" py-copy-class-bol
                    :help " `py-copy-class-bol'

Delete class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def or class bol" py-copy-def-or-class-bol
                    :help " `py-copy-def-or-class-bol'

Delete def-or-class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy statement bol" py-copy-statement-bol
                    :help " `py-copy-statement-bol'

Delete statement, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy minor block bol" py-copy-minor-block-bol
                    :help " `py-copy-minor-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.

See `py-minor-block-re' "]))

		 ("Hide-Show"

		  ["Hide region" py-hide-region
		   :help " `py-hide-region'

Hide active region\. "]

		  ["Hide statement" py-hide-statement
		   :help " `py-hide-statement'

Hide statement at point\. "]

		  ["Hide block" py-hide-block
		   :help " `py-hide-block'

Hide block at point\. "]

		  ["Hide clause" py-hide-clause
		   :help " `py-hide-clause'

Hide clause at point\. "]

		  ["Hide block or clause" py-hide-block-or-clause
		   :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		  ["Hide def" py-hide-def
		   :help " `py-hide-def'

Hide def at point\. "]

		  ["Hide class" py-hide-class
		   :help " `py-hide-class'

Hide class at point\. "]

		  ["Hide expression" py-hide-expression
		   :help " `py-hide-expression'

Hide expression at point\. "]

		  ["Hide partial expression" py-hide-partial-expression
		   :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		  ["Hide line" py-hide-line
		   :help " `py-hide-line'

Hide line at point\. "]

		  ["Hide top level" py-hide-top-level
		   :help " `py-hide-top-level'

Hide top-level at point\. "]

		  ("Show"

		   ["Show region" py-show-region
		    :help " `py-show-region'

Un-hide active region\. "]

		   ["Show statement" py-show-statement
		    :help " `py-show-statement'

Show statement at point\. "]

		   ["Show block" py-show-block
		    :help " `py-show-block'

Show block at point\. "]

		   ["Show clause" py-show-clause
		    :help " `py-show-clause'

Show clause at point\. "]

		   ["Show block or clause" py-show-block-or-clause
		    :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		   ["Show def" py-show-def
		    :help " `py-show-def'

Show def at point\. "]

		   ["Show class" py-show-class
		    :help " `py-show-class'

Show class at point\. "]

		   ["Show expression" py-show-expression
		    :help " `py-show-expression'

Show expression at point\. "]

		   ["Show partial expression" py-show-partial-expression
		    :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		   ["Show line" py-show-line
		    :help " `py-show-line'

Show line at point\. "]

		   ["Show top level" py-show-top-level
		    :help " `py-show-top-level'

Show top-level at point\. "]))

                 "-"

                 ["Execute region" py-execute-region
                  :help " `py-execute-region'

Send the region to a Python interpreter.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment. "]

                 ["Execute buffer" py-execute-buffer
                  :help " `py-execute-buffer'

Send the contents of the buffer to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment.

If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file."]

                 ["Execute def or class" py-execute-def-or-class
                  :help " `py-execute-def-or-class'

Send def-or-class at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment."]

                 ["Execute statement" py-execute-statement
                  :help " `py-execute-statement'

Send statement at point to a Python interpreter.

When called with C-u, execution through `default-value' of `py-shell-name' is forced.
See also `py-force-py-shell-name-p'.

When called with C-u followed by a number different from 4 and 1, user is prompted to specify a shell. This might be the name of a system-wide shell or include the path to a virtual environment."]

                 ["Execute string" py-execute-string
                  :help " `py-execute-string'

Send the argument STRING to a Python interpreter.

See also `py-execute-region'. "]

                 ["Execute line" py-execute-line
                  :help " `py-execute-line'

Send current line from beginning of indent to Python interpreter\.  "]

                 ("More... "
                  :help "Python-specific features"

                  ["Execute top level" py-execute-top-level
                   :help " `py-execute-top-level'

Send top-level form at point to a Python interpreter. "]

                  ;; statement
                  ("Execute statement "
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

                   ["py-execute-statement-python3.3" py-execute-statement-python3.3
                    :help "Execute statement through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-statement-bpython" py-execute-statement-bpython
                    :help "Execute statement through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute statement dedicated" py-execute-statement-dedicated
		     :help " `py-execute-statement-dedicated'

Send statement to unique interpreter\. "]

		    ["py-execute-statement-python-dedicated" py-execute-statement-python-dedicated
		     :help "Execute statement through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-ipython-dedicated" py-execute-statement-ipython-dedicated
		     :help "Execute statement through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-python3-dedicated" py-execute-statement-python3-dedicated
		     :help "Execute statement through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-python2-dedicated" py-execute-statement-python2-dedicated
		     :help "Execute statement through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-python2.7-dedicated" py-execute-statement-python2.7-dedicated
		     :help "Execute statement through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-jython-dedicated" py-execute-statement-jython-dedicated
		     :help "Execute statement through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-python3.3-dedicated" py-execute-statement-python3.3-dedicated
		     :help "Execute statement through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-statement-bpython-dedicated" py-execute-statement-bpython-dedicated
		     :help "Execute statement through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
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

                    ["py-execute-statement-python3.3-switch" py-execute-statement-python3.3-switch
                     :help "Execute statement through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-statement-bpython-switch" py-execute-statement-bpython-switch
                     :help "Execute statement through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]

                    ;; dedicated-switch

                    ["py-execute-statement-python-dedicated-switch" py-execute-statement-python-dedicated-switch
                     :help "Execute statement through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-ipython-dedicated-switch" py-execute-statement-ipython-dedicated-switch
                     :help "Execute statement through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python3-dedicated-switch" py-execute-statement-python3-dedicated-switch
                     :help "Execute statement through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python2-dedicated-switch" py-execute-statement-python2-dedicated-switch
                     :help "Execute statement through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python2.7-dedicated-switch" py-execute-statement-python2.7-dedicated-switch
                     :help "Execute statement through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-jython-dedicated-switch" py-execute-statement-jython-dedicated-switch
                     :help "Execute statement through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-python3.3-dedicated-switch" py-execute-statement-python3.3-dedicated-switch
                     :help "Execute statement through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-statement-bpython-dedicated-switch" py-execute-statement-bpython-dedicated-switch
                     :help "Execute statement through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; block
                  ("Execute block "
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

                   ["py-execute-block-python3.3" py-execute-block-python3.3
                    :help "Execute block through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-block-bpython" py-execute-block-bpython
                    :help "Execute block through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute block dedicated" py-execute-block-dedicated
		     :help " `py-execute-block-dedicated'

Send block to unique interpreter\. "]

		    ["py-execute-block-python-dedicated" py-execute-block-python-dedicated
		     :help "Execute block through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-ipython-dedicated" py-execute-block-ipython-dedicated
		     :help "Execute block through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-python3-dedicated" py-execute-block-python3-dedicated
		     :help "Execute block through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-python2-dedicated" py-execute-block-python2-dedicated
		     :help "Execute block through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-python2.7-dedicated" py-execute-block-python2.7-dedicated
		     :help "Execute block through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-jython-dedicated" py-execute-block-jython-dedicated
		     :help "Execute block through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-python3.3-dedicated" py-execute-block-python3.3-dedicated
		     :help "Execute block through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-block-bpython-dedicated" py-execute-block-bpython-dedicated
		     :help "Execute block through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

                   ("Ignoring defaults "
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

                    ["py-execute-block-python3.3-switch" py-execute-block-python3.3-switch
                     :help "Execute block through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-block-bpython-switch" py-execute-block-bpython-switch
                     :help "Execute block through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-block-python-dedicated-switch" py-execute-block-python-dedicated-switch
                     :help "Execute block through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-ipython-dedicated-switch" py-execute-block-ipython-dedicated-switch
                     :help "Execute block through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python3-dedicated-switch" py-execute-block-python3-dedicated-switch
                     :help "Execute block through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python2-dedicated-switch" py-execute-block-python2-dedicated-switch
                     :help "Execute block through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python2.7-dedicated-switch" py-execute-block-python2.7-dedicated-switch
                     :help "Execute block through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-jython-dedicated-switch" py-execute-block-jython-dedicated-switch
                     :help "Execute block through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-python3.3-dedicated-switch" py-execute-block-python3.3-dedicated-switch
                     :help "Execute block through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-block-bpython-dedicated-switch" py-execute-block-bpython-dedicated-switch
                     :help "Execute block through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; def
                  ("Execute def "
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

                   ["py-execute-def-python3.3" py-execute-def-python3.3
                    :help "Execute def through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-def-bpython" py-execute-def-bpython
                    :help "Execute def through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute def dedicated" py-execute-def-dedicated
		     :help " `py-execute-def-dedicated'

Send def to unique interpreter\. "]

		    ["py-execute-def-python-dedicated" py-execute-def-python-dedicated
		     :help "Execute def through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-ipython-dedicated" py-execute-def-ipython-dedicated
		     :help "Execute def through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-python3-dedicated" py-execute-def-python3-dedicated
		     :help "Execute def through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-python2-dedicated" py-execute-def-python2-dedicated
		     :help "Execute def through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-python2.7-dedicated" py-execute-def-python2.7-dedicated
		     :help "Execute def through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-jython-dedicated" py-execute-def-jython-dedicated
		     :help "Execute def through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-python3.3-dedicated" py-execute-def-python3.3-dedicated
		     :help "Execute def through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-def-bpython-dedicated" py-execute-def-bpython-dedicated
		     :help "Execute def through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		   )

                   ("Ignoring defaults "
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

                    ["py-execute-def-python3.3-switch" py-execute-def-python3.3-switch
                     :help "Execute def through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-def-bpython-switch" py-execute-def-bpython-switch
                     :help "Execute def through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-def-python-dedicated-switch" py-execute-def-python-dedicated-switch
                     :help "Execute def through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-ipython-dedicated-switch" py-execute-def-ipython-dedicated-switch
                     :help "Execute def through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python3-dedicated-switch" py-execute-def-python3-dedicated-switch
                     :help "Execute def through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python2-dedicated-switch" py-execute-def-python2-dedicated-switch
                     :help "Execute def through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python2.7-dedicated-switch" py-execute-def-python2.7-dedicated-switch
                     :help "Execute def through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-jython-dedicated-switch" py-execute-def-jython-dedicated-switch
                     :help "Execute def through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-python3.3-dedicated-switch" py-execute-def-python3.3-dedicated-switch
                     :help "Execute def through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-def-bpython-dedicated-switch" py-execute-def-bpython-dedicated-switch
                     :help "Execute def through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; class
                  ("Execute class "
                   :help "Execute class functions"

		   ["Execute class" py-execute-class
		    :help " `py-execute-class'

Send class at point to a Python interpreter\. "]

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

                   ["py-execute-class-python3.3" py-execute-class-python3.3
                    :help "Execute class through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-class-bpython" py-execute-class-bpython
                    :help "Execute class through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["py-execute-class-python-dedicated" py-execute-class-python-dedicated
		     :help "Execute class through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-ipython-dedicated" py-execute-class-ipython-dedicated
		     :help "Execute class through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-python3-dedicated" py-execute-class-python3-dedicated
		     :help "Execute class through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-python2-dedicated" py-execute-class-python2-dedicated
		     :help "Execute class through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-python2.7-dedicated" py-execute-class-python2.7-dedicated
		     :help "Execute class through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-jython-dedicated" py-execute-class-jython-dedicated
		     :help "Execute class through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-python3.3-dedicated" py-execute-class-python3.3-dedicated
		     :help "Execute class through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-class-bpython-dedicated" py-execute-class-bpython-dedicated
		     :help "Execute class through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
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

                    ["py-execute-class-python3.3-switch" py-execute-class-python3.3-switch
                     :help "Execute class through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-class-bpython-switch" py-execute-class-bpython-switch
                     :help "Execute class through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-class-python-dedicated-switch" py-execute-class-python-dedicated-switch
                     :help "Execute class through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-ipython-dedicated-switch" py-execute-class-ipython-dedicated-switch
                     :help "Execute class through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python3-dedicated-switch" py-execute-class-python3-dedicated-switch
                     :help "Execute class through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python2-dedicated-switch" py-execute-class-python2-dedicated-switch
                     :help "Execute class through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python2.7-dedicated-switch" py-execute-class-python2.7-dedicated-switch
                     :help "Execute class through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-jython-dedicated-switch" py-execute-class-jython-dedicated-switch
                     :help "Execute class through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-python3.3-dedicated-switch" py-execute-class-python3.3-dedicated-switch
                     :help "Execute class through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-class-bpython-dedicated-switch" py-execute-class-bpython-dedicated-switch
                     :help "Execute class through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; region
                  ("Execute region "
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

                   ["py-execute-region-python3.3" py-execute-region-python3.3
                    :help "Execute region through a Python3.3 interpreter.
        With \\[universal-argument] use an unique Python3.3 interpreter. "]

                   ["py-execute-region-bpython" py-execute-region-bpython
                    :help "Execute region through a Bpython interpreter.
        With \\[universal-argument] use an unique Bpython interpreter. "]
                   ("Dedicated"

		    ["Execute region dedicated" py-execute-region-dedicated
		     :help " `py-execute-region-dedicated'

Send region to unique interpreter\. "]

		    ["py-execute-region-python-dedicated" py-execute-region-python-dedicated
		     :help "Execute region through a unique Python interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-ipython-dedicated" py-execute-region-ipython-dedicated
		     :help "Execute region through a unique IPython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-python3-dedicated" py-execute-region-python3-dedicated
		     :help "Execute region through a unique Python3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-python2-dedicated" py-execute-region-python2-dedicated
		     :help "Execute region through a unique Python2 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-python2.7-dedicated" py-execute-region-python2.7-dedicated
		     :help "Execute region through a unique Python2.7 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-jython-dedicated" py-execute-region-jython-dedicated
		     :help "Execute region through a unique Jython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-python3.3-dedicated" py-execute-region-python3.3-dedicated
		     :help "Execute region through a unique Python3.3 interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]

		    ["py-execute-region-bpython-dedicated" py-execute-region-bpython-dedicated
		     :help "Execute region through a unique Bpython interpreter.
Optional \\[universal-argument] forces switch to output buffer, ignores `py-switch-buffers-on-execute-p'. "]
		    )

		   ("Ignoring defaults "
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

                    ["py-execute-region-python3.3-switch" py-execute-region-python3.3-switch
                     :help "Execute region through a Python3.3 interpreter.
With \\[universal-argument] use an unique Python3.3 interpreter. "]

                    ["py-execute-region-bpython-switch" py-execute-region-bpython-switch
                     :help "Execute region through a Bpython interpreter.
With \\[universal-argument] use an unique Bpython interpreter. "]
                    ;; dedicated-switch

                    ["py-execute-region-python-dedicated-switch" py-execute-region-python-dedicated-switch
                     :help "Execute region through a unique Python interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-ipython-dedicated-switch" py-execute-region-ipython-dedicated-switch
                     :help "Execute region through a uniquen IPython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python3-dedicated-switch" py-execute-region-python3-dedicated-switch
                     :help "Execute region through a unique Python3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python2-dedicated-switch" py-execute-region-python2-dedicated-switch
                     :help "Execute region through a unique Python2 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python2.7-dedicated-switch" py-execute-region-python2.7-dedicated-switch
                     :help "Execute region through a unique Python2.7 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-jython-dedicated-switch" py-execute-region-jython-dedicated-switch
                     :help "Execute region through a unique Jython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-python3.3-dedicated-switch" py-execute-region-python3.3-dedicated-switch
                     :help "Execute region through a unique Python3.3 interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]

                    ["py-execute-region-bpython-dedicated-switch" py-execute-region-bpython-dedicated-switch
                     :help "Execute region through a unique Bpython interpreter.
Switch to output buffer; ignores `py-switch-buffers-on-execute-p' "]
                    ))

                  ;; file
                  ("Execute file "
                   :help "Execute file functions"

                  ["Execute file" py-execute-file
                   :help "`py-execute-file'
       Send file at point to Python interpreter. "]

                   ["Execute file python" py-execute-file-python
                    :help " `py-execute-file-python'
Send file to a Python interpreter. "]

                   ["Execute file ipython" py-execute-file-ipython
                    :help " `py-execute-file-ipython'
Send file to a Ipython interpreter. "]

                   ["Execute file python3" py-execute-file-python3
                    :help " `py-execute-file-python3'
Send file to a Python3 interpreter. "]

                   ["Execute file python2" py-execute-file-python2
                    :help " `py-execute-file-python2'
Send file to a Python2 interpreter. "]

                   ["Execute file python2.7" py-execute-file-python2.7
                    :help " `py-execute-file-python2.7'
Send file to a Python2.7 interpreter. "]

                   ["Execute file jython" py-execute-file-jython
                    :help " `py-execute-file-jython'
Send file to a Jython interpreter. "]

                   ["Execute file python3.3" py-execute-file-python3.3
                    :help " `py-execute-file-python3.3'
Send file to a Python3.3 interpreter. "]

                   ["Execute file bpython" py-execute-file-bpython
                    :help " `py-execute-file-bpython'
Send file to a Bpython interpreter. "]

		   ("Dedicated"
		    ["Execute file dedicated" py-execute-file-dedicated
		     :help " `py-execute-file-dedicated'

"])

                   ("Ignoring defaults "
                    :help "Commands will ignore default setting of
`py-switch-buffers-on-execute-p' and `py-split-windows-on-execute-p'"

                    ["Execute file python switch" py-execute-file-python-switch
                     :help " `py-execute-file-python-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python no-switch" py-execute-file-python-no-switch
                     :help " `py-execute-file-python-no-switch'
Send file to a Python interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]
		    
                    ["Execute file python dedicated" py-execute-file-python-dedicated
                     :help " `py-execute-file-python-dedicated'
Send file to a Python interpreter.

Uses a dedicated shell. "]

                    ["Execute file python dedicated switch" py-execute-file-python-dedicated-switch
                     :help " `py-execute-file-python-dedicated-switch'
Send file to a Python interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython switch" py-execute-file-ipython-switch
                     :help " `py-execute-file-ipython-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file ipython no-switch" py-execute-file-ipython-no-switch
                     :help " `py-execute-file-ipython-no-switch'
Send file to a Ipython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file ipython dedicated" py-execute-file-ipython-dedicated
                     :help " `py-execute-file-ipython-dedicated'
Send file to a Ipython interpreter.

Uses a dedicated shell. "]

                    ["Execute file ipython dedicated switch" py-execute-file-ipython-dedicated-switch
                     :help " `py-execute-file-ipython-dedicated-switch'
Send file to a Ipython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 switch" py-execute-file-python3-switch
                     :help " `py-execute-file-python3-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3 no-switch" py-execute-file-python3-no-switch
                     :help " `py-execute-file-python3-no-switch'
Send file to a Python3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3 dedicated" py-execute-file-python3-dedicated
                     :help " `py-execute-file-python3-dedicated'
Send file to a Python3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3 dedicated switch" py-execute-file-python3-dedicated-switch
                     :help " `py-execute-file-python3-dedicated-switch'
Send file to a Python3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 switch" py-execute-file-python2-switch
                     :help " `py-execute-file-python2-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2 no-switch" py-execute-file-python2-no-switch
                     :help " `py-execute-file-python2-no-switch'
Send file to a Python2 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2 dedicated" py-execute-file-python2-dedicated
                     :help " `py-execute-file-python2-dedicated'
Send file to a Python2 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2 dedicated switch" py-execute-file-python2-dedicated-switch
                     :help " `py-execute-file-python2-dedicated-switch'
Send file to a Python2 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 switch" py-execute-file-python2.7-switch
                     :help " `py-execute-file-python2.7-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python2.7 no-switch" py-execute-file-python2.7-no-switch
                     :help " `py-execute-file-python2.7-no-switch'
Send file to a Python2.7 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python2.7 dedicated" py-execute-file-python2.7-dedicated
                     :help " `py-execute-file-python2.7-dedicated'
Send file to a Python2.7 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python2.7 dedicated switch" py-execute-file-python2.7-dedicated-switch
                     :help " `py-execute-file-python2.7-dedicated-switch'
Send file to a Python2.7 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython switch" py-execute-file-jython-switch
                     :help " `py-execute-file-jython-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file jython no-switch" py-execute-file-jython-no-switch
                     :help " `py-execute-file-jython-no-switch'
Send file to a Jython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file jython dedicated" py-execute-file-jython-dedicated
                     :help " `py-execute-file-jython-dedicated'
Send file to a Jython interpreter.

Uses a dedicated shell. "]

                    ["Execute file jython dedicated switch" py-execute-file-jython-dedicated-switch
                     :help " `py-execute-file-jython-dedicated-switch'
Send file to a Jython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 switch" py-execute-file-python3.3-switch
                     :help " `py-execute-file-python3.3-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file python3.3 no-switch" py-execute-file-python3.3-no-switch
                     :help " `py-execute-file-python3.3-no-switch'
Send file to a Python3.3 interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file python3.3 dedicated" py-execute-file-python3.3-dedicated
                     :help " `py-execute-file-python3.3-dedicated'
Send file to a Python3.3 interpreter.

Uses a dedicated shell. "]

                    ["Execute file python3.3 dedicated switch" py-execute-file-python3.3-dedicated-switch
                     :help " `py-execute-file-python3.3-dedicated-switch'
Send file to a Python3.3 interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython switch" py-execute-file-bpython-switch
                     :help " `py-execute-file-bpython-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]

                    ["Execute file bpython no-switch" py-execute-file-bpython-no-switch
                     :help " `py-execute-file-bpython-no-switch'
Send file to a Bpython interpreter.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "nil". "]

                    ["Execute file bpython dedicated" py-execute-file-bpython-dedicated
                     :help " `py-execute-file-bpython-dedicated'
Send file to a Bpython interpreter.

Uses a dedicated shell. "]

                    ["Execute file bpython dedicated switch" py-execute-file-bpython-dedicated-switch
                     :help " `py-execute-file-bpython-dedicated-switch'
Send file to a Bpython interpreter.

Uses a dedicated shell.
Ignores default of `py-switch-buffers-on-execute-p', uses it with value "non-nil". "]))


                  ["Execute minor block" py-execute-minor-block
                   :help " `py-execute-minor-block'

Send minor-block at point to a Python interpreter.

A minor block is started by a `for', `if', `try' or `with'.
. "]

                  ["Execute def" py-execute-def
                   :help "`py-execute-def'
       Send def at point to Python interpreter. "]

                  ["Execute class" py-execute-class
                   :help "`py-execute-class'
       Send class at point to Python interpreter. "]


		  )

                 ("Fast process..."

                  ["Fast send string" py--fast-send-string
                   :help " `py--fast-send-string'

Process Python strings, being prepared for large output\.

Output buffer displays \"Fast\" in name by default
See also `py-fast-shell'"]

                  ["Process region fast" py-process-region-fast
                   :help " `py-process-region-fast'

 "]

                  ["Execute statement fast" py-execute-statement-fast
                   :help " `py-execute-statement-fast'

Process statement at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute block fast" py-execute-block-fast
                   :help " `py-execute-block-fast'

Process block at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute block or clause fast" py-execute-block-or-clause-fast
                   :help " `py-execute-block-or-clause-fast'

Process block-or-clause at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute def fast" py-execute-def-fast
                   :help " `py-execute-def-fast'

Process def at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute class fast" py-execute-class-fast
                   :help " `py-execute-class-fast'

Process class at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute def or class fast" py-execute-def-or-class-fast
                   :help " `py-execute-def-or-class-fast'

Process def-or-class at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute expression fast" py-execute-expression-fast
                   :help " `py-execute-expression-fast'

Process expression at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute partial expression fast" py-execute-partial-expression-fast
                   :help " `py-execute-partial-expression-fast'

Process partial-expression at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute top level fast" py-execute-top-level-fast
                   :help " `py-execute-top-level-fast'

Process top-level at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Output-buffer is not in comint-mode "]

                  ["Execute clause fast" py-execute-clause-fast
                   :help " `py-execute-clause-fast'

Process clause at point by a Python interpreter\.

Suitable for large output, doesn't mess up interactive shell\.
Result arrives in output-buffer, which is not in comint-mode "])

                 "-"

                 ("Virtualenv"

                  ["Virtualenv workon" virtualenv-workon
                   :help " runs `virtualenv-workon'

Make sure virtualenv is provided

"]

                  ["Virtualenv activate" virtualenv-activate
                   :help " `virtualenv-activate'

Activate the virtualenv located in DIR. "]

                  ["Virtualenv deactivate" virtualenv-deactivate
                   :help " `virtualenv-deactivate'

Deactivate the current virtual enviroment. "]

                  ["Virtualenv p" virtualenv-p
                   :help " `virtualenv-p'

Check if a directory is a virtualenv. "])

                 ["Execute import or reload" py-execute-import-or-reload
                  :help " `py-execute-import-or-reload'

Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in ".py", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See also `M-x py-execute-region'.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions. "]

                 ("Help"

		  ["Find definition" py-find-definition
		   :help " `py-find-definition'

Find source of definition of SYMBOL\.

Interactively, prompt for SYMBOL\."]

		  ["Imenu" imenu
		   :help " `imenu'

Jump to a INDEX-ITEM "]

                  ["Info lookup symbol" py-info-lookup-symbol
                   :help " `py-info-lookup-symbol'

Calls `info-lookup-symbol'.

Sends help if stuff is missing. "]

                  ["Symbol at point" py-symbol-at-point
                   :help " `py-symbol-at-point'

Return the current Python symbol\. "]

		  "-"

                  ["Describe mode"        py-describe-mode t]

                  ["Help on symbol" py-help-at-point
                   :help "`py-help-at-point'\n
Use pydoc on symbol at point"]

		  )

                 ("Debugger"

                  ["pdb" pdb
		   :help "`pdb' Run pdb under GUD"]

		  ["Execute statement pdb" py-execute-statement-pdb
		   :help " `py-execute-statement-pdb'

Execute statement running pdb\. . "])
                 ("Checks"

                  ["pychecker-run" py-pychecker-run
                   :help "`py-pychecker-run'
Run pychecker

Call `easy_install pyflakes' resp. `pip... 'if not available"]

                  ("Pylint "
                   :help "Extendet report options

Call `easy_install pylint' resp. `pip...' if not available"

                   ["py-pylint-run" py-pylint-run
                    :help "`py-pylint-run'
Pylint will display a number of messages as it analyzes the code,
as well as some statistics about the number of warnings and
errors found in different files - unless called with arg \"--errors-only\". The messages are classified
under various categories such as errors and warnings

Pylint checks length of lines of code, if variable names are
well-formed according to your coding standard, if declared
interfaces are truly implemented, and much more. Additionally, it
is possible to write plugins.

Call `easy_install pylint' resp. `pip...' if not available
"]

                   ["py-pylint-help" py-pylint-help
                    :help "`py-pylint-help'
List extendet report options
"]
                   ["pylint-flymake-mode" pylint-flymake-mode
                    :help "`pylint-flymake-mode'
Toggle flymake-mode running `pylint'
"])

                  ("pep8 "
                   :help "Check formatting

Call `easy_install pep8' resp. `pip...' if not available"

                   ["pep8-run" py-pep8-run
                    :help "`py-pep8-run'
Check formatting (default on the file currently visited)

Call `easy_install pep8' resp. `pip...' if not available
"]

                   ["pep8-help" py-pep8-help
                    :help "`py-pep8-help'
Display help for pep8 format checker)
"]

                   ["pep8-flymake-mode" pep8-flymake-mode
                    :help "`pep8-flymake-mode'
Toggle flymake-mode running `pep8'
"])

                  ("Pyflakes " :help "Non intrusive code checker

Call `easy_install pyflakes' resp. `pip...' if not available"

                   ["pyflakes-run" py-pyflakes-run :help
                    "`py-pyflakes-run' Run pyflakes

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakes-help" py-pyflakes-help :help
                    "`py-pyflakes-help' Display help for
              Pyflakes "]

                   ["pyflakes-flymake-mode" pyflakes-flymake-mode :help
                    "`pyflakes-flymake-mode'
Toggle flymake-mode running `pyflakes' "])

                  ("Flake8 " :help
                   "code checker running "

                   ["Flake8 run" py-flake8-run
                    :help " `py-flake8-run'

        Flake8 is a wrapper around these tools:
        - PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - lines that contain a ``# noqa`` comment at the end will not issue warnings.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points.

. "]

                   ["Flake8 help" py-flake8-help
                    :help " `py-flake8-help'

Display flake8 command line help messages. "])

                  ("Pyflakes-pep8 " :help
                   "Non intrusive code checker running `pyflakes' and `pep8'
call `easy_install pyflakes' resp. `pip...' and `easy_install pep8' if basics not available"

                   ["pyflakespep8-run" py-pyflakespep8-run :help
                    "`py-pyflakespep8-run' Run `pyflakespep8'

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakespep8-help" py-pyflakespep8-help :help
                    "`py-pyflakespep8-help' Display help for
              Pyflakespep8 "]

                   ["pyflakespep8-flymake-mode" pyflakespep8-flymake-mode :help
                    "`pyflakespep8-flymake-mode'
Toggle flymake-mode running `pyflakespep8' "]))

                 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"
                   ("Interpreter"

                    ["Shell prompt read only"
                     (setq py-shell-prompt-read-only
                           (not py-shell-prompt-read-only))
                     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-shell-prompt-read-only]

                    ["Remove cwd from path"
                     (setq py-remove-cwd-from-path
                           (not py-remove-cwd-from-path))
                     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-remove-cwd-from-path]

                    ["Honor IPYTHONDIR "
                     (setq py-honor-IPYTHONDIR-p
                           (not py-honor-IPYTHONDIR-p))
                     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

                    ["Honor PYTHONHISTORY "
                     (setq py-honor-PYTHONHISTORY-p
                           (not py-honor-PYTHONHISTORY-p))
                     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-honor-PYTHONHISTORY-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "])

                   ("Execute"

		    ["Fast process" py-fast-process-p
		     :help " `py-fast-process-p'

Use `py-fast-process'\.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Output-buffer is not in comint-mode"
		     :style toggle :selected py-fast-process-p]

		    ["Python mode v5 behavior"
                     (setq python-mode-v5-behavior-p
                           (not python-mode-v5-behavior-p))
                     :help "Execute region through `shell-command-on-region' as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected python-mode-v5-behavior-p]

                    ["Force shell name "
                     (setq py-force-py-shell-name-p
                           (not py-force-py-shell-name-p))
                     :help "When `t', execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-force-py-shell-name-p]

                    ["Cleanup temporary"
                     (setq py-cleanup-temporary
                           (not py-cleanup-temporary))
                     :help "If temporary buffers and files used by functions executing region should be deleted afterwards. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-cleanup-temporary]

                    ["Execute \"if name == main\" blocks p"
                     (setq py-if-name-main-permission-p
                           (not py-if-name-main-permission-p))
                     :help " `py-if-name-main-permission-p'

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
                     :style toggle :selected py-if-name-main-permission-p]

                    ["Ask about save"
                     (setq py-ask-about-save
                           (not py-ask-about-save))
                     :help "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-ask-about-save]

                    ["Store result"
                     (setq py-store-result-p
                           (not py-store-result-p))
                     :help " `py-store-result-p'

When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked. "
                     :style toggle :selected py-store-result-p]

                    ["Prompt on changed "
                     (setq py-prompt-on-changed-p
                           (not py-prompt-on-changed-p))
                     :help "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is `t'Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-prompt-on-changed-p]

                    ["Dedicated process "
                     (setq py-dedicated-process-p
                           (not py-dedicated-process-p))
                     :help "If commands executing code use a dedicated shell.

Default is nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-dedicated-process-p]

                    ["Execute without temporary file"
                     (setq py-execute-no-temp-p
                           (not py-execute-no-temp-p))
                     :help " `py-execute-no-temp-p'
Seems Emacs-24.3 provided a way executing stuff without temporary files.
In experimental state yet "
                     :style toggle :selected py-execute-no-temp-p]

                    ["Warn tmp files left "
                     (setq py--warn-tmp-files-left-p
                           (not py--warn-tmp-files-left-p))
                     :help "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py--warn-tmp-files-left-p])

                   ("Edit"

                    ("Completion"

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found\. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Set Pymacs-based complete keymap "
                      (setq py-set-complete-keymap-p
                            (not py-set-complete-keymap-p))
                      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-set-complete-keymap-p]

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Indent no completion "
                      (setq py-indent-no-completion-p
                            (not py-indent-no-completion-p))
                      :help "If completion function should indent when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-no-completion-p]

                     ["Company pycomplete "
                      (setq py-company-pycomplete-p
                            (not py-company-pycomplete-p))
                      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-company-pycomplete-p])

                    ("Autopair mode"
                     :help "Toggle `autopair-mode'"

                     ["Toggle autopair mode" toggle-py-autopair-mode
                      :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off.

  Returns value of `autopair-mode ' switched to. "]

                     ["Autopair mode on" py-autopair-mode-on
                      :help " `autopair-mode on'

Make sure, `autopair-mode' is on.

Returns value of `autopair-mode'. "]

                     ["Autopair mode off" py-autopair-mode-off
                      :help " `autopair-mode' off

Make sure, `autopair-mode' is off.

Returns value of `autopair-mode'. "])

                    ;; py-smart-operator-mode-p forms
                    ("Smart operator mode"
                     :help "Toggle `smart-operator-mode'"

                     ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                      :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off.

  Returns value of `smart-operator-mode ' switched to. "]

                     ["Smart operator mode on" py-smart-operator-mode-p-on
                      :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on.

Returns value of `smart-operator-mode'. "]

                     ["Smart operator mode off" py-smart-operator-mode-p-off
                      :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off.

Returns value of `smart-operator-mode'. "])

                    ("Filling"

                     ("Docstring styles"
                      :help "Switch docstring-style"

                      ["Nil" py-set-nil-docstring-style
                       :help " `py-set-nil-docstring-style'

Set py-docstring-style to nil, format string normally. "]

                      ["pep-257-nn" py-set-pep-257-nn-docstring-style
                       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

                      ["pep-257" py-set-pep-257-docstring-style
                       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

                      ["django" py-set-django-docstring-style
                       :help " `py-set-django-docstring-style'

Set py-docstring-style to 'django "]

                      ["onetwo" py-set-onetwo-docstring-style
                       :help " `py-set-onetwo-docstring-style'

Set py-docstring-style to 'onetwo "]

                      ["symmetric" py-set-symmetric-docstring-style
                       :help " `py-set-symmetric-docstring-style'

Set py-docstring-style to 'symmetric "])

                     ["Auto-fill mode"
                      (setq py-auto-fill-mode
                            (not py-auto-fill-mode))
                      :help "Fill according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-auto-fill-mode])

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    ("Indent"
		     ("TAB related"

		      ["indent-tabs-mode"
		       (setq indent-tabs-mode
			     (not indent-tabs-mode))
		       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected indent-tabs-mode]

		      ["Tab indent"
		       (setq py-tab-indent
			     (not py-tab-indent))
		       :help "Non-nil means TAB in Python mode calls `py-indent-line'.Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indent]

		      ["Tab shifts region "
		       (setq py-tab-shifts-region-p
			     (not py-tab-shifts-region-p))
		       :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-shifts-region-p]

		      ["Tab indents region "
		       (setq py-tab-indents-region-p
			     (not py-tab-indents-region-p))
		       :help "When `t' and first TAB doesn't shift, indent-region is called.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indents-region-p])

                     ["Close at start column"
                      (setq py-closing-list-dedents-bos
                            (not py-closing-list-dedents-bos))
                      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-dedents-bos]

                     ["Closing list keeps space"
                      (setq py-closing-list-keeps-space
                            (not py-closing-list-keeps-space))
                      :help "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-keeps-space]

                     ["Closing list space"
                      (setq py-closing-list-space
                            (not py-closing-list-space))
                      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-space]

                     ["Tab shifts region "
                      (setq py-tab-shifts-region-p
                            (not py-tab-shifts-region-p))
                      :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-tab-shifts-region-p]

                     ["Lhs inbound indent"
                      (setq py-lhs-inbound-indent
                            (not py-lhs-inbound-indent))
                      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-lhs-inbound-indent]

                     ["Continuation offset"
                      (setq py-continuation-offset
                            (not py-continuation-offset))
                      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-continuation-offset]

                     ["Electric colon"
                      (setq py-electric-colon-active-p
                            (not py-electric-colon-active-p))
                      :help " `py-electric-colon-active-p'

`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
                      :style toggle :selected py-electric-colon-active-p]

                     ["Electric colon at beginning of block only"
                      (setq py-electric-colon-bobl-only
                            (not py-electric-colon-bobl-only))
                      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-colon-bobl-only]

                     ["Electric yank active "
                      (setq py-electric-yank-active-p
                            (not py-electric-yank-active-p))
                      :help " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-yank-active-p]

                     ["Electric kill backward "
                      (setq py-electric-kill-backward-p
                            (not py-electric-kill-backward-p))
                      :help "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string\[0:1]
--------------^

==>

my_string\[]
----------^

In result cursor is insided emptied delimited form.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-kill-backward-p]

                     ["Trailing whitespace smart delete "
                      (setq py-trailing-whitespace-smart-delete-p
                            (not py-trailing-whitespace-smart-delete-p))
                      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-trailing-whitespace-smart-delete-p]

                     ["Newline delete trailing whitespace "
                      (setq py-newline-delete-trailing-whitespace-p
                            (not py-newline-delete-trailing-whitespace-p))
                      :help "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-newline-delete-trailing-whitespace-p]

                     ["Dedent keep relative column"
                      (setq py-dedent-keep-relative-column
                            (not py-dedent-keep-relative-column))
                      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-dedent-keep-relative-column]

                     ["Indent paren spanned multilines "
                      (setq py-indent-paren-spanned-multilines-p
                            (not py-indent-paren-spanned-multilines-p))
                      :help "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-paren-spanned-multilines-p]

                     ["Indent honors multiline listing"
                      (setq py-indent-honors-multiline-listing
                            (not py-indent-honors-multiline-listing))
                      :help "If `t', indents to 1\+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-multiline-listing]

                     ["Indent comment "
                      (setq py-indent-comments
                            (not py-indent-comments))
                      :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-comments]

                     ["Uncomment indents "
                      (setq py-uncomment-indents-p
                            (not py-uncomment-indents-p))
                      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-uncomment-indents-p]

                     ["Indent honors inline comment"
                      (setq py-indent-honors-inline-comment
                            (not py-indent-honors-inline-comment))
                      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-inline-comment]

                     ["Kill empty line"
                      (setq py-kill-empty-line
                            (not py-kill-empty-line))
                      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-kill-empty-line]

                     ("Smart indentation"
                      :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

                      ["Toggle py-smart-indentation" toggle-py-smart-indentation
                       :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation on" py-smart-indentation-on
                       :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation off" py-smart-indentation-off
                       :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"])

                     ["Beep if tab change"
                      (setq py-beep-if-tab-change
                            (not py-beep-if-tab-change))
                      :help "Ring the bell if `tab-width' is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-beep-if-tab-change]

                     ["Highlight indentation" highlight-indentation
                      :help "Toggle highlight indentation.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is installed"

                      ]

                     ["Electric comment "
                      (setq py-electric-comment-p
                            (not py-electric-comment-p))
                      :help "If \"#\" should call `py-electric-comment'. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-p]

                     ["Electric comment add space "
                      (setq py-electric-comment-add-space-p
                            (not py-electric-comment-add-space-p))
                      :help "If py-electric-comment should add a space.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-add-space-p]

                     ["Empty line closes "
                      (setq py-empty-line-closes-p
                            (not py-empty-line-closes-p))
                      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-empty-line-closes-p])
                    ["Defun use top level "
                     (setq py-defun-use-top-level-p
                           (not py-defun-use-top-level-p))
                     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc. "
                     :style toggle :selected py-defun-use-top-level-p]

                    ["Close provides newline"
                     (setq py-close-provides-newline
                           (not py-close-provides-newline))
                     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-close-provides-newline]

                    ["Block comment prefix "
                     (setq py-block-comment-prefix-p
                           (not py-block-comment-prefix-p))
                     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-block-comment-prefix-p])

                   ("Display"

                    ("Index"

                     ["Imenu create index "
                      (setq py--imenu-create-index-p
                            (not py--imenu-create-index-p))
                      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py--imenu-create-index-p]

                     ["Imenu show method args "
                      (setq py-imenu-show-method-args-p
                            (not py-imenu-show-method-args-p))
                      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-imenu-show-method-args-p]
                     ["Switch index-function" py-switch-imenu-index-function
                      :help "`py-switch-imenu-index-function'
Switch between `py--imenu-create-index' from 5.1 series and `py--imenu-create-index-new'."])

                    ("Fontification"

                     ["Mark decorators"
                      (setq py-mark-decorators
                            (not py-mark-decorators))
                      :help "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-mark-decorators]

                     ["Fontify shell buffer "
                      (setq py-fontify-shell-buffer-p
                            (not py-fontify-shell-buffer-p))
                      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fontify-shell-buffer-p]

                     ["Use font lock doc face "
                      (setq py-use-font-lock-doc-face-p
                            (not py-use-font-lock-doc-face-p))
                      :help "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.

Call M-x `customize-face' in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-font-lock-doc-face-p])

                    ["Switch buffers on execute"
                     (setq py-switch-buffers-on-execute-p
                           (not py-switch-buffers-on-execute-p))
                     :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-switch-buffers-on-execute-p]

                    ["Split windows on execute"
                     (setq py-split-windows-on-execute-p
                           (not py-split-windows-on-execute-p))
                     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-p]

                    ["Keep windows configuration"
                     (setq py-keep-windows-configuration
                           (not py-keep-windows-configuration))
                     :help "If a windows is splitted displaying results, this is directed by variable `py-split-windows-on-execute-p'\. Also setting `py-switch-buffers-on-execute-p' affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting `py-keep-windows-configuration' to `t' will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-keep-windows-configuration]

                    ["Which split windows on execute function"
                     (progn
                       (if (eq 'split-window-vertically py-split-windows-on-execute-function)
                           (setq py-split-windows-on-execute-function'split-window-horizontally)
                         (setq py-split-windows-on-execute-function 'split-window-vertically))
                       (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

                     :help "If `split-window-vertically' or `...-horizontally'. Use `M-x customize-variable' RET `py-split-windows-on-execute-function' RET to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-function]

                    ["Modeline display full path "
                     (setq py-modeline-display-full-path-p
                           (not py-modeline-display-full-path-p))
                     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-display-full-path-p]

                    ["Modeline acronym display home "
                     (setq py-modeline-acronym-display-home-p
                           (not py-modeline-acronym-display-home-p))
                     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-acronym-display-home-p]

                    ["Hide show hide docstrings"
                     (setq py-hide-show-hide-docstrings
                           (not py-hide-show-hide-docstrings))
                     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-show-hide-docstrings]

                    ["Hide comments when hiding all"
                     (setq py-hide-comments-when-hiding-all
                           (not py-hide-comments-when-hiding-all))
                     :help "Hide the comments too when you do `hs-hide-all'. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-comments-when-hiding-all]

                    ["Max help buffer "
                     (setq py-max-help-buffer-p
                           (not py-max-help-buffer-p))
                     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-max-help-buffer-p]

                    ["Current defun show"
                     (setq py-current-defun-show
                           (not py-current-defun-show))
                     :help "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-current-defun-show]

                    ["Match paren mode"
                     (setq py-match-paren-mode
                           (not py-match-paren-mode))
                     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-match-paren-mode])

                   ("Debug"
		    
		    ["py-debug-p"
		     (setq py-debug-p
			   (not py-debug-p))
		     :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
		     :style toggle :selected py-debug-p]

                    ["Pdbtrack do tracking "
                     (setq py-pdbtrack-do-tracking-p
                           (not py-pdbtrack-do-tracking-p))
                     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-pdbtrack-do-tracking-p]

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]

                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p])

                   ("Other"

                    ("Directory"

                     ["Guess install directory "
                      (setq py-guess-py-install-directory-p
                            (not py-guess-py-install-directory-p))
                      :help "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-guess-py-install-directory-p]

                     ["Use local default"
                      (setq py-use-local-default
                            (not py-use-local-default))
                      :help "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-local-default]

                     ["Use current dir when execute "
                      (setq py-use-current-dir-when-execute-p
                            (not py-use-current-dir-when-execute-p))
                      :help "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-current-dir-when-execute-p]

                     ["Keep shell dir when execute "
                      (setq py-keep-shell-dir-when-execute-p
                            (not py-keep-shell-dir-when-execute-p))
                      :help "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-keep-shell-dir-when-execute-p]

                     ["Fileless buffer use default directory "
                      (setq py-fileless-buffer-use-default-directory-p
                            (not py-fileless-buffer-use-default-directory-p))
                      :help "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fileless-buffer-use-default-directory-p])

                    ("Underscore word syntax"
                     :help "Toggle `py-underscore-word-syntax-p'"

                     ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                      :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax on" py-underscore-word-syntax-p-on
                      :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax off" py-underscore-word-syntax-p-off
                      :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"])

                    ["Load pymacs "
                     (setq py-load-pymacs-p
                           (not py-load-pymacs-p))
                     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-load-pymacs-p]

                    ["Verbose "
                     (setq py-verbose-p
                           (not py-verbose-p))
                     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-verbose-p]

                    ["Empty comment line separates paragraph "
                     (setq empty-comment-line-separates-paragraph-p
                           (not empty-comment-line-separates-paragraph-p))
                     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected empty-comment-line-separates-paragraph-p]

                    ["Org cycle "
                     (setq py-org-cycle-p
                           (not py-org-cycle-p))
                     :help "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-org-cycle-p]

                    ["Set pager cat"
                     (setq py-set-pager-cat-p
                           (not py-set-pager-cat-p))
                     :help "If the shell environment variable \$PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' Use `M-x customize-variable' to
set it permanently"
                     :style toggle :selected py-set-pager-cat-p]

                    ["Edit only "
                     (setq py-edit-only-p
                           (not py-edit-only-p))
                     :help "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-edit-only-p])))

                 ("More... "

                  ("Edit commands "

		   ("Hide"
		    ["Hide statement" py-hide-statement
		     :help " `py-hide-statement'

Hide statement at point\. "]

		    ["Hide block" py-hide-block
		     :help " `py-hide-block'

Hide block at point\. "]

		    ["Hide clause" py-hide-clause
		     :help " `py-hide-clause'

Hide clause at point\. "]

		    ["Hide block or clause" py-hide-block-or-clause
		     :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		    ["Hide def" py-hide-def
		     :help " `py-hide-def'

Hide def at point\. "]

		    ["Hide class" py-hide-class
		     :help " `py-hide-class'

Hide class at point\. "]

		    ["Hide expression" py-hide-expression
		     :help " `py-hide-expression'

Hide expression at point\. "]

		    ["Hide partial expression" py-hide-partial-expression
		     :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		    ["Hide line" py-hide-line
		     :help " `py-hide-line'

Hide line at point\. "]

		    ["Hide top level" py-hide-top-level
		     :help " `py-hide-top-level'

Hide top-level at point\. "])

		   ("Show"

		    ["Show statement" py-show-statement
		     :help " `py-show-statement'

Show statement at point\. "]

		    ["Show block" py-show-block
		     :help " `py-show-block'

Show block at point\. "]

		    ["Show clause" py-show-clause
		     :help " `py-show-clause'

Show clause at point\. "]

		    ["Show block or clause" py-show-block-or-clause
		     :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		    ["Show def" py-show-def
		     :help " `py-show-def'

Show def at point\. "]

		    ["Show class" py-show-class
		     :help " `py-show-class'

Show class at point\. "]

		    ["Show expression" py-show-expression
		     :help " `py-show-expression'

Show expression at point\. "]

		    ["Show partial expression" py-show-partial-expression
		     :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		    ["Show line" py-show-line
		     :help " `py-show-line'

Show line at point\. "]

		    ["Show top level" py-show-top-level
		     :help " `py-show-top-level'

Show top-level at point\. "])

                   ("Kill "

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill top level" py-kill-top-level
                     :help " `py-kill-top-level'

Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill minor block" py-kill-minor-block
                     :help " `py-kill-minor-block'

Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill def-or-class" py-kill-def-or-class
                     :help "`py-kill-def-or-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill expression" py-kill-expression
                     :help "`py-kill-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill partial-expression" py-kill-partial-expression
                     :help "`py-kill-partial-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill class" py-kill-class
                     :help "`py-kill-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill def" py-kill-def
                     :help "`py-kill-def'
Delete innermost compound statement at point, store deleted string in kill-ring"])

                   ("Delete"
                    ["Delete statement " py-delete-statement
                     :help "`py-delete-statement'
Delete STATEMENT at point, don't store in kill-ring. "]

                    ["Delete top-level " py-delete-top-level
                     :help "`py-delete-top-level'
Delete TOP-LEVEL at point, don't store in kill-ring. "]

                    ["Delete block " py-delete-block
                     :help "`py-delete-block'
Delete BLOCK at point, don't store in kill-ring. "]

                    ["Delete block-or-clause " py-delete-block-or-clause
                     :help "`py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point, don't store in kill-ring. "]

                    ["Delete def " py-delete-def
                     :help "`py-delete-def'
Delete DEF at point, don't store in kill-ring. "]

                    ["Delete class " py-delete-class
                     :help "`py-delete-class'
Delete CLASS at point, don't store in kill-ring. "]

                    ["Delete def-or-class " py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete DEF-OR-CLASS at point, don't store in kill-ring. "]

                    ["Delete expression " py-delete-expression
                     :help "`py-delete-expression'
Delete EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete partial-expression " py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete minor-block " py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete MINOR-BLOCK at point, don't store in kill-ring.

A minor block is started by a `for', `if', `try' or `with'. "])
                   "-"

                   ("Shift right "
                    ["Shift block right" py-shift-block-right
                     :help "`py-shift-block-right'
Shift block right. "]

                    ["Shift clause right" py-shift-clause-right
                     :help "`py-shift-clause-right'
Shift clause right. "]

                    ["Shift statement right" py-shift-statement-right
                     :help "`py-shift-statement-right'
Shift statement right. "]

                    ["Shift minor block right" py-shift-minor-block-right
                     :help " `py-shift-minor-block-right'

Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

                    ["Shift def-or-class right" py-shift-def-or-class-right
                     :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

                    ["Shift class right" py-shift-class-right
                     :help "`py-shift-class-right'
Shift class right. "]

                    ["Shift def right" py-shift-def-right
                     :help "`py-shift-def-right'
Shift def right. "]

                    ["Shift block-or-clause right" py-shift-block-or-clause-right
                     :help "`py-shift-block-or-clause-right'
Shift block-or-clause right. "]

                    ["Shift region left" py-shift-region-left
                     :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                    ["Shift region right" py-shift-region-right
                     :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "])

                   ("Shift left "

                    ["Shift block left" py-shift-block-left
                     :help "`py-shift-block-left'
Shift block left. "]

                    ["Shift clause left" py-shift-clause-left
                     :help "`py-shift-clause-left'
Shift clause left. "]

                    ["Shift statement left" py-shift-statement-left
                     :help "`py-shift-statement-left'
Shift statement left. "]

                    ["Shift minor block left" py-shift-minor-block-left
                     :help " `py-shift-minor-block-left'

Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

                    ["Shift def-or-class left" py-shift-def-or-class-left
                     :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

                    ["Shift class left" py-shift-class-left
                     :help "`py-shift-class-left'
Shift class left. "]

                    ["Shift def left" py-shift-def-left
                     :help "`py-shift-def-left'
Shift def left. "]

                    ["Shift block-or-clause left" py-shift-block-or-clause-left
                     :help "`py-shift-block-or-clause-left'
Shift block-or-clause left. "])
                   ("More"
                    :help "extended edit commands'"

                    ["Kill buffer unconditional" py-kill-buffer-unconditional
                     :help " `py-kill-buffer-unconditional'

Kill buffer unconditional, kill buffer-process if existing\. "]

                    ["Empty out list backward" py-empty-out-list-backward
                     :help " `py-empty-out-list-backward'
Deletes all elements from list before point. "]

                    ["Revert boolean assignent" py-boolswitch
                     :help " `py-boolswitch'
Edit the assigment of a boolean variable, rever them.

I.e. switch it from \"True\" to \"False\" and vice versa "]

                    ["Remove overlays at point" py-remove-overlays-at-point
                     :help " `py-remove-overlays-at-point'

Remove overlays as set when `py-highlight-error-source-p' is non-nil. "]))

                  "-"
                  ("Forms "
                   ("Comment"

                    ["Beginning of comment" py-beginning-of-comment
                     :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                    ["End of comment" py-end-of-comment
                     :help " `py-end-of-comment'

Go to end of comment at point. "])
                   ("Block"
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

                    ["Comment block" py-comment-block
                     :help " `py-comment-block'

Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Top-level form"

                    ["Beginning of top-level form" py-beginning-of-top-level
                     :help "`py-beginning-of-top-level'
Go to start of top-level form form at point"]

                    ["End of top-level form" py-end-of-top-level
                     :help "`py-end-of-top-level'
Go to end of top-level form at point"]

                    ["Down top-level form" py-down-top-level
                     :help "`py-down-top-level'

Go to the beginning of top-level form below in buffer. "]

                    ["Up top-level form" py-up-top-level
                     :help "`py-up-top-level'

Go upwards to the beginning of next top-level form in buffer. "]

                    ["Copy top-level form" py-copy-top-level
                     :help "`py-copy-top-level'
Copy innermost top-level form at point"]

                    ["Kill top-level form" py-kill-top-level
                     :help "`py-kill-top-level'
Delete top-level form at point, store deleted string in kill-ring"]

                    ["Delete top-level form" py-delete-top-level
                     :help "`py-delete-top-level'
Delete top-level form at point, don't store deleted string in kill-ring"]

                    ["Comment top-level form" py-comment-top-level
                     :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Minor-block"

                    ["Beginning of minor-block" py-beginning-of-minor-block
                     :help "`py-beginning-of-minor-block'
Go to start of innermost minor-block at point"]
                    ["End of minor-block" py-end-of-minor-block
                     :help "`py-end-of-minor-block'
Go to end of innermost minor-block at point"]

                    ["Down minor-block" py-down-minor-block
                     :help "`py-down-minor-block'

Go to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Up minor-block" py-up-minor-block
                     :help "`py-up-minor-block'

Go upwards to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Copy minor-block" py-copy-minor-block
                     :help "`py-copy-minor-block'
Copy innermost minor-block at point"]

                    ["Kill minor-block" py-kill-minor-block
                     :help "`py-kill-minor-block'
Delete innermost minor-block at point, store deleted string in kill-ring"]

                    ["Delete minor-block" py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete innermost minor-block at point, don't store deleted string in kill-ring"]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left. "]

                    ["Comment minor-block" py-comment-minor-block
                     :help " `py-comment-minor-block'

Comments minor-block at point.

Uses double hash (`#') comment starter when `py-minor-block-comment-prefix-p' is `t',
the default. "])

                   ("Def-or-class "

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

                    ["Comment def or class" py-comment-def-or-class
                     :help " `py-comment-def-or-class'

Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Clause "

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

                    ["Comment clause" py-comment-clause
                     :help " `py-comment-clause'

Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Statement "

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

                    ["Comment statement" py-comment-statement
                     :help " `py-comment-statement'

Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Expression"

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
Delete expression at point, don't store deleted string in kill-ring"])

                   ("Partial expression"

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
Delete partial-expression at point, don't store deleted string in kill-ring"])

                   ("Class "

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

                    ["Comment class" py-comment-class
                     :help " `py-comment-class'

Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Def "

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

                    ["Comment def" py-comment-def
                     :help " `py-comment-def'

Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   "-"

                   ("Block bol "

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
Shift block left. "])

                   ("Minor-block bol "

                    ["Beginning of minor-block bol" py-beginning-of-minor-block-bol
                     :help "`py-beginning-of-minor-block-bol'
Go to beginning of line at beginning of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["End of minor-block bol" py-end-of-minor-block-bol
                     :help "`py-end-of-minor-block-bol'
Go to beginning of line following end of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Up minor-block bol" py-up-minor-block-bol
                     :help "`py-up-minor-block-bol'
Go to next minor-block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Down minor-block bol" py-down-minor-block-bol
                     :help "`py-down-minor-block-bol'
Go to next minor-block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Mark minor-block bol" py-mark-minor-block-bol
                     :help "`py-mark-minor-block-bol'
Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Copy minor-block bol" py-copy-minor-block-bol
                     :help "`py-copy-minor-block-bol'
Copy minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Kill minor-block bol" py-kill-minor-block-bol
                     :help "`py-kill-minor-block-bol'
Kill minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Delete minor-block bol" py-delete-minor-block-bol
                     :help "`py-delete-minor-block-bol'
Delete minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left.

A minor block is started by a `for', `if', `try' or `with'. "])

                   ("Clause bol "

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
Shift clause left. "])

                   ("Block-Or-Clause bol "

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
Shift block-or-clause left. "])

                   ("Def bol "

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
Shift def left. "])

                   ("Class bol "
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
Shift class left. "])

                   ("Def-Or-Class bol "
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
Shift def-or-class left. "])

                   ("Statement bol "
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
Shift statement left. "]))
                  "-"
                  ("Filling"
                   :help "see also customizable `py-docstring-style'"

                   ["Fill string" py-fill-string
                    :help " `py-fill-string'

Uses value of `py-docstring-style', if set. "]

                   ["Fill paragraph" py-fill-paragraph
                    :help " `py-fill-paragraph'

Uses value of `py-docstring-style', if set. "]

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

See available styles at `py-fill-paragraph' or var `py-docstring-style'
 "]

                   ["py fill string onetwo" py-fill-string-onetwo
                    :help " `py-fill-string-onetwo'
One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257" py-fill-string-pep-257
                    :help " `py-fill-string-pep-257'

PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257 nn" py-fill-string-pep-257-nn
                    :help " `py-fill-string-pep-257-nn'

PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string symmetric" py-fill-string-symmetric
                    :help " `py-fill-string-symmetric'

Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"])

                  ("Electric "
                   :help "electric commands'"

                   ["Hungry delete backwards" py-hungry-delete-backwards
                    :help " `py-hungry-delete-backwards'

Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also C-c <delete>. "]

                   ["Hungry delete forward" py-hungry-delete-forward
                    :help " `py-hungry-delete-forward'

Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>. "]

                   ["Electric colon" py-electric-colon
                    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "]

                   ["Electric colon greedy "
                    (setq py-electric-colon-greedy-p
                          (not py-electric-colon-greedy-p))
                    :help "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-greedy-p]

                   ["Electric colon newline and indent "
                    (setq py-electric-colon-newline-and-indent-p
                          (not py-electric-colon-newline-and-indent-p))
                    :help "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-newline-and-indent-p]

                   ["Electric delete" py-electric-delete
                    :help " `py-electric-delete'
Delete following character or levels of whitespace.

With ARG do that ARG times. "]

                   ["Electric backspace" py-electric-backspace
                    :help " `py-electric-backspace'
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "]

                   ["Electric comment" py-electric-comment
                    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment. "]

                   ["Electric left paren" py-complete-electric-lparen
                    :help " `py-complete-electric-lparen'
electricly insert '(', and try to get a signature for the stuff to the left.\n
Needs Pymacs"]

                   ["Complete electric comma" py-complete-electric-comma
                    :help " `py-complete-electric-comma'
electricly insert ',', and redisplay latest signature.\n
Needs Pymacs"]

                   ["Electric yank" py-electric-yank
                    :help " `py-electric-yank'
Perform command `yank' followed by an `indent-according-to-mode' . "])

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

                  ("Completion"
                   :help "Completion options"

		   ["Auto complete mode"
		    (setq py-auto-complete-p
			  (not py-auto-complete-p))
		    :help "Auto complete mode

Use `M-x customize-variable' to set it permanently"
		    :style toggle :selected py-auto-complete-p]

		   ["Indent or complete" py-indent-or-complete
		    :help " `py-indent-or-complete'

Complete or indent depending on the context\.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

Use `C-q TAB' to insert a literally TAB-character "]

                   ["Complete symbol" py-shell-complete
                    :help "`py-shell-complete'
Complete (qualified) symbol before point"]

                   ["Complete" py-complete
                    :help " `py-complete'
Complete symbol before point using Pymacs . "])

                  ["Find function" py-find-function
                   :help "`py-find-function'
Try to find source definition of function at point"]))))
        map))

;; avoid errors from ipython.el - which isn't needed anymore
(defvaralias 'py-mode-map 'python-mode-map)

(defvar py-shell-mode-map nil)
(setq py-shell-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "RET") 'comint-send-input)
        (define-key map [(control c)(-)] 'py-up-exception)
        (define-key map [(control c)(=)] 'py-down-exception)
	(define-key map (kbd "TAB") 'py-indent-or-complete)
	(define-key map [(meta tab)] 'py-shell-complete)
	(define-key map [(control c)(!)] 'py-shell)
	(define-key map [(control c)(control t)] 'py-toggle-shell)
        ;; electric keys
        (define-key map [(:)] 'py-electric-colon)
        (define-key map [(\#)] 'py-electric-comment)
        (define-key map [(delete)] 'py-electric-delete)
        (define-key map [(backspace)] 'py-electric-backspace)
        (define-key map [(control backspace)] 'py-hungry-delete-backwards)
        (define-key map [(control c) (delete)] 'py-hungry-delete-forward)
        ;; (define-key map [(control y)] 'py-electric-yank)
        ;; moving point
        (define-key map [(control c)(control p)] 'py-beginning-of-statement)
        (define-key map [(control c)(control n)] 'py-end-of-statement)
        (define-key map [(control c)(control u)] 'py-beginning-of-block)
        (define-key map [(control c)(control q)] 'py-end-of-block)
        (define-key map [(control meta a)] 'py-beginning-of-def-or-class)
        (define-key map [(control meta e)] 'py-end-of-def-or-class)
        (define-key map [(control j)] 'py-newline-and-indent)
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
        (define-key map [(control meta h)] 'py-mark-def-or-class)
        (define-key map [(control c)(control k)] 'py-mark-block-or-clause)
        (define-key map [(control c)(.)] 'py-expression)
        ;; Miscellaneous
        (define-key map [(super q)] 'py-copy-statement)
        (define-key map [(control c)(control d)] 'py-pdbtrack-toggle-stack-tracking)
        (define-key map [(control c)(\#)] 'py-comment-region)
        (define-key map [(control c)(\?)] 'py-describe-mode)
        (define-key map [(control c)(control e)] 'py-help-at-point)
        (define-key map [(control x) (n) (d)] 'py-narrow-to-defun)
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

        (and (ignore-errors (require 'easymenu) t)
             ;; (easy-menu-define py-menu map "Python Tools"
             ;;           `("PyTools"
             (easy-menu-define
               py-shell-menu map "Py-Shell menu"
               `("Py-Shell"
                 ("Interpreter"

		  ["Comint send input" comint-send-input
		   :help " `comint-send-input'

Send input to process\.
After the process output mark, sends all text from the process mark to
point as input to the process\.  Before the process output mark, calls
value of variable `comint-get-old-input' to retrieve old input, copies
it to the process mark, and sends it\.

This command also sends and inserts a final newline, unless
NO-NEWLINE is non-nil\.

Any history reference may be expanded depending on the value of the variable
`comint-input-autoexpand'\.  The list of function names contained in the value
of `comint-input-filter-functions' is called on the input before sending it\.
The input is entered into the input history ring, if the value of variable
`comint-input-filter' returns non-nil when called on the input\.

\(fn &optional NO-NEWLINE ARTIFICIAL)"]

		  ["Up exception" py-up-exception
		   :help " `py-up-exception'

Go to the previous line up in the traceback\.
With C-u (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack\."]

		  ["Down exception" py-down-exception
		   :help " `py-down-exception'

Go to the next line down in the traceback\.
With M-x univeral-argument (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack\."]

		  ["Default interpreter..." py-shell
                   :help " `py-shell'

Start an interactive Python interpreter.

Interactively, C-u 4 prompts for a buffer.
C-u 2 prompts for `py-python-command-args'.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

. "]
                  ("Other"
                   :help "Alternative Python Shells"

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

                   ["python3.3" python3.3
                    :help "`python3.3'
Start an Python3.3 interpreter.

Optional C-u prompts for options to pass to the Python3.3 interpreter. See `py-python-command-args'."]

                   ["python3.4" python3.4
                    :help "`python3.3'
Start an Python3.4 interpreter.

Optional C-u prompts for options to pass to the Python3.4 interpreter. See `py-python-command-args'."]

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

                   )
                  "-"

                  ["Kill shell unconditional" py-kill-shell-unconditional
                   :help " `py-kill-shell-unconditional'

With optional argument SHELL\.

Otherwise kill default (I)Python shell\.
Kill buffer and its process.
Receives a buffer-name as argument "]

                  ["Kill default shell unconditional" py-kill-default-shell-unconditional
                   :help " `py-kill-default-shell-unconditional'

Kill buffer \"*Python*\" and its process\.  "])
		                   ("Completion"
                   :help "Completion options"

		   ["Indent or complete" py-indent-or-complete
		    :help " `py-indent-or-complete'

Complete or indent depending on the context\.

If cursor is at end of line, try to complete
Otherwise call `py-indent-line'

Use `C-q TAB' to insert a literally TAB-character "]

                   ["Complete symbol" py-shell-complete
                    :help "`py-shell-complete'
Complete (qualified) symbol before point"]

                   ["Complete" py-complete
                    :help " `py-complete'
Complete symbol before point using Pymacs . "]
		   )

                 "-"
                 ("Mark"

                  ["Mark block" py-mark-block
                   :help " `py-mark-block'

Mark block at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark minor block" py-mark-minor-block
                   :help " `py-mark-minor-block'

Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Returns beginning and end positions of marked area, a cons. "]

                  ["Mark def or class" py-mark-def-or-class
                   :help " `py-mark-def-or-class'

Mark def-or-class at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark statement" py-mark-statement
                   :help "`py-mark-statement'
Mark statement at point"]

                  ["Mark top level" py-mark-top-level
                   :help " `py-mark-top-level'

Mark top-level form at point.

Returns beginning and end positions of marked area, a cons. "]

                  ["Mark clause" py-mark-clause
                   :help "`py-mark-clause'
Mark innermost compound statement at point"]

                  ["Mark def" py-mark-def
                   :help "`py-mark-def'
Mark innermost definition at point"]

                  ["Mark expression" py-mark-expression
                   :help "`py-mark-expression'
Mark expression at point"]

                  ["Mark partial expression" py-mark-partial-expression
                   :help "`py-mark-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]

                  ["Mark class" py-mark-class
                   :help "`py-mark-class'
Mark innermost definition at point"]

                  ["Mark comment" py-mark-comment
                   :help "`py-mark-comment'
Mark commented section at point"]

                  ("BOL forms"

		   ["Mark block bol" py-mark-block-bol
		    :help "`py-mark-block-bol'
Mark block at point reaching beginning-of-line. "]

		   ["Mark clause bol" py-mark-clause-bol
		    :help "`py-mark-clause-bol'
Mark clause at point reaching beginning-of-line. "]

		   ["Mark block-or-clause bol" py-mark-block-or-clause-bol
		    :help "`py-mark-block-or-clause-bol'
Mark block-or-clause at point reaching beginning-of-line. "]

		   ["Mark def bol" py-mark-def-bol
		    :help "`py-mark-def-bol'
Mark def at point reaching beginning-of-line. "]

		   ["Mark class bol" py-mark-class-bol
		    :help "`py-mark-class-bol'
Mark class at point reaching beginning-of-line. "]

		   ["Mark def-or-class bol" py-mark-def-or-class-bol
		    :help "`py-mark-def-or-class-bol'
Mark def-or-class at point reaching beginning-of-line. "]

		   ["Mark if-block bol" py-mark-if-block-bol
		    :help "`py-mark-if-block-bol'
Mark if-block at point reaching beginning-of-line. "]

		   ["Mark try-block bol" py-mark-try-block-bol
		    :help "`py-mark-try-block-bol'
Mark try-block at point reaching beginning-of-line. "]

		   ["Mark minor-block bol" py-mark-minor-block-bol
		    :help "`py-mark-minor-block-bol'

Mark minor-block at point reaching beginning-of-line.
A minor block is started by a `for', `if', `try' or `with'."]))

		 "-"

                 ["Shift region left" py-shift-region-left
                  :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                 ["Shift region right" py-shift-region-right
                  :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "]

                 "-"

                 ("Comment"
                  ["Comment Region"   py-comment-region (point) (mark)
                   :help "Like `comment-region' but uses double hash (`#') comment starter." ]
                  ["Uncomment" py-uncomment
                   :help " `py-uncomment'

Uncomment commented lines at point.

If region is active, restrict uncommenting at region . "]

                  ["Uncomment Region"     (py-comment-region (point) (mark) '(4))
                   :help "(py-comment-region (point) (mark) '(4))" ]
                  "-"
                  ["Comment block" py-comment-block
                   :help " `py-comment-block'
Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment minor-block" py-comment-minor-block
                   :help " `py-comment-minor-block'
Comments minor-block at point.

A minor block is started by a `for', `if', `try' or `with'.
Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment top level" py-comment-top-level
                   :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment clause" py-comment-clause
                   :help " `py-comment-clause'
Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment block or clause" py-comment-block-or-clause
                   :help " `py-comment-block-or-clause'
Comments block-or-clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def" py-comment-def
                   :help " `py-comment-def'
Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment class" py-comment-class
                   :help " `py-comment-class'
Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment def or class" py-comment-def-or-class
                   :help " `py-comment-def-or-class'
Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "]

                  ["Comment statement" py-comment-statement
                   :help " `py-comment-statement'
Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                 "-"

                 ("Move"

		  ["Backward same level" py-backward-same-level
		   :help " `py-backward-same-level'

Go form backward keeping indent level if possible\.

If inside a delimited form --string or list-- go to its beginning\.
If not at beginning of a statement or block, go to its beginning\.
If at beginning of a statement or block, go to previous beginning of compound statement or definition at point\.
If no further element at same level, go one level up. "]

                  ["Beginning of block" py-beginning-of-block
                   :help " `py-beginning-of-block'

Go to beginning block, skip whitespace at BOL. "]

                  ["Go to end of block" py-end-of-block]

                  "-"

                  ["Beginning of def or class" py-beginning-of-def-or-class
                   :help " `py-beginning-of-def-or-class'

Go to beginning def-or-class, skip whitespace at BOL.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too. "]

                  ["End of def or class" py-end-of-def-or-class
                   :help " `py-end-of-def-or-class'

Go to end of def-or-class.

Returns end of def-or-class if successful, nil otherwise

With M-x universal argument or `py-mark-decorators' set to `t', decorators are marked too. "]

                  "-"

                  ["Beginning of statement" py-beginning-of-statement
                   :help " `py-beginning-of-statement'

Go to the initial line of a simple statement. "]

                  ["End of statement" py-end-of-statement
                   :help " `py-end-of-statement'

Go to the last char of current statement.

To go just beyond the final line of the current statement, use `py-down-statement-bol'. "]

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

                  ("BOL forms"
                   ("Beginning"

                    ["Beginning of block bol" py-beginning-of-block-bol
                     :help " `py-beginning-of-block-bol'

Go to beginning block, go to beginning-of-line\.

Returns beginning of block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of clause bol" py-beginning-of-clause-bol
                     :help " `py-beginning-of-clause-bol'

Go to beginning clause, go to beginning-of-line\.

Returns beginning of clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of block or clause bol" py-beginning-of-block-or-clause-bol
                     :help " `py-beginning-of-block-or-clause-bol'

Go to beginning block-or-clause, go to beginning-of-line\.

Returns beginning of block-or-clause if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def bol" py-beginning-of-def-bol
                     :help " `py-beginning-of-def-bol'

Go to beginning def, go to beginning-of-line\.

Returns beginning of def if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of class bol" py-beginning-of-class-bol
                     :help " `py-beginning-of-class-bol'

Go to beginning class, go to beginning-of-line\.

Returns beginning of class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of def or class bol" py-beginning-of-def-or-class-bol
                     :help " `py-beginning-of-def-or-class-bol'

Go to beginning def-or-class, go to beginning-of-line\.

Returns beginning of def-or-class if successful, nil otherwise

When `py-mark-decorators' is non-nil, decorators are considered too\.

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of if block bol" py-beginning-of-if-block-bol
                     :help " `py-beginning-of-if-block-bol'

Go to beginning if-block, go to beginning-of-line\.

Returns beginning of if-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of try block bol" py-beginning-of-try-block-bol
                     :help " `py-beginning-of-try-block-bol'

Go to beginning try-block, go to beginning-of-line\.

Returns beginning of try-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of minor block bol" py-beginning-of-minor-block-bol
                     :help " `py-beginning-of-minor-block-bol'

Go to beginning minor-block, go to beginning-of-line\.

Returns beginning of minor-block if successful, nil otherwise

Referring python program structures see for example:
http://docs\.python\.org/reference/compound_stmts\.html "]

                    ["Beginning of statement bol" py-beginning-of-statement-bol
                     :help " `py-beginning-of-statement-bol'

Goto beginning of line where statement starts\.
  Returns position reached, if successful, nil otherwise\.

See also `py-up-statement': up from current definition to next beginning of statement above\.  "])
                   ("End"

                    ["End of block bol" py-end-of-block-bol
                     :help " `py-end-of-block-bol'

Goto beginning of line following end of block\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block': down from current definition to next beginning of block below\.  "]

                    ["End of clause bol" py-end-of-clause-bol
                     :help " `py-end-of-clause-bol'

Goto beginning of line following end of clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-clause': down from current definition to next beginning of clause below\.  "]

                    ["End of block or clause bol" py-end-of-block-or-clause-bol
                     :help " `py-end-of-block-or-clause-bol'

Goto beginning of line following end of block-or-clause\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-block-or-clause': down from current definition to next beginning of block-or-clause below\.  "]

                    ["End of def bol" py-end-of-def-bol
                     :help " `py-end-of-def-bol'

Goto beginning of line following end of def\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def': down from current definition to next beginning of def below\.  "]

                    ["End of class bol" py-end-of-class-bol
                     :help " `py-end-of-class-bol'

Goto beginning of line following end of class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-class': down from current definition to next beginning of class below\.  "]

                    ["End of def or class bol" py-end-of-def-or-class-bol
                     :help " `py-end-of-def-or-class-bol'

Goto beginning of line following end of def-or-class\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-def-or-class': down from current definition to next beginning of def-or-class below\.  "]

                    ["End of if block bol" py-end-of-if-block-bol
                     :help " `py-end-of-if-block-bol'

 "]

                    ["End of try block bol" py-end-of-try-block-bol
                     :help " `py-end-of-try-block-bol'

 "]

                    ["End of minor block bol" py-end-of-minor-block-bol
                     :help " `py-end-of-minor-block-bol'

 "]

                    ["End of statement bol" py-end-of-statement-bol
                     :help " `py-end-of-statement-bol'

Goto beginning of line following end of statement\.
  Returns position reached, if successful, nil otherwise\.

See also `py-down-statement': down from current definition to next beginning of statement below\.  "]))

                  "-"

                  ("More"

                   ["Up level" py-up
                    :help " `py-up'
Go to beginning one level above of compound statement or definition at point. "]

                   ["Down level" py-down
                    :help " `py-down'
Go to beginning one level below of compound statement or definition at point. "]

                   "-"

                   ["Beginning of top level" py-beginning-of-top-level
                    :help " `py-beginning-of-top-level'

Go to the very beginning of top-level form at point. "]

                   ["End of top level" py-end-of-top-level
                    :help " `py-end-of-top-level'

Go to end of top-level form at point. "]

                   "-"

                   ["Beginning of block current-column" py-beginning-of-block-current-column
                    :help " `py-beginning-of-block-current-column'

Reach next beginning of block upwards which starts at current column.

Return position. "]

                   "-"

                   ["Move to start of def" py-beginning-of-def t]

                   ["Move to end of def"   py-end-of-def t]

                   "-"

                   ["Beginning of clause" py-beginning-of-clause
                    :help " `py-beginning-of-clause'

Go to beginning clause, skip whitespace at BOL. "]

                   ["End of clause" py-end-of-clause
                    :help " `py-end-of-clause'

Go to end of clause. "]

                   "-"

                   ["Beginning of comment" py-beginning-of-comment
                    :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                   ["End of comment" py-end-of-comment
                    :help " `py-end-of-comment'

Go to end of comment at point. "]

                   "-"

                   ["Go to start of expression" (py-beginning-of-expression t) t]
                   ["Move to end of expression" (py-end-of-expression t) t]

                   "-"

                   ["Go to start of minor-expression" (py-beginning-of-minor-expression t) t]

                   ["Move to end of minor-expression" (py-end-of-minor-expression t) t]
                   "-"

                   ["Beginning of minor block" py-beginning-of-minor-block
                    :help " `py-beginning-of-minor-block'

Go to beginning minor-block, skip whitespace at BOL.

Returns beginning of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'.

"]

                   ["End of minor block" py-end-of-minor-block
                    :help " `py-end-of-minor-block'

Go to end of minor-block.

Returns end of minor-block if successful, nil otherwise

A minor block is started by a `for', `if', `try' or `with'. "]))

                 "-"

                 ("Copy "
                  ["Copy statement" py-copy-statement
                   :help "`py-copy-statement'
Copy statement at point"]

                  ["Copy top level" py-copy-top-level
                   :help " `py-copy-top-level'

Copy top-level form at point. "]

                  ["Copy clause" py-copy-clause
                   :help "`py-copy-clause'
Copy innermost clause at point"]

                  ["Copy block" py-copy-block
                   :help "`py-copy-block'
Copy innermost block at point"]

                  ["Copy minor block" py-copy-minor-block
                   :help " `py-copy-minor-block'

Copy minor-block at point.

Store data in kill ring, so it might yanked back.
A minor block is started by a `for', `if', `try' or `with'. "]

                  ["Copy def" py-copy-def
                   :help "`py-copy-def'
Copy innermost definition at point"]
                  ["Copy expression" py-copy-expression
                   :help "`py-copy-expression'
Copy expression at point"]
                  ["Copy partial expression" py-copy-partial-expression
                   :help "`py-copy-partial-expression'
\".\" operators delimit a partial-expression expression on it's level"]
                  ["Copy class" py-copy-class
                   :help "`py-copy-class'
Copy innermost definition at point"]

                  ["Copy Def-or-Class" py-copy-def-or-class
                   :help "`py-copy-def-or-class'
Copy innermost definition at point"]

                  ("BOL forms"

                   ["Copy block bol" py-copy-block-bol
                    :help " `py-copy-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy clause bol" py-copy-clause-bol
                    :help " `py-copy-clause-bol'

Delete clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy block or clause bol" py-copy-block-or-clause-bol
                    :help " `py-copy-block-or-clause-bol'

Delete block-or-clause, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def bol" py-copy-def-bol
                    :help " `py-copy-def-bol'

Delete def, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy class bol" py-copy-class-bol
                    :help " `py-copy-class-bol'

Delete class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy def or class bol" py-copy-def-or-class-bol
                    :help " `py-copy-def-or-class-bol'

Delete def-or-class, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy statement bol" py-copy-statement-bol
                    :help " `py-copy-statement-bol'

Delete statement, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.  "]

                   ["Copy minor block bol" py-copy-minor-block-bol
                    :help " `py-copy-minor-block-bol'

Delete block, use position from beginning-of-line\.

Stores data in kill ring\. Might be yanked back using `C-y'\.

See `py-minor-block-re' "]))

		 ("Hide-Show"

		  ["Hide region" py-hide-region
		   :help " `py-hide-region'

Hide active region\. "]

		  ["Hide statement" py-hide-statement
		   :help " `py-hide-statement'

Hide statement at point\. "]

		  ["Hide block" py-hide-block
		   :help " `py-hide-block'

Hide block at point\. "]

		  ["Hide clause" py-hide-clause
		   :help " `py-hide-clause'

Hide clause at point\. "]

		  ["Hide block or clause" py-hide-block-or-clause
		   :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		  ["Hide def" py-hide-def
		   :help " `py-hide-def'

Hide def at point\. "]

		  ["Hide class" py-hide-class
		   :help " `py-hide-class'

Hide class at point\. "]

		  ["Hide expression" py-hide-expression
		   :help " `py-hide-expression'

Hide expression at point\. "]

		  ["Hide partial expression" py-hide-partial-expression
		   :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		  ["Hide line" py-hide-line
		   :help " `py-hide-line'

Hide line at point\. "]

		  ["Hide top level" py-hide-top-level
		   :help " `py-hide-top-level'

Hide top-level at point\. "]

		  ("Show"

		   ["Show region" py-show-region
		    :help " `py-show-region'

Un-hide active region\. "]

		   ["Show statement" py-show-statement
		    :help " `py-show-statement'

Show statement at point\. "]

		   ["Show block" py-show-block
		    :help " `py-show-block'

Show block at point\. "]

		   ["Show clause" py-show-clause
		    :help " `py-show-clause'

Show clause at point\. "]

		   ["Show block or clause" py-show-block-or-clause
		    :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		   ["Show def" py-show-def
		    :help " `py-show-def'

Show def at point\. "]

		   ["Show class" py-show-class
		    :help " `py-show-class'

Show class at point\. "]

		   ["Show expression" py-show-expression
		    :help " `py-show-expression'

Show expression at point\. "]

		   ["Show partial expression" py-show-partial-expression
		    :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		   ["Show line" py-show-line
		    :help " `py-show-line'

Show line at point\. "]

		   ["Show top level" py-show-top-level
		    :help " `py-show-top-level'

Show top-level at point\. "]))

                 "-"

                 ("Virtualenv"

                  ["Virtualenv workon" virtualenv-workon
                   :help " runs `virtualenv-workon'

Make sure virtualenv is provided

"]

                  ["Virtualenv activate" virtualenv-activate
                   :help " `virtualenv-activate'

Activate the virtualenv located in DIR. "]

                  ["Virtualenv deactivate" virtualenv-deactivate
                   :help " `virtualenv-deactivate'

Deactivate the current virtual enviroment. "]

                  ["Virtualenv p" virtualenv-p
                   :help " `virtualenv-p'

Check if a directory is a virtualenv. "])

                 ("Help"

		  ["Find definition" py-find-definition
		   :help " `py-find-definition'

Find source of definition of SYMBOL\.

Interactively, prompt for SYMBOL\."]

		  ["Imenu" imenu
		   :help " `imenu'

Jump to a INDEX-ITEM "]

                  ["Info lookup symbol" py-info-lookup-symbol
                   :help " `py-info-lookup-symbol'

Calls `info-lookup-symbol'.

Sends help if stuff is missing. "]

                  ["Symbol at point" py-symbol-at-point
                   :help " `py-symbol-at-point'

Return the current Python symbol\. "]

		  "-"

                  ["Describe mode"        py-describe-mode t]

                  ["Help on symbol" py-help-at-point
                   :help "`py-help-at-point'\n
Use pydoc on symbol at point"]

		  )

                 ("Debugger"

                  ["pdb" pdb
		   :help "`pdb' Run pdb under GUD"]

                 ("Checks"

                  ["pychecker-run" py-pychecker-run
                   :help "`py-pychecker-run'
Run pychecker

Call `easy_install pyflakes' resp. `pip... 'if not available"]

                  ("Pylint "
                   :help "Extendet report options

Call `easy_install pylint' resp. `pip...' if not available"

                   ["py-pylint-run" py-pylint-run
                    :help "`py-pylint-run'
Pylint will display a number of messages as it analyzes the code,
as well as some statistics about the number of warnings and
errors found in different files - unless called with arg \"--errors-only\". The messages are classified
under various categories such as errors and warnings

Pylint checks length of lines of code, if variable names are
well-formed according to your coding standard, if declared
interfaces are truly implemented, and much more. Additionally, it
is possible to write plugins.

Call `easy_install pylint' resp. `pip...' if not available
"]

                   ["py-pylint-help" py-pylint-help
                    :help "`py-pylint-help'
List extendet report options
"]
                   ["pylint-flymake-mode" pylint-flymake-mode
                    :help "`pylint-flymake-mode'
Toggle flymake-mode running `pylint'
"])

                  ("pep8 "
                   :help "Check formatting

Call `easy_install pep8' resp. `pip...' if not available"

                   ["pep8-run" py-pep8-run
                    :help "`py-pep8-run'
Check formatting (default on the file currently visited)

Call `easy_install pep8' resp. `pip...' if not available
"]

                   ["pep8-help" py-pep8-help
                    :help "`py-pep8-help'
Display help for pep8 format checker)
"]

                   ["pep8-flymake-mode" pep8-flymake-mode
                    :help "`pep8-flymake-mode'
Toggle flymake-mode running `pep8'
"])

                  ("Pyflakes " :help "Non intrusive code checker

Call `easy_install pyflakes' resp. `pip...' if not available"

                   ["pyflakes-run" py-pyflakes-run :help
                    "`py-pyflakes-run' Run pyflakes

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakes-help" py-pyflakes-help :help
                    "`py-pyflakes-help' Display help for
              Pyflakes "]

                   ["pyflakes-flymake-mode" pyflakes-flymake-mode :help
                    "`pyflakes-flymake-mode'
Toggle flymake-mode running `pyflakes' "])

                  ("Flake8 " :help
                   "code checker running "

                   ["Flake8 run" py-flake8-run
                    :help " `py-flake8-run'

        Flake8 is a wrapper around these tools:
        - PyFlakes
        - pep8
        - Ned Batchelder's McCabe script

        It also adds features:
        - files that contain this line are skipped::
            # flake8: noqa
        - lines that contain a ``# noqa`` comment at the end will not issue warnings.
        - a Git and a Mercurial hook.
        - a McCabe complexity checker.
        - extendable through ``flake8.extension`` entry points.

. "]

                   ["Flake8 help" py-flake8-help
                    :help " `py-flake8-help'

Display flake8 command line help messages. "])

                  ("Pyflakes-pep8 " :help
                   "Non intrusive code checker running `pyflakes' and `pep8'
call `easy_install pyflakes' resp. `pip...' and `easy_install pep8' if basics not available"

                   ["pyflakespep8-run" py-pyflakespep8-run :help
                    "`py-pyflakespep8-run' Run `pyflakespep8'

Call `easy_install pyflakes' resp. `pip...' if not available"]

                   ["pyflakespep8-help" py-pyflakespep8-help :help
                    "`py-pyflakespep8-help' Display help for
              Pyflakespep8 "]

                   ["pyflakespep8-flymake-mode" pyflakespep8-flymake-mode :help
                    "`pyflakespep8-flymake-mode'
Toggle flymake-mode running `pyflakespep8' "]))

                 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"
                   ("Interpreter"

                    ["Shell prompt read only"
                     (setq py-shell-prompt-read-only
                           (not py-shell-prompt-read-only))
                     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-shell-prompt-read-only]

                    ["Remove cwd from path"
                     (setq py-remove-cwd-from-path
                           (not py-remove-cwd-from-path))
                     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-remove-cwd-from-path]

                    ["Honor IPYTHONDIR "
                     (setq py-honor-IPYTHONDIR-p
                           (not py-honor-IPYTHONDIR-p))
                     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

                    ["Honor PYTHONHISTORY "
                     (setq py-honor-PYTHONHISTORY-p
                           (not py-honor-PYTHONHISTORY-p))
                     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-honor-PYTHONHISTORY-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "])

                   )

                   ("Edit"

                    ("Completion"

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found\. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Set Pymacs-based complete keymap "
                      (setq py-set-complete-keymap-p
                            (not py-set-complete-keymap-p))
                      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `py-shell-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-set-complete-keymap-p]

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Indent no completion "
                      (setq py-indent-no-completion-p
                            (not py-indent-no-completion-p))
                      :help "If completion function should indent when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-no-completion-p]

                     ["Company pycomplete "
                      (setq py-company-pycomplete-p
                            (not py-company-pycomplete-p))
                      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-company-pycomplete-p])

                    ("Autopair mode"
                     :help "Toggle `autopair-mode'"

                     ["Toggle autopair mode" toggle-py-autopair-mode
                      :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off.

  Returns value of `autopair-mode ' switched to. "]

                     ["Autopair mode on" py-autopair-mode-on
                      :help " `autopair-mode on'

Make sure, `autopair-mode' is on.

Returns value of `autopair-mode'. "]

                     ["Autopair mode off" py-autopair-mode-off
                      :help " `autopair-mode' off

Make sure, `autopair-mode' is off.

Returns value of `autopair-mode'. "])

                    ;; py-smart-operator-mode-p forms
                    ("Smart operator mode"
                     :help "Toggle `smart-operator-mode'"

                     ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                      :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off.

  Returns value of `smart-operator-mode ' switched to. "]

                     ["Smart operator mode on" py-smart-operator-mode-p-on
                      :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on.

Returns value of `smart-operator-mode'. "]

                     ["Smart operator mode off" py-smart-operator-mode-p-off
                      :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off.

Returns value of `smart-operator-mode'. "])

                    ("Filling"

                     ("Docstring styles"
                      :help "Switch docstring-style"

                      ["Nil" py-set-nil-docstring-style
                       :help " `py-set-nil-docstring-style'

Set py-docstring-style to nil, format string normally. "]

                      ["pep-257-nn" py-set-pep-257-nn-docstring-style
                       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

                      ["pep-257" py-set-pep-257-docstring-style
                       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

                      ["django" py-set-django-docstring-style
                       :help " `py-set-django-docstring-style'

Set py-docstring-style to 'django "]

                      ["onetwo" py-set-onetwo-docstring-style
                       :help " `py-set-onetwo-docstring-style'

Set py-docstring-style to 'onetwo "]

                      ["symmetric" py-set-symmetric-docstring-style
                       :help " `py-set-symmetric-docstring-style'

Set py-docstring-style to 'symmetric "])

                     ["Auto-fill mode"
                      (setq py-auto-fill-mode
                            (not py-auto-fill-mode))
                      :help "Fill according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-auto-fill-mode])

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    ("Indent"
		     ("TAB related"

		      ["indent-tabs-mode"
		       (setq indent-tabs-mode
			     (not indent-tabs-mode))
		       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected indent-tabs-mode]

		      ["Tab indent"
		       (setq py-tab-indent
			     (not py-tab-indent))
		       :help "Non-nil means TAB in Python mode calls `py-indent-line'.Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indent]

		      ["Tab shifts region "
		       (setq py-tab-shifts-region-p
			     (not py-tab-shifts-region-p))
		       :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-shifts-region-p]

		      ["Tab indents region "
		       (setq py-tab-indents-region-p
			     (not py-tab-indents-region-p))
		       :help "When `t' and first TAB doesn't shift, indent-region is called.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indents-region-p])

                     ["Close at start column"
                      (setq py-closing-list-dedents-bos
                            (not py-closing-list-dedents-bos))
                      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-dedents-bos]

                     ["Closing list keeps space"
                      (setq py-closing-list-keeps-space
                            (not py-closing-list-keeps-space))
                      :help "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-keeps-space]

                     ["Closing list space"
                      (setq py-closing-list-space
                            (not py-closing-list-space))
                      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-space]

                     ["Tab shifts region "
                      (setq py-tab-shifts-region-p
                            (not py-tab-shifts-region-p))
                      :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-tab-shifts-region-p]

                     ["Lhs inbound indent"
                      (setq py-lhs-inbound-indent
                            (not py-lhs-inbound-indent))
                      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-lhs-inbound-indent]

                     ["Continuation offset"
                      (setq py-continuation-offset
                            (not py-continuation-offset))
                      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-continuation-offset]

                     ["Electric colon"
                      (setq py-electric-colon-active-p
                            (not py-electric-colon-active-p))
                      :help " `py-electric-colon-active-p'

`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
                      :style toggle :selected py-electric-colon-active-p]

                     ["Electric colon at beginning of block only"
                      (setq py-electric-colon-bobl-only
                            (not py-electric-colon-bobl-only))
                      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-colon-bobl-only]

                     ["Electric yank active "
                      (setq py-electric-yank-active-p
                            (not py-electric-yank-active-p))
                      :help " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-yank-active-p]

                     ["Electric kill backward "
                      (setq py-electric-kill-backward-p
                            (not py-electric-kill-backward-p))
                      :help "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string\[0:1]
--------------^

==>

my_string\[]
----------^

In result cursor is insided emptied delimited form.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-kill-backward-p]

                     ["Trailing whitespace smart delete "
                      (setq py-trailing-whitespace-smart-delete-p
                            (not py-trailing-whitespace-smart-delete-p))
                      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-trailing-whitespace-smart-delete-p]

                     ["Newline delete trailing whitespace "
                      (setq py-newline-delete-trailing-whitespace-p
                            (not py-newline-delete-trailing-whitespace-p))
                      :help "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-newline-delete-trailing-whitespace-p]

                     ["Dedent keep relative column"
                      (setq py-dedent-keep-relative-column
                            (not py-dedent-keep-relative-column))
                      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-dedent-keep-relative-column]

                     ["Indent paren spanned multilines "
                      (setq py-indent-paren-spanned-multilines-p
                            (not py-indent-paren-spanned-multilines-p))
                      :help "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-paren-spanned-multilines-p]

                     ["Indent honors multiline listing"
                      (setq py-indent-honors-multiline-listing
                            (not py-indent-honors-multiline-listing))
                      :help "If `t', indents to 1\+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-multiline-listing]

                     ["Indent comment "
                      (setq py-indent-comments
                            (not py-indent-comments))
                      :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-comments]

                     ["Uncomment indents "
                      (setq py-uncomment-indents-p
                            (not py-uncomment-indents-p))
                      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-uncomment-indents-p]

                     ["Indent honors inline comment"
                      (setq py-indent-honors-inline-comment
                            (not py-indent-honors-inline-comment))
                      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-inline-comment]

                     ["Kill empty line"
                      (setq py-kill-empty-line
                            (not py-kill-empty-line))
                      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-kill-empty-line]

                     ("Smart indentation"
                      :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

                      ["Toggle py-smart-indentation" toggle-py-smart-indentation
                       :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation on" py-smart-indentation-on
                       :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation off" py-smart-indentation-off
                       :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"])

                     ["Beep if tab change"
                      (setq py-beep-if-tab-change
                            (not py-beep-if-tab-change))
                      :help "Ring the bell if `tab-width' is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-beep-if-tab-change]

                     ["Highlight indentation" highlight-indentation
                      :help "Toggle highlight indentation.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is installed"

                      ]

                     ["Electric comment "
                      (setq py-electric-comment-p
                            (not py-electric-comment-p))
                      :help "If \"#\" should call `py-electric-comment'. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-p]

                     ["Electric comment add space "
                      (setq py-electric-comment-add-space-p
                            (not py-electric-comment-add-space-p))
                      :help "If py-electric-comment should add a space.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-add-space-p]

                     ["Empty line closes "
                      (setq py-empty-line-closes-p
                            (not py-empty-line-closes-p))
                      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-empty-line-closes-p])
                    ["Defun use top level "
                     (setq py-defun-use-top-level-p
                           (not py-defun-use-top-level-p))
                     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc. "
                     :style toggle :selected py-defun-use-top-level-p]

                    ["Close provides newline"
                     (setq py-close-provides-newline
                           (not py-close-provides-newline))
                     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-close-provides-newline]

                    ["Block comment prefix "
                     (setq py-block-comment-prefix-p
                           (not py-block-comment-prefix-p))
                     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-block-comment-prefix-p])

                   ("Display"

                    ("Index"

                     ["Imenu create index "
                      (setq py--imenu-create-index-p
                            (not py--imenu-create-index-p))
                      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py--imenu-create-index-p]

                     ["Imenu show method args "
                      (setq py-imenu-show-method-args-p
                            (not py-imenu-show-method-args-p))
                      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-imenu-show-method-args-p]
                     ["Switch index-function" py-switch-imenu-index-function
                      :help "`py-switch-imenu-index-function'
Switch between `py--imenu-create-index' from 5.1 series and `py--imenu-create-index-new'."])

                    ("Fontification"

                     ["Mark decorators"
                      (setq py-mark-decorators
                            (not py-mark-decorators))
                      :help "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-mark-decorators]

                     ["Fontify shell buffer "
                      (setq py-fontify-shell-buffer-p
                            (not py-fontify-shell-buffer-p))
                      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fontify-shell-buffer-p]

                     ["Use font lock doc face "
                      (setq py-use-font-lock-doc-face-p
                            (not py-use-font-lock-doc-face-p))
                      :help "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.

Call M-x `customize-face' in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-font-lock-doc-face-p])

                    ["Switch buffers on execute"
                     (setq py-switch-buffers-on-execute-p
                           (not py-switch-buffers-on-execute-p))
                     :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-switch-buffers-on-execute-p]

                    ["Split windows on execute"
                     (setq py-split-windows-on-execute-p
                           (not py-split-windows-on-execute-p))
                     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-p]

                    ["Keep windows configuration"
                     (setq py-keep-windows-configuration
                           (not py-keep-windows-configuration))
                     :help "If a windows is splitted displaying results, this is directed by variable `py-split-windows-on-execute-p'\. Also setting `py-switch-buffers-on-execute-p' affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting `py-keep-windows-configuration' to `t' will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-keep-windows-configuration]

                    ["Which split windows on execute function"
                     (progn
                       (if (eq 'split-window-vertically py-split-windows-on-execute-function)
                           (setq py-split-windows-on-execute-function'split-window-horizontally)
                         (setq py-split-windows-on-execute-function 'split-window-vertically))
                       (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

                     :help "If `split-window-vertically' or `...-horizontally'. Use `M-x customize-variable' RET `py-split-windows-on-execute-function' RET to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-function]

                    ["Modeline display full path "
                     (setq py-modeline-display-full-path-p
                           (not py-modeline-display-full-path-p))
                     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-display-full-path-p]

                    ["Modeline acronym display home "
                     (setq py-modeline-acronym-display-home-p
                           (not py-modeline-acronym-display-home-p))
                     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-acronym-display-home-p]

                    ["Hide show hide docstrings"
                     (setq py-hide-show-hide-docstrings
                           (not py-hide-show-hide-docstrings))
                     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-show-hide-docstrings]

                    ["Hide comments when hiding all"
                     (setq py-hide-comments-when-hiding-all
                           (not py-hide-comments-when-hiding-all))
                     :help "Hide the comments too when you do `hs-hide-all'. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-comments-when-hiding-all]

                    ["Max help buffer "
                     (setq py-max-help-buffer-p
                           (not py-max-help-buffer-p))
                     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-max-help-buffer-p]

                    ["Current defun show"
                     (setq py-current-defun-show
                           (not py-current-defun-show))
                     :help "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-current-defun-show]

                    ["Match paren mode"
                     (setq py-match-paren-mode
                           (not py-match-paren-mode))
                     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in py-shell-mode-map.
Customize `py-match-paren-key' which key to use. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-match-paren-mode])

                   ("Debug"

		    ["py-debug-p"
		     (setq py-debug-p
			   (not py-debug-p))
		     :help "When non-nil, keep resp\. store information useful for debugging\.

Temporary files are not deleted\. Other functions might implement
some logging etc\. Use `M-x customize-variable' to set it permanently"
		     :style toggle :selected py-debug-p]

                    ["Pdbtrack do tracking "
                     (setq py-pdbtrack-do-tracking-p
                           (not py-pdbtrack-do-tracking-p))
                     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-pdbtrack-do-tracking-p]

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]

                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p])

                   ("Other"

                    ("Directory"

                     ["Guess install directory "
                      (setq py-guess-py-install-directory-p
                            (not py-guess-py-install-directory-p))
                      :help "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-guess-py-install-directory-p]

                     ["Use local default"
                      (setq py-use-local-default
                            (not py-use-local-default))
                      :help "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-local-default]

                     ["Use current dir when execute "
                      (setq py-use-current-dir-when-execute-p
                            (not py-use-current-dir-when-execute-p))
                      :help "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-current-dir-when-execute-p]

                     ["Keep shell dir when execute "
                      (setq py-keep-shell-dir-when-execute-p
                            (not py-keep-shell-dir-when-execute-p))
                      :help "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-keep-shell-dir-when-execute-p]

                     ["Fileless buffer use default directory "
                      (setq py-fileless-buffer-use-default-directory-p
                            (not py-fileless-buffer-use-default-directory-p))
                      :help "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fileless-buffer-use-default-directory-p])

                    ("Underscore word syntax"
                     :help "Toggle `py-underscore-word-syntax-p'"

                     ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                      :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax on" py-underscore-word-syntax-p-on
                      :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax off" py-underscore-word-syntax-p-off
                      :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"])

                    ["Load pymacs "
                     (setq py-load-pymacs-p
                           (not py-load-pymacs-p))
                     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-load-pymacs-p]

                    ["Verbose "
                     (setq py-verbose-p
                           (not py-verbose-p))
                     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-verbose-p]

                    ["Empty comment line separates paragraph "
                     (setq empty-comment-line-separates-paragraph-p
                           (not empty-comment-line-separates-paragraph-p))
                     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected empty-comment-line-separates-paragraph-p]

                    ["Org cycle "
                     (setq py-org-cycle-p
                           (not py-org-cycle-p))
                     :help "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-org-cycle-p]

                    ["Set pager cat"
                     (setq py-set-pager-cat-p
                           (not py-set-pager-cat-p))
                     :help "If the shell environment variable \$PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' Use `M-x customize-variable' to
set it permanently"
                     :style toggle :selected py-set-pager-cat-p]

                    ["Edit only "
                     (setq py-edit-only-p
                           (not py-edit-only-p))
                     :help "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-edit-only-p])))
		 ("Customize"

                  ["Python-mode customize group" (customize-group 'python-mode)
                   :help "Open the customization buffer for Python mode"]
                  ("Switches"
                   :help "Toggle useful modes like `highlight-indentation'"
                   ("Interpreter"

                    ["Shell prompt read only"
                     (setq py-shell-prompt-read-only
                           (not py-shell-prompt-read-only))
                     :help "If non-nil, the python prompt is read only.  Setting this variable will only effect new shells.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-shell-prompt-read-only]

                    ["Remove cwd from path"
                     (setq py-remove-cwd-from-path
                           (not py-remove-cwd-from-path))
                     :help "Whether to allow loading of Python modules from the current directory.
If this is non-nil, Emacs removes '' from sys.path when starting
a Python process.  This is the default, for security
reasons, as it is easy for the Python process to be started
without the user's realization (e.g. to perform completion).Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-remove-cwd-from-path]

                    ["Honor IPYTHONDIR "
                     (setq py-honor-IPYTHONDIR-p
                           (not py-honor-IPYTHONDIR-p))
                     :help "When non-nil ipython-history file is constructed by \$IPYTHONDIR
followed by "/history". Default is nil.

Otherwise value of py-ipython-history is used. Use `M-x customize-variable' to set it permanently"
:style toggle :selected py-honor-IPYTHONDIR-p]

                    ["Honor PYTHONHISTORY "
                     (setq py-honor-PYTHONHISTORY-p
                           (not py-honor-PYTHONHISTORY-p))
                     :help "When non-nil python-history file is set by \$PYTHONHISTORY
Default is nil.

Otherwise value of py-python-history is used. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-honor-PYTHONHISTORY-p]

                    ["Enforce py-shell-name" force-py-shell-name-p-on
                     :help "Enforce customized default `py-shell-name' should upon execution. "]

                    ["Don't enforce default interpreter" force-py-shell-name-p-off
                     :help "Make execute commands guess interpreter from environment"]

                    ["Enforce local Python shell " py-force-local-shell-on
                     :help "Locally indicated Python being enforced upon sessions execute commands. "]

                    ["Remove local Python shell enforcement, restore default" py-force-local-shell-off
                     :help "Restore `py-shell-name' default value and `behaviour'. "])

                   ("Execute"

		    ["Fast process" py-fast-process-p
		     :help " `py-fast-process-p'

Use `py-fast-process'\.

Commands prefixed \"py-fast-...\" suitable for large output

See: large output makes Emacs freeze, lp:1253907

Output-buffer is not in comint-mode"
		     :style toggle :selected py-fast-process-p]

		    ["Python mode v5 behavior"
                     (setq python-mode-v5-behavior-p
                           (not python-mode-v5-behavior-p))
                     :help "Execute region through `shell-command-on-region' as
v5 did it - lp:990079. This might fail with certain chars - see UnicodeEncodeError lp:550661

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected python-mode-v5-behavior-p]

                    ["Force shell name "
                     (setq py-force-py-shell-name-p
                           (not py-force-py-shell-name-p))
                     :help "When `t', execution with kind of Python specified in `py-shell-name' is enforced, possibly shebang doesn't take precedence. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-force-py-shell-name-p]

                    ["Cleanup temporary"
                     (setq py-cleanup-temporary
                           (not py-cleanup-temporary))
                     :help "If temporary buffers and files used by functions executing region should be deleted afterwards. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-cleanup-temporary]

                    ["Execute \"if name == main\" blocks p"
                     (setq py-if-name-main-permission-p
                           (not py-if-name-main-permission-p))
                     :help " `py-if-name-main-permission-p'

Allow execution of code inside blocks delimited by
if __name__ == '__main__'

Default is non-nil. "
                     :style toggle :selected py-if-name-main-permission-p]

                    ["Ask about save"
                     (setq py-ask-about-save
                           (not py-ask-about-save))
                     :help "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-ask-about-save]

                    ["Store result"
                     (setq py-store-result-p
                           (not py-store-result-p))
                     :help " `py-store-result-p'

When non-nil, put resulting string of `py-execute-...' into kill-ring, so it might be yanked. "
                     :style toggle :selected py-store-result-p]

                    ["Prompt on changed "
                     (setq py-prompt-on-changed-p
                           (not py-prompt-on-changed-p))
                     :help "When called interactively, ask for save before a changed buffer is sent to interpreter.

Default is `t'Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-prompt-on-changed-p]

                    ["Dedicated process "
                     (setq py-dedicated-process-p
                           (not py-dedicated-process-p))
                     :help "If commands executing code use a dedicated shell.

Default is nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-dedicated-process-p]

                    ["Execute without temporary file"
                     (setq py-execute-no-temp-p
                           (not py-execute-no-temp-p))
                     :help " `py-execute-no-temp-p'
Seems Emacs-24.3 provided a way executing stuff without temporary files.
In experimental state yet "
                     :style toggle :selected py-execute-no-temp-p]

                    ["Warn tmp files left "
                     (setq py--warn-tmp-files-left-p
                           (not py--warn-tmp-files-left-p))
                     :help "Messages a warning, when `py-temp-directory' contains files susceptible being left by previous Python-mode sessions. See also lp:987534 Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py--warn-tmp-files-left-p])

                   ("Edit"

                    ("Completion"

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found\. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Set Pymacs-based complete keymap "
                      (setq py-set-complete-keymap-p
                            (not py-set-complete-keymap-p))
                      :help "If `py-complete-initialize', which sets up enviroment for Pymacs based py-complete, should load it's keys into `python-mode-map'

Default is nil.
See also resp. edit `py-complete-set-keymap' Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-set-complete-keymap-p]

                     ["No completion calls dabbrev expand "
                      (setq py-no-completion-calls-dabbrev-expand-p
                            (not py-no-completion-calls-dabbrev-expand-p))
                      :help "If completion function should call dabbrev-expand when no completion found. Default is `t'

See also `py-indent-no-completion-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-no-completion-calls-dabbrev-expand-p]

                     ["Indent no completion "
                      (setq py-indent-no-completion-p
                            (not py-indent-no-completion-p))
                      :help "If completion function should indent when no completion found. Default is `t'

See also `py-no-completion-calls-dabbrev-expand-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-no-completion-p]

                     ["Company pycomplete "
                      (setq py-company-pycomplete-p
                            (not py-company-pycomplete-p))
                      :help "Load company-pycomplete stuff. Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-company-pycomplete-p])

                    ("Autopair mode"
                     :help "Toggle `autopair-mode'"

                     ["Toggle autopair mode" toggle-py-autopair-mode
                      :help " `toggle-autopair-mode'

If `autopair-mode' should be on or off.

  Returns value of `autopair-mode ' switched to. "]

                     ["Autopair mode on" py-autopair-mode-on
                      :help " `autopair-mode on'

Make sure, `autopair-mode' is on.

Returns value of `autopair-mode'. "]

                     ["Autopair mode off" py-autopair-mode-off
                      :help " `autopair-mode' off

Make sure, `autopair-mode' is off.

Returns value of `autopair-mode'. "])

                    ;; py-smart-operator-mode-p forms
                    ("Smart operator mode"
                     :help "Toggle `smart-operator-mode'"

                     ["Toggle smart operator mode" toggle-py-smart-operator-mode-p
                      :help " `toggle-smart-operator-mode'

If `smart-operator-mode' should be on or off.

  Returns value of `smart-operator-mode ' switched to. "]

                     ["Smart operator mode on" py-smart-operator-mode-p-on
                      :help " `smart-operator-mode -on'

Make sure, `smart-operator-mode' is on.

Returns value of `smart-operator-mode'. "]

                     ["Smart operator mode off" py-smart-operator-mode-p-off
                      :help " `smart-operator-mode' off

Make sure, `smart-operator-mode' is off.

Returns value of `smart-operator-mode'. "])

                    ("Filling"

                     ("Docstring styles"
                      :help "Switch docstring-style"

                      ["Nil" py-set-nil-docstring-style
                       :help " `py-set-nil-docstring-style'

Set py-docstring-style to nil, format string normally. "]

                      ["pep-257-nn" py-set-pep-257-nn-docstring-style
                       :help " `py-set-pep-257-nn-docstring-style'

Set py-docstring-style to 'pep-257-nn "]

                      ["pep-257" py-set-pep-257-docstring-style
                       :help " `py-set-pep-257-docstring-style'

Set py-docstring-style to 'pep-257 "]

                      ["django" py-set-django-docstring-style
                       :help " `py-set-django-docstring-style'

Set py-docstring-style to 'django "]

                      ["onetwo" py-set-onetwo-docstring-style
                       :help " `py-set-onetwo-docstring-style'

Set py-docstring-style to 'onetwo "]

                      ["symmetric" py-set-symmetric-docstring-style
                       :help " `py-set-symmetric-docstring-style'

Set py-docstring-style to 'symmetric "])

                     ["Auto-fill mode"
                      (setq py-auto-fill-mode
                            (not py-auto-fill-mode))
                      :help "Fill according to `py-docstring-fill-column' and `py-comment-fill-column'

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-auto-fill-mode])

                    ["Use current dir when execute"
                     (setq py-use-current-dir-when-execute-p
                           (not py-use-current-dir-when-execute-p))
                     :help " `toggle-py-use-current-dir-when-execute-p'

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-use-current-dir-when-execute-p]

                    ("Indent"
		     ("TAB related"

		      ["indent-tabs-mode"
		       (setq indent-tabs-mode
			     (not indent-tabs-mode))
		       :help "Indentation can insert tabs if this is non-nil.

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected indent-tabs-mode]

		      ["Tab indent"
		       (setq py-tab-indent
			     (not py-tab-indent))
		       :help "Non-nil means TAB in Python mode calls `py-indent-line'.Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indent]

		      ["Tab shifts region "
		       (setq py-tab-shifts-region-p
			     (not py-tab-shifts-region-p))
		       :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-shifts-region-p]

		      ["Tab indents region "
		       (setq py-tab-indents-region-p
			     (not py-tab-indents-region-p))
		       :help "When `t' and first TAB doesn't shift, indent-region is called.

Default is nil
See also `py-tab-shifts-region-p'

Use `M-x customize-variable' to set it permanently"
		       :style toggle :selected py-tab-indents-region-p])

                     ["Close at start column"
                      (setq py-closing-list-dedents-bos
                            (not py-closing-list-dedents-bos))
                      :help "When non-nil, indent list's closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = \[
    1, 2, 3,
    4, 5, 6,
]

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-dedents-bos]

                     ["Closing list keeps space"
                      (setq py-closing-list-keeps-space
                            (not py-closing-list-keeps-space))
                      :help "If non-nil, closing parenthesis dedents onto column of opening plus `py-closing-list-space', default is nil Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-keeps-space]

                     ["Closing list space"
                      (setq py-closing-list-space
                            (not py-closing-list-space))
                      :help "Number of chars, closing parenthesis outdent from opening, default is 1 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-closing-list-space]

                     ["Tab shifts region "
                      (setq py-tab-shifts-region-p
                            (not py-tab-shifts-region-p))
                      :help "If `t', TAB will indent/cycle the region, not just the current line.

Default is nil
See also `py-tab-indents-region-p'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-tab-shifts-region-p]

                     ["Lhs inbound indent"
                      (setq py-lhs-inbound-indent
                            (not py-lhs-inbound-indent))
                      :help "When line starts a multiline-assignment: How many colums indent should be more than opening bracket, brace or parenthesis. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-lhs-inbound-indent]

                     ["Continuation offset"
                      (setq py-continuation-offset
                            (not py-continuation-offset))
                      :help "With numeric ARG different from 1 py-continuation-offset is set to that value; returns py-continuation-offset. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-continuation-offset]

                     ["Electric colon"
                      (setq py-electric-colon-active-p
                            (not py-electric-colon-active-p))
                      :help " `py-electric-colon-active-p'

`py-electric-colon' feature.  Default is `nil'. See lp:837065 for discussions. "
                      :style toggle :selected py-electric-colon-active-p]

                     ["Electric colon at beginning of block only"
                      (setq py-electric-colon-bobl-only
                            (not py-electric-colon-bobl-only))
                      :help "When inserting a colon, do not indent lines unless at beginning of block.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-colon-bobl-only]

                     ["Electric yank active "
                      (setq py-electric-yank-active-p
                            (not py-electric-yank-active-p))
                      :help " When non-nil, `yank' will be followed by an `indent-according-to-mode'.

Default is nilUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-yank-active-p]

                     ["Electric kill backward "
                      (setq py-electric-kill-backward-p
                            (not py-electric-kill-backward-p))
                      :help "Affects `py-electric-backspace'. Default is nil.

If behind a delimited form of braces, brackets or parentheses,
backspace will kill it's contents

With when cursor after
my_string\[0:1]
--------------^

==>

my_string\[]
----------^

In result cursor is insided emptied delimited form.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-kill-backward-p]

                     ["Trailing whitespace smart delete "
                      (setq py-trailing-whitespace-smart-delete-p
                            (not py-trailing-whitespace-smart-delete-p))
                      :help "Default is nil. When t, python-mode calls
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-trailing-whitespace-smart-delete-p]

                     ["Newline delete trailing whitespace "
                      (setq py-newline-delete-trailing-whitespace-p
                            (not py-newline-delete-trailing-whitespace-p))
                      :help "Delete trailing whitespace maybe left by `py-newline-and-indent'.

Default is `t'. See lp:1100892 Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-newline-delete-trailing-whitespace-p]

                     ["Dedent keep relative column"
                      (setq py-dedent-keep-relative-column
                            (not py-dedent-keep-relative-column))
                      :help "If point should follow dedent or kind of electric move to end of line. Default is t - keep relative position. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-dedent-keep-relative-column]

                     ["Indent paren spanned multilines "
                      (setq py-indent-paren-spanned-multilines-p
                            (not py-indent-paren-spanned-multilines-p))
                      :help "If non-nil, indents elements of list a value of `py-indent-offset' to first element:

def foo():
    if (foo &&
            baz):
        bar()

Default lines up with first element:

def foo():
    if (foo &&
        baz):
        bar()
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-paren-spanned-multilines-p]

                     ["Indent honors multiline listing"
                      (setq py-indent-honors-multiline-listing
                            (not py-indent-honors-multiline-listing))
                      :help "If `t', indents to 1\+ column of opening delimiter. If `nil', indent adds one level to the beginning of statement. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-multiline-listing]

                     ["Indent comment "
                      (setq py-indent-comments
                            (not py-indent-comments))
                      :help "If comments should be indented like code. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-comments]

                     ["Uncomment indents "
                      (setq py-uncomment-indents-p
                            (not py-uncomment-indents-p))
                      :help "When non-nil, after uncomment indent lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-uncomment-indents-p]

                     ["Indent honors inline comment"
                      (setq py-indent-honors-inline-comment
                            (not py-indent-honors-inline-comment))
                      :help "If non-nil, indents to column of inlined comment start.
Default is nil. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-indent-honors-inline-comment]

                     ["Kill empty line"
                      (setq py-kill-empty-line
                            (not py-kill-empty-line))
                      :help "If t, py-indent-forward-line kills empty lines. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-kill-empty-line]

                     ("Smart indentation"
                      :help "Toggle py-smart-indentation'

Use `M-x customize-variable' to set it permanently"

                      ["Toggle py-smart-indentation" toggle-py-smart-indentation
                       :help "Toggles py-smart-indentation

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation on" py-smart-indentation-on
                       :help "Switches py-smart-indentation on

Use `M-x customize-variable' to set it permanently"]

                      ["py-smart-indentation off" py-smart-indentation-off
                       :help "Switches py-smart-indentation off

Use `M-x customize-variable' to set it permanently"])

                     ["Beep if tab change"
                      (setq py-beep-if-tab-change
                            (not py-beep-if-tab-change))
                      :help "Ring the bell if `tab-width' is changed.
If a comment of the form

                           	# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-beep-if-tab-change]

                     ["Highlight indentation" highlight-indentation
                      :help "Toggle highlight indentation.

Use `M-x customize-variable' to set it permanently

Make sure `highlight-indentation' is installed"

                      ]

                     ["Electric comment "
                      (setq py-electric-comment-p
                            (not py-electric-comment-p))
                      :help "If \"#\" should call `py-electric-comment'. Default is `nil'.

Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-p]

                     ["Electric comment add space "
                      (setq py-electric-comment-add-space-p
                            (not py-electric-comment-add-space-p))
                      :help "If py-electric-comment should add a space.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-electric-comment-add-space-p]

                     ["Empty line closes "
                      (setq py-empty-line-closes-p
                            (not py-empty-line-closes-p))
                      :help "When non-nil, dedent after empty line following block

if True:
    print(\"Part of the if-statement\")

print(\"Not part of the if-statement\")

Default is nil

If non-nil, a C-j from empty line dedents.
Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-empty-line-closes-p])
                    ["Defun use top level "
                     (setq py-defun-use-top-level-p
                           (not py-defun-use-top-level-p))
                     :help "When non-nil, keys C-M-a, C-M-e address top-level form.

Beginning- end-of-defun forms use
commands `py-beginning-of-top-level', `py-end-of-top-level'

mark-defun marks top-level form at point etc. "
                     :style toggle :selected py-defun-use-top-level-p]

                    ["Close provides newline"
                     (setq py-close-provides-newline
                           (not py-close-provides-newline))
                     :help "If a newline is inserted, when line after block isn't empty. Default is non-nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-close-provides-newline]

                    ["Block comment prefix "
                     (setq py-block-comment-prefix-p
                           (not py-block-comment-prefix-p))
                     :help "If py-comment inserts py-block-comment-prefix.

Default is tUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-block-comment-prefix-p])

                   ("Display"

                    ("Index"

                     ["Imenu create index "
                      (setq py--imenu-create-index-p
                            (not py--imenu-create-index-p))
                      :help "Non-nil means Python mode creates and displays an index menu of functions and global variables. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py--imenu-create-index-p]

                     ["Imenu show method args "
                      (setq py-imenu-show-method-args-p
                            (not py-imenu-show-method-args-p))
                      :help "Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed.Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-imenu-show-method-args-p]
                     ["Switch index-function" py-switch-imenu-index-function
                      :help "`py-switch-imenu-index-function'
Switch between `py--imenu-create-index' from 5.1 series and `py--imenu-create-index-new'."])

                    ("Fontification"

                     ["Mark decorators"
                      (setq py-mark-decorators
                            (not py-mark-decorators))
                      :help "If py-mark-def-or-class functions should mark decorators too. Default is `nil'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-mark-decorators]

                     ["Fontify shell buffer "
                      (setq py-fontify-shell-buffer-p
                            (not py-fontify-shell-buffer-p))
                      :help "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If `t', related vars like `comment-start' will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fontify-shell-buffer-p]

                     ["Use font lock doc face "
                      (setq py-use-font-lock-doc-face-p
                            (not py-use-font-lock-doc-face-p))
                      :help "If documention string inside of def or class get `font-lock-doc-face'.

`font-lock-doc-face' inherits `font-lock-string-face'.

Call M-x `customize-face' in order to have a visible effect. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-font-lock-doc-face-p])

                    ["Switch buffers on execute"
                     (setq py-switch-buffers-on-execute-p
                           (not py-switch-buffers-on-execute-p))
                     :help "When non-nil switch to the Python output buffer.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-switch-buffers-on-execute-p]

                    ["Split windows on execute"
                     (setq py-split-windows-on-execute-p
                           (not py-split-windows-on-execute-p))
                     :help "When non-nil split windows.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-p]

                    ["Keep windows configuration"
                     (setq py-keep-windows-configuration
                           (not py-keep-windows-configuration))
                     :help "If a windows is splitted displaying results, this is directed by variable `py-split-windows-on-execute-p'\. Also setting `py-switch-buffers-on-execute-p' affects window-configuration\. While commonly a screen splitted into source and Python-shell buffer is assumed, user may want to keep a different config\.

Setting `py-keep-windows-configuration' to `t' will restore windows-config regardless of settings mentioned above\. However, if an error occurs, it's displayed\.

To suppres window-changes due to error-signaling also: M-x customize-variable RET. Set `py-keep-4windows-configuration' onto 'force

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-keep-windows-configuration]

                    ["Which split windows on execute function"
                     (progn
                       (if (eq 'split-window-vertically py-split-windows-on-execute-function)
                           (setq py-split-windows-on-execute-function'split-window-horizontally)
                         (setq py-split-windows-on-execute-function 'split-window-vertically))
                       (message "py-split-windows-on-execute-function set to: %s" py-split-windows-on-execute-function))

                     :help "If `split-window-vertically' or `...-horizontally'. Use `M-x customize-variable' RET `py-split-windows-on-execute-function' RET to set it permanently"
                     :style toggle :selected py-split-windows-on-execute-function]

                    ["Modeline display full path "
                     (setq py-modeline-display-full-path-p
                           (not py-modeline-display-full-path-p))
                     :help "If the full PATH/TO/PYTHON should be displayed in shell modeline.

Default is nil. Note: when `py-shell-name' is specified with path, it's shown as an acronym in buffer-name already. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-display-full-path-p]

                    ["Modeline acronym display home "
                     (setq py-modeline-acronym-display-home-p
                           (not py-modeline-acronym-display-home-p))
                     :help "If the modeline acronym should contain chars indicating the home-directory.

Default is nil Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-modeline-acronym-display-home-p]

                    ["Hide show hide docstrings"
                     (setq py-hide-show-hide-docstrings
                           (not py-hide-show-hide-docstrings))
                     :help "Controls if doc strings can be hidden by hide-showUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-show-hide-docstrings]

                    ["Hide comments when hiding all"
                     (setq py-hide-comments-when-hiding-all
                           (not py-hide-comments-when-hiding-all))
                     :help "Hide the comments too when you do `hs-hide-all'. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-hide-comments-when-hiding-all]

                    ["Max help buffer "
                     (setq py-max-help-buffer-p
                           (not py-max-help-buffer-p))
                     :help "If \"\*Python-Help\*\"-buffer should appear as the only visible.

Default is nil. In help-buffer, \"q\" will close it.  Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-max-help-buffer-p]

                    ["Current defun show"
                     (setq py-current-defun-show
                           (not py-current-defun-show))
                     :help "If `py-current-defun' should jump to the definition, highlight it while waiting PY-WHICH-FUNC-DELAY seconds, before returning to previous position.

Default is `t'.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-current-defun-show]

                    ["Match paren mode"
                     (setq py-match-paren-mode
                           (not py-match-paren-mode))
                     :help "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets `py-match-paren-key' in python-mode-map.
Customize `py-match-paren-key' which key to use. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-match-paren-mode])

                   ("Debug"

                    ["Pdbtrack do tracking "
                     (setq py-pdbtrack-do-tracking-p
                           (not py-pdbtrack-do-tracking-p))
                     :help "Controls whether the pdbtrack feature is enabled or not.
When non-nil, pdbtrack is enabled in all comint-based buffers,
e.g. shell buffers and the \*Python\* buffer.  When using pdb to debug a
Python program, pdbtrack notices the pdb prompt and displays the
source file and line that the program is stopped at, much the same way
as gud-mode does for debugging C programs with gdb.Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-pdbtrack-do-tracking-p]

                    ["Jump on exception"
                     (setq py-jump-on-exception
                           (not py-jump-on-exception))
                     :help "Jump to innermost exception frame in Python output buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame.

Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-jump-on-exception]

                    ["Highlight error in source "
                     (setq py-highlight-error-source-p
                           (not py-highlight-error-source-p))
                     :help "Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-highlight-error-source-p])

                   ("Other"

                    ("Directory"

                     ["Guess install directory "
                      (setq py-guess-py-install-directory-p
                            (not py-guess-py-install-directory-p))
                      :help "If in cases, `py-install-directory' isn't set,  `py-set-load-path'should guess it from `buffer-file-name'. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-guess-py-install-directory-p]

                     ["Use local default"
                      (setq py-use-local-default
                            (not py-use-local-default))
                      :help "If `t', py-shell will use `py-shell-local-path' instead
of default Python.

Making switch between several virtualenv's easier,
                               `python-mode' should deliver an installer, so named-shells pointing to virtualenv's will be available. Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-local-default]

                     ["Use current dir when execute "
                      (setq py-use-current-dir-when-execute-p
                            (not py-use-current-dir-when-execute-p))
                      :help "When `t', current directory is used by Python-shell for output of `py-execute-buffer' and related commands.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-use-current-dir-when-execute-p]

                     ["Keep shell dir when execute "
                      (setq py-keep-shell-dir-when-execute-p
                            (not py-keep-shell-dir-when-execute-p))
                      :help "Don't change Python shell's current working directory when sending code.

See also `py-execute-directory'Use `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-keep-shell-dir-when-execute-p]

                     ["Fileless buffer use default directory "
                      (setq py-fileless-buffer-use-default-directory-p
                            (not py-fileless-buffer-use-default-directory-p))
                      :help "When `py-use-current-dir-when-execute-p' is non-nil and no buffer-file exists, value of `default-directory' sets current working directory of Python output shellUse `M-x customize-variable' to set it permanently"
                      :style toggle :selected py-fileless-buffer-use-default-directory-p])

                    ("Underscore word syntax"
                     :help "Toggle `py-underscore-word-syntax-p'"

                     ["Toggle underscore word syntax" toggle-py-underscore-word-syntax-p
                      :help " `toggle-py-underscore-word-syntax-p'

If `py-underscore-word-syntax-p' should be on or off.

  Returns value of `py-underscore-word-syntax-p' switched to. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax on" py-underscore-word-syntax-p-on
                      :help " `py-underscore-word-syntax-p-on'

Make sure, py-underscore-word-syntax-p' is on.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"]

                     ["Underscore word syntax off" py-underscore-word-syntax-p-off
                      :help " `py-underscore-word-syntax-p-off'

Make sure, `py-underscore-word-syntax-p' is off.

Returns value of `py-underscore-word-syntax-p'. .

Use `M-x customize-variable' to set it permanently"])

                    ["Load pymacs "
                     (setq py-load-pymacs-p
                           (not py-load-pymacs-p))
                     :help "If Pymacs related stuff should be loaded.

Default is nil.

Pymacs has been written by François Pinard and many others.
See original source: http://pymacs.progiciels-bpi.caUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-load-pymacs-p]

                    ["Verbose "
                     (setq py-verbose-p
                           (not py-verbose-p))
                     :help "If functions should report results.

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-verbose-p]

                    ["Empty comment line separates paragraph "
                     (setq empty-comment-line-separates-paragraph-p
                           (not empty-comment-line-separates-paragraph-p))
                     :help "Consider paragraph start/end lines with nothing inside but comment sign.

Default is non-nilUse `M-x customize-variable' to set it permanently"
                     :style toggle :selected empty-comment-line-separates-paragraph-p]

                    ["Org cycle "
                     (setq py-org-cycle-p
                           (not py-org-cycle-p))
                     :help "When non-nil, command `org-cycle' is available at shift-TAB, <backtab>

Default is nil. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-org-cycle-p]

                    ["Set pager cat"
                     (setq py-set-pager-cat-p
                           (not py-set-pager-cat-p))
                     :help "If the shell environment variable \$PAGER should set to `cat'.

If `t', use `C-c C-r' to jump to beginning of output. Then scroll normally.

Avoids lp:783828, \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module `os' Use `M-x customize-variable' to
set it permanently"
                     :style toggle :selected py-set-pager-cat-p]

                    ["Edit only "
                     (setq py-edit-only-p
                           (not py-edit-only-p))
                     :help "When `t' `python-mode' will not take resort nor check for installed Python executables. Default is nil.

See bug report at launchpad, lp:944093. Use `M-x customize-variable' to set it permanently"
                     :style toggle :selected py-edit-only-p])))

                 ("More... "

                  ("Edit commands "

		   ("Hide"
		    ["Hide statement" py-hide-statement
		     :help " `py-hide-statement'

Hide statement at point\. "]

		    ["Hide block" py-hide-block
		     :help " `py-hide-block'

Hide block at point\. "]

		    ["Hide clause" py-hide-clause
		     :help " `py-hide-clause'

Hide clause at point\. "]

		    ["Hide block or clause" py-hide-block-or-clause
		     :help " `py-hide-block-or-clause'

Hide block-or-clause at point\. "]

		    ["Hide def" py-hide-def
		     :help " `py-hide-def'

Hide def at point\. "]

		    ["Hide class" py-hide-class
		     :help " `py-hide-class'

Hide class at point\. "]

		    ["Hide expression" py-hide-expression
		     :help " `py-hide-expression'

Hide expression at point\. "]

		    ["Hide partial expression" py-hide-partial-expression
		     :help " `py-hide-partial-expression'

Hide partial-expression at point\. "]

		    ["Hide line" py-hide-line
		     :help " `py-hide-line'

Hide line at point\. "]

		    ["Hide top level" py-hide-top-level
		     :help " `py-hide-top-level'

Hide top-level at point\. "])

		   ("Show"

		    ["Show statement" py-show-statement
		     :help " `py-show-statement'

Show statement at point\. "]

		    ["Show block" py-show-block
		     :help " `py-show-block'

Show block at point\. "]

		    ["Show clause" py-show-clause
		     :help " `py-show-clause'

Show clause at point\. "]

		    ["Show block or clause" py-show-block-or-clause
		     :help " `py-show-block-or-clause'

Show block-or-clause at point\. "]

		    ["Show def" py-show-def
		     :help " `py-show-def'

Show def at point\. "]

		    ["Show class" py-show-class
		     :help " `py-show-class'

Show class at point\. "]

		    ["Show expression" py-show-expression
		     :help " `py-show-expression'

Show expression at point\. "]

		    ["Show partial expression" py-show-partial-expression
		     :help " `py-show-partial-expression'

Show partial-expression at point\. "]

		    ["Show line" py-show-line
		     :help " `py-show-line'

Show line at point\. "]

		    ["Show top level" py-show-top-level
		     :help " `py-show-top-level'

Show top-level at point\. "])

                   ("Kill "

                    ["Kill statement" py-kill-statement
                     :help "`py-kill-statement'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill top level" py-kill-top-level
                     :help " `py-kill-top-level'

Delete top-level form at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill clause" py-kill-clause
                     :help "`py-kill-clause'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill block" py-kill-block
                     :help "`py-kill-block'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill minor block" py-kill-minor-block
                     :help " `py-kill-minor-block'

Delete minor-block at point.

Stores data in kill ring. Might be yanked back using `C-y'. "]

                    ["Kill def-or-class" py-kill-def-or-class
                     :help "`py-kill-def-or-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill expression" py-kill-expression
                     :help "`py-kill-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill partial-expression" py-kill-partial-expression
                     :help "`py-kill-partial-expression'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill class" py-kill-class
                     :help "`py-kill-class'
Delete innermost compound statement at point, store deleted string in kill-ring"]

                    ["Kill def" py-kill-def
                     :help "`py-kill-def'
Delete innermost compound statement at point, store deleted string in kill-ring"])

                   ("Delete"
                    ["Delete statement " py-delete-statement
                     :help "`py-delete-statement'
Delete STATEMENT at point, don't store in kill-ring. "]

                    ["Delete top-level " py-delete-top-level
                     :help "`py-delete-top-level'
Delete TOP-LEVEL at point, don't store in kill-ring. "]

                    ["Delete block " py-delete-block
                     :help "`py-delete-block'
Delete BLOCK at point, don't store in kill-ring. "]

                    ["Delete block-or-clause " py-delete-block-or-clause
                     :help "`py-delete-block-or-clause'
Delete BLOCK-OR-CLAUSE at point, don't store in kill-ring. "]

                    ["Delete def " py-delete-def
                     :help "`py-delete-def'
Delete DEF at point, don't store in kill-ring. "]

                    ["Delete class " py-delete-class
                     :help "`py-delete-class'
Delete CLASS at point, don't store in kill-ring. "]

                    ["Delete def-or-class " py-delete-def-or-class
                     :help "`py-delete-def-or-class'
Delete DEF-OR-CLASS at point, don't store in kill-ring. "]

                    ["Delete expression " py-delete-expression
                     :help "`py-delete-expression'
Delete EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete partial-expression " py-delete-partial-expression
                     :help "`py-delete-partial-expression'
Delete PARTIAL-EXPRESSION at point, don't store in kill-ring. "]

                    ["Delete minor-block " py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete MINOR-BLOCK at point, don't store in kill-ring.

A minor block is started by a `for', `if', `try' or `with'. "])
                   "-"

                   ("Shift right "
                    ["Shift block right" py-shift-block-right
                     :help "`py-shift-block-right'
Shift block right. "]

                    ["Shift clause right" py-shift-clause-right
                     :help "`py-shift-clause-right'
Shift clause right. "]

                    ["Shift statement right" py-shift-statement-right
                     :help "`py-shift-statement-right'
Shift statement right. "]

                    ["Shift minor block right" py-shift-minor-block-right
                     :help " `py-shift-minor-block-right'

Indent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

                    ["Shift def-or-class right" py-shift-def-or-class-right
                     :help "`py-shift-def-or-class-right'
Shift def-or-class right. "]

                    ["Shift class right" py-shift-class-right
                     :help "`py-shift-class-right'
Shift class right. "]

                    ["Shift def right" py-shift-def-right
                     :help "`py-shift-def-right'
Shift def right. "]

                    ["Shift block-or-clause right" py-shift-block-or-clause-right
                     :help "`py-shift-block-or-clause-right'
Shift block-or-clause right. "]

                    ["Shift region left" py-shift-region-left
                     :help " `py-shift-region-left'

Dedent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is dedented.
Returns indentation reached. "]

                    ["Shift region right" py-shift-region-right
                     :help " `py-shift-region-right'

Indent region according to `py-indent-offset' by COUNT times.

If no region is active, current line is indented.
Returns indentation reached. "])

                   ("Shift left "

                    ["Shift block left" py-shift-block-left
                     :help "`py-shift-block-left'
Shift block left. "]

                    ["Shift clause left" py-shift-clause-left
                     :help "`py-shift-clause-left'
Shift clause left. "]

                    ["Shift statement left" py-shift-statement-left
                     :help "`py-shift-statement-left'
Shift statement left. "]

                    ["Shift minor block left" py-shift-minor-block-left
                     :help " `py-shift-minor-block-left'

Dedent minor-block by COUNT spaces.

COUNT defaults to `py-indent-offset',
use \[universal-argument] to specify a different value.

Returns outmost indentation reached. "]

                    ["Shift def-or-class left" py-shift-def-or-class-left
                     :help "`py-shift-def-or-class-left'
Shift def-or-class left. "]

                    ["Shift class left" py-shift-class-left
                     :help "`py-shift-class-left'
Shift class left. "]

                    ["Shift def left" py-shift-def-left
                     :help "`py-shift-def-left'
Shift def left. "]

                    ["Shift block-or-clause left" py-shift-block-or-clause-left
                     :help "`py-shift-block-or-clause-left'
Shift block-or-clause left. "])
                   ("More"
                    :help "extended edit commands'"

                    ["Kill buffer unconditional" py-kill-buffer-unconditional
                     :help " `py-kill-buffer-unconditional'

Kill buffer unconditional, kill buffer-process if existing\. "]

                    ["Empty out list backward" py-empty-out-list-backward
                     :help " `py-empty-out-list-backward'
Deletes all elements from list before point. "]

                    ["Revert boolean assignent" py-boolswitch
                     :help " `py-boolswitch'
Edit the assigment of a boolean variable, rever them.

I.e. switch it from \"True\" to \"False\" and vice versa "]

                    ["Remove overlays at point" py-remove-overlays-at-point
                     :help " `py-remove-overlays-at-point'

Remove overlays as set when `py-highlight-error-source-p' is non-nil. "]))

                  "-"
                  ("Forms "
                   ("Comment"

                    ["Beginning of comment" py-beginning-of-comment
                     :help " `py-beginning-of-comment'
Go to beginning of comment at point. "]

                    ["End of comment" py-end-of-comment
                     :help " `py-end-of-comment'

Go to end of comment at point. "])
                   ("Block"
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

                    ["Comment block" py-comment-block
                     :help " `py-comment-block'

Comments block at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Top-level form"

                    ["Beginning of top-level form" py-beginning-of-top-level
                     :help "`py-beginning-of-top-level'
Go to start of top-level form form at point"]

                    ["End of top-level form" py-end-of-top-level
                     :help "`py-end-of-top-level'
Go to end of top-level form at point"]

                    ["Down top-level form" py-down-top-level
                     :help "`py-down-top-level'

Go to the beginning of top-level form below in buffer. "]

                    ["Up top-level form" py-up-top-level
                     :help "`py-up-top-level'

Go upwards to the beginning of next top-level form in buffer. "]

                    ["Copy top-level form" py-copy-top-level
                     :help "`py-copy-top-level'
Copy innermost top-level form at point"]

                    ["Kill top-level form" py-kill-top-level
                     :help "`py-kill-top-level'
Delete top-level form at point, store deleted string in kill-ring"]

                    ["Delete top-level form" py-delete-top-level
                     :help "`py-delete-top-level'
Delete top-level form at point, don't store deleted string in kill-ring"]

                    ["Comment top-level form" py-comment-top-level
                     :help " `py-comment-top-level'

Comments top-level form at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Minor-block"

                    ["Beginning of minor-block" py-beginning-of-minor-block
                     :help "`py-beginning-of-minor-block'
Go to start of innermost minor-block at point"]
                    ["End of minor-block" py-end-of-minor-block
                     :help "`py-end-of-minor-block'
Go to end of innermost minor-block at point"]

                    ["Down minor-block" py-down-minor-block
                     :help "`py-down-minor-block'

Go to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Up minor-block" py-up-minor-block
                     :help "`py-up-minor-block'

Go upwards to the beginning of next minor-block below in buffer.

Returns indentation if minor-block found, nil otherwise. "]

                    ["Copy minor-block" py-copy-minor-block
                     :help "`py-copy-minor-block'
Copy innermost minor-block at point"]

                    ["Kill minor-block" py-kill-minor-block
                     :help "`py-kill-minor-block'
Delete innermost minor-block at point, store deleted string in kill-ring"]

                    ["Delete minor-block" py-delete-minor-block
                     :help "`py-delete-minor-block'
Delete innermost minor-block at point, don't store deleted string in kill-ring"]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left. "]

                    ["Comment minor-block" py-comment-minor-block
                     :help " `py-comment-minor-block'

Comments minor-block at point.

Uses double hash (`#') comment starter when `py-minor-block-comment-prefix-p' is `t',
the default. "])

                   ("Def-or-class "

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

                    ["Comment def or class" py-comment-def-or-class
                     :help " `py-comment-def-or-class'

Comments def-or-class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Clause "

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

                    ["Comment clause" py-comment-clause
                     :help " `py-comment-clause'

Comments clause at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Statement "

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

                    ["Comment statement" py-comment-statement
                     :help " `py-comment-statement'

Comments statement at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Expression"

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
Delete expression at point, don't store deleted string in kill-ring"])

                   ("Partial expression"

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
Delete partial-expression at point, don't store deleted string in kill-ring"])

                   ("Class "

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

                    ["Comment class" py-comment-class
                     :help " `py-comment-class'

Comments class at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   ("Def "

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

                    ["Comment def" py-comment-def
                     :help " `py-comment-def'

Comments def at point.

Uses double hash (`#') comment starter when `py-block-comment-prefix-p' is `t',
the default. "])

                   "-"

                   ("Block bol "

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
Shift block left. "])

                   ("Minor-block bol "

                    ["Beginning of minor-block bol" py-beginning-of-minor-block-bol
                     :help "`py-beginning-of-minor-block-bol'
Go to beginning of line at beginning of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["End of minor-block bol" py-end-of-minor-block-bol
                     :help "`py-end-of-minor-block-bol'
Go to beginning of line following end of minor-block.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Up minor-block bol" py-up-minor-block-bol
                     :help "`py-up-minor-block-bol'
Go to next minor-block upwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Down minor-block bol" py-down-minor-block-bol
                     :help "`py-down-minor-block-bol'
Go to next minor-block downwards in buffer if any. Go to beginning of line.

Returns position reached, if successful, nil otherwise.
A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Mark minor-block bol" py-mark-minor-block-bol
                     :help "`py-mark-minor-block-bol'
Mark minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Copy minor-block bol" py-copy-minor-block-bol
                     :help "`py-copy-minor-block-bol'
Copy minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Kill minor-block bol" py-kill-minor-block-bol
                     :help "`py-kill-minor-block-bol'
Kill minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Delete minor-block bol" py-delete-minor-block-bol
                     :help "`py-delete-minor-block-bol'
Delete minor-block at point.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block right" py-shift-minor-block-right
                     :help "`py-shift-minor-block-right'
Shift minor-block right.

A minor block is started by a `for', `if', `try' or `with'. "]

                    ["Shift minor-block left" py-shift-minor-block-left
                     :help "`py-shift-minor-block-left'
Shift minor-block left.

A minor block is started by a `for', `if', `try' or `with'. "])

                   ("Clause bol "

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
Shift clause left. "])

                   ("Block-Or-Clause bol "

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
Shift block-or-clause left. "])

                   ("Def bol "

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
Shift def left. "])

                   ("Class bol "
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
Shift class left. "])

                   ("Def-Or-Class bol "
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
Shift def-or-class left. "])

                   ("Statement bol "
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
Shift statement left. "]))
                  "-"
                  ("Filling"
                   :help "see also customizable `py-docstring-style'"

                   ["Fill string" py-fill-string
                    :help " `py-fill-string'

Uses value of `py-docstring-style', if set. "]

                   ["Fill paragraph" py-fill-paragraph
                    :help " `py-fill-paragraph'

Uses value of `py-docstring-style', if set. "]

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

See available styles at `py-fill-paragraph' or var `py-docstring-style'
 "]

                   ["py fill string onetwo" py-fill-string-onetwo
                    :help " `py-fill-string-onetwo'
One newline and start and Two at end style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257" py-fill-string-pep-257
                    :help " `py-fill-string-pep-257'

PEP-257 with 2 newlines at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string pep 257 nn" py-fill-string-pep-257-nn
                    :help " `py-fill-string-pep-257-nn'

PEP-257 with 1 newline at end of string.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"]

                   ["py fill string symmetric" py-fill-string-symmetric
                    :help " `py-fill-string-symmetric'

Symmetric style.

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

See available styles at `py-fill-paragraph' or var `py-docstring-style'"])

                  ("Electric "
                   :help "electric commands'"

                   ["Hungry delete backwards" py-hungry-delete-backwards
                    :help " `py-hungry-delete-backwards'

Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.
See also C-c <delete>. "]

                   ["Hungry delete forward" py-hungry-delete-forward
                    :help " `py-hungry-delete-forward'

Delete the following character or all following whitespace
up to the next non-whitespace character.
See also C-c <C-backspace>. "]

                   ["Electric colon" py-electric-colon
                    :help " `py-electric-colon'
Insert a colon and indent accordingly.

If a numeric argument ARG is provided, that many colons are inserted
non-electrically.

Electric behavior is inhibited inside a string or
comment or by universal prefix C-u.

Switched by `py-electric-colon-active-p', default is nil
See also `py-electric-colon-greedy-p' "]

                   ["Electric colon greedy "
                    (setq py-electric-colon-greedy-p
                          (not py-electric-colon-greedy-p))
                    :help "If py-electric-colon should indent to the outmost reasonable level.

If nil, default, it will not move from at any reasonable level. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-greedy-p]

                   ["Electric colon newline and indent "
                    (setq py-electric-colon-newline-and-indent-p
                          (not py-electric-colon-newline-and-indent-p))
                    :help "If non-nil, `py-electric-colon' will call `newline-and-indent'.  Default is `nil'. Use `M-x customize-variable' to set it permanently"
                    :style toggle :selected py-electric-colon-newline-and-indent-p]

                   ["Electric delete" py-electric-delete
                    :help " `py-electric-delete'
Delete following character or levels of whitespace.

With ARG do that ARG times. "]

                   ["Electric backspace" py-electric-backspace
                    :help " `py-electric-backspace'
Delete preceding character or level of indentation.

With ARG do that ARG times.
Returns column reached. "]

                   ["Electric comment" py-electric-comment
                    :help " `py-electric-comment'
Insert a comment. If starting a comment, indent accordingly.

If a numeric argument ARG is provided, that many \"#\" are inserted
non-electrically.
With C-u \"#\" electric behavior is inhibited inside a string or comment. "]

                   ["Electric left paren" py-complete-electric-lparen
                    :help " `py-complete-electric-lparen'
electricly insert '(', and try to get a signature for the stuff to the left.\n
Needs Pymacs"]

                   ["Complete electric comma" py-complete-electric-comma
                    :help " `py-complete-electric-comma'
electricly insert ',', and redisplay latest signature.\n
Needs Pymacs"]

                   ["Electric yank" py-electric-yank
                    :help " `py-electric-yank'
Perform command `yank' followed by an `indent-according-to-mode' . "])

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

                  ["Find function" py-find-function
                   :help "`py-find-function'
Try to find source definition of function at point"]))))
        map))
;; avoid errors from ipython.el - which isn't needed anymore
(defvaralias 'py-shell-map 'py-shell-mode-map)

(when py-org-cycle-p
  (define-key python-mode-map (kbd "<backtab>") 'org-cycle))

(defun py--kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapc #'(lambda (filename)
            (ignore-errors (delete-file filename)))
        py-file-queue))

;; Add a designator to the minor mode strings
(or (assq 'py-pdbtrack-is-tracking-p minor-mode-alist)
    (push '(py-pdbtrack-is-tracking-p py-pdbtrack-minor-mode-string)
          minor-mode-alist))

;; bottle.py
;; py   = sys.version_info
;; py3k = py >= (3,0,0)
;; py25 = py <  (2,6,0)
;; py31 = (3,1,0) <= py < (3,2,0)

;; sys.version_info[0]
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
  "`comint-input-filter' function for Python process.
Don't save anything for STR matching `py-history-filter-regexp'."
  (not (string-match py-history-filter-regexp str)))

(defun py-load-file (file-name)
  "Load a Python file FILE-NAME into the Python process.

If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
  (interactive "f")
  (py--execute-file-base (get-buffer-process (get-buffer (py-shell))) file-name))

(defun py-proc (&optional argprompt)
  "Return the current Python process.

Start a new process if necessary. "
  (interactive "P")
  (let ((erg
         (cond ((comint-check-proc (current-buffer))
		(get-buffer-process (buffer-name (current-buffer))))
	       (t (py-shell argprompt)))))
    (when (interactive-p) (message "%S" erg))
    erg))

;;; Miscellany.
(defun py--shell-simple-send (proc string)
  (let* ((strg (substring-no-properties string))
         (nln (string-match "\n$" strg)))
    ;; (or nln (setq strg (concat strg "\n")))
    ;; (comint-simple-send proc (substring-no-properties string))
    (process-send-string proc strg)
    (or nln (process-send-string proc "\n"))))

(defalias
  'py-shell-redirect-send-command-to-process
  'comint-redirect-send-command-to-process)
(defalias
  'py-shell-dynamic-simple-complete
  'comint-dynamic-simple-complete)

;;;; Modes.
;; pdb tracking is alert once this file is loaded, but takes no action if

;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;      gud-pdb-command-name (symbol-name pdb-path))

(unless py-separator-char (setq py-separator-char (py-update-separator-char)))

;;; Hooks
;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py--kill-emacs-hook)

(when py--warn-tmp-files-left-p
  (add-hook 'python-mode-hook 'py--warn-tmp-files-left))


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

;; backward compatibility
(defalias 'py-switch-shells 'py-switch-shell)
(defalias 'py-toggle-shell 'py-switch-shell)
(defun py-switch-shell (&optional arg)
  "Toggles between the interpreter customized in `py-shell-toggle-1' resp. `py-shell-toggle-2'. Was hard-coded CPython and Jython in earlier versions, now starts with Python2 and Python3 by default.

ARG might be a python-version string to set to.

\\[universal-argument] `py-toggle-shell' prompts to specify a reachable Python command.
\\[universal-argument] followed by numerical arg 2 or 3, `py-toggle-shell' opens a respective Python shell.
\\[universal-argument] followed by numerical arg 5 opens a Jython shell.

Should you need more shells to select, extend this command by adding inside the first cond:

                    ((eq NUMBER (prefix-numeric-value arg))
                     \"MY-PATH-TO-SHELL\")"
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
                 py-which-bufname (py--choose-buffer-name)
                 msg "CPython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "jython" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
                 msg "Jython"
                 mode-name (py--choose-buffer-name)))
          ((string-match "python" name)
           (setq py-shell-name name
                 py-which-bufname (py--choose-buffer-name)
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
    (add-hook 'python-mode-hook
              (lambda ()
                (setq py-this-abbrevs-changed abbrevs-changed)
                (load abbrev-file-name nil t)
                (setq abbrevs-changed py-this-abbrevs-changed)))
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
                (py-end-of-block-or-clause))
              nil))

;;;

(defun py--input-filter (str)
  "`comint-input-filter' function for Python.

Don't save anything for STR matching `py-input-filter-re' "
  (not (string-match py-input-filter-re str)))

(make-obsolete 'jpython-mode 'jython-mode nil)

(add-to-list 'same-window-buffer-names (purecopy "*Python*"))
(add-to-list 'same-window-buffer-names (purecopy "*IPython*"))

(add-to-list 'auto-mode-alist (cons (purecopy "\\.py\\'")  'python-mode))

;; (add-to-list 'interpreter-mode-alist
;; (cons (purecopy "[bi]*python[0-9.]*") 'python-mode))
;; 
;; (add-to-list 'interpreter-mode-alist
;; (cons (purecopy "jython[0-9.]*") 'jython-mode))

(add-to-list 'magic-mode-alist
	     '("!#[ \t]*/.*[jp]ython[0-9.]*" . python-mode))

;; lp:1355458, what about using `magic-mode-alist'?

(defun py--set-auto-fill-values ()
  "Internal use by `py--run-auto-fill-timer'"
  (let ((pps (syntax-ppss)))
    (cond ((and (nth 4 pps)(numberp py-comment-fill-column))
           (setq fill-column py-comment-fill-column))
          ((and (nth 3 pps)(numberp py-docstring-fill-column))
           (set (make-local-variable 'fill-column) py-docstring-fill-column))
          (t (setq fill-column py-fill-column-orig)))))

(defun py--run-auto-fill-timer ()
  "Set fill-column to values of `py-docstring-fill-column' resp. to `py-comment-fill-column' according to environment. "
  (when py-auto-fill-mode
    (unless py-autofill-timer
      (setq py-autofill-timer
            (run-with-idle-timer
             py-autofill-timer-delay t
             'py--set-auto-fill-values)))))

(defvar py--timer nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer)

(defvar py--timer-delay nil
  "Used by `py--run-unfontify-timer'")
(make-variable-buffer-local 'py--timer-delay)

(defun py--run-unfontify-timer (&optional buffer)
  "Unfontify the shell banner-text "
  (when py--shell-unfontify
    (let ((buffer (or buffer (current-buffer)))
	  done)
      (if (and (buffer-live-p buffer)(eq major-mode 'py-shell-mode))
	  (unless py--timer
	    (setq py--timer
		  (run-with-idle-timer
		   (if py--timer-delay (setq py--timer-delay 3)
		     (setq py--timer-delay 0.1))
		   t
		   #'py--unfontify-banner buffer)))
	(cancel-timer py--timer)))))

;;; unconditional Hooks
;; (orgstruct-mode 1)
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'py--imenu-create-index-function)
	    (setq indent-tabs-mode py-indent-tabs-mode)))

(remove-hook 'python-mode-hook 'python-setup-brm)
;;;

(defun py-complete-auto ()
  "Auto-complete function using py-complete. "
  ;; disable company
  ;; (when company-mode (company-mode))
  (let ((modified (buffer-chars-modified-tick)))
    ;; don't try completion if buffer wasn't modified
    (unless (eq modified py-complete-last-modified)
      (if py-auto-completion-mode-p
	  (if (string= "*PythonCompletions*" (buffer-name (current-buffer)))
	      (sit-for 1 t)
	    (if
		(eq py-auto-completion-buffer (current-buffer))
		;; not after whitespace, TAB or newline
		(unless (member (char-before) (list 32 9 10))
		  (py-complete)
		  (setq py-complete-last-modified (buffer-chars-modified-tick)))
	      (setq py-auto-completion-mode-p nil
		    py-auto-completion-buffer nil)
	      (cancel-timer py--auto-complete-timer)))))))

(define-derived-mode py-auto-completion-mode python-mode "Pac"
  "Run auto-completion"
  ;; disable company
  ;; (when company-mode (company-mode))
  (if py-auto-completion-mode-p
      (progn
	(setq py-auto-completion-mode-p nil
	      py-auto-completion-buffer nil)
	(when (timerp py--auto-complete-timer)(cancel-timer py--auto-complete-timer)))
    (setq py-auto-completion-mode-p t
	  py-auto-completion-buffer (current-buffer))
    (setq py--auto-complete-timer
	  (run-with-idle-timer
	   py--auto-complete-timer-delay
	   ;; 1
	   t
	   #'py-complete-auto))))

;; (add-hook 'after-change-major-mode-hook #'py-protect-other-buffers-ac)

;; after-change-major-mode-hook

;;;
(define-derived-mode python-mode fundamental-mode python-mode-modeline-display
  "Major mode for editing Python files.

To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS

`py-shell'\tStart an interactive Python interpreter in another window
`py-execute-statement'\tSend statement at point to a Python interpreter
`py-beginning-of-statement'\tGo to the initial line of a simple statement

etc.

See available commands listed in files commands-python-mode at directory doc

VARIABLES

`py-indent-offset'	indentation increment
`py-shell-name'		shell command to invoke Python interpreter
`py-split-windows-on-execute-p'		When non-nil split windows
`py-switch-buffers-on-execute-p'	When non-nil switch to the Python output buffer

See available customizations listed in files variables-python-mode at directory doc

\\{python-mode-map}"
  :group 'python-mode
  ;; Local vars
  (set (make-local-variable 'electric-indent-inhibit) nil)
  (set (make-local-variable 'outline-regexp)
       (concat (mapconcat 'identity
                          (mapcar #'(lambda (x) (concat "^\\s-*" x "\\_>"))
                                  py-outline-mode-keywords)
                          "\\|")))
  (if py-use-font-lock-doc-face-p
      (set (make-local-variable 'font-lock-defaults)
           '(python-font-lock-keywords nil nil nil nil
				       (font-lock-syntactic-keywords
					. py-font-lock-syntactic-keywords)
				       (font-lock-syntactic-face-function
					. py--font-lock-syntactic-face-function)))
    (set (make-local-variable 'font-lock-defaults)
         '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  ;; avoid to run py-choose-shell again from `py--fix-start'
  (cond ((and (boundp 'py-buffer-name) py-buffer-name)
	 (if (string-match "python3" py-buffer-name)
	     (font-lock-add-keywords 'python-mode
				     '(("\\<print\\>" . 'py-builtins-face)))
	   '(("\\<print\\>" . 'font-lock-keyword-face))))
	((string-match "python3" (py-choose-shell))
	 (font-lock-add-keywords 'python-mode
				 '(("\\<print\\>" . 'py-builtins-face))))
	(t (font-lock-add-keywords 'python-mode
				   '(("\\<print\\>" . 'font-lock-keyword-face)))))

  (set (make-local-variable 'which-func-functions) 'py-which-def-or-class)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start) "#")
  (if empty-comment-line-separates-paragraph-p
      (progn
        (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
        (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
    (set (make-local-variable 'paragraph-separate) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$")
    (set (make-local-variable 'paragraph-start) "\f\\|^[ \t]*$\\|^[ \t]*#[ \t]*$\\|^[ \t\f]*:[[:alpha:]]+ [[:alpha:]]+:.+$"))
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'hs-hide-comments-when-hiding-all) 'py-hide-comments-when-hiding-all)
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'add-log-current-defun-function) 'py-current-defun)
  (set (make-local-variable 'fill-paragraph-function) 'py-fill-paragraph)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'tab-width) py-indent-offset)
  (set (make-local-variable 'eldoc-documentation-function)
       #'py-eldoc-function)
  (and py-load-skeletons-p
       (py-load-skeletons)
       (set (make-local-variable 'skeleton-further-elements)
            '((< '(backward-delete-char-untabify (min py-indent-offset
                                                      (current-column))))
              (^ '(- (1+ (current-indentation)))))))
  ;; (set (make-local-variable 'imenu-create-index-function) 'py--imenu-create-index-function)
  (setq imenu-create-index-function 'py--imenu-create-index-function)

  (and py-guess-py-install-directory-p (py-set-load-path))
  ;;  (unless gud-pdb-history (when (buffer-file-name) (add-to-list 'gud-pdb-history (buffer-file-name))))
  (and py-autopair-mode
       (load-library "autopair")
       (add-hook 'python-mode-hook
                 #'(lambda ()
                     (setq autopair-handle-action-fns
                           (list #'autopair-default-handle-action
                                 #'autopair-python-triple-quote-action))))
       (py-autopair-mode-on))
  (when py-trailing-whitespace-smart-delete-p
    (add-hook 'before-save-hook 'delete-trailing-whitespace nil 'local))
  (when py-pdbtrack-do-tracking-p
    (add-hook 'comint-output-filter-functions 'py--pdbtrack-track-stack-file t))
  (cond
   (py-complete-function
    (add-hook 'completion-at-point-functions
              py-complete-function nil 'local))
   (py-load-pymacs-p
    (add-hook 'completion-at-point-functions
              'py-complete-completion-at-point nil 'local))
   (t
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)))
  ;; (if py-auto-complete-p
  ;; (add-hook 'python-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'python-mode-hook 'py--run-completion-timer))
  ;; (when py-auto-complete-p
  ;; (add-hook 'python-mode-hook
  ;; (lambda ()
  ;; (run-with-idle-timer 1 t 'py-shell-complete))))
  (if py-auto-fill-mode
      (add-hook 'python-mode-hook 'py--run-auto-fill-timer)
    (remove-hook 'python-mode-hook 'py--run-auto-fill-timer))

  ;; caused insert-file-contents error lp:1293172
  ;;  (add-hook 'after-change-functions 'py--after-change-function nil t)
  (if py-defun-use-top-level-p
      (progn
        (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-top-level)
        (set (make-local-variable 'end-of-defun-function) 'py-end-of-top-level)
        (define-key python-mode-map [(control meta a)] 'py-beginning-of-top-level)
        (define-key python-mode-map [(control meta e)] 'py-end-of-top-level))
    (set (make-local-variable 'beginning-of-defun-function) 'py-beginning-of-def-or-class)
    (set (make-local-variable 'end-of-defun-function) 'py-end-of-def-or-class)
    (define-key python-mode-map [(control meta a)] 'py-beginning-of-def-or-class)
    (define-key python-mode-map [(control meta e)] 'py-end-of-def-or-class))
  (when (and py--imenu-create-index-p
             (fboundp 'imenu-add-to-menubar)
             (ignore-errors (require 'imenu)))
    (setq imenu--index-alist (funcall py--imenu-create-index-function))
    ;; (setq imenu--index-alist (py--imenu-create-index-new))
    ;; (message "imenu--index-alist: %s" imenu--index-alist)
    (imenu-add-to-menubar "PyIndex"))
  ;; add the menu
  (when py-menu
    (easy-menu-add py-menu))
  (when py-hide-show-minor-mode-p (hs-minor-mode 1))
  (when py-outline-minor-mode-p (outline-minor-mode 1))
  (when (interactive-p) (message "python-mode loaded from: %s" python-mode-message-string)))

(define-derived-mode py-shell-mode comint-mode "Py"
  "Major mode for interacting with a Python process.
A Python process can be started with \\[py-shell].

You can send text to the Python process from other buffers
containing Python source.
 * \\[py-execute-region] sends the current region to the Python process.

Sets basic comint variables, see also versions-related stuff in `py-shell'.
\\{py-shell-mode-map}"
  :group 'python-mode
  ;; (require 'ansi-color) ; for ipython
  (setq mode-line-process '(":%s"))
  (when py-fontify-shell-buffer-p
    (set (make-local-variable 'font-lock-defaults)
	 '(python-font-lock-keywords nil nil nil nil
				     (font-lock-syntactic-keywords
				      . py-font-lock-syntactic-keywords))))
  (setenv "PAGER" "cat")
  (setenv "TERM" "dumb")
  (set-syntax-table python-mode-syntax-table)
  (set (make-local-variable 'py--shell-unfontify) 'py-shell-unfontify-p)
  ;; (if py-auto-complete-p
  ;; (add-hook 'py-shell-mode-hook 'py--run-completion-timer)
  ;; (remove-hook 'py-shell-mode-hook 'py--run-completion-timer))
  (if py-shell-unfontify-p
      (add-hook 'py-shell-mode-hook #'py--run-unfontify-timer (current-buffer))
    (remove-hook 'py-shell-mode-hook 'py--run-unfontify-timer))

  ;; comint settings
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
  (remove-hook 'comint-output-filter-functions 'font-lock-extend-jit-lock-region-after-change t)

  (make-local-variable 'comint-output-filter-functions)
  ;; (set (make-local-variable 'comint-input-filter) 'py--input-filter)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  (set (make-local-variable 'comint-input-filter) 'py-history-input-filter)
  (set (make-local-variable 'comint-prompt-read-only) py-shell-prompt-read-only)
  ;; It might be useful having a different setting of `comint-use-prompt-regexp' in py-shell - please report when a use-case shows up
  ;; (set (make-local-variable 'comint-use-prompt-regexp) nil)
  (set (make-local-variable 'compilation-error-regexp-alist)
       py-compilation-regexp-alist)
  ;; (setq completion-at-point-functions nil)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "^[ \t]*#+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) #'py--comment-indent-function)
  (set (make-local-variable 'indent-region-function) 'py-indent-region)
  (set (make-local-variable 'indent-line-function) 'py-indent-line)
  (set (make-local-variable 'inhibit-point-motion-hooks) t)
  (set (make-local-variable 'comint-input-sender) 'py--shell-simple-send)
  ;; (sit-for 0.1)
  (setq comint-input-ring-file-name
        (cond ((string-match "[iI][pP]ython[[:alnum:]*-]*$" py-buffer-name)
               (if py-honor-IPYTHONDIR-p
                   (if (getenv "IPYTHONDIR")
                       (concat (getenv "IPYTHONDIR") "/history")
                     py-ipython-history)
                 py-ipython-history))
              (t
               (if py-honor-PYTHONHISTORY-p
                   (if (getenv "PYTHONHISTORY")
                       (concat (getenv "PYTHONHISTORY") "/" (py--report-executable py-buffer-name) "_history")
                     py-ipython-history)
                 py-ipython-history))))
  (comint-read-input-ring t)
  (compilation-shell-minor-mode 1)
  ;;
  (if py-complete-function
      (progn
  	(add-hook 'completion-at-point-functions
  		  py-complete-function nil 'local)
  	(add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		     py-complete-function))
    (add-hook 'completion-at-point-functions
              'py-shell-complete nil 'local)
    (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
  		 'py-shell-complete))
  (when py-shell-menu
    (easy-menu-add py-menu)))

;;; python-components-mode.el ends here
