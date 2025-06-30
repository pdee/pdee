;;; python-components-vars.el --- Edit, debug, develop, run Python programs. -*- lexical-binding: t; -*-

;; Version: 6.3.1

;; URL: https://gitlab.com/groups/python-mode-devs

;; Package-Requires: ((emacs "24"))

;; Author: 2015-2025 https://gitlab.com/groups/python-mode-devs
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

;; As for ‘py-add-abbrev’:
;; Similar to ‘add-mode-abbrev’, but uses
;; ‘py-partial-expression’ before point for expansion to
;; store, not ‘word’.  Also provides a proposal for new
;; abbrevs.

;; Proposal for an abbrev is composed from the downcased
;; initials of expansion - provided they are of char-class
;; [:alpha:]
;;
;; For example code below would be recognised as a
;; ‘py-expression’ composed by three
;; py-partial-expressions.
;;
;; OrderedDict.popitem(last=True)
;;
;; Putting the curser at the EOL, M-3 M-x py-add-abbrev
;;
;; would prompt "op" for an abbrev to store, as first
;; ‘py-partial-expression’ beginns with a "(", which is
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
(require 'hideshow)
(require 'json)
(require 'shell)
(require 'thingatpt)
(require 'which-func)
(require 'tramp)
(require 'tramp-sh)
(require 'org-loaddefs)
(unless (functionp 'mapcan)
  (require 'cl-extra)
  ;; mapcan does not exist in Emacs 25
  (defalias 'mapcan 'cl-mapcan)
  )

(define-minor-mode py-electric-backspace-mode
  "When on, <backspace> key will delete all whitespace chars before point.

Default is nil"
  :group 'python-mode
  :lighter " eb"
  (if py-electric-backspace-mode
      (if (ignore-errors (functionp 'keymap-local-set))
          (keymap-local-set "<backspace>" 'py-electric-backspace)
        (local-set-key "<backspace>" 'py-electric-backspace))
    (if (ignore-errors (functionp 'keymap-local-unset))
        (keymap-local-unset "<backspace>")
      (local-unset-key "<backspace>"))))

(defvar py-comment-start-re           "[ \t]*#"
  "Beginning of a comment")

(defvar py-comment-end-re "\n"
  "Regexp to match the end of a comment.")

(defvar py-comment-start-skip-re      "#+ *"
"Regexp to match the start of a comment plus everything up to its body.")

(defvar comint-mime-setup-script-dir nil
  "Avoid compiler warning")

(defvar comint-mime-enabled-types nil
  "Avoid compiler warning")

(defvar comint-mime-setup-function-alist nil
  "Avoid compiler warning")

(defvar comint-mime-setup-function-alist nil
  "Avoid compiler warning")

(defgroup python-mode nil
  "Support for the programming language"
  :group 'languages
  :prefix "py-")

(defconst py-version "6.3.1")

(defvar py-install-directory nil
  "Make sure it exists.")

(defcustom py-install-directory nil
  "Directory where python-mode.el and its subdirectories should be installed.

Needed for completion and other environment stuff only."

  :type 'string
  :tag "py-install-directory"
  :group 'python-mode)

(defcustom py-font-lock-defaults-p t
  "If fontification is not required, avoiding it might speed up things."

  :type 'boolean
  :tag "py-font-lock-defaults-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-register-shell-buffer-p nil
  "If non-nil, register new py-shell according to py-register-char as REGISTER.

Default is nil.
See ‘window-configuration-to-register’"

  :type 'boolean
  :tag "py-register-shell-buffer-p"
  :group 'python-mode
  :safe 'booleanp)

(defcustom py-register-char ?y
  "Char used by py-register-shell-buffer-p

Default is ‘y’.
See also ‘window-configuration-to-register’"

  :type 'char
  :tag "py-register-char"
  :group 'python-mode
  :safe 'characterp)

(defcustom py-pythonpath ""
  "Define $PYTHONPATH here, if needed.

Emacs does not read .bashrc"

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

Make sure it is accessible from Emacs by \\<emacs-lisp-mode-map> \\[info] ...
See INSTALL-INFO-FILES for help."

  :type 'string
  :tag "info-lookup-mode"
  :group 'python-mode)

(defcustom py-fast-process-p nil
  "Use ‘py-fast-process’.

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
          (group (1+ (not (any "\"<")))) ; avoid ‘<stdin>’ &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "‘compilation-error-regexp-alist’ for ‘py-shell’."
  :type '(alist string)
  :tag "py-shell-compilation-regexp-alist"
  :group 'python-mode)

(defcustom py-shift-require-transient-mark-mode-p t
  "If py-shift commands require variable ‘transient-mark-mode’ set to t.

Default is t"

  :type 'boolean
  :tag "py-shift-require-transient-mark-mode-p"
  :group 'python-mode
  :safe 'booleanp)

(defvar py-fast-output-buffer "*Python Fast*"
  "Internally used. ‘buffer-name’ for fast-processes.")

(defvar py-this-result nil
  "Internally used, store return-value.")

(defconst py-coding-re
  "\\(# *coding[ \t]*=\\|#[ \t]*\-*\-[ \t]*coding:\\|#[ \t]*encoding:\\)[ \t]*\\([[:graph:]+]\\)"
 "Fetch the coding cookie maybe.")

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

(defvar py-chars-before " \t\n\r\f"
  "Used by ‘py--string-strip’.")

(defvar py-chars-after " \t\n\r\f"
    "Used by ‘py--string-strip’.")

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

\\<python-mode-map> ‘py-remove-overlays-at-point’ removes that highlighting."
  :type 'boolean
  :tag "py-highlight-error-source-p"
  :group 'python-mode)

(defcustom py-set-pager-cat-p nil
  "If the shell environment variable $PAGER should set to ‘cat’.

Avoids lp:783828,
 \"Terminal not fully functional\", for help('COMMAND') in python-shell

When non-nil, imports module ‘os’"

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

When non-nil and ‘py-session-p’, an existing
dedicated process is re-used instead of default
 - which allows executing stuff in parallel."
  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defcustom py-store-result-p nil
  "Put resulting string of ‘py-execute-...’ into ‘kill-ring’.

Default is nil"

  :type 'boolean
  :tag "py-dedicated-process-p"
  :group 'python-mode)

(defvar py-shell--font-lock-buffer "*PSFLB*"
  "May contain the ‘py-buffer-name’ currently fontified." )

(defvar py-return-result-p nil
  "Internally used.

When non-nil, return resulting string of ‘py-execute-...’.
Imports will use it with nil.
Default is nil")

(defcustom py--execute-use-temp-file-p nil
 "Assume execution at a remote machine.

 where write-access is not given."

 :type 'boolean
 :tag "py--execute-use-temp-file-p"
 :group 'python-mode)

(defvar py--match-paren-forward-p nil
  "Internally used by ‘py-match-paren’.")

(defvar py-new-session-p t
  "Internally used.  See lp:1393882.

Restart ‘py-shell’ once with new Emacs/‘python-mode’.")

(defcustom py-electric-close-active-p nil
  "Close completion buffer if no longer needed.

Works around a bug in ‘choose-completion’.
Default is nil"
  :type 'boolean
  :tag "py-electric-close-active-p"
  :group 'python-mode)

(defcustom py-hide-show-minor-mode-p nil
  "If hide-show minor-mode should be on, default is nil."

  :type 'boolean
  :tag "py-hide-show-minor-mode-p"
  :group 'python-mode)

(defcustom py-do-completion-p t
  "Permits disabling all python-mode native completion.

Default is ‘t’.
See #144, how to disable process spawn for autocompletion"

  :type 'boolean
  :tag "py-do-completion-p"
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
  "If documention string inside of def or class get ‘font-lock-doc-face’.

‘font-lock-doc-face’ inherits ‘font-lock-string-face’.
Call \\<emacs-lisp-mode-map> \\[customize-face] in order to have a effect."

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
in ‘py-comment-fill-column’ and ‘py-docstring-fill-column’.
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

  "If a new comint buffer is connected to Python.
Commands like completion might need some delay."

  :type 'float
  :tag "py-new-shell-delay"
  :group 'python-mode)

(defcustom py-autofill-timer-delay 1
  "Delay when idle."
  :type 'integer
  :tag "py-autofill-timer-delay"
  :group 'python-mode)

(defcustom py-docstring-fill-column 72
  "Value of ‘fill-column’ to use when filling a docstring.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current ‘fill-column’" t))
  :tag "py-docstring-fill-column"
  :group 'python-mode)

(defcustom py-comment-fill-column 79
  "Value of ‘fill-column’ to use when filling a comment.
Any non-integer value means do not use a different value of
‘fill-column’ when filling docstrings."
  :type '(choice (integer)
                 (const :tag "Use the current ‘fill-column’" t))
  :tag "py-comment-fill-column"
  :group 'python-mode)

(defcustom py-fontify-shell-buffer-p nil
  "If code in Python shell should be highlighted as in script buffer.

Default is nil.

If t, related vars like ‘comment-start’ will be set too.
Seems convenient when playing with stuff in IPython shell
Might not be TRT when a lot of output arrives"

  :type 'boolean
  :tag "py-fontify-shell-buffer-p"
  :group 'python-mode)

(defvar py-modeline-display ""
  "Internally used.")

(defcustom py-modeline-display-full-path-p nil
  "If the full PATH/TO/PYTHON be in modeline.

Default is nil. Note: when ‘py-python-command’ is
specified with path, it is shown as an acronym in
‘buffer-name’ already."

  :type 'boolean
  :tag "py-modeline-display-full-path-p"
  :group 'python-mode)

(defcustom py-modeline-acronym-display-home-p nil
  "If the modeline acronym should contain chars indicating the home-directory.

Default is nil"
  :type 'boolean
  :tag "py-modeline-acronym-display-home-p"
  :group 'python-mode)

(defvar highlight-indent-active nil)
;; (defvar autopair-mode nil)

(defvar-local py--editbeg nil
  "Internally used by ‘py-edit-docstring’ and others")

(defvar-local py--editend nil
  "Internally used by ‘py-edit-docstring’ and others")

(defvar py--oldbuf nil
  "Internally used by ‘py-edit-docstring’.")

(defvar py-edit-buffer "Edit docstring"
  "Name of the temporary buffer to use when editing.")

(defvar py--edit-register nil)

(defvar py-result nil
  "Internally used.  May store result from Python process.

See var ‘py-return-result-p’ and command ‘py-toggle-py-return-result-p’")

(defvar py-error nil
  "Takes the error-messages from Python process.")

(defvar py-python-completions "*Python Completions*"
  "Buffer name for Python-shell completions, internally used.")

(defvar py-ipython-completions "*IPython Completions*"
  "Buffer name for IPython-shell completions, internally used.")

(defcustom py-timer-close-completions-p t
  "If ‘py-timer-close-completion-buffer’ should run, default is non-nil."

  :type 'boolean
  :tag "py-timer-close-completions-p"
  :group 'python-mode)

;; (defcustom py-autopair-mode nil
;;   "If ‘python-mode’ calls (autopair-mode-on)

;; Default is nil
;; Load ‘autopair-mode’ written by Joao Tavora <joaotavora [at] gmail.com>
;; URL: http://autopair.googlecode.com"
;;   :type 'boolean
;;   :tag "py-autopair-mode"
;;   :group 'python-mode)

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
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py-complete-last-modified nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py--auto-complete-timer nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py-auto-completion-buffer nil
  "Internally used by ‘py-auto-completion-mode’.")

(defvar py--auto-complete-timer-delay 1
  "Seconds Emacs must be idle to trigger auto-completion.

See ‘py-auto-completion-mode’")

(defcustom py-auto-complete-p nil
  "Run python-mode's built-in auto-completion via ‘py-complete-function’.

Default is  nil."

  :type 'boolean
  :tag "py-auto-complete-p"
  :group 'python-mode)

(defcustom py-tab-shifts-region-p nil
  "If t, TAB will indent/cycle the region, not just the current line.

Default is  nil
See also ‘py-tab-indents-region-p’"

  :type 'boolean
  :tag "py-tab-shifts-region-p"
  :group 'python-mode)

(defcustom py-tab-indents-region-p nil
  "When t and first TAB does not shift, ‘indent-region’ is called.

Default is  nil
See also ‘py-tab-shifts-region-p’"

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
  "When non-nil, command ‘org-cycle’ is available at shift-TAB, <backtab>.

Default is nil."
  :type 'boolean
  :tag "py-org-cycle-p"
  :group 'python-mode)

(defcustom py-set-complete-keymap-p  nil
  "If ‘py-complete-initialize’.

Sets up enviroment for Pymacs based py-complete.
 Should load its keys into ‘python-mode-map’
Default is nil.
See also resp. edit ‘py-complete-set-keymap’"

  :type 'boolean
  :tag "py-set-complete-keymap-p"
  :group 'python-mode)

(defcustom py-outline-minor-mode-p t
  "If outline minor-mode should be on, default is t."
  :type 'boolean
  :tag "py-outline-minor-mode-p"
  :group 'python-mode)

(defvar py-guess-py-install-directory-p nil
  "If in cases, ‘py-install-directory’ is not set,  ‘py-set-load-path’ guess it.")

(defcustom py-guess-py-install-directory-p nil
  "If in cases, ‘py-install-directory’ is not set, ‘py-set-load-path’ guesses it."
  :type 'boolean
  :tag "py-guess-py-install-directory-p"
  :group 'python-mode)

(defcustom py-load-pymacs-p nil
  "If Pymacs related stuff should be loaded. Default is nil.

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
  "Called instead of ‘forward-sexp’, ‘backward-sexp’.

Default is nil."

  :type '(choice

          (const :tag "default" nil)
          (const :tag "py-forward-partial-expression" py-forward-partial-expression)
          (const :tag "py-forward-expression" py-forward-expression))
  :tag "py-sexp-function"
  :group 'python-mode)

(defcustom py-close-provides-newline t
  "If a newline is inserted, when line after block is not empty.

Default is non-nil.
When non-nil, ‘py-forward-def’ and related will work faster"
  :type 'boolean
  :tag "py-close-provides-newline"
  :group 'python-mode)

(defcustom py-dedent-keep-relative-column t
  "If point should follow dedent or kind of electric move to end of line.

Default is t - keep relative position."
  :type 'boolean
  :tag "py-dedent-keep-relative-column"
  :group 'python-mode)

(defcustom py-indent-list-style 'line-up-with-first-element
  "Sets the basic indentation style of lists.

The term ‘list’ here is seen from Emacs Lisp editing purpose.
A list symbolic expression means everything delimited by
brackets, parentheses or braces.

Setting here might be ignored in case of canonical indent.

‘line-up-with-first-element’ indents to 1+ column
of opening delimiter

def foo (a,
         b):

but ‘one-level-to-beginning-of-statement’ in case of EOL at list-start

def foo (
    a,
    b):

‘one-level-to-beginning-of-statement’ adds
‘py-indent-offset’ to beginning

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
          (const :tag "one-level-from-first-element" one-level-from-first-element)
          )
  :tag "py-indent-list-style"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-list-style)

(defcustom py-closing-list-dedents-bos nil
  "When non-nil, indent lists closing delimiter like start-column.

It will be lined up under the first character of
 the line that starts the multi-line construct, as in:

my_list = [
    1, 2, 3,
    4, 5, 6
]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
)

Default is nil, i.e.

my_list = [
    1, 2, 3,
    4, 5, 6
    ]

result = some_function_that_takes_arguments(
    \\='a\\=', \\='b\\=', \\='c\\=',
    \\='d\\=', \\='e\\=', \\='f\\='
    )

Examples from PEP8
URL: https://www.python.org/dev/peps/pep-0008/#indentation"
  :type 'boolean
  :tag "py-closing-list-dedents-bos"
  :group 'python-mode)

(defvar py-imenu-max-items 99)
(defcustom py-imenu-max-items 99
 "Python-mode specific ‘imenu-max-items’."
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

See lp:1235375. In case code is not to navigate due to errors,
command ‘which-function-mode’ and others might make Emacs hang.

Rather exit than."

  :type 'number
  :tag "py-max-specpdl-size"
  :group 'python-mode)

(defcustom py-closing-list-keeps-space nil
  "If non-nil, closing parenthesis dedents onto column of opening.
Adds ‘py-closing-list-space’.
Default is nil."
  :type 'boolean
  :tag "py-closing-list-keeps-space"
  :group 'python-mode)

(defcustom py-electric-colon-active-p nil
  "‘py-electric-colon’ feature.

Default is nil.  See lp:837065 for discussions.
See also ‘py-electric-colon-bobl-only’"
  :type 'boolean
  :tag "py-electric-colon-active-p"
  :group 'python-mode)

(defcustom py-electric-colon-bobl-only t

  "When inserting a colon, do not indent lines unless at beginning of block.

See lp:1207405 resp. ‘py-electric-colon-active-p’"

  :type 'boolean
  :tag "py-electric-colon-bobl-only"
  :group 'python-mode)

(defcustom py-electric-yank-active-p nil
  "When non-nil, ‘yank’ will be followed by an ‘indent-according-to-mode’.

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
  "If non-nil, ‘py-electric-colon’ will call ‘newline-and-indent’.

Default is nil."
  :type 'boolean
  :tag "py-electric-colon-newline-and-indent-p"
  :group 'python-mode)

(defcustom py-electric-comment-p nil
  "If \"#\" should call ‘py-electric-comment’. Default is nil."
  :type 'boolean
  :tag "py-electric-comment-p"
  :group 'python-mode)

(defcustom py-electric-comment-add-space-p nil
  "If ‘py-electric-comment’ should add a space.  Default is nil."
  :type 'boolean
  :tag "py-electric-comment-add-space-p"
  :group 'python-mode)

(defcustom py-defun-use-top-level-p nil
 "If ‘beginning-of-defun’, ‘end-of-defun’ calls function ‘top-level’ form.

Default is nil.

beginning-of defun, ‘end-of-defun’ forms use
commands ‘py-backward-top-level’, ‘py-forward-top-level’

‘mark-defun’ marks function ‘top-level’ form at point etc."

 :type 'boolean
  :tag "py-defun-use-top-level-p"
 :group 'python-mode)

(defcustom py-tab-indent t
  "Non-nil means TAB in Python mode calls ‘py-indent-line’."
  :type 'boolean
  :tag "py-tab-indent"
  :group 'python-mode)

(defcustom py-return-key 'py-newline-and-indent
  "Which command <return> should call."
  :type '(choice

          (const :tag "default" py-newline-and-indent)
          (const :tag "newline" newline)
          (const :tag "py-newline-and-dedent" py-newline-and-dedent)
          )
  :tag "py-return-key"
  :group 'python-mode)

(defcustom py-complete-function 'py-fast-complete
  "When set, enforces function todo completion, default is ‘py-fast-complete’.

Might not affect IPython, as ‘py-shell-complete’ is the only known working here.
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
  "When line starts a multiline-assignment.

How many colums indent more than opening bracket, brace or parenthesis."
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
  "Python-mode starts ‘indent-tabs-mode’ with the value specified here.

Default is nil."
  :type 'boolean
  :tag "py-indent-tabs-mode"
  :group 'python-mode)

(defcustom py-smart-indentation nil
  "Guess ‘py-indent-offset’.  Default is nil.

Setting it to t seems useful only in cases where customizing
‘py-indent-offset’ is no option - for example because the
indentation step is unknown or differs inside the code.

When this variable is non-nil, ‘py-indent-offset’ is guessed from existing code.

Which might slow down the proceeding."

  :type 'boolean
  :tag "py-smart-indentation"
  :group 'python-mode)

(defcustom py-block-comment-prefix "##"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands will not get confused (i.e., the string
should be of the form ‘#x...’ where ‘x’ is not a blank or a tab, and
 ‘...’ is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :tag "py-block-comment-prefix"
  :group 'python-mode)

(defcustom py-indent-offset 4
  "Amount of offset per level of indentation.
‘\\[py-guess-indent-offset]’ can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :tag "py-indent-offset"
  :group 'python-mode)
(make-variable-buffer-local 'py-indent-offset)
(put 'py-indent-offset 'safe-local-variable 'integerp)

(defcustom py-backslashed-lines-indent-offset 5
  "Amount of offset per level of indentation of backslashed.
No semantic indent,  which diff to ‘py-indent-offset’ indicates"
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

(defcustom py-separator-char "/"
  "The character, which separates the system file-path components.

Precedes guessing when not empty, returned by function ‘py-separator-char’."
  :type 'string
  :tag "py-separator-char"
  :group 'python-mode)

(defvar py-separator-char "/"
  "Values set by defcustom only will not be seen in batch-mode.")

(and
 ;; used as a string finally
 ;; kept a character not to break existing customizations
 (characterp py-separator-char)(setq py-separator-char (char-to-string py-separator-char)))

(defcustom py-custom-temp-directory ""
  "If set, will take precedence over guessed values from ‘py-temp-directory’.

Default is the empty string."
  :type 'string
  :tag "py-custom-temp-directory"
  :group 'python-mode)

(defcustom py-beep-if-tab-change t
  "Ring the bell if ‘tab-width’ is changed.
If a comment of the form

                           \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) ‘tab-width’ does not
equal <number>, ‘tab-width’ is set to <number>, a message saying so is
displayed in the echo area, and if ‘py-beep-if-tab-change’ is non-nil
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
  "Function called by ‘py-electric-delete’ when deleting forwards."
  :type 'function
  :tag "py-delete-function"
  :group 'python-mode)

(defcustom py-import-check-point-max
  20000
  "Max number of characters to search Java-ish import statement.

When ‘python-mode’ tries to calculate the shell
-- either a CPython or a Jython shell --
it looks at the so-called ‘shebang’.
If that's not available, it looks at some of the
file heading imports to see if they look Java-like."
  :type 'integer
  :tag "py-import-check-point-max
"
  :group 'python-mode)

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
  "A list of shells for finer grained commands.
like ‘py-execute-statement-ipython’
Expects its executables installed

Edit for your needs."
  :type '(repeat string)
  :tag "py-shells"
  :group 'python-mode)

(defcustom py-jython-packages
  '("java" "javax")
  "Imported packages that imply ‘jython-mode’."
  :type '(repeat string)
  :tag "py-jython-packages
"
  :group 'python-mode)

(defcustom py-current-defun-show t
  "If ‘py-current-defun’ should jump to the definition.

Highlights it while waiting PY-WHICH-FUNC-DELAY seconds.
Afterwards returning to previous position.

Default is t."

  :type 'boolean
  :tag "py-current-defun-show"
  :group 'python-mode)

(defcustom py-current-defun-delay 2
  "‘py-current-defun’ waits PY-WHICH-FUNC-DELAY seconds.

Before returning to previous position."

  :type 'number
  :tag "py-current-defun-delay"
  :group 'python-mode)

(defcustom py-python-send-delay 1
  "Seconds to wait for output, used by ‘py--send-...’ functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python-send-delay"
  :group 'python-mode)

(defcustom py-python3-send-delay 1
  "Seconds to wait for output, used by ‘py--send-...’ functions.

See also ‘py-ipython-send-delay’"

  :type 'number
  :tag "py-python3-send-delay"
  :group 'python-mode)

(defcustom py-ipython-send-delay 1
  "Seconds to wait for output, used by ‘py--send-...’ functions.

See also ‘py-python-send-delay’"

  :type 'number
  :tag "py-ipython-send-delay"
  :group 'python-mode)

(defcustom py-master-file nil
  "Execute the named master file instead of the buffer's file.

Default is nil.
With relative path variable ‘default-directory’ is prepended.

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

(defcustom py-pyflakes3-command "pyflakes3"
  "Shell command used to run Pyflakes3."
  :type 'string
  :tag "py-pyflakes3-command"
  :group 'python-mode)

(defcustom py-pyflakes3-command-args ""
  "String arguments to be passed to pyflakes3.

Default is \"\""
  :type 'string
  :tag "py-pyflakes3-command-args"
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
  "Shell command used to run ‘pyflakespep8’."
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
(setq py-pdbtrack-input-prompt "^[(<]*[Ii]?[Pp]y?db[>)]+ *")

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
  "Regular Expression matching input prompt of python shell.
It should not contain a caret (^) at the beginning.")

(defvar py-shell-ipython-input-prompt-2-regexp "   \\.\\.\\.: "
  "Regular Expression matching second level input prompt of python shell.
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
  "Internally used by ‘py-fast-filter’.
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
  "Internally used by ‘py-fast-filter’.
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
  "Python-history default file.

Used when ‘py-honor-PYTHONHISTORY-p’ is nil (default)."

  :type 'string
  :tag "py-python-history"
  :group 'python-mode)

(defcustom py-switch-buffers-on-execute-p nil
  "When non-nil switch to the Python output buffer.

If ‘py-keep-windows-configuration’ is t, this will take precedence
over setting here."

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

With \\='always, results will displayed in a new window.

Both t and ‘always’ is experimental still.

For the moment: If a multitude of python-shells/buffers should be
visible, open them manually and set ‘py-keep-windows-configuration’ to t.

See also ‘py-keep-windows-configuration’"
  :type `(choice
          (const :tag "default" just-two)
          (const :tag "reuse" t)
          (const :tag "no split" nil)
          (const :tag "always" always))
  :tag "py-split-window-on-execute"
  :group 'python-mode)

(defcustom py-split-window-on-execute-threshold 3
  "Maximal number of displayed windows.

Honored, when ‘py-split-window-on-execute’ is t, i.e. \"reuse\".
Do not split when max number of displayed windows is reached."
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
    "with"     "match"  "case")
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
  "Hide the comments too when you do an ‘hs-hide-all’."
  :type 'boolean
  :tag "py-hide-comments-when-hiding-all"
  :group 'python-mode)

(defcustom py-outline-mode-keywords
  '("class"    "def"    "elif"    "else"    "except"
    "for"      "if"     "while"   "finally" "try"
    "with"     "match"  "case")
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

;; (defcustom py-shell-name
;;   (if (eq system-type 'windows-nt)
;;       "C:/Python27/python"
;;     "python")

;;   "A PATH/TO/EXECUTABLE or default value ‘py-shell’ may look for.

;; If no shell is specified by command.

;; On Windows default is C:/Python27/python
;; --there is no garantee it exists, please check your system--

;; Else python"
;;   :type 'string
;;   :tag "py-shell-name
;; "
;;   :group 'python-mode)

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

(defvar py-shell-name py-python-command)
;; (defvaralias 'py-shell-name 'py-python-command)

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

  "A PATH/TO/EXECUTABLE or default value ‘py-shell’ may look for.

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
    (if (string-match "^[0-4]" (ignore-errors (shell-command-to-string (concat "ipython" " -V"))))
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
  "A PATH/TO/EXECUTABLE or default value used by ‘py-toggle-shell’."
  :type 'string
  :tag "py-shell-toggle-1"
  :group 'python-mode)

(defcustom py-shell-toggle-2 py-python3-command
  "A PATH/TO/EXECUTABLE or default value used by ‘py-toggle-shell’."
  :type 'string
  :tag "py-shell-toggle-2"
  :group 'python-mode)

(defcustom py--imenu-create-index-p nil
  "Non-nil means Python mode creates and displays an index menu.

Of functions and global variables."
  :type 'boolean
  :tag "py--imenu-create-index-p"
  :group 'python-mode)

(defvar py-history-filter-regexp "\\‘\\s-*\\S-?\\S-?\\s-*\\'\\|''’/tmp/"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defcustom py-match-paren-mode nil
  "Non-nil means, cursor will jump to beginning or end of a block.
This vice versa, to beginning first.
Sets ‘py-match-paren-key’ in ‘python-mode-map’.
Customize ‘py-match-paren-key’ which key to use."
  :type 'boolean
  :tag "py-match-paren-mode"
  :group 'python-mode)

(defcustom py-match-paren-key "%"
  "String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands will not get confused (i.e., the string
should be of the form ‘#x...’ where ‘x’ is not a blank or a tab, and
                               ‘...’ is arbitrary).
However, this string should not end in whitespace."
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
  "If t, ‘py-shell’ will use ‘py-shell-local-path’.

Alternative to default Python.

Making switch between several virtualenv's easier,‘python-mode’ should
deliver an installer, named-shells pointing to virtualenv's will be available."
  :type 'boolean
  :tag "py-use-local-default"
  :group 'python-mode)

(defcustom py-edit-only-p nil
  "Do not check for installed Python executables.

Default is nil.

See bug report at launchpad, lp:944093."
  :type 'boolean
  :tag "py-edit-only-p"
  :group 'python-mode)

(defcustom py-force-py-shell-name-p nil
  "When t, execution specified in ‘py-shell-name’ is enforced.

Possibly shebang does not take precedence."

  :type 'boolean
  :tag "py-force-py-shell-name-p"
  :group 'python-mode)

(defcustom python-mode-v5-behavior-p nil
  "Execute region through ‘shell-command-on-region’.

As v5 did it - lp:990079.
This might fail with certain chars - see UnicodeEncodeError lp:550661"

  :type 'boolean
  :tag "python-mode-v5-behavior-p"
  :group 'python-mode)

(defcustom py-trailing-whitespace-smart-delete-p nil
  "Default is nil.

When t, ‘python-mode’ calls
\(add-hook \\='before-save-hook \\='delete-trailing-whitespace nil \\='local)

Also commands may delete trailing whitespace by the way.
When editing other peoples code, this may produce a larger diff than expected"
  :type 'boolean
  :tag "py-trailing-whitespace-smart-delete-p"
  :group 'python-mode)

(defcustom py-newline-delete-trailing-whitespace-p t
  "Delete trailing whitespace maybe left by ‘py-newline-and-indent’.

Default is t. See lp:1100892"
  :type 'boolean
  :tag "py-newline-delete-trailing-whitespace-p"
  :group 'python-mode)

(defcustom py--warn-tmp-files-left-p nil
  "Warn, when ‘py-temp-directory’ contains files susceptible being left.

WRT previous Python-mode sessions. See also lp:987534."
  :type 'boolean
  :tag "py--warn-tmp-files-left-p"
  :group 'python-mode)

(defcustom py-complete-ac-sources '(ac-source-pycomplete)
  "List of ‘auto-complete’ sources assigned to ‘ac-sources’.

In ‘py-complete-initialize’.

Default is known to work an Ubuntu 14.10 - having python-
mode, pymacs and auto-complete-el, with the following minimal
Emacs initialization:

\(require \\='pymacs)
\(require \\='auto-complete-config)
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
  "‘py-shell’ will use EXECUTABLE indicated here incl. path.

If ‘py-use-local-default’ is non-nil."

  :type 'string
  :tag "py-shell-local-path"
  :group 'python-mode)

(defcustom py-python-edit-version "python3"
  "Default is \"python3\".

When empty, version is guessed via ‘py-choose-shell’."

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
  "See also ‘py-ipython-module-completion-code’.")

(defcustom py--imenu-create-index-function 'py--imenu-index
  "Switch between ‘py--imenu-create-index-new’  and series 5. index-machine."
  :type '(choice
          (const :tag "'py--imenu-create-index-new, also lists modules variables " py--imenu-create-index-new)

          (const :tag "py--imenu-create-index, series 5. index-machine" py--imenu-create-index)
          (const :tag "py--imenu-index, honor type annotations" py--imenu-index)

          )
  :tag "py--imenu-create-index-function"
  :group 'python-mode)

(defvar py-line-re "^"
  "Used by generated functions." )

(defvar py-input-filter-re "\\‘\\s-*\\S-?\\S-?\\s-*\\’"
  "Input matching this regexp is not saved on the history list.
Default ignores all inputs of 0, 1, or 2 non-blank characters.")

(defvar strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING.

See also ‘string-chars-preserve’")

(defvar strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING.

See also ‘string-chars-preserve’")

(defcustom py-docstring-style 'pep-257-nn
  "Implemented styles:

 are DJANGO, ONETWO, PEP-257, PEP-257-NN,SYMMETRIC, and NIL.

A value of NIL wo not care about quotes
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

          (const :tag "Do not format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257-nn with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :tag "py-docstring-style"
  :group 'python-mode)

(defcustom py-execute-directory nil
  "Stores the file's default directory-name py-execute-... functions act upon.

Used by Python-shell for output of ‘py-execute-buffer’ and related commands.
See also ‘py-use-current-dir-when-execute-p’"
  :type 'string
  :tag "py-execute-directory"
  :group 'python-mode)

(defcustom py-use-current-dir-when-execute-p t
  "Current directory used for output.

See also ‘py-execute-directory’"
  :type 'boolean
  :tag "py-use-current-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-keep-shell-dir-when-execute-p nil
  "Do not change Python shell's current working directory when sending code.

See also ‘py-execute-directory’"
  :type 'boolean
  :tag "py-keep-shell-dir-when-execute-p"
  :group 'python-mode)

(defcustom py-fileless-buffer-use-default-directory-p t
  "‘default-directory’ sets current working directory of Python output shell.

When ‘py-use-current-dir-when-execute-p’ is non-nil and no buffer-file exists."
  :type 'boolean
  :tag "py-fileless-buffer-use-default-directory-p"
  :group 'python-mode)

(defcustom py-check-command "pychecker --stdlib"
  "Command used to check a Python file."
  :type 'string
  :tag "py-check-command"
  :group 'python-mode)

;; (defvar py-this-abbrevs-changed nil
;;   "Internally used by ‘python-mode-hook’.")

(defvar py-buffer-name nil
  "Internal use.

The buffer last output was sent to.")

(defvar py-orig-buffer-or-file nil
  "Internal use.")

(defcustom py-keep-windows-configuration nil
  "Takes precedence over:

 ‘py-split-window-on-execute’ and ‘py-switch-buffers-on-execute-p’.
See lp:1239498

To suppres window-changes due to error-signaling also.
Set ‘py-keep-windows-configuration’ onto \\'force

Default is nil"

  :type '(choice
          (const :tag "nil" nil)
          (const :tag "t" t)
          (const :tag "force" force))
  :tag "py-keep-windows-configuration"
  :group 'python-mode)

(defvar py-output-buffer ""
      "Used if ‘python-mode-v5-behavior-p’ is t.

Otherwise output buffer is created dynamically according to version process.")

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
‘py-shell-exec-path’ will take precedence to paths defined
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

(defcustom py-start-in-virtualenv-p nil
  "When ‘py-shell-virtualenv-root’ is set, Emacs should start there."
  :type 'boolean
  :tag "py-start-in-virtualenv-p"
  :group 'python-mode)

(defvar py-shell-completion-native-redirect-buffer
  " *Py completions redirect*"
  "Buffer to be used to redirect output of readline commands.")

(defvar py-shell--block-prompt nil
  "Input block prompt for inferior python shell.
Do not set this variable directly, instead use
‘py-shell-prompt-set-calculated-regexps’.")

(defvar py-shell-output-filter-in-progress nil)
(defvar py-shell-output-filter-buffer nil)

(defvar py-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior python shell.
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ is not run.")

(defvar py-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior python shell.

‘py-shell-prompt-set-calculated-regexps’
Do not set this variable directly.

Iff ‘py-shell--prompt-calculated-input-regexp’
or ‘py-shell--prompt-calculated-output-regexp’ are set
‘py-shell-prompt-set-calculated-regexps’ is not run.")

(defvar py-shell-prompt-output-regexp ""
  "See ‘py-shell-prompt-output-regexps’.")

(defvar py-shell-prompt-output-regexps
  '(""                                  ; Python
    "Out\\[[0-9]+\\]: "                 ; IPython
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts.")

(defvar py-underscore-word-syntax-p t
  "This is set later by defcustom, only initial value here.

If underscore chars should be of ‘syntax-class’ ‘word’, not of ‘symbol’.
Underscores in word-class makes ‘forward-word’.
Travels the indentifiers. Default is t.
See also command ‘py-toggle-underscore-word-syntax-p’")

(defvar py-autofill-timer nil)
(defvar py-fill-column-orig fill-column
  "Used to reset fill-column")

;; defvared value is not updated maybe
(defvar python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
          (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode")
  "Internally used. Reports the ‘python-mode’ branch.")

;; defvared value is not updated maybe
(setq python-mode-message-string
  (if (or (string= "python-mode.el" (buffer-name))
          (ignore-errors (string-match "python-mode.el" (py--buffer-filename-remote-maybe))))
      "python-mode.el"
    "python-components-mode"))

(defvar python-mode-syntax-table nil
  "Give punctuation syntax to ASCII that normally has symbol.

Syntax or has word syntax and is not a letter.")

(setq python-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; Give punctuation syntax to ASCII that normally has symbol
        ;; syntax or has word syntax and is not a letter.
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

(defvar py-shell-mode-syntax-table nil
  "Set from py-shell")

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

(defvar py-temp-directory
  (let ((ok #'(lambda (x)
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
      "Could not find a usable temp directory -- set ‘py-temp-directory’"))
    (when erg (setq py-temp-directory erg)))
  "Directory used for temporary files created by a *Python* process.
By default, guesses the first directory from this list that exists and that you
can write into: the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, /var/tmp, or the current directory.

 ‘py-custom-temp-directory’ will take precedence when setq")

(defvar py-exec-command nil
  "Internally used.")

(defvar py-which-bufname "Python")

(defvar py-pychecker-history nil)

(defvar py-pyflakes3-history nil)

(defvar py-pep8-history nil)

(defvar py-pyflakespep8-history nil)

(defvar py-pylint-history nil)

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")

(defvar hs-hide-comments-when-hiding-all t
  "Defined in hideshow.el, silence compiler warnings here.")

(defvar py-shell-complete-debug nil
  "For interal use when debugging, stores completions." )

(defvar py-debug-p nil
  "Activate extra code for analysis and test purpose when non-nil.

Temporary files are not deleted. Other functions might implement
some logging, etc.
For normal operation, leave it set to nil, its default.
Defined with a defvar form to allow testing the loading of new versions.")

(defcustom py-shell-complete-p nil
  "Enable native completion."

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
  "Internal use.

Restore ‘py-restore-window-configuration’.")

(defvar py-exception-buffer nil
  "Will be set internally.

Remember source buffer where error might occur.")

(defvar py-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar py-star-labelled-re "[ \\t]*[\\*-] +[[:graph:]]"
  "When looking at a star label.")

(defvar py-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+"
  "When looking at a colon label.")
;; (setq py-colon-labelled-re "[ \\t]*[[:graph:]]* *: *[[:graph:]]+\\|[ \\t]*[\\*-] +[[:graph:]]")

(defvar py-labelled-re (concat py-colon-labelled-re "\\|" py-star-labelled-re)
  "When looking at label.")

;; "[ \t]+\\c.+"
(defvar py-symbol-re "[ \t]*\\c.+[ \t]*$"
  "Matching lines only containing symbols.")
(setq py-symbol-re "[ \t]*\\c.+[ \t]*")

(defvar py-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "Expression possibly composing a ‘py-expression’.")

(defvar py-expression-skip-chars "^ (=#\t\r\n\f"
  "Chars composing a ‘py-expression’.")

(setq py-expression-skip-chars "^ [{(=#\t\r\n\f")

(defvar py-expression-re "[^ =#\t\r\n\f]+"
  "Expression possibly composing a ‘py-expression’.")

(defcustom py-paragraph-re paragraph-start
  "Allow Python specific ‘paragraph-start’ var."
  :type 'string
  :tag "py-paragraph-re"
  :group 'python-mode)

(defvar py-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "Regexp indicated probably will not compose a ‘py-expression’.")

(defvar py-not-expression-chars " #\t\r\n\f"
  "Chars indicated probably will not compose a ‘py-expression’.")

;; (defvar py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
(defvar py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f"
    "Chars indicated which not possibly compose a ‘py-partial-expression’,
stop at it.")
;; (setq py-partial-expression-stop-backward-chars "^] .=,\"'()[{}:#\t\r\n\f")

(defvar py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")
;; (setq py-partial-expression-forward-chars "^ .\"')}]:#\t\r\n\f")

(defvar py-partial-expression-re (concat "[" py-partial-expression-stop-backward-chars (substring py-partial-expression-forward-chars 1) "]+"))
(setq py-partial-expression-re (concat "[" py-partial-expression-stop-backward-chars "]+"))

;; (defvar py-statement-re py-partial-expression-re)
(defvar py-statement-re "[^] .=,\"'()[{}:#
]+" "Match beginning of a statement")

(defvar py-indent-re ".+"
  "This var is introduced for regularity only.")
(setq py-indent-re ".+")

(defvar py-operator-re "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\|=\\)[ \t]*"
  "Matches most of Python syntactical meaningful characters.

See also ‘py-assignment-re’")

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

(defface py-variable-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face method decorators."
  :tag "py-variable-name-face"
  :group 'python-mode)

(defvar py-variable-name-face 'py-variable-name-face)
(setq py-variable-name-face 'py-variable-name-face)

(defvar py-number-face 'py-number-face)

(defvar py-decorators-face 'py-decorators-face)

(defvar py-object-reference-face 'py-object-reference-face)

(defvar py-builtins-face 'py-builtins-face)

(defvar py-class-name-face 'py-class-name-face)

(defvar py-def-face 'py-def-face)

(defvar py-exception-name-face 'py-exception-name-face)

(defvar py-import-from-face 'py-import-from-face)

(defvar py-def-class-face 'py-def-class-face)

(defvar py-try-if-face 'py-try-if-face)

(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")

(defvar jython-mode-hook nil
  "Hook called by ‘jython-mode’.
‘jython-mode’ also calls ‘python-mode-hook’.")

(defvar py-shell-hook nil
  "Hook called by ‘py-shell’.")

;; (defvar python-font-lock-keywords nil)

(defvar py-dotted-expression-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?."_" table)
    table)
  "Syntax table used to identify Python dotted expressions.")

(defvar python-default-template "if"
  "Default template to expand by ‘python-expand-template’.
Updated on each expansion.")

(defvar-local py-already-guessed-indent-offset nil
  "Internal use by ‘py-indent-line’.

When ‘this-command’ is ‘eq’ to ‘last-command’, use the guess already computed.")

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
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]*"
  "Matches the beginning of a class, method or compound statement.")

(setq py-block-closing-keywords-re
  "[ \t]*\\_<\\(return\\|raise\\|break\\|continue\\|pass\\)\\_>[ \n\t]*")

(defconst py-finally-re
  "[ \t]*\\_<finally:"
  "Regular expression matching keyword which closes a try-block.")

(defconst py-except-re "[ \t]*\\_<except\\_>"
  "Matches the beginning of a ‘except’ block.")

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
   "case"
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

(defcustom py-block-re-raw
  (list
   "async def"
   "async for"
   "async with"
   "class"
   "def"
   "for"
   "if"
   "match"
   "try"
   "while"
   "with"
   )
  "Matches the beginning of a compound statement but not its clause."
  :type '(repeat string)
  :tag "py-block-re-raw"
  :group 'python-mode)

(defconst py-block-re (concat
                       ;; def main():
                       ;; |   if len(sys.argv) == 1:
                       ;;         usage()
                       ;;         # sys.exit()

                       ;;     class asdf(object):

                       "[ \t]*"
                       (regexp-opt py-block-re-raw 'symbols)
                       ".*[:( \n\t]"
                       )
  "Matches the beginning of a compound statement.")

(defconst py-minor-block-re-raw (list
                                      "async for"
                                      "async with"
                                      "case"
                                      "except"
                                      "for"
                                      "if"
                                      "match"
                                      "try"
                                      "with"
                                      )
  "Matches the beginning of an case ‘for’, ‘if’, ‘try’, ‘except’ or ‘with’ block.")

(defconst py-minor-block-re
  (concat
   "[ \t]*"
   (regexp-opt py-minor-block-re-raw 'symbols)
   "[:( \n\t]")

  "Regular expression matching lines not to augment indent after.

See ‘py-minor-block-re-raw’ for better readable content")

(defconst py-try-re "[ \t]*\\_<try\\_>[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst py-case-re "[ \t]*\\_<case\\_>[: \t][^:]*:"
  "Matches a ‘case’ clause.")

(defconst py-match-case-re "[ \t]*\\_<match\\|case\\_>[: \t][^:]*:"
  "Matches a ‘match case’ clause.")

(defconst py-for-re "[ \t]*\\_<\\(async for\\|for\\)\\_> +[[:alpha:]_][[:alnum:]_]* +in +[[:alpha:]_][[:alnum:]_()]* *[: \n\t]"
  "Matches the beginning of a ‘try’ block.")

(defconst py-if-re "[ \t]*\\_<if\\_>[ (]+"
  "Matches the beginning of an ‘if’ block.")

(defconst py-else-re "[ \t]*\\_<else:"
  "Matches the beginning of an ‘else’ block.")

(setq py-else-re "else")

(defconst py-elif-re "[ \t]*\\_<\\elif\\_>[( \n\t]"
  "Matches the beginning of a compound if-statement's clause exclusively.")

;; (defconst py-elif-block-re "[ \t]*\\_<elif\\_> +[[:alpha:]_][[:alnum:]_]* *[: \n\t]"
;;   "Matches the beginning of an ‘elif’ block.")

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
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or its clause."
  :type '(repeat string)
  :tag "py-block-or-clause-re-raw"
  :group 'python-mode)

(defvar py-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-block-or-clause-re-raw 'symbols)
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
   "with"
   "match"
   "case"
   )
  "Matches the beginning of a compound statement or its clause."
  :type '(repeat string)
  :tag "py-extended-block-or-clause-re-raw"
  :group 'python-mode)

(defconst py-extended-block-or-clause-re
  (concat
   "[ \t]*"
   (regexp-opt  py-extended-block-or-clause-re-raw 'symbols)
   "[( \t:]+")
  "See ‘py-block-or-clause-re-raw’, which it reads.")

(defconst py-clause-re py-extended-block-or-clause-re
  "See also py-minor-clause re.")

(defcustom py-minor-clause-re-raw
  (list
   "case"
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
   "[( \t]*.*:")
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

(defvar py-comment-re "#[ \t]*"
  "Needed for normalized processing.")

(defconst py-block-keywords
   (regexp-opt py-block-or-clause-re-raw 'symbols)
  "Matches known keywords opening a block.

Customizing ‘py-block-or-clause-re-raw’  will change values here")

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
          (group (1+ (not (any "\"<")))) ; avoid ‘<stdin>’ &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "Fetch errors from py-shell.
hooked into ‘compilation-error-regexp-alist’"
  :type '(alist string)
  :tag "py-compilation-regexp-alist"
  :group 'python-mode)

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

(defconst py--windows-config-register 313465889
  "Internal used by ‘window-configuration-to-register’.")

;; (setq py--windows-config-register 313;; 465889)

;; testing
(defvar py-ert-test-default-executables
  (list "python" "python3" "ipython")
  "Serialize tests employing dolist.")

(defcustom py-shell-unfontify-p t
  "Run ‘py--run-unfontify-timer’ unfontifying the shell banner-text.

Default is nil"

  :type 'boolean
  :tag "py-shell-unfontify-p"
  :group 'python-mode)

;; Pdb
;; #62, pdb-track in a shell buffer
(defcustom pdb-track-stack-from-shell-p t
  "If t, track source from shell-buffer.

Default is t.
Add hook \\='comint-output-filter-functions \\='py--pdbtrack-track-stack-file"

  :type 'boolean
  :tag "pdb-track-stack-from-shell-p"
  :group 'python-mode)

(defvar gud-pdb-history ""
  "Silence compiler warning.")

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
‘py-pdbtrack-set-tracked-buffer’ instead.")

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

(defvar py--docbeg nil
  "Internally used by ‘py--write-edit’.")

(defvar py--docend nil
  "Internally used by ‘py--write-edit’.")

(defvar py-completion-setup-code  "def __PYTHON_EL_get_completions(text):
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
  "Code used to setup completion in inferior Python processes.")

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

(defface py-XXX-tag-face
  '((t (:inherit font-lock-string-face)))
  "XXX\\|TODO\\|FIXME "
  :tag "py-XXX-tag-face"
  :group 'python-mode)

(defface py-pseudo-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for pseudo keywords in Python mode, like self, True, False,
  Ellipsis.

See also ‘py-object-reference-face’"
  :tag "py-pseudo-keyword-face"
  :group 'python-mode)

(defface py-object-reference-face
  '((t (:inherit py-pseudo-keyword-face)))
  "Face when referencing object members from its class resp. method.,
commonly \"cls\" and \"self\""
  :tag "py-object-reference-face"
  :group 'python-mode)

(defface py-number-face
 '((t (:inherit nil)))
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

(defface py-def-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for definitions."
  :tag "py-def-face"
  :group 'python-mode)

(defface py-exception-name-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Python exceptions."
  :tag "py-exception-name-face"
  :group 'python-mode)

;; subr-x.el might not exist yet
;; #73, Byte compilation on Emacs 25.3 fails on different trim-right signature

(defsubst py--string-trim-left (strg &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") strg)
      (replace-match "" t t strg)
    strg))

(defsubst py--string-trim-right (strg &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") strg)
      (replace-match "" t t strg)
    strg))

(defsubst py--string-trim (strg &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (py--string-trim-left (py--string-trim-right strg trim-right) trim-left))

(defcustom py-empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :tag "py-empty-line-p-chars"
  :group 'python-mode)

(defcustom py-default-working-directory ""
  "If not empty used by ‘py-set-current-working-directory’."
  :type 'string
  :tag "py-default-working-directory"
  :group 'python-mode)

(defcustom py-python-ffap-setup-code
  "
def __FFAP_get_module_path(objstr):
    try:
        import inspect
        import os.path
        # NameError exceptions are delayed until this point.
        obj = eval(objstr)
        module = inspect.getmodule(obj)
        filename = module.__file__
        ext = os.path.splitext(filename)[1]
        if ext in ('.pyc', '.pyo'):
            # Point to the source file.
            filename = filename[:-1]
        if os.path.exists(filename):
            return filename
        return ''
    except:
        return ''"
  "Python code to get a module path."
  :type 'string
  :tag "py-python-ffap-setup-code"
  :group 'python-mode)

;; (defvar py-ffap-string-code
;;   "__FFAP_get_module_path('''%s''')\n"
;;   "Python code used to get a string with the path of a module.")

(defcustom py-ffap-string-code
  "__FFAP_get_module_path('''%s''')"
  "Python code used to get a string with the path of a module."
  :type 'string
  :tag "py-python-ffap-string-code"
  :group 'python-mode)

(defvar python-mode-map nil)

(defvar py-debug-p nil
  "Used for development purposes.")

;; This and other stuff from python.el

(defvar py-last-exeption-buffer nil
  "Internal use only - when ‘py-up-exception’ is called.

In source-buffer, this will deliver the exception-buffer again.")

(defcustom py-electric-backspace-p nil
  "When ‘t’, <backspace> key will delete all whitespace chars before point.

Default nil"

  :type 'boolean
  :tag "py-electric-backspace-p"
  :group 'python-mode
  :safe 'booleanp
  :set (lambda (symbol value)
         (set-default symbol value)
         (py-electric-backspace-mode (if value 1 0))))

(defcustom py-mark-decorators nil
  "If decorators should be marked too.

Default is nil.

Also used by navigation"
  :type 'boolean
  :tag "py-mark-decorators")

(provide 'python-components-vars)
;;; python-components-vars.el ends here
