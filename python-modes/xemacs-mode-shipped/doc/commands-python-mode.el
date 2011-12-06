;; a list of python-mode.el commands
(setq python-mode.el-commands (quote (("py-choose-shell" . "Choose CPython or Jython mode. Returns the appropriate mode function.
This does the following:
 - look for an interpreter with `py-choose-shell-by-shebang'
 - examine imports using `py-choose-shell-by-import'
 - default to the variable `py-default-interpreter'") ("python-mode" . "Major mode for editing Python files.
To submit a problem report, enter `M-x py-submit-bug-report' from a
`python-mode' buffer.  Do `M-x py-describe-mode' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `M-x py-version'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
key             binding
---             -------

C-c             Prefix Command
C-j             py-newline-and-indent
RET             py-newline-and-indent
C-x             Prefix Command
ESC             Prefix Command
#               py-electric-comment
:               py-electric-colon
<C-backspace>   py-hungry-delete-backwards
<backspace>     py-electric-backspace
<delete>        py-electric-delete
<s-backspace>   py-dedent

C-x n           Prefix Command

C-M-a           py-beginning-of-def-or-class
C-M-e           py-end-of-def-or-class
C-M-h           py-mark-def-or-class
M-TAB           completion-at-point
C-M-x           py-execute-def-or-class

C-c C-a         py-mark-statement
C-c C-b         py-submit-bug-report
C-c C-c         py-execute-buffer
C-c C-d         py-pdbtrack-toggle-stack-tracking
C-c C-e         py-describe-symbol
C-c C-f         py-sort-imports
C-c C-k         py-mark-block-or-clause
C-c C-l         py-shift-left
C-c RET         py-execute-import-or-reload
C-c C-n         py-end-of-statement
C-c C-p         py-beginning-of-statement
C-c C-q         py-end-of-block
C-c C-r         py-shift-right
C-c C-s         py-execute-string
C-c C-t         py-toggle-shells
C-c C-u         py-beginning-of-block
C-c C-v         py-version
C-c C-w         py-pychecker-run
C-c !           py-shell
C-c #           py-comment-region
C-c -           py-up-exception
C-c :           py-guess-indent-offset
C-c <           py-shift-left
C-c =           py-down-exception
C-c >           py-shift-right
C-c ?           py-describe-mode
C-c c           py-compute-indentation
C-c |           py-execute-region
C-c <delete>    py-hungry-delete-forward
C-c <tab>       py-indent-region

C-x n d         py-narrow-to-defun


VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by `comment-region'
py-python-command		shell command to invoke Python interpreter
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if `tab-width' is changed") ("jython-mode" . "Major mode for editing Jython/Jython files.
This is a simple wrapper around `python-mode'.
It runs `jython-mode-hook' then calls `python-mode.'
It is added to `interpreter-mode-alist' and `py-choose-shell'.
") ("py-electric-colon" . "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment.") ("py-toggle-shells" . "Toggles between the CPython and Jython shells.

With positive argument ARG (interactively C-u),
uses the CPython shell, with negative ARG uses the Jython shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `cpython' or
`jython', equivalent to positive arg and negative arg respectively.") ("py-shell" . "Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional C-u, the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
Jython interpreter by hitting M-x py-toggle-shells.  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*Jython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter.") ("py-clear-queue" . "Clear the queue of temporary files waiting to execute.") ("py-execute-region" . "Execute the region in a Python interpreter.

The region is first copied into a temporary file (in the directory
`py-temp-directory').  If there is no Python interpreter shell
running, this file is executed synchronously using
`shell-command-on-region'.  If the program is long running, use
C-u to run the command asynchronously in its own
buffer.

When this function is used programmatically, arguments START and END
specify the region to execute, and optional third argument ASYNC, if
non-nil, specifies to run the command asynchronously in its own
buffer.

If the Python interpreter shell is running, the region is execfile()'d
in that shell.  If you try to execute regions too quickly,
`python-mode' will queue them up and execute them one at a time when
it sees a `>>> ' prompt from Python.  Each time this happens, the
process buffer is popped into a window (if it's not already in some
window) so you can see it, and a comment of the form

    	## working on region in file <name>...

is inserted at the end.  See also the command `py-clear-queue'.") ("py-execute-buffer" . "Send the contents of the buffer to a Python interpreter.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Python* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.") ("py-execute-import-or-reload" . "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `M-x py-execute-buffer' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions.") ("py-execute-def-or-class" . "Send the current function or class definition to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.") ("py-execute-string" . "Send the argument STRING to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `M-x py-execute-region' docs for an account of some
subtleties, including the use of the optional ASYNC argument.") ("py-mouseto-exception" . "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click.") ("py-goto-exception" . "Go to the line indicated by the traceback.") ("py-down-exception" . "Go to the next line down in the traceback.
With M-x univeral-argument (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack.") ("py-up-exception" . "Go to the previous line up in the traceback.
With C-u (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack.") ("py-electric-backspace" . "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `py-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
C-u dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
C-u specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above.") ("py-electric-delete" . "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to M-x c-electric-backspace.

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `py-delete-function'.

C-u (programmatically, argument ARG) specifies the
number of characters to delete (default is 1).") ("py-indent-line" . "Fix the indentation of the current line according to Python rules.
With C-u (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
TAB will call it.") ("py-newline-and-indent" . "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Python code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented.") ("py-guess-indent-offset" . "Guess a good value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value, so that other Python buffers are not affected.  With
C-u (programmatically, optional argument GLOBAL),
change the global value of `py-indent-offset'.  This affects all
Python buffers (that don't have their own buffer-local copy), both
those currently existing and those created later in the Emacs session.

Some people use a different value for `py-indent-offset' than you use.
There's no excuse for such foolishness, but sometimes you have to deal
with their ugly code anyway.  This function examines the file and sets
`py-indent-offset' to what it thinks it was when they created the
mess.

Specifically, it searches forward from the statement containing point,
looking for a line that opens a block of code.  `py-indent-offset' is
set to the difference in indentation between that line and the Python
statement following it.  If the search doesn't succeed going forward,
it's tried again going backward.") ("py-narrow-to-defun" . "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'.") ("py-shift-region-left" . "Shift region of Python code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero.") ("py-shift-region-right" . "Shift region of Python code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line.") ("py-indent-region" . "Reindent a region of Python code.

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
initial line; and comment lines beginning in column 1 are ignored.") ("py-comment-region" . "Like `comment-region' but uses double hash (`#') comment starter.") ("py-sort-imports" . "Sort multiline imports.
Put point inside the parentheses of a multiline import and hit
M-x py-sort-imports to sort the imports lexicographically") ("py-previous-statement" . "Go to the start of the COUNTth preceding Python statement.
By default, goes to the previous statement.  If there is no such
statement, goes to the first statement.  Return count of statements
left to move.  `Statements' do not include blank, comment, or
continuation lines.") ("py-next-statement" . "Go to the start of next Python statement.
If the statement at point is the i'th Python statement, goes to the
start of statement i+COUNT.  If there is no such statement, goes to the
last statement.  Returns count of statements left to move.  `Statements'
do not include blank, comment, or continuation lines.") ("py-goto-block-up" . "Move up to start of current block.
Go to the statement that starts the smallest enclosing block; roughly
speaking, this will be the closest preceding statement that ends with a
colon and is indented less than the statement you started on.  If
successful, also sets the mark to the starting point.

`M-x py-mark-block' can be used afterward to mark the whole code
block, if desired.

If called from a program, the mark will not be set if optional argument
NOMARK is not nil.") ("py-beginning-of-def-or-class" . "Move point to start of `def' or `class'.

Searches back for the closest preceding `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth start of `def'.

If point is in a `def' statement already, and after the `d', simply
moves point to the start of the statement.

Otherwise (i.e. when point is not in a `def' statement, or at or
before the `d' of a `def' statement), searches for the closest
preceding `def' statement, and leaves point at its start.  If no such
statement can be found, leaves point at the start of the buffer.

Returns t iff a `def' statement is found by these rules.

Note that doing this command repeatedly will take you closer to the
start of the buffer each time.

To mark the current `def', see `M-x py-mark-def-or-class'.") ("py-end-of-def-or-class" . "Move point beyond end of `def' or `class' body.

By default, looks for an appropriate `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth end of `def'.

If point is in a `def' statement already, this is the `def' we use.

Else, if the `def' found by `M-x py-beginning-of-def-or-class'
contains the statement you started on, that's the `def' we use.

Otherwise, we search forward for the closest following `def', and use that.

If a `def' can be found by these rules, point is moved to the start of
the line immediately following the `def' block, and the position of the
start of the `def' is returned.

Else point is moved to the end of the buffer, and nil is returned.

Note that doing this command repeatedly will take you closer to the
end of the buffer each time.

To mark the current `def', see `M-x py-mark-def-or-class'.") ("py-mark-block" . "Mark following block of lines.  With prefix arg, mark structure.
Easier to use than explain.  It sets the region to an `interesting'
block of succeeding lines.  If point is on a blank line, it goes down to
the next non-blank line.  That will be the start of the region.  The end
of the region depends on the kind of line at the start:

 - If a comment, the region will include all succeeding comment lines up
   to (but not including) the next non-comment line (if any).

 - Else if a prefix arg is given, and the line begins one of these
   structures:

     if elif else try except finally for while def class

   the region will be set to the body of the structure, including
   following blocks that `belong' to it, but excluding trailing blank
   and comment lines.  E.g., if on a `try' statement, the `try' block
   and all (if any) of the following `except' and `finally' blocks
   that belong to the `try' structure will be in the region.  Ditto
   for if/elif/else, for/else and while/else structures, and (a bit
   degenerate, since they're always one-block structures) def and
   class blocks.

 - Else if no prefix argument is given, and the line begins a Python
   block (see list above), and the block is not a `one-liner' (i.e.,
   the statement ends with a colon, not with code), the region will
   include all succeeding lines up to (but not including) the next
   code statement (if any) that's indented no more than the starting
   line, except that trailing blank and comment lines are excluded.
   E.g., if the starting line begins a multi-statement `def'
   structure, the region will be set to the full function definition,
   but without any trailing `noise' lines.

 - Else the region will include all succeeding lines up to (but not
   including) the next blank line, or code or indenting-comment line
   indented strictly less than the starting line.  Trailing indenting
   comment lines are included in this case, but not trailing blank
   lines.

A msg identifying the location of the mark is displayed in the echo
area; or do `C-x C-x' to flip down to the end.

If called from a program, optional argument EXTEND plays the role of
the prefix arg, and if optional argument JUST-MOVE is not nil, just
moves to the end of the block (& does not set mark or display a msg).") ("py-mark-def-or-class" . "Set region to body of def (or class, with prefix arg) enclosing point.
Pushes the current mark, then point, on the mark ring (all language
modes do this, but although it's handy it's never documented ...).

In most Emacs language modes, this function bears at least a
hallucinogenic resemblance to `M-x py-end-of-def-or-class' and
`M-x py-beginning-of-def-or-class'.

And in earlier versions of Python mode, all 3 were tightly connected.
Turned out that was more confusing than useful: the `goto start' and
`goto end' commands are usually used to search through a file, and
people expect them to act a lot like `search backward' and `search
forward' string-search commands.  But because Python `def' and `class'
can nest to arbitrary levels, finding the smallest def containing
point cannot be done via a simple backward search: the def containing
point may not be the closest preceding def, or even the closest
preceding def that's indented less.  The fancy algorithm required is
appropriate for the usual uses of this `mark' command, but not for the
`goto' variations.

So the def marked by this command may not be the one either of the
`goto' commands find: If point is on a blank or non-indenting comment
line, moves back to start of the closest preceding code statement or
indenting comment line.  If this is a `def' statement, that's the def
we use.  Else searches for the smallest enclosing `def' block and uses
that.  Else signals an error.

When an enclosing def is found: The mark is left immediately beyond
the last line of the def block.  Point is left at the start of the
def, except that: if the def is preceded by a number of comment lines
followed by (at most) one optional blank line, point is left at the
start of the comments; else if the def is preceded by a blank line,
point is left at its start.

The intent is to mark the containing def/class and its associated
documentation, to make moving and duplicating functions and classes
pleasant.") ("py-forward-into-nomenclature" . "Move forward to end of a nomenclature section or word.
With C-u (programmatically, optional argument ARG),
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.") ("py-backward-into-nomenclature" . "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores.") ("py-pdbtrack-toggle-stack-tracking") ("turn-on-pdbtrack") ("turn-off-pdbtrack") ("py-pychecker-run" . "*Run pychecker (default on the file currently visited).") ("py-help-at-point" . "Get help from Python based on the symbol nearest point.") ("py-describe-mode" . "Dump long form of Python-mode docs.") ("py-version" . "Echo the current version of `python-mode' in the minibuffer.") ("py-submit-bug-report" . "Submit via mail a bug report on `python-mode'.
With C-u (programmatically, argument ENHANCEMENT-P
non-nil) just submit an enhancement request.") ("py-fill-paragraph" . "Like M-q, but handle Python comments and strings.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial `#'s.
If point is inside a string, narrow to that string and fill.
"))))
