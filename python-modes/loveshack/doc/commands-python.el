;; a list of python.el<3> commands
(setq python.el<3>-commands (quote (("python-guess-indent" . "Guess step for indentation of current buffer.
Set `python-indent' locally to the value guessed.") ("python-indent-line" . "Indent current line as Python code.
When invoked via `indent-for-tab-command', cycle through possible
indentations for current line.  The cycle is broken by a command
different from `indent-for-tab-command', i.e. successive TABs do
the cycling.") ("python-previous-statement" . "Go to start of previous statement.
With argument COUNT, do it COUNT times.  Stop at beginning of buffer.
Return count of statements left to move.") ("python-next-statement" . "Go to start of next statement.
With argument COUNT, do it COUNT times.  Stop at end of buffer.
Return count of statements left to move.") ("python-beginning-of-block" . "Go to start of current block.
With numeric arg, do it that many times.  If ARG is negative, call
`python-end-of-block' instead.
If point is on the first statement of a block, use its outer block.
If current statement is in column zero and doesn't start a block, or
point is already at the start of an outer block, don't move and return nil.
Otherwise return non-nil.") ("python-end-of-block" . "Go to end of current block.
With numeric arg, do it that many times.  If ARG is negative,
call `python-beginning-of-block' instead.
If current statement is in column zero and doesn't open a block, or
point is already at the start of an outer block,don't move and return nil.
Otherwise return t.") ("python-electric-colon" . "Insert a colon and maybe outdent the line if it is a statement like `else'.
With numeric ARG, just insert that many colons.  With C-u,
just insert a single colon.") ("python-backspace" . "Maybe delete a level of indentation on the current line.
Do so if point is at the end of the line's indentation outside
strings and comments.
Otherwise just call `backward-delete-char-untabify'.
Repeat ARG times.") ("python-check" . "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default.") ("run-python" . "Run an inferior Python process, input and output via buffer *Python*.
CMD is the Python command to run.  NOSHOW non-nil means don't show the
buffer automatically.

Normally, if there is a process already running in `python-buffer',
switch to that buffer.  Interactively, a prefix arg allows you to edit
the initial command line (default is `python-command'); `-i' etc. args
will be added to this as appropriate.  A new process is started if:
one isn't running attached to `python-buffer', or interactively the
default `python-command', or argument NEW is non-nil.  See also the
documentation for `python-buffer'.

Note that, as a security measure, modules won't be loaded from the
current directory if this command is invoked initially in a
world-writable directory.

Runs the hook `inferior-python-mode-hook' (after the
`comint-mode-hook' is run).  (Type C-h m in the process
buffer for a list of commands.)") ("python-send-region" . "Send the region to the inferior Python process.
May print a message, so only suitable for interactive use.") ("python-send-string" . "Evaluate STRING in inferior Python process.") ("python-send-buffer" . "Send the current buffer to the inferior Python process.") ("python-send-defun" . "Send the current defun (class or method) to the inferior Python process.") ("python-switch-to-python" . "Switch to the Python process buffer, maybe starting new process.
With prefix arg, position cursor at end of buffer.") ("python-switch-to-source" . "Return whence M-x python-switch-to-python was last called.
Only succeeds once after each use of M-x python-switch-to-python.") ("python-send-region-and-go" . "Send the region to the inferior Python process.
Then switch to the process buffer.") ("python-load-file" . "Load a Python file FILE-NAME into the inferior Python process.
If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names.") ("python-set-proc" . "Set the default value of `python-buffer' to correspond to this buffer.
If the current buffer has a local value of `python-buffer', set the
default (global) value to that.  The associated Python process is
the one that gets input from M-x python-send-region et al when used
in a buffer that doesn't have a local value of `python-buffer'.") ("python-describe-symbol" . "Get help on SYMBOL using `help'.
Interactively, prompt for symbol.

Symbol may be anything recognized by the interpreter's `help'
command -- e.g. `CALLS' -- not just variables in scope in the
interpreter.  This only works for Python version 2.2 or newer
since earlier interpreters don't support `help'.

In some cases where this doesn't find documentation, C-h S
will.") ("python-fill-paragraph" . "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.") ("python-shift-left" . "Shift lines in region COUNT (the prefix arg) columns to the left.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.  It is an error if any lines in the region are indented less than
COUNT columns.") ("python-shift-right" . "Shift lines in region COUNT (the prefix arg) columns to the right.
COUNT defaults to `python-indent'.  If region isn't active, just shift
current line.  The region shifted includes the lines in which START and
END lie.") ("python-mark-block" . "Mark the block around point and go to the beginning of it.
Do nothing if not in a block.
Uses `python-beginning-of-block', `python-end-of-block'.") ("python-find-imports" . "Find top-level import statements, updating `python-imports'.") ("python-find-function" . "Find source of definition of function NAME.
Interactively, prompt for name.") ("python-expand-template" . "Expand template named NAME.
Interactively, prompt for the name with completion.") ("python-setup-brm" . "Set up Bicycle Repair Man refactoring tool (if available).

Note that the `refactoring' features change files independently of
Emacs and may modify and save the contents of the current buffer
without confirmation.") ("python-2-mode" . "Turn on Python mode with Python 2 keywords.") ("python-3-mode" . "Turn on Python mode with Python 3 keywords."))))
