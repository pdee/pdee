;; a list of python.el<3> commands
(setq python.el<3>-commands (quote (("python-indent-dedent-line" . "De-indent current line.") ("python-indent-dedent-line-backspace" . "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is  not in between the indentation.") ("python-indent-shift-left" . "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `python-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns.") ("python-indent-shift-right" . "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `python-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.") ("python-indent-electric-colon" . "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
C-u, just insert a single colon.") ("python-end-of-defun-function" . "Move point to the end of def or class.
Returns nil if point is not in a def or class.") ("python-nav-sentence-start" . "Move to start of current sentence.") ("python-nav-sentence-end" . "Move to end of current sentence.") ("python-nav-backward-sentence" . "Move backward to start of sentence.  With ARG, do it arg times.
See `python-nav-forward-sentence' for more information.") ("python-nav-forward-sentence" . "Move forward to next end of sentence.  With ARG, repeat.
With negative argument, move backward repeatedly to start of sentence.") ("python-nav-jump-to-defun" . "Jump to the definition of DEF in current file.") ("run-python" . "Run an inferior Python process.
Input and output via buffer named after
`python-shell-buffer-name'.  If there is a process already
running in that buffer, just switch to it.
With argument, allows you to define DEDICATED, so a dedicated
process for the current buffer is open, and define CMD so you can
edit the command used to call the interpreter (default is value
of `python-shell-interpreter' and arguments defined in
`python-shell-interpreter-args').  Runs the hook
`inferior-python-mode-hook' (after the `comint-mode-hook' is
run).
(Type C-h m in the process buffer for a list of commands.)") ("run-python-internal" . "Run an inferior Internal Python process.
Input and output via buffer named after
`python-shell-internal-buffer-name' and what
`python-shell-internal-get-process-name' returns.  This new kind
of shell is intended to be used for generic communication related
to defined configurations.  The main difference with global or
dedicated shells is that these ones are attached to a
configuration, not a buffer.  This means that can be used for
example to retrieve the sys.path and other stuff, without messing
with user shells.  Runs the hook
`inferior-python-mode-hook' (after the `comint-mode-hook' is
run).  (Type C-h m in the process buffer for a list
of commands.)") ("python-shell-send-string" . "Send STRING to inferior Python PROCESS.
When MSG is non-nil messages the first line of STRING.") ("python-shell-send-region" . "Send the region delimited by START and END to inferior Python process.") ("python-shell-send-buffer" . "Send the entire buffer to inferior Python process.") ("python-shell-send-defun" . "Send the current defun to inferior Python process.
When argument ARG is non-nil sends the innermost defun.") ("python-shell-send-file" . "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.") ("python-shell-switch-to-shell" . "Switch to inferior Python process buffer.") ("python-shell-completion-complete-at-point" . "Perform completion at point in inferior Python process.") ("python-shell-completion-complete-or-indent" . "Complete or indent depending on the context.
If content before pointer is all whitespace indent.  If not try
to complete.") ("python-completion-complete-at-point" . "Complete current symbol at point.
For this to work the best as possible you should call
`python-shell-send-buffer' from time to time so context in
inferior python process is updated properly.") ("python-fill-paragraph-function" . "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified.") ("python-check" . "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.  See
`python-check-command' for the default.") ("python-eldoc-at-point" . "Get help on SYMBOL using `help'.
Interactively, prompt for symbol."))))
